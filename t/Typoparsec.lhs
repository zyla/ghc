---
title: TypoParsec - a parser combinator library for TLH
---

In the [previous post](/posts/2016-09-06-tlh.html), we have written a toy
example program in Type Level Haskell. This was hardly satisfying; time to
write something more sophisticated - a parser. Since Haskellers know better
than to handwrite parsers, we will make a small parser combinator library,
taking inspiration from [parsec](https://hackage.haskell.org/package/parsec).

This post is Literate Haskell. You can copy'n'paste it and watch your CPU heat
up while GHC checks these bloated types.

Boilerplate out of the way!

> {-# LANGUAGE DataKinds, TypeOperators, TypeInType, TypeFamilies, RankNTypes,
>   UndecidableInstances, GADTs, ConstraintKinds, TypeApplications #-}
> module TypoParsec where
> 
> import Prelude hiding (Char)
> import GHC.TypeLits
> type Char = Nat
>
> type family a && b where
>   True  && b = b
>   False && _ = False
> infixr 3 &&
>
> type family If cond a b where
>   If True  a _ = a
>   If False _ b = b

== Parser

Our parser definition is very simple. There are three basic actions (consume a
character, produce a value, fail) and two ways to combine them: sequential and
parallel. We will not bother with error messages.

> data Parser a where
>  Consume :: Parser Nat
>  Return :: a -> Parser a
>  Fail :: Parser a
>  (:>>=) :: Parser b -> (b -> Parser a) -> Parser a
>  (:<|>) :: Parser a -> Parser a -> Parser a
>
> type family RunParser (p :: Parser a) (s :: [Char]) :: Maybe (a, [Char])

The basic actions are easy:

> type instance RunParser Consume (x ': xs) = Just '(x, xs)
> type instance RunParser Consume '[] = Nothing
> type instance RunParser (Return x) xs = Just '(x, xs)
> type instance RunParser Fail _ = Nothing

For sequential composition, we need the continuations to be defunctionalized.
But since this is a library, it would be unwise to use a closed data type.
Let's define generic defunctionalized functions:

For example, let's define a function that will be useful later - that
checks if a character is in range:

> type family IsBetween lo hi c where IsBetween lo hi c = lo <=? c && c <=? hi

== Unit tests

This function is non-trivial, we should test it. Unit tests in TLH are easy:

> -- like 'Dict' from ekmett's constraints library
> data Test c where Test :: c => Test c

> isBetweenTests = Test @
>   (  (IsBetween 2 4) 1 ~ False
>   ,  (IsBetween 2 4) 2 ~ True
>   ,  (IsBetween 2 4) 3 ~ True
>   ,  (IsBetween 2 4) 4 ~ True
>   ,  (IsBetween 2 4) 5 ~ False
>   )

That's it. Use of the `Test` data constructor will force the compiler to
evaluate these constraints. If they can't be satisfied, the code won't even
compile!

Back to `Parser`.

== Sequential composition

> type instance RunParser (p :>>= k) s = RunParserSeqHandleResult (RunParser p s) k
> type family RunParserSeqHandleResult result k where
>   RunParserSeqHandleResult (Just '(x, xs)) k = RunParser ( k x) xs
>   RunParserSeqHandleResult Nothing         _ = Nothing

Does it work? Basic test:

> type BasicTest = Consume :>>= Const Consume
>
> basicTest = Test @( RunParser BasicTest [1,2,3] ~ Just '( 2 , '[3]) )

To make it useful, we need some standark monadic combinators:

> -- Const x b :: a -> b where x :: a
> --  (Const x t) y = x
> -- where y :: t
> type family Const x b where Const (x :: a) (b :: k) = x

> -- p1 *> p2 -- parse p1 and p2, return result of p2
> type family p1 *> p2 where (p1 :: Parser a) *> (p2 :: Parser b) = p1 :>>= Const p2

> -- p1 <* p2 -- parse p1 and p2, return result of p1.
> type family p1 <* p2 where
>   (p1 :: Parser a) <* (p2 :: Parser b) = p1 :>>= FollowedByCont p2
>
> type family FollowedByCont p2 x where FollowedByCont p2 x = p2 *> Return x
>
> followedByTest = Test @( RunParser (CharP 1 <* CharP 2) '[1,2] ~ Just '( 1, '[] ) )

> -- Fmap :: (a -> b) -> Parser a -> Parser b
> type Fmap (f :: a -> b) (p :: Parser a) = p :>>= FmapCont f
> type family FmapCont (f :: a -> b) x where FmapCont f x = Return (f x)
>
> fmapTest = Test @( RunParser (Just `Fmap` Return 1) '[] ~ Just '( Just 1 , '[]) )

> -- Ap :: Parser (a -> b) -> Parser a -> Parser b
> type Ap (pf :: Parser (a -> b)) (pa :: Parser a) = pf :>>= ApCont0 pa
> type family ApCont0 pa (f :: a -> b) where ApCont0 pa f = Fmap f pa

We can now write Applicative-style parsers:

> fmapApTest = Test @( RunParser ( '(,) `Fmap` Consume `Ap` Consume ) [1,2,3]
>   ~ Just '( '(1, 2), '[3] ) )

== Single-character utilities

> -- Satisfy :: (Char -> Bool) -> Parser Char
> -- Consume character satisfying given predicate and return it
> type Satisfy pred = Consume :>>= SatisfyCont pred
> type family SatisfyCont pred (c :: Char) where
>   SatisfyCont pred c =
>     If (pred c)
>        (Return c)
>        Fail

> -- CharP :: Char -> Parser Char
> -- Consume exactly the given character
> type CharP c = Satisfy (IsBetween c c)

> charTest1 = Test @( RunParser (CharP 1) '[1] ~ Just '( 1, '[] ) )
> charTest2 = Test @( RunParser (CharP 1) '[2] ~ Nothing )

== Alternative

> type instance RunParser (a :<|> b) s = RunParserAltHandleResult (RunParser a s) b s
> type family RunParserAltHandleResult result b s where
>   RunParserAltHandleResult (Just r) _ _ = Just r
>   RunParserAltHandleResult Nothing  b s = RunParser b s

> alternativeTests = Test @
>   ( RunParser (Return 1 :<|> Return 2) '[] ~ Just '( 1, '[] )
>   , RunParser (Fail     :<|> Return 1) '[] ~ Just '( 1, '[] )
>   -- failed branch shouldn't consume input
>   , RunParser ((Consume *> Fail) :<|> Consume) '[1] ~ Just '( 1, '[] )
>   )

This lets us implement the `many` combinator:

> -- Many p -- parse p until it fails, return list of results.
> type family Many p where Many (p :: Parser a) = (p :>>= ManyCont p) :<|> Return '[]
> type family ManyCont (p :: Parser a) x where ManyCont p x = Fmap ('(:) x) (Many p)

> manyTest1 = Test @( RunParser (Many Consume) '[] ~ Just '( '[], '[] ) )
> manyTest2 = Test @( RunParser (Many Consume) '[1] ~ Just '( '[1], '[] ) )
> manyTest3 = Test @( RunParser (Many Consume) '[1,2] ~ Just '( '[1,2], '[] ) )
> manyTest4 = Test @( RunParser (Many (CharP 1)) '[1,1,1,2] ~ Just '( '[1,1,1], '[2] ) )

> type Many1 p = '(:) `Fmap` p `Ap` Many p

== A real example

For a test of this machinery, let's build a s-expression parser.

> data SExpr = Atom [Char] | List [SExpr]

For simplicity, atoms will be strings of lowercase Latin letters.

> type IsAlpha = IsBetween 97 122 -- 'a'..'z'
> type AtomP = Fmap Atom (Many1 (Satisfy IsAlpha))
>
> atomTest1 = Test @( RunParser AtomP '[97,98,32] ~ Just '( Atom '[97,98], '[32] ) )

> type IsSpace = IsBetween 32 32 -- ' '
> type WhiteSpace = Many (Satisfy IsSpace)

> type LeftParenC = 40 -- '('
> type RightParenC = 41 -- ')'

> type ListP = Fmap List (CharP LeftParenC *> Many ( WhiteSpace *> LazySExprP ) <* CharP RightParenC)

> listTest1 = Test @( RunParser ListP '[40,97,32,98,99,41]
>    ~ Just '( List '[Atom '[97], Atom '[98,99]], '[] ) )
> listTest2 = Test @( RunParser ListP '[40,41] ~ Just '( List '[], '[] ) )
> listTest3 = Test @( RunParser ListP '[40,40,41,32,40,41,41]
>   ~ Just '( List '[List '[], List '[]], '[] ) )

> type family SExprP where SExprP = AtomP :<|> ListP

=== Tying the knot

Notice we used `LazySExprP` in the definition of `ListP`. Otherwise we would
create a cycle in type family declarations (using type families would put GHC
in an infinite loop). We need to introduce another combinator, `Lazy`.

> -- Lazy p -- works the same as ` p '()`.
> type Lazy (p :: () -> Parser a) = Return '() :>>= p
> lazyTest1 = Test @( RunParser (Lazy (Const (Return 42))) '[] ~ Just '( 42, '[] ) )

> type family LazySExprP :: Parser SExpr where LazySExprP = Lazy UnitToSExprP
> type family UnitToSExprP unit where UnitToSExprP unit = SExprP

Final test:

> -- "(a (c d) j kl)"    ->    List [Atom "a", List [Atom "c", Atom "d"], Atom "j", Atom "kl"]
> sexprTest1 = Test @( RunParser SExprP '[40,97,32,40,99,32,100,41,32,32,106,107,41]
>   ~ Just '( List '[Atom '[97], List '[Atom '[99], Atom '[100]], Atom '[106,107]], '[] ) )
>
> -- "(a b" -> failure
> sexprTest2 = Test @( RunParser SExprP '[40,97,32,98] ~ Nothing )
>
> -- "1" -> failure
> sexprTest3 = Test @( RunParser SExprP '[49] ~ Nothing )

It works!

== Conclusions

TLH is a very powerful language. We can write parsers with little effort, just
like in Haskell. It also offers very good testing facilities - tests are ran
automatically during compilation.
