{-# LANGUAGE OverloadedHLists, GADTs, DataKinds, KindSignatures, MultiParamTypeClasses, FlexibleInstances, TypeOperators #-}

import GHC.Exts (IsCons(..), IsNil(..))

list :: [Char]
list = ['a', 'b', 'c']

data Nat = Z | S Nat
data Vec (len :: Nat) a where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

instance IsNil (Vec Z a) where
  nil = VNil

instance IsCons a (Vec n a) (Vec (S n) a) where
  cons = VCons

vecToList :: Vec n a -> [a]
vecToList VNil = []
vecToList (VCons x xs) = x : vecToList xs

vec :: Vec (S (S Z)) Char
vec = ['a', 'b']

data HList xs where
  HNil :: HList '[]
  HCons :: a -> HList xs -> HList (a ': xs)

instance IsNil (HList '[]) where
  nil = HNil

instance IsCons a (HList xs) (HList (a ': xs)) where
  cons = HCons

infixr 1 `HCons`

hlist :: HList '[Int, String, Char]
hlist = [1, "foo", 'X']

main = do
    print list
    print (vecToList vec)
    case hlist of
      (int `HCons` string `HCons` char `HCons` HNil) -> do
        print int
        print string
        print char
