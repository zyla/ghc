{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators #-}
module TFA where

type family Map f x where
    Map f '[] = '[]
    Map f (x ': xs) = f x ': Map f xs

data P (a :: [*]) = P

type family AddListToInt x where
    AddListToInt Int = [Int]
    AddListToInt x = x

x :: P '[[Int], String]
x = P :: P (Map AddListToInt '[Int, String])


type family Id x where Id x = x

data P1 (a :: * -> *) = PC

y :: P1 Id
y = PC
