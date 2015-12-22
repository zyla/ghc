{-# LANGUAGE DataKinds, FlexibleInstances, MagicHash,
             MultiParamTypeClasses, TypeFamilies #-}

import GHC.Prim (Proxy#, proxy#)
import GHC.Records (HasField(..))

type family B where B = Bool

data T = MkT { foo :: Int, bar :: B }

data U a b = MkU { baf :: a }

t = MkT 42 True

u :: U Char Char
u = MkU 'x'

-- A virtual foo field for U
instance HasField "foo" (U a b) [Char] where
  getField _ _ = "virtual"

main = do print (getField (proxy# :: Proxy# "foo") t)
          print (getField (proxy# :: Proxy# "bar") t)
          print (getField (proxy# :: Proxy# "baf") u)
          print (getField (proxy# :: Proxy# "foo") u)
