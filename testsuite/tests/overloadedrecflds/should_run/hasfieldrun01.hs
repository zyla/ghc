{-# LANGUAGE DataKinds, MagicHash, TypeFamilies #-}

import GHC.Prim (Proxy#, proxy#)
import GHC.Records (getField)

type family B where B = Bool

data T = MkT { foo :: Int, bar :: B }

data U a b = MkU { baf :: a }

t = MkT 42 True
u = MkU 'x'

main = do print (getField (proxy# :: Proxy# "foo") t)
          print (getField (proxy# :: Proxy# "bar") t)
          print (getField (proxy# :: Proxy# "baf") u)
