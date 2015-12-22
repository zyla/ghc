{-# LANGUAGE DataKinds, MagicHash, TypeFamilies #-}

import HasFieldFail01_A (T(MkT))

import GHC.Prim (Proxy#, proxy#)
import GHC.Records (HasField(..))

-- This should fail to solve the HasField constraint, because foo is
-- not in scope.
main = print (getField (proxy# :: Proxy# "foo") (MkT 42) :: Int)
