{-# LANGUAGE DataKinds, ExistentialQuantification, MagicHash, RankNTypes #-}

import GHC.Prim (Proxy#, proxy#)
import GHC.Records (HasField(..))

data T = MkT { foo :: forall a . a -> a }
data U = forall b . MkU { bar :: b }

-- This should fail because foo is higher-rank.
x = getField (proxy# :: Proxy# "foo") (MkT id)

-- This should fail because bar is a naughty record selector (it
-- involves an existential).
y = getField (proxy# :: Proxy# "bar") (MkU True)

main = return ()
