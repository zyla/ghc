{-# LANGUAGE TypeFamilies, DataKinds #-}
module SymVal where

import GHC.TypeLits
import Data.Proxy

test1, test2, test3 :: ()

test1 = () :: SymbolVal "test" ~ [116, 101, 115, 116] => ()

test2 = () :: SymbolVal "" ~ '[] => ()

test3 = () :: SymbolVal "∀" ~ '[8704] => ()
