{-# LANGUAGE NoImplicitPrelude
           , MultiParamTypeClasses
           , MagicHash
           , KindSignatures
           , DataKinds
           , FunctionalDependencies
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Records
-- Copyright   :  (c) Adam Gundry 2015
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
--
-----------------------------------------------------------------------------

module GHC.Records
       ( HasField(..)
       ) where

import GHC.Base ( Symbol )
import GHC.Exts ( Proxy# )

class HasField (x :: Symbol) r a | x r -> a where
  getField :: Proxy# x -> r -> a
