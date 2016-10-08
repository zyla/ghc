{-# LANGUAGE AllowAmbiguousTypes
           , DataKinds
           , ExplicitForAll
           , FunctionalDependencies
           , KindSignatures
           , MultiParamTypeClasses
           , NoImplicitPrelude
           , PolyKinds
           , ScopedTypeVariables
           , TypeApplications
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

class HasField (x :: k) r a | x r -> a where
  fromLabel :: r -> a
