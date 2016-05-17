-- Exists to allow TyCoRep to import pretty-printers
module IfaceType where

import Var (TyVar)
import {-# SOURCE #-} TyCoRep (Type, TyLit, TyBinder)
import Outputable

data IfaceType
data IfaceTyLit
data IfaceForAllBndr
data IfaceTyConBinder
data IfaceTvBndr

pprIfaceType, pprParendIfaceType :: IfaceType -> SDoc
pprIfaceSigmaType :: IfaceType -> SDoc
pprIfaceTyLit :: IfaceTyLit -> SDoc
pprIfaceForAll :: [IfaceForAllBndr] -> SDoc
pprIfaceTvBndr :: Bool -> IfaceTvBndr -> SDoc
pprUserIfaceForAll :: [IfaceForAllBndr] -> SDoc
pprIfaceContext :: Outputable a => [a] -> SDoc
pprIfaceContextArr :: Outputable a => [a] -> SDoc

toIfaceType :: Type -> IfaceType
toIfaceTyLit :: TyLit -> IfaceTyLit

zipIfaceBinders :: [TyVar] -> [TyBinder] -> [IfaceTyConBinder]

toDegenerateBinders :: [TyBinder] -> [IfaceTyConBinder]
