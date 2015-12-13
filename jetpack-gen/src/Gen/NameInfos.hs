module Gen.NameInfos where

import           Name
import           Outputable

getNameInfos n =
  [ ppr $ getSrcLoc n --       -> Unique
  , ppr $ nameUnique n --       -> Unique
  , ppr $ nameOccName n --      -> OccName
  , ppr $ nameModule n --       -> Module
  , ppr $ nameModule_maybe n -- -> Maybe Module
  , ppr $ localiseName n --     -> Name
  , ppr $ nameSrcLoc n --       -> SrcLoc
  , ppr $ nameSrcSpan n --      -> SrcSpan
  , pprNameDefnLoc n --   -> SDoc
  , pprDefinedAt n --     -> SDoc
  , ppr $ isSystemName n --     -> Bool
  , ppr $ isInternalName n --   -> Bool
  , ppr $ isExternalName n --   -> Bool
  , ppr $ isTyVarName n --      -> Bool
  , ppr $ isTyConName n --      -> Bool
  , ppr $ isDataConName n --    -> Bool
  , ppr $ isValName n --        -> Bool
  , ppr $ isVarName n --        -> Bool
  , ppr $ isWiredInName n --    -> Bool
  , ppr $ isBuiltInSyntax n --  -> Bool
  , ppr $ wiredInNameTyThing_maybe n -- -> Maybe TyThing
  ]
