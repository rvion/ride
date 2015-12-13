module IDE.Types where

import qualified Data.Map as Map
import Data.Map (Map)
import IfaceSyn

type Modules = Map String FilePath
-- | RTerm stands for Reexportable Term
data RTerm
  = RId { rName, rType :: String }
  | RData { rName, rType :: String, rNbTyVars :: Int }
  -- | RSynonym { rName :: String, rNbTyVars :: Int }


jetpackFolder :: String
jetpackFolder = "jetpack/src/"