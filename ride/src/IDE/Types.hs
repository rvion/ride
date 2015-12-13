module IDE.Types where

import qualified Data.Map as Map
import Data.Map (Map)
import IfaceSyn

type Modules = Map String FilePath

-- | RTerm stands for Reexportable Term
data RTerm
  = RId { rName :: String }
  | RData { rName :: String, rNbTyVars :: Int }
  -- | RSynonym { rName :: String, rNbTyVars :: Int }
