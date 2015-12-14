module Gen.Types where

import           Data.Map (Map)
import qualified Data.Map as Map
import           IfaceSyn

type Modules = Map String FilePath
-- | RTerm stands for Reexportable Term

data RTerm
  = RId { rName, rType :: String }
  | RData { rName, rType :: String, rNbTyVars :: Int }
  | RClass { rName :: String, rAssos :: [RTerm]}
  deriving Show

-- | RSynonym { rName :: String, rNbTyVars :: Int }


jetpackFolder :: String
jetpackFolder = "jetpack/"

jetpackLibFolder :: String
jetpackLibFolder = jetpackFolder ++ "src/"

