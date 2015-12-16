module Gen.Types where

import           Data.Map (Map)
import qualified Data.Map as Map
import           IfaceSyn

type Modules = Map String FilePath
-- | RTerm stands for Reexportable Term

data RTerm
  = RId { rName, rType :: String }
  | RData { rName, rType :: String, rNbTyVars :: Int, rDataTyCon:: [RTerm] }
  | RDataCon { rName :: String, rTyVars::[String], rNbTyVars :: Int}
  | RClass { rName :: String, rNameExported :: Bool, rAssos :: [RTerm]}
  deriving Show

shoCon RId{} = "RId"
shoCon RData{} = "RData"
shoCon RDataCon{} = "RDataCon"
shoCon RClass{} = "RClass"

isRClass :: RTerm -> Bool
isRClass x = case x of
  RClass{} -> True
  _ -> False

-- | RSynonym { rName :: String, rNbTyVars :: Int }


jetpackFolder :: String
jetpackFolder = "jetpack/"

jetpackLibFolder :: String
jetpackLibFolder = jetpackFolder ++ "src/"

