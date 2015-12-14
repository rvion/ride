module Route where

import           ClassyPrelude
import           Web.Spock.Safe (Path, var, (<//>))
-- import Data.Text (Text)

helloR :: Path '[Text]
helloR = "hello" <//> var

addR :: Path '[Int, Int]
addR = "calculator" <//> var <//> "+" <//> var

userR :: Path '[]
userR = "user"

registerR :: Path '[]
registerR = "register"

urlPath :: Path '[String]
urlPath = "url" <//> var

urlNewPath :: Path '[]
urlNewPath = "url" <//> "new"

