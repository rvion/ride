module Handler.User where

import           ClassyPrelude
import           Forms
import           Route
import           Util

userHandler :: Handler a b
userHandler = do
  (form, mbU) <- runForm "user" userForm
  let user = maybe "" (toHtml . show) mbU

  page $ do
    user
    h1_ "Hello you 2 :)"
    h2_ (return . show $ (3 :: Int))
    mkForm [method_ "post", action_ (renderRoute userR)] $ do
      userView form
      button_ [] "Submit"

data UserForm = UserForm
  { userName :: Text
  , userMail :: Text
  } deriving (Show)

userForm :: Monad m => Form HTML m UserForm
userForm = UserForm
  <$> "name" .: checktest (text Nothing)
  <*> "mail" .: checkEmail (text Nothing)

userView :: View HTML -> HTML
userView view = div_ $ do
  h3_ "user form plop"
  textField "name" view
  textField "mail" view
