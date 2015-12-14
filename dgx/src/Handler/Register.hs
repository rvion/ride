{-# LANGUAGE OverloadedStrings #-}
module Handler.Register where

import           ClassyPrelude
import           Forms
-- import           GHC.TypeLits  (Symbol)
import           Handler.Auth
import           Hooks
import           Route
import           Util

registerHandler ::
  ( ListContains n IsGuest xs
  , NotInList (UserId, User) xs ~ 'True
  ) => Handler (HVect xs) a
registerHandler = do
  (view, mbRegister) <- runForm "registerForm" registerForm
  let formView msg view = page $
        simpleError "Register" msg $
          mkForm [method_ "post", action_ (renderRoute registerR)] $ do
            registerView view
            button_ [] "Submit"

  case mbRegister of
    (Nothing) -> formView "No Error" view
    (Just registerReq) ->
      if rr_password registerReq /= rr_passwordConfirm registerReq
      then formView "Passwords do not match" view
      else do
        registerRes <- db $ registerUser (rr_username registerReq) (rr_email registerReq) (rr_password registerReq)
        case registerRes of
          Left errMsg -> formView (pack errMsg) view
          Right _ -> page $ simpleError "Register - Success!" "" "Great! You may now login."

data RegisterForm = RegisterForm
  { rr_username        :: Text
  , rr_password        :: Text
  , rr_passwordConfirm :: Text
  , rr_email           :: Text
  } deriving (Show)

registerForm :: Monad m => Form HTML m RegisterForm
registerForm = RegisterForm
  <$> "name" .: usernameFormlet Nothing
  <*> "password1" .: passwordFormlet Nothing
  <*> "password2" .: passwordFormlet Nothing
  <*> "email" .: emailFormlet Nothing

registerView :: View HTML -> HTML
registerView view = div_ $ do
  h3_ "user form plop"
  textField "name" view
  textField "password1" view
  textField "password2" view
  textField "email" view
