module Hooks where

import           ClassyPrelude
import qualified Network.HTTP.Types.Status as Http
import           Util
import           Web.Spock.Safe            (text)
baseHook :: Handler () (HVect '[])
baseHook = return HNil

-- authHook :: Handler (HVect xs) (HVect ((UserId, User) ': xs))
-- authHook =
--   maybeUser $ \mUser -> do
--     oldCtx <- getContext
--     case mUser of
--       Nothing ->
--         noAccessPage "Unknown user. Login first!"
--       Just val ->
--         return (val :&: oldCtx)

noAccessPage :: Text -> Handler ctx a
noAccessPage msg = do
  setStatus Http.status403
  prefResp <- preferredFormat
  case prefResp of
    PrefJSON -> json ("status 403" :: String)
    _ -> lucid $ toHtml ("status 403" :: String)

data IsAdmin = IsAdmin

adminHook :: ListContains n (UserId, User) xs => Handler (HVect xs) (HVect (IsAdmin ': xs))
adminHook = do
  (_ :: UserId, user) <- liftM findFirst getContext
  oldCtx <- getContext
  if userIsAdmin user
    then return (IsAdmin :&: oldCtx)
    else noAccessPage "You don't have enough rights, sorry" >> redirect "/"

-- data IsAuthor = IsAuthor

-- authorHook :: ListContains n (UserId, User) xs => Handler (HVect xs) (HVect (IsAuthor ': xs))
-- authorHook =
--     do (_ :: UserId, user) <- liftM findFirst getContext
--        oldCtx <- getContext
--        if userIsAuthor user then return (IsAuthor :&: oldCtx) else noAccessPage "You don't have enough rights, sorry"

data IsGuest = IsGuest

guestOnlyHook :: Handler (HVect xs) (HVect (IsGuest ': xs))
guestOnlyHook =
    maybeUser $ \mUser ->
    do oldCtx <- getContext
       case mUser of
         Nothing -> return (IsGuest :&: oldCtx)
         Just _ -> text "Sorry, you are already logged in!"

-- noAccessPage :: T.Text -> Handler ctx a
-- noAccessPage msg =
--     do setStatus Http.status403
--        prefResp <- preferredFormat
--        case prefResp of
--          PrefJSON ->
--              json (CommonError msg)
--          _ ->
--              mkSite' (panelWithErrorView "No Access" Nothing (toHtml msg))

-- maybeUser = unde
maybeUser :: (Maybe (UserId, User) -> Handler ctx a) -> Handler ctx a
maybeUser action = do
  sess <- readSession
  case sess of
    Nothing -> action Nothing
    Just sid -> do
      mUser <- db $ loadUser sid
      action mUser

loadUser :: SessionId -> Req (Maybe (UserId, User))
loadUser sessId = do
  mSess <- get sessId
  now <- liftIO getCurrentTime
  case mSess of
    Just sess | sessionValidUntil sess > now -> do
      mUser <- get (sessionUserId sess)
      return $ fmap (\user -> (sessionUserId sess, user)) mUser
    _ -> return Nothing
