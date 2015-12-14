


module Handler.Auth where

import           ClassyPrelude
import qualified Data.Text              as T
import           Data.Word8
import           Database.Persist.TH
import           System.Random
import           Util

import qualified Crypto.Hash.SHA512     as SHA
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding     as T

hashPassword :: T.Text -> BS.ByteString -> BS.ByteString
hashPassword password salt =
     SHA.finalize $ SHA.updates SHA.init [salt, T.encodeUtf8 password]

createSession :: UserId -> Req SessionId
createSession userId = do
  now <- liftIO getCurrentTime
  insert (Session (addUTCTime (5 * 3600) now) userId)
  -- return ()

killSessions :: UserId -> Req ()
killSessions userId = deleteWhere [ SessionUserId ==. userId ]

loginUser :: Text -> T.Text -> Req (Maybe UserId)
loginUser username password = do
   mUserU <- getBy (UniqueUsername username)
   mUserE <- getBy (UniqueEmail username)
   case mUserU `mplus` mUserE of
     Just userEntity ->
       let user = entityVal userEntity
       in if userPassword user == makeHex (hashPassword password (decodeHex $ userSalt user))
          then return $ Just (entityKey userEntity)
          else return Nothing
     Nothing ->
       return Nothing

registerUser :: T.Text -> T.Text -> T.Text -> Req (Either String String)
registerUser username email password =
    do mUserU <- getBy (UniqueUsername username)
       mUserE <- getBy (UniqueEmail email)
       case (mUserU, mUserE) of
         (Just _, _) -> return (Left "Username already taken!")
         (_, Just _) -> return (Left "Email already registered!")
         (Nothing, Nothing) ->
             do g <- liftIO getStdGen
                let salt = randomBS 512 g
                    hash = hashPassword password salt
                _ <- insert (User username (makeHex hash) (makeHex salt) email False False)
                return (Right "Signup complete. You may now login.")


randomBytes:: Int -> StdGen -> [Word8]
randomBytes 0 _ = []
randomBytes ct g =
    let (value, nextG) = next g
    in fromIntegral value:randomBytes (ct - 1) nextG

randomBS :: Int -> StdGen -> BS.ByteString
randomBS len g =
    BS.pack $ randomBytes len g

makeHex :: BS.ByteString -> T.Text
makeHex = T.decodeUtf8 . B16.encode
{-# INLINE makeHex #-}

decodeHex :: T.Text -> BS.ByteString
decodeHex = fst . B16.decode . T.encodeUtf8
{-# INLINE decodeHex #-}
