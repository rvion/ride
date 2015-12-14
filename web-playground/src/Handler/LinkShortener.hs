module Handler.LinkShortener where

import           ClassyPrelude
import           Control.Exception         (SomeException (..))
import qualified Data.ByteString           as BS
import           Data.Time.Clock           (getCurrentTime)
import           Forms
import           Network.HTTP.Types.Method
import           Network.Wai
import           Route                     (urlPath)
import           Template.Layout
import           Util
import           Web.Hashids               as HID
type Widget a = Handler a HTML

allLinksWidget :: Widget a
allLinksWidget = do
  pageIdx <- fromMaybe 0 <$> param "page"
  totalNbPages <- db (count allLinks)
  _url <- getPageUrl
  _allUrls <- db . recentLinks $ pageIdx

  return $ do
    h2_ "existing links"
    table_ [class_ "table table-striped table-condensed"] $ do
      thead_ [] $ tr_ $ mapM (th_ [] .asT) ["url", "shorturl", "id"]
      tbody_ [] $ forM_ _allUrls $ \(Entity k  (Link url shortId _)) ->
        tr_ [] $ do
        td_ [] $ url_ url
        td_ [] $ url_ (renderRoute urlPath (unpackBS shortId))
        td_ [] $ toS (keyToInt k)
    div_ [] $ do
      when (pageIdx > 0) $ a_ [href_ (_url <> "?page=" <> (pack.show$pageIdx - 1)), class_ "btn"] "<"
      toS totalNbPages
      a_ [href_ (_url <> "?page=" <> (pack.show$pageIdx + 1)), class_ "btn"] ">"

  where
    nbResultsPerPage :: Int
    nbResultsPerPage = 10
    recentLinks :: Int -> Req [Entity Link]
    recentLinks pageIdx = catch
      (selectList []
        [ OffsetBy (pageIdx * nbResultsPerPage)
        , LimitTo nbResultsPerPage
        , Desc LinkId])
      (\(SomeException e) -> error . show $ e )

saveResultWidget :: Maybe NewLinkForm -> Widget a
saveResultWidget mbU = case mbU of
  Nothing -> return (notice "please fix errors")
  Just (NewLinkForm url pref) -> do
    mbExist <- db $ selectFirst [LinkUrl ==. url] []
    time_ <- liftIO getCurrentTime
    case mbExist of
      Just e ->  return (toS e)
      Nothing -> do
        _hash <- shorten <$>
          db (seed <$> selectFirst [] [Desc LinkId])
          -- (traceShowId <$> db (count allLinks))
        let link = Link url _hash time_
        dbSafe (insert_ link) >>= \x -> case x of
          Right _ -> return $ notice (tshow link <> " saved!")
          Left err -> return $ notice err

getPageUrl :: Handler a Text
getPageUrl = decodeUtf8 . rawPathInfo <$> request

isGet :: Handler a Bool
isGet = (== methodGet) . requestMethod <$> request

linkShortenerHandler :: Handler a b
linkShortenerHandler = do
  req <- getPageUrl
  (view, mbU) <- runForm "user" newLinkForm
  _isGet <- isGet
  _msg <- if _isGet
    then return $ h2_ [] (asT "fill the form below")
    else saveResultWidget mbU
  _allLinksWidget <- allLinksWidget

  page $ do
    h1_ "Add new link:"
    unless _isGet _msg
    mkForm [method_ "post", action_ req] $ do
      newLinkView view
      button_ [class_ "btn btn-success "] "Submit"
    _allLinksWidget
    -- div_ (toS req)

-- type Req a = SqlPersistT (LoggingT (ResourceT IO)) a
-- a :: Req (Maybe (Entity Link))
-- a = db $ selectFirst [LinkUrl ==. "url"] [] -- get (toSqlKey 1)
-- TODO
getUrl :: String -> Handler a b
getUrl l = do
  mbLink <- db $ getBy (UniqueHash (fromString l)) -- undefined --a
  case mbLink of
    Just (Entity _ (Link{linkUrl})) -> redirect linkUrl
    Nothing -> page $ notice "Nothing to see here."


allLinks :: [Filter Link]
allLinks = []

url_ :: Text -> HTML
url_ link = a_ [href_ link] $ toS link

ctx :: HID.HashidsContext
ctx = HID.hashidsSimple "this is my salt"

shorten :: Int -> ByteString
shorten x = HID.encodeList ctx [x]

restore :: ByteString -> [Int]
restore x = HID.decode ctx x

data NewLinkForm = NewLinkForm
  { url          :: Text
  , preferedHash :: Maybe Text
  } deriving (Show)

newLinkForm :: Monad m => Form HTML m NewLinkForm
newLinkForm = NewLinkForm
  <$> "url" .: check "should be an url" (\url -> "http" `isPrefixOf` url) (text Nothing)
  <*> "preferedHash" .: emptyOrStringCheck (optionalText Nothing)
  where
    emptyOrStringCheck = check "only '-', [a..z], [A..Z], [0..9] are accepted" emptyOrString
    emptyOrString (Just pref) = all (`elem` validChars) pref -- "http" `isPrefixOf` url
    emptyOrString Nothing = True
    validChars = '-' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']


newLinkView :: View HTML -> HTML
newLinkView view = div_ $ do
  textField "url" view
  textField "preferedHash" view


addError :: Text -> HTML ->View HTML -> View HTML
addError field err view = view {viewErrors = ([field], err) : viewErrors view}

seed :: Maybe (Entity Link) -> Int
seed mbe = maybe 1 (\(Entity k _) -> 1 + keyToInt k) mbe

keyToInt :: (ToBackendKey SqlBackend a) => Key a -> Int
keyToInt = fromIntegral . fromSqlKey



