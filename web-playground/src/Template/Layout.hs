module Template.Layout where

import           ClassyPrelude
-- import           Data.Text     (Text)
import           Lucid

-- layout :: HTML -> HTML
defaultLayout :: (Monad m) => HtmlT m b -> HtmlT m b
defaultLayout content = do
  doctype_
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport",content_ "width=device-width, initial-scale=1"]
    link_ [href_ "//fonts.googleapis.com/css?family=Open+Sans",rel_ "stylesheet",type_ "text/css"]
    link_ [href_ "//cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.1.0/css/bootstrap.min.css",rel_ "stylesheet",type_ "text/css"]
    title_ "YSU Closing Status"
  body_ $
    div_ [class_ "container"] content

simpleError :: Monad m => Text -> Text -> HtmlT m b -> HtmlT m b
simpleError title errMsg ct =
  div_ [class_ "panel panel-info", style_ "margin-top: 30px;"] $
    div_ [class_ "panel-heading"] $ do
  div_ [class_ "panel-title"] $ toHtml title
  div_ [class_ "panel-body"] $ do
    toHtml errMsg
    div_ ct

notice :: Monad m => Text -> HtmlT m ()
notice msg =
  div_ [class_ "panel panel-info", style_ "margin-top: 30px;"] $
    div_ [class_ "panel-heading"] $
      div_ [class_ "panel-body"] (toHtml msg)
