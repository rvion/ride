{-# LANGUAGE OverloadedStrings #-}
module Command where

import           Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as A
import Data.Aeson
import qualified Data.Map   as M
import qualified Data.Text  as T
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
type LBS = ByteString

data Command = Command
  { commandId     :: Int
  , commandMethod :: Text
  , commandParams :: [(Text, Value)]
  } deriving (Show)

instance ToJSON Command where
  toJSON cmd = A.object
    [ "id"     .= commandId cmd
    , "method" .= commandMethod cmd
    , "params" .= M.fromList (commandParams cmd)
    ]

data CommandResult = CommandResult
  { resultId     :: Int
  , resultValue  :: Value
  } deriving (Show)

instance FromJSON CommandResult where
  parseJSON (Object v) = CommandResult
    <$> v .: "id"
    <*> (v .: "result" >>= (.: "result") >>= (.: "value"))-- .: "result" .: "value"


-- Chrome Remote actions
goToPage :: Text ->  Command
goToPage page = Command
  { commandId     = 1
  , commandMethod = "Page.navigate"
  , commandParams = [("url", A.String page)]
  }

searchName :: Text ->  Command
searchName name = jsEval (go "#main-search-box" name)
  where
    go :: Text -> Text -> Text
    go inpt txt = T.concat ["$(\"", inpt,"\").val(\"", txt,"\");$(\"#global-search\").submit()"]

clickOnFirstResult :: Command
clickOnFirstResult = jsEval
    "window.location.href = $('#results>li:first-child a.main-headline').attr('href');"

getExperiences :: Command
getExperiences = jsEval getExperiences'

getExperiences'  :: Text
getExperiences' = T.concat $
  [ "result={"
  , "  name: $('#name .full-name').text(),"
  , "  life: $.map($('#background #background-experience-container .editable-item.section-item'),function(e){"
  , "    $e=$(e);"
  , "    return ({"
  , "      title: $e.find('header h4>a').text()," -- Directrice des investissements corporate venture
  , "      company: $e.find('header h5 a').text()," -- Investor Relations Deputy Director
  , "      starttime: $e.find('.experience-date-locale time:first-child').text()," -- "février 2015"
  , "      endtime: $e.find('.experience-date-locale time:nth-child(2)').text(),"  -- may be empty
  , "    })"
  , "  })"
  , "};"
  ]


-- [Object, Object, Object, Object]

jsEval :: Text -> Command
jsEval code = Command
  { commandId     = 1
  , commandMethod = "Runtime.evaluate"
  , commandParams =
      [ ("expression", A.String code)
      , ("returnByValue", A.Bool True)
      ]
  }
