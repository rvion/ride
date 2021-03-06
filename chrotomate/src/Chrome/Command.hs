{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Chrome.Command where
import           Jetpack
-- import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, Value(..))
-- import qualified Data.Aeson           as A
-- import           Data.ByteString.Lazy (ByteString)
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.Map             as M
-- import           Data.Text            (Text)
-- import qualified Data.Text            as T
-- type LBS = ByteString

data Command = Command
  { commandId     :: Int
  , commandMethod :: TText
  , commandParams :: [(TText, JsValue)]
  } deriving (Show)

instance ToJSON Command where
  toJSON cmd = js_object
    [ "id"     .= commandId cmd
    , "method" .= commandMethod cmd
    , "params" .= map_fromList (commandParams cmd)
    ]

data CommandResult = CommandResult
  { resultId    :: Int
  , resultValue :: JsValue
  } deriving (Show)

instance FromJSON CommandResult where
  parseJSON (JsObject v) = CommandResult
    <$> v .: "id"
    <*> (v .: "result" >>= (.: "result") >>= (.: "value"))-- .: "result" .: "value"


-- Chrome Remote actions
goToPage :: TText ->  Command
goToPage page = Command
  { commandId     = 1
  , commandMethod = "Page.navigate"
  , commandParams = [("url", js_mk'String page)]
  }

searchName :: TText ->  Command
searchName name = jsEval (go "#main-search-box" name)
  where
    go :: TText -> TText -> TText
    go inpt txt = t_concat ["$(\"", inpt,"\").val(\"", txt,"\");$(\"#global-search\").submit()"]

clickOnFirstResult :: Command
clickOnFirstResult = jsEval
    "window.location.href = $('#results>li:first-child a.main-headline').attr('href');"

getExperiences :: Command
getExperiences = jsEval getExperiences'

getExperiences'  :: TText
getExperiences' = t_concat
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

jsEval :: TText -> Command
jsEval code = Command
  { commandId     = 1
  , commandMethod = "Runtime.evaluate"
  , commandParams =
      [ ("expression", js_mk'String code)
      , ("returnByValue", js_mk'Bool True)
      ]
  }
