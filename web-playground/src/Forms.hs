module Forms
  ( module Forms
  , module X
  ) where

import           ClassyPrelude
import qualified Data.Text           as T
import           Text.Digestive.Lucid.Html5 as X (errorList, inputText, label)
import           Text.Digestive      as X hiding (Path, file, text)
import           Text.Digestive      as X (text)
import           Util                hiding (text)
import           Web.Spock.Digestive as X (runForm)

checkEmail :: Monad m => Form HTML m Text  -> Form HTML m Text
checkEmail = check "Not a valid email address"
  (isJust . T.find (== '@'))

checktest :: Monad m => Form HTML m Text  -> Form HTML m Text
checktest = check "should be zedze" (=="zedze")

mkForm :: Term [Attribute] result => [Attribute] -> result
mkForm p = form_ (class_ "form-horizontal" : p)

textField :: Text -> View HTML -> HTML
textField str view = do
  let err = case errors str view of {[] -> False; _ -> True}
  div_ [class_ ("form-group" <> if err then " has-error" else "")] $ do
    with (label     str view (toHtml str)) [class_ "col-sm-2 control-label"]
    div_ [class_ "col-sm-10"] $ do
      with (inputText str view) [class_ " form-control"]
      when err $ span_ [class_ "help-block"] $ errorList str view

usernameFormlet :: Monad m => Maybe Text -> Form HTML m Text
usernameFormlet mTxt =
  validate (minMaxLen (3, 12)) (text mTxt)

passwordFormlet :: Monad m => Maybe Text -> Form HTML m Text
passwordFormlet mTxt =
  validate (minMaxLen (6, 40)) (text mTxt)

emailFormlet :: Monad m => Maybe Text -> Form HTML m Text
emailFormlet mTxt =
  check "Not a valid email address" (isJust . T.find (== '@')) $
  validate (minMaxLen(4, 50)) (text mTxt)


minMaxLen :: (Int, Int) -> Text -> Result HTML Text
minMaxLen (minLen, maxLen) t =
  if len >= minLen && len <= maxLen
  then Success stripped
  else Error $ toHtml $ "Must be longer than " ++ show minLen ++ " and shorter than " ++ show maxLen ++ " characters"
  where
    stripped = T.strip t
    len = T.length stripped


data AForm model p m = Aform
  { formData :: Maybe model
  , formDef  :: Maybe model -> Form HTML m model
  , formView :: View HTML -> HTML
  }



-- form = FormMeta
--   { fm_method = FormPost
--   , fm_target = root
--   , fm_elements = []
--   , fm_submitText = "OK"
--   }

-- data FormAction = FormPost | FormGet
-- data FormElementCfg = TextField | PasswordField
-- data FormElement = FormElement
--   { fe_name :: Text
--   , fe_label :: Maybe Text
--   , fe_base :: FormElementCfg
--   }
-- data FormMeta x = FormMeta
--   { fm_method :: FormAction
--   , fm_target :: Path x
--   , fm_elements :: [FormElement]
--   , fm_submitText :: Text
--   }
