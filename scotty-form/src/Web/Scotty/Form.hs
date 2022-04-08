{-# LANGUAGE 
    OverloadedStrings
  , MultiParamTypeClasses
  , FlexibleInstances
  , TypeFamilies
  , BangPatterns
#-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Scotty.Form where

import Data.Text (Text)
import Ditto.Core hiding (view)
import Ditto.Lucid
import Ditto.Types
import Lucid (HtmlT, Html, ToHtml (toHtml))
import Web.Scotty
import Ditto.Backend
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Bifunctor (first)
import Lucid.Base (ToHtml (toHtmlRaw))

instance Environment ActionM [Param] where
  environment formId = do
    qp <- params
    let !formId' = TL.fromStrict $ encodeFormId formId
    case filter (\(x,_) -> x == formId') qp of
      [] -> pure Missing
      xs -> pure (Found xs)

instance FormInput [Param] where
  type FileType [Param] = ()
  getInputStrings xs = fmap (TL.unpack . snd) xs
  getInputFile _ = Left $ commonFormError $ (NoFileFound [("","No support for file uploads")] :: CommonFormError [Param])

instance FormError [Param] ScottyFormError where
  commonFormError = SFECommon

data ScottyFormError 
  = SFECommon (CommonFormError [Param])
  | SFEUnexpectedEmpty
  | SFEUnexpectedMultiple
  | SFEParseError Text

instance ToHtml ScottyFormError where
  toHtml (SFECommon ps) = toHtml $ commonFormErrorText encQP ps
  toHtml (SFEParseError t) = toHtml t
  toHtml SFEUnexpectedEmpty = "Unexpected empty query param list"
  toHtml SFEUnexpectedMultiple = "Unexpected multiple query param list"
  toHtmlRaw (SFECommon ps) = toHtmlRaw $ commonFormErrorText encQP ps
  toHtmlRaw (SFEParseError t) = toHtmlRaw t
  toHtmlRaw SFEUnexpectedEmpty = "Unexpected empty query param list"
  toHtmlRaw SFEUnexpectedMultiple = "Unexpected multiple query param list"

encQP :: [(a, TL.Text)] -> Text
encQP [] = ""
encQP xs = T.intercalate ", " (fmap (TL.toStrict . snd) xs)

type ScottyForm a = Form ActionM [Param] ScottyFormError (Html ()) a

ditto :: (Monoid view)  
  => ([(Text, Text)] -> view -> view) -- ^ wrap raw form html inside a <form> tag
  -> Text -- ^ form name prefix
  -> Form ActionM [Param] err view a  -- ^ the formlet
  -> ActionM (Result err a, view)
ditto toForm prefix formlet = do 
  dittoSingle toForm' prefix formlet
  where
  toForm' hidden view = toForm (("formname", prefix) : hidden) view

dittoSingle
  :: ([(Text, Text)] -> view -> view)
  -> Text
  -> Form ActionM [Param] err view a
  -> ActionM (Result err a, view)
dittoSingle toForm prefix formlet = do
  (View viewf, res) <- runForm prefix formlet
  case res of
    Error errs -> pure (Error errs, toForm [] $ viewf errs)
    Ok (Proved _ unProved') -> pure (Ok unProved', toForm [] $ viewf [])

simpleDittoGET :: (Applicative f) 
  => Text
  -> Form ActionM [Param] err (HtmlT f ()) b 
  -> ActionM (Result err b, HtmlT f ())
simpleDittoGET action form = ditto (formGenGET action) "ditto" form

simpleDittoPOST :: (Applicative f) 
  => Text
  -> Form ActionM [Param] err (HtmlT f ()) b 
  -> ActionM (Result err b, HtmlT f ())
simpleDittoPOST action form = ditto (formGenPOST action) "ditto" form

-- | lift a function which parses @Text@ into a function which parses a @[Param]@
liftParser' :: (Text -> Either Text a) -> ([Param] -> Either ScottyFormError a)
liftParser' f [(_,x)] = first SFEParseError $ f (TL.toStrict x)
liftParser' _ [] = Left SFEUnexpectedEmpty
liftParser' _ _ = Left SFEUnexpectedMultiple

-- | lift a function which parses @Text@ into a function which parses a @[Param]@
liftParser :: (TL.Text -> Either TL.Text a) -> ([Param] -> Either ScottyFormError a)
liftParser f [(_,x)] = first (SFEParseError . TL.toStrict) $ f x
liftParser _ [] = Left SFEUnexpectedEmpty
liftParser _ _ = Left SFEUnexpectedMultiple
