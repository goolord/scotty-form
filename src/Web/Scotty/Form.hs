{-# language OverloadedStrings #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Scotty.Form where

import Data.Text (Text)
import Ditto.Core hiding (view)
import Ditto.Lucid
import Ditto.Types
import Lucid (HtmlT, Html)
import Web.Scotty
import Ditto.Backend
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

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

instance FormError [Param] Text where
  commonFormError e = commonFormErrorText encQP e
    where
    encQP [] = ""
    encQP xs = T.intercalate ", " (fmap (TL.toStrict . snd) xs)

type ScottyForm a = Form ActionM [Param] Text (Html ()) a

reform :: (Monoid view)  
  => ([(Text, Text)] -> view -> view) -- ^ wrap raw form html inside a <form> tag
  -> Text -- ^ form name prefix
  -> Form ActionM [Param] err view a  -- ^ the formlet
  -> ActionM (Result err a, view)
reform toForm prefix formlet = do 
  reformSingle toForm' prefix formlet
  where
  toForm' hidden view = toForm (("formname", prefix) : hidden) view

reformSingle
  :: ([(Text, Text)] -> view -> view)
  -> Text
  -> Form ActionM [Param] err view a
  -> ActionM (Result err a, view)
reformSingle toForm prefix formlet = do
  (View viewf, res) <- runForm prefix formlet
  case res of
    Error errs -> pure (Error errs, toForm [] $ viewf errs)
    Ok (Proved _ unProved') -> pure (Ok unProved', toForm [] $ viewf [])

simpleReformGET :: (Applicative f) 
  => Text
  -> Form ActionM [Param] err (HtmlT f ()) b 
  -> ActionM (Result err b, HtmlT f ())
simpleReformGET action form = reform (formGenGET action) "reform" form

simpleReformPOST :: (Applicative f) 
  => Text
  -> Form ActionM [Param] err (HtmlT f ()) b 
  -> ActionM (Result err b, HtmlT f ())
simpleReformPOST action form = reform (formGenPOST action) "reform" form

-- | lift a function which parses @Text@ into a function which parses a @[Param]@
liftParser :: (Text -> Either Text a) -> ([Param] -> Either Text a)
liftParser f [(_,x)] = f (TL.toStrict x)
liftParser _ [] = Left "Unexpected empty query param list"
liftParser _ _ = Left "Unexpected multiple query param list"
