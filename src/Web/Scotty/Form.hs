{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Scotty.Form where

import Data.Text (Text)
import Ditto.Backend
import Ditto.Core hiding (view)
import Ditto.Lucid
import Ditto.Result
import Lucid (HtmlT, Html)
import Web.Scotty
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

instance FormError Text where
  type ErrorInputType Text = Text
  commonFormError = T.pack . (commonFormErrorStr T.unpack)

type ScottyForm a = Form ActionM Text Text (Html ()) a

reform :: (Monoid view)  
  => ([(Text, Text)] -> view -> view) -- ^ wrap raw form html inside a <form> tag
  -> Text -- ^ form name prefix
  -> Form ActionM Text err view a  -- ^ the formlet
  -> ActionM (Result err a, view)
reform toForm prefix formlet = do 
  reformSingle toForm' prefix formlet
  where
  toForm' hidden view = toForm (("formname", prefix) : hidden) view

reformSingle
  :: ([(Text, Text)] -> view -> view)
  -> Text
  -> Form ActionM Text err view a
  -> ActionM (Result err a, view)
reformSingle toForm prefix formlet = do
  qp <- params
  (View viewf, res') <- runForm (Environment $ env qp) (TL.fromStrict prefix) formlet
  res <- res'
  case res of
    Error errs -> pure (Error errs, toForm [] $ viewf errs)
    Ok (Proved _ unProved') -> pure (Ok unProved', toForm [] $ viewf [])
  where
  env :: [Param] -> FormId -> ActionM (Value Text)
  env qp' formId = do
    case lookup (TL.pack $ show formId) qp' of
      Nothing -> pure Missing
      Just x -> pure (Found $ TL.toStrict x)

simpleReformGET :: (Show b, Applicative f) 
  => Text
  -> Form ActionM Text err (HtmlT f ()) b 
  -> ActionM (Result err b, HtmlT f ())
simpleReformGET action form = reform (formGenGET action) "reform" form

simpleReformPOST :: (Show b, Applicative f) 
  => Text
  -> Form ActionM Text err (HtmlT f ()) b 
  -> ActionM (Result err b, HtmlT f ())
simpleReformPOST action form = reform (formGenPOST action) "reform" form

