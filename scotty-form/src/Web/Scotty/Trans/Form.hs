{-# OPTIONS_GHC -fno-warn-orphans -Wno-error=orphans #-}

module Web.Scotty.Trans.Form where

import Data.Bifunctor (first)
import UnliftIO (MonadUnliftIO)
import Data.Text (Text)
import Ditto.Backend
import Ditto.Core hiding (view)
import Ditto.Lucid
import Ditto.Types
import Lucid (HtmlT, ToHtml (toHtml))
import Lucid.Base (ToHtml (toHtmlRaw))
import Web.Scotty.Trans
import qualified Data.Text as T

instance MonadUnliftIO m => Environment (ActionT m) [Param] where
  environment formId = do
    pp <- pathParams
    qp <- queryParams
    fp <- formParams
    let !formId' = encodeFormId formId
    case filter (\(x, _) -> x == formId') (pp ++ qp ++ fp) of
      [] -> pure Missing
      xs -> pure (Found xs)

instance FormInput [Param] where
  type FileType [Param] = ()
  getInputStrings xs = fmap (T.unpack . snd) xs
  getInputFile _ = Left $ commonFormError $ (NoFileFound [("","No support for file uploads")] :: CommonFormError [Param])

instance FormError [Param] ScottyFormError where
  commonFormError = SFECommon

-- | the error case of running a 'ScottyForm'
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

encQP :: [(a, Text)] -> Text
encQP [] = ""
encQP xs = T.intercalate ", " (fmap snd xs)

-- | a @ditto@ formlet for @scotty@
type ScottyForm m a = Form (ActionT m) [Param] ScottyFormError (HtmlT (ActionT m) ()) a

ditto :: Monad m
  => ([(Text, Text)] -> view -> view) -- ^ wrap raw form html inside a <form> tag
  -> Text -- ^ form name prefix
  -> Form (ActionT m) [Param] err view a -- ^ the formlet
  -> ActionT m (Result err a, view)
ditto toForm prefix formlet = do
  dittoSingle toForm' prefix formlet
  where
  toForm' hidden view = toForm (("formname", prefix) : hidden) view

-- | a helpful wrapper around 'runForm'
dittoSingle
  :: Monad m
  => ([(Text, Text)] -> view -> view) -- ^ wrap raw form html inside a <form> tag
  -> Text -- ^ form name prefix
  -> Form (ActionT m) [Param] err view a -- ^ the formlet
  -> ActionT m (Result err a, view)
dittoSingle toForm prefix formlet = do
  (View viewf, res) <- runForm prefix formlet
  case res of
    Error errs -> pure (Error errs, toForm [] $ viewf errs)
    Ok (Proved _ unProved') -> pure (Ok unProved', toForm [] $ viewf [])

-- | create @\<form action=action method=\"GET\" enctype=\"application/xxx-form-urlencoded\"\>@
simpleDittoGET
  :: (Applicative f, Monad m)
  => Text -- ^ action
  -> Form (ActionT m) [Param] err (HtmlT f ()) b -- ^ formlet
  -> ActionT m (Result err b, HtmlT f ())
simpleDittoGET action form = ditto (formGenGET action) "ditto" form

-- | create @\<form action=action method=\"POST\" enctype=\"application/xxx-form-urlencoded\"\>@
simpleDittoPOST
  :: (Applicative f, Monad m)
  => Text -- ^ action
  -> Form (ActionT m) [Param] err (HtmlT f ()) b -- ^ formlet
  -> ActionT m (Result err b, HtmlT f ())
simpleDittoPOST action form = ditto (formGenPOST action) "ditto" form

-- | lift a function which parses strict @Text@ into a function which parses a @[Param]@
liftParser' :: (Text -> Either Text a) -> ([Param] -> Either ScottyFormError a)
liftParser' f [(_, x)] = first SFEParseError $ f x
liftParser' _ [] = Left SFEUnexpectedEmpty
liftParser' _ _ = Left SFEUnexpectedMultiple

-- | lift a function which parses strict @Text@ into a function which parses a @[Param]@
--
-- @
-- parserRead :: [Param] -> Either ScottyFormError Int
-- parserRead = liftParser' $ \t ->
--   maybe (Left "not an integer") Right (readMaybe @Int (T.unpack t))
-- @
liftParser :: (Text -> Either Text a) -> ([Param] -> Either ScottyFormError a)
liftParser = liftParser'
