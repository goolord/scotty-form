module Web.Scotty.Form
  ( Trans.ScottyFormError(..)
  , encQP
  , ScottyForm
  , ditto
  , dittoSingle
  , simpleDittoGET
  , simpleDittoPOST
  , liftParser'
  , liftParser
  ) where

import Data.Text (Text)
import Ditto.Core hiding (view)
import Ditto.Types
import Web.Scotty
import qualified Web.Scotty.Trans.Form as Trans
import qualified Data.Text.Lazy as TL
import Lucid

encQP :: [(a, TL.Text)] -> Text
encQP = Trans.encQP

-- | a @ditto@ formlet for @scotty@
type ScottyForm a = Form ActionM [Param] Trans.ScottyFormError (Html ()) a

ditto :: (Monoid view)
  => ([(Text, Text)] -> view -> view) -- ^ wrap raw form html inside a <form> tag
  -> Text -- ^ form name prefix
  -> Form ActionM [Param] err view a -- ^ the formlet
  -> ActionM (Result err a, view)
ditto = Trans.ditto

-- | a helpful wrapper around 'runForm'
dittoSingle
  :: ([(Text, Text)] -> view -> view) -- ^ wrap raw form html inside a <form> tag
  -> Text -- ^ form name prefix
  -> Form ActionM [Param] err view a -- ^ the formlet
  -> ActionM (Result err a, view)
dittoSingle = Trans.dittoSingle

-- | create @\<form action=action method=\"GET\" enctype=\"application/xxx-form-urlencoded\"\>@
simpleDittoGET :: (Applicative f)
  => Text -- ^ action
  -> Form ActionM [Param] err (HtmlT f ()) b -- ^ formlet
  -> ActionM (Result err b, HtmlT f ())
simpleDittoGET = Trans.simpleDittoGET

-- | create @\<form action=action method=\"POST\" enctype=\"application/xxx-form-urlencoded\"\>@
simpleDittoPOST :: (Applicative f)
  => Text -- ^ action
  -> Form ActionM [Param] err (HtmlT f ()) b -- ^ formlet
  -> ActionM (Result err b, HtmlT f ())
simpleDittoPOST = Trans.simpleDittoPOST

-- | lift a function which parses strict @Text@ into a function which parses a @[Param]@
liftParser' :: (Text -> Either Text a) -> ([Param] -> Either Trans.ScottyFormError a)
liftParser' = Trans.liftParser'

-- | lift a function which parses lazy @Text@ into a function which parses a @[Param]@
-- e.g.
--
-- @
-- parserRead :: Read a => [Param] -> Either ScottyFormError a
-- parserRead = liftParser readEither
-- @
liftParser :: (TL.Text -> Either TL.Text a) -> ([Param] -> Either Trans.ScottyFormError a)
liftParser = Trans.liftParser
