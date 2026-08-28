module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Web.Scotty
import Web.Scotty.Form
import Ditto.Lucid
import Ditto.Lucid.Named
import Lucid
import Web.PathPieces
import Ditto
import Text.Read (readMaybe)
import qualified Data.List.NonEmpty as NonEmpty

data User = User
  { userAge :: Int
  , userId :: Int
  , userName :: Text
  , userCountry :: Country
  }
  deriving Show

data Country = C Char Char
  deriving (Eq, Show)

instance PathPiece Country where
  fromPathPiece = either (const Nothing) Just . parserCountry
  toPathPiece (C c1 c2) = T.pack [c1, c2]

parserCountry :: Text -> Either Text Country
parserCountry t = case T.splitAt 2 t of
  (ct, remain)
    | T.null remain -> case T.unpack ct of
      [c1,c2] -> Right $ C c1 c2
      _ -> Left "input too short"
    | otherwise -> Left "input too long"

countries :: [Country]
countries = do
  c1 <- ['A'..'Z']
  c2 <- ['A'..'Z']
  pure $ C c1 c2

formUser :: ScottyForm User
formUser = do
  childErrorList
  view $ h1_ [] "User form"
  label "Age" "age"
  userAge <- inputInt parserRead "age" 18
  userId <- pure 0 -- imagine this is maybe some DB effect
  label "Name" "name"
  userName <- inputText (liftParser' Right) "name" ""
  label "Country" "country"
  userCountry <- select "country"
    (NonEmpty.fromList $ fmap (\x -> (x, toHtml $ toPathPiece x))
      countries
    )
    (liftParser' parserCountry)
    (== C 'U' 'S')
  submit
  pure User{..}

submit :: ScottyForm ()
submit = br *> buttonSubmit (const (Right ())) "submit" () ("Submit" :: Text) *> pure ()

parserRead :: [Param] -> Either ScottyFormError Int
parserRead = liftParser' $ \t ->
  maybe (Left "not an integer") Right (readMaybe @Int (T.unpack t))

sakura :: Html ()
sakura = link_ [rel_ "stylesheet", href_ "https://unpkg.com/sakura.css/css/sakura.css", type_ "text/css"]

main :: IO ()
main = scotty 3000 $
  get "/user" $ do
    (res, html') <- simpleDittoGET "/user" formUser
    html $ renderText $ do
      sakura
      html'
      toHtml $ case res of
        Ok user -> toHtml $ show user
        Error {} -> mempty
