{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Lib
    ( startApp
    ) where

import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Data.Foldable (foldMap)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Lucid
import Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html
import LibFetch (fetchInterNews)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]
      :<|> "news" :> Get '[JSON, HTML] [InterNews]

data InterNews = InterNews
  { headline :: String } deriving (Eq, Show, Generic)

instance ToJSON InterNews

instance ToHtml InterNews where
  toHtml n =
    tr_ $ do
      td_ (toHtml $ headline n)

  toHtmlRaw = toHtml

instance ToHtml [InterNews] where
  toHtml ns = table_ $ do
    tr_ $ do
      th_ "Headline"

    foldMap toHtml ns
  
  toHtmlRaw = toHtml

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users
    :<|> newsFromString
    
newsFromString :: EitherT ServantErr IO [InterNews]
newsFromString = do
  headlines <- liftIO fetchInterNews
  let ls = map (\s -> InterNews s) headlines
  return ls

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
