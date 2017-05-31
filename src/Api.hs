{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api ( app ) where

import           Control.Monad.Except
import           Control.Monad.Reader             (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Aeson                       (Result (..), fromJSON,
                                                   toJSON)
import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeUtf8)
import           Database.Persist.Postgresql      (Entity (..), deleteWhere,
                                                   entityKey, insert,
                                                   selectFirst, toSqlKey, (==.))
import           Network.Wai
import           Servant
import           Servant.API
import           Servant.Server.Experimental.Auth
import qualified Web.JWT                          as JWT

import           Config                           (App (..), Config (..))
import           DB                               (runDb)

import           Api.Article
import           Api.Comment
import           Api.Favorite
import           Api.Profile
import           Api.Registration
import           Api.Tag

import           Models.User
import           Types

type API = RegistrationAPI
      :<|> ProfileAPI
      :<|> ArticleAPI
      :<|> TagAPI
      :<|> CommentAPI
      :<|> FavoriteAPI

-- | Combinate all endpoints to be served.
server :: ServerT API App
server = registrationServer
    :<|> profileServer
    :<|> articleServer
    :<|> tagServer
    :<|> commentServer
    :<|> favoriteServer

appApi :: Proxy API
appApi = Proxy

-- | Converts 'App' monad into the @ExceptT ServantErr IO@ monad that Servant's
-- 'enter' function needs in order to run the application.  The ':~>' type is a
-- natural transformation, or, in non-category theory terms, a function that
-- converts two type constructors without looking at the values in the types.
convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

-- | Tell Servant how to run the 'App' monad with the 'server' function.
appToServer :: Config -> Server API
appToServer cfg = enter (convertApp cfg) server

app :: Config -> Application
app cfg = serveWithContext appApi serverAuthContext (appToServer cfg)
    where
        serverAuthContext :: Context (AuthHandler Request (Entity User) ': AuthHandler Request (Maybe (Entity User)) ': '[])
        serverAuthContext = authHandler :. authOptionalHandler :. EmptyContext

--------------------------------------------------------------------------------
--  Authentication

-- FIXME Secret in source.
secret :: JWT.Secret
secret = JWT.secret "unsafePerformIO"

type instance AuthServerData (AuthProtect "JWT") = Entity User
type instance AuthServerData (AuthProtect "JWT-optional") = (Maybe (Entity User))

authOptionalHandler :: AuthHandler Request (Maybe (Entity User))
authOptionalHandler =
    let handler req =
            case lookup "Authorization" (requestHeaders req) of
              Nothing -> return Nothing
              Just token ->
                  case decodeToken token of
                    Nothing -> return Nothing
                    Just username -> runDb $ selectFirst [UserUsername ==. username] []
    in mkAuthHandler handler

-- FIXME Too nested.
authHandler :: AuthHandler Request (Entity User)
authHandler =
    let handler req =
            case lookup "Authorization" (requestHeaders req) of
              Nothing -> throwError err401 { errBody = "Missing 'Authorization' header" }
              Just token ->
                  case decodeToken token of
                    Nothing -> throwError err401 { errBody = "Bad 'Authorization' token" }
                    Just username -> do
                        usr <- runDb $ selectFirst [UserUsername ==. username] []
                        case usr of
                          Nothing -> throwError err401 { errBody = "User doesn't exist" }
                          Just usr -> return usr
    in mkAuthHandler handler

decodeToken :: BS.ByteString -> Maybe Username
decodeToken auth =
  -- TODO Maybe we should use safe version of decodeUtf8.
  let auth' = decodeUtf8 auth
      (_, token) = T.splitAt (T.length "Token ") auth'
      jwt = JWT.decodeAndVerifySignature secret token
      json =
        Map.lookup "username" =<<
        fmap (JWT.unregisteredClaims . JWT.claims) jwt
      username = fmap fromJSON json
  in case username of
        Just (Success (Usr x)) -> Just x
        _                      -> Nothing

token :: Username -> JWT.JSON
token username =
  JWT.encodeSigned
    JWT.HS256
    secret
    JWT.def
    {JWT.unregisteredClaims = Map.singleton "username" . toJSON $ Usr username}
