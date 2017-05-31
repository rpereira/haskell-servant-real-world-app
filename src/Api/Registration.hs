{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Registration where

import Control.Monad.IO.Class      (liftIO)
import Data.Int                    (Int64)
import Data.Maybe
import Data.Text                   (Text)
import Data.Time                   (getCurrentTime)
import Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                    selectFirst, (==.))
import Servant

import Config                      (App (..), Config (..))
import DB                          (runDb)
import Models.User
import Types

type RegistrationAPI =
         "users" :> ReqBody '[JSON] (Usr NewUser)
                 :> PostCreated '[JSON] (Usr (Maybe AuthUser))

    :<|> "users" :> "login"
                 :> ReqBody '[JSON] (Usr Login)
                 :> Post '[JSON] (Usr (Maybe AuthUser))

registrationServer :: ServerT RegistrationAPI App
registrationServer = createUser :<|> login

userToAuth :: Entity User -> AuthUser
userToAuth (Entity k User {..}) =
    AuthUser userUsername userUsername userEmail userBio userImage

createUser :: Usr NewUser -> App (Usr (Maybe AuthUser))
createUser (Usr u) = do
    exists <- runDb $ selectFirst [UserUsername ==. nusUsername u] []
    case exists of
      Just _ -> throwError err409 { errBody = "Username already exists"  }
      Nothing -> do
          time <- liftIO getCurrentTime
          newUser <- runDb $
              insert (User (nusUsername u) (nusEmail u) (nusPassword u) Nothing Nothing time Nothing)
          user <- runDb $ selectFirst [UserId ==. newUser] []
          return . Usr $ fmap userToAuth user

login :: Usr Login -> App (Usr (Maybe AuthUser))
login (Usr (Login email password)) = do
    maybeUser <- runDb $
        selectFirst [UserEmail ==. email, UserPassword ==. password] []
    case maybeUser of
      Nothing -> throwError err401 { errBody = "Incorrect username or password" }
      Just user -> return . Usr $ fmap userToAuth maybeUser
