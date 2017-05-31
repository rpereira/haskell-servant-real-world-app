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

type RegistrationAPI = "users"
                    :> ReqBody '[JSON] (Usr NewUser)
                    :> PostCreated '[JSON] (Usr (Maybe AuthUser))

registrationServer :: ServerT RegistrationAPI App
registrationServer = createUser

userToAuth :: Entity User -> AuthUser
userToAuth (Entity k User {..}) =
    AuthUser userUsername userUsername userEmail userBio userImage

createUser :: Usr NewUser -> App (Usr (Maybe AuthUser))
createUser (Usr u) = do
    exists <- runDb $ selectFirst [UserUsername ==. username u] []
    case exists of
      Just _ -> throwError err409 { errBody = "Username already exists"  }
      Nothing -> do
          time <- liftIO getCurrentTime
          newUser <- runDb $
              insert (User (username u) (email u) Nothing Nothing time Nothing)
          user <- runDb $ selectFirst [UserId ==. newUser] []
          return . Usr $ fmap userToAuth user
