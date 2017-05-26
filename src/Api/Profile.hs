{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Api.Profile where

import Data.Aeson                  ( FromJSON )
import Data.Int                    ( Int64 )
import Data.Maybe
import Data.Text                   ( Text )
import Data.Time                   ( getCurrentTime )
import Database.Persist.Postgresql ( Entity (..), fromSqlKey, insert, selectList
                                   , selectFirst, (==.) )
import Servant

import Config                      ( App (..), Config (..) )
import Models
import Types

type ProfileAPI = "profiles"
                :> Capture "username" Username
                :> Get '[JSON] (Profile (Maybe UserProfile))

profileServer :: ServerT ProfileAPI App
profileServer = getProfile

getProfile :: Username -> App (Profile (Maybe UserProfile))
getProfile username = do
    maybeUser <- runDb $ selectFirst [UserUsername ==. username] []
    case maybeUser of
      Nothing -> throwError err404
      Just user -> do
          let profile = fmap (userToProfile False) user
          return $ Profile profile

userToProfile :: Bool -> User -> UserProfile
userToProfile follows User {..} =
    UserProfile usrUsername usrBio usrImage follows
