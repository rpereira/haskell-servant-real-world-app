{-# LANGUAGE DataKinds       #-}
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
                :> Get '[JSON] (Entity User)

profileServer :: ServerT ProfileAPI App
profileServer = getProfile

getProfile :: Username -> App (Entity User)
getProfile username = do
    maybeUser <- runDb $ selectFirst [UserUsername ==. username] []
    case maybeUser of
      Nothing -> throwError err404
      Just user -> return user
