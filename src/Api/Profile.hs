{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Profile where

import Data.Aeson                  ( FromJSON )
import Data.Int                    ( Int64 )
import Data.Maybe
import Data.Text                   ( Text )
import Data.Time                   ( getCurrentTime )
import Database.Persist.Postgresql ( Entity (..), (==.), entityKey, insert
                                   , selectFirst, toSqlKey )
import Servant

import Config                      ( App (..), Config (..) )
import Models
import Types

type ProfileAPI =
         "profiles" :> Capture "username" Username
                    :> Get '[JSON] (Entity User)

    :<|> "profiles" :> Capture "username" Username
                    :> "follow"
                    :> Capture "followerId" Int64
                    :> Post '[JSON] (Entity User)

profileServer :: ServerT ProfileAPI App
profileServer = getProfile :<|> followProfile

getProfile :: Username -> App (Entity User)
getProfile username = do
    maybeUser <- runDb $ selectFirst [UserUsername ==. username] []
    case maybeUser of
      Nothing -> throwError err404
      Just user -> return user

-- | While authentication is not supported, let's pass the followerId as an
-- argument as well.
followProfile :: Username -> Int64 -> App (Entity User)
followProfile username followerId = do
    maybeUser <- runDb $ selectFirst [UserUsername ==. username] []
    case maybeUser of
      Nothing -> throwError err404
      Just followee -> do
          runDb $ insert (UserFollower (entityKey followee) (toSqlKey followerId))
          return followee