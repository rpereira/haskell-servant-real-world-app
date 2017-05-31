{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Api.Profile where

import Data.Int                    (Int64)
import Data.Time                   (getCurrentTime)
import Database.Persist.Postgresql (Entity (..), deleteWhere, entityKey, insert,
                                    selectFirst, toSqlKey, (==.))
import Servant

import Config                      (App (..))
import DB                          (runDb)
import Models.User
import Models.UserFollower
import Types

type ProfileAPI =
         "profiles" :> Capture "username" Username
                    :> Get '[JSON] (Profile (Maybe UserProfile))

    :<|> "profiles" :> Capture "username" Username
                    :> "follow"
                    :> Capture "followerId" Int64
                    :> Post '[JSON] (Profile (Entity User))

    :<|> "profiles" :> Capture "username" Username
                    :> "follow"
                    :> Capture "followerId" Int64
                    :> Delete '[JSON] (Profile (Entity User))

profileServer :: ServerT ProfileAPI App
profileServer =
         getProfile
    :<|> followProfile
    :<|> unfollowProfile

userToProfile :: Entity User -> UserProfile
userToProfile (Entity k User {..}) =
    UserProfile userUsername userBio userImage

-- | TODO: return err404 instead of Maybe?
--     case maybeUser of
--       Nothing   -> throwError err404
--       Just user -> return . Profile $ userToProfile (Entity user)
getProfile :: Username -> App (Profile (Maybe UserProfile))
getProfile username = do
    maybeUser <- runDb $ selectFirst [UserUsername ==. username] []
    let profile = fmap userToProfile maybeUser
    return $ Profile profile

-- | While authentication is not supported, let's pass the followerId as an
-- argument as well.
followProfile :: Username -> Int64 -> App (Profile (Entity User))
followProfile username followerId = do
    maybeUser <- runDb $ selectFirst [UserUsername ==. username] []
    case maybeUser of
      Nothing -> throwError err404
      Just followee -> do
          runDb $ insert (UserFollower (entityKey followee) (toSqlKey followerId))
          return $ Profile followee

unfollowProfile :: Username -> Int64 -> App (Profile (Entity User))
unfollowProfile username followerId = do
    maybeUser <- runDb $ selectFirst [UserUsername ==. username] []
    case maybeUser of
      Nothing -> throwError err404
      Just followee -> do
          runDb $ deleteWhere [ UserFollowerUserId ==. entityKey followee
                              , UserFollowerFollowerId ==. toSqlKey followerId ]
          return $ Profile followee
