{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Api.User where

import Data.Aeson                  ( FromJSON )
import Data.Int                    ( Int64 )
import Data.Map                    ( singleton )
import Data.Text                   ( Text )
import Database.Persist.Postgresql ( Entity (..), fromSqlKey, insert, selectList
                                   , selectFirst, (==.) )
import Servant
import qualified Web.JWT as JWT

import Config                      ( App (..), Config (..) )
import Models
import Types                       -- ( AuthUser, NewUser, Login, Username )

type UserAPI =
    -- | Registration
       "users" :> ReqBody '[JSON] (User NewUser)
               :> Post '[JSON] (User (Maybe AuthUser))

    -- | Authentication
  -- :<|> "users" :> "login"
               -- :> ReqBody '[JSON] User

userServer :: ServerT UserAPI App
userServer = registerUser

registerUser :: User NewUser -> App (User (Maybe AuthUser))
registerUser (User newUser) = do
    addedUser <- createUser newUser
    return $ User $ fmap userToAuthUser addedUser

createUser :: User NewUser -> App (User (Maybe AuthUser))
createUser (User newUser) = do
  newUser <- runDb (insert (User (userName p) (userAge p)))
  return $ fromSqlKey newUser

token :: Username -> JWT.JSON
token username =
  JWT.encodeSigned
    JWT.HS256
    secret
    JWT.def
    {JWT.unregisteredClaims = singleton "username" . toJSON $ User username}

userToAuthUser :: User -> AuthUser
userToAuthUser User {..} =
    let aEmail = email
        aToken = token usrUsername
        aUsername = usrUsername
        aBio = usrBio
        aImage = usrImage
     in AuthUser {..}
