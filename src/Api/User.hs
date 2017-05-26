{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Control.Monad.IO.Class      ( liftIO )
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

type UserAPI = "user"
            :> ReqBody '[JSON] UpdateUser
            :> PostCreated '[JSON] Int64

userServer :: ServerT UserAPI App
userServer = updateUser

updateUser :: UpdateUser -> User
updateUser (User update) = do
    time <- liftIO getCurrentTime
    newUser <- runDb $
        insert (User (username p) (email p) Nothing Nothing time Nothing)
    return $ fromSqlKey newUser
