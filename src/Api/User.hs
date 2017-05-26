{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Control.Monad.IO.Class      ( liftIO )
import Data.Aeson                  ( FromJSON )
import Data.Int                    ( Int64 )
import Data.Maybe
import Data.Text                   ( Text )
import Data.Time                   ( getCurrentTime )
import Database.Persist.Postgresql ( Entity (..), (==.) , insert, replace
                                   , selectFirst, toSqlKey )
import Servant

import Config                      ( App (..), Config (..) )
import Models
import Types

type UserAPI = "users"
            :> Capture "id" Int64
            :> ReqBody '[JSON] (Entity User)
            :> Put '[] ()

registrarionServer :: ServerT UserAPI App
registrarionServer = updateUser

updateUser :: Int64 -> Entity User -> App ()
updateUser userId user = do
    let key = toSqlKey userId
    replacement <- runDb $ replace key user
    case replacement of
      Nothing -> throwError err409 { errBody = "Username already taken" }
      Just x -> return ()
