{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Registration where

import Control.Monad.IO.Class      (liftIO)
import Data.Int                    (Int64)
import Data.Maybe
import Data.Text                   (Text)
import Data.Time                   (getCurrentTime)
import Database.Persist.Postgresql (Entity (..), fromSqlKey, insert, (==.))
import Servant

import Config                      (App (..), Config (..))
import DB                          (runDb)
import Models.User
import Types

type RegistrationAPI = "users"
                    :> ReqBody '[JSON] (Usr NewUser)
                    :> PostCreated '[JSON] Int64

registrarionServer :: ServerT RegistrationAPI App
registrarionServer = createUser

-- TODO: handle case when an already existing user is given
createUser :: Usr NewUser -> App Int64
createUser (Usr u) = do
    time <- liftIO getCurrentTime
    newUser <- runDb $
        insert (User (username u) (email u) Nothing Nothing time Nothing)
    return $ fromSqlKey newUser
