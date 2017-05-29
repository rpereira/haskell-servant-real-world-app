{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Tag where

import Data.Aeson                  ( FromJSON )
import Data.Int                    ( Int64 )
import Data.Maybe
import Data.Text                   ( Text )
import Data.Time                   ( getCurrentTime )
import Database.Persist.Postgresql ( Entity (..), (==.), deleteWhere, entityKey
                                   , insert, selectList, toSqlKey )
import Servant

import Config                      ( App (..), Config (..) )
import Models                      ( runDb )
import Models.Tag
import Types

type TagAPI = "tags" :> Get '[JSON] (TagList [Entity Tag])

tagServer :: ServerT TagAPI App
tagServer = getTags

getTags :: App (TagList [Entity Tag])
getTags = do
    tags <- runDb (selectList [] [])
    return $ TagList tags
