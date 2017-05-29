{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Tag where

import Database.Persist.Postgresql (Entity (..), selectList)
import Servant

import Config                      (App (..))
import DB                          (runDb)
import Models.Tag
import Types

type TagAPI = "tags" :> Get '[JSON] (TagList [Entity Tag])

tagServer :: ServerT TagAPI App
tagServer = getTags

getTags :: App (TagList [Entity Tag])
getTags = do
    tags <- runDb (selectList [] [])
    return $ TagList tags
