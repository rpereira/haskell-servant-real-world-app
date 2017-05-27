{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Control.Monad.Reader
import Data.Aeson           ( FromJSON, ToJSON )
import Data.Text            ( Text )
import Data.Time            ( UTCTime )
import Database.Persist.Sql
import Database.Persist.TH  ( mkMigrate, mkPersist, persistLowerCase, share
                            , sqlSettings )
import GHC.Generics         ( Generic )

import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json sql=users
    username  Text
    email     Text
    bio       Text Maybe default=NULL
    image     Text Maybe default=NULL
    createdAt UTCTime default=now()
    updatedAt UTCTime Maybe default=NULL

    UniqueUser username email
    deriving Show

UserFollower json sql=user_followers
    userId     UserId
    followerId UserId

    Primary userId followerId

    deriving Show

Article json sql=articles
    slug        Text
    title       Text
    description Text
    body        Text
    createdAt   UTCTime default=now()
    updatedAt   UTCTime Maybe default=NULL
    userId      UserId

    UniqueSlug slug
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
