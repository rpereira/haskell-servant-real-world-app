{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module DB
    ( runDb
    , runMigrations
    ) where

import Control.Monad.IO.Class      ( liftIO )
import Control.Monad.Reader

import Database.Persist.Sql (SqlPersistT(..), runMigration, runSqlPool)

import Config

import Models.Article
import Models.Comment
import Models.Favorite
import Models.User
import Models.UserFollower
import Models.Tag
import Models.Tagging

-- | Perform database migrations.
runMigrations :: SqlPersistT IO ()
runMigrations = do
    runMigration migrateArticle
    runMigration migrateComment
    runMigration migrateFavorite
    runMigration migrateUser
    runMigration migrateUserFollower
    runMigration migrateTag
    runMigration migrateTagging

-- | Run database actions.
runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
