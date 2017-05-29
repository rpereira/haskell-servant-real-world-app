{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Models where

import Control.Monad.Reader
import Database.Persist.Sql

import Models.Article
import Models.Comment
import Models.Favorite
import Models.User
import Models.UserFollower
import Models.Tag
import Models.Tagging
import Config
import Types

doMigrations :: SqlPersistT IO ()
doMigrations = do
    runMigration migrateArticle
    runMigration migrateComment
    runMigration migrateFavorite
    runMigration migrateUser
    runMigration migrateUserFollower
    runMigration migrateTag
    runMigration migrateTagging

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
