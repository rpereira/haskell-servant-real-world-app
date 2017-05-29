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

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader

import Database.Persist.Sql   (SqlPersistT (..), runMigration, runSqlPool)

import Config

import Models.Article
import Models.Comment
import Models.Favorite
import Models.Tag
import Models.Tagging
import Models.User
import Models.UserFollower

-- | List of database migrations to perform.
migrations =
    [ migrateArticle
    , migrateComment
    , migrateFavorite
    , migrateUser
    , migrateUserFollower
    , migrateTag
    , migrateTagging
    ]

-- | Perform database migrations.
runMigrations :: SqlPersistT IO ()
runMigrations = forM_ migrations $ \m ->
    runMigration m

-- | Run database actions.
runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
