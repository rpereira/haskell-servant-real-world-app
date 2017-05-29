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

import Models.Article
import Models.User
import Models.UserFollower
import Models.Tag
import Models.Tagging
import Config
import Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Comment json sql=comments
    body      Text
    createdAt UTCTime default=now()
    updatedAt UTCTime Maybe default=NULL
    userId    UserId
    articleId ArticleId

    deriving Show

Favorite json sql=favorits
    userId    UserId
    articleId ArticleId

    Primary userId articleId
    UniqueFavorite userId articleId
|]

doMigrations :: SqlPersistT IO ()
doMigrations = do
    runMigration migrateAll
    runMigration migrateArticle
    runMigration migrateUser
    runMigration migrateUserFollower
    runMigration migrateTag
    runMigration migrateTagging

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
