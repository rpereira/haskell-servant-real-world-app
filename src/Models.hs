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

import Models.User
import Models.UserFollower
import Config
import Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Article json sql=articles
    slug        Slug
    title       Text
    description Text
    body        Text
    createdAt   UTCTime default=now()
    updatedAt   UTCTime Maybe default=NULL
    userId      UserId

    UniqueSlug slug

Tag json sql=tags
    name Text
    UniqueName name
    deriving Show

Tagging json sql=taggings
    articleId ArticleId
    tagId TagId

    Primary articleId tagId
    UniqueTagging articleId tagId

    deriving Show

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
    runMigration migrateUser
    runMigration migrateUserFollower

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
