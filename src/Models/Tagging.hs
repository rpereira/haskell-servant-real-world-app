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

module Models.Tagging where

import Control.Monad.Reader
import Data.Aeson           (FromJSON, ToJSON)
import Data.Text            (Text)
import Database.Persist.Sql
import Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase, share,
                             sqlSettings)
import GHC.Generics         (Generic)

import Models.Article
import Models.Tag

share [mkPersist sqlSettings, mkMigrate "migrateTagging"] [persistLowerCase|
Tagging json sql=taggings
    articleId ArticleId
    tagId TagId

    Primary articleId tagId
    UniqueTagging articleId tagId

    deriving Show
|]
