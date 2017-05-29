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

module Models.Comment where

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

share [mkPersist sqlSettings, mkMigrate "migrateComment"] [persistLowerCase|
Comment json sql=comments
    body      Text
    createdAt UTCTime default=now()
    updatedAt UTCTime Maybe default=NULL
    userId    UserId
    articleId ArticleId

    deriving Show
|]
