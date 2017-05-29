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

module Models.UserFollower where

import Control.Monad.Reader
import Data.Aeson           (FromJSON, ToJSON)
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Database.Persist.Sql
import Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase, share,
                             sqlSettings)
import GHC.Generics         (Generic)

import Models.User

share [mkPersist sqlSettings, mkMigrate "migrateUserFollower"] [persistLowerCase|
UserFollower json sql=user_followers
    userId     UserId
    followerId UserId

    Primary userId followerId

    deriving Show
|]
