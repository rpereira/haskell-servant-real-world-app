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

module Models.User where

import Data.Text           (Text)
import Data.Time           (UTCTime)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share,
                            sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateUser"] [persistLowerCase|
User json sql=users
    username  Text
    email     Text
    password  Text
    bio       Text Maybe default=NULL
    image     Text Maybe default=NULL
    createdAt UTCTime default=now()
    updatedAt UTCTime Maybe default=NULL

    UniqueUser username email
    deriving Show
|]
