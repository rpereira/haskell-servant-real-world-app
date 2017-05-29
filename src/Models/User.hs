{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models.User where

import Data.Aeson                  (ToJSON, object, toJSON, (.=))
import Data.Text                   (Text)
import Data.Time                   (UTCTime)
import Database.Persist.Postgresql (Entity (..), entityVal)
import Database.Persist.TH         (mkMigrate, mkPersist, persistLowerCase,
                                    share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateUser"] [persistLowerCase|
User sql=users
    username  Text
    email     Text
    bio       Text Maybe default=NULL
    image     Text Maybe default=NULL
    createdAt UTCTime default=now()
    updatedAt UTCTime Maybe default=NULL

    UniqueUser username email
    deriving Show
|]

instance ToJSON (Entity User) where
    toJSON entity = object
        [ "username" .= userUsername val
        , "bio"      .= userBio val
        , "image"    .= userImage val
        ]
            where val = entityVal entity
