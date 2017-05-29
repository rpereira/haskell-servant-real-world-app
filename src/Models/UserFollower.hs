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

import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share,
                            sqlSettings)

import Models.User

share [mkPersist sqlSettings, mkMigrate "migrateUserFollower"] [persistLowerCase|
UserFollower json sql=user_followers
    userId     UserId
    followerId UserId

    Primary userId followerId

    deriving Show
|]
