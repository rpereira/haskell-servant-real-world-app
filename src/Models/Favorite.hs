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

module Models.Favorite where

import Data.Text           (Text)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share,
                            sqlSettings)

import Models.Article
import Models.User

share [mkPersist sqlSettings, mkMigrate "migrateFavorite"] [persistLowerCase|
Favorite json sql=favorits
    userId    UserId
    articleId ArticleId

    Primary userId articleId
    UniqueFavorite userId articleId
|]
