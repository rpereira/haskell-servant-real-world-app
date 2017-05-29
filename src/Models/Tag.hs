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

module Models.Tag where

import Data.Text           (Text)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share,
                            sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTag"] [persistLowerCase|
Tag json sql=tags
    name Text
    UniqueName name
    deriving Show
|]
