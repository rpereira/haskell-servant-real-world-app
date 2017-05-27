{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson ( FromJSON, ToJSON )
import Data.Text
import GHC.Generics

type Username = Text

--------------------------------------------------------------------------------
--  QueryParams

type Limit = Int
type Offset = Int

data NewUser = NewUser
    { username :: Username
    , email    :: Text
    } deriving (Show, Generic)

instance FromJSON NewUser

data NewArticle = NewArticle
    { title       :: Text
    , description :: Text
    , body        :: Text
    } deriving (Show, Generic)

instance FromJSON NewArticle
