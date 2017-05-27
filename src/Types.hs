{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson.Types ( FromJSON, (.=), ToJSON, object, toJSON )
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

--------------------------------------------------------------------------------
--  Article

data NewArticle = NewArticle
    { title       :: Text
    , description :: Text
    , body        :: Text
    } deriving (Show, Generic)

instance FromJSON NewArticle

data Arts a = Arts a Int
    deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Arts a) where
    toJSON (Arts a i) = object ["articles" .= a, "articlesCount" .= i]
