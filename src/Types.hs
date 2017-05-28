{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson.Types ( FromJSON, (.:), (.=), ToJSON, object, parseJSON
                        , toJSON, withObject )
import Data.Text
import GHC.Generics

type Username = Text
type Slug = Text

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

data Art a = Art a
    deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Art a) where
    toJSON (Art a) = object ["article" .= a]

instance FromJSON a => FromJSON (Art a) where
    parseJSON = withObject "article" $ \o -> do
        a <- o .: "article"
        return (Art a)
