{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson.Types (FromJSON, ToJSON, defaultOptions, fieldLabelModifier,
                         genericParseJSON, genericToJSON, object, parseJSON,
                         toJSON, withObject, (.:), (.=))
import Data.Char        (toLower)
import Data.Text        (Text)
import GHC.Generics

toJSONoptions =
    defaultOptions { fieldLabelModifier = map toLower . drop 3 }

type Username = Text
type Slug = Text

--------------------------------------------------------------------------------
--  QueryParams

type Limit = Int
type Offset = Int

--------------------------------------------------------------------------------
--  User

data NewUser = NewUser
    { username :: Username
    , email    :: Text
    } deriving (Show, Generic)

instance FromJSON NewUser

data AuthUser = AuthUser
    { aurUsername :: Username
    , aurToken    :: Text
    , aurEmail    :: Text
    , aurBio      :: Maybe Text
    , aurImage    :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToJSON AuthUser where
    toJSON = genericToJSON toJSONoptions

data Usr a = Usr a
    deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Usr a) where
    toJSON (Usr a) = object ["user" .= a]

instance FromJSON a => FromJSON (Usr a) where
    parseJSON = withObject "user" $ \o -> do
        a <- o .: "user"
        return (Usr a)

--------------------------------------------------------------------------------
--  Profile

data UserProfile = UserProfile
    { proUsername :: Username
    , proBio      :: Maybe Text
    , proImage    :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToJSON UserProfile where
    toJSON = genericToJSON toJSONoptions

data Profile a = Profile a
    deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Profile a) where
    toJSON (Profile a) = object ["profile" .= a]

instance FromJSON a => FromJSON (Profile a) where
    parseJSON = withObject "profile" $ \o -> do
        a <- o .: "profile"
        return (Profile a)

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

--------------------------------------------------------------------------------
--  Tags

data TagList a = TagList a
    deriving (Eq, Show)

instance ToJSON a => ToJSON (TagList a) where
    toJSON (TagList a) = object ["tags" .= a]

--------------------------------------------------------------------------------
--  Comments

data Cmts a = Cmts a
    deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Cmts a) where
    toJSON (Cmts a) = object ["comments" .= a]

data Cmt a = Cmt a
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Cmt a) where
  toJSON (Cmt a) = object ["comment" .= a]

instance FromJSON a => FromJSON (Cmt a) where
  parseJSON = withObject "comment" $ \o -> do
    a <- o .: "comment"
    return (Cmt a)

data NewComment = NewComment { cmtBody :: Text }
    deriving (Eq, Show, Generic)

instance FromJSON NewComment where
    parseJSON = genericParseJSON toJSONoptions
