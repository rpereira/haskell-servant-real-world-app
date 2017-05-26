{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson.Types ( FromJSON, ToJSON, defaultOptions, fieldLabelModifier
                        , genericToJSON, object, parseJSON, toJSON, withObject
                        , (.:), (.=) )
import Data.Char        ( toLower )
import Data.Int         ( Int64 )
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import GHC.Generics

type Username = Text

--------------------------------------------------------------------------------
--  User

data DBUser = DBUser
    { usrId       :: Int64
    , usrUsername :: Username
    , usrEmail    :: Text
    , usrBio      :: Maybe Text
    , usrImage    :: Maybe Text
    } deriving (Eq, Show, Generic)

data NewUser = NewUser
    { username :: Username
    , email    :: Text
    } deriving (Show, Generic)

instance FromJSON NewUser

--------------------------------------------------------------------------------
--  Profile

data UserProfile = UserProfile
  { proUsername  :: Username
  , proBio       :: Maybe Text
  , proImage     :: Maybe Text
  , proFollowing :: Bool
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
--  Utils

toJSONoptions = defaultOptions {
              fieldLabelModifier = headToLower . drop 3 }
  where
    headToLower x = toLower (head x) : tail x
