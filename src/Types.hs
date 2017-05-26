{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Aeson ( FromJSON )
import Data.Text
import GHC.Generics

type Username = Text

newtype Password = Password { unPassword :: Text }
    deriving (Show, FromJSON)

data NewUser = NewUser
    { nName     :: Username
    , nEmail    :: Text
    , nPassword :: Password
    , nBio      :: Maybe Text
    , nImage    :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON NewUser

data AuthUser = AuthUser
    { aUsername :: Username
    , aEmail    :: Text
    , aToken    :: Text
    , aBio      :: Maybe Text
    , aImage    :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromJSON AuthUser

data Login = Login
    { authEmail    :: Text
    , authPassword :: Password
    } deriving (Show, Generic)

instance FromJSON Login
