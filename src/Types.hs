{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson ( FromJSON )
import Data.Text
import GHC.Generics

type Username = Text

data NewUser = NewUser
    { username :: Username
    , email    :: Text
    } deriving (Show, Generic)

instance FromJSON NewUser
