{-# LANGUAGE DeriveGeneric #-}

module TodoType
    ( 
       Item (Item)
       , ItemDueBy
       , ItemIndex
       , ItemTitle 
       , ItemDescription
       , ItemPriority
       , Priority (..)
    ) where

import           Data.Aeson
import           Data.Time
import           GHC.Generics (Generic)

data Priority = Low | Normal | High deriving (Generic, Show)
instance ToJSON Priority
instance FromJSON Priority

type ItemIndex = Int
type ItemTitle = String
type ItemDescription = String
type ItemPriority = Maybe Priority
type ItemDueBy = Maybe LocalTime

data Item = Item
    { title :: ItemTitle
    , description :: ItemDescription
    , priority :: ItemPriority
    , dueBy :: ItemDueBy
    } deriving (Generic, Show)
instance ToJSON Item
instance FromJSON Item