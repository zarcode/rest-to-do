{-# LANGUAGE DeriveGeneric #-}

module Models.Todo
    ( 
       Item (Item)
       , ItemDueBy
       , ItemUpdateDueBy
       , ItemIndex
       , ItemTitle 
       , ItemDescription
       , ItemPriority
       , ItemUpdate (ItemUpdate)
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

type ItemUpdateDueBy = Maybe String

data ItemUpdate = ItemUpdate
    { mbTitle :: Maybe ItemTitle
    , mbDescription :: Maybe ItemDescription
    , mbPriority :: Maybe ItemPriority
    , mbDueBy :: Maybe ItemUpdateDueBy
    } deriving (Generic, Show)
instance ToJSON ItemUpdate
instance FromJSON ItemUpdate