{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators   #-}

module TodosType
    (
        ItemNew (ItemNew)
        , ToDoList (ToDoList)
    ) where

import Data.Aeson hiding (Success)
import GHC.Generics
import Servant
import TodoType (Item, ItemDescription, ItemTitle, Priority)

data ToDoList = ToDoList [Item] deriving (Generic, Show)
instance ToJSON ToDoList
instance FromJSON ToDoList

data ItemNew = ItemNew
    { title :: ItemTitle
    , description :: ItemDescription
    , priority :: Priority
    , dueBy :: String
    } deriving (Generic, Show)
instance ToJSON ItemNew
instance FromJSON ItemNew
