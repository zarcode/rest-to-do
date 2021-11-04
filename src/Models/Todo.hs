{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

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

import Data.Aeson ( FromJSON, ToJSON )
import qualified Data.Text as T
import Data.Time ( LocalTime, defaultTimeLocale, parseTimeM  )
import Database.SQLite.Simple.FromRow ( field, FromRow(..) )
import Database.SQLite.Simple.ToRow ( ToRow(..) )
import Database.SQLite.Simple.FromField
    ( ResultError(ConversionFailed), returnError, FromField(..) )
import Database.SQLite.Simple.ToField ( ToField(..) )
import Database.SQLite.Simple.Internal ( Field(Field) )
import Database.SQLite.Simple.Ok ( Ok(Ok) )
import Database.SQLite.Simple ( SQLData(SQLText) )
import           GHC.Generics (Generic)

instance MonadFail (Either String) where 
  fail = Left

parseDateTimeMaybe :: String -> Either [Char] LocalTime
parseDateTimeMaybe = parseTimeM False defaultTimeLocale dateTimeFormat
                        where dateTimeFormat = "%Y-%m-%d %H:%M:%S"              
instance FromField LocalTime where
  fromField f@(Field (SQLText t) _) =
    case parseDateTimeMaybe (T.unpack t) of
      Right t -> Ok t
      Left e -> returnError ConversionFailed f ("couldn't parse UTCTime field: " ++ (show e))

  fromField f = returnError ConversionFailed f "expecting SQLText column type"
   
instance ToField LocalTime where
  toField = SQLText . T.pack . show

data Priority = Low | Normal | High deriving (Generic, Show)
instance ToJSON Priority
instance FromJSON Priority
instance FromField Priority where
  fromField (Field (SQLText "Low") _) = Ok Low
  fromField (Field (SQLText "Normal") _) = Ok Normal
  fromField (Field (SQLText "High") _) = Ok High
  fromField f = returnError ConversionFailed f "need 'Low', 'Normal' or 'High'"
instance ToField Priority where
  toField = SQLText . T.pack . show

type ItemIndex = Int
type ItemTitle = String
type ItemDescription = String
type ItemPriority = Maybe Priority
type ItemDueBy = Maybe LocalTime

data Item = Item
    { id :: ItemIndex
    , title :: ItemTitle
    , description :: ItemDescription
    , priority :: ItemPriority
    , dueBy :: ItemDueBy
    } deriving (Generic, Show)
instance ToJSON Item
instance FromJSON Item
instance FromRow Item where fromRow = Item <$> field <*> field <*> field <*> field <*> field 
instance ToRow Item where
  toRow (Item _id title description priority dueBy) = toRow (title, description, priority, dueBy)



type ItemUpdateDueBy = Maybe String

data ItemUpdate = ItemUpdate
    { mbTitle :: Maybe ItemTitle
    , mbDescription :: Maybe ItemDescription
    , mbPriority :: Maybe ItemPriority
    , mbDueBy :: Maybe ItemUpdateDueBy
    } deriving (Generic, Show)
instance ToJSON ItemUpdate
instance FromJSON ItemUpdate