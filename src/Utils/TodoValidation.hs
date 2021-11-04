{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Utils.TodoValidation
    ( validateInputPriorityFormat
    , validateInputDateFormat
    , mergeErrorMessages
    , validateItemNew
    ) where

import Data.List.Safe ((!!), intercalate)
import Data.Time ( defaultTimeLocale, parseTimeM )
import Data.Validation

import Models.Todo
import Models.Todos

data ValidationError = 
    InvalidPriorityValue Priority
    | InvalidDateFormat String
    deriving Show

validateInputPriorityFormat :: Priority -> Validation [ValidationError] ItemPriority
validateInputPriorityFormat priority = case priority of
                Low -> Success $ Just Low
                Normal -> Success $ Just Normal
                High -> Success $ Just High
                _ -> Failure $ [InvalidPriorityValue priority]

validateInputDateFormat :: String -> Validation [ValidationError] ItemDueBy
validateInputDateFormat dueBy = 
        case parseDateTimeMaybe dueBy of
                Just dateTime -> Success (Just dateTime)
                Nothing -> Failure $ [InvalidDateFormat dateTimeFormat]
        where         
              parseDateTimeMaybe = parseTimeM False defaultTimeLocale dateTimeFormat
              dateTimeFormat = "%Y/%m/%d %H:%M:%S"

mergeErrorMessages = intercalate "," . (map errorToString)

errorToString (InvalidPriorityValue priority) = "Invalid priority value " ++ show priority
errorToString (InvalidDateFormat dateTimeFormat) = "Date/time string must be in " ++ dateTimeFormat ++ " format"


validateItemNew :: ItemNew -> Validation [ValidationError] Item
validateItemNew (ItemNew title description priority dueBy) = 
    Item 0 title description <$> validateInputPriorityFormat priority <*> validateInputDateFormat dueBy
