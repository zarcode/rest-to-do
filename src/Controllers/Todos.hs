{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators   #-}

module Controllers.Todos
    (
        addItem
        , readToDoList
        , writeToDoList
    ) where

import Control.Monad.IO.Class
import Data.Aeson hiding (Success)
import Data.Validation
import GHC.Generics
import Servant

import Models.Todos
import Models.Todo (Item, ItemDescription, ItemTitle, Priority)
import Utils.TodoUtils (readYamlFile, writeYamlFile, makeError)
import Utils.TodoValidation (mergeErrorMessages, validateItemNew)

fileCorruptedError = "YAML file is corrupt"

readToDoList :: FilePath -> Servant.Handler ToDoList 
readToDoList dataPath = do
  mbToDoList <- liftIO $ readYamlFile dataPath
  case mbToDoList of
    Just toDoList -> return toDoList
    Nothing -> throwError $ makeError err500 fileCorruptedError

writeToDoList :: FilePath -> ToDoList -> Servant.Handler ()
writeToDoList dataPath toDoList = liftIO $ writeYamlFile dataPath toDoList  

addItem :: FilePath -> ItemNew -> Servant.Handler Item
addItem dataPath itemNew =
    case validateItemNew itemNew of 
        Failure l -> throwError $ makeError err400 (mergeErrorMessages l)
        Success item -> do
            ToDoList items <- readToDoList dataPath
            let toDoList = ToDoList (item : items)
            writeToDoList dataPath toDoList
            return item