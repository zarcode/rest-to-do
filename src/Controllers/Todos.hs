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

import Models.Todos ( ItemNew, ToDoList(..) )
import Models.Todo (Item, ItemDescription, ItemTitle, Priority, ItemIndex)
import Utils.FileDBUtils (readYamlFile, writeYamlFile)
import Utils.SQLiteDBUtils (readToDoListFromDb, addItemToDb)
import Utils.TodoUtils (makeError)
import Utils.TodoValidation (mergeErrorMessages, validateItemNew)

readToDoList :: FilePath -> Servant.Handler ToDoList 
readToDoList dataPath = do
  eitherToDoList <- liftIO $ readToDoListFromDb dataPath
  case eitherToDoList of
    Right toDoList -> return toDoList
    Left e -> throwError $ makeError err500 e

writeToDoList :: FilePath -> ToDoList -> Servant.Handler ()
writeToDoList dataPath toDoList = liftIO $ writeYamlFile dataPath toDoList 

addItemToDoList :: FilePath -> Item -> Servant.Handler ()
addItemToDoList dataPath item = liftIO $ addItemToDb dataPath item 

addItem :: FilePath -> ItemNew -> Servant.Handler Item
addItem dataPath itemNew =
    case validateItemNew itemNew of 
        Failure l -> throwError $ makeError err400 (mergeErrorMessages l)
        Success item -> do
            addItemToDoList dataPath item
            return item