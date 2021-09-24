{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators   #-}

module Todos
    (
        todosServer
        , readToDoList
        , writeToDoList
        , ToDosAPI
    ) where

import Control.Monad.IO.Class
import Data.Aeson hiding (Success)
import Data.Validation
import GHC.Generics
import Servant
-- import TodosType (ItemNew, ToDoList)
import TodosType
import TodoType (Item, ItemDescription, ItemTitle, Priority)
import Utils (readYamlFile, writeYamlFile, makeError)
import Validation (mergeErrorMessages, validateItemNew)

defaultDataPath :: FilePath
defaultDataPath = "todos.yaml"

fileCorruptedError = "YAML file is corrupt"

type ToDosAPI = "todos" :> Get '[JSON] ToDoList
                :<|> "todos" :> ReqBody '[JSON] ItemNew :> Post '[JSON] Item

todosServer :: Server ToDosAPI
todosServer = 
        todos
        :<|> todosAdd
    where 
        todos = readToDoList defaultDataPath
        todosAdd = addItem defaultDataPath

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