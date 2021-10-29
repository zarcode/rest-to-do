{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators   #-}

module Routes.Todos
    (
        todosServer
        , ToDosAPI
    ) where

import Control.Monad.IO.Class
import Data.Aeson hiding (Success)
import Data.Validation
import GHC.Generics
import Servant

import Controllers.Todos
import Models.Todos
import Models.Todo (Item, ItemDescription, ItemTitle, Priority)
import Utils.TodoValidation (mergeErrorMessages, validateItemNew)

fileCorruptedError = "YAML file is corrupt"

type ToDosAPI = "todos" :> Get '[JSON] ToDoList
                :<|> "todos" :> ReqBody '[JSON] ItemNew :> Post '[JSON] Item

todosServer :: FilePath -> Server ToDosAPI
todosServer dbfile = 
        todos
        :<|> todosAdd
    where 
        todos = readToDoList dbfile
        todosAdd = addItem dbfile