{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators   #-}

module Routes.Todo
    (
        todoServer, 
        ToDoAPI
    ) where

import Control.Monad.IO.Class
import Data.Aeson hiding (Success)
import Data.List.Safe ((!!))
import Data.Validation
import GHC.Generics
import Prelude hiding ((!!))
import Servant

import Controllers.Todo
import Routes.Todos
import Models.Todos
import Models.Todo
import Utils.TodoUtils (makeError)
import Utils.TodoValidation (mergeErrorMessages, validateInputDateFormat)

cantFindItemError = "Invalid item index"

defaultDataPath :: FilePath
defaultDataPath = "todos.yaml"

type ToDoAPI = "todo" :> Capture "id" Int :> Get '[JSON] Item
                :<|> "todo" :> Capture "id" Int :> ReqBody '[JSON] ItemUpdate :> Post '[JSON] Item
                :<|> "todo" :> Capture "id" Int :> Delete '[JSON] NoContent 
    
todoServer :: Server ToDoAPI
todoServer = 
        todo
        :<|> todoUpdate
        :<|> todoDelete
    where 
        todo = viewItem defaultDataPath
        todoUpdate :: Int -> ItemUpdate -> Servant.Handler Item
        todoUpdate = updateItem defaultDataPath
        todoDelete = removeItem defaultDataPath