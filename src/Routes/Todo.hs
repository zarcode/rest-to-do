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

type ToDoAPI = "todo" :> Capture "id" Int :> Get '[JSON] Item
                :<|> "todo" :> Capture "id" Int :> ReqBody '[JSON] ItemUpdate :> Post '[JSON] Item
                :<|> "todo" :> Capture "id" Int :> Delete '[JSON] NoContent 
    
todoServer :: FilePath -> Server ToDoAPI
todoServer dbfile = 
        todo
        :<|> todoUpdate
        :<|> todoDelete
    where 
        todo = viewItem dbfile
        todoUpdate :: Int -> ItemUpdate -> Servant.Handler Item
        todoUpdate = updateItem dbfile
        todoDelete = removeItem dbfile