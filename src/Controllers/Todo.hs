{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators   #-}

module Controllers.Todo
    (
        viewItem,
        updateItem,
        removeItem
    ) where

import Control.Monad.IO.Class
import Data.Aeson hiding (Success)
import Data.List.Safe ((!!))
import Data.Validation
import GHC.Generics
import Prelude hiding ((!!))
import Servant

import Controllers.Todos
import Routes.Todos
import Models.Todos
import Models.Todo
import Utils.TodoUtils (makeError)
import Utils.TodoValidation (mergeErrorMessages, validateInputDateFormat)

cantFindItemError = "Invalid item index"

defaultDataPath :: FilePath
defaultDataPath = "todos.yaml"

viewItem :: FilePath -> ItemIndex -> Servant.Handler Item
viewItem dataPath idx = do
    ToDoList items <- readToDoList dataPath
    let mbItem = items !! idx
    case mbItem of
        Just item -> return item
        Nothing -> throwError $ makeError err404 cantFindItemError

getUpdateDueBy :: Maybe ItemUpdateDueBy -> Servant.Handler (Maybe ItemDueBy)
getUpdateDueBy (Just value) =
    case value of
                Just dueBy ->
                    case validateInputDateFormat dueBy of
                        Failure l -> throwError $ makeError err400 (mergeErrorMessages l)
                        Success dueByDate -> return $ Just dueByDate
                Nothing -> return Nothing
getUpdateDueBy Nothing = return Nothing 

updateItem :: FilePath -> ItemIndex -> ItemUpdate -> Servant.Handler Item
updateItem dataPath idx (ItemUpdate mbTitle mbDescription mbPriority mbDueBy) = do
    validatedMbDueBy <- getUpdateDueBy mbDueBy 
    ToDoList items <- readToDoList dataPath
    let update (Item title description priority dueBy) = Item
            (updateField mbTitle title)
            (updateField mbDescription description)
            (updateField mbPriority priority)
            (updateField validatedMbDueBy dueBy)
        updateField (Just value) _ = value
        updateField Nothing value = value
        updateResult = updateAt items idx update
    case updateResult of
        Nothing -> throwError $ makeError err404 cantFindItemError
        Just (items', changedItem) -> do
            let toDoList = ToDoList items'
            writeToDoList dataPath toDoList
            return changedItem

updateAt :: [a] -> Int -> (a -> a) -> Maybe ([a], a)
updateAt xs idx f =
    if idx < 0 || idx >= length xs
    then Nothing
    else
        let (before, after) = splitAt idx xs
            element : after' = after
            newElement = f element
            xs' = before ++ newElement : after'
        in Just (xs', newElement)

removeItem :: FilePath -> ItemIndex -> Servant.Handler NoContent 
removeItem dataPath idx = do
    ToDoList items <- readToDoList dataPath
    let mbItems = items `removeAt` idx
    case mbItems of
        Nothing -> throwError $ makeError err404 cantFindItemError
        Just items' -> do
            let toDoList = ToDoList items'
            writeToDoList dataPath toDoList
            return NoContent

removeAt :: [a] -> Int -> Maybe [a]
removeAt xs idx =
    if idx < 0 || idx >= length xs
    then Nothing
    else
        let (before, after) = splitAt idx xs
            _ : after' = after
            xs' = before ++ after'
        in Just xs'