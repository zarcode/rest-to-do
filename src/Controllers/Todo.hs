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
import Utils.SQLiteDBUtils (deleteItemFromDb, getItemFromDb, updateItemInDb)
import Utils.TodoUtils (makeError)
import Utils.TodoValidation (mergeErrorMessages, validateInputDateFormat)

cantFindItemError = "Invalid item index"

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
    eithItem <- liftIO $ getItemFromDb dataPath idx
    case eithItem of
        Left e          -> throwError $ makeError err500 e
        Right ([])      -> throwError $ makeError err500 ("Item with " ++ (show idx) ++ " is not found.")
        Right (item:xs) -> do
            let update (Item id title description priority dueBy) = Item
                    id
                    (updateField mbTitle title)
                    (updateField mbDescription description)
                    (updateField mbPriority priority)
                    (updateField validatedMbDueBy dueBy)
                updateField (Just value) _ = value
                updateField Nothing value = value
                updateResult = update item
            result <- liftIO $ updateItemInDb dataPath updateResult 
            case result of
                Right _ -> return updateResult
                Left e -> throwError $ makeError err500 e

-- updateItem :: FilePath -> ItemIndex -> ItemUpdate -> Servant.Handler Item
-- updateItem dataPath idx (ItemUpdate mbTitle mbDescription mbPriority mbDueBy) = do
--     validatedMbDueBy <- getUpdateDueBy mbDueBy
--     let updateResult = 
--         [
--             ("title", pack (updateField mbTitle)), 
--             ("description", pack (updateField mbDescription) ), 
--             ("priority", pack (updateField mbPriority)), 
--             ("dueBy", pack (updateField validatedMbDueBy)), 
--             ("id", pack id)
--         ]
--         updateField (Just value) = value
--         updateField Nothing = Nothing
--     result <- liftIO $ updateItemInDb dataPath updateResult 
--     case result of
--         Right _ -> return result
--         Left e -> throwError $ makeError err500 e

removeItem :: FilePath -> ItemIndex -> Servant.Handler NoContent 
removeItem dataPath idx = do
    result <- liftIO $ deleteItemFromDb dataPath idx
    case result of
        Right _ -> return NoContent
        Left e -> throwError $ makeError err500 e