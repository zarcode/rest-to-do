{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.SQLiteDBUtils
    ( 
        addItemToDb,
        getItemFromDb,
        deleteItemFromDb,
        readToDoListFromDb,
        updateItemInDb
    ) where

import           Control.Exception
import           Data.Aeson hiding (Success)
import qualified Data.ByteString.Char8 as BS
import           Database.SQLite.SimpleErrors
import           Database.SQLite.SimpleErrors.Types
import           Database.SQLite.Simple ( execute, executeNamed, queryNamed, query_, withConnection, NamedParam( (:=) ), Only(Only) )
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import           GHC.Generics
import           System.IO.Error
import           Servant

import           Models.Todo
import           Models.Todos
 
addItemToDb :: FilePath -> Item -> IO ()
addItemToDb dataPath item = withConnection dataPath $ \conn ->
                                execute conn
                                        "INSERT INTO todos (title, description, priority, dueBy) VALUES (?, ?, ?, ?)" item

getItemFromDb :: FilePath -> ItemIndex -> IO (Either String [Item])
getItemFromDb dataPath idx = withConnection dataPath $ \conn -> do
                                    eithItem <- runDBAction $ (queryNamed conn "SELECT id, title, description, priority, dueBy from todos where id = :id" [":id" := idx] :: IO [Item])
                                    case eithItem of
                                        Left e  -> return $ Left (show e)
                                        Right r -> return $ Right r

updateItemInDb :: FilePath -> Item -> IO (Either String ())
updateItemInDb dataPath (Item id title description priority dueBy) = withConnection dataPath $ \conn -> do
                                    res <- runDBAction $ ( executeNamed conn "UPDATE todos SET title = :title, description = :description, priority = :priority, dueBy = :dueBy WHERE id = :id" [":title" := title, ":description" := description, ":priority" := priority, ":dueBy" := dueBy, ":id" := id] )
                                    case res of
                                        Left e  -> return $ Left (show e)
                                        Right r -> return $ Right r

deleteItemFromDb :: FilePath -> ItemIndex -> IO (Either String ())
deleteItemFromDb dataPath idx = withConnection dataPath $ \conn -> do
                                    eithItem <- getItemFromDb dataPath idx
                                    case eithItem of
                                        Left e  -> return $ Left e
                                        Right r -> if((length r) > 0)
                                                        then do
                                                            res <- runDBAction $ execute conn
                                                                "DELETE FROM todos WHERE id = ?" (Only idx)
                                                            case res of
                                                                Left e  -> return $ Left (show e)
                                                                Right r -> return $ Right r 
                                                        else return $ Left ("Item with " ++ (show idx) ++ " is not found.")
                                        
readToDoListFromDb :: String -> IO (Either String ToDoList)
readToDoListFromDb dataPath = 
    withConnection dataPath $ \conn -> do
                res <- runDBAction $ (query_ conn "SELECT id, title, description, priority, dueBy FROM todos" :: IO [Item])
                case res of
                    Left e  -> return $ Left (show e)
                    Right items -> return $ Right (ToDoList items) 
