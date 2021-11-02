{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.SQLiteDBUtils
    ( 
        readToDoListFromDb,
        addItemToDb
    ) where

import           Control.Exception
import           Data.Aeson hiding (Success)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml
import Database.SQLite.Simple
    ( execute, query_, withConnection, Only(Only) )
import           GHC.Generics
import           System.IO.Error
import           Servant

import           Models.Todo
import           Models.Todos

readToDoListFromDb :: String -> IO (Maybe ToDoList)
readToDoListFromDb dataPath = do
    items <- withConnection dataPath $ \conn ->
            query_ conn "SELECT title, description, priority, dueBy FROM todos" :: IO [Item]
    return $ Just (ToDoList items)
 
addItemToDb :: FilePath -> Item -> IO ()
addItemToDb dataPath item = withConnection dataPath $ \conn ->
                                execute conn
                                        "INSERT INTO todos (title, description, priority, dueBy) VALUES (?, ?, ?, ?)" item

-- readToDoListFromDb dataPath =
--     catchJust
--             (\e -> if isDoesNotExistError e then Just () else Nothing)
--             (do
--                 items <- withConnection dataPath $ \conn -> query_ conn "SELECT mmm FROM todos" :: IO [Item]
--                 return $ Just (ToDoList items) 
--             )
--             (\_ -> return $ Just (ToDoList []))