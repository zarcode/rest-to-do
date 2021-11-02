{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.FileDBUtils
    ( 
        readYamlFile,
        writeYamlFile
    ) where

import           Control.Exception
import           Data.Aeson hiding (Success)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Yaml
import           GHC.Generics
import           System.IO.Error
import           Servant

import           Models.Todos

readYamlFile :: FilePath -> IO (Maybe ToDoList)
readYamlFile dataPath = catchJust
            (\e -> if isDoesNotExistError e then Just () else Nothing)
            (BS.readFile dataPath >>= return . Yaml.decode)
            (\_ -> return $ Just (ToDoList []))

writeYamlFile :: FilePath -> ToDoList -> IO ()
writeYamlFile dataPath toDoList = BS.writeFile dataPath (Yaml.encode toDoList)