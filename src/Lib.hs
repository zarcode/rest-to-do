{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as BS
import           Data.List.Safe ((!!))
import qualified Data.Yaml as Yaml
import           Data.Time
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Prelude hiding ((!!))
import           Servant
import           System.Directory
import           System.IO.Error

type ItemIndex = Int
type ItemTitle = String
type ItemDescription = String
type ItemPriority = Maybe Priority
type ItemDueBy = Maybe LocalTime

data Priority = Low | Normal | High deriving (Generic, Show)
instance ToJSON Priority
instance FromJSON Priority

data Item = Item
    { title :: ItemTitle
    , description :: ItemDescription
    , priority :: ItemPriority
    , dueBy :: ItemDueBy
    } deriving (Generic, Show)
instance ToJSON Item
instance FromJSON Item

data ToDoList = ToDoList [Item] deriving (Generic, Show)
instance ToJSON ToDoList
instance FromJSON ToDoList

-- $(deriveJSON defaultOptions ''Item)

type API = "todos" :> Get '[JSON] ToDoList
      :<|> "todo" :> Capture "id" Int :> Get '[JSON] Item

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
-- server = liftIO (readToDoList defaultDataPath)

server = todos
     :<|> todo

  where todos :: Servant.Handler ToDoList
        todos = liftIO $ viewItems defaultDataPath

        todo :: Int -> Servant.Handler Item
        todo id = liftIO $ viewItem defaultDataPath id

-- users :: ToDoList
-- users = ToDoList [ Item 1 "Isaac" "Newton", Item 2 "Albert" "Einstein"]

defaultDataPath :: FilePath
defaultDataPath = "todos.yaml"
-- defaultDataPath = "~/.users.yaml"

readToDoList :: FilePath -> IO ToDoList
readToDoList dataPath = do
    mbToDoList <- catchJust
        (\e -> if isDoesNotExistError e then Just () else Nothing)
        (BS.readFile dataPath >>= return . Yaml.decode)
        (\_ -> return $ Just (ToDoList []))
    case mbToDoList of
        Nothing -> error "YAML file is corrupt"
        Just toDoList -> return toDoList

viewItem :: FilePath -> ItemIndex -> IO Item
viewItem dataPath idx = do
    ToDoList items <- readToDoList dataPath
    let mbItem = items !! idx
    case mbItem of
        Nothing -> error "Invalid item index"
        Just item -> return item
    

viewItems :: FilePath -> IO ToDoList
viewItems = readToDoList

-- run :: FilePath -> IO ()
-- run dataPath List = viewItems dataPath
