{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Data.Text
import           Data.String.Conversions (cs)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Prelude hiding ((!!))
import           Servant
import           Servant.API.ContentTypes
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

data APIError = APIError
    { message:: Text
    } deriving (Generic, Show)
instance ToJSON APIError
instance FromJSON APIError

fileCorruptedError = Data.Aeson.encode $ APIError "YAML file is corrupt"
cantFindItemError = Data.Aeson.encode $ APIError "Invalid item index"
notFoundError req = Data.Aeson.encode $ APIError (cs $ "Not found path: " <> rawPathInfo req)

-- $(deriveJSON defaultOptions ''Item)

type API = "todos" :> Get '[JSON] ToDoList
      :<|> "todos" :> ReqBody '[JSON] Item :> Post '[JSON] Item
      :<|> "todo" :> Capture "id" Int :> Get '[JSON] Item

startApp :: IO ()
startApp = run 8080 app

customFormatter :: ErrorFormatter
customFormatter tr req err =
  let
    -- aeson Value which will be sent to the client
    value = APIError $ cs err
    -- Accept header of the request
    accH = getAcceptHeader req
  in
  -- handleAcceptH is Servant's function that checks whether the client can accept a
  -- certain message type.
  -- In this case we call it with "Proxy '[JSON]" argument, meaning that we want to return a JSON.
  case handleAcceptH (Proxy :: Proxy '[JSON]) accH value of
    -- If client can't handle JSON, we just return the body the old way
    Nothing -> err400 { errBody = cs err }
    -- Otherwise, we return the JSON formatted body and set the "Content-Type" header.
    Just (ctypeH, body) -> err400
      { errBody = body
      , errHeaders = [("Content-Type", cs ctypeH)]
      }

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter req =
  err404 { errBody = notFoundError req }

customFormatters :: ErrorFormatters
customFormatters = defaultErrorFormatters
  { bodyParserErrorFormatter = customFormatter
  , notFoundErrorFormatter = notFoundFormatter
  }

app :: Application
-- app = serve api server
app = serveWithContext api (customFormatters :. EmptyContext) server

api :: Proxy API
api = Proxy

server :: Server API

server = todos
     :<|> addTodoItem
     :<|> todo

  where 
    todos = readToDoList defaultDataPath
    addTodoItem item = addItem defaultDataPath item
    todo id = viewItem defaultDataPath id

defaultDataPath :: FilePath
defaultDataPath = "todos.yaml"
-- defaultDataPath = "~/.users.yaml"

readYamlFile :: FilePath -> IO (Maybe ToDoList)
readYamlFile dataPath = catchJust
            (\e -> if isDoesNotExistError e then Just () else Nothing)
            (BS.readFile dataPath >>= return . Yaml.decode)
            (\_ -> return $ Just (ToDoList []))

readToDoList :: FilePath -> Servant.Handler ToDoList 
readToDoList dataPath = do
  mbToDoList <- liftIO $ readYamlFile dataPath
  case mbToDoList of
    Just toDoList -> return toDoList
    Nothing -> throwError err500 { errBody = fileCorruptedError }

writeYamlFile :: FilePath -> ToDoList -> IO ()
writeYamlFile dataPath toDoList = BS.writeFile dataPath (Yaml.encode toDoList)

writeToDoList :: FilePath -> ToDoList -> Servant.Handler ()
writeToDoList dataPath toDoList = liftIO $ writeYamlFile dataPath toDoList  

addItem :: FilePath -> Item -> Servant.Handler Item
addItem dataPath item = do
    ToDoList items <- readToDoList dataPath
    let toDoList = ToDoList (item : items)
    writeToDoList dataPath toDoList
    return item

viewItem :: FilePath -> ItemIndex -> Servant.Handler Item
viewItem dataPath idx = do
    ToDoList items <- readToDoList dataPath
    let mbItem = items !! idx
    case mbItem of
        Just item -> return item
        Nothing -> throwError err404 { errBody = cantFindItemError }
  
