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
-- import           Data.Text
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

data ItemUpdate = ItemUpdate
    { titleUpdate :: Maybe ItemTitle
    , descriptionUpdate :: Maybe ItemDescription
    , priorityUpdate :: Maybe ItemPriority
    , dueByUpdate :: Maybe ItemDueBy
    } deriving (Generic, Show)
instance ToJSON ItemUpdate
instance FromJSON ItemUpdate

data ToDoList = ToDoList [Item] deriving (Generic, Show)
instance ToJSON ToDoList
instance FromJSON ToDoList

data APIError = APIError
    { message:: String
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
      :<|> "todo" :> Capture "id" Int :> ReqBody '[JSON] ItemUpdate :> Post '[JSON] Item
      :<|> "todo" :> Capture "id" Int :> Delete '[JSON] NoContent

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
     :<|> todosAdd
     :<|> todo
     :<|> todoUpdate
     :<|> todoDelete

  where 
    todos = readToDoList defaultDataPath
    todosAdd item = addItem defaultDataPath item
    todo id = viewItem defaultDataPath id
    todoUpdate :: Int -> ItemUpdate -> Servant.Handler Item
    todoUpdate id itemUpdate = updateItem defaultDataPath id itemUpdate
    todoDelete id = removeItem defaultDataPath id

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
  
updateItem :: FilePath -> ItemIndex -> ItemUpdate -> Servant.Handler Item
updateItem dataPath idx (ItemUpdate mbTitle mbDescription mbPriority mbDueBy) = do
    ToDoList items <- readToDoList dataPath
    let update (Item title description priority dueBy) = Item
            (updateField mbTitle title)
            (updateField mbDescription description)
            (updateField mbPriority priority)
            (updateField mbDueBy dueBy)
        updateField (Just value) _ = value
        updateField Nothing value = value
        updateResult = updateAt items idx update
    case updateResult of
        Nothing -> throwError err404 { errBody = cantFindItemError }
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
        Nothing -> throwError err404 { errBody = cantFindItemError }
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