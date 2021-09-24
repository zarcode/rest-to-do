{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib
    ( startApp
    ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Aeson hiding (Success)
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
import           Data.Validation

import           TodoType
import           Todo
import           TodosType
import           Todos
import           Utils
import           Validation

type ItemUpdateDueBy = Maybe String

data ItemUpdate = ItemUpdate
    { mbTitle :: Maybe ItemTitle
    , mbDescription :: Maybe ItemDescription
    , mbPriority :: Maybe ItemPriority
    , mbDueBy :: Maybe ItemUpdateDueBy
    } deriving (Generic, Show)
instance ToJSON ItemUpdate
instance FromJSON ItemUpdate

cantFindItemError = "Invalid item index"

data ValidationError = 
    InvalidPriorityValue Priority
    | InvalidDateFormat String
    deriving Show

type API = 
      ToDosAPI
      :<|> "todo" :> Capture "id" Int :> Get '[JSON] Item
      :<|> "todo" :> Capture "id" Int :> ReqBody '[JSON] ItemUpdate :> Post '[JSON] Item
      :<|> "todo" :> Capture "id" Int :> Delete '[JSON] NoContent

app :: Application
-- app = serve api server
app = serveWithContext api (customFormatters :. EmptyContext) server

api :: Proxy API
api = Proxy

server :: Server API
server = 
    todosServer
     :<|> todo
     :<|> todoUpdate
     :<|> todoDelete

  where 
    todo = viewItem defaultDataPath
    todoUpdate :: Int -> ItemUpdate -> Servant.Handler Item
    todoUpdate = updateItem defaultDataPath
    todoDelete = removeItem defaultDataPath

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
notFoundFormatter req = makeError err404 (cs $ "Not found path: " <> rawPathInfo req)

customFormatters :: ErrorFormatters
customFormatters = defaultErrorFormatters
  { bodyParserErrorFormatter = customFormatter
  , notFoundErrorFormatter = notFoundFormatter
  }

defaultDataPath :: FilePath
defaultDataPath = "todos.yaml"
-- defaultDataPath = "~/.users.yaml"

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