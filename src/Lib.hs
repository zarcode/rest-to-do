{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    ) where

import           Data.Aeson hiding (Success)
import           Data.Aeson.TH
import           Data.String.Conversions (cs)
import           Database.SQLite.Simple ( execute_, withConnection )
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Prelude hiding ((!!))
import           Servant
import           Servant.API.ContentTypes
import           System.Directory

import           Routes.Todo
import           Routes.Todos
import           Models.Todo
import           Models.Todos
import           Utils.TodoUtils
import           Utils.TodoValidation

data ValidationError = 
    InvalidPriorityValue Priority
    | InvalidDateFormat String
    deriving Show

type API = 
      ToDosAPI
      :<|> ToDoAPI

initDB :: FilePath -> IO ()
initDB dbfile = withConnection dbfile $ \conn ->
  execute_ conn
    "CREATE TABLE IF NOT EXISTS todos (title TEXT, description TEXT, priority TEXT, dueBy TEXT)"

app :: FilePath -> Application
app dbfile = serveWithContext api (customFormatters :. EmptyContext) $ server dbfile

api :: Proxy API
api = Proxy

server :: FilePath -> Server API
server dbfile = 
    (todosServer dbfile)
    :<|> (todoServer dbfile)

runApp :: FilePath -> IO ()
runApp dbfile = run 8080 (app dbfile)

startApp :: IO ()
startApp = do
  -- TODO: we could read file path from config file
  -- let dbfile = "todos.yaml"
  -- initDB dbfile
  -- mgr <- newManager defaultManagerSettings
  -- runApp dbfile
  let dbfile = "todos.db"
  initDB dbfile
  runApp dbfile
  -- bracket (forkIO $ runApp dbfile) killThread $ \_ -> do
  --   ms <- flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
  --     postMsg "hello"
  --     postMsg "world"
  --     getMsgs
  --   print ms

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
