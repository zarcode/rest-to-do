{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.TodoUtils
    (
        makeError,
        APIError (APIError)
    ) where

import           Data.Aeson hiding (Success)
import qualified Data.Yaml as Yaml
import           GHC.Generics
import           System.IO.Error
import           Servant

data APIError = APIError
    { message:: String
    } deriving (Generic, Show)
instance ToJSON APIError
instance FromJSON APIError

makeError code m = code { errBody = Data.Aeson.encode $ APIError m
                        , errHeaders = [("Content-Type", "application/json")]
                    }