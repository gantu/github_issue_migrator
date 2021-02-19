{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Fetcher where

import           Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
import Control.Applicative -- <$>, <*>
import Data.Maybe (fromJust)
import Control.Monad (forM_)

data Issue = Issue { title :: String
                   , body :: String
                   } deriving (Show)

instance FromJSON Issue where
    parseJSON (Object v) = Issue <$>
                           v .: "title" <*>
                           v .: "body"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Issue from YAML/JSON"

instance ToJSON Issue where
    toJSON (Issue title body) = object
      [ "title" .= title
      , "body" .= body
      ]

fetchData :: IO (Maybe [Issue])
fetchData = do
  let request = setRequestHeader "Accept" ["application/vnd.github.v3+json"]
              $ setRequestHeader "User-Agent" ["request"]
              $ setRequestHeader "Authorization" ["token OAUTHToken"]
              $ "GET https://api.github.com/repos/{owner}/{repo_name}/issues"
  response <- httpJSON request             
                                          
  let decodedData = encode (getResponseBody response :: Value)
  return (decode decodedData :: Maybe [Issue])
  

createIssues :: Maybe [Issue] -> IO ()
createIssues issues = do
  case issues of
    Nothing -> putStrLn $ "No issue to create!"
    Just issueList -> forM_ issueList $ \issue -> do
      let request = setRequestHeader "Accept" ["application/vnd.github.v3+json"]
                  $ setRequestHeader "User-Agent" ["request"]   
                  $ setRequestHeader "Authorization" ["token OAUTHToken"]
                  $ setRequestBodyJSON issue                 
                  $ "POST https://api.github.com/repos/{owner}/{reponame}/issues"
      response <- httpJSON request                          
      S8.putStrLn $ Yaml.encode (getResponseBody response :: Value) 

migrate :: IO ()
migrate = do

  fetchedData <- fetchData
  createIssues fetchedData
