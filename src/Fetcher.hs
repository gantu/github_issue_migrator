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

fetch :: IO ()
fetch = do

    let request = setRequestHeader "Accept" ["application/vnd.github.v3+json"]
    		$ setRequestHeader "User-Agent" ["request"]
		$ setRequestHeader "Authorization" ["token OAUTHToken"]
                $ "GET https://api.github.com/repos/{owner}/{reponame}/issues"
    response <- httpJSON request
   
    let yamlData = Yaml.encode (getResponseBody response :: Value)
        decodedData = Yaml.decode yamlData :: Maybe [Issue]

    case decodedData of
    	Nothing -> putStrLn $ "No Data in response"
	Just issues -> forM_ issues $ \issue -> do
		let request' = setRequestHeader "Accept" ["application/vnd.github.v3+json"]
			     $ setRequestHeader "User-Agent" ["request"]
			     $ setRequestHeader "Authorization" ["token OAUTHToken"]
		             $ setRequestBodyJSON issue 
                             $ "POST https://api.github.com/repos/{owner}/{reponame}/issues"
		response' <- httpJSON request' 
		S8.putStrLn $ Yaml.encode (getResponseBody response' :: Value)

    putStrLn $ show decodedData
