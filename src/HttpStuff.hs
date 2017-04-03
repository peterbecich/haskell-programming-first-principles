module HttpStuff where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

urls :: [String]
urls = [ "http://httpbin.org/ip", "http://httpbin.org/bytes/5"]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedResponses :: IO [Response ByteString]
traversedResponses = traverse get urls
