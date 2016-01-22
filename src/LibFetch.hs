

module LibFetch
    ( get, getCode, filterContent, fetchInterNews
    ) where

import Network.HTTP
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
-- Non HTTPS

-- 1. Perform a basic HTTP get request and return the body
get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

-- 2. Get the response code
getCode :: String -> IO ResponseCode
getCode url = simpleHTTP req >>= getResponseCode
    where req = getRequest url

filterContent :: String -> String -> String
filterContent substr htmlAsString = unlines $ filter (\s -> isInfixOf substr s) (lines htmlAsString)

fetchInterNews :: IO [String]
fetchInterNews = do
  content <- get "http://www.newsnow.co.uk/h/Sport/Football/Europe/Italy/Inter+Milan"
  let ls = splitOn "<div class=\"hl" content
  let ls' = filterHeadlines ls
  let ls'' = map (\s -> unlines $ filterHeadlines $ lines s) ls'
  return $ drop 1 $ take 3 ls''
  where filterHeadlines lss = filter (\s -> isInfixOf "<a class=\"hll" s) lss
