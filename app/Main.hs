module Main where

--import Data.List (isInfixOf)
--import Data.List.Split (splitOn)
import Lib
import LibFetch

main :: IO ()
main = startApp
--main = regularFetch
--main = do
--  news <- fetchInterNews
--  putStrLn $ unlines news

regularFetch = do
  content <- get url
  -- putStrLn content
  --let ls = lines content
  --putStrLn $ unlines $ filter (\s -> isInfixOf "<div" s) ls
  putStrLn $ filterContent "" content
  code <- getCode url
  putStrLn ("Return code of calling [" ++ url ++ "] is: " ++ show code)
  where url = "http://sadmir.se/"

  