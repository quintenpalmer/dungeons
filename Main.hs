module Main where

import Data.Maybe (fromJust)

import Character (loadPlayer,
                  serializePlayerForTerminal)

main = do
    let filename = "prompt"
    jsonString <- readFile $ "data/" ++ filename ++ ".json"
    let mPlayer = fromJust $ loadPlayer jsonString
    putStrLn $ serializePlayerForTerminal mPlayer
