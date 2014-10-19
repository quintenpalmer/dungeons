module Main where

import Data.Maybe (fromJust)

import Character (selectPlayer,
                  serializePlayerForTerminal)

main = do
    mPlayer <- selectPlayer "Prompt"
    putStrLn $ serializePlayerForTerminal $ fromJust mPlayer
