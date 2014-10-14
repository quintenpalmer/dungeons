module Main where

import Data.Maybe (fromJust)

import Character (loadPlayer,
                  serializePlayerForTerminal)

main = do
    mPlayer <- loadPlayer "prompt"
    putStrLn $ serializePlayerForTerminal $ fromJust mPlayer
