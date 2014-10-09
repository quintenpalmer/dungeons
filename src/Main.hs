import Data.Maybe (fromJust)

import Character.Loader (loadPlayer)
import Character (showPlayer)

main = do
    let filename = "prompt"
    jsonString <- readFile $ "data/" ++ filename ++ ".json"
    let mPlayer = fromJust $ loadPlayer jsonString
    putStrLn $ showPlayer mPlayer
