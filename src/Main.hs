import Instances.Prompt (prompt)

import Character (showPlayer)

main = do
    let player = prompt
    putStrLn $ showPlayer player
