module Character.Util (
    getHalfLevel,
    getAbilModFromScore
) where

getHalfLevel :: Int -> Int
getHalfLevel level = (level `div` 2)

getAbilModFromScore :: Int -> Int
getAbilModFromScore abilScore = (abilScore - 10) `div` 2
