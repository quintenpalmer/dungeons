module Character.Util (
    getHalfLevel,
    getAbilModFromScore,
    readMaybe,
    allValues
) where

getHalfLevel :: Int -> Int
getHalfLevel level = (level `div` 2)

getAbilModFromScore :: Int -> Int
getAbilModFromScore abilScore = (abilScore - 10) `div` 2

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound ..]
