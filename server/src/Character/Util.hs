module Character.Util (
    getHalfLevel,
    getAbilModFromScore,
    readMaybe,
    allValues,
    splitTwice,
    splitOnce
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

splitTwice :: Char -> String -> (String, String, String)
splitTwice delim string =
    let (buf, mid) = sOnce delim string ""
        (buf2, rest) = sOnce delim mid "" in
    (buf, buf2, rest)

splitOnce :: Char -> String -> (String, String)
splitOnce delim string = sOnce delim string ""

sOnce :: Char -> String -> String -> (String, String)
sOnce _ [] buf = (buf, "")
sOnce delim (x:xs) buf = if delim == x
    then (buf, xs)
    else sOnce delim xs (buf ++ [x])
