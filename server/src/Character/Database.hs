{-# LANGUAGE OverloadedStrings #-}

module Character.Database (
    selectPlayer,
    updatePlayer
) where

import Control.Applicative ((<$>), (<*>))
import Database.SQLite.Simple (FromRow(..),
                               Connection(..),
                               Query(..),
                               Only(..),
                               field,
                               open,
                               query,
                               execute,
                               close)
import Data.Map (Map, fromList, lookup)
import Prelude hiding (lookup)
import Data.Text (pack)
import Character.Util (readMaybe)
import Character.Types
import Instances.Classes
import Instances.Races

selectPlayer :: String -> IO (Maybe Player)
selectPlayer inputName = do
    conn <- open "dungeons.db"
    dbps <- ((query conn "SELECT * from character WHERE name = ?" (Only inputName)) :: IO [DBPlayer])
    case dbps of
        [dbp] -> do
            let p = pid dbp

            descIds <- (subSelect "featsToCharacters" conn p :: IO [AToB])
            feats <- getDescribedList descIds "feats" conn

            miIds <- (subSelect "magicItemsToCharacters" conn p :: IO [AToB])
            mis <- getDescribedList miIds "magicItems" conn

            powerIds <- (subSelect "powersToCharacters" conn p :: IO [AToB])
            powers <- getDescribedList powerIds "powers" conn

            skillIds <- (subSelect "skillsToCharacters" conn p :: IO [AToB])

            items <- (subSelect "itemsToCharacters" conn p :: IO [Counted])

            close conn
            return $ buildPlayer dbp feats mis powers skillIds items
        _ -> return Nothing

subSelect :: FromRow r => [Char] -> Connection -> Int -> IO [r]
subSelect tableName conn p = query conn (Query $ pack $ "SELECT * FROM " ++ tableName ++ " WHERE charId = ?") (Only p)

buildPlayer :: DBPlayer -> [Described] -> [Described] -> [Described] -> [AToB] -> [Counted] -> Maybe Player
buildPlayer dbp dbfeats dbmis dbpowers dbskills dbitems = do
    r <- parseRace (race dbp)
    c <- parseClass (class_ dbp) (classSpec dbp)
    a <- parseArmor (armor dbp)
    w <- parseWeapon (weapon dbp)
    sk <- parseSkills dbskills
    return $ newPlayer (name dbp) (buildFeats dbfeats) (level dbp) (xp dbp) r c a w (buildMagicItems dbmis) (buildItems dbitems) (buildPowers dbpowers) sk (buildSkills dbp)


updatePlayer :: Map String String -> String -> IO ()
updatePlayer params playerName = do
    conn <- open "dungeons.db"
    case getKeyVal params of
        Just (key, val) -> do
            dbPlayers <- ((query conn "SELECT * FROM character WHERE name = ?" (Only playerName)) :: IO [DBPlayer])
            case dbPlayers of
                [dbPlayer] -> updatePlayerParser key val conn $ pid dbPlayer
                _ -> return ()
        Nothing -> return ()

getKeyVal :: (Map String String) -> Maybe (String, Int)
getKeyVal params = do
    key <- lookup "key" params
    val <- lookup "value" params
    intVal <- readMaybe val :: Maybe Int
    return (key, intVal)

updatePlayerParser :: String -> Int -> Connection -> Int -> IO ()
updatePlayerParser key val conn p = do
    case key of
        "xp" -> do
            execute conn (Query $ pack $ "UPDATE character SET xp = " ++ show val ++ " WHERE charId = ?") (Only p)
        _ -> do
            itemCount <- ((query conn "SELECT * FROM itemsToCharacters WHERE name = ? AND charId = ?" (key, p)) :: IO [Counted])
            case itemCount of
                [] -> execute conn (Query $ pack $ "INSERT INTO itemsToCharacters VALUES (?, ?, ?)") (key, val, p)
                _ -> case val of
                        0 -> execute conn (Query $ pack $ "DELETE FROM itemsToCharacters WHERE name = ? AND charId = ?") (key, p)
                        _ -> execute conn (Query $ pack $ "UPDATE itemsToCharacters SET count = " ++ show val ++ " WHERE name = '" ++ key ++ "' AND charId = ?") (Only p)

-- DB Data Structures

data DBPlayer = DBPlayer { pid :: Int
                         , name :: String
                         , level :: Int
                         , xp :: Int
                         , class_ :: String
                         , classSpec :: String
                         , race :: String
                         , str :: Int
                         , con :: Int
                         , dex :: Int
                         , int :: Int
                         , wis :: Int
                         , cha :: Int
                         , armor :: String
                         , weapon :: String } deriving (Show)

instance FromRow DBPlayer where
    fromRow = DBPlayer <$>
        field <*> field <*> field <*> field <*>
        field <*> field <*> field <*> field <*>
        field <*> field <*> field <*> field <*>
        field <*> field <*> field

data Described = Described { descId :: String
                           , descName :: String
                           , descDescription :: String }

instance FromRow Described where
    fromRow = Described <$> field <*> field <*> field

data Counted = Counted { countedName :: String
                       , countedCount :: Int
                       , countedCharId :: Int }

instance FromRow Counted where
    fromRow = Counted <$> field <*> field <*> field

data AToB = AToB { aToBDesc :: String
                 , aToBChar :: Int }

instance FromRow AToB where
    fromRow = AToB <$> field <*> field

getDescribedList :: [AToB] -> String -> Connection -> IO [Described]
getDescribedList [] _ _ = return []
getDescribedList (x:xs) tableName conn = do
    let selectStatement = (Query $ pack $ "SELECT * from " ++ tableName ++ " WHERE id = ?")
    feat <- ((query conn selectStatement (Only $ aToBDesc x)) :: IO [Described])
    feats <- getDescribedList xs tableName conn
    return $ (head feat):feats

-- DB -> Internal Data Structure Translators

buildSkills :: DBPlayer -> (Map Ability Int)
buildSkills dbp = fromList [
    (Str, str dbp),
    (Con, con dbp),
    (Dex, dex dbp),
    (Int, int dbp),
    (Wis, wis dbp),
    (Cha, cha dbp)]

buildFeats :: [Described] -> [Feat]
buildFeats [] = []
buildFeats (x:xs) = (Feat (descName x) (descDescription x)):(buildFeats xs)

buildMagicItems :: [Described] -> [MagicItem]
buildMagicItems [] = []
buildMagicItems (x:xs) = (MagicItem (descName x) (descDescription x)):(buildMagicItems xs)

buildPowers :: [Described] -> [Power]
buildPowers [] = []
buildPowers (x:xs) = (Power (descName x) (descDescription x)):(buildPowers xs)

buildItems :: [Counted] -> (Map Item Int)
buildItems items = fromList $ buildItems_ items

buildItems_ :: [Counted] -> [(Item, Int)]
buildItems_ [] = []
buildItems_ (x:xs) = ((Item $ countedName x, countedCount x)):(buildItems_ xs)

parseSkills :: [AToB] -> Maybe [Skill]
parseSkills [] = Just []
parseSkills (x:xs) = do
    sk <- readMaybe $ aToBDesc x
    sks <- parseSkills xs
    return $ sk:sks

parseArmor :: String -> Maybe Armor
parseArmor "light" = Just $ Armor Light 1
parseArmor _ = Nothing

parseWeapon :: String -> Maybe Weapons
parseWeapon "staff" = Just $ TwoHandedWeapon $ TwoHander Staff 8
parseWeapon _ = Nothing

parseRace :: String -> Maybe Race
parseRace "halfling" = Just halfling
parseRace  _ = Nothing

parseClass :: String -> String -> Maybe Class
parseClass "druid" "primalPredator" = Just $ druid primalPredator
parseClass _ _ = Nothing
