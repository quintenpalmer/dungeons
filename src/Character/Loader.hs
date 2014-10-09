{-# LANGUAGE OverloadedStrings #-}

module Character.Loader (
    loadPlayer
) where

import Data.Map (Map, toList)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>),
                            (<*>))
import Control.Monad (mzero)
import Data.ByteString.Lazy.Char8 (pack)

import Data.Aeson (decode,
                   Object(..),
                   Value(..),
                   FromJSON(..),
                   (.:))

import Character (readMaybe)
import Character.Types (Player,
                        Race,
                        Class,
                        Skill,
                        Ability,
                        newPlayer)
import Instances.Classes (druid)
import Instances.Races (halfling)

loadPlayer :: String -> Maybe Player
loadPlayer jsonString = do
    (FlatPlayer n l mc mr mts mbas) <- decode (pack jsonString)
    c <- parseClass mc
    r <- parseRace mr
    ts <- parseSkills mts
    bas <- parseBaseAbilityScores mbas
    return $ newPlayer n l r c ts bas

data FlatPlayer = FlatPlayer { name :: String
                             , level :: Int
                             , class_ :: String
                             , race :: String
                             , trainedSkills :: [String]
                             , baseAbilityScores :: (Map String Int) } deriving Show


instance FromJSON FlatPlayer where
    parseJSON (Object v) = FlatPlayer <$>
                           v .: "name" <*>
                           v .: "level" <*>
                           v .: "class_" <*>
                           v .: "race" <*>
                           v .: "trainedSkills" <*>
                           v .: "baseAbilityScores"
    parseJSON _ = mzero


parseRace :: String -> Maybe Race
parseRace "halfling" = Just halfling
parseRace  _ = Nothing

parseClass :: String -> Maybe Class
parseClass "druid" = Just druid
parseClass _ = Nothing

parseSkills :: [String] -> Maybe [Skill]
parseSkills [] = Just []
parseSkills (x:xs) = do
    skill <- readMaybe x
    skills <- parseSkills xs
    return (skill:skills)

parseBaseAbilityScores :: (Map String Int) -> Maybe [(Ability, Int)]
parseBaseAbilityScores map = parseHelper $ toList map

parseHelper :: [(String, Int)] -> Maybe [(Ability, Int)]
parseHelper [] = Just []
parseHelper ((name, val):xs) = do
    ability <- readMaybe name
    abilities <- parseHelper xs
    return ((ability, val):abilities)
