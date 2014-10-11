{-# LANGUAGE OverloadedStrings #-}

module Character.Loader (
    loadPlayer
) where

import Data.Map (Map, toList)
import Control.Applicative ((<$>),
                            (<*>))
import Control.Monad (mzero)
import Data.ByteString.Lazy.Char8 (pack)

import Data.Aeson (decode,
                   Value(..),
                   FromJSON(..),
                   (.:))

import Character.Util (readMaybe)
import Character.Types (Player,
                        Race,
                        Feat(..),
                        Class,
                        Skill,
                        Ability,
                        newPlayer,
                        Armor(..),
                        ArmorType(..),
                        Weapons(..),
                        OneHanderType(..),
                        TwoHanderType(..),
                        TwoHander(..),
                        OneHander(..))
import Instances.Classes (druid)
import Instances.Races (halfling)

loadPlayer :: String -> Maybe Player
loadPlayer jsonString = do
    (FlatPlayer n mfs l x mc mr ma mw mts mbas) <- decode (pack jsonString)
    fs <- parseFeats mfs
    c <- parseClass mc
    r <- parseRace mr
    a <- parseArmor ma
    w <- parseWeapon mw
    ts <- parseSkills mts
    bas <- parseBaseAbilityScores mbas
    return $ newPlayer n fs l x r c a w ts bas

data FlatPlayer = FlatPlayer { name :: String
                             , feats :: [String]
                             , level :: Int
                             , xp :: Int
                             , class_ :: String
                             , race :: String
                             , armor :: String
                             , weapon :: String
                             , trainedSkills :: [String]
                             , baseAbilityScores :: (Map String Int) } deriving Show


instance FromJSON FlatPlayer where
    parseJSON (Object v) = FlatPlayer <$>
                           v .: "name" <*>
                           v .: "feats" <*>
                           v .: "level" <*>
                           v .: "xp" <*>
                           v .: "class_" <*>
                           v .: "race" <*>
                           v .: "armor" <*>
                           v .: "weapon" <*>
                           v .: "trainedSkills" <*>
                           v .: "baseAbilityScores"
    parseJSON _ = mzero


parseArmor :: String -> Maybe Armor
parseArmor "leather" = Just $ Armor Leather 2
parseArmor _ = Nothing

parseWeapon :: String -> Maybe Weapons
parseWeapon "staff" = Just $ TwoHandedWeapon $ TwoHander Staff 8
parseWeapon _ = Nothing

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

parseFeats :: [String] -> Maybe [Feat]
parseFeats [] = Just []
parseFeats (x:xs) = do
    feat <- parseFeat x
    fs <- parseFeats xs
    return (feat:fs)

parseFeat :: String -> Maybe Feat
parseFeat "enragedBoarForm" = Just $ Feat "Enraged Boar Form" "+1 attack, +2 damage when charging in beast form"
parseFeat "ritualCasting" = Just $ Feat "Ritual Casting" "Animal Messanger"
parseFeat "improvedTiger" = Just $ Feat "Improved Tiger" "+2 damage with combat advantage in beast form"
parseFeat "primalFury" = Just $ Feat "Primal Fury" "+1 to attacks with primal powers against bloodied enemies"
parseFeat _ = Nothing

parseBaseAbilityScores :: (Map String Int) -> Maybe [(Ability, Int)]
parseBaseAbilityScores baseScores = parseHelper $ toList baseScores

parseHelper :: [(String, Int)] -> Maybe [(Ability, Int)]
parseHelper [] = Just []
parseHelper ((abilName, val):xs) = do
    ability <- readMaybe abilName
    abilities <- parseHelper xs
    return ((ability, val):abilities)
