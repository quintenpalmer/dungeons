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
                        MagicItem(..),
                        Power(..),
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
    (FlatPlayer n mfs mmis mps l x mc mr ma mw mts mbas) <- decode (pack jsonString)
    fs <- parseFeats mfs
    mis <- parseMagicItems mmis
    ps <- parsePowers mps
    c <- parseClass mc
    r <- parseRace mr
    a <- parseArmor ma
    w <- parseWeapon mw
    ts <- parseSkills mts
    bas <- parseBaseAbilityScores mbas
    return $ newPlayer n fs l x r c a w mis ps ts bas

data FlatPlayer = FlatPlayer { name :: String
                             , feats :: [String]
                             , magicItems :: [String]
                             , powers :: [String]
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
                           v .: "magicItems" <*>
                           v .: "powers" <*>
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

parseMagicItems :: [String] -> Maybe [MagicItem]
parseMagicItems [] = Just []
parseMagicItems (x:xs) = do
    magicItem <- parseMagicItem x
    ms <- parseMagicItems xs
    return (magicItem:ms)

parseMagicItem :: String -> Maybe MagicItem
parseMagicItem "autumnHarvestTotem" = Just $ MagicItem "Autumn Harvest Totem" "On crit +1d6 per plus or +1d10 damage per plus vs a bloodied creature. - Attacks made through this item do extra damage against bloodied creatures equal to 1 + 1/2 enchantment"
parseMagicItem "pouncingBeastArmor" = Just $ MagicItem "Pouncing Beast Armor" "+1 AC (Hide Armor) - The Armor urges you to attack. - Wild Shape: on transform you shift +1 square. - Power (Daily): Shift up to 5 squares on transformation, MUST end adjacent to enemy"
parseMagicItem "emeraldStaff" = Just $ MagicItem "Emerald Staff" "It glows with an errie light that catches the eye. Gold elvish runes inscripted on the handle. - Major Action (Encounter Power): Can dominate animal of lesser level (Wisdom vs. Will) x 1/2 animal level"
parseMagicItem _ = Nothing

parsePowers :: [String] -> Maybe [Power]
parsePowers [] = Just []
parsePowers (x:xs) = do
    magicItem <- parsePower x
    ms <- parsePowers xs
    return (magicItem:ms)

parsePower :: String -> Maybe Power
parsePower "wildShape" = Just $ Power "Wild Shape" "Effect: You change from your humanoid form to best form or vie versa. When you change from beast form back to munaoid form, you shift 2 square. While you are in best form you can't use attack utility of reat power that lack the best form keyword, although you can sustain such powers. - You choose a speicific form whenever you use wild shape to change into best form. The best form is your size, resembles a natural best for fey best, and normally doesn't change your game statistics or momement modes. Your equipment becomes part of your beast form, but you drop andything you holding except implements you can use. You continute to gainthe benefits of the equipment you wear. - You can use the porpertiest of the powers of implements as well as magic items that you wear, but not the properties or powers of weapons or the powers of wonderous items. While quirepment is part of your best form, it cannot be removed and anything in a container that is part of your best form is inaccessible"
parsePower _ = Nothing

parseBaseAbilityScores :: (Map String Int) -> Maybe [(Ability, Int)]
parseBaseAbilityScores baseScores = parseHelper $ toList baseScores

parseHelper :: [(String, Int)] -> Maybe [(Ability, Int)]
parseHelper [] = Just []
parseHelper ((abilName, val):xs) = do
    ability <- readMaybe abilName
    abilities <- parseHelper xs
    return ((ability, val):abilities)
