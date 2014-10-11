{-# LANGUAGE OverloadedStrings #-}

module Character.Serialize (
    serializePlayerForNetwork,
    serializePlayerForTerminal
) where

import Data.Map (Map, fromList)

import Character.Types (
    Player(..),
    Ability(..),
    Skill(..),
    Defense(..))
import Character.Util (allValues)
import Character.Player (
    getRaceName,
    getClassName,
    getArmorName,
    getWeaponName,
    getAbilScore,
    getAbilMod,
    getAbilModPlus,
    getSkill,
    getDefense,
    getInitiative,
    getPassiveInsight,
    getPassivePerception,
    getSpeed,
    getHealth,
    getFeats,
    getFeatName,
    getFeatDescription,
    getBloodied,
    getSurgeValue,
    getSurgesPerDay)

import Data.ByteString.Lazy.Char8 (unpack)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson(encode,
                  ToJSON(..),
                  object,
                  (.=))

serializePlayerForNetwork :: Player -> String
serializePlayerForNetwork player = unpack $ encode $ buildNetworkPlayer player

serializePlayerForTerminal :: Player -> String
serializePlayerForTerminal player = unpack $ encodePretty $ buildNetworkPlayer player

data NetworkPlayer = NetworkPlayer { name :: String
                                   , race :: String
                                   , class_ :: String
                                   , level :: Int
                                   , xp :: Int
                                   , initiative :: Int
                                   , hitPoints :: Int
                                   , bloodied :: Int
                                   , surgeValue :: Int
                                   , surgesPerDay :: Int
                                   , speed :: Int
                                   , passiveInsight :: Int
                                   , passivePerception :: Int
                                   , abilityScores :: (Map String Int)
                                   , abilityMods :: (Map String Int)
                                   , abilityModsPlus :: (Map String Int)
                                   , skills :: (Map String Int)
                                   , defenses :: (Map String Int)
                                   , feats :: (Map String String)
                                   , armor :: String
                                   , weapons :: String
                                   }

instance ToJSON NetworkPlayer where
   toJSON np = object [
        "name" .= name np,
        "race" .= race np,
        "class_" .= class_ np,
        "level" .= level np,
        "xp" .= xp np,
        "initiative" .= initiative np,
        "hitPoints" .= hitPoints np,
        "bloodied" .= bloodied np,
        "surgeValue" .= surgeValue np,
        "surgesPerDay" .= surgesPerDay np,
        "speed" .= speed np,
        "passiveInsight" .= passiveInsight np,
        "passivePerception" .= passivePerception np,
        "abilityScores" .= abilityScores np,
        "abilityMods" .= abilityMods np,
        "abilityModsPlus" .= abilityModsPlus np,
        "skills" .= skills np,
        "defenses" .= defenses np,
        "feats" .= feats np,
        "armor" .= armor np,
        "weapons" .= weapons np]

buildNetworkPlayer :: Player -> NetworkPlayer
buildNetworkPlayer player =
    NetworkPlayer
        (getName player)
        (getRaceName player)
        (getClassName player)
        (getLevel player)
        (getXp player)
        (getInitiative player)
        (getHealth player)
        (getBloodied player)
        (getSurgeValue player)
        (getSurgesPerDay player)
        (getSpeed player)
        (getPassiveInsight player)
        (getPassivePerception player)
        (getAbilityScores player)
        (getAbilityMods player)
        (getAbilityModsPlus player)
        (getSkills player)
        (getDefenses player)
        (getFeatMap player)
        (getArmorName player)
        (getWeaponName player)


getFeatMap :: Player -> (Map String String)
getFeatMap player = fromList $ map (\x -> (getFeatName x, getFeatDescription x)) (getFeats player)

getAbilityScores :: Player -> (Map String Int)
getAbilityScores player = fromList $ map (\x -> (show x, getAbilScore x player)) (allValues :: [Ability])

getAbilityMods :: Player -> (Map String Int)
getAbilityMods player = fromList $ map (\x -> (show x, getAbilMod x player)) (allValues :: [Ability])

getAbilityModsPlus :: Player -> (Map String Int)
getAbilityModsPlus player = fromList $ map (\x -> (show x, getAbilModPlus x player)) (allValues :: [Ability])

getSkills :: Player -> (Map String Int)
getSkills player = fromList $ map (\x -> (show x, getSkill x player)) (allValues :: [Skill])

getDefenses :: Player -> (Map String Int)
getDefenses player = fromList $ map (\x -> (show x, getDefense x player)) (allValues :: [Defense])
