{-# LANGUAGE OverloadedStrings #-}

module Character.Serialize (
    serializePlayerForNetwork,
    serializePlayerForTerminal
) where

import Data.Map (Map, empty, fromList)

import Character.Types (
    Player(..),
    Ability(..),
    Skill(..),
    Defense(..))
import Character.Util (allValues)
import Character.Player (
    getAttribute,
    getAbilScore,
    getAbilMod,
    getAbilModPlus,
    getSkill,
    getDefense,
    getInitiative,
    getSpeed,
    getHealth)

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
                                   , level :: Int
                                   , initiative :: Int
                                   , hitPoints :: Int
                                   , speed :: Int
                                   , abilityScores :: (Map String Int)
                                   , abilityMods :: (Map String Int)
                                   , abilityModsPlus :: (Map String Int)
                                   , skills :: (Map String Int)
                                   , defenses :: (Map String Int) }

instance ToJSON NetworkPlayer where
   toJSON np = object [
        "name" .= name np,
        "level" .= level np,
        "initiative" .= initiative np,
        "hitPoints" .= hitPoints np,
        "speed" .= speed np,
        "abilityScores" .= abilityScores np,
        "abilityMods" .= abilityMods np,
        "abilityModsPlus" .= abilityModsPlus np,
        "skills" .= skills np,
        "defenses" .= defenses np]

buildNetworkPlayer :: Player -> NetworkPlayer
buildNetworkPlayer player =
    let nName = getName player
        nLevel = getLevel player
        nInitiative = getInitiative player
        nHitPoints = getHealth player
        nSpeed = getSpeed player
        nAbilScores = getAbilityScores player
        nAbilMods = getAbilityMods player
        nAbilModsPlus = getAbilityModsPlus player
        nSkills = getSkills player
        nDefenses = getDefenses player in
    NetworkPlayer nName nLevel nInitiative nHitPoints nSpeed nAbilScores nAbilMods nAbilModsPlus nSkills nDefenses


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
