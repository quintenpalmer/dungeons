module Character.Player (
    getAttribute,
    getInitiative,
    getAbilScore,
    getAbilMod,
    getAbilModPlus,
    getDefense,
    getSpeed,
    getSkill,
    getHealth,
    getBloodied,
    getSurgeValue,
    getSurgesPerDay
) where

import Character.Util (
    getHalfLevel,
    getAbilModFromScore,
    readMaybe)
import Character.Types (
    Player(..),
    getRacialSkill,
    getRacialAbility,
    getRacialSpeed,
    getClassDefense,
    getHealth,
    getSkillAbil,
    getTrainedSkillAmount,
    Ability(..),
    Skill(..),
    Defense(..),
    getRawAbilityScore)

getAttribute :: String -> String -> Player -> Maybe String
getAttribute request param player = case request of
    "name" -> Just $ getName player
    "level" -> Just $ show $ getLevel player
    "initiative" -> Just $ show $ getInitiative player
    "speed" -> Just $ show $ getSpeed player
    "health" -> Just $ show $ getHealth player
    "abilityScore" -> case readMaybe param of
        Just ability -> Just $ show $ getAbilScore ability player
        Nothing -> Nothing
    "abilityMod" -> case readMaybe param of
        Just ability -> Just $ show $ getAbilMod ability player
        Nothing -> Nothing
    "abilityModPlus" -> case readMaybe param of
        Just ability -> Just $ show $ getAbilModPlus ability player
        Nothing -> Nothing
    "skill" -> case readMaybe param of
        Just skill -> Just $ show $ getSkill skill player
        Nothing -> Nothing
    "defense" -> case readMaybe param of
        Just def -> Just $ show $ getDefense def player
        Nothing -> Nothing
    _ -> Nothing

getSpeed :: Player -> Int
getSpeed player =
    getRacialSpeed $ getRace player

getSkill :: Skill -> Player -> Int
getSkill skill player =
    (getAbilModPlus (getSkillAbil skill) player) +
    (getTrainedSkillAmount skill player) +
    (getRacialSkill skill (getRace player))

getAbilScore :: Ability -> Player -> Int
getAbilScore abil player =
    (getRacialAbility abil (getRace player)) +
    (getRawAbilityScore abil $ getRawBaseStats player)

getAbilMod :: Ability -> Player -> Int
getAbilMod abil player =
    (getAbilModFromScore $ getAbilScore abil player)

getAbilModPlus :: Ability -> Player -> Int
getAbilModPlus abil player =
    (getHalfPlayerLevel player) +
    (getAbilMod abil player)

getDefense :: Defense -> Player -> Int
getDefense defense player =
    10 +
    (getClassDefense defense $ getClass player) +
    (getHalfPlayerLevel player) +
    (maximum (map
        ((flip getAbilMod) player)
        (getDefenseModifiers defense)))

getDefenseModifiers :: Defense -> [Ability]
getDefenseModifiers defense = case defense of
    Fort -> [Str, Con]
    Ref -> [Dex, Int]
    Will -> [Wis, Cha]
    Ac -> [Dex, Int]

getInitiative :: Player -> Int
getInitiative player =
    (getAbilMod Dex player) +
    (getHalfPlayerLevel player)

getBloodied :: Player -> Int
getBloodied player = getHealth player `div` 2

getSurgeValue :: Player -> Int
getSurgeValue player = getBloodied player `div` 2

getSurgesPerDay :: Player -> Int
getSurgesPerDay player = (getAbilMod Con player) + 7

getHalfPlayerLevel :: Player -> Int
getHalfPlayerLevel player = (getHalfLevel $ getLevel player)
