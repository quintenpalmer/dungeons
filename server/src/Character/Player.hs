module Character.Player (
    getRaceName,
    getClassName,
    getClassSpecName,
    getAttribute,
    getInitiative,
    getAbilScore,
    getAbilMod,
    getAbilModPlus,
    getDefense,
    getSpeed,
    getPassiveInsight,
    getPassivePerception,
    getSkill,
    getHealth,
    getBloodied,
    getSurgeValue,
    getSurgesPerDay,
    getFeats,
    getFeatName,
    getFeatDescription,
    getItems,
    getMagicItems,
    getMagicItemName,
    getMagicItemDescription,
    getPowers,
    getPowerName,
    getPowerDescription,
    getArmorName,
    getWeaponName
) where

import Data.Map (Map)
import Character.Util (
    getHalfLevel,
    getAbilModFromScore,
    readMaybe)
import Character.Types (
    Player(..),
    Class(..),
    Race(..),
    Feat(..),
    MagicItem(..),
    Item(..),
    Power(..),
    getRacialSkill,
    getRacialAbility,
    getRacialSpeed,
    getClassDefense,
    getRawArmorName,
    getRawWeaponName,
    getHealth,
    getSkillAbil,
    getTrainedSkillAmount,
    Ability(..),
    Skill(..),
    Defense(..),
    Armor(..),
    ArmorType(..),
    Weapons(..),
    getAcFromArmor,
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

getRaceName :: Player -> String
getRaceName player = raceName $ getRace player

getClassName :: Player -> String
getClassName player = className $ getClass player

getClassSpecName :: Player -> String
getClassSpecName player = show $ getRawClassSpec $ getClass player

getSpeed :: Player -> Int
getSpeed player =
    (getRacialSpeed $ getRace player) +
    (sum (map (\x -> x player) (getSpeedMisc player)))

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
        (getDefenseModifiers defense))) +
    (getArmorIfAc defense player)

getArmorIfAc :: Defense -> Player -> Int
getArmorIfAc Ac player = getAcFromArmor $ getArmor player
getArmorIfAc _ _ = 0

getArmorName :: Player -> String
getArmorName player = getRawArmorName $ getArmor player

getWeaponName :: Player -> String
getWeaponName player = getRawWeaponName $ getWeapons player

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

getFeats :: Player -> [Feat]
getFeats player = getRawFeats player

getFeatName :: Feat -> String
getFeatName = getRawFeatName

getFeatDescription :: Feat -> String
getFeatDescription = getRawFeatDescription

getMagicItems :: Player -> [MagicItem]
getMagicItems player = getRawMagicItems player

getItems :: Player -> (Map Item Int)
getItems player = getRawItems player

getMagicItemName :: MagicItem -> String
getMagicItemName = getRawMagicItemName

getMagicItemDescription :: MagicItem -> String
getMagicItemDescription = getRawMagicItemDescription

getPowers :: Player -> [Power]
getPowers player = getRawPowers player

getPowerName :: Power -> String
getPowerName = getRawPowerName

getPowerDescription :: Power -> String
getPowerDescription = getRawPowerDescription

getBloodied :: Player -> Int
getBloodied player = getHealth player `div` 2

getSurgeValue :: Player -> Int
getSurgeValue player = getBloodied player `div` 2

getSurgesPerDay :: Player -> Int
getSurgesPerDay player = (getAbilMod Con player) + 7

getPassiveInsight :: Player -> Int
getPassiveInsight player = (getSkill Insight player) + 10

getPassivePerception :: Player -> Int
getPassivePerception player = (getSkill Perception player) + 10

getHalfPlayerLevel :: Player -> Int
getHalfPlayerLevel player = (getHalfLevel $ getLevel player)
