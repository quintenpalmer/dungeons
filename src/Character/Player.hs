module Character.Player (
    getInitiative,
    getAbilScore,
    getAbilMod,
    getAbilModPlus,
    getDefense,
    getSpeed,
    getHealth,
    getSkill
) where

import Character.Util (
    getHalfLevel,
    getAbilModFromScore)
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

getHalfPlayerLevel :: Player -> Int
getHalfPlayerLevel player = (getHalfLevel $ getLevel player)
