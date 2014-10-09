module Character (
    Player,
    newPlayer,
    showPlayer,
    getAttribute,
    serializePlayerForNetwork,
    serializePlayerForTerminal,
    readMaybe
) where

import Character.Util (
    readMaybe)
import Character.Player (
    getAbilScore,
    getAbilMod,
    getAbilModPlus,
    getSkill,
    getDefense,
    getInitiative,
    getSpeed,
    getHealth)
import Character.Types (
    Player(..),
    newPlayer,
    Ability(..),
    Skill(..),
    Defense(..))
import Character.Serialize (
    serializePlayerForNetwork,
    serializePlayerForTerminal)

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

showPlayer :: Player -> String
showPlayer player = "Player:" ++
    "\nName: " ++ getName player ++
    "\nLevel: " ++ show (getLevel player) ++
    "\nInitiative: " ++ show (getInitiative player) ++
    "\nSpeed: " ++ show (getSpeed player) ++
    "\nAbility Scores: " ++
    showAbil Str player ++
    showAbil Con player ++
    showAbil Dex player ++
    showAbil Int player ++
    showAbil Wis player ++
    showAbil Cha player ++
    "\nDefenses:" ++
    showDef Ac player ++
    showDef Fort player ++
    showDef Ref player ++
    showDef Will player ++
    "\nHealth: " ++ show (getHealth player) ++
    "\nSkills:" ++
    showSkill Acrobatics player ++
    showSkill Arcana player ++
    showSkill Athletics player ++
    showSkill Bluff player ++
    showSkill Diplomacy player ++
    showSkill Dungeoneering player ++
    showSkill Endurance player ++
    showSkill Heal player ++
    showSkill History player ++
    showSkill Insight player ++
    showSkill Intimidate player ++
    showSkill Nature player ++
    showSkill Perception player ++
    showSkill Religion player ++
    showSkill Stealth player ++
    showSkill Streetwise player ++
    showSkill Thievery player

showAbil :: Ability -> Player -> String
showAbil abil player =
    "\n\t" ++ show abil ++ ": " ++ show (getAbilScore abil player) ++
        " -> " ++ show (getAbilMod abil player) ++
        " (" ++ show (getAbilModPlus abil player) ++ ")"

showDef :: Defense -> Player -> String
showDef def player =
    "\n\t" ++ show def ++ ": " ++ show (getDefense def player)

showSkill :: Skill -> Player -> String
showSkill skill player =
    "\n\t" ++ show skill ++ ": " ++ show (getSkill skill player)
