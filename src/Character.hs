module Character (
    showPlayer,
    getAbilScore,
    getAbilMod,
    getAbilModPlus,
    getDefense
) where

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
    Ability(..),
    Skill(..),
    Defense(..))

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
    showDef AC player ++
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
