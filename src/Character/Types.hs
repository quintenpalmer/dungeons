module Character.Types (
    Player(..),
    newPlayer,
    getRawAbilityScore,
    getTrainedSkillAmount,
    getHealth,

    Skill(..),
    getSkillAbil,

    Ability(..),
    Defense(..),

    Race(..),
    newRace,
    getRacialSkill,
    getRacialAbility,
    getRacialSpeed,

    Armor(..),
    ArmorType(..),
    Weapons(..),
    OneHanderType(..),
    TwoHanderType(..),
    TwoHander(..),
    OneHander(..),
    getRawWeaponName,
    getRawArmorName,
    getAcFromArmor,

    Class(..),
    getClassDefense,
    newClass,

    Feat(..),
    MagicItem(..),
    Item(..),
    Power(..)
) where

import Data.Map (
    Map,
    fromList,
    (!),
    findWithDefault)

data Player = Player { getName :: String
                     , getRawFeats :: [Feat]
                     , getLevel :: Int
                     , getXp :: Int
                     , getRace :: Race
                     , getClass :: Class
                     , getArmor :: Armor
                     , getWeapons :: Weapons
                     , getRawMagicItems :: [MagicItem]
                     , getRawItems :: (Map Item Int)
                     , getRawPowers :: [Power]
                     , getRawTrainedSkills :: [Skill]
                     , getRawBaseStats :: BaseStats }

newPlayer :: String -> [Feat] -> Int -> Int ->
             Race -> Class -> Armor -> Weapons ->
             [MagicItem] -> (Map Item Int) -> [Power] -> [Skill] -> (Map Ability Int) ->
             Player
newPlayer name feats level xp race class_ armor weapon magicItems items power trainedSkills baseStats =
    Player name feats level xp race class_ armor weapon magicItems items power trainedSkills $ BaseStats $ baseStats

data Skill = Acrobatics |
             Arcana |
             Athletics |
             Bluff |
             Diplomacy |
             Dungeoneering |
             Endurance |
             Heal |
             History |
             Insight |
             Intimidate |
             Nature |
             Perception |
             Religion |
             Stealth |
             Streetwise |
             Thievery deriving (Ord, Eq, Show, Read, Enum, Bounded)

getSkillAbil :: Skill -> Ability
getSkillAbil skill = skillAbilMap ! skill
    where skillAbilMap = fromList [
            (Acrobatics, Dex),
            (Arcana, Int),
            (Athletics, Str),
            (Bluff, Cha),
            (Diplomacy, Cha),
            (Dungeoneering, Wis),
            (Endurance, Con),
            (Heal, Wis),
            (History, Int),
            (Insight, Wis),
            (Intimidate, Cha),
            (Nature, Wis),
            (Perception, Wis),
            (Religion, Int),
            (Stealth, Dex),
            (Streetwise, Cha),
            (Thievery, Dex)]

data Ability = Str |
               Con |
               Dex |
               Int |
               Wis |
               Cha deriving (Ord, Eq, Show, Read, Enum, Bounded)

data Defense = Ac |
               Fort |
               Ref |
               Will deriving (Ord, Eq, Show, Read, Enum, Bounded)

getHealth :: Player -> Int
getHealth player =
    (getStartingHealthValue (getClass player)) +
    ((getHealthPerLevel (getClass player)) * (getLevel player))

getTrainedSkillAmount :: Skill -> Player -> Int
getTrainedSkillAmount skill player =
    if skill `elem` (getRawTrainedSkills player)
    then 5
    else 0

-- Race

data Race = Race { raceName :: String
                 , getRawRacialSkills :: SkillMap
                 , getRawRacialAbilities :: AbilityMap
                 , getRawRacialSpeed :: Int }

data SkillMap = SkillMap (Map Skill Int)

data AbilityMap = AbilityMap (Map Ability Int)

newRace :: String -> [(Skill, Int)] -> [(Ability, Int)] -> Int -> Race
newRace name skills abilities speed =
    Race name
         (SkillMap (fromList skills))
         (AbilityMap (fromList abilities))
         speed

getRacialSpeed :: Race -> Int
getRacialSpeed = getRawRacialSpeed

getRacialSkill :: Skill -> Race -> Int
getRacialSkill skill race = let (SkillMap skillMap) = getRawRacialSkills race in
    findWithDefault 0 skill skillMap

getRacialAbility :: Ability -> Race -> Int
getRacialAbility abil race = let (AbilityMap abilMap) = getRawRacialAbilities race in
    findWithDefault 0 abil abilMap

-- Class

data Class = Class { className :: String
                   , getRawClassDefenses :: DefenseMap
                   , getStartingHealthValue :: Int
                   , getHealthPerLevel :: Int }

data DefenseMap = DefenseMap (Map Defense Int)

newClass :: String -> [(Defense, Int)] -> Int -> Int -> Class
newClass name defenses startingHealth healthPerLevel =
    Class name (DefenseMap $ fromList defenses) startingHealth healthPerLevel

getClassDefense :: Defense -> Class -> Int
getClassDefense def class_ = let (DefenseMap defMap) = getRawClassDefenses class_ in
    findWithDefault 0 def defMap

-- Armor/Weapon

data Armor = Armor ArmorType Int

instance Show Armor where
    show (Armor a _) = show a

data ArmorType = Heavy
               | Leather
               | Light deriving (Show)

getRawArmorName :: Armor -> String
getRawArmorName = show

getAcFromArmor :: Armor -> Int
getAcFromArmor (Armor _ val) = val

data Weapons = TwoHandedWeapon TwoHander
            | OneHandedWeapons OneHander OneHander

data TwoHander = TwoHander TwoHanderType Int

data TwoHanderType = BigAxe
                   | BigSword
                   | CrossBow
                   | Staff deriving (Show)

data OneHander = OneHander OneHanderType Int

data OneHanderType = SmallAxe
                   | SmallSword deriving (Show)

getRawWeaponName :: Weapons -> String
getRawWeaponName (TwoHandedWeapon t) = show t
getRawWeaponName (OneHandedWeapons l r) = show l ++ " + " ++ show r

instance Show TwoHander where
    show (TwoHander t _) = case t of
        BigAxe -> "Two Handed Axe"
        BigSword -> "Two Handed Sword"
        CrossBow -> "Crossbow"
        Staff -> "Staff"

instance Show OneHander where
    show (OneHander o _) = case o of
        SmallAxe -> "Axe"
        SmallSword -> "Sword"

-- Feats

data Feat = Feat { getRawFeatName :: String
                 , getRawFeatDescription :: String }

-- Magic Items

data MagicItem = MagicItem { getRawMagicItemName :: String
                           , getRawMagicItemDescription :: String }

-- Items

data Item = Item { getRawItemName :: String } deriving (Ord, Eq)

instance Show Item where
    show = getRawItemName

-- Powers

data Power = Power { getRawPowerName :: String
                   , getRawPowerDescription :: String }

-- Base Stats

data BaseStats = BaseStats (Map Ability Int)

getRawAbilityScore :: Ability -> BaseStats -> Int
getRawAbilityScore abil (BaseStats baseStats) = baseStats ! abil

-- Need to Use

-- data DefensesAbilities = DefensesAbilities [Ability]

-- data Defenses = Defenses [DefensesAbilities]
