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

    Class(..),
    getClassDefense,
    newClass,
) where

import Data.Map (
    Map(..),
    fromList,
    (!),
    findWithDefault)

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

skillMap :: Map Skill Ability
skillMap = fromList [
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

getSkillAbil :: Skill -> Ability
getSkillAbil skill = skillMap ! skill

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

data Player = Player { getName :: String
                     , getLevel :: Int
                     , getXp :: Int
                     , getRace :: Race
                     , getClass :: Class
                     , getRawTrainedSkills :: [Skill]
                     , getRawBaseStats :: BaseStats }

newPlayer :: String -> Int -> Int -> Race -> Class -> [Skill] -> [(Ability, Int)] -> Player
newPlayer name level xp race class_ trainedSkills baseStats =
    Player name level xp race class_ trainedSkills $ BaseStats $ fromList baseStats

getHealth :: Player -> Int
getHealth player =
    (getStartingHealthValue (getClass player)) +
    ((getHealthPerLevel (getClass player)) * (getLevel player))

getTrainedSkillAmount :: Skill -> Player -> Int
getTrainedSkillAmount skill player =
    if skill `elem` (getRawTrainedSkills player)
    then 5
    else 0

data Race = Race { raceName :: String
                 , getRawRacialSkills :: SkillMap
                 , getRawRacialAbilities :: AbilityMap
                 , getRawRacialSpeed :: Int }

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

data Class = Class { className :: String
                   , getRawClassDefenses :: DefenseMap
                   , getStartingHealthValue :: Int
                   , getHealthPerLevel :: Int }

newClass :: String -> [(Defense, Int)] -> Int -> Int -> Class
newClass name defenses startingHealth healthPerLevel =
    Class name (DefenseMap $ fromList defenses) startingHealth healthPerLevel

getClassDefense :: Defense -> Class -> Int
getClassDefense def class_ = let (DefenseMap defMap) = getRawClassDefenses class_ in
    findWithDefault 0 def defMap


data BaseStats = BaseStats (Map Ability Int)

getRawAbilityScore :: Ability -> BaseStats -> Int
getRawAbilityScore abil (BaseStats map) = map ! abil

data SkillMap = SkillMap (Map Skill Int)

data AbilityMap = AbilityMap (Map Ability Int)

data DefenseMap = DefenseMap (Map Defense Int)

-- Need to Use

data DefensesAbilities = DefensesAbilities [Ability]

data Defenses = Defenses [DefensesAbilities]
