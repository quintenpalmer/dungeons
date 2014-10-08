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

    Race,
    newRace,
    getRacialSkill,
    getRacialAbility,
    getRacialSpeed,

    Class,
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
             Thievery deriving (Ord, Eq, Show)

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
               Cha deriving (Ord, Eq, Show)

data Defense = AC |
               Fort |
               Ref |
               Will deriving (Ord, Eq, Show)

data Player = Player { getName :: String
                     , getLevel :: Int
                     , getRace :: Race
                     , getClass :: Class
                     , getRawTrainedSkills :: [Skill]
                     , getRawBaseStats :: BaseStats }

newPlayer :: String -> Int -> Race -> Class -> [Skill] -> [(Ability, Int)] -> Player
newPlayer name level race class_ trainedSkills baseStats =
    Player name level race class_ trainedSkills $ BaseStats $ fromList baseStats

getHealth :: Player -> Int
getHealth player =
    (getStartingHealthValue (getClass player)) +
    ((getHealthPerLevel (getClass player)) * (getLevel player))

getTrainedSkillAmount :: Skill -> Player -> Int
getTrainedSkillAmount skill player =
    if skill `elem` (getRawTrainedSkills player)
    then 5
    else 0

data Race = Race { getRawRacialSkills :: SkillMap
                 , getRawRacialAbilities :: AbilityMap
                 , getRawRacialSpeed :: Int }

newRace :: [(Skill, Int)] -> [(Ability, Int)] -> Int -> Race
newRace skills abilities speed =
    Race (SkillMap (fromList skills))
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

data Class = Class { getRawClassDefenses :: DefenseMap
                   , getStartingHealthValue :: Int
                   , getHealthPerLevel :: Int }

newClass :: [(Defense, Int)] -> Int -> Int -> Class
newClass defenses startingHealth healthPerLevel =
    Class (DefenseMap $ fromList defenses) startingHealth healthPerLevel

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
