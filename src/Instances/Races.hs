module Instances.Races (
    halfling,
) where

import Character.Types (
    Skill(..),
    Ability(..),
    Race(..),
    newRace)

halfling :: Race
halfling = newRace [(Acrobatics, 2), (Thievery, 2)] [(Dex, 2), (Cha, 2)] 6
