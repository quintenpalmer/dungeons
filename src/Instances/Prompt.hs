module Instances.Prompt (
    prompt
) where

import Character.Types (
    Ability(..),
    Skill(..),
    newPlayer)

import Instances.Races (
    halfling)
import Instances.Classes (
    druid)

trainedSkills = [Endurance, Heal, Nature, Perception]

baseStats = [
    (Str, 12),
    (Con, 14),
    (Dex, 16),
    (Int, 11),
    (Wis, 18),
    (Cha, 11)]

prompt = newPlayer "Prompt" 4 halfling druid trainedSkills baseStats
