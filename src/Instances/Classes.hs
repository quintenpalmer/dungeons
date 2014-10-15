module Instances.Classes (
    druid,
    primalPredator
) where

import Character.Types (
    Player(..),
    wearingHeavyArmor,
    ClassSpec(..),
    Defense(..),
    Class(..),
    newClass)

druid :: ClassSpec -> Class
druid = newClass "Druid" [(Ref, 1), (Will, 1)] (12 + 9) 5

primalPredator :: ClassSpec
primalPredator = ClassSpec "Primal Predator"
    (\(Player name feats level xp race class_ miscSpeed armor weapon magicItems items power trainedSkills baseStats) ->
        let miscSpeedPlus = miscSpeed ++ [(\player -> if wearingHeavyArmor player then 0 else 1)] in
            (Player name feats level xp race class_ miscSpeedPlus armor weapon magicItems items power trainedSkills baseStats))
