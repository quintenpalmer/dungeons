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
    (\player ->
        let miscSpeedPlus = (getSpeedMisc player) ++ [oneSpeedIfLightArmor] in
            player { getSpeedMisc = miscSpeedPlus })

oneSpeedIfLightArmor :: Player -> Int
oneSpeedIfLightArmor player = if wearingHeavyArmor player then 0 else 1
