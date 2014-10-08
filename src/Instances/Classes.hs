module Instances.Classes (
    druid
) where

import Character.Types (
    Defense(..),
    Class(..),
    newClass)

druid :: Class
druid = newClass [(Ref, 1), (Will, 1)] (12 + 9) 5
