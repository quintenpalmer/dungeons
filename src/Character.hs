module Character (
    Player,
    newPlayer,
    getAttribute,
    serializePlayerForNetwork,
    serializePlayerForTerminal,
    updatePlayer,
    loadPlayer,
    readMaybe,
    splitOnce,
    splitTwice
) where

import Character.Player (
    getAttribute)
import Character.Types (
    Player(..),
    newPlayer)
import Character.Serialize (
    serializePlayerForNetwork,
    serializePlayerForTerminal)
import Character.Loader (
    loadPlayer,
    updatePlayer)
import Character.Util (
    readMaybe,
    splitOnce,
    splitTwice)
