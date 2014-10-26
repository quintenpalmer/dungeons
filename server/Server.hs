module Main where

import Control.Concurrent (forkIO)
import Data.Map (Map, lookup)
import Prelude hiding (lookup)
import Network (accept,
                Socket,
                listenOn,
                PortID(..),
                withSocketsDo)
import System.IO (hPutStrLn,
                  Handle,
                  hSetBuffering,
                  BufferMode(..),
                  hGetLine,
                  hFlush,
                  hClose)

import Character (Player,
                  serializePlayerForNetwork,
                  getPlayers,
                  selectPlayer,
                  updatePlayer,
                  parseParams,
                  splitOnce)

main :: IO ()
main = withSocketsDo $ do
    let portNo = 5269
    putStrLn $ "Starting Server on " ++ show portNo ++ "..."
    sock <- listenOn $ PortNumber portNo
    loop sock

loop :: Socket -> IO ()
loop sock = do
    (h, _, _) <- accept sock
    hSetBuffering h LineBuffering
    forkIO $ respond h
    loop sock

respond :: Handle -> IO ()
respond h = do
    rawRequest <- hGetLine h
    let (request, rawParams) = parseRequest rawRequest
    let params = parseParams rawParams
    doRequest request params
    response <- getResponse request params
    sendResponse h $ response

sendResponse :: Handle -> String -> IO ()
sendResponse h message = do
    hPutStrLn h message
    hFlush h
    respond h

doRequest :: String -> Map String String -> IO ()
doRequest "update" params = do
    mPlayerName <- getPlayerName params
    case mPlayerName of
        Just playerName -> updatePlayer params playerName
        Nothing -> return ()
doRequest _ _ = return ()

parseRequest :: String -> (String, String)
parseRequest msg = splitOnce ':' msg

getResponse :: String -> Map String String -> IO String
getResponse "player" params = do
    mPlayer <- getPlayer params
    case mPlayer of
        Just player -> return $ serializePlayerForNetwork player
        Nothing -> return $ reportFailure "could not find player"
getResponse "update" params = return $ reportSuccess "update"
getResponse "allPlayers" params = do
    players <- getPlayers
    return $ show players
getResponse request _ = return $ reportFailure request

getPlayer :: Map String String -> IO (Maybe Player)
getPlayer params = do
    mPlayerName <- getPlayerName params
    case mPlayerName of
        Just playerName -> do
            selectPlayer playerName
        Nothing -> return Nothing

getPlayerName :: Map String String -> IO (Maybe String)
getPlayerName params = do
    return $ lookup "name" params


reportSuccess :: String -> String
reportSuccess name = "{\"status\": \"success\", \"command\": \"" ++ name ++ "\" }"

reportFailure :: String -> String
reportFailure name = "{\"status\": \"failure\", \"command\": \"" ++ name ++ "\" }"
