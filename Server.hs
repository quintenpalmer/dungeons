module Main where

import Control.Concurrent (forkIO)
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

import Character (getAttribute,
                  Player,
                  serializePlayerForNetwork,
                  updatePlayer,
                  loadPlayer,
                  splitTwice)

import Data.Maybe (fromJust)

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
    let (request, playerName, rawParams) = parseRequest rawRequest
    let params = rawParams
    mPlayer <- loadPlayer playerName
    case mPlayer of
        Just player -> do
            doRequest request params playerName player
            hPutStrLn h $ getResponse request params player
            hFlush h
            respond h
        Nothing -> do
            hPutStrLn h $ "player " ++ playerName ++ " not found"
            hFlush h
            respond h

doRequest :: String -> String -> String -> Player -> IO ()
doRequest "update" params playerName _ = updatePlayer params playerName
doRequest _ _ _ _ = return ()

parseRequest :: String -> (String, String, String)
parseRequest msg = splitTwice ':' msg
--_ -> "{\"error\": \"request must have one or no parameters\", \"request\": " ++ msg ++ "}"

getResponse :: String -> String -> Player -> String
getResponse request params player = sendServerRequest request params player

sendServerRequest :: String -> String -> Player -> String
sendServerRequest "player" params player = serializePlayerForNetwork player
sendServerRequest "update" params player = reportSuccess "update"
sendServerRequest request params player = case getAttribute request params player of
    Just response -> "{\"" ++ request ++ "\": \"" ++ response ++ "\"}"
    Nothing -> "{\"error\": \"attribute '" ++ request ++ "' not found\", \"params\": " ++ params ++ "}"


reportSuccess :: String -> String
reportSuccess name = "{\"status\": \"success\", \"command\": \"" ++ name ++ "\" }"
