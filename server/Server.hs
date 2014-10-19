module Main where

import Control.Concurrent (forkIO)
import Data.Map (Map)
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
                  selectPlayer,
                  parseParams,
                  splitTwice)

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
    let params = parseParams rawParams
    mPlayer <- selectPlayer playerName
    case mPlayer of
        Just player -> do
            doRequest request params playerName player
            hPutStrLn h $ getResponse request params player
            hFlush h
            respond h
        Nothing -> do
            hPutStrLn h $ reportFailure playerName
            hFlush h
            respond h

doRequest :: String -> Map String String -> String -> Player -> IO ()
doRequest "update" params playerName _ = updatePlayer params playerName
doRequest _ _ _ _ = return ()

parseRequest :: String -> (String, String, String)
parseRequest msg = splitTwice ':' msg

getResponse :: String -> Map String String -> Player -> String
getResponse request params player = sendServerRequest request params player

sendServerRequest :: String -> Map String String -> Player -> String
sendServerRequest "player" params player = serializePlayerForNetwork player
sendServerRequest "update" params player = reportSuccess "update"
sendServerRequest request _ _ = reportFailure request


reportSuccess :: String -> String
reportSuccess name = "{\"status\": \"success\", \"command\": \"" ++ name ++ "\" }"

reportFailure :: String -> String
reportFailure name = "{\"status\": \"failure\", \"command\": \"" ++ name ++ "\" }"
