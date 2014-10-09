import Control.Concurrent (forkIO)
import Data.List.Split (splitOn)
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

import Character (getAttribute, Player)

import Data.Maybe (fromJust)
import Character.Loader (loadPlayer)

prompt :: IO Player
prompt = do
    let filename = "prompt"
    jsonString <- readFile $ "data/" ++ filename ++ ".json"
    return $ fromJust $ loadPlayer jsonString

main :: IO ()
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 5269
    loop sock

loop :: Socket -> IO ()
loop sock = do
    (h, _, _) <- accept sock
    hSetBuffering h LineBuffering
    forkIO $ respond h
    loop sock

respond :: Handle -> IO ()
respond h = do
    player <- prompt
    request <- hGetLine h
    hPutStrLn h $ getResponse request player
    hFlush h
    respond h

getResponse :: String -> Player -> String
getResponse msg player = case splitOn ":" msg of
    [request] -> sendServerRequest request "" player
    [request, params] -> sendServerRequest request params player
    _ -> "{\"error\": \"request: " ++ msg ++ " must have one or no parameters\"}"

sendServerRequest :: String -> String -> Player -> String
sendServerRequest request params player = case getAttribute request params player of
    Just response -> "{\"" ++ request ++ "\": \"" ++ response ++ "\"}"
    Nothing -> "{\"error\": \"attribute " ++ request ++ ":" ++ params ++ " not found\"}"
