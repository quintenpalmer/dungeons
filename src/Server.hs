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

import Character (getAttribute)
import Instances.Prompt (prompt)

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
    request <- hGetLine h
    hPutStrLn h $ getResponse request
    hFlush h
    respond h

getResponse :: String -> String
getResponse msg = case splitOn ":" msg of
    [request] -> sendServerRequest request ""
    [request, params] -> sendServerRequest request params
    _ -> "{\"error\": \"request: " ++ msg ++ " must have one or no parameters\"}"

sendServerRequest :: String -> String -> String
sendServerRequest request params = case getAttribute request params prompt of
    Just response -> "{\"" ++ request ++ "\": \"" ++ response ++ "\"}"
    Nothing -> "{\"error\": \"attribute " ++ request ++ ":" ++ params ++ " not found\"}"
