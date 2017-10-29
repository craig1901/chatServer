module Main where

import System.IO
import Control.Exception
import Network.Socket
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)

type Msg = (Int, String)
-- type User = String
-- type Connections = [User]
-- type ChatRooms = [String]
--
-- addConnections :: User -> Connections
-- addConnections user  = (_:user)
--
-- addRoom :: String -> [String]
-- addRoom s = (_:s)

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0 -- create socket Family SocketType Protocol Number
    setSocketOption sock ReuseAddr 1 -- make reuseable, setSocketOption Socket SocketOption Int
    bind sock (SockAddrInet 4242 iNADDR_ANY) -- listen on TCP port 4242, bind Socket SockAddr
    listen sock 2 -- max 2 connections, listen Socket Int
    channel <- newChan --create channel
    _ <- forkIO $ fix $ \loop -> do
        (_,_) <- readChan channel
        loop
    mainLoop sock channel 0 -- pass socket and new channel into mainLoop


mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock channel msgNum = do
    connection <- accept sock -- accept a connection and handle it
    forkIO (runConn connection channel msgNum)
    mainLoop sock channel $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) channel msgNum = do
    let broadcast msg = writeChan channel (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    hPutStrLn hdl "JOIN CHATROOM: 1"
    hPutStrLn hdl "CLIENT_IP: "
    hPutStrLn hdl "PORT: 4242"
    hPutStrLn hdl "CLIENT_NAME: "
    name <- fmap init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered chat.")
    hPutStrLn hdl "JOINED CHATROOM: 1"
    hPutStrLn hdl "SERVER IP: ..."
    hPutStrLn hdl "PORT: 4242"
    hPutStrLn hdl "ROOM_REF: 3456"
    hPutStrLn hdl "JOIN_ID: 876"

    commLine <- dupChan channel

    -- fork off a thread for reading from duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn hdl line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        case line of
             -- If an exception is caught, send a message and break the loop
             "quit" -> hPutStrLn hdl "Bye!"
             -- else, continue looping.
             _      -> broadcast (name ++ ": " ++ line) >> loop

    killThread reader                      -- kill after the loop ends
    broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
    hClose hdl                             -- close the handle
