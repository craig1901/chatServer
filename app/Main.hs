module Main where

import System.IO
import Control.Exception
import Network.Socket
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)

main :: IO ()
main = do
    print "Starting Server..."
    sock <- socket AF_INET Stream 0 -- create socket Family SocketType Protocol Number
    setSocketOption sock ReuseAddr 1 -- make reuseable, setSocketOption Socket SocketOption Int
    bind sock (SockAddrInet 4242 iNADDR_ANY) -- listen on TCP port 4242, bind Socket SockAddr
    listen sock 2 -- max 2 connections, listen Socket Int
    channel <- newChan --create channel
    _ <- forkIO $ fix $ \loop -> do
        (_,_) <- readChan channel
        loop
    mainLoop sock channel 0 -- pass socket and new channel into mainLoop

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock channel msgNum = do
    connection <- accept sock -- accept a connection and handle it
    print "Client connected!"
    forkIO (runConn connection channel msgNum)
    mainLoop sock channel $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) channel msgNum = do
    let broadcast msg = writeChan channel (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    hPutStrLn hdl "Hi, what's your name?"
    name <- fmap init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered chat.")
    hPutStrLn hdl ("Welcome " ++ name ++ "!")

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
    print "Client left :("
