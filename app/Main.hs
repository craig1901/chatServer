module Main where

import Lib
import Network.Socket

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0 -- create socket Family SocketType Protocol Number
    setSocketOption sock ReuseAddr 1 -- make reuseable, setSocketOption Socket SocketOption Int
    bind sock (SockAddrInet 4242 iNADDR_ANY) -- listen on TCP port 4242, bind Socket SockAddr
    listen sock 2 -- max 2 connections, listen Socket Int
    mainLoop sock -- todo

mainLoop :: Socket -> IO ()
mainLoop sock = do
    connection <- accept sock -- accept a connection and handle it
    runConn connection
    mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    send sock "Hi There!\n"
    close sock
