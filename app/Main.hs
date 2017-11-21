module Main where

import System.IO
import Control.Exception
import Network.Socket
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import qualified Data.Map as Map
import Control.Concurrent
import Control.Monad
import DataTypes

runChatClient :: Handle -> Int -> ChatList -> IO ()
runChatClient hdl n rooms = do
    loop
    return ()

    where
    loop = do
        cmd <- hGetLine hdl
        case words cmd of
            ["JOIN_CHATROOM:", roomName] -> do
                nextCmds <- replicateM 3 $ hGetLine hdl
                case map words nextCmds of
                    [["CLIENT_IP:", _], [ "PORT:", _], ["CLIENT_NAME:", name]] -> do
                        client <- newClient name n hdl
                        addToRoom client roomName rooms
                        -- hPutStr hdl $ "You have joined room: " ++ roomName ++ "\n"
                        
                    _ -> do
                        hPutStr hdl "Try again.\n" >> loop
            _ -> do
                hPutStr hdl "Please join a chat room 1st!\n" >> loop


handleConnections :: Socket -> Int -> ChatList -> IO ()
handleConnections sock msgNum chatRooms = do
    (connection, _) <- accept sock
    print "New client connection"
    hdl <- socketToHandle connection ReadWriteMode
    hSetBuffering hdl NoBuffering
    forkIO (runChatClient hdl msgNum chatRooms)
    handleConnections sock (msgNum + 1) chatRooms


main :: IO ()
main = do
  print "Starting server..."
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  chatRooms <- atomically $ newTVar Map.empty
  listen sock 2
  handleConnections sock 0 chatRooms
  return ()
