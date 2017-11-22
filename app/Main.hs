module Main where

import System.IO
import Control.Exception
import Network.Socket hiding (Broadcast)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import qualified Data.Map as Map
import Control.Monad
import DataTypes

runChat :: Client -> ChatList -> IO ()
runChat client rooms = do
    print (clientName client ++ " is running.")
    sendMsg
    where
        sendMsg = forever $ do
            cmd <- hGetLine (clientHandle client)
            case words cmd of
                ["JOIN_CHATROOM:", roomName] -> do
                    nextCmds <- replicateM 3 $ hGetLine (clientHandle client)
                    case map words nextCmds of
                        [["CLIENT_IP:", _], [ "PORT:", _], ["CLIENT_NAME:", name]] -> do
                            print "join another.\n"
                ["CHAT:", roomRef] -> do
                    nextCmds <- replicateM 3 $ hGetLine (clientHandle client)
                    case map words nextCmds of
                        [["JOIN_ID:", cId], ["CLIENT_NAME:", name], ["MESSAGE:", msg]] -> do
                            sendChatMessage (Chat roomRef (clientName client) msg) (read roomRef :: Int) rooms client
                _ -> do
                    hPutStr (clientHandle client) "Try again.\n" >> sendMsg

runClient :: Handle -> Int -> ChatList -> IO ()
runClient hdl n rooms = do
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
                        -- TODO: Broadcast Message --
                        runChat client rooms
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
    forkIO (runClient hdl msgNum chatRooms)
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
