module Main where

import System.IO
import System.Environment
import Control.Exception
import Network.Socket hiding (Broadcast)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Fix (fix)
import Data.Hashable (hash)
import DataTypes

runChat :: Client -> ChatList -> IO ()
runChat client rooms = do
    print (clientName client ++ " is running.")
    -- fork off a thread for reading messages from client channel
    recv <- forkIO $ fix $ \loop -> do
        msg <- atomically $ do readTChan (channel client)
        handleMsgTypes msg client rooms
        loop

    sendMsg
    -- killThread recv
    where
        sendMsg = forever $ do
            cmd <- hGetLine (clientHandle client)
            case words cmd of
                ["JOIN_CHATROOM:", roomName] -> do
                    nextCmds <- replicateM 3 $ hGetLine (clientHandle client)
                    case map words nextCmds of
                        [["CLIENT_IP:", _], [ "PORT:", _], ["CLIENT_NAME:", name]] -> do
                            addToRoom client roomName rooms
                            sendMessage (Chat (show $ hash roomName) (clientName client) $ name ++ " has joined this room.") (hash roomName) rooms client
                ["CHAT:", roomRef] -> do
                    nextCmds <- replicateM 4 $ hGetLine (clientHandle client)
                    case map words nextCmds of
                        [["JOIN_ID:", cId], ["CLIENT_NAME:", name], ("MESSAGE:":msg), []] -> do
                            print msg
                            sendMessage (Chat roomRef name (unwords msg)) (read roomRef :: Int) rooms client
                        _ -> do hPutStr (clientHandle client) "nope\n\n" >> sendMsg
                ["LEAVE_CHATROOM:", roomRef] -> do
                    nextCmds <- replicateM 2 $ hGetLine (clientHandle client)
                    case map words nextCmds of
                        [["JOIN_ID:", cId], ["CLIENT_NAME:", name]] -> do
                            removeClient (read roomRef :: Int) client rooms
                ["DISCONNECT:", _] -> do
                    nextCmds <- replicateM 2 $ hGetLine (clientHandle client)
                    case map words nextCmds of
                        [["PORT:", _], ["CLIENT_NAME:", name]] -> do
                            disconnectClient client rooms
                            hClose (clientHandle client)

                _ -> do
                    hPutStr (clientHandle client) "Try again.\n" >> sendMsg

runClient :: Handle -> Int -> ChatList -> IO ()
runClient hdl n rooms = do
    loop

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
                        sendMessage (Chat (show $ hash roomName) (clientName client)  $ name ++ " has joined this room.") (hash roomName) rooms client
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
    args <- getArgs
    let port = head args
    print "Starting server..."
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet (read port :: PortNumber) iNADDR_ANY)
    chatRooms <- atomically $ newTVar Map.empty
    listen sock 2
    handleConnections sock 0 chatRooms
    return ()
