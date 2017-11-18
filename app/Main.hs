module Main where

import System.IO
import Control.Exception
import Network.Socket
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Data.Map as Map
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import DataTypes


runClient :: Handle -> Int -> ChatList -> IO ()
runClient hdl n rooms = do
    hPutStr hdl "Welcome!\n"
    name <- fmap init (hGetLine hdl)
    client <- newClient name n hdl
    hPutStr hdl "You are now a client.\n"
    roomName <- fmap init (hGetLine hdl)
    addToRoom client roomName rooms
    hPutStr hdl "Bye\n"
    hClose hdl
    return ()


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
