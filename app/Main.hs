module Main where

import System.IO
import Control.Exception
import Network.Socket
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)

type Msg = (Int, String)
-- type ChatName = String
-- type MessageChannel = Chan Msg
-- type User = String
-- type Connections = [User]
-- type ChatRooms = [String]
--
-- addConnections :: User -> Connections
-- addConnections user  = (_:user)
--
-- addRoom :: String -> [String]
-- addRoom s = (_:s)


data Chat = Chat {  name :: String,
                    chatChannel :: Chan Msg
                  }

newChat :: String -> Chan Msg -> IO Chat
newChat chatName chan =

    return Chat {      name = chatName,
                chatChannel = chan
          }

addIfAbsent :: Chat -> [Chat] -> [Chat]
-- addIfAbsent chat (x:[]) =  x ++ [chat]
addIfAbsent chat ls = if isAlreadyChat (name chat) ls then ls
                        else ls ++ [chat]

getChatTwo :: String -> [Chat] -> IO Chat
getChatTwo str [] = do
  print "making new chat"
  c <- newChan
  newChat str c

getChatTwo str (x:xs) = do
  if str == (name x) then return x
  else getChatTwo str xs


isAlreadyChat :: String -> [Chat] -> Bool
isAlreadyChat str [] = False
isAlreadyChat str (x:xs) =
    if str == (name x) then True
    else (isAlreadyChat str xs)


main :: IO ()
main = do
    print "Starting server..."
    sock <- socket AF_INET Stream 0 -- create socket Family SocketType Protocol Number
    setSocketOption sock ReuseAddr 1 -- make reuseable, setSocketOption Socket SocketOption Int
    bind sock (SockAddrInet 4242 iNADDR_ANY) -- listen on TCP port 4242, bind Socket SockAddr
    listen sock 2 -- max 2 connections, listen Socket Int
    mainLoop sock []  0 -- pass socket and new channel into mainLoop


mainLoop :: Socket -> [Chat] -> Int -> IO ()
mainLoop sock chatList msgNum = do
    (connection, _) <- accept sock -- accept a connection and handle it
    print "New client connection."
    print $ length chatList
    hdl <- socketToHandle connection ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStr hdl "JOIN CHATROOM: "
    chatName <- fmap init (hGetLine hdl)
    -- let getChat =
    chat <- getChatTwo chatName chatList
    let newList = addIfAbsent chat chatList
    print $ length newList
    let c = chatChannel chat
    forkIO (runConn hdl c chat msgNum)
    mainLoop sock newList $! msgNum + 1

runConn :: Handle -> Chan Msg -> Chat -> Int -> IO ()
runConn hdl channel chat msgNum = do
    let broadcast msg = writeChan channel (msgNum, msg)
    -- hdl <- socketToHandle sock ReadWriteMode

    hPutStrLn hdl "CLIENT_IP: 0"
    hPutStrLn hdl "PORT: 0"
    hPutStr hdl "CLIENT_NAME: "
    name <- fmap init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered chat.")
    hPutStrLn hdl "JOINED CHATROOM: " ++ (name chat)
    hPutStrLn hdl "SERVER IP: "
    hPutStrLn hdl "PORT: 4242"
    hPutStrLn hdl "ROOM_REF: "
    hPutStrLn hdl "JOIN_ID: "

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
