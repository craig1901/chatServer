module DataTypes where
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Data.Map as Map
import Data.Hashable (hash)
import System.IO


data ChatRoom = ChatRoom {roomStr :: String, roomRef :: Int, clients :: TVar (Map Int Client)}
data Client = Client  {clientName :: String, clientId :: Int, clientHandle :: Handle, channel :: TChan Message}
data Message = Error String | Chat String String String
type ChatList = TVar (Map Int ChatRoom)

------------------------------ Constructors ------------------------------------------------------

newClient :: String -> Int -> Handle -> IO Client
newClient name ident hdl = do
    chan <- newTChanIO
    return Client { clientName = name, clientId = ident, clientHandle = hdl, channel = chan}

newChatRoom :: String -> Client -> STM ChatRoom
newChatRoom str client = do
    let d = Map.insert (clientId client) client Map.empty
    c <- newTVar d
    return ChatRoom {roomStr = str, roomRef = (hash str), clients = c}


------------------------------ Data Type Methods ------------------------------------------------------

sendChatMessage :: Message -> Int -> ChatList -> Client -> IO ()
sendChatMessage msg ref rooms client = do
    roomMap <- atomically $ do readTVar rooms
    let c = Map.lookup ref roomMap
    case c of
        Nothing -> do
            hPutStr (clientHandle client) "ERROR"
        Just c -> do
            clientMap <- atomically $ do readTVar (clients c)
            atomically $ do mapM_ (\c -> writeTChan (channel c) msg) (Map.elems clientMap)
            print "Sent to Channel"

addToRoom :: Client -> String -> ChatList -> IO ()
addToRoom client roomName rooms = do
    roomMap <- atomically $ do readTVar rooms
    let c = Map.lookup (hash roomName) roomMap
    case c of
        Nothing -> do
            room <- atomically $ do newChatRoom roomName client
            let newMap = Map.insert (roomRef room) room roomMap
            atomically $ do writeTVar rooms newMap
            print ((clientName client) ++ " joined room: " ++ (roomStr room))
            hPutStr (clientHandle client) $ "JOINED_CHATROOM: " ++ (roomStr room) ++ "\n" ++ "SERVER_IP: 0\n" ++ "PORT: 0\n" ++ "ROOM_REF: " ++ (show $ roomRef room) ++ "\n" ++ "JOIN_ID: " ++ (show $ clientId client) ++ "\n"

        Just c -> do
            clientMap <- atomically $ do readTVar (clients c)
            let newMap = Map.insert (clientId client) client clientMap
            atomically $ do writeTVar (clients c) newMap
            hPutStr (clientHandle client) $ "JOINED_CHATROOM: " ++ (roomStr c) ++ "\n" ++ "SERVER_IP: 0\n" ++ "PORT: 0\n" ++ "ROOM_REF: " ++ (show $ roomRef c) ++ "\n" ++ "JOIN_ID: " ++ (show $ clientId client) ++ "\n"
            print "Found it!"
            print ((clientName client) ++ " joined room: " ++ (roomStr c))
            return ()
