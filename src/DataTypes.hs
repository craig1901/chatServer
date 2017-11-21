module DataTypes where
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Data.Map as Map
import Data.Hashable (hash)
import System.IO


data ChatRoom = ChatRoom {roomStr :: String, roomRef :: Int, clients :: TVar (Map Int Client)}
data Client = Client  {clientName :: String, clientId :: Int, handle :: Handle, channel :: TChan Message}
type Message = String
type ChatList = TVar (Map Int ChatRoom)

------------------------------ Constructors ------------------------------------------------------

newClient :: String -> Int -> Handle -> IO Client
newClient name ident hdl = do
    chan <- newTChanIO
    return Client { clientName = name, clientId = ident, handle = hdl, channel = chan}

newChatRoom :: String -> Client -> STM ChatRoom
newChatRoom str client = do
    let d = Map.insert (clientId client) client Map.empty
    c <- newTVar d
    return ChatRoom {roomStr = str, roomRef = (hash str), clients = c}


------------------------------ Data Type Methods ------------------------------------------------------

addToRoom :: Client -> String -> ChatList -> IO ()
addToRoom client roomName rooms = do
    roomMap <- atomically $ do readTVar rooms
    let c = Map.lookup (hash roomName) roomMap
    case c of
        Nothing -> do
            room <- atomically $ do newChatRoom roomName client
            let newMap = Map.insert (roomRef room) room roomMap
            atomically $ do writeTVar rooms newMap
            hPutStr (handle client) $ "JOINED_CHATROOM: " ++ (roomStr room) ++ "\n" ++ "SERVER_IP: 0\n" ++ "PORT: 0\n" ++ "ROOM_REF: " ++ (show $ roomRef room) ++ "\n" ++ "JOIN_ID: " ++ (show $ clientId client) ++ "\n"

        Just c -> do
            clientMap <- atomically $ do readTVar (clients c)
            let newMap = Map.insert (clientId client) client clientMap
            atomically $ do writeTVar (clients c) newMap
            hPutStr (handle client) $ "JOINED_CHATROOM: " ++ (roomStr c) ++ "\n" ++ "SERVER_IP: 0\n" ++ "PORT: 0\n" ++ "ROOM_REF: " ++ (show $ roomRef c) ++ "\n" ++ "JOIN_ID: " ++ (show $ clientId client) ++ "\n"
            print "Found it!"
            return ()
