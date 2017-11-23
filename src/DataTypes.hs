module DataTypes where
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Data.Map as Map
import Data.Hashable (hash)
import System.IO


data ChatRoom = ChatRoom {roomStr :: String, roomRef :: Int, clients :: TVar (Map Int Client)}
data Client = Client  {clientName :: String, clientId :: Int, clientHandle :: Handle, channel :: TChan Message}
data Message = Chat String String String | Error String String | Broadcast String String
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

handleMsgTypes :: Message -> Client -> ChatList -> IO ()
handleMsgTypes msg client rooms = do
    case msg of
        Chat roomRef name msg -> send ("CHAT: " ++ roomRef ++ "\nCLIENT_NAME: " ++ name ++ "\nMESSAGE: " ++ msg ++ "\n\n")
        Error num msg -> send ("ERROR: " ++ num ++ "\nMEssage: " ++ msg ++ "\n\n")
    where
        send x = hPutStr (clientHandle client) x

sendMessage :: Message -> Int -> ChatList -> Client -> IO ()
sendMessage msg ref rooms client = do
    print "sending..."
    roomMap <- atomically $ do readTVar rooms
    let c = Map.lookup ref roomMap
    case c of
        Nothing -> do
            atomically $ do writeTChan (channel client) ( Error "200" "Chatroom doesn't exist.")
        Just c -> do
            clientMap <- atomically $ do readTVar (clients c)
            atomically $ do mapM_ (\c -> writeTChan (channel c) msg) (Map.elems clientMap)

disconnectClient :: Client -> ChatList -> IO ()
disconnectClient client rooms = do
    print "disconnecting client"
    roomMap <- atomically $ do readTVar rooms
    let roomNames = Prelude.map (\room -> roomStr room) (Map.elems roomMap)
    print roomNames
    mapM_ (\name -> removeClient (hash name) client rooms) (roomNames)

removeClient :: Int -> Client -> ChatList -> IO ()
removeClient roomRef client rooms = do
    print "Removing..."
    roomMap <- atomically $ do readTVar rooms
    let c = Map.lookup roomRef roomMap
    case c of
        Nothing -> do
            atomically $ do writeTChan (channel client) ( Error "200" "Chatroom doesn't exist.\n\n")
        Just c -> do
            let roomName = (roomStr c)
            let name = clientName client
            clientMap <- atomically $ do readTVar (clients c)
            sendMessage (Chat (show roomRef) name $ name ++ " has left this chatroom.") roomRef rooms client
            hPutStr (clientHandle client) $ "LEFT_CHATROOM: " ++ (show roomRef) ++ "\nJOIN_ID: " ++ (show $ clientId client) ++ "\n"
            let newMap = Map.delete (clientId client) clientMap
            atomically $ do writeTVar (clients c) newMap
            print "client left"

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
            hPutStr (clientHandle client) $ "JOINED_CHATROOM: " ++ (roomStr room) ++  "\nSERVER_IP: 0.0.0.0" ++ "\nPORT: 0" ++ "\nROOM_REF: " ++ (show $ roomRef room) ++ "\nJOIN_ID: " ++ (show $ clientId client) ++ "\n"

        Just c -> do
            clientMap <- atomically $ do readTVar (clients c)
            let newMap = Map.insert (clientId client) client clientMap
            atomically $ do writeTVar (clients c) newMap
            hPutStr (clientHandle client) $ "JOINED_CHATROOM: " ++ (roomStr c) ++  "\nSERVER_IP: 0.0.0.0" ++ "\nPORT: 0" ++ "\nROOM_REF: " ++ (show $ roomRef c) ++ "\nJOIN_ID: " ++ (show $ clientId client) ++ "\n"
            print "Found it!"
            print ((clientName client) ++ " joined room: " ++ (roomStr c))
