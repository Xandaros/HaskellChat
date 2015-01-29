module Client
    ( Client(Client)
    , addClient
    , removeClient
    , newNick
    , _nick
    , _connection
    )
    where

import           Data.IORef
import           Data.List
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

data Client = Client { _nick       :: T.Text
                     , _connection :: WS.Connection
                     }

instance Eq Client where
    a == b = _nick a == _nick b

addClient :: Client -> IORef [Client] -> IO ()
addClient client = flip modifyIORef (client:)

removeClient :: Client -> IORef [Client] -> IO ()
removeClient client = flip modifyIORef (delete client)

newNick :: IORef [Client] -> IO T.Text
newNick clients = existingNicks >>= \nicks -> return $ fromJust $ (find (`notElem` nicks) nickList)
    where
        existingNicks = readIORef clients >>= return . (map _nick)
        nickList = map (T.pack . ("client"++) . show) [1..]
