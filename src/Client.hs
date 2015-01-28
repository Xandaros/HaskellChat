module Client
    where

import Data.IORef
import Data.List
import Network.WebSockets as WS

data Client = Client { _id         :: Int
                     , _connection :: WS.Connection
                     }

instance Eq Client where
    a == b = _id a == _id b

addClient :: Client -> IORef [Client] -> IO ()
addClient client = flip modifyIORef (client:)

removeClient :: Client -> IORef [Client] -> IO ()
removeClient client = flip modifyIORef (delete client)
