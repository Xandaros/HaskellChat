module Client
    ( Client(Client)
    , addClient
    , removeClient
    , updateClient
    , newNick
    , _nick
    , _connection
    )
    where

import           Control.Applicative
import           Control.Monad (when)
import           Control.Monad.Trans (lift)
import           Control.Lens
import           Data.IORef
import           Data.List
import           Data.Maybe (fromJust, isJust)
import qualified Data.Traversable as Trav (sequence)
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

updateClient :: T.Text -> IORef [Client] -> Client -> IO ()
updateClient nick clients newClient = do
    clientList <- readIORef clients
    let clientM = find ((==nick) . _nick) clientList
    let client = fromJust clientM
    when (isJust clientM) $ putStrLn "Client found" >>
        removeClient client clients >> addClient newClient clients >> putStrLn ("New Nick: " ++ T.unpack (_nick newClient))

newNick :: IORef [Client] -> IO T.Text
newNick clients = existingNicks >>= \nicks -> return $ fromJust $ find (`notElem` nicks) nickList
    where
        existingNicks = map _nick <$> readIORef clients
        nickList = map (T.pack . ("client"++) . show) [1..]
