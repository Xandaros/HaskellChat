{-# LANGUAGE OverloadedStrings #-}

module MessageHandler
    ( socketApp
    , broadcast
    , handleMessage
    ) where

import           Control.Exception (try)
import           Control.Monad.Trans (liftIO)
import           Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (ByteString, append)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Network.WebSockets

import           Application
import           Client

socketApp :: App -> PendingConnection -> IO ()
socketApp app pending = do
    let clients_ = _clients app

    connection <- acceptRequest pending
    forkPingThread connection 5

    client <- liftM (flip Client connection) $ newNick clients_

    addClient client (_clients app)

    liftIO $ putStrLn ("New client: " ++ T.unpack (_nick client))

    (try $ clientThread app client) :: IO (Either ConnectionException ())

    liftIO $ putStrLn ((T.unpack (_nick client)) ++ " Disconnected")

    removeClient client (_clients app)

    return ()

clientThread :: App -> Client -> IO ()
clientThread app client = do
    msg <- receive (_connection client)
    result <- handleMessage app client msg
    when result $ clientThread app client

handleMessage :: App -> Client -> Message -> IO Bool
handleMessage _ client (ControlMessage a) = case a of
    (Ping p) -> send connection (ControlMessage $ Pong p) >> return True
    (Pong _) -> return True
    (Close _ b) -> sendClose connection b >> return False
    where connection = _connection client
handleMessage app client (DataMessage a) = case a of
    (Binary _) -> return True -- Unsupported
    (Text t) -> liftM (map _connection) clients_ >>=
        broadcast (encodeUtf8 ("MSG " `LT.append` LT.fromChunks [_nick client] `LT.append` " ") `LBS8.append` t) >> return True
        where clients_ = readIORef (_clients app)

broadcast :: LBS8.ByteString -> [Connection] -> IO ()
broadcast msg = mapM_ (`send` (DataMessage $ Text msg))
