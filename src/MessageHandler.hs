{-# LANGUAGE OverloadedStrings #-}

module MessageHandler
    ( socketApp
    , broadcast
    , handleMessage
    , isCommand -- TODO
    ) where

import           Control.Exception (try)
import           Control.Lens
import           Control.Monad (when, liftM)
import           Control.Monad.State
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (ByteString, append)
import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text.Lazy.Encoding as LT (encodeUtf8, decodeUtf8)
import           Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Network.WebSockets

import           Application
import           Client hiding (nick)

data HandlerState = HandlerState { _client :: Client
                                 }

------------------------------------------------------------------------------
-- | Util
isCommand :: LT.Text -> Bool
isCommand = ("/" `LT.isPrefixOf`)

------------------------------------------------------------------------------
-- | Sending stuff
connections :: App -> IO [Connection]
connections = liftM (map _connection) . readIORef . _clients

broadcast :: LBS8.ByteString -> [Connection] -> IO ()
broadcast msg = mapM_ (`send` (DataMessage $ Text msg))

broadcastPacket :: [LT.Text] -> [Connection] -> IO ()
broadcastPacket = broadcast . LT.encodeUtf8 . LT.unwords

------------------------------------------------------------------------------
-- | Receiving stuff
socketApp :: App -> PendingConnection -> IO ()
socketApp app pending = do
    let clients_ = _clients app

    connection <- acceptRequest pending
    forkPingThread connection 5

    client <- liftM (flip Client connection) $ newNick clients_

    addClient client (_clients app)
    putStrLn ("New client: " ++ T.unpack (_nick client))

    (try $ clientThread app client) :: IO (Either ConnectionException ())

    connections app >>= broadcastPacket ["QUIT", LT.fromChunks [_nick client]]
    putStrLn (T.unpack (_nick client) ++ " Disconnected")

    removeClient client (_clients app)

    return ()

clientThread :: App -> Client -> IO ()
clientThread app client = do
    msg <- receiveDataMessage (_connection client)
    (result, newState) <- runStateT (handleMessage app msg) (HandlerState client)
    when result $ clientThread app (_client newState)

handleMessage :: App -> DataMessage -> StateT HandlerState IO Bool
handleMessage app a = case a of
    (Binary _) -> return True -- Unsupported
    (Text t) -> handleTextMessage app t

handleTextMessage :: App -> LBS8.ByteString -> StateT HandlerState IO Bool
handleTextMessage app msg = do
    if isCommand (LT.decodeUtf8 msg)
        then handleCommand app cmd (drop 1 split')
        else do
            client <- liftM _client get
            lift $ connections app
               >>= broadcastPacket ["MSG", LT.fromChunks [_nick client], LT.decodeUtf8 msg]
    (lift.return) True

    where split' = LT.splitOn " " (LT.decodeUtf8 msg)
          cmd = fromMaybe "" $ liftM (LT.drop 1) (listToMaybe split')

handleCommand :: App -> LT.Text -> [LT.Text] -> StateT HandlerState IO ()
handleCommand app cmd args = do
    let clientList = _clients app
    client <- liftM _client get
    when (cmd == "nick") $ case listToMaybe args of
        Just newNick -> do
            lift $ connections app
               >>= broadcastPacket ["NICK", LT.fromChunks [_nick client], newNick]

            lift $ updateClient (_nick client) clientList client{_nick = LT.toStrict newNick}
            modify (\x -> x{_client = setNick (LT.toStrict newNick) (_client x)})
        Nothing -> return ()
    where
        setNick :: T.Text -> Client -> Client
        setNick t c = c{_nick = t}
