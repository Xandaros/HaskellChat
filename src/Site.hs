{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Exception (try)
import           Control.Monad.Trans (liftIO)
import           Control.Monad (when, liftM)
import           Control.Monad.State (get)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (ByteString, append)
import qualified Data.ByteString as BS (ByteString)
import           Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
------------------------------------------------------------------------------
import           Application
import           Client


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(BS.ByteString, Handler App App ())]
routes = [ ("/static", serveDirectory "static")
         , ("/socket", socketHandler)
         ]

socketHandler :: Handler App App ()
socketHandler = do
    appState <- get
    WS.runWebSocketsSnap (socketApp appState)

socketApp :: App -> WS.PendingConnection -> IO ()
socketApp app pending = do
    let clients = _clients app

    connection <- WS.acceptRequest pending
    forkPingThread connection 5

    clientAmnt <- liftM length $ readIORef clients
    client <- newNick clients >>= return . (flip Client connection)

    addClient client (_clients app)

    liftIO $ putStrLn ("New client: " ++ (T.unpack $ _nick client))

    (try $ clientThread app client) :: IO (Either ConnectionException ())

    liftIO $ putStrLn ((T.unpack (_nick client)) ++ " Disconnected")

    removeClient client (_clients app)

    return ()

clientThread :: App -> Client -> IO ()
clientThread app client = do
    msg <- WS.receive (_connection client)
    result <- handleMessage app client msg
    when result $ clientThread app client

handleMessage :: App -> Client -> WS.Message -> IO Bool
handleMessage _ client (ControlMessage a) = case a of
    (Ping p) -> send connection (ControlMessage $ Pong p) >> return True
    (Pong _) -> return True
    (Close _ b) -> sendClose connection b >> return False
    where connection = _connection client
handleMessage app client (DataMessage a) = case a of
    (Binary _) -> return True -- Unsupported
    (Text t) -> clients >>= (return . map _connection) >>=
        broadcast (encodeUtf8 (("MSG " `LT.append` LT.fromChunks [_nick client] `LT.append` " ")) `LBS8.append` t) >> return True
        where clients = readIORef (_clients app)

broadcast :: LBS8.ByteString -> [WS.Connection] -> IO ()
broadcast msg = mapM_ (flip send (DataMessage $ Text msg))

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    cls <- liftIO $ newIORef []

    addRoutes routes
    return $ App h cls
