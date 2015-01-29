{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Exception (try, Exception)
import           Control.Monad.Trans (liftIO)
import           Control.Monad (when, liftM)
import           Control.Monad.State (get)
import           Data.ByteString.Lazy.Char8 as LBS8 (unpack, ByteString)
import           Data.ByteString as BS (ByteString)
import           Data.IORef
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Network.WebSockets as WS
import           Network.WebSockets.Snap as WS
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
    connection <- WS.acceptRequest pending
    forkPingThread connection 5

    clientAmnt <- liftM length $ readIORef (_clients app)

    let client = Client (clientAmnt + 1) connection
    
    addClient client (_clients app)

    liftIO $ putStrLn ("Clients: " ++ show clientAmnt)

    (try $ clientThread app connection) :: IO (Either ConnectionException ())

    liftIO $ putStrLn "Client Disconnected"

    removeClient client (_clients app)

    return ()

clientThread :: App -> WS.Connection -> IO ()
clientThread app client = do
    msg <- WS.receive client
    result <- handleMessage app client msg
    when result $ clientThread app client

handleMessage :: App -> WS.Connection -> WS.Message -> IO Bool
handleMessage _ client (ControlMessage a) = case a of
    (Ping p) -> send client (ControlMessage $ Pong p) >> return True
    (Pong _) -> return True
    (Close _ b) -> sendClose client b >> return False
handleMessage app _ (DataMessage a) = case a of
    (Binary _) -> return True -- Unsupported
    (Text t) -> readIORef (_clients app) >>= (return . map _connection) >>=
        broadcast t >> return True

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
