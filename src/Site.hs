{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Monad.Trans (liftIO)
import           Control.Monad.State (get)
import qualified Data.ByteString as BS (ByteString)
import           Data.IORef
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import qualified Network.WebSockets.Snap as WS
------------------------------------------------------------------------------
import           Application
import           MessageHandler (socketApp)


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(BS.ByteString, Handler App App ())]
routes = [ ("/static", serveDirectory "static")
         , ("/socket", get >>= WS.runWebSocketsSnap . socketApp)
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    cls <- liftIO $ newIORef []

    addRoutes routes
    return $ App h cls
