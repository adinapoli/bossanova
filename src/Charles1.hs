import Prelude hiding ((.), id)

import Control.Wire
import FRP.Netwire.Move
import qualified SFML.Window as W
import qualified SFML.Graphics as G
import qualified SFML.System as S
import Control.Monad.SFML
import SFML.Graphics.Color
import Control.Monad
import GHC.Float


--------------------------------------------------------------------------------
main :: IO ()
main = runSFML $ do
    let ctxSettings = Just $ W.ContextSettings 24 8 0 1 2
    wnd <- createRenderWindow
           (W.VideoMode 640 480 32)
           "OCharles' Challenge 1"
           [W.SFDefaultStyle]
           ctxSettings
    rect <- createRectangleShape
    setSize rect (S.Vec2f 40 40)
    setFillColor rect red
    go wnd rect (countSession_ 1) challenge1

  where
    go wnd rect sess wire = do
      clearRenderWindow wnd blue
      (dt, sess') <- stepSession sess
      liftIO $ print dt
      (Right res, wire') <- stepWire wire dt (Right (dtime dt))
      liftIO $ print res
      let render = G.renderStates {
                     G.transform = G.translation (double2Float res) 40
                   }
      drawRectangle wnd rect $ Just render
      display wnd
      evt <- waitEvent wnd
      case evt of
        Just W.SFEvtClosed -> return ()
        _ -> go wnd rect sess' wire'


--------------------------------------------------------------------------------
challenge1 :: (Monad m, HasTime t s ) => Wire s e m Double Double
challenge1 = integral 0
