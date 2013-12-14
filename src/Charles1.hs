{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
import Prelude hiding ((.), id)

import Control.Wire
import Control.Lens
import FRP.Netwire.Move
import qualified SFML.Window as W
import qualified SFML.Graphics as G
import qualified SFML.System as S
import Control.Monad.SFML
import SFML.Graphics.Color
import Control.Monad
import Data.Map
import qualified Data.Map as Map
import GHC.Float


type GameWire a b = Wire (Timed NominalDiffTime ()) () SFML a b

--------------------------------------------------------------------------------
data Entity a b = Entity {
    _graphics :: G.Sprite
  , _rState :: G.RenderStates
  , _wire :: GameWire a b 
}

$(makeLenses ''Entity)




--------------------------------------------------------------------------------
-- Generate an event (e.g. move the mouse) to advance the simulation
main :: IO ()
main = runSFML $ do
    let ctxSettings = Just $ W.ContextSettings 24 8 0 1 2
    wnd <- createRenderWindow
           (W.VideoMode 640 480 32)
           "OCharles' Challenge 1"
           [W.SFDefaultStyle]
           ctxSettings
    spr <- createSprite
    setTextureRect spr (G.IntRect 40 40 40 40)
    text <- textureFromFile "resources/wood.jpg" (Just $ G.IntRect 40 40 40 40)
    setTexture spr text True
    spr2 <- createSprite
    setTextureRect spr (G.IntRect 40 40 40 40)
    setTexture spr2 text True

    let e1 = (Entity spr G.renderStates challenge1)
        e2 = (Entity spr2 G.renderStates periodicW)

    go wnd e1 e2 (countSession_ 1)

  where
    go wnd e1 e2 sess = do
      clearRenderWindow wnd blue
      (dt, sess') <- stepSession sess
      (Right res, wire') <- stepWire (e1 ^. wire) dt (Right (dtime dt))
      (Right res2, wire2) <- stepWire (e2 ^. wire) dt (Right (dtime dt))
      let dx = (truncate res :: Int) `mod` 500
      let dx2 = (truncate res2 :: Int) `mod` 500
      let r1 = (e1 ^. rState ) { G.transform = G.translation (fromIntegral dx) 40 }
      let r2 = (e2 ^. rState ) { G.transform = G.translation (fromIntegral dx2) 80 }
      drawSprite wnd (e1 ^. graphics) $ Just r1
      drawSprite wnd (e2 ^. graphics) $ Just r2
      display wnd
      evt <- pollEvent wnd
      case evt of
        Just W.SFEvtClosed -> return ()
        _ -> go wnd (wire .~ wire' $ rState .~ r1 $ e1)
                    (wire .~ wire2 $ rState .~ r2 $ e2) sess'




--------------------------------------------------------------------------------
challenge1 = for 500 . 0.5 * time --> for 500 . (-0.5) * time --> challenge1

periodicW = for 500 . (-0.5) * time --> for 500 .  0.5 * time --> periodicW
