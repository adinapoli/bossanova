{-# LANGUAGE RecordWildCards #-}

module Components where

import Prelude hiding ((.), id)
import Linear.V2
import qualified SFML.Graphics as G
import Control.Wire


import Types
import Wires


--------------------------------------------------------------------------------
sprite :: G.Sprite -> Component
sprite = Component Renderable . Sprite


--------------------------------------------------------------------------------
text :: G.Text -> Component
text = Component Renderable . Text


--------------------------------------------------------------------------------
textCaption :: String -> Component
textCaption = Component Caption . TextCaption


--------------------------------------------------------------------------------
intSize :: Int -> Component
intSize = Component Size . SizeInt


--------------------------------------------------------------------------------
colour :: G.Color -> Component
colour = Component Colour . RenderColour


--------------------------------------------------------------------------------
blink :: NominalDiffTime -> NominalDiffTime -> Component
blink cooldown blinkTime =
  Component AffectRendering (MustRenderWire (blinkWire cooldown blinkTime))


--------------------------------------------------------------------------------
position :: Int -> Int -> Component
position x y = Component Position (PosInt (V2 x y))


--------------------------------------------------------------------------------
translationFromV2 :: V2 Int -> G.RenderStates
translationFromV2 (V2 x y) = G.renderStates {
  G.transform = G.translation (fromIntegral x) (fromIntegral y)
  }


--------------------------------------------------------------------------------
keyboard :: GameWire NominalDiffTime (V2 Int) -> Component
keyboard wire = Component Keyboard (PlKbWire wire)


--------------------------------------------------------------------------------
onEvents :: [GameEvent] -> Component
onEvents = Component EventListener . Events
