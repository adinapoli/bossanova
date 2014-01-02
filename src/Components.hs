{-# LANGUAGE RecordWildCards #-}

module Components where

import Prelude hiding ((.), id)
import Linear.V2
import qualified SFML.Graphics as G
import Control.Wire hiding (at)
import Control.Lens
import Control.Monad.SFML
import qualified Physics.Hipmunk as H

import Types
import Wires
import Physics


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


--------------------------------------------------------------------------------
linearForce :: V2 Int -> Component
linearForce = Component LinearForce . ForceInt


--------------------------------------------------------------------------------
physicalObj :: H.ShapeType -> Component
physicalObj typ =
  let callback = addShapeCallback typ
  in Component CollisionShape
     (PhysicalShape (HipmunkUninitializedShape callback))


--------------------------------------------------------------------------------
addShapeCallback :: H.ShapeType -> Entity -> GameMonad H.Shape
addShapeCallback typ e =
  case _components e ^. at Position of
    Just (Component _ (PosInt pos)) -> do
      liftIO $ print pos
      addShape typ (fmap fromIntegral pos)
    _ -> addShape typ (V2 0.0 0.0)
