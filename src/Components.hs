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
import Callbacks


--------------------------------------------------------------------------------
sprite :: Component
sprite = Component Renderable (Sprite (UninitializedSprite initSpriteClbk))


--------------------------------------------------------------------------------
textureFrom :: FilePath -> Component
textureFrom path = Component Texture
                   (SFMLTexture (UninitializedTexture (initTextureClbk path)))


--------------------------------------------------------------------------------
text :: Component
text = Component Renderable (Text $ UninitializedText initTextClbk)


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
onEvents :: [GameCallback] -> Component
onEvents = Component EventListener . Events


--------------------------------------------------------------------------------
linearForce :: V2 Int -> Component
linearForce = Component LinearForce . ForceInt


--------------------------------------------------------------------------------
dynamicObj :: H.ShapeType -> Component
dynamicObj typ =
  let callback = addShapeCallback addDynamicShape typ
  in Component DynamicBody
     (CollisionShape (HipmunkUninitializedShape callback))

--------------------------------------------------------------------------------
staticObj :: H.ShapeType -> Component
staticObj typ =
  let callback = addShapeCallback addStaticShape typ
  in Component StaticBody
     (CollisionShape (HipmunkUninitializedShape callback))

--------------------------------------------------------------------------------
addShapeCallback :: (H.ShapeType -> V2 Double -> GameMonad H.Shape)
                 -> H.ShapeType -> Entity -> GameMonad H.Shape
addShapeCallback fn typ e =
  case _components e ^. at Position of
    Just (Component _ (PosInt pos)) -> fn typ (fmap fromIntegral pos)
    _ -> fn typ (V2 0.0 0.0)


--------------------------------------------------------------------------------
mouseCallback :: GameMonad () -> Component
mouseCallback = Component Mouse . MouseCallback


--------------------------------------------------------------------------------
rect :: Int -> Int -> Int -> Int -> Component
rect x1 y1 x2 y2 = Component BoundingBox (IntRect (G.IntRect x1 y1 x2 y2))
