{-# LANGUAGE RecordWildCards #-}

module Components where

import Prelude hiding ((.), id)
import Linear.V2
import Data.Word
import qualified SFML.Graphics as G
import Control.Wire hiding (at, when)
import Control.Lens
import Control.Monad.SFML
import Control.Monad
import qualified Physics.Hipmunk as H
import Control.Monad.Trans.State
import qualified Data.Map.Strict as SMap

import Types
import Wires
import Physics
import Callbacks
import Animation
import Entities
import Utils
import qualified SFML.Window as SFML


--------------------------------------------------------------------------------
sprite :: Component st
sprite = Component Renderable (Sprite (UninitializedSprite initSpriteClbk))


--------------------------------------------------------------------------------
textureFrom :: FilePath -> Component st
textureFrom path = Component Texture
                   (SFMLTexture (UninitializedTexture (initTextureClbk path)))


--------------------------------------------------------------------------------
text :: Component st
text = Component Renderable (Text $ UninitializedText initTextClbk)


--------------------------------------------------------------------------------
textCaption :: String -> Component st
textCaption = Component Caption . TextCaption


--------------------------------------------------------------------------------
intSize :: Int -> Component st
intSize = Component Size . SizeInt


--------------------------------------------------------------------------------
colour :: G.Color -> Component st
colour = Component Colour . RenderColour


--------------------------------------------------------------------------------
blink :: NominalDiffTime -> NominalDiffTime -> Component st
blink cooldown blinkTime =
  Component AffectRendering (MustRenderWire (blinkWire cooldown blinkTime))


--------------------------------------------------------------------------------
position :: Int -> Int -> Component st
position x y = Component Position (PosInt (V2 x y))


--------------------------------------------------------------------------------
translationFromV2 :: V2 Int -> G.RenderStates
translationFromV2 (V2 x y) = G.renderStates {
  G.transform = G.translation (fromIntegral x) (fromIntegral y)
  }


--------------------------------------------------------------------------------
keyboard :: Wire (StateDelta st) () SFML [SFML.KeyCode] (st -> st, V2 Int) -> Component st
keyboard wire = Component Keyboard (PlKbWire wire)


--------------------------------------------------------------------------------
onEvents :: [GameCallback st] -> Component st
onEvents = Component EventListener . Events


--------------------------------------------------------------------------------
linearForce :: V2 Int -> Component st
linearForce = Component LinearForce . ForceInt


--------------------------------------------------------------------------------
dynamicObj :: H.ShapeType -> Component st
dynamicObj typ =
  let callback = addShapeCallback addDynamicShape typ
  in Component DynamicBody
     (CollisionShape (HipmunkUninitializedShape callback))

--------------------------------------------------------------------------------
staticObj :: H.ShapeType -> Component st
staticObj typ =
  let callback = addShapeCallback addStaticShape typ
  in Component StaticBody
     (CollisionShape (HipmunkUninitializedShape callback))

--------------------------------------------------------------------------------
addShapeCallback :: (H.ShapeType -> V2 Double -> GameMonad st H.Shape)
                 -> H.ShapeType -> Entity st -> GameMonad st H.Shape
addShapeCallback fn typ e =
  case _components e ^. at Position of
    Just (Component _ (PosInt pos)) -> fn typ (fmap fromIntegral pos)
    _ -> fn typ (V2 0.0 0.0)


--------------------------------------------------------------------------------
mouseCallback :: GameCallback st -> Component st
mouseCallback = Component Callback . CCallback


--------------------------------------------------------------------------------
rect :: Int -> Int -> Int -> Int -> Component st
rect x1 y1 x2 y2 = Component BoundingBox (IntRect (G.IntRect x1 y1 x2 y2))


--------------------------------------------------------------------------------
discreteTimer :: Word64 -> Component st
discreteTimer step = Component Timer (CTimer (DiscreteTimer 0 step))


noop :: Component st
noop = Component Tagless Void

--------------------------------------------------------------------------------
-- | Spawn a projectile.
spawnProjectile :: GameCallback st
spawnProjectile = GameCallback $ \e ->
  case liftM2 (,)
       (_components e ^. at Timer)
       (_components e ^. at Position) of
    Just ( Component _ (CTimer (DiscreteTimer internalTime stepInterval))
         , Component _ (PosInt (V2 x y))) -> do
      now <- liftIO milliTime
      when (now - internalTime >= stepInterval) $ do
        let newT = DiscreteTimer now stepInterval
        e #.= Component Timer (CTimer newT)
        void $
          (#>) (Entity 0 NoAlias
              (SMap.fromList 
                [ (Renderable, animation "resources/anims/projectile.json" 500)
                , (Position, position (x + 10) (y + 10))
                , (DynamicBody, dynamicObj (H.Circle 16))
                , (Disposable, noop)
                ]
              ))
      return spawnProjectile
    _ -> return spawnProjectile
