module Animation where

import qualified Data.Vector as V
import Control.Monad
import Control.Wire hiding (at, (.))
import Control.Monad.SFML.Graphics
import Control.Monad.SFML
import qualified SFML.Graphics as G
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Lens

import Types
import Callbacks



--------------------------------------------------------------------------------
-- Ideally this takes a list of bounding boxes and textures the sprites
-- accordingly.
animation :: FilePath
          -> [G.IntRect]
          -> GameWire NominalDiffTime a
          -> Component
animation animArt boxes wire =
  Component Renderable (CAnimation (UninitializedAnimation clbk))
  where
    clbk = do
      frms <- forM boxes buildFrame
      return $ Animation (V.fromList frms) 0 (advanceAnimation wire)

    buildFrame box = do
      tex <- initTextureClbk animArt
      spr <- initSpriteClbk
      lift $ setTexture spr tex True
      lift $ setTextureRect spr box
      return $ AnimationFrame box spr


--------------------------------------------------------------------------------
advanceAnimation :: GameWire NominalDiffTime a -> AnimationStepper
advanceAnimation wire = AnimationStepper $ \anim -> do
  gameState <- gets . view $ gameTime
  (dt, _) <- stepSession gameState
  (res, wire') <- stepWire wire dt (Right $ dtime dt)
  case res of
    Left _ -> do
      liftIO $ print "no way"
      return (anim, advanceAnimation wire')
    Right _ -> do
      let newIdx = (anim ^. animationIdx + 1) `mod`
                   (V.length . view frames $ anim)
      liftIO $ print $ show newIdx
      let anim' = animationIdx .~ newIdx $ anim
      return (anim', advanceAnimation wire')
