{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Animation where

import qualified Data.Vector as V
import Data.String
import Data.Aeson
import Data.Maybe
import Control.Lens.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Control.Monad
import qualified Control.Wire.Unsafe.Event as Unsafe
import Control.Wire hiding (at, (.))
import Control.Monad.SFML.Graphics
import Control.Monad.SFML
import qualified SFML.Graphics as G
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Lens

import Types
import Utils
import Callbacks



--------------------------------------------------------------------------------
-- Ideally this takes a list of bounding boxes and textures the sprites
-- accordingly.
animation :: FilePath
          -- ^ Where the animation json is stored
          -> Double
          -- ^ How much display each frame
          -> Component
animation pathToJson frameTime =
  Component Renderable (CAnimation (UninitializedAnimation clbk))
  where
    clbk :: GameMonad Animation
    clbk = do
      now <- liftIO milliTime
      allJson <- (fromJust . decode <$> liftIO (BL.readFile pathToJson)) :: GameMonad Value
      let (Just texFileName) = allJson ^? key "meta" . key "image" . _String
      let frms = buildFrames (fromJust $ allJson ^? key "frames" . _Array)
      spr <- initSpriteClbk
      tex <- initTextureClbk . fromString $
             (T.unpack $ "resources/anims/" <> texFileName)
      lift $ setTexture spr tex True
      lift $ setTextureRect spr (frms V.! 0)
      return Animation {
                _animationBoundingBoxes = frms
              , _animationSprite  = spr
              , _animationTexture = tex
              , _animationPlaying = True
              , _animationInternalTime = now
              , _animationCurrentIdx = 0
              , _animFrameTime = frameTime
              }

    buildFrames :: V.Vector Value -> V.Vector G.IntRect
    buildFrames = V.map (\v -> buildFrame . fromJust $ v ^? key "frame")

    buildFrame :: Value -> G.IntRect
    buildFrame v = G.IntRect top left width height
      where 
        top    = fromInteger . fromJust $ v ^? key "x" . _Integer
        left   = fromInteger . fromJust $ v ^? key "y" . _Integer
        width  = fromInteger . fromJust $ v ^? key "w" . _Integer
        height = fromInteger . fromJust $ v ^? key "h" . _Integer


--------------------------------------------------------------------------------
playAnimation :: Animation -> Animation
playAnimation anim = animationPlaying .~ True $ anim


--------------------------------------------------------------------------------
stopAnimation :: Animation -> Animation
stopAnimation anim = (animationPlaying .~ False) .
                     (animationCurrentIdx .~ 0) $ anim

--------------------------------------------------------------------------------
stepAnimation :: Animation -> GameMonad Animation
stepAnimation anim@Animation{..} = do
  now <- liftIO milliTime
  if anim ^. animationPlaying && now - _animationInternalTime >=
     (fromIntegral . fromEnum $ _animFrameTime)
    then do
      -- Time to step!
      let newIdx = (anim ^. animationCurrentIdx + 1) `mod`
                   (V.length (view animationBoundingBoxes anim))
      let newBB = (anim ^. animationBoundingBoxes) V.! newIdx
      lift $ setTextureRect (anim ^. animationSprite) newBB
      return $ (animationCurrentIdx .~ newIdx) .
               (animationInternalTime .~ now) $ anim
    else return anim
