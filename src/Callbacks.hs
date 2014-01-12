module Callbacks where

import qualified SFML.Graphics as G
import Control.Monad.SFML.Graphics
import Control.Concurrent.STM
import Control.Monad.SFML
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Lens
import qualified Data.Map.Strict as SMap

import Types


--------------------------------------------------------------------------------
initTextClbk :: GameMonad G.Text
initTextClbk = do
  t <- lift createText
  fnt <- lift $ fontFromFile "resources/ProFont.ttf"
  lift $ setTextFont t fnt
  return t

--------------------------------------------------------------------------------
initSpriteClbk :: GameMonad G.Sprite
initSpriteClbk = do
  spool <- gets . view $ managers . artMgr . spritePool
  spr <- liftIO $ atomically $ tryReadTQueue spool
  case spr of
    Nothing -> do
      managers . artMgr . sprites += 1
      lift createSprite
    Just s -> return s

--------------------------------------------------------------------------------
initTextureClbk :: FilePath -> GameMonad G.Texture
initTextureClbk path = do
  aMgr <- gets . view $ managers . artMgr . textures
  case aMgr ^. at path of
    Just t -> return t
    Nothing -> do
      tex <- lift $ textureFromFile path Nothing
      managers . artMgr . textures .= SMap.insert path tex aMgr
      return tex
