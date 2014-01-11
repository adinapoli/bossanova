module Callbacks where

import qualified SFML.Graphics as G
import Control.Monad.SFML.Graphics
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
initSpriteClbk = lift createSprite

--------------------------------------------------------------------------------
initTextureClbk :: FilePath -> GameMonad G.Texture
initTextureClbk path = do
  aMgr <- gets $ view artMgr
  case aMgr ^. at path of
    Just t -> return t
    Nothing -> do
      tex <- lift $ textureFromFile path Nothing
      artMgr .= SMap.insert path tex aMgr
      return tex
