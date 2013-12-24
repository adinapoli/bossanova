{-# LANGUAGE RecordWildCards #-}

module Components where

import Prelude hiding ((.), id)
import qualified SFML.Graphics as G
import Types
import Control.Wire
import Control.Monad.Trans.State
import Control.Monad.SFML
import Control.Monad.SFML.Graphics
import Control.Lens hiding (at)
import Control.Monad.Trans.Class (lift)
import qualified SFML.System as S
import qualified Data.IntMap.Strict as Map


--------------------------------------------------------------------------------
tickComponents :: Map.Key -> GameMonad ()
tickComponents idx = do
  gameState <- get
  eMgr <- gets $ view entityMgr
  let e = eMgr Map.! idx
  let comp = e ^. components
  tickedComponents <- mapM (`tick` gameState) comp
  entityMgr .= Map.insert idx (components .~ tickedComponents $ e) eMgr


--------------------------------------------------------------------------------
spriteComponent :: G.Sprite
                -> GameWire NominalDiffTime b
                -> Component
spriteComponent spr wire = Component $ \GameState{..} -> do
                            sess <- gets $ view gameTime
                            (dt, sess') <- stepSession sess
                            (res, wire') <- stepWire wire dt (Right (dtime dt))
                            case res of
                              Right _ -> do
                                lift $ drawSprite _gameWin spr Nothing
                                return (spriteComponent spr wire')
                              Left  _ -> return $ spriteComponent spr wire'


--------------------------------------------------------------------------------
translateComponent :: G.Sprite -> GameWire NominalDiffTime NominalDiffTime
                   -> Component
translateComponent spr wire = Component $ \GameState{..} -> do
                            sess <- gets $ view gameTime
                            (dt, sess') <- stepSession sess
                            (res, wire') <- stepWire wire dt (Right (dtime dt))
                            case res of
                              Right dx -> do
                                lift $ move spr
                                      (S.Vec2f (cos $ fromIntegral . fromEnum $ dx)
                                               (cos $ fromIntegral . fromEnum $ dx))
                                return (translateComponent spr wire')
                              Left  _ -> return $ translateComponent spr wire'

--------------------------------------------------------------------------------
moveComponent :: G.Sprite
              -> GameWire NominalDiffTime (Int, Int)
              -> Component
moveComponent spr wire = Component $ \GameState{..} -> do
                            sess <- gets $ view gameTime
                            (dt, sess') <- stepSession sess
                            (res, wire') <- stepWire wire dt (Right (dtime dt))
                            case res of
                              Right (dx, dy) -> do
                                lift $ move spr
                                      (S.Vec2f (fromIntegral dx)
                                               (fromIntegral dy))
                                return (moveComponent spr wire')
                              Left  _ -> return $ moveComponent spr wire'
