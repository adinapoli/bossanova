{-# LANGUAGE RecordWildCards #-}

module Components where

import Prelude hiding ((.), id)
import qualified SFML.Graphics as G
import Types
import Control.Monad
import Control.Parallel.Strategies
import Control.Wire
import Control.Monad.Trans.State
import Control.Monad.SFML
import Control.Monad.SFML.Graphics
import Control.Lens hiding (at)
import Control.Monad.Trans.Class (lift)
import qualified SFML.System as S
import qualified Data.IntMap.Strict as Map


--------------------------------------------------------------------------------
-- Split and parallelise this on the long run to make it scale.
updateComponents :: Map.Key -> GameMonad ()
updateComponents idx = do
  gameState <- get
  eMgr <- gets $ view entityMgr
  let e = eMgr Map.! idx
  let lcomp = e ^. lcomponents
  let uicomp = e ^. uicomponents
  (logic, ui) <- runEval $ do
    tickedComponents <- rpar $ mapM (`tick` gameState) lcomp
    renderedComponents <- rpar $ mapM (`render` gameState) uicomp
    rseq tickedComponents
    rseq renderedComponents
    return $ liftM2 (,) tickedComponents renderedComponents
  entityMgr .= Map.insert idx (lcomponents .~ logic $ e) eMgr
  entityMgr .= Map.insert idx (uicomponents .~ ui $ e) eMgr


--------------------------------------------------------------------------------
spriteComponent :: G.Sprite
                -> GameWire NominalDiffTime b
                -> UIComponent
spriteComponent spr wire = UIComponent $ \GameState{..} -> do
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
                   -> LogicComponent
translateComponent spr wire = LogicComponent $ \GameState{..} -> do
                            sess <- gets $ view gameTime
                            (dt, sess') <- stepSession sess
                            (res, wire') <- stepWire wire dt (Right (dtime dt))
                            case res of
                              Right dx -> do
                                lift $ move spr
                                      (S.Vec2f (cos $ fromIntegral . fromEnum $ dx)
                                               (sin $ fromIntegral . fromEnum $ dx))
                                return (translateComponent spr wire')
                              Left  _ -> return $ translateComponent spr wire'

--------------------------------------------------------------------------------
moveComponent :: G.Sprite
              -> GameWire NominalDiffTime (Int, Int)
              -> LogicComponent
moveComponent spr wire = LogicComponent $ \GameState{..} -> do
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

--------------------------------------------------------------------------------
textSizeComponent :: G.Text
                  -> GameWire NominalDiffTime Int
                  -> UIComponent
textSizeComponent txt wire = UIComponent $ \GameState{..} -> do
                               sess <- gets $ view gameTime
                               (dt, sess') <- stepSession sess
                               (res, wire') <- stepWire wire dt (Right (dtime dt))
                               case res of
                                 Right v -> do
                                   lift $ setTextCharacterSize txt v
                                   lift $ drawText _gameWin txt Nothing
                                   return (textSizeComponent txt wire')
                                 Left  _ -> return $ textSizeComponent txt wire'
