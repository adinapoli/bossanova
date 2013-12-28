{-# LANGUAGE RecordWildCards #-}

module Systems where

import Prelude hiding ((.), id)
import Linear.V2
import qualified SFML.Graphics as G
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
import qualified Data.Map.Strict as SMap


import Types
import Components


--------------------------------------------------------------------------------
newtonianSystem :: GameWire NominalDiffTime NominalDiffTime -> System
newtonianSystem wire = System $ \GameState{..} -> do
  sess <- gets $ view gameTime
  (dt, sess') <- stepSession sess
  (res, wire') <- stepWire wire dt (Right (dtime dt))
  case res of
    Right v -> do
      let allEntities = Map.elems _entityMgr
      mapM_ updateSingle allEntities
      return $ newtonianSystem wire'
    Left  _ -> return $ newtonianSystem wire'
  where
    updateSingle :: Entity -> GameMonad ()
    updateSingle e = return () -- TODO



--------------------------------------------------------------------------------
-- Render and display the sprite in the current position.
rendererSystem :: GameWire NominalDiffTime Bool -> System
rendererSystem wire = System $ \GameState{..} -> do
  sess <- gets $ view gameTime
  (dt, sess') <- stepSession sess
  (res, wire') <- stepWire wire dt (Right (dtime dt))
  case res of
    Right v -> do
      let allEntities = Map.elems _entityMgr
      mapM_ updateSingle allEntities
      return $ rendererSystem wire'
    Left  _ -> return $ rendererSystem wire'
  where
    updateSingle :: Entity -> GameMonad ()
    updateSingle e = let comp = _components e in
      case liftM2 (,)
           (SMap.lookup Renderable comp)
           (SMap.lookup Position comp) of
        Just (Component _ (Sprite s),
              Component _ (PosInt currentPos)) -> do
          win <- gets $ view gameWin
          let pos = Just $ translationFromV2 currentPos
          lift $ drawSprite win s pos
        _ -> return ()


--------------------------------------------------------------------------------
inputSystem :: GameWire NominalDiffTime (Int, Int) -> System
inputSystem wire = System $ \GameState{..} -> do
  sess <- gets $ view gameTime
  (dt, sess') <- stepSession sess
  (res, wire') <- stepWire wire dt (Right (dtime dt))
  case res of
    Right (dx, dy) -> do
      let allEntities = Map.elems _entityMgr
      mapM_ (updateSingle dx dy) allEntities
      return $ inputSystem wire'
    Left  _ -> return $ inputSystem wire'
  where
    updateSingle :: Int -> Int -> Entity -> GameMonad ()
    updateSingle dx dy e = let comp = _components e in
      case liftM2 (,) (SMap.lookup Keyboard comp) (SMap.lookup Position comp) of
        Just (_, c@(Component _ (PosInt oldPos))) -> do
         eMgr <- gets $ view entityMgr
         let newC = compData .~ PosInt (oldPos + V2 dx dy) $ c
         let newE = components .~ SMap.insert Position newC comp $ e
         entityMgr .= Map.insert (_eId newE) newE eMgr
        _ -> return ()
