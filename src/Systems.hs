{-# LANGUAGE RecordWildCards #-}

module Systems where

import Prelude hiding ((.), id)
import Linear.V2
import qualified SFML.Graphics as G
import Control.Monad
import Control.Parallel.Strategies
import Control.Wire hiding (at, when)
import Control.Monad.Trans.State
import Control.Monad.SFML
import Control.Monad.SFML.Graphics
import Control.Lens
import qualified Data.List as List
import Control.Concurrent.STM
import Control.Monad.Trans.Class (lift)
import qualified SFML.System as S
import qualified Data.IntMap.Strict as Map
import qualified Data.Map.Strict as SMap


import Types
import Components


--------------------------------------------------------------------------------
newtonianSystem :: System
newtonianSystem = System $ do
  sess <- gets $ view gameTime
  eMgr <- gets $ view entityMgr
  (dt, sess') <- stepSession sess
  --(res, wire') <- stepWire wire dt (Right (dtime dt))
  case Left () of
    Right v -> do
      let allEntities = Map.elems eMgr
      mapM_ updateSingle allEntities
      return ()
    Left  _ -> return ()
  where
    updateSingle :: Entity -> GameMonad ()
    updateSingle e = return () -- TODO


--------------------------------------------------------------------------------
textSizeSystem :: System
textSizeSystem = System $ do
  eMgr <- gets $ view entityMgr
  let allEntities = Map.elems eMgr
  mapM_ updateSingle allEntities
  return ()
  where
    updateSingle :: Entity -> GameMonad ()
    updateSingle e = let comp = _components e in
      case liftM2 (,) (comp ^. at Renderable) (comp ^. at Size) of
        Just (Component _ (Text t),
              Component _ (SizeInt sz)) ->
          lift $ setTextCharacterSize t sz
        _ -> return ()

--------------------------------------------------------------------------------
textCaptionSystem :: System
textCaptionSystem = System $ do
  eMgr <- gets $ view entityMgr
  let allEntities = Map.elems eMgr
  mapM_ updateSingle allEntities
  return ()
  where
    updateSingle :: Entity -> GameMonad ()
    updateSingle e = let comp = _components e in
      case liftM2 (,) (comp ^. at Renderable) (comp ^. at Caption) of
        Just (Component _ (Text t),
              Component _ (TextCaption cap)) -> lift $ setTextString t cap
        _ -> return ()


--------------------------------------------------------------------------------
eventSystem :: System
eventSystem = System $ do
  eMgr <- gets $ view entityMgr
  let allEntities = Map.elems eMgr
  mapM_ updateSingle allEntities
  return ()
  where
    updateSingle :: Entity -> GameMonad ()
    updateSingle e = case (comp e) ^. at EventListener of
        Just (Component _ (Events evts)) -> do
          steppedEvts <- forM evts $ \(GameEvent fn) -> fn e
          let newC = Component EventListener (Events steppedEvts)
          e #.= newC
        Nothing -> return ()


comp :: Entity -> Components
comp = _components

--------------------------------------------------------------------------------
textColourSystem :: System
textColourSystem = System $ do
  eMgr <- gets $ view entityMgr
  let allEntities = Map.elems eMgr
  mapM_ updateSingle allEntities
  return ()
  where
    updateSingle :: Entity -> GameMonad ()
    updateSingle e = let comp = _components e in
      case liftM2 (,) (comp ^. at Renderable) (comp ^. at Colour) of
        Just (Component _ (Text t),
              Component _ (RenderColour cl)) ->
          lift $ setTextColor t cl
        _ -> return ()


--------------------------------------------------------------------------------
-- Render and display the sprite in the current position.
-- TODO: Multicast for renderables?
-- e.g. blink
rendererSystem :: System
rendererSystem = System $ do
  eMgr <- gets $ view entityMgr
  let allEntities = Map.elems eMgr
  mapM_ updateSingle allEntities
  return ()
  where
    updateSingle :: Entity -> GameMonad ()
    updateSingle e = let comp = _components e in
      case (comp ^. at AffectRendering) of
        Just (Component _ (MustRenderWire w)) -> do
          sess <- gets $ view gameTime
          (dt, sess') <- stepSession sess
          (res, wire') <- stepWire w dt (Right (dtime dt))
          let newC = Component AffectRendering (MustRenderWire wire')
          e #.= newC
          case res of
            Right _ -> do
              updateSprite comp
              updateText comp
            Left  _ -> return ()
        _ -> do 
          updateSprite comp
          updateText comp


--------------------------------------------------------------------------------
updateText :: Components -> GameMonad ()
updateText comp =
  case liftM2 (,) (comp ^. at Renderable) (comp ^. at Position) of
    Just (Component _ (Text t),
          Component _ (PosInt currentPos)) -> do
      win <- gets $ view gameWin
      let pos = Just $ translationFromV2 currentPos
      lift $ drawText win t pos
    _ -> return ()


--------------------------------------------------------------------------------
updateSprite :: Components -> GameMonad ()
updateSprite comp =
  case liftM2 (,) (comp ^. at Renderable) (comp ^. at Position) of
    Just (Component _ (Sprite s),
          Component _ (PosInt currentPos)) -> do
      win <- gets $ view gameWin
      let pos = Just $ translationFromV2 currentPos
      lift $ drawSprite win s pos
    _ -> return ()


--------------------------------------------------------------------------------
inputSystem :: System
inputSystem = System $ do
  eMgr <- gets $ view entityMgr
  let allEntities = Map.elems eMgr
  mapM_ updateSingle allEntities
  return ()
  where
    updateSingle :: Entity -> GameMonad ()
    updateSingle e = let comp = _components e in
      case liftM2 (,) (SMap.lookup Keyboard comp) (SMap.lookup Position comp) of
        Just (k@(Component _ (PlKbWire w)),
              c@(Component _ (PosInt oldPos))) -> do
         sess <- gets $ view gameTime
         (dt, sess') <- stepSession sess
         (res, wire') <- stepWire w dt (Right (dtime dt))
         case res of
           Right ds -> do
             updateKbWire wire' k e
             let newC = compData .~ PosInt (oldPos + ds) $ c
             e #.= newC
           Left  _  -> updateKbWire wire' k e
        _ -> return ()

    updateKbWire wire k e = do
      let newK = compData .~ PlKbWire wire $ k
      e #.= newK


--------------------------------------------------------------------------------
-- Common pitfall: Once you update the entity you must ask the entityManager
-- for the new entity again, since everything is immutable. To avoid this, we
-- don't update the entity directly, but we just use its ID to fetch the entity
-- from the EntityManager.
(#.=) :: Entity -> Component -> GameMonad ()
ent #.= newC = do
  eMgr <- gets $ view entityMgr
  case eMgr ^. at (_eId ent) of
    Just oldE -> do
      let tg = _compTag newC
      let newE = components .~ SMap.insert tg newC (_components oldE) $ ent
      entityMgr .= Map.insert (_eId newE) newE eMgr
    Nothing -> return ()
