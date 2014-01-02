module Systems where

import Prelude hiding ((.), id)
import Control.Monad
import Control.Wire hiding (at, when)
import Control.Monad.Trans.State
import Control.Monad.SFML
import Control.Monad.SFML.Graphics
import Control.Lens
import Control.Monad.Trans.Class (lift)
import qualified Data.IntMap.Strict as Map
import qualified Data.Map.Strict as SMap
import qualified Physics.Hipmunk as H
import qualified Data.StateVar as SV


import Types
import Components
import Physics


--------------------------------------------------------------------------------
comp :: Entity -> Components
comp = _components


--------------------------------------------------------------------------------
updateAll :: (Entity -> GameMonad ()) -> GameMonad ()
updateAll fn = do
  eMgr <- gets $ view entityMgr
  let allEntities = Map.elems eMgr
  mapM_ fn allEntities
  return ()


hipmunkSystem :: System
hipmunkSystem = System $ do
  pMgr <- gets $ view physicsMgr
  let wrld = pMgr ^. world
  sess <- gets $ view gameTime
  (dt, _) <- stepSession sess
  liftIO $ H.step wrld (fromIntegral . fromEnum $ dtime dt / 1e10)
  updateAll $ \e ->
    case liftM2 (,)
         (comp e ^. at CollisionShape)
         (comp e ^. at Position) of
     Just (Component _ (PhysicalShape (HipmunkUninitializedShape clbk)),
           _) -> do
       newShp <- clbk e
       e #.= Component CollisionShape
             (PhysicalShape (HipmunkInitializedShape newShp))
     Just (Component _ (PhysicalShape (HipmunkInitializedShape sh)),
           Component _ (PosInt pos)) -> do
       newPos <- liftIO $ SV.get . H.position $ H.body sh
       e #.= Component Position
             (PosInt (pos + fmap truncate (fromHipmunkVector newPos)))
     _ -> return ()


--------------------------------------------------------------------------------
newtonianSystem :: System
newtonianSystem = System $ updateAll $ \e ->
  case liftM2 (,)
       (comp e ^. at LinearForce)
       (comp e ^. at Position) of
    Just (Component _ (ForceInt ds),
          Component _ (PosInt pos)) ->
      e #.= Component Position (PosInt (pos + ds))
    _ -> return ()


--------------------------------------------------------------------------------
textSizeSystem :: System
textSizeSystem = System $ updateAll $ \e -> 
  case liftM2 (,)
       (comp e ^. at Renderable)
       (comp e ^. at Size) of
    Just (Component _ (Text t),
          Component _ (SizeInt sz)) ->
      lift $ setTextCharacterSize t sz
    _ -> return ()


--------------------------------------------------------------------------------
textCaptionSystem :: System
textCaptionSystem = System $ updateAll $ \e ->
  case liftM2 (,)
       (comp e ^. at Renderable)
       (comp e ^. at Caption) of
    Just (Component _ (Text t),
          Component _ (TextCaption cap)) -> lift $ setTextString t cap
    _ -> return ()


--------------------------------------------------------------------------------
eventSystem :: System
eventSystem = System $ updateAll $ \e ->
  case comp e ^. at EventListener of
    Just (Component _ (Events evts)) -> do
      steppedEvts <- forM evts $ \(GameEvent fn) -> fn e
      let newC = Component EventListener (Events steppedEvts)
      e #.= newC
    Nothing -> return ()


--------------------------------------------------------------------------------
textColourSystem :: System
textColourSystem = System $ updateAll $ \e ->
  case liftM2 (,)
       (comp e ^. at Renderable)
       (comp e ^. at Colour) of
    Just (Component _ (Text t),
          Component _ (RenderColour cl)) ->
      lift $ setTextColor t cl
    _ -> return ()


--------------------------------------------------------------------------------
-- Render and display the sprite in the current position.
-- TODO: Multicast for renderables?
-- e.g. blink
rendererSystem :: System
rendererSystem = System $ updateAll $ \e ->
  case comp e ^. at AffectRendering of
    Just (Component _ (MustRenderWire w)) -> do
      sess <- gets $ view gameTime
      (dt, _) <- stepSession sess
      (res, wire') <- stepWire w dt (Right (dtime dt))
      let newC = Component AffectRendering (MustRenderWire wire')
      e #.= newC
      case res of
        Right _ -> do
          updateSprite (comp e)
          updateText (comp e)
        Left  _ -> return ()
    _ -> do 
      updateSprite (comp e)
      updateText (comp e)


--------------------------------------------------------------------------------
updateText :: Components -> GameMonad ()
updateText co =
  case liftM2 (,) (co ^. at Renderable) (co ^. at Position) of
    Just (Component _ (Text t),
          Component _ (PosInt currentPos)) -> do
      win <- gets $ view gameWin
      let pos = Just $ translationFromV2 currentPos
      lift $ drawText win t pos
    _ -> return ()


--------------------------------------------------------------------------------
updateSprite :: Components -> GameMonad ()
updateSprite co =
  case liftM2 (,) (co ^. at Renderable) (co ^. at Position) of
    Just (Component _ (Sprite s),
          Component _ (PosInt currentPos)) -> do
      win <- gets $ view gameWin
      let pos = Just $ translationFromV2 currentPos
      lift $ drawSprite win s pos
    _ -> return ()


--------------------------------------------------------------------------------
inputSystem :: System
inputSystem = System $ updateAll $ \e ->
  case liftM2 (,)
       (SMap.lookup Keyboard (comp e))
       (SMap.lookup Position (comp e)) of
    Just (k@(Component _ (PlKbWire w)),
          c@(Component _ (PosInt oldPos))) -> do
     sess <- gets $ view gameTime
     (dt, _) <- stepSession sess
     (res, wire') <- stepWire w dt (Right (dtime dt))
     case res of
       Right ds -> do
         updateKbWire wire' k e
         let newC = compData .~ PosInt (oldPos + ds) $ c
         e #.= newC
       Left  _  -> updateKbWire wire' k e
    _ -> return ()

  where
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
