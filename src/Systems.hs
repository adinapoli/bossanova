module Systems where

import Prelude hiding ((.), id)
import Control.Monad
import Data.Vector ((!))
import Control.Parallel.Strategies
import Control.Concurrent.STM
import Linear.V2
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
import Entities
import Physics
import Settings
import Animation
import Utils


--------------------------------------------------------------------------------
comp :: Entity st -> Components st
comp = _components


--------------------------------------------------------------------------------
updateAll :: (Entity st -> GameMonad st ()) -> GameMonad st ()
updateAll fn = do
  eMgr <- gets . view $ managers . entityMgr . entities
  let allEntities = Map.elems eMgr
  --sequence_ $ parMap rpar fn allEntities
  mapM_ fn allEntities



--------------------------------------------------------------------------------
-- For now it does not use the pool and the SFML camera.
-- this causes the physic sprites to go crazy. why?
-- It's also dangerous to naively acquire the entity and modify
-- it and and interleave might be:
-- a) Get hold of the underlying physic body (hipmunkSystem)
-- b) The deallocator kicks in and delete the object (deallocatorSystem)
-- c) HipmunkSystem tried to do something with the body. Crash!
-- For this reason atm I'm running everything in a single thread.
deallocatorSystem :: System st
deallocatorSystem = System $ updateAll $ \e ->
  case liftM2 (,)
       (comp e ^. at Position)
       (comp e ^. at Disposable) of
    Just (Component _ (PosInt (V2 x y)), _) ->
      when (x > windowWidth || x < 0 ||
            y > windowHeight || y < 0) $ do
           (#.~) e
           returnResources e
    _ -> return ()


--------------------------------------------------------------------------------
returnResources :: Entity st -> GameMonad st ()
returnResources e = returnPhysicResources >> returnRenderingResources
  where
    returnPhysicResources = case comp e ^. at DynamicBody of
      Just (Component DynamicBody (CollisionShape (HipmunkInitializedShape sh))) -> do
        wrld <- gets . view $ managers . physicsMgr . world
        pool <- gets . view $ managers . physicsMgr . bodyPool
        let body = H.body sh
        liftIO $ H.spaceRemove wrld body
        liftIO $ H.spaceRemove wrld sh
        liftIO $ H.resetForces body
        liftIO $ H.velocity body SV.$= 0
        liftIO $ atomically $ writeTQueue pool body
        managers . physicsMgr . bodyPool .= pool
      _ -> return ()

    returnRenderingResources = case comp e ^. at Renderable of
      Just (Component _ (Sprite (InitializedSprite s))) -> do
        spool <- gets . view $ managers . artMgr . spritePool
        liftIO $ atomically $ writeTQueue spool s
        managers . artMgr . spritePool .= spool
      _ -> return () -- don't dispose for now


--------------------------------------------------------------------------------
hipmunkSystem :: System st
hipmunkSystem = System $ do
  pMgr <- gets . view $ managers . physicsMgr
  let wrld = pMgr ^. world
  liftIO $ H.step wrld 1.0
  updateAll $ \e -> do
    updateStaticBody e
    updateDynamicBody e
  where

    ----------------------------------------------------------------------------
    updateStaticBody e =
      case comp e ^. at StaticBody of

       Just (Component _ (CollisionShape (HipmunkUninitializedShape clbk))) -> do
         newShp <- clbk e
         e #.= Component StaticBody
               (CollisionShape (HipmunkInitializedShape newShp))

       Just (Component _ (CollisionShape (HipmunkInitializedShape sh))) -> do
         case comp e ^. at Position of
           Just (Component _ (PosInt pos)) -> do
             liftIO $ H.position (H.body sh) SV.$= toHipmunkVectorI pos
           _ -> return ()

       _ -> return ()

    ----------------------------------------------------------------------------
    updateDynamicBody e =
      case liftM2 (,)
           (comp e ^. at DynamicBody)
           (comp e ^. at Position) of

       Just (Component _ (CollisionShape (HipmunkUninitializedShape clbk)),
             _) -> do
         newShp <- clbk e
         e #.= Component DynamicBody
               (CollisionShape (HipmunkInitializedShape newShp))

       Just (Component DynamicBody (CollisionShape (HipmunkInitializedShape sh)),
             Component _ (PosInt _)) -> do
         newPos <- liftIO $ SV.get . H.position $ H.body sh
         e #.= Component Position
               (PosInt (fmap truncate (fromHipmunkVector newPos)))

       _ -> return ()


--------------------------------------------------------------------------------
newtonianSystem :: System st
newtonianSystem = System $ updateAll $ \e ->
  case liftM2 (,)
       (comp e ^. at LinearForce)
       (comp e ^. at Position) of
    Just (Component _ (ForceInt ds),
          Component _ (PosInt pos)) ->
      e #.= Component Position (PosInt (pos + ds))
    _ -> return ()

--------------------------------------------------------------------------------
spriteInitSystem :: System st
spriteInitSystem = System $ updateAll $ \e ->
  case comp e ^. at Renderable of
    Just (Component _ (Sprite (UninitializedSprite clbk))) -> do
      s <- clbk
      e #.= Component Renderable (Sprite (InitializedSprite s))
    _ -> return ()

--------------------------------------------------------------------------------
textureInitSystem :: System st
textureInitSystem = System $ updateAll $ \e ->
  case comp e ^. at Texture of
    Just (Component _ (SFMLTexture (UninitializedTexture clbk))) -> do
      t <- clbk
      e #.= Component Texture (SFMLTexture (InitializedTexture False t))
    _ -> return ()

--------------------------------------------------------------------------------
textureAttacherSystem :: System st
textureAttacherSystem = System $ updateAll $ \e ->
  case liftM3 (,,)
       (comp e ^. at Texture)
       (comp e ^. at Renderable)
       (comp e ^. at BoundingBox) of
    Just (Component _ (SFMLTexture (InitializedTexture False t))
         ,Component _ (Sprite (InitializedSprite spr))
         ,Component _ (IntRect r)) -> do
      lift $ setTexture spr t True
      lift $ setTextureRect spr r
      e #.= Component Texture (SFMLTexture (InitializedTexture True t))
    _ -> return ()

--------------------------------------------------------------------------------
textSizeSystem :: System st
textSizeSystem = System $ updateAll $ \e ->
  case liftM2 (,)
       (comp e ^. at Renderable)
       (comp e ^. at Size) of
    Just (Component _ (Text (InitializedText t)),
          Component _ (SizeInt sz)) ->
      lift $ setTextCharacterSize t sz
    _ -> return ()


--------------------------------------------------------------------------------
textInitSystem :: System st
textInitSystem = System $ updateAll $ \e ->
  case comp e ^. at Renderable of
    Just (Component _ (Text (UninitializedText clbk))) -> do
      t <- clbk
      e #.= Component Renderable (Text (InitializedText t))
    _ -> return ()


--------------------------------------------------------------------------------
textCaptionSystem :: System st
textCaptionSystem = System $ updateAll $ \e ->
  case liftM2 (,)
       (comp e ^. at Renderable)
       (comp e ^. at Caption) of
    Just (Component _ (Text (InitializedText t)),
          Component _ (TextCaption cap)) -> lift $ setTextString t cap
    _ -> return ()


--------------------------------------------------------------------------------
eventSystem :: System st
eventSystem = System $ updateAll $ \e ->
  case comp e ^. at EventListener of
    Just (Component _ (Events evts)) -> do
      steppedEvts <- forM evts $ \(GameCallback fn) -> fn e
      let newC = Component EventListener (Events steppedEvts)
      e #.= newC
    _ -> return ()


--------------------------------------------------------------------------------
textColourSystem :: System st
textColourSystem = System $ updateAll $ \e ->
  case liftM2 (,)
       (comp e ^. at Renderable)
       (comp e ^. at Colour) of
    Just (Component _ (Text (InitializedText t)),
          Component _ (RenderColour cl)) ->
      lift $ setTextColor t cl
    _ -> return ()


--------------------------------------------------------------------------------
-- Render and display the sprite in the current position.
rendererSystem :: System st
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
updateText :: Components st -> GameMonad st ()
updateText co =
  case liftM2 (,) (co ^. at Renderable) (co ^. at Position) of
    Just (Component _ (Text (InitializedText t)),
          Component _ (PosInt currentPos)) -> do
      win <- gets $ view gameWin
      let pos = Just $ translationFromV2 currentPos
      lift $ drawText win t pos
    _ -> return ()


--------------------------------------------------------------------------------
updateSprite :: Components st -> GameMonad st ()
updateSprite co =
  case liftM2 (,) (co ^. at Renderable) (co ^. at Position) of
    Just (Component _ (Sprite (InitializedSprite s)),
          Component _ (PosInt currentPos)) -> do
      win <- gets $ view gameWin
      let pos = Just $ translationFromV2 currentPos
      lift $ drawSprite win s pos
    _ -> return ()


--------------------------------------------------------------------------------
inputSystem :: System st
inputSystem = System $ updateAll $ \e -> do
  updateMouse e
  updateKeyboard e

  where
    updateMouse e =
      case SMap.lookup Callback (comp e) of
        Just (Component _ (CCallback clbk)) -> do
          clbk' <- runEvent clbk e
          e #.= Component Callback (CCallback clbk')
        _ -> return ()
    updateKeyboard e =
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

    updateKbWire wire k e = do
      let newK = compData .~ PlKbWire wire $ k
      e #.= newK

--------------------------------------------------------------------------------
-- Render and display the sprite in the current position.
animationSystem :: System st
animationSystem = System $ updateAll $ \e ->
  case liftM2 (,)
       (comp e ^. at Position)
       (comp e ^. at Renderable) of
    Just (_,
          Component _ (CAnimation (UninitializedAnimation clbk))) -> do
      anim <- clbk
      let newC = Component Renderable (CAnimation (InitializedAnimation anim))
      e #.= newC
    Just ( Component _ (PosInt currentPos)
         , Component _ (CAnimation (InitializedAnimation anim))) -> do
     win <- gets $ view gameWin
     anim' <- stepAnimation anim
     let pos = Just $ translationFromV2 currentPos
     lift $ drawSprite win (_animationSprite anim') pos
     let newC = Component Renderable (CAnimation (InitializedAnimation anim'))
     e #.= newC
    _ -> return ()
