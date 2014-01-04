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
import Entities
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


--------------------------------------------------------------------------------
hipmunkSystem :: System
hipmunkSystem = System $ do
  pMgr <- gets $ view physicsMgr
  let wrld = pMgr ^. world
  sess <- gets $ view gameTime
  (dt, _) <- stepSession sess
  liftIO $ H.step wrld (fromIntegral . fromEnum $ dtime dt / 1e10)
  updateAll $ \e -> do

    updateStaticBody e
    updateDynamicBody e
  where
    updateStaticBody e = 
      case comp e ^. at StaticBody of

       Just (Component _ (CollisionShape (HipmunkUninitializedShape clbk))) -> do
         when (e ^. alias == Special) $ liftIO $ print "init shp"
         newShp <- clbk e
         e #.= Component StaticBody
               (CollisionShape (HipmunkInitializedShape newShp))

       Just (Component _ (CollisionShape (HipmunkInitializedShape sh))) -> do
         newPos <- liftIO $ SV.get . H.position $ H.body sh
         when (e ^. alias == Special) (liftIO $ print $ "S->" ++ show newPos)

       _ -> return ()

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
             Component _ (PosInt pos)) -> do
         when (e ^. alias == Special) (liftIO $ print $ "-->" ++ show pos)
         newPos <- liftIO $ SV.get . H.position $ H.body sh
         e #.= Component Position
               (PosInt (fmap truncate (fromHipmunkVector newPos)))

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
spriteInitSystem :: System
spriteInitSystem = System $ updateAll $ \e ->
  case comp e ^. at Renderable of
    Just (Component _ (Sprite (UninitializedSprite clbk))) -> do
      s <- clbk
      e #.= Component Renderable (Sprite (InitializedSprite s))
    _ -> return ()

--------------------------------------------------------------------------------
textureInitSystem :: System
textureInitSystem = System $ updateAll $ \e ->
  case comp e ^. at Texture of
    Just (Component _ (SFMLTexture (UninitializedTexture clbk))) -> do
      t <- clbk
      e #.= Component Texture (SFMLTexture (InitializedTexture False t))
    _ -> return ()

--------------------------------------------------------------------------------
textureAttacherSystem :: System
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
textSizeSystem :: System
textSizeSystem = System $ updateAll $ \e -> 
  case liftM2 (,)
       (comp e ^. at Renderable)
       (comp e ^. at Size) of
    Just (Component _ (Text (InitializedText t)),
          Component _ (SizeInt sz)) ->
      lift $ setTextCharacterSize t sz
    _ -> return ()


--------------------------------------------------------------------------------
textInitSystem :: System
textInitSystem = System $ updateAll $ \e ->
  case comp e ^. at Renderable of
    Just (Component _ (Text (UninitializedText clbk))) -> do
      t <- clbk
      e #.= Component Renderable (Text (InitializedText t))
    _ -> return ()


--------------------------------------------------------------------------------
textCaptionSystem :: System
textCaptionSystem = System $ updateAll $ \e ->
  case liftM2 (,)
       (comp e ^. at Renderable)
       (comp e ^. at Caption) of
    Just (Component _ (Text (InitializedText t)),
          Component _ (TextCaption cap)) -> lift $ setTextString t cap
    _ -> return ()


--------------------------------------------------------------------------------
eventSystem :: System
eventSystem = System $ updateAll $ \e ->
  case comp e ^. at EventListener of
    Just (Component _ (Events evts)) -> do
      steppedEvts <- forM evts $ \(GameCallback fn) -> fn e
      let newC = Component EventListener (Events steppedEvts)
      e #.= newC
    Nothing -> return ()


--------------------------------------------------------------------------------
textColourSystem :: System
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
    Just (Component _ (Text (InitializedText t)),
          Component _ (PosInt currentPos)) -> do
      win <- gets $ view gameWin
      let pos = Just $ translationFromV2 currentPos
      lift $ drawText win t pos
    _ -> return ()


--------------------------------------------------------------------------------
updateSprite :: Components -> GameMonad ()
updateSprite co =
  case liftM2 (,) (co ^. at Renderable) (co ^. at Position) of
    Just (Component _ (Sprite (InitializedSprite s)),
          Component _ (PosInt currentPos)) -> do
      win <- gets $ view gameWin
      let pos = Just $ translationFromV2 currentPos
      lift $ drawSprite win s pos
    _ -> return ()


--------------------------------------------------------------------------------
inputSystem :: System
inputSystem = System $ updateAll $ \e -> do
  updateMouse e
  updateKeyboard e

  where
    updateMouse e =
      case SMap.lookup Mouse (comp e) of
        Just (Component _ (MouseCallback clbk)) -> clbk
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
