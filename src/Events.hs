module Events where

import Prelude hiding ((.))
import Control.Wire hiding (at)
import Control.Lens
import Control.Monad
import Control.Monad.SFML
import Control.Monad.Trans.State
import qualified SFML.Graphics as G
import Safe hiding (at)

import Types
import Utils
import Systems
import Wires
import Entities


--------------------------------------------------------------------------------
updateCaption :: Show a
              => GameWire NominalDiffTime a
              -> Entity
              -> GameMonad GameCallback
updateCaption wire e = do
  wire' <- stepTimed wire $ \_ -> do
    pl  <- entityByAlias ThePlayer
    case liftM2 (,)
         (headMay pl >>= \e' -> comp e' ^. at Position)
         (comp e ^. at Caption) of
      Just ( Component _ (PosInt pos)
           , Component _ (TextCaption _)) -> do
        let newC = Component Caption (TextCaption (show pos))
        e #.= newC
      Nothing -> return ()
  return $ GameCallback (updateCaption wire')


--------------------------------------------------------------------------------
updateColour :: G.Color
             -> GameWire NominalDiffTime a
             -> Entity
             -> GameMonad GameCallback
updateColour targetCol wire e = do
  wire' <- stepTimed wire $ \_ ->
    case comp e ^. at Colour of
      Just (Component _ (RenderColour _)) ->
           e #.= Component Colour (RenderColour targetCol)
  return $ GameCallback (updateColour targetCol wire')


--------------------------------------------------------------------------------
toggleColour :: G.Color
             -> G.Color
             -> GameWire NominalDiffTime a
             -> Entity
             -> GameMonad GameCallback
toggleColour startCol endCol wire e = do
  sess <- gets $ view gameTime
  (dt, _) <- stepSession sess
  (res, wire') <- stepWire wire dt (Right (dtime dt))
  case res of
    Right _ -> do
      case comp e ^. at Colour of
        Just (Component _ (RenderColour col)) ->
             if col == startCol
                then e #.= Component Colour (RenderColour endCol)
                else e #.= Component Colour (RenderColour startCol)
        _ -> return ()
      return $ GameCallback (toggleColour startCol endCol wire')
    Left _ -> do
      case comp e ^. at Colour of
        Just (Component _ (RenderColour _)) ->
             e #.= Component Colour (RenderColour startCol)
        _ -> return ()
      return $ GameCallback (toggleColour startCol endCol wire')



------------------------------------------------------------------------------
displayPhysicsBodyCount :: Entity -> GameMonad GameCallback
displayPhysicsBodyCount e = do
  pMgr <- gets . view $ managers . physicsMgr
  bc  <- entityByAlias BodyCounter
  case (headMay bc >>= \e' -> comp e' ^. at Caption) of
    Just (Component _ (TextCaption _)) -> do
      let newT = "Bodies: " ++ show (pMgr ^. bodies)
      let newC = Component Caption (TextCaption newT)
      e #.= newC
      return $ GameCallback displayPhysicsBodyCount
    Nothing -> return $ GameCallback displayPhysicsBodyCount

------------------------------------------------------------------------------
displaySpritesCount :: Entity -> GameMonad GameCallback
displaySpritesCount e = do
  artMgr <- gets . view $ managers . artMgr
  bc  <- entityByAlias SpriteCounter
  case (headMay bc >>= \e' -> comp e' ^. at Caption) of
    Just (Component _ (TextCaption _)) -> do
      let newT = "Sprites: " ++ show (artMgr ^. sprites)
      let newC = Component Caption (TextCaption newT)
      e #.= newC
      return $ GameCallback displaySpritesCount
    Nothing -> return $ GameCallback displaySpritesCount


--------------------------------------------------------------------------------
updateAndDisplayFPS :: Entity -> GameMonad GameCallback
updateAndDisplayFPS e = do
  fTime <- gets $ view frameTime
  fps' <- gets $ view fps
  curTime <- liftIO milliTime
  let dt = curTime - fTime
  if dt >= 1000
    then do
      frameTime .= curTime
      fps .= 0
      fc  <- entityByAlias FPSCounter
      case (headMay fc >>= \e' -> comp e' ^. at Caption) of
        Just (Component _ (TextCaption _)) -> do
          let newT = "FPS: " ++ show (fps' + 1)
          let newC = Component Caption (TextCaption newT)
          e #.= newC
          return $ GameCallback updateAndDisplayFPS
        Nothing -> return $ GameCallback updateAndDisplayFPS
    else do
      fps += 1
      return $ GameCallback updateAndDisplayFPS
