module Events where

import Prelude hiding ((.))
import Control.Wire hiding (at)
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import qualified SFML.Graphics as G
import Safe hiding (at)

import Types
import Systems
import Wires
import Entities


--------------------------------------------------------------------------------
updateCaption :: Show a
              => GameWire NominalDiffTime a
              -> Entity
              -> GameMonad GameEvent
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
  return $ GameEvent (updateCaption wire')


--------------------------------------------------------------------------------
updateColour :: G.Color
             -> GameWire NominalDiffTime a
             -> Entity
             -> GameMonad GameEvent
updateColour targetCol wire e = do
  wire' <- stepTimed wire $ \_ ->
    case comp e ^. at Colour of
      Just (Component _ (RenderColour _)) ->
           e #.= Component Colour (RenderColour targetCol)
  return $ GameEvent (updateColour targetCol wire')


--------------------------------------------------------------------------------
toggleColour :: G.Color
             -> G.Color
             -> GameWire NominalDiffTime a
             -> Entity
             -> GameMonad GameEvent
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
      return $ GameEvent (toggleColour startCol endCol wire')
    Left _ -> do
      case comp e ^. at Colour of
        Just (Component _ (RenderColour _)) ->
             e #.= Component Colour (RenderColour startCol)
        _ -> return ()
      return $ GameEvent (toggleColour startCol endCol wire')
