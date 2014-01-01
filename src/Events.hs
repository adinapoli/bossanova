module Events where

import Prelude hiding ((.))
import Control.Wire hiding (at)
import Control.Lens
import Control.Monad.Trans.State
import qualified SFML.Graphics as G

import Types
import Systems
import Wires


--------------------------------------------------------------------------------
updateCaption :: Show a
              => GameWire NominalDiffTime a
              -> Entity
              -> GameMonad GameEvent
updateCaption wire e = do
  sess <- gets $ view gameTime
  (dt, _) <- stepSession sess
  (res, wire') <- stepWire wire dt (Right (dtime dt))
  case res of
    Right v -> case comp e ^. at Caption of
      Just (Component _ (TextCaption _)) -> do
        let newC = Component Caption (TextCaption (show v))
        e #.= newC
        return $ GameEvent (updateCaption wire')
    Left _ -> return $ GameEvent (updateCaption wire')


--------------------------------------------------------------------------------
updateColour :: GameWire NominalDiffTime a
             -> Entity
             -> GameMonad GameEvent
updateColour wire e = do
  wire' <- stepTimed wire $ \_ ->
    case comp e ^. at Colour of
      Just (Component _ (RenderColour col)) -> if col == G.blue
           then e #.= Component Colour (RenderColour G.magenta)
           else e #.= Component Colour (RenderColour G.blue)
  return $ GameEvent (updateColour wire')
