module Events where

import Prelude hiding ((.))
import Control.Wire hiding (at)
import Control.Lens
import Control.Monad
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
