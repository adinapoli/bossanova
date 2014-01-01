module Events where

import Prelude hiding ((.))
import Control.Wire hiding (at)
import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.SFML
import Types
import Systems


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
    Right v -> let comp = _components e in
      case comp ^. at Caption of
        Just (Component _ (TextCaption _)) -> do
          let newC = Component Caption (TextCaption (show v))
          e #.= newC
          case (_components e) ^. at Caption of
            Just (Component _ (TextCaption cap)) -> liftIO $ print cap
            Nothing -> return ()
          return $ GameEvent (updateCaption wire')
    Left _ -> return $ GameEvent (updateCaption wire')
