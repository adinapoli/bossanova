{-# LANGUAGE TupleSections #-}
module Wires where


import Prelude hiding ((.), id)
import Control.Wire
import Linear.V2
import FRP.Netwire.Move
import qualified SFML.Window as W
import Control.Monad.SFML.Window
import Control.Lens hiding (at)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Types



--------------------------------------------------------------------------------
challenge1 :: GameWire NominalDiffTime NominalDiffTime
challenge1 = for 10 . integral 0 . pure 20    -->
             for 10 . integral 0 . pure (-20) -->
             challenge1


--------------------------------------------------------------------------------
blinkWire :: NominalDiffTime -> NominalDiffTime -> GameWire NominalDiffTime Bool
blinkWire cooldown blinkTime = after cooldown .
                               (for blinkTime . pure True -->
                                blinkWire cooldown blinkTime)


--------------------------------------------------------------------------------
ifPressedGo :: W.KeyCode -> V2 Int -> GameWire NominalDiffTime (V2 Int)
ifPressedGo code coords = mkGen_ $ \_ -> do
  keyPressed <- lift $ isKeyPressed code
  if keyPressed
     then return . Right $ coords
     else return . Left $ ()

--------------------------------------------------------------------------------
moveLeft :: GameWire NominalDiffTime (V2 Int)
moveLeft = ifPressedGo W.KeyA (V2 (-5) 0)


--------------------------------------------------------------------------------
moveRight :: GameWire NominalDiffTime (V2 Int)
moveRight = ifPressedGo W.KeyD (V2 5 0)


--------------------------------------------------------------------------------
moveUp :: GameWire NominalDiffTime (V2 Int)
moveUp = ifPressedGo W.KeyW (V2 0 (-5))


--------------------------------------------------------------------------------
moveDown :: GameWire NominalDiffTime (V2 Int)
moveDown = ifPressedGo W.KeyS (V2 0 5)


--------------------------------------------------------------------------------
playerKeyboard :: GameWire NominalDiffTime (V2 Int)
playerKeyboard = moveLeft <|>
                 moveRight <|>
                 moveUp <|> 
                 moveDown <|>
                 inhibit ()


--------------------------------------------------------------------------------
always :: GameWire a Bool
always = pure True


--------------------------------------------------------------------------------
glowingText :: GameWire NominalDiffTime Int
glowingText = for 0.5 . pure 20 -->
              for 0.5 . pure 22 -->
              for 0.5 . pure 24 -->
              glowingText


--------------------------------------------------------------------------------
stepTimed :: GameWire NominalDiffTime b
          -> (b -> GameMonad c)
          -> GameMonad (GameWire NominalDiffTime b)
stepTimed wire fn = do
  sess <- gets $ view gameTime
  (dt, _) <- stepSession sess
  (res, wire') <- stepWire wire dt (Right (dtime dt))
  either (const $ return wire') (\v -> fn v >> return wire') res


--------------------------------------------------------------------------------
stepTimed' :: GameWire NominalDiffTime b
          -> (b -> GameMonad c)
          -> (() -> GameMonad d)
          -> GameMonad (GameWire NominalDiffTime b)
stepTimed' wire ok ko = do
  sess <- gets $ view gameTime
  (dt, _) <- stepSession sess
  (res, wire') <- stepWire wire dt (Right (dtime dt))
  case res of
    Right v -> ok v >> return wire'
    Left e -> ko e >> return wire'
