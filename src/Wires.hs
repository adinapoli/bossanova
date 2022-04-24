{-# LANGUAGE TupleSections #-}
module Wires where


import Prelude hiding ((.), id, until)
import Control.Applicative
import Control.Wire
import Linear.V2
import FRP.Netwire.Move
import qualified SFML.Window as W
import qualified SFML.Window.Keyboard as W
import Control.Monad.SFML.Window
import Control.Lens hiding (at)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Types
import Data.Foldable (asum)



--------------------------------------------------------------------------------
challenge1 :: GameWire st NominalDiffTime NominalDiffTime
challenge1 = for 10 . integral 0 . pure 20    -->
             for 10 . integral 0 . pure (-20) -->
             challenge1


--------------------------------------------------------------------------------
blinkWire :: NominalDiffTime -> NominalDiffTime -> GameWire st NominalDiffTime Bool
blinkWire cooldown blinkTime = after cooldown .
                               (for blinkTime . pure True -->
                                blinkWire cooldown blinkTime)


--------------------------------------------------------------------------------
ifPressedGo :: W.KeyCode -> V2 Int -> GameWire st NominalDiffTime (V2 Int)
ifPressedGo code coords = ifPressed code (pure coords)

ifPressed :: W.KeyCode -> GameMonad st a -> GameWire st NominalDiffTime a
ifPressed code m = mkGen_ $ \_ -> do
  keyPressed <- lift $ isKeyPressed code
  a <- m
  if keyPressed
     then return . Right $ a
     else return . Left $ ()

type PlayerControls st = GameWire st NominalDiffTime (V2 Int)

wasdControls :: Int -> PlayerControls st
wasdControls d = moveLeft d W.KeyA <|>
                 moveRight d W.KeyD <|>
                 moveUp d W.KeyW  <|>
                 moveDown d W.KeyS

arrowControls :: Int -> PlayerControls st
arrowControls d = arrowControls_X d <|>
                  moveUp d W.KeyUp  <|>
                  moveDown d W.KeyDown

arrowControls_X :: Int -> PlayerControls st
arrowControls_X d = moveLeft d W.KeyLeft <|> moveRight d W.KeyRight

--------------------------------------------------------------------------------
moveLeft :: Int -> W.KeyCode -> GameWire st NominalDiffTime (V2 Int)
moveLeft dx k = ifPressedGo k (V2 (-dx) 0)


--------------------------------------------------------------------------------
moveRight :: Int -> W.KeyCode -> GameWire st NominalDiffTime (V2 Int)
moveRight dx k = ifPressedGo k (V2 dx 0)


--------------------------------------------------------------------------------
moveUp :: Int -> W.KeyCode -> GameWire st NominalDiffTime (V2 Int)
moveUp dy k = ifPressedGo k (V2 0 (-dy))


--------------------------------------------------------------------------------
moveDown :: Int -> W.KeyCode -> GameWire st NominalDiffTime (V2 Int)
moveDown dy k = ifPressedGo k (V2 0 (dy))


--------------------------------------------------------------------------------
playerKeyboard :: Int -> PlayerControls st
playerKeyboard d = arrowControls d <|> inhibit ()

--------------------------------------------------------------------------------
seagullPlayerKeyboard :: Int -> GameWire st NominalDiffTime (V2 Int)
seagullPlayerKeyboard delta = arrowControls_X delta <|> inhibit ()

barberCombatPlayerKeyboard :: Int -> GameWire st NominalDiffTime (V2 Int)
barberCombatPlayerKeyboard delta =
  asum [
    ifPressed W.KeyRight $ do
      pure (V2 delta 0)
  , ifPressed W.KeyLeft $ do
      pure (V2 (-delta) 0)
  ]

--------------------------------------------------------------------------------
always :: GameWire st a Bool
always = pure True


--------------------------------------------------------------------------------
glowingText :: GameWire st NominalDiffTime Int
glowingText = for 0.5 . pure 20 -->
              for 0.5 . pure 22 -->
              for 0.5 . pure 24 -->
              glowingText


--------------------------------------------------------------------------------
stepTimed :: GameWire st NominalDiffTime b
          -> (b -> GameMonad st c)
          -> GameMonad st (GameWire st NominalDiffTime b)
stepTimed wire fn = do
  sess <- gets $ view gameTime
  (dt, _) <- stepSession sess
  (res, wire') <- stepWire wire dt (Right (dtime dt))
  either (const $ return wire') (\v -> fn v >> return wire') res


--------------------------------------------------------------------------------
stepTimed' :: GameWire st NominalDiffTime b
          -> (b -> GameMonad st c)
          -> (() -> GameMonad st d)
          -> GameMonad st (GameWire st NominalDiffTime b)
stepTimed' wire ok ko = do
  sess <- gets $ view gameTime
  (dt, _) <- stepSession sess
  (res, wire') <- stepWire wire dt (Right (dtime dt))
  case res of
    Right v -> ok v >> return wire'
    Left e -> ko e >> return wire'


stepEvery :: NominalDiffTime
          -> GameWire st NominalDiffTime NominalDiffTime
stepEvery interval = for interval . pure 0 -->
                     onlyOnce --> stepEvery interval

-- This works but is BAD, because is dependant upon time.
-- What we really want is something which triggers ONCE.
-- (for (interval + 0.01) --> stepEvery interval) . after interval

onlyOnce :: GameWire st NominalDiffTime NominalDiffTime
onlyOnce = mkGen_ $ \a ->
  return $ if mod (fromEnum a) 2 == 0 then Right a else Left ()
