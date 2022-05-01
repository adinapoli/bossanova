{-# LANGUAGE TupleSections #-}
module Wires where


import Prelude hiding ((.), id, until)
import Control.Applicative
import Control.Wire hiding (Last)
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
import Control.Monad.SFML (SFML)
import qualified SFML.Window.Keyboard as SFML
import Control.Monad (forM)
import Data.Maybe (catMaybes)
import qualified Data.List as L


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
ifPressedGo :: W.KeyCode -> a -> PlayerControls st a
ifPressedGo code coords = ifPressed code (id, coords)

ifPressed :: W.KeyCode -> (st -> st, a) -> PlayerControls st a
ifPressed code (f, a) = mkPure_ $ \pressedThisFrame ->
  if L.elem code pressedThisFrame then Right (f, a) else Left ()

type PlayerControls st a = Wire (StateDelta st) () SFML [SFML.KeyCode] (st -> st, a)

wasdControls :: Int -> PlayerControls st (V2 Int)
wasdControls d = moveLeft d W.KeyA <|>
                 moveRight d W.KeyD <|>
                 moveUp d W.KeyW  <|>
                 moveDown d W.KeyS

arrowControls :: Int -> PlayerControls st (V2 Int)
arrowControls d = arrowControls_X d <|>
                  moveUp d W.KeyUp  <|>
                  moveDown d W.KeyDown

arrowControls_X :: Int -> PlayerControls st (V2 Int)
arrowControls_X d = moveLeft d W.KeyLeft <|> moveRight d W.KeyRight

pressedKeysWire :: [SFML.KeyCode] -> Wire s () SFML a [SFML.KeyCode]
pressedKeysWire maybePressed = mkGen_ $ \_ -> do
  allPressed <- catMaybes <$>
    forM maybePressed (\k -> isKeyPressed k >>= \p -> pure $ if p then Just k else Nothing)
  case allPressed of
    [] -> pure $ Left ()
    x  -> pure $ Right x

--------------------------------------------------------------------------------
moveLeft :: Int -> W.KeyCode -> PlayerControls st (V2 Int)
moveLeft dx k = ifPressedGo k (V2 (-dx) 0)

--------------------------------------------------------------------------------
moveRight :: Int -> W.KeyCode -> PlayerControls st (V2 Int)
moveRight dx k = ifPressedGo k (V2 dx 0)

--------------------------------------------------------------------------------
moveUp :: Int -> W.KeyCode -> PlayerControls st (V2 Int)
moveUp dy k = ifPressedGo k (V2 0 (-dy))

--------------------------------------------------------------------------------
moveDown :: Int -> W.KeyCode -> PlayerControls st (V2 Int)
moveDown dy k = ifPressedGo k (V2 0 (dy))

--------------------------------------------------------------------------------
playerKeyboard :: Int -> PlayerControls st (V2 Int)
playerKeyboard d =
  arrowControls d . pressedKeysWire [SFML.KeyLeft, SFML.KeyRight, SFML.KeyUp, SFML.KeyDown]

--------------------------------------------------------------------------------
seagullPlayerKeyboard :: Int
                      -> PlayerControls st (V2 Int)
seagullPlayerKeyboard delta = arrowControls_X delta . pressedKeysWire [SFML.KeyLeft, SFML.KeyRight]

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
stepTimed :: Monoid a => GameWire st a b
          -> (b -> GameMonad st c)
          -> GameMonad st (GameWire st a b)
stepTimed wire fn = do
  sess <- gets $ view gameSession
  (dt, _) <- stepSession sess
  (res, wire') <- stepWire wire dt (Right mempty)
  either (const $ return wire') (\v -> fn v >> return wire') res


--------------------------------------------------------------------------------
stepTimed' :: GameWire st NominalDiffTime b
          -> (b -> GameMonad st c)
          -> (() -> GameMonad st d)
          -> GameMonad st (GameWire st NominalDiffTime b)
stepTimed' wire ok ko = do
  sess <- gets $ view gameSession
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
