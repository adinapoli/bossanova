{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Prelude hiding ((.), id)

import Control.Wire hiding (Last)
import Control.Parallel.Strategies
import Linear.V2
import System.Random
import Control.Lens hiding (at)
import Control.Concurrent.STM
import qualified SFML.Window as W
import qualified SFML.Graphics as G
import Control.Lens
import Control.Monad.SFML
import Control.Monad.SFML.Graphics
import SFML.Graphics.Color
import Control.Monad hiding (when)
import qualified Data.IntMap.Strict as Map
import qualified Data.Map.Strict as SMap
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import qualified Physics.Hipmunk as H


--------------------------------------------------------------------------------
import Types
import Input
import Entities
import Components
import Wires
import Systems
import Events
import Settings
import Physics
import Animation
import Data.Foldable
import Debug.Trace
import qualified SFML.Window.Keyboard as SFML
import Data.Monoid
import Data.Map.Strict (Map)
import Data.Maybe


data PlayerState =
    PS_Idle
  | PS_Running
  | PS_Attack1
  deriving (Show, Eq, Ord)

data BarberGameState = BarberGameState
  { _gsPlayerState :: PlayerState
  } deriving (Show, Eq)

makeLenses ''BarberGameState

--------------------------------------------------------------------------------
main :: IO ()
main = runSFML $ do
      fMgr <- liftIO createPhysicsManager
      sQueue <- liftIO newTQueueIO
      let manag = Managers {
        _entityMgr    = EntityManager 0 Map.empty
        , _physicsMgr = fMgr
        , _artMgr     = ArtManager SMap.empty 0 sQueue
      }
      gameState  <- initState manag
      flip evalStateT gameState $ do
        buildEntities
        gameLoop
        destroyPhysicManager


--------------------------------------------------------------------------------
runAndDealloc :: GameState BarberGameState -> GameMonad BarberGameState a -> SFML ()
runAndDealloc st action = liftIO $ runSFML $ evalStateT action st

gameWidth :: Int
gameWidth = 2048

gameHeight :: Int
gameHeight = 1024

--------------------------------------------------------------------------------
initState :: Managers BarberGameState -> SFML (GameState BarberGameState)
initState mgrs = do
    g <- liftIO getStdGen
    let ctxSettings = Just $ W.ContextSettings 24 8 4 2 1 []
    wnd <- createRenderWindow
           (W.VideoMode gameWidth gameHeight 32)
           "Barber Combat"
           [W.SFDefaultStyle]
           ctxSettings
    setFramerateLimit wnd 60
    setWindowSize wnd (W.Vec2u (fromIntegral gameWidth) (fromIntegral gameHeight))
    return GameState {
        _gameWin    = wnd
      , _gameSession = clockSession <&> (\f -> StateDelta $ f (Last (Just (traceShow "restored!" initialGameState))))
      , _timeWire   = timeF
      , _frameTime  = 0
      , _fps        = 0
      , _randGen    = g
      , _managers   = mgrs
      , _systems    = [
          inputSystem
        , textureInitSystem
        , textureAttacherSystem
        , spriteInitSystem
        , textSizeSystem
        , textColourSystem
        , textCaptionSystem
        , textInitSystem
        , rendererSystem
        , eventSystem
        , newtonianSystem
        , hipmunkSystem
        , deallocatorSystem
        , animationSystem
        ]
      , _gameState = initialGameState
    }
  where
    initialGameState = BarberGameState PS_Idle

------------------------------------------------------------------------------
buildEntities :: GameMonad BarberGameState ()
buildEntities = do
    (#>) (Entity 0 NoAlias
               (SMap.fromList
                 [ (Renderable, sprite)
                 , (Texture, textureFrom "resources/barber/City1_Nino.png")
                 , (BoundingBox, rect 0 0 gameWidth gameHeight)
                 , (Position, position 0 0)
                 ]
               ))
    (#>) (Entity 0 ThePlayer
               (SMap.fromList
                 [(Renderable, animationWire $ playerAnimationWire [
                     (PS_Idle, ("resources/anims/barber/biker_idle.json", 2000))
                  ,  (PS_Running, ("resources/anims/barber/biker_run.json", 500))
                  ])
                 , (StaticBody, staticObj (H.Circle 30))
                 , (Position, position 0 (gameHeight - 400))
                 , (Keyboard, keyboard (barberCombatPlayerKeyboard 5))
                 ]
               ))
    return ()

barberCombatPlayerKeyboard :: Int -> PlayerControls BarberGameState (V2 Int)
barberCombatPlayerKeyboard delta =
  let playerMoves =
        asum [
            ifPressed W.KeyRight (over gsPlayerState (const PS_Running), V2 delta 0)
          , ifPressed W.KeyLeft (over gsPlayerState (const PS_Running), V2 (-delta) 0)
          , ifPressed W.KeySpace (over gsPlayerState (const PS_Attack1), V2 0 0)
          , playerIdle
          ] . pressedKeysWire [SFML.KeyRight, SFML.KeyLeft, SFML.KeySpace]
  in playerMoves
  where
    playerIdle :: PlayerControls BarberGameState (V2 Int)
    playerIdle = mkGen_ $ \pressed -> case pressed of
      [] -> pure $ Right (over gsPlayerState (const PS_Idle), V2 0 0)
      xs -> pure $ Left ()

animationWire :: GameWire BarberGameState (Maybe Animation) Animation -> Component BarberGameState
animationWire = Component Renderable . CAnimation . WireAnimation Nothing

playerAnimationWire :: [(PlayerState, (FilePath, Double))]
                    -> GameWire BarberGameState (Maybe Animation) Animation
playerAnimationWire animationBundle =
  let w1 = mkGen $ \s initialisedAnims -> case fromStateDelta s of
            (Last Nothing) -> pure (Left (), w1)
            (Last (Just st)) -> case SMap.lookup (_gsPlayerState st) initialisedAnims of
                Nothing -> error $ "Player state " <> show st <> " doesn't have correspoding animation."
                Just a  -> pure (Right a, w1)
      in w1 . initAnims
  where

    fromStateDelta :: StateDelta s -> Last s
    fromStateDelta (StateDelta tmd) = case tmd of Timed _ l -> l

    initAnims :: GameWire BarberGameState (Maybe Animation) (Map PlayerState Animation)
    initAnims = mkGen_ $ \_ -> do
      anims <- forM animationBundle $ \(ps, (fp, d)) -> (ps,) <$> mkAnimation fp d
      pure $ Right (SMap.fromList anims)

--------------------------------------------------------------------------------
gameLoop :: GameMonad BarberGameState ()
gameLoop = do
  wnd  <- gets $ view gameWin
  sys  <- gets $ view systems
  lift $ clearRenderWindow wnd yellow
  updateWorld sys
  updateGameState wnd

  evt <- lift $ pollEvent wnd
  case evt of
    Just W.SFEvtClosed -> return ()
    _ -> gameLoop


--------------------------------------------------------------------------------
updateWorld :: [System BarberGameState] -> GameMonad BarberGameState ()
updateWorld = do
  --sequence_ . parMap rpar tick
  mapM_ tick


--------------------------------------------------------------------------------
updateGameState :: G.RenderWindow -> GameMonad BarberGameState ()
updateGameState wnd = do
  gs <- get
  gameLogicState <- gets $ view gameState
  sess <- gets $ view gameSession
  tm   <- gets $ view timeWire
  (dt, sess') <- stepSession sess
  (_, wire') <- stepWire (tm . mkSF_ fromStateDelta) dt (Right dt)
  gameSession .= (sess' <&> \(StateDelta tmd) -> toStateDelta gameLogicState tmd)
  timeWire .= wire' . mkSF_ (toStateDelta' gameLogicState)
  randGen .= gs ^. randGen . to (snd . next)
  lift $ display wnd
  where
    toStateDelta :: BarberGameState
                 -> Timed NominalDiffTime (Last BarberGameState)
                 -> StateDelta BarberGameState
    toStateDelta st tmd = StateDelta (tmd <&> modifyIfChanged st)

    toStateDelta' :: BarberGameState -> Timed NominalDiffTime a -> StateDelta BarberGameState
    toStateDelta' st tmd = StateDelta (tmd <&> const (Last (Just st)))

    fromStateDelta :: StateDelta BarberGameState -> Timed NominalDiffTime ()
    fromStateDelta (StateDelta tmd) = fmap (const ()) tmd

modifyIfChanged :: (Show a, Eq a) => a -> Last a -> Last a
modifyIfChanged new (Last Nothing) = Last (Just new)
modifyIfChanged new (Last (Just old))
  = if new == old then Last Nothing else Last $ Just new
