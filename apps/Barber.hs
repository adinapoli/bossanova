{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}

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


data PlayerState =
    PS_Idle
  | PS_Running
  | PS_Attack1
  deriving (Show, Eq)

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
      , _gameSession = clockSession <&> (\f -> StateDelta $ f (Last (Just initialGameState)))
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
                 [(Renderable, animation
                                "resources/anims/barber/biker_idle.json"
                                2000
                   )
                 , (StaticBody, staticObj (H.Circle 30))
                 , (Position, position 0 (gameHeight - 400))
                 , (Keyboard, keyboard (barberCombatPlayerKeyboard 5))
                 ]
               ))
    return ()

barberCombatPlayerKeyboard :: Int -> PlayerControls BarberGameState (V2 Int)
barberCombatPlayerKeyboard delta = do
  asum [
      ifPressed W.KeyRight (over gsPlayerState (const PS_Running), V2 delta 0)
    , ifPressed W.KeyLeft (over gsPlayerState (const PS_Running), V2 (-delta) 0)
    , ifPressed W.KeySpace (over gsPlayerState (const PS_Attack1), V2 0 0)
    , mkGen_ $ \_ -> pure $ Right (over gsPlayerState (const PS_Idle), V2 0 0)
    ] . pressedKeysWire [SFML.KeyRight, SFML.KeyLeft, SFML.KeySpace]

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
  gameSession .= sess'
  timeWire .= wire' . mkSF_ (toStateDelta gameLogicState)
  randGen .= gs ^. randGen . to (snd . next)
  lift $ display wnd
  where
    toStateDelta :: BarberGameState -> Timed NominalDiffTime () -> StateDelta BarberGameState
    toStateDelta st tmd = StateDelta (tmd <&> const (Last $ Just st))

    fromStateDelta :: StateDelta BarberGameState -> Timed NominalDiffTime ()
    fromStateDelta (StateDelta tmd) = fmap (const ()) tmd
