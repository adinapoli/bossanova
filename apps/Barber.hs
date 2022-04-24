{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Prelude hiding ((.), id)

import Control.Wire
import Control.Parallel.Strategies
import Linear.V2
import System.Random
import Control.Lens hiding (at)
import Control.Concurrent.STM
import qualified SFML.Window as W
import qualified SFML.Graphics as G
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

data PlayerState =
    PS_Idle
  | PS_Running
  | PS_Attack1
  deriving (Show, Eq)

data BarberGameState = BarberGameState
  { _gsPlayerState :: PlayerState
  } deriving (Show, Eq)

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
      , _gameTime   = clockSession_
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
      , _gameState = BarberGameState PS_Idle
    }


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
    (#>) (enemy (V2 20 20))
    (#>) (enemy (V2 200 20))
    (#>) (enemy (V2 400 20))
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


enemy :: V2 Int -> Entity BarberGameState
enemy (V2 x y) = Entity 0 Enemy
  (SMap.fromList
    [ (Renderable, animation "resources/anims/blackBird.json" 800)
    , (Timer, discreteTimer 2000)
    , (EventListener, onEvents [spawnProjectile])
    , (Position, position x y)
    ]
  )

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
  gameState <- get
  sess <- gets $ view gameTime
  tm   <- gets $ view timeWire
  (dt, sess') <- stepSession sess
  (_, wire') <- stepWire tm dt (Right dt)
  gameTime .= sess'
  timeWire .= wire'
  randGen .= gameState ^. randGen . to (snd . next)
  lift $ display wnd

makeLenses ''BarberGameState
