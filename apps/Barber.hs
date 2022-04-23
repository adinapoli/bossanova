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
      --runAndDealloc gameState showMenu
      flip evalStateT gameState $ do
        buildEntities
        gameLoop
        destroyPhysicManager


--------------------------------------------------------------------------------
runAndDealloc :: GameState -> GameMonad a -> SFML ()
runAndDealloc st action = liftIO $ runSFML $ evalStateT action st

gameWidth :: Int
gameWidth = 2048

gameHeight :: Int
gameHeight = 1024

--------------------------------------------------------------------------------
showMenu :: GameMonad ()
showMenu = do
    (#>) (Entity 0 NoAlias
         (SMap.fromList [
                     (Renderable, sprite)
                   , (Texture, textureFrom "resources/menu.png")
                   , (BoundingBox, rect 0 0 gameWidth gameHeight)
                   , (Position, position 0 0)
                   ]))
    (#>) (Entity 0 NoAlias
         (SMap.fromList [
                     (Renderable, text)
                   , (Size, intSize 20)
                   , (Caption, textCaption "Press Enter to play!")
                   , (Colour, colour white)
                   , (EventListener, onEvents [
                      GameCallback (toggleColour white gray (blinkWire 1 2))
                   ])
                   , (Position, position 200 430)
                   ]))
    showMenuLoop

  where
    showMenuLoop = do
        win <- gets $ view gameWin
        sess <- gets $ view gameTime
        sys <- gets $ view systems
        lift $ clearRenderWindow win white

        -- Update the world
        forM_ sys tick

        -- Update the game state
        (_, sess') <- stepSession sess
        gameTime .= sess'
        lift $ display win

        wantsToPlay <- lift $ pollEvent win
        case wantsToPlay of
         Just (W.SFEvtKeyPressed W.KeyReturn _ _ _ _) -> do
            popEntity
            popEntity
            return ()
         _ -> showMenuLoop


--------------------------------------------------------------------------------
initState :: Managers -> SFML GameState
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
    }


------------------------------------------------------------------------------
buildEntities :: GameMonad ()
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
    (#>) (Entity 0 NoAlias
               (SMap.fromList
                 [ (Renderable, animation
                                "resources/anims/snail.json"
                                1000
                   )
                 , (Position, position 70 430)
                 ]
               ))
    (#>) (Entity 0 NoAlias
               (SMap.fromList
                 [ (Renderable, animation
                                "resources/anims/sun.json"
                                60)
                 , (Position, position 500 (-100))
                 ]
               ))
    (#>) (Entity 0 ThePlayer
               (SMap.fromList
                 [(Renderable, animation
                                "resources/anims/player.json"
                                300
                   )
                 , (StaticBody, staticObj (H.Circle 30))
                 , (Position, position 0 (gameHeight - 200))
                 , (Keyboard, keyboard (seagullPlayerKeyboard 5))
                 ]
               ))
    return ()


enemy :: V2 Int -> Entity
enemy (V2 x y) = Entity 0 Enemy
  (SMap.fromList
    [ (Renderable, animation "resources/anims/blackBird.json" 800)
    , (Timer, discreteTimer 2000)
    , (EventListener, onEvents [spawnProjectile])
    , (Position, position x y)
    ]
  )


--------------------------------------------------------------------------------
gameLoop :: GameMonad ()
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
updateWorld :: [System] -> GameMonad ()
updateWorld = do
  --sequence_ . parMap rpar tick
  mapM_ tick


--------------------------------------------------------------------------------
updateGameState :: G.RenderWindow -> GameMonad ()
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
