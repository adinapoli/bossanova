{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}

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


--------------------------------------------------------------------------------
-- MAIN STARTS HERE
-- | not worring about resource alloc/dealloc. Atm everything is retained in
-- memory.
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
      runAndDealloc gameState showMenu
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
           (W.VideoMode windowWidth windowHeight 32)
           "The Lost Lens"
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
        ]
    }


------------------------------------------------------------------------------
buildEntities :: GameMonad ()
buildEntities = do
    -- screen bounds
    (#>) (Entity 0 NoAlias
               (SMap.fromList
                 [(Position, position 0 440)
                 ,(StaticBody, staticObj
                    (H.LineSegment (H.Vector 0 440) (H.Vector (fromIntegral gameWidth) (fromIntegral gameHeight)) 1.0))
                 ]))

    -- spawner of sprites
    (#>) (Entity 0 NoAlias
         (SMap.fromList [(Callback, mouseCallback spawnRigidBody)
                   ]))

    -- counters
    (#>) (Entity 0 BodyCounter
         (SMap.fromList [
                     (Renderable, text)
                   , (Size, intSize 20)
                   , (Colour, colour red)
                   , (Caption, textCaption "")
                   , (EventListener, onEvents [
                       GameCallback displayPhysicsBodyCount
                   ])
                   , (Position, position 10 40)
                   ]))

    (#>) (Entity 0 SpriteCounter
         (SMap.fromList [
                     (Renderable, text)
                   , (Size, intSize 20)
                   , (Colour, colour red)
                   , (Caption, textCaption "Sprites: ")
                   , (EventListener, onEvents [
                       GameCallback displaySpritesCount
                   ])
                   , (Position, position 10 60)
                   ]))

    (#>) (Entity 0 FPSCounter
         (SMap.fromList [
                     (Renderable, text)
                   , (Size, intSize 20)
                   , (Colour, colour red)
                   , (Caption, textCaption "FPS: ")
                   , (EventListener, onEvents [
                       GameCallback updateAndDisplayFPS
                   ])
                   , (Position, position 10 10)
                   ]))

    (#>) (Entity 0 NoAlias
               (SMap.fromList
                 [(Renderable, sprite)
                 ,(Texture, textureFrom "resources/sprites.png")
                 ,(BoundingBox, rect 1 1 32 32)
                 ,(Position, position 100 100)
                 ,(LinearForce, linearForce (V2 0 1))
                 ]))

    (#>) (Entity 0 Special
               (SMap.fromList
                 [ (Renderable, sprite)
                 , (Texture, textureFrom "resources/sprites.png")
                 , (BoundingBox, rect 1 1 32 32)
                 , (Position, position 100 0)
                 , (StaticBody, staticObj (H.Circle 16))
                 ]))

    (#>) (Entity 0 ThePlayer
               (SMap.fromList 
                 [ (Renderable, sprite)
                 , (Texture, textureFrom "resources/sprites.png")
                 , (BoundingBox, rect 34 1 32 32)
                 , (Position, position 20 300)
                 , (Keyboard, keyboard (playerKeyboard 5))
                 ]
               ))

    (#>) (Entity 0 PointCounter
               (SMap.fromList
                 [ (Renderable, text)
                 , (Size, intSize 20)
                 , (Colour, colour red)
                 , (Caption, textCaption "Move the player to update")
                 , (EventListener, onEvents [
                     GameCallback (updateCaption (playerKeyboard 5))
                   , GameCallback (toggleColour red green (blinkWire 1 2))
                 ])
                 , (Position, position 10 80)
                 ]
               ))
    return ()


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
