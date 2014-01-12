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


--------------------------------------------------------------------------------
showMenu :: GameMonad ()
showMenu = do
    (#>) (Entity 0 NoAlias
         (SMap.fromList [
                     (Renderable, sprite)
                   , (Texture, textureFrom "resources/menu.png")
                   , (BoundingBox, rect 0 0 640 480)
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
    let ctxSettings = Just $ W.ContextSettings 24 8 4 3 3
    wnd <- createRenderWindow
           (W.VideoMode windowWidth windowHeight 32)
           "Seagull Madness"
           [W.SFDefaultStyle]
           ctxSettings
    setFramerateLimit wnd 60
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
                 , (Texture, textureFrom "resources/beach.png")
                 , (BoundingBox, rect 0 0 640 480)
                 , (Position, position 0 0)
                 ]
               ))
    (#>) (Entity 0 NoAlias
               (SMap.fromList 
                 [ (Renderable, animation
                                "resources/enemies.png"
                                [G.IntRect 0 0 20 20
                                ,G.IntRect 20 20 40 50] (holdFor 1 . periodic 3))
                 , (Position, position 20 20)
                 ]
               ))
    (#>) (Entity 0 ThePlayer
               (SMap.fromList 
                 [ (Renderable, sprite)
                 , (Texture, textureFrom "resources/sprites.png")
                 , (BoundingBox, rect 34 1 32 32)
                 , (Position, position 330 440)
                 , (Keyboard, keyboard (seagullPlayerKeyboard 5))
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