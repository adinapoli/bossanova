{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}

import Prelude hiding ((.), id)

import Control.Wire
import System.Random
import Control.Lens hiding (at)
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
import Data.Word
import Data.Time.Clock.POSIX (getPOSIXTime)


--------------------------------------------------------------------------------
import Types
import Entities
import Components
import Wires
import Systems
import Events


--------------------------------------------------------------------------------
-- MAIN STARTS HERE
-- | not worring about resource alloc/dealloc. Atm everything is retained in
-- memory.
main :: IO ()
main = runSFML $ do
      gameState  <- initState
      runAndDealloc gameState showMenu
      flip evalStateT gameState $ do
        buildEntities
        gameLoop


--------------------------------------------------------------------------------
runAndDealloc :: GameState -> GameMonad a -> SFML ()
runAndDealloc st action = liftIO $ runSFML $ evalStateT action st


--------------------------------------------------------------------------------
showMenu :: GameMonad ()
showMenu = do
    (m, p) <- lift $ do
      playTxt <- createText
      fnt <- fontFromFile "resources/ProFont.ttf"
      setTextFont playTxt fnt
      setTextString playTxt "Press Enter to play!"
      menuTex <- textureFromFile
                 "resources/menu.png"
                 (Just $ G.IntRect 0 0 640 480)
      menuSpr <- createSprite
      setTexture menuSpr menuTex True
      return (menuSpr, playTxt)
    (#>) (Entity 0 NoAlias
         (SMap.fromList [
                     (Renderable, sprite m)
                   , (Position, position 0 0)
                   ]))
    (#>) (Entity 0 NoAlias
         (SMap.fromList [
                     (Renderable, text p)
                   , (Size, intSize 20)
                   , (Colour, colour white)
                   , (EventListener, onEvents [
                      GameEvent (toggleColour white gray (blinkWire 1 2))
                   ])
                   , (Position, position 200 430)
                   ]))
    showMenuLoop

  where
    showMenuLoop = do
        win <- gets $ view gameWin
        sess <- gets $ view gameTime
        eMgr <- gets $ view entityMgr
        sys <- gets $ view systems
        lift $ clearRenderWindow win white

        -- Update the world
        forM_ sys tick

        -- Update the game state
        (dt, sess') <- stepSession sess
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
initState :: SFML GameState
initState = do
    g <- liftIO getStdGen
    let ctxSettings = Just $ W.ContextSettings 24 8 4 3 3
    wnd <- createRenderWindow
           (W.VideoMode 640 480 32)
           "The Lost Lens"
           [W.SFDefaultStyle]
           ctxSettings
    setFramerateLimit wnd 60
    return GameState {
        _gameWin    = wnd
      , _gameTime   = clockSession_
      , _frameTime  = 0
      , _fps        = 0
      , _entityMgr  = Map.empty
      , _randGen    = g
      , _systems    = [
          inputSystem
        , textSizeSystem
        , textColourSystem
        , textCaptionSystem
        , rendererSystem
        , eventSystem
        ]
    }


------------------------------------------------------------------------------
milliTime :: IO Word64
milliTime = do
    seconds <- realToFrac `fmap` getPOSIXTime :: IO Double
    return $ round $ seconds * 1e3


------------------------------------------------------------------------------
buildEntities :: GameMonad ()
buildEntities = do
    entityMgr .= Map.empty
    spr <- lift createSprite
    tex <- lift $ textureFromFile "resources/sprites.png" Nothing
    lift $ setTexture spr tex True
    lift $ setTextureRect spr (G.IntRect 1 1 32 32)
    spr2 <- lift createSprite
    lift $ setTexture spr2 tex True
    lift $ setTextureRect spr2 (G.IntRect 1 1 32 32)
    spr3 <- lift createSprite
    lift $ setTexture spr3 tex True
    lift $ setTextureRect spr3 (G.IntRect 34 1 32 32)
    t <- lift createText
    fnt <- lift $ fontFromFile "resources/ProFont.ttf"
    lift $ setTextFont t fnt
    (#>) (Entity 0 NoAlias
               (SMap.fromList
                 [(Renderable, sprite spr)
                 ,(Position, position 100 100)
                 ]))
    (#>) (Entity 0 NoAlias
               (SMap.fromList
                 [(Renderable, sprite spr2)
                 ,(Position, position 400 300)
                 ,(AffectRendering, blink 1 2)
                 ]))
    (#>) (Entity 0 ThePlayer
               (SMap.fromList 
                 [ (Renderable, sprite spr3)
                 , (Position, position 20 300)
                 , (Keyboard, keyboard playerKeyboard)
                 ]
               ))
    (#>) (Entity 0 PointCounter
         (SMap.fromList [
                     (Renderable, text t)
                   , (Size, intSize 20)
                   , (Colour, colour red)
                   , (Caption, textCaption "Move the player to update")
                   , (EventListener, onEvents [
                       GameEvent (updateCaption playerKeyboard)
                     , GameEvent (toggleColour red green (blinkWire 1 2))
                   ])
                   , (Position, position 400 40)
                   ]))
    return ()


--------------------------------------------------------------------------------
gameLoop :: GameMonad ()
gameLoop = do
  gameState <- get
  sess <- gets $ view gameTime
  wnd  <- gets $ view gameWin
  sys  <- gets $ view systems
  lift $ clearRenderWindow wnd yellow

  -- Update the world
  forM_ sys tick

  -- Update the game state
  (_, sess') <- stepSession sess
  gameTime .= sess'
  lift $ display wnd

  -- Update the stdGen
  randGen .= gameState ^. randGen . to (snd . next)

  updateAndDisplayFPS

  evt <- lift $ pollEvent wnd
  case evt of
    Just W.SFEvtClosed -> return ()
    _ -> gameLoop


--------------------------------------------------------------------------------
updateAndDisplayFPS :: GameMonad ()
updateAndDisplayFPS = do
  fTime <- gets $ view frameTime
  fps' <- gets $ view fps
  curTime <- liftIO milliTime
  let dt = curTime - fTime
  if dt >= 1000
    then do
      frameTime .= curTime
      fps .= 0
      liftIO $ putStrLn $ "FPS: " ++ show (fps' + 1)
    else fps += 1
