{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude hiding ((.), id)

import Control.Wire
import Control.Lens hiding (at)
import qualified SFML.Window as W
import qualified SFML.Graphics as G
import qualified SFML.System as S
import Control.Monad.SFML
import Control.Monad.SFML.Graphics
import Control.Monad.SFML.Window
import Control.Monad.SFML.System
import SFML.Graphics.Color
import Control.Monad hiding (when)
import qualified Control.Monad as M
import qualified Data.IntMap.Strict as Map
import GHC.Float
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Data.Word
import Data.Time.Clock.POSIX (getPOSIXTime)


--------------------------------------------------------------------------------
import Types
import Entities
import Components
import Wires


--------------------------------------------------------------------------------
-- MAIN STARTS HERE
-- | not worring about resource alloc/dealloc. Atm everything is retained in
-- memory.
main :: IO ()
main =  runSFML $ do
      gameState  <- initState
      flip evalStateT gameState $ do
        showMenu
        buildEntities
        gameLoop


--------------------------------------------------------------------------------
showMenu :: GameMonad ()
showMenu = do
    win <- gets $ view gameWin
    (m, p) <- lift $ do
      playTxt <- createText
      fnt <- fontFromFile "resources/ProFont.ttf"
      setTextFont playTxt fnt
      setTextString playTxt "Press Enter to play!"
      setTextCharacterSize playTxt 20
      setTextColor playTxt white
      setPosition playTxt (S.Vec2f 200 430)
      menuTex <- textureFromFile
                 "resources/menu.png"
                 (Just $ G.IntRect 0 0 640 480)
      menuSpr <- createSprite
      setTexture menuSpr menuTex True
      return (menuSpr, playTxt)
    (#>) (Entity [] [spriteComponent m always])
    (#>) (Entity [] [textSizeComponent p glowingText])
    showMenuLoop

  where
    showMenuLoop = do
        win <- gets $ view gameWin
        sess <- gets $ view gameTime
        eMgr <- gets $ view entityMgr
        lift $ clearRenderWindow win white

        -- Update the world
        mapM_ updateComponents (Map.keys eMgr)

        -- Update the game state
        (dt, sess') <- stepSession sess
        gameTime .= sess'
        lift $ display win

        wantsToPlay <- lift $ pollEvent win
        case wantsToPlay of
         Just (W.SFEvtKeyPressed W.KeyReturn _ _ _ _) -> do
            popEntity 
            return ()
         _ -> showMenuLoop


--------------------------------------------------------------------------------
initState = do
    let ctxSettings = Just $ W.ContextSettings 24 8 4 3 3
    wnd <- createRenderWindow
           (W.VideoMode 640 480 32)
           "The Lost Lens"
           [W.SFDefaultStyle]
           ctxSettings
    setFramerateLimit wnd 60
    return (GameState wnd clockSession_ 0 0 Map.empty)


------------------------------------------------------------------------------
milliTime :: IO Word64
milliTime = do
    seconds <- realToFrac `fmap` getPOSIXTime :: IO Double
    return $ round $ seconds * 1e3


------------------------------------------------------------------------------
buildEntities :: GameMonad ()
buildEntities = do
    ent <- lift $ do
      spr <- createSprite
      text <- textureFromFile "resources/sprites.png" Nothing
      setTexture spr text True
      setTextureRect spr (G.IntRect 1 1 32 32)
      spr2 <- createSprite
      setTexture spr2 text True
      setTextureRect spr2 (G.IntRect 1 1 32 32)
      move spr2 (S.Vec2f 400 300)
      spr3 <- createSprite
      setTexture spr3 text True
      setTextureRect spr3 (G.IntRect 34 1 32 32)
      move spr3 (S.Vec2f 20 300)
      let e1 = (Entity
               [ translateComponent spr challenge1]
               [ spriteComponent spr always]
               )
          e2 = (Entity
               []
               [ spriteComponent spr2 blink]
               )
          player = (Entity
                    [ moveComponent spr3 playerKeyboard]
                    [ spriteComponent spr3 always ]
                   )
       in return [e1, e2, player]
    entityMgr .= fromList ent



--------------------------------------------------------------------------------
gameLoop :: GameMonad ()
gameLoop = do
  gameState <- get
  sess <- gets $ view gameTime
  wnd  <- gets $ view gameWin
  eMgr <- gets $ view entityMgr
  lift $ clearRenderWindow wnd yellow

  -- Update the world
  mapM_ updateComponents (Map.keys eMgr)

  -- Update the game state
  (dt, sess') <- stepSession sess
  gameTime .= sess'
  lift $ display wnd

  updateAndDisplayFPS

  evt <- lift $ pollEvent wnd
  case evt of
    Just W.SFEvtClosed -> return ()
    _ -> gameLoop


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
    else do
      fps += 1
