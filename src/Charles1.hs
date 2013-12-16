{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude hiding ((.), id)

import Control.Wire
import Control.Lens hiding (at)
import FRP.Netwire.Move
import qualified SFML.Window as W
import qualified SFML.Graphics as G
import qualified SFML.System as S
import Control.Monad.SFML
import Control.Monad.SFML.Graphics
import Control.Monad.SFML.Window
import Control.Monad.SFML.System
import SFML.Graphics.Color
import Control.Monad
import Data.Map
import qualified Data.Map as Map
import GHC.Float
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)



type GameWire = Wire (Timed NominalDiffTime ()) () GameMonad

--------------------------------------------------------------------------------
type GameMonad = StateT GameState SFML

--------------------------------------------------------------------------------
newtype Component = Component { tick :: GameState -> GameMonad Component }

--------------------------------------------------------------------------------
data Entity = Entity {
    _rState :: G.RenderStates
  , _components :: [Component]
}

data GameState = GameState {
    _gameWin   :: G.RenderWindow
  , _gameTime  :: Session GameMonad (Timed NominalDiffTime ())
  , _entities  :: [Entity]
}

$(makeLenses ''GameState)
$(makeLenses ''Entity)
$(makeLensesFor [("transform", "l_transform")] ''G.RenderStates)



--------------------------------------------------------------------------------
spriteComponent :: G.Sprite
                -> GameWire NominalDiffTime b
                -> Component
spriteComponent spr wire = Component $ \GameState{..} -> do
                            sess <- gets $ view gameTime
                            (dt, sess') <- stepSession sess
                            (res, wire') <- stepWire wire dt (Right (dtime dt))
                            case res of
                              Right _ -> do
                                lift $ drawSprite _gameWin spr Nothing
                                return (spriteComponent spr wire')
                              Left  _ -> do
                                return $ spriteComponent spr wire'


--------------------------------------------------------------------------------
translateComponent :: G.Sprite -> GameWire NominalDiffTime NominalDiffTime
                   -> Component
translateComponent spr wire = Component $ \GameState{..} -> do
                            sess <- gets $ view gameTime
                            (dt, sess') <- stepSession sess
                            (res, wire') <- stepWire wire dt (Right (dtime dt))
                            case res of
                              Right dx -> do
                                lift $ move spr
                                      (S.Vec2f (cos $ fromIntegral . fromEnum $ dx)
                                               (cos $ fromIntegral . fromEnum $ dx))
                                return (translateComponent spr wire')
                              Left  _ -> return $ spriteComponent spr wire'


--------------------------------------------------------------------------------
-- MAIN STARTS HERE
main :: IO ()
main =  runSFML $ do
      initState  <- initGame
      evalStateT gameLoop initState


--------------------------------------------------------------------------------
initGame = do
    let ctxSettings = Just $ W.ContextSettings 24 8 0 1 2
    wnd <- createRenderWindow
           (W.VideoMode 640 480 32)
           "OCharles' Challenge 1"
           [W.SFDefaultStyle]
           ctxSettings
    setFramerateLimit wnd 30
    spr <- createSprite
    setTextureRect spr (G.IntRect 40 40 40 40)
    text <- textureFromFile "resources/wood.jpg" (Just $ G.IntRect 40 40 40 40)
    setTexture spr text True
    spr2 <- createSprite
    setTextureRect spr2 (G.IntRect 40 40 40 40)
    setTexture spr2 text True
    move spr2 (S.Vec2f 400 300)

    let e1 = (Entity G.renderStates [
            translateComponent spr challenge1
          , spriteComponent spr challenge1
          ])
        e2 = (Entity G.renderStates [
            spriteComponent spr2 blink
          ])
    return (GameState wnd clockSession_ [e1, e2])


gameLoop = do
  gameState <- get
  sess <- gets $ view gameTime
  wnd <- gets  $ view gameWin
  ent <- gets $ view entities
  lift $ clearRenderWindow wnd yellow

  -- Update the entities
  ticked <- mapM tickComponents ent

  -- Update the game state
  (dt, sess') <- stepSession sess
  liftIO $ print dt
  gameTime .= sess'
  entities .= ticked
  lift $ display wnd

  evt <- lift $ pollEvent wnd
  case evt of
    Just W.SFEvtClosed -> return ()
    _ -> gameLoop


tickComponents :: Entity -> GameMonad Entity
tickComponents e = do
  gameState <- get
  let comp = e ^. components
  tickedComponents <- mapM (flip tick gameState) comp
  return $ components .~ tickedComponents $ e


--------------------------------------------------------------------------------
challenge1 = for 10 . integral 0 . pure 20    -->
             for 10 . integral 0 . pure (-20) -->
             challenge1

-- I'm trying to create a wire which produce and inhibits periodically.
-- But I'm failing.
blink = after 4 . (for 5 --> blink)
