{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude hiding ((.), id)

import Control.Wire
import Control.Lens
import FRP.Netwire.Move
import qualified SFML.Window as W
import qualified SFML.Graphics as G
import qualified SFML.System as S
import Control.Monad.SFML
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
                -> GameWire NominalDiffTime NominalDiffTime
                -> Component
spriteComponent spr wire = Component $ \GameState{..} -> do
                            sess <- gets $ view gameTime
                            (dt, sess') <- stepSession sess
                            (res, wire') <- stepWire wire dt (Right (dtime dt))
                            case res of
                              Right _ -> do
                                lift $ drawSprite _gameWin spr Nothing
                                return (spriteComponent spr wire')
                              Left  _ -> return $ spriteComponent spr wire



--------------------------------------------------------------------------------
-- MAIN STARTS HERE
main :: IO ()
main =  runSFML $ do
      initState  <- initGame
      flip evalStateT initState gameLoop


--------------------------------------------------------------------------------
initGame = do
    let ctxSettings = Just $ W.ContextSettings 24 8 0 1 2
    wnd <- createRenderWindow
           (W.VideoMode 640 480 32)
           "OCharles' Challenge 1"
           [W.SFDefaultStyle]
           ctxSettings
    spr <- createSprite
    setTextureRect spr (G.IntRect 40 40 40 40)
    text <- textureFromFile "resources/wood.jpg" (Just $ G.IntRect 40 40 40 40)
    setTexture spr text True
    spr2 <- createSprite
    setTextureRect spr (G.IntRect 40 40 40 40)
    setTexture spr2 text True
    move spr2 (S.Vec2f 400 300)

    let e1 = (Entity G.renderStates [spriteComponent spr challenge1])
        e2 = (Entity G.renderStates [spriteComponent spr2 periodicW])
    return (GameState wnd (countSession_ 1) [e1, e2])


gameLoop = do
  gameState <- get
  sess <- gets $ view gameTime
  wnd <- gets  $ view gameWin
  ent <- gets $ view entities
  lift $ clearRenderWindow wnd blue
  (dt, sess') <- stepSession sess
  --let dx = (truncate res :: Int) `mod` 500
  --let dx2 = (truncate res2 :: Int) `mod` 500
  --let e1' = rState . l_transform .~ G.translation (fromIntegral dx) 40 $ e1
  --let e2' = rState . l_transform .~ G.translation 200 (fromIntegral dx2) $ e2
  --lift $ drawSprite wnd (e1 ^. graphics) $ Just (e1' ^. rState)
  --lift $ drawSprite wnd (e2 ^. graphics) $ Just (e2' ^. rState)
  evt <- lift $ pollEvent wnd

  ticked <- mapM tickComponents ent
  gameTime .= sess'
  entities .= ticked
  lift $ display wnd

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
challenge1 = for 600 . time 
-- --> for 500 . (-1) * time --> challenge1

periodicW = (after 500 . (-1) * time --> for 500 .  time --> periodicW)
            <|> pure 30
