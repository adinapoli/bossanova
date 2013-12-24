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
import Control.Monad hiding (when)
import qualified Data.IntMap.Strict as Map
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


--------------------------------------------------------------------------------
type EntityManager = Map.IntMap Entity


--------------------------------------------------------------------------------
data GameState = GameState {
    _gameWin    :: G.RenderWindow
  , _gameTime   :: Session GameMonad (Timed NominalDiffTime ())
  , _entityNum  :: Int
  , _entityMgr  :: EntityManager
}


--------------------------------------------------------------------------------
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
                              Left  _ -> return $ spriteComponent spr wire'


--------------------------------------------------------------------------------
-- | Add an entity.
(#>) :: Entity -> GameMonad ()
(#>) e = do
  currentId <- gets $ view entityNum
  eMgr <- gets $ view entityMgr
  entityMgr .= Map.insert currentId e eMgr
  entityNum += 1


--------------------------------------------------------------------------------
-- | Builds an entity manager from a list of entities.
fromList :: [Entity] -> EntityManager
fromList ls = Map.fromList (zip [1..] ls)


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
                              Left  _ -> return $ translateComponent spr wire'

--------------------------------------------------------------------------------
moveComponent :: G.Sprite
              -> GameWire NominalDiffTime (Int, Int)
              -> Component
moveComponent spr wire = Component $ \GameState{..} -> do
                            sess <- gets $ view gameTime
                            (dt, sess') <- stepSession sess
                            (res, wire') <- stepWire wire dt (Right (dtime dt))
                            case res of
                              Right (dx, dy) -> do
                                lift $ move spr
                                      (S.Vec2f (fromIntegral dx)
                                               (fromIntegral dy))
                                return (moveComponent spr wire')
                              Left  _ -> return $ moveComponent spr wire'

--------------------------------------------------------------------------------
-- MAIN STARTS HERE
main :: IO ()
main =  runSFML $ do
      initState  <- initGame
      evalStateT gameLoop initState


--------------------------------------------------------------------------------
initGame = do
    let ctxSettings = Just $ W.ContextSettings 24 8 4 3 3
    wnd <- createRenderWindow
           (W.VideoMode 640 480 32)
           "Stupid Game"
           [W.SFDefaultStyle]
           ctxSettings
    setFramerateLimit wnd 60
    spr <- createSprite
    setTextureRect spr (G.IntRect 40 40 40 40)
    text <- textureFromFile "resources/wood.jpg" (Just $ G.IntRect 40 40 40 40)
    setTexture spr text True
    spr2 <- createSprite
    setTexture spr2 text True
    move spr2 (S.Vec2f 400 300)
    spr3 <- createSprite
    setTexture spr3 text True
    move spr3 (S.Vec2f 20 300)

    let e1 = (Entity G.renderStates [
            translateComponent spr challenge1
          , spriteComponent spr challenge1
          ])
        e2 = (Entity G.renderStates [
            spriteComponent spr2 blink
          ])
        player = (Entity G.renderStates [
                 spriteComponent spr3 always
               , moveComponent spr3 playerKeyboard
          ])
        initialEntities = [e1, e2, player]
    return (GameState wnd
                      clockSession_
                      (length initialEntities)
                      (fromList initialEntities))



--------------------------------------------------------------------------------
gameLoop :: GameMonad ()
gameLoop = do
  gameState <- get
  sess <- gets $ view gameTime
  wnd  <- gets $ view gameWin
  eMgr <- gets $ view entityMgr
  lift $ clearRenderWindow wnd yellow

  -- Update the logic
  mapM_ tickComponents (Map.keys eMgr)

  -- Update the graphics
  -- TODO

  -- Update the game state
  (dt, sess') <- stepSession sess
  liftIO $ print dt
  gameTime .= sess'
  lift $ display wnd

  evt <- lift $ pollEvent wnd
  case evt of
    Just W.SFEvtClosed -> return ()
    _ -> gameLoop


--------------------------------------------------------------------------------
tickComponents :: Map.Key -> GameMonad ()
tickComponents idx = do
  gameState <- get
  eMgr <- gets $ view entityMgr
  let e = eMgr Map.! idx
  let comp = e ^. components
  tickedComponents <- mapM (`tick` gameState) comp
  entityMgr .= Map.insert idx (components .~ tickedComponents $ e) eMgr


--------------------------------------------------------------------------------
challenge1 = for 10 . integral 0 . pure 20    -->
             for 10 . integral 0 . pure (-20) -->
             challenge1


--------------------------------------------------------------------------------
-- I'm trying to create a wire which produce and inhibits periodically.
-- But I'm failing.
blink = after 4 . (for 5 --> blink)


ifPressedGo :: W.KeyCode -> (Int,Int) -> GameWire NominalDiffTime (Int, Int)
ifPressedGo code coords = mkGen_ $ \_ -> do
  keyPressed <- lift $ isKeyPressed code
  if keyPressed
     then return . Right $ coords
     else return . Left $ ()

--------------------------------------------------------------------------------
moveLeft :: GameWire NominalDiffTime (Int, Int)
moveLeft = ifPressedGo W.KeyA (-5, 0)


--------------------------------------------------------------------------------
moveRight :: GameWire NominalDiffTime (Int, Int)
moveRight = ifPressedGo W.KeyD (5, 0)

--------------------------------------------------------------------------------
moveUp :: GameWire NominalDiffTime (Int, Int)
moveUp = ifPressedGo W.KeyW (0, -5)

--------------------------------------------------------------------------------
moveDown :: GameWire NominalDiffTime (Int, Int)
moveDown = ifPressedGo W.KeyS (0, 5)

--------------------------------------------------------------------------------
stand :: GameWire NominalDiffTime (Int, Int)
stand = pure (0,0)


--------------------------------------------------------------------------------
playerKeyboard :: GameWire NominalDiffTime (Int, Int)
playerKeyboard = moveLeft <|>
                 moveRight <|>
                 moveUp <|> 
                 moveDown <|>
                 stand




--------------------------------------------------------------------------------
always = pure True
