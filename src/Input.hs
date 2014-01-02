module Input where

import Types
import qualified Data.Map.Strict as SMap
import Entities
import Components
import Control.Monad.SFML
import Control.Monad.SFML.Graphics
import qualified SFML.Window as W
import qualified SFML.Graphics as G
import qualified SFML.System as S
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad
import Control.Lens
import Linear.V2
import qualified Physics.Hipmunk as H



--------------------------------------------------------------------------------
spawnRigidBody :: GameMonad ()
spawnRigidBody = do
  leftPressed <- liftIO $ W.isMouseButtonPressed W.MouseLeft
  when leftPressed $ do
    win <- gets $ view gameWin
    spr <- lift createSprite
    tex <- lift $ textureFromFile "resources/sprites.png" Nothing
    lift $ setTexture spr tex True
    lift $ setTextureRect spr (G.IntRect 1 1 32 32)
    (S.Vec2i x y) <- liftIO $ W.getMousePosition (Just win)
    (#>) (Entity 0 NoAlias
               (SMap.fromList 
                 [ (Renderable, sprite spr)
                 , (Position, position x y)
                 , (CollisionShape, physicalObj (H.Circle 32))
                 ]
               ))
    return ()
