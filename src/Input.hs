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
  rightPressed <- liftIO $ W.isMouseButtonPressed W.MouseRight
  when rightPressed $ do
    win <- gets $ view gameWin
    (S.Vec2i x y) <- liftIO $ W.getMousePosition (Just win)
    (#>) (Entity 0 NoAlias
               (SMap.fromList 
                 [ (Renderable, sprite)
                 , (Texture, textureFrom "resources/sprites.png")
                 , (BoundingBox, rect 1 1 32 32)
                 , (Position, position x y)
                 , (StaticBody, staticObj (H.Circle 16))
                 ]
               ))
    return ()
  when leftPressed $ do
    win <- gets $ view gameWin
    (S.Vec2i x y) <- liftIO $ W.getMousePosition (Just win)
    (#>) (Entity 0 NoAlias
               (SMap.fromList 
                 [ (Renderable, sprite)
                 , (Texture, textureFrom "resources/sprites.png")
                 , (BoundingBox, rect 1 1 32 32)
                 , (Position, position x y)
                 , (DynamicBody, dynamicObj (H.Circle 16))
                 ]
               ))
    return ()
