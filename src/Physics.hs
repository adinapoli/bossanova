module Physics where

import Physics.Hipmunk
import Types
import Control.Monad.SFML
import Control.Monad.Trans.State
import Control.Lens
import Data.Default
import Linear.V2
import Data.StateVar


instance Default PhysicsConfig where
  def = PhysicsConfig {
      _defGravity       = V2 0.0 0.05
    , _defMass          = 1.0
    , _defMoment        = momentForShape
    }


--------------------------------------------------------------------------------
createPhysicsManager :: IO PhysicsManager
createPhysicsManager = do
  initChipmunk
  newWorld <- newSpace
  let cfg = def
  gravity newWorld $= toHipmunkVector (cfg ^. defGravity)
  return PhysicsManager {
      _world = newWorld
    , _bodies = 0
    , _physicsCfg = def
  }


--------------------------------------------------------------------------------
destroyPhysicManager :: GameMonad ()
destroyPhysicManager = do
  mgr <- gets $ view physicsMgr
  liftIO $ freeSpace (mgr ^. world)



--------------------------------------------------------------------------------
toHipmunkVector :: V2 Double -> Vector
toHipmunkVector (V2 x y) = Vector x y


--------------------------------------------------------------------------------
fromHipmunkVector :: Vector -> V2 Double
fromHipmunkVector (Vector x y) = V2 x y


--------------------------------------------------------------------------------
-- Add a new ridig body to the Physic Manager
addShape :: ShapeType -> V2 Double -> GameMonad Shape
addShape shpTyp pos = do
  pMgr <- gets $ view physicsMgr
  let wrld = pMgr ^. world
  let cfg  = pMgr ^. physicsCfg
  let defaultMass   = cfg ^. defMass 
  let momentFn = cfg ^. defMoment
  let defaultMoment = momentFn defaultMass shpTyp (toHipmunkVector pos)
  bd <- liftIO $ newBody defaultMass defaultMoment
  sh <- liftIO $ newShape bd shpTyp (toHipmunkVector pos)
  liftIO $ spaceAdd wrld bd
  liftIO $ spaceAdd wrld sh
  physicsMgr .= (bodies +~ 1 $ pMgr)
  return sh
