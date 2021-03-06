module Physics where

import Physics.Hipmunk
import Control.Concurrent.STM
import Types
import Control.Monad.SFML
import Control.Monad
import Control.Monad.Trans.State
import Control.Lens
import Data.Default
import Data.IORef
import Linear.V2
import Data.StateVar


instance Default PhysicsConfig where
  def = PhysicsConfig {
      _defGravity       = V2 0.0 0.01
    , _defMass          = 1.0
    , _defFriction      = 1.0
    , _defElasticity    = 0.0
    , _defMoment        = momentForShape
    }


--------------------------------------------------------------------------------
defaultCollisionHandler :: IORef [(Shape, Shape)] -> CollisionHandler
defaultCollisionHandler collPool = Handler Nothing Nothing (Just postSolveHldr) Nothing
  where
    postSolveHldr = do
      shapesInvolved <- shapes
      liftIO $ modifyIORef collPool (shapesInvolved :)
      fmap length (liftIO $ readIORef collPool)
      return ()

--------------------------------------------------------------------------------
createPhysicsManager :: IO PhysicsManager
createPhysicsManager = do
  initChipmunk
  newWorld <- newSpace
  queue <- newTQueueIO
  collPool <- newIORef []

  let cfg = def
  gravity newWorld $= toHipmunkVector (cfg ^. defGravity)
  --setDefaultCollisionHandler newWorld (defaultCollisionHandler collPool)
  return PhysicsManager {
      _world = newWorld
    , _bodies = 0
    , _collisionPool = collPool
    , _bodyPool = queue
    , _physicsCfg = def
  }


--------------------------------------------------------------------------------
destroyPhysicManager :: GameMonad ()
destroyPhysicManager = do
  mgr <- gets . view $ managers . physicsMgr
  liftIO $ freeSpace (mgr ^. world)



--------------------------------------------------------------------------------
toHipmunkVector :: V2 Double -> Vector
toHipmunkVector (V2 x y) = Vector x y

--------------------------------------------------------------------------------
toHipmunkVectorI :: V2 Int -> Vector
toHipmunkVectorI (V2 x y) = Vector (fromIntegral x) (fromIntegral y)

--------------------------------------------------------------------------------
fromHipmunkVector :: Vector -> V2 Double
fromHipmunkVector (Vector x y) = V2 x y


--------------------------------------------------------------------------------
-- Add a new dynamic body to the Physic Manager
addDynamicShape :: ShapeType -> V2 Double -> GameMonad Shape
addDynamicShape shpTyp pos = do
  pMgr <- gets . view $ managers . physicsMgr
  let cfg  = pMgr ^. physicsCfg
  let defaultMass   = cfg ^. defMass 
  let momentFn = cfg ^. defMoment
  let defaultMoment = momentFn defaultMass shpTyp 0
  addShape' defaultMass defaultMoment False shpTyp pos


--------------------------------------------------------------------------------
-- Add a new static body to the Physic Manager
addStaticShape :: ShapeType -> V2 Double -> GameMonad Shape
addStaticShape = addShape' infinity infinity True


--------------------------------------------------------------------------------
bodyFromPool :: Mass -> Moment -> GameMonad Body
bodyFromPool mss mom = do
  pMgr <- gets $ view $ managers . physicsMgr
  pool <- gets $ view $ managers . physicsMgr . bodyPool
  bod <- liftIO $ atomically $ tryReadTQueue pool
  case bod of
    Nothing -> do
      body' <- liftIO $ newBody mss mom
      managers . physicsMgr .= (bodies +~ 1 $ pMgr)
      initBody body'
    Just b -> initBody b
  where
    initBody bod = do
      liftIO $ moment bod $= mom
      liftIO $ mass   bod $= mss
      return bod
  

--------------------------------------------------------------------------------
addShape' :: Double
          -> Double
          -> Bool
          -> ShapeType
          -> V2 Double
          -> GameMonad Shape
addShape' mss momt isStatic shpTyp pos = do
  pMgr <- gets $ view $ managers . physicsMgr
  let defaultFriction   = pMgr ^. physicsCfg . defFriction
  let defaultElasticity = pMgr ^. physicsCfg . defElasticity
  let wrld = pMgr ^. world
  bd <- bodyFromPool mss momt
  liftIO $ position bd   $= toHipmunkVector pos
  sh <- liftIO $ newShape bd shpTyp 0
  liftIO $ do
    friction sh   $= defaultFriction
    elasticity sh $= defaultElasticity
  unless isStatic $ liftIO $ spaceAdd wrld bd
  liftIO $ spaceAdd wrld sh
  return sh
