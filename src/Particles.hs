{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Particles where

import Prelude hiding (id, (.))
import Linear.V2
import Data.Time
import Control.Lens
import Control.Applicative
import qualified Control.Monad as M
import Data.Vector (Vector)
import Data.Word
import GHC.Float
import qualified Data.Vector as V
import Control.Wire
import Types
import Control.Monad.Trans.Class (lift)
import Data.Default
import Control.Monad.Trans.State
import System.Random

import Control.Monad.SFML
import Control.Monad.SFML.Graphics
import qualified SFML.System as S
import qualified SFML.Graphics as G

--------------------------------------------------------------------------------
data ParticleColour =
    PCBlue
  | PCRed deriving (Enum, Bounded)


--------------------------------------------------------------------------------
data Particle = Particle {
    _partPos       :: V2 Double
  , _partMaxLife   :: NominalDiffTime
  , _partCurLife   :: NominalDiffTime
  , _partAngle     :: Int -- Degrees
  , _partSpeed     :: Int -- Pixels/sec
  , _partMaxScale  :: Double
  , _partCurScale  :: Double
  , _partAlpha     :: Double
  , _partColour    :: ParticleColour
  }

$(makeLenses ''Particle)


--------------------------------------------------------------------------------
newtype ParticlePool = ParticlePool { getPool :: Vector G.CircleShape }


data ParticleConfig = ParticleConfig {
    partCfgMinLife   :: NominalDiffTime
  , partCfgMaxLife   :: NominalDiffTime
  , partCfgMinAngle  :: Int
  , partCfgMaxAngle  :: Int
  , partCfgMinScale  :: Double
  , partCfgMaxScale  :: Double
  , partCfgColour    :: ParticleColour
  , partCfgMinAlpha  :: Double
  , partCfgMaxAlpha  :: Double
  , partCfgMinSpeed  :: Int
  , partCfgMaxSpeed  :: Int
  }

instance Default ParticleConfig where
  def = ParticleConfig {
    partCfgMinLife   = 1
  , partCfgMaxLife   = 5
  , partCfgMinAngle  = 0
  , partCfgMaxAngle  = 90
  , partCfgMinScale  = 0.1
  , partCfgMaxScale  = 2.0
  , partCfgColour    = PCBlue
  , partCfgMinAlpha  = 0.0
  , partCfgMaxAlpha  = 1.0
  , partCfgMinSpeed  = 0
  , partCfgMaxSpeed  = 20
  }

--------------------------------------------------------------------------------
data EmitterConfig = EmitterConfig {
  partConfig :: ParticleConfig
  }

instance Default EmitterConfig where
  def = EmitterConfig def


--------------------------------------------------------------------------------
data Emitter = Emitter {
    _emitTime  :: Session GameMonad (Timed NominalDiffTime ())
  , _emitTimer :: NominalDiffTime --for how many seconds it must emit
  , _emitRate  :: Int --How many particle to spawn per tick.
  , _emitWire  :: GameWire NominalDiffTime NominalDiffTime
  }

$(makeLenses ''Emitter)


--------------------------------------------------------------------------------
angle2rad :: Int -> Double
angle2rad angle = fromIntegral angle * pi / 180;


--------------------------------------------------------------------------------
velocity :: Particle -> V2 Double
velocity Particle{..} = V2 (fromIntegral _partSpeed    * cos (angle2rad _partAngle))
                           (fromIntegral (-_partSpeed) * sin (angle2rad _partAngle))


--------------------------------------------------------------------------------
time2Double :: NominalDiffTime -> Double
time2Double = fromIntegral . fromEnum


--------------------------------------------------------------------------------
-- No Wire for now, no elegant Lens abstractions
update :: Particle -> NominalDiffTime -> Particle
update p dt =
  let p' = (partCurLife -~ dt $ p)
  in if p' ^. partCurLife > 0 then updateStats p' else p'
  where
    updateStats :: Particle -> Particle
    updateStats pa =
      let ageRatio = (pa ^. partCurLife) / (pa ^. partMaxLife)
          oSize    = pa  ^. partMaxScale
      in (partPos +~ velocity pa * pure (time2Double dt)) .
         (partCurScale .~ (oSize * time2Double ageRatio)) .
         (partAlpha .~ time2Double ageRatio) $ pa


--------------------------------------------------------------------------------
-- Takes a vector of particles, as given by the emitter (?) and
-- draw all the particles. For now it will use a fixed pool.
-- Let's not worry about memory allocation shall we?
renderer :: G.SFRenderTarget a
             => Vector Particle
             -> a
             -> GameMonad ()
renderer parts renderTarget = V.forM_ parts spriteFromParticle
  where
    spriteFromParticle :: Particle -> GameMonad ()
    spriteFromParticle Particle{..} = M.unless (_partCurLife <= 0) $ do
      shp <- lift createCircleShape
      lift $ setFillColor shp (colourToSFMLColour _partColour _partAlpha)
      lift $ setPosition shp (toVec2f _partPos)
      lift $ setScale shp (toVec2f . pure $ _partCurScale)
      lift $ drawCircle renderTarget shp Nothing


--------------------------------------------------------------------------------
colourToSFMLColour :: ParticleColour -> Double -> G.Color
colourToSFMLColour PCBlue a = G.Color 0 0 255 (double2Word8 a)


--------------------------------------------------------------------------------
toVec2f :: V2 Double -> S.Vec2f
toVec2f (V2 x y) = S.Vec2f (double2Float x) (double2Float y)


--------------------------------------------------------------------------------
double2Word8 :: Double -> Word8
double2Word8 v = truncate (255.0 / v)


--------------------------------------------------------------------------------
enumRandom  :: (RandomGen g, Enum e) => g -> (e, g)
enumRandom gen = 
    let (int, gen') = random gen in (toEnum int, gen')


--------------------------------------------------------------------------------
genRandParticle :: V2 Double -> ParticleConfig -> GameMonad Particle
genRandParticle startPos ParticleConfig{..} = do
  rRand    <- gets $ view randGen
  let (l, g1)  = randomR (fromEnum partCfgMinLife,
                          fromEnum partCfgMaxLife) rRand
  let (a, g2)  = randomR (partCfgMinAngle, partCfgMaxAngle) g1
  let (sp, g3) = randomR (partCfgMinSpeed, partCfgMaxSpeed) g2
  let (sc, g4) = randomR (partCfgMinScale, partCfgMaxScale) g3
  let (al, g5) = randomR (partCfgMinAlpha, partCfgMaxAlpha) g4
  let (cl, g6) = enumRandom  g5
  randGen .= g6
  return Particle {
            _partPos       = startPos
          , _partMaxLife   = toEnum l
          , _partCurLife   = toEnum l
          , _partAngle     = a
          , _partSpeed     = sp
          , _partMaxScale  = sc
          , _partCurScale  = sc
          , _partAlpha     = al
          , _partColour    = cl
          }
