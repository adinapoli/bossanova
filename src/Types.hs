{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Vector
import Data.Word
import Data.IORef
import Linear.V2
import System.Random
import Control.Wire hiding (Last)
import Control.Lens hiding (at)
import Control.Monad.Trans.State
import Control.Monad.SFML
import Control.Concurrent.STM
import qualified SFML.Graphics as G
import qualified Data.IntMap.Strict as Map
import qualified Data.Map.Strict as SMap

import qualified Physics.Hipmunk as H
import qualified SFML.Window as SFML
import Data.Monoid (Last)


newtype StateDelta s = StateDelta { getStateDelta :: Timed NominalDiffTime (Last s) }
  deriving (Show, Eq, Semigroup, Monoid)

instance HasTime NominalDiffTime (StateDelta s) where
  dtime (StateDelta tmd) = dtime tmd

--------------------------------------------------------------------------------
type GameWire st i o = Wire (StateDelta st) () (GameMonad st) i o


--------------------------------------------------------------------------------
type GameMonad st = StateT (GameState st) SFML


--------------------------------------------------------------------------------
data Tag =
    Position
  | AffectRendering
  | BoundingBox
  | Callback
  | Caption
  | Colour
  | Disposable
  | DynamicBody
  | EventHolder
  | EventListener
  | EventPublisher
  | Joint
  | Keyboard
  | LinearForce
  | Renderable
  | Size
  | StaticBody
  | Tagless
  | Texture
  | Timer
  | Velocity
  deriving (Show, Ord, Eq)


--------------------------------------------------------------------------------
data Component st = Component {
    _compTag :: !Tag
  , _compData :: !(ComponentData st)
}


--------------------------------------------------------------------------------
data ComponentData st =
    Sprite !(SpriteState st)
  | Text !(TextState st)
  | TextCaption !String
  | SFMLTexture !(TextureState st)
  | SizeInt !Int
  | CAnimation !(AnimationState st)
  | IntRect !G.IntRect
  | RenderColour !G.Color
  | Events ![GameCallback st]
  | PosInt !(V2 Int)
  | ForceInt !(V2 Int)
  | CTimer DiscreteTimer
  | CCallback !(GameCallback st)
  | CollisionShape !(ShapeState st)
  | MustRenderWire (GameWire st NominalDiffTime Bool)
  | Void
  | PlKbWire (Wire (StateDelta st) () SFML [SFML.KeyCode] (st -> st, V2 Int))

--------------------------------------------------------------------------------
data SpriteState st =
      UninitializedSprite (GameMonad st G.Sprite)
    | InitializedSprite !G.Sprite

--------------------------------------------------------------------------------
data TextState st =
      UninitializedText (GameMonad st G.Text)
    | InitializedText !G.Text


--------------------------------------------------------------------------------
data DiscreteTimer = DiscreteTimer
  { _dtInternalTime :: !Word64
  , _dtStepTime :: !Word64
  } deriving Show


--------------------------------------------------------------------------------
type Attached = Bool

--------------------------------------------------------------------------------
data TextureState st =
      UninitializedTexture (GameMonad st G.Texture)
    | InitializedTexture Attached !G.Texture

--------------------------------------------------------------------------------
data ShapeState st =
    HipmunkUninitializedShape (Entity st -> GameMonad st H.Shape)
  | HipmunkInitializedShape !H.Shape

--------------------------------------------------------------------------------
newtype System st = System { tick :: GameMonad st () }


--------------------------------------------------------------------------------
type Components st = SMap.Map Tag (Component st)


--------------------------------------------------------------------------------
data Alias =
    NoAlias
  | ThePlayer
  | BodyCounter
  | FPSCounter
  | SpriteCounter
  | Special
  | Enemy
  | PointCounter deriving (Show, Eq)


--------------------------------------------------------------------------------
data Entity st = Entity {
    _eId :: !Int
  , _alias :: !Alias
  , _components :: !(Components st)
}


--------------------------------------------------------------------------------
newtype GameCallback st = GameCallback {
  runEvent :: Entity st -> GameMonad st (GameCallback st)
  }



--------------------------------------------------------------------------------
data EntityManager st = EntityManager {
    _entityCounter :: !Int
  , _entities      :: !(Map.IntMap (Entity st))
  }


--------------------------------------------------------------------------------
data GameState st = GameState {
    _gameWin     :: !G.RenderWindow
  , _gameSession :: !(Session (GameMonad st) (StateDelta st))
  , _timeWire    :: !(GameWire st (Timed NominalDiffTime ()) Double)
  , _frameTime   :: !Word64
  , _fps         :: !Int
  , _managers    :: !(Managers st)
  , _randGen     :: !StdGen
  , _systems     :: ![System st]
  , _gameState   :: !st
}


--------------------------------------------------------------------------------
data Managers st = Managers {
    _entityMgr   :: !(EntityManager st)
  , _physicsMgr  :: !PhysicsManager
  , _artMgr      :: !ArtManager
}


--------------------------------------------------------------------------------
type PhysicsBodyId = Int


--------------------------------------------------------------------------------
data ArtManager = ArtManager {
    _textures   :: !(SMap.Map FilePath G.Texture)
  , _sprites    :: !Int
  , _spritePool :: !(TQueue G.Sprite)
  }


--------------------------------------------------------------------------------
data PhysicsManager = PhysicsManager {
    _world         :: !H.Space
  , _bodies        :: !PhysicsBodyId
  , _bodyPool      :: !(TQueue H.Body)
  , _collisionPool :: !(IORef [(H.Shape, H.Shape)])
  , _physicsCfg    :: !PhysicsConfig
}


--------------------------------------------------------------------------------
data PhysicsConfig = PhysicsConfig {
    _defGravity       :: !(V2 Double)
  , _defMass          :: !Double
  , _defFriction      :: !Double
  , _defElasticity    :: !Double
  , _defMoment        :: H.Mass -> H.ShapeType -> H.Position -> H.Moment
}


data Animation = Animation {
    _animationBoundingBoxes :: !(Vector G.IntRect)
  , _animationTexture :: !G.Texture
  , _animationSprite  :: !G.Sprite
  , _animationPlaying :: !Bool
  , _animationInternalTime :: !Word64
  , _animationCurrentIdx :: !Int
  , _animFrameTime :: !Double
  }


--------------------------------------------------------------------------------
data AnimationState st =
      UninitializedAnimation !(GameMonad st Animation)
    | InitializedAnimation !Animation
    | WireAnimation !(GameWire st () Animation)


--------------------------------------------------------------------------------
$(makeLenses ''GameState)
$(makeLenses ''Entity)
$(makeLenses ''Component)
$(makeLenses ''GameCallback)
$(makeLenses ''Managers)
$(makeLenses ''EntityManager)
$(makeLenses ''ArtManager)
$(makeLenses ''PhysicsManager)
$(makeLenses ''PhysicsConfig)
$(makeLenses ''Animation)
$(makeLenses ''DiscreteTimer)
$(makeLensesFor [("transform", "l_transform")] ''G.RenderStates)


--------------------------------------------------------------------------------
gray :: G.Color
gray = G.Color 20 20 20 255
