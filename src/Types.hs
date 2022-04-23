{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

module Types where

import Data.Vector
import Data.Word
import Data.IORef
import Linear.V2
import System.Random
import Control.Wire
import Control.Lens hiding (at)
import Control.Monad.Trans.State
import Control.Monad.SFML
import Control.Concurrent.STM
import qualified SFML.Graphics as G
import qualified Data.IntMap.Strict as Map
import qualified Data.Map.Strict as SMap

import qualified Physics.Hipmunk as H


--------------------------------------------------------------------------------
type GameWire = Wire (Timed NominalDiffTime ()) () GameMonad


--------------------------------------------------------------------------------
type GameMonad = StateT GameState SFML


--------------------------------------------------------------------------------
data Tag =
    Position
  | Tagless
  | Velocity
  | LinearForce
  | DynamicBody
  | Texture
  | BoundingBox
  | StaticBody
  | Joint
  | Keyboard
  | Callback
  | Timer
  | AffectRendering
  | EventListener
  | EventPublisher
  | Disposable
  | EventHolder
  | Size
  | Colour
  | Caption
  | Renderable deriving (Show, Ord, Eq)


--------------------------------------------------------------------------------
data Component = Component {
    _compTag :: !Tag
  , _compData :: !ComponentData
}


--------------------------------------------------------------------------------
data ComponentData =
    Sprite !SpriteState
  | Text !TextState
  | TextCaption !String
  | SFMLTexture !TextureState
  | SizeInt !Int
  | CAnimation !AnimationState
  | IntRect !G.IntRect
  | RenderColour !G.Color
  | Events ![GameCallback]
  | PosInt !(V2 Int)
  | ForceInt !(V2 Int)
  | CTimer DiscreteTimer
  | CCallback !GameCallback
  | CollisionShape !ShapeState
  | MustRenderWire (GameWire NominalDiffTime Bool)
  | Void
  | PlKbWire (GameWire NominalDiffTime (V2 Int))

--------------------------------------------------------------------------------
data SpriteState =
      UninitializedSprite (GameMonad G.Sprite)
    | InitializedSprite !G.Sprite

--------------------------------------------------------------------------------
data TextState =
      UninitializedText (GameMonad G.Text)
    | InitializedText !G.Text


--------------------------------------------------------------------------------
data DiscreteTimer = DiscreteTimer
  { _dtInternalTime :: !Word64
  , _dtStepTime :: !Word64
  } deriving Show


--------------------------------------------------------------------------------
type Attached = Bool

--------------------------------------------------------------------------------
data TextureState =
      UninitializedTexture (GameMonad G.Texture)
    | InitializedTexture Attached !G.Texture

--------------------------------------------------------------------------------
data ShapeState =
    HipmunkUninitializedShape (Entity -> GameMonad H.Shape)
  | HipmunkInitializedShape !H.Shape

--------------------------------------------------------------------------------
newtype System = System { tick :: GameMonad () }


--------------------------------------------------------------------------------
type Components = SMap.Map Tag Component


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
data Entity = Entity {
    _eId :: !Int
  , _alias :: !Alias
  , _components :: !Components
}


--------------------------------------------------------------------------------
newtype GameCallback = GameCallback {
  runEvent :: Entity -> GameMonad GameCallback
  }



--------------------------------------------------------------------------------
data EntityManager = EntityManager {
    _entityCounter :: !Int
  , _entities      :: !(Map.IntMap Entity)
  }


--------------------------------------------------------------------------------
data GameState = GameState {
    _gameWin     :: !G.RenderWindow
  , _gameTime    :: !(Session GameMonad (Timed NominalDiffTime ()))
  , _timeWire    :: !(GameWire (Timed NominalDiffTime ()) Double)
  , _frameTime   :: !Word64
  , _fps         :: !Int
  , _managers    :: !Managers
  , _randGen     :: !StdGen
  , _systems     :: ![System]
}


--------------------------------------------------------------------------------
data Managers = Managers {
    _entityMgr   :: !EntityManager
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
data AnimationState =
      UninitializedAnimation !(GameMonad Animation)
    | InitializedAnimation !Animation


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
