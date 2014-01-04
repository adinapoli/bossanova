{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

module Types where

import Data.Word
import Linear.V2
import System.Random
import Control.Wire
import Control.Lens hiding (at)
import Control.Monad.Trans.State
import Control.Monad.SFML
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
  | Velocity
  | LinearForce
  | DynamicBody
  | Texture
  | BoundingBox
  | StaticBody
  | Joint
  | Keyboard
  | Mouse
  | AffectRendering
  | EventListener
  | EventPublisher
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
  | IntRect !G.IntRect
  | RenderColour !G.Color
  | Events ![GameCallback]
  | PosInt !(V2 Int)
  | ForceInt !(V2 Int)
  | MouseCallback (GameMonad ())
  | CollisionShape !ShapeState
  | MustRenderWire (GameWire NominalDiffTime Bool)
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
  | Special
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
type EntityManager = Map.IntMap Entity


--------------------------------------------------------------------------------
data GameState = GameState {
    _gameWin     :: !G.RenderWindow
  , _gameTime    :: !(Session GameMonad (Timed NominalDiffTime ()))
  , _frameTime   :: !Word64
  , _fps         :: !Int
  , _entityMgr   :: !EntityManager
  , _physicsMgr  :: !PhysicsManager
  , _artMgr      :: !ArtManager
  , _randGen     :: !StdGen
  , _systems     :: ![System]
}


--------------------------------------------------------------------------------
type PhysicsBodyId = Int


--------------------------------------------------------------------------------
type ArtManager = SMap.Map FilePath G.Texture


--------------------------------------------------------------------------------
data PhysicsManager = PhysicsManager {
    _world      :: !H.Space
  , _bodies     :: !PhysicsBodyId
  , _physicsCfg :: !PhysicsConfig
}


--------------------------------------------------------------------------------
data PhysicsConfig = PhysicsConfig {
    _defGravity       :: !(V2 Double)
  , _defMass          :: !Double
  , _defFriction      :: !Double
  , _defElasticity    :: !Double
  , _defMoment        :: H.Mass -> H.ShapeType -> H.Position -> H.Moment
}


--------------------------------------------------------------------------------
$(makeLenses ''GameState)
$(makeLenses ''Entity)
$(makeLenses ''Component)
$(makeLenses ''GameCallback)
$(makeLenses ''PhysicsManager)
$(makeLenses ''PhysicsConfig)
$(makeLensesFor [("transform", "l_transform")] ''G.RenderStates)


--------------------------------------------------------------------------------
gray :: G.Color
gray = G.Color 20 20 20 255
