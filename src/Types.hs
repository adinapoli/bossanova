{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Types where

import Data.Word
import Linear.V2
import System.Random
import Control.Wire
import Control.Lens hiding (at)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.SFML
import Control.Concurrent.STM
import qualified SFML.Graphics as G
import qualified Data.IntMap.Strict as Map
import qualified Data.Map.Strict as SMap


--------------------------------------------------------------------------------
type GameWire = Wire (Timed NominalDiffTime ()) () GameMonad


--------------------------------------------------------------------------------
type GameMonad = StateT GameState SFML


--------------------------------------------------------------------------------
data Tag =
    Position
  | Velocity
  | Keyboard
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
    _compTag :: Tag
  , _compData :: ComponentData
}


--------------------------------------------------------------------------------
data ComponentData =
    Sprite G.Sprite
  | Text G.Text
  | TextCaption String
  | SizeInt Int
  | RenderColour G.Color
  | Events [GameEvent]
  | PosInt (V2 Int)
  | MustRenderWire (GameWire NominalDiffTime Bool)
  | PlKbWire (GameWire NominalDiffTime (V2 Int))


--------------------------------------------------------------------------------
newtype System = System { tick :: GameMonad () }


--------------------------------------------------------------------------------
type Components = SMap.Map Tag Component


--------------------------------------------------------------------------------
type Alias = String


--------------------------------------------------------------------------------
data Entity = Entity {
    _eId :: Int
  , _alias :: Maybe Alias
  , _components :: Components
}


--------------------------------------------------------------------------------
newtype GameEvent = GameEvent { runEvent :: Entity -> GameMonad GameEvent }



--------------------------------------------------------------------------------
type EntityManager = Map.IntMap Entity


--------------------------------------------------------------------------------
data GameState = GameState {
    _gameWin    :: G.RenderWindow
  , _gameTime   :: Session GameMonad (Timed NominalDiffTime ())
  , _frameTime  :: Word64
  , _fps        :: Int
  , _entityMgr  :: EntityManager
  , _randGen    :: StdGen
  , _systems    :: [System]
}


--------------------------------------------------------------------------------
$(makeLenses ''GameState)
$(makeLenses ''Entity)
$(makeLenses ''Component)
$(makeLenses ''GameEvent)
$(makeLensesFor [("transform", "l_transform")] ''G.RenderStates)
