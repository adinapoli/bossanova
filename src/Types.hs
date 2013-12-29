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
  | Renderable deriving (Show, Ord, Eq)


--------------------------------------------------------------------------------
data Component = Component {
    _compTag :: Tag
  , _compData :: ComponentData
}


data ComponentData =
    Sprite G.Sprite
  | PosInt (V2 Int)
  | MustRenderWire (GameWire NominalDiffTime Bool)
  | PlKbWire (GameWire NominalDiffTime (V2 Int))


--------------------------------------------------------------------------------
newtype System = System { tick :: GameMonad () }


--------------------------------------------------------------------------------
data Entity = Entity {
    _eId :: Int
  , _components :: SMap.Map Tag Component
}


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
$(makeLensesFor [("transform", "l_transform")] ''G.RenderStates)
