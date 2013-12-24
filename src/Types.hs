{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Wire
import Control.Lens hiding (at)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.SFML
import qualified SFML.Graphics as G
import qualified Data.IntMap.Strict as Map


type GameWire = Wire (Timed NominalDiffTime ()) () GameMonad

--------------------------------------------------------------------------------
type GameMonad = StateT GameState SFML

--------------------------------------------------------------------------------
newtype Component = Component { tick :: GameState -> GameMonad Component }

--------------------------------------------------------------------------------
data Entity = Entity {
    _rState :: G.RenderStates
  , _components :: [Component]
}


--------------------------------------------------------------------------------
type EntityManager = Map.IntMap Entity


--------------------------------------------------------------------------------
data GameState = GameState {
    _gameWin    :: G.RenderWindow
  , _gameTime   :: Session GameMonad (Timed NominalDiffTime ())
  , _entityNum  :: Int
  , _entityMgr  :: EntityManager
}


--------------------------------------------------------------------------------
$(makeLenses ''GameState)
$(makeLenses ''Entity)
$(makeLensesFor [("transform", "l_transform")] ''G.RenderStates)
