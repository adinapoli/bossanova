{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Word
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
newtype LogicComponent = LogicComponent
                         { tick :: GameState -> GameMonad LogicComponent }

--------------------------------------------------------------------------------
newtype UIComponent = UIComponent
                      { render :: GameState -> GameMonad UIComponent }


--------------------------------------------------------------------------------
data Entity = Entity {
    _lcomponents :: [LogicComponent]
  , _uicomponents :: [UIComponent]
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
}


--------------------------------------------------------------------------------
$(makeLenses ''GameState)
$(makeLenses ''Entity)
$(makeLensesFor [("transform", "l_transform")] ''G.RenderStates)
