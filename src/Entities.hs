module Entities where

import Control.Wire
import Control.Lens hiding (at)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.SFML
import qualified SFML.Graphics as G
import qualified Data.IntMap.Strict as Map
import Types

--------------------------------------------------------------------------------
-- | Add an entity.
(#>) :: Entity -> GameMonad ()
(#>) e = do
  currentId <- gets $ view entityNum
  eMgr <- gets $ view entityMgr
  entityMgr .= Map.insert currentId e eMgr
  entityNum += 1


--------------------------------------------------------------------------------
-- | Builds an entity manager from a list of entities.
fromList :: [Entity] -> EntityManager
fromList ls = Map.fromList (zip [1..] ls)
