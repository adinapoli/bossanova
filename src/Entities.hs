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
  currentId <- fmap Map.size (gets $ view entityMgr)
  eMgr <- gets $ view entityMgr
  entityMgr .= Map.insert currentId e eMgr


--------------------------------------------------------------------------------
-- | Delete an entity.
(#~) :: Int -> GameMonad ()
(#~) eId = do
  eMgr <- gets $ view entityMgr
  entityMgr .= Map.delete eId eMgr

--------------------------------------------------------------------------------
-- | Delete the last entity.
popEntity :: GameMonad ()
popEntity = do
  currentId <- fmap Map.size (gets $ view entityMgr)
  eMgr <- gets $ view entityMgr
  entityMgr .= Map.delete (currentId - 1) eMgr
  
--------------------------------------------------------------------------------
-- | Builds an entity manager from a list of entities.
fromList :: [Entity] -> EntityManager
fromList ls = Map.fromList (zip [1..] ls)
