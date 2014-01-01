module Entities where

import Prelude hiding ((.))
import Control.Wire
import Control.Lens hiding (at)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.SFML
import qualified SFML.Graphics as G
import qualified Data.IntMap.Strict as Map
import Types

--------------------------------------------------------------------------------
-- | Add an entity. Returns the id of the created entity.
(#>) :: Entity -> GameMonad Int
(#>) e = do
  currentId <- fmap Map.size (gets $ view entityMgr)
  eMgr <- gets $ view entityMgr
  entityMgr .= Map.insert currentId (eId .~ currentId $ e) eMgr
  return currentId


--------------------------------------------------------------------------------
-- | Delete an entity.
(#~) :: Int -> GameMonad ()
(#~) e = do
  eMgr <- gets $ view entityMgr
  entityMgr .= Map.delete e eMgr

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
fromList ls = Map.fromList $
              map (\(cId, e) -> (cId, eId .~ cId $ e)) (zip [0..] ls)


--------------------------------------------------------------------------------
-- This should be accomplished by an "Alias manager", but it would be
-- brittle to keep in sync it with the entityMgr.
-- this method call is slow as it takes O(n).
getByAlias :: Alias -> GameMonad [Entity]
getByAlias a = do
  eMgr <- gets $ view entityMgr
  return . map snd . Map.toList . Map.filter (\v -> v ^. alias == a) $ eMgr
