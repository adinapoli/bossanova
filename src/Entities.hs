module Entities where

import Prelude hiding ((.))
import Control.Wire hiding (at)
import Control.Lens
import Control.Monad.Trans.State
import qualified Data.Map.Strict as SMap
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
entityByAlias :: Alias -> GameMonad [Entity]
entityByAlias a = do
  eMgr <- gets $ view entityMgr
  return . map snd . Map.toList . Map.filter (\v -> v ^. alias == a) $ eMgr


--------------------------------------------------------------------------------
-- Common pitfall: Once you update the entity you must ask the entityManager
-- for the new entity again, since everything is immutable. To avoid this, we
-- don't update the entity directly, but we just use its ID to fetch the entity
-- from the EntityManager.
(#.=) :: Entity -> Component -> GameMonad ()
ent #.= newC = do
  eMgr <- gets $ view entityMgr
  case eMgr ^. at (_eId ent) of
    Just oldE -> do
      let tg = _compTag newC
      let newE = components .~ SMap.insert tg newC (_components oldE) $ ent
      entityMgr .= Map.insert (_eId newE) newE eMgr
    Nothing -> return ()
