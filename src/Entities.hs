module Entities where

import Prelude hiding ((.))
import Control.Wire hiding (at)
import Control.Lens
import Control.Monad.Trans.State
import qualified Data.Map.Strict as SMap
import qualified Data.IntMap.Strict as Map
import Types

--------------------------------------------------------------------------------
-- | Add an entity. Returns the id of the created entity.
(#>) :: Entity st -> GameMonad st Int
(#>) e = do
  managers . entityMgr . entityCounter += 1
  eMgr <- gets . view $ managers . entityMgr
  let eMap = eMgr ^. entities
  let currentId = eMgr ^. entityCounter
  managers . entityMgr . entities .= Map.insert currentId (eId .~ currentId $ e) eMap
  return currentId


--------------------------------------------------------------------------------
-- | Delete an entity.
(#.~) :: Entity st -> GameMonad st ()
(#.~) e = do
  eMgr <- gets . view $ managers . entityMgr . entities
  managers . entityMgr . entities .= Map.delete (_eId e) eMgr


--------------------------------------------------------------------------------
-- | Delete the last entity.
popEntity :: GameMonad st ()
popEntity = do
  eMgr <- gets . view $ managers . entityMgr
  let currentId = view entityCounter eMgr
  managers . entityMgr . entities .= Map.delete currentId (view entities eMgr)


--------------------------------------------------------------------------------
-- This should be accomplished by an "Alias manager", but it would be
-- brittle to keep in sync it with the entityMgr.
-- this method call is slow as it takes O(n).
entityByAlias :: Alias -> GameMonad st [Entity st]
entityByAlias a = do
  eMgr <- gets . view $ managers . entityMgr . entities
  return . map snd . Map.toList . Map.filter (\v -> v ^. alias == a) $ eMgr


--------------------------------------------------------------------------------
-- Common pitfall: Once you update the entity you must ask the entityManager
-- for the new entity again, since everything is immutable. To avoid this, we
-- don't update the entity directly, but we just use its ID to fetch the entity
-- from the EntityManager.
(#.=) :: Entity st -> Component st -> GameMonad st ()
ent #.= newC = do
  eMgr <- gets . view $ managers . entityMgr . entities
  case eMgr ^. at (_eId ent) of
    Just oldE -> do
      let tg = _compTag newC
      let newE = components .~ SMap.insert tg newC (_components oldE) $ ent
      managers . entityMgr . entities .= Map.insert (_eId newE) newE eMgr
    Nothing -> return ()
