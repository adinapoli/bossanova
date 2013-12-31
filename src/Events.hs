module Events where

import Prelude hiding ((.))
import Control.Wire
import Control.Lens hiding (at)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.SFML
import qualified SFML.Graphics as G
import qualified Data.IntMap.Strict as Map
import Types
import Linear.V2


--playerMoved :: GameWire (Event GameEvent) (V2 Int)
--playerMoved = asSoonAs . (V2 20 30)
