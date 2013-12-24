module Wires where


import Prelude hiding ((.), id)
import Control.Wire
import FRP.Netwire.Move
import qualified SFML.Window as W
import Control.Monad.SFML.Window
import Control.Lens hiding (at)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.SFML
import qualified SFML.Graphics as G
import qualified Data.IntMap.Strict as Map
import Types



--------------------------------------------------------------------------------
challenge1 :: GameWire NominalDiffTime NominalDiffTime
challenge1 = for 10 . integral 0 . pure 20    -->
             for 10 . integral 0 . pure (-20) -->
             challenge1


--------------------------------------------------------------------------------
blink :: GameWire NominalDiffTime NominalDiffTime
blink = after 4 . (for 5 --> blink)


--------------------------------------------------------------------------------
ifPressedGo :: W.KeyCode -> (Int,Int) -> GameWire NominalDiffTime (Int, Int)
ifPressedGo code coords = mkGen_ $ \_ -> do
  keyPressed <- lift $ isKeyPressed code
  if keyPressed
     then return . Right $ coords
     else return . Left $ ()

--------------------------------------------------------------------------------
moveLeft :: GameWire NominalDiffTime (Int, Int)
moveLeft = ifPressedGo W.KeyA (-5, 0)


--------------------------------------------------------------------------------
moveRight :: GameWire NominalDiffTime (Int, Int)
moveRight = ifPressedGo W.KeyD (5, 0)

--------------------------------------------------------------------------------
moveUp :: GameWire NominalDiffTime (Int, Int)
moveUp = ifPressedGo W.KeyW (0, -5)

--------------------------------------------------------------------------------
moveDown :: GameWire NominalDiffTime (Int, Int)
moveDown = ifPressedGo W.KeyS (0, 5)

--------------------------------------------------------------------------------
stand :: GameWire NominalDiffTime (Int, Int)
stand = pure (0,0)


--------------------------------------------------------------------------------
playerKeyboard :: GameWire NominalDiffTime (Int, Int)
playerKeyboard = moveLeft <|>
                 moveRight <|>
                 moveUp <|> 
                 moveDown <|>
                 stand


--------------------------------------------------------------------------------
always :: GameWire a Bool
always = pure True
