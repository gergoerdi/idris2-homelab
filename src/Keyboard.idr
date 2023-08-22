module Keyboard

import Data.SortedMap

public export
0 KeyCode : Type
KeyCode = String

-- 0 KeyState : Type
-- KeyState = SortedMap KeyCode Bool

public export
0 KeyState : Type
KeyState = KeyCode -> Bool
