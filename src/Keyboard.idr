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

-- %foreign "javascript:lambda: () => code => (keystate[code] && 1) || 0"
-- prim__keyState : PrimIO KeyState

-- keyState : HasIO io => io KeyState
-- keyState = primIO prim__keyState
