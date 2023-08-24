module Emu.Keyboard

import Data.SortedSet

public export
0 KeyCode : Type
KeyCode = String

export
0 KeyState : Type
KeyState = SortedSet KeyCode

public export
empty : KeyState
empty = Data.SortedSet.empty

public export
keyPressed : KeyState -> KeyCode -> Bool
keyPressed keyState code = contains code keyState

public export
setKeyState : Bool -> KeyCode -> KeyState -> KeyState
setKeyState True = insert
setKeyState False = delete
