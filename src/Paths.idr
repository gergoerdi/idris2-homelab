module Paths

%default total

public export
dataFile : String -> String
dataFile s = "../data/hl2/" <+> s

public export
tapeFile : String -> String
tapeFile s = "../tapes/hl2/" <+> s
