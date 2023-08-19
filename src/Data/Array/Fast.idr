-- From https://github.com/gergoerdi/functional-mos6502-web-performance
module Data.Array.Fast

import JS.Buffer

%foreign "javascript:lambda:(arr,n,w) => arr[n]"
prim__readArr : UInt8Array -> Bits32 -> PrimIO Bits8

%foreign "javascript:lambda:(arr,n,v,w) => { arr[n] = v }"
prim__writeArr : UInt8Array -> Bits32 -> Bits8 -> PrimIO ()

public export
readArray : HasIO io => UInt8Array -> Bits32 -> io Bits8
readArray arr idx = primIO $ prim__readArr arr idx

public export
writeArray : HasIO io => UInt8Array -> Bits32 -> Bits8 -> io ()
writeArray arr idx x = primIO $ prim__writeArr arr idx x
