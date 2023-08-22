module Web.Interval

-- Copied from Web.MVC.Animate internals

||| ID used to identify and cancel a running timer.
public export
data IntervalId : Type where [external]

%foreign "browser:lambda:(n,h,w)=>setInterval(() => h(w),n)"
prim__setInterval : Bits32 -> IO () -> PrimIO IntervalId

public export
setInterval : HasIO io => Bits32 -> IO () -> io IntervalId
setInterval ms action = primIO $ prim__setInterval ms action

%foreign "browser:lambda:(i,w)=>clearInterval(i)"
prim__clearInterval : IntervalId -> PrimIO ()

public export
clearInterval : HasIO io => IntervalId -> io ()
clearInterval id = primIO $ prim__clearInterval id
