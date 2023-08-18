module CPU

import Web.MVC
import Core

export
data CPU : Type where [external]

%foreign """
  javascript:lambda: (mem_read, mem_write, io_read, io_write) =>
    Z80({
      mem_read: addr => mem_read(addr)(),
      mem_write: (addr, val) => mem_write(addr)(val)(),
      io_read: addr => io_read(addr)(),
      io_write: (addr, val) => io_write(addr)(val)() })
  """
prim__initCPU :
     (Bits16 -> PrimIO Bits8)
  -> (Bits16 -> Bits8 -> PrimIO ())
  -> (Bits8 -> PrimIO Bits8)
  -> (Bits8 -> Bits8 -> PrimIO ())
  -> PrimIO CPU

public export
initCPU : Core -> IO CPU
initCPU core = primIO $ prim__initCPU
  (\addr => toPrim $ core.readMem addr)
  (\addr, val => toPrim $ core.writeMem addr val)
  (\port => toPrim $ core.readIO port)
  (\port, val => toPrim $ core.writeIO port val)

public export
%foreign "javascript:lambda: cpu => () => cpu.run_instruction()"
runInstruction : CPU -> IO Bits8
