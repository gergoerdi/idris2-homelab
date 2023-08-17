module CPU

import Web.MVC
import Core

export
data CPU : Type where [external]

public export
%foreign """
  javascript:lambda: idr_core => () => {
    const core = {
       mem_read: addr => idr_core.a1(addr)(),
       mem_write: (addr, val) => idr_core.a2(addr)(val)(),
       io_read: addr => idr_core.a3(addr)(),
       io_write: (addr, val) => idr_core.a4(addr)(val)(),
    };
    cpu = Z80(core);

    return cpu;
  }
  """
initCPU : Core -> IO CPU

public export
%foreign "javascript:lambda: cpu => () => cpu.run_instruction()"
runInstruction : CPU -> IO Bits8
