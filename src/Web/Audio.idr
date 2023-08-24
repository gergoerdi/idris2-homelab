module Web.Audio

import JS.Array

%default total

export
data AudioBuffer : Type where [external]

public export
%foreign "javascript:lambda: buf => buf.sampleRate"
sampleRate : AudioBuffer -> Bits32

public export
%foreign "javascript:lambda: (buf, i) => buf.getChannelData(i)"
channelData : AudioBuffer -> Bits32 -> Array Double
