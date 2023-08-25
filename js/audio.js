const load_audio = async (url) => {
    const AudioContext = window.AudioContext || window.webkitAudioContext;
    var audioCtx = new AudioContext();

    const response = await fetch(url);
    const buf = await audioCtx.decodeAudioData(await response.arrayBuffer());

    return buf;
}

const load_audio_ = (url, cb) => load_audio(url).then(buf => cb(buf)());

