let cpu = null;

const startVideo = async vram => {
    const char_buf = await (await fetch("../data/hl2/charset.bin")).arrayBuffer();
    const char_rom = new Uint8Array(char_buf.slice());

    const render = render_textbuf(document.getElementById("crt"), char_rom);

    function animate(t) {
        requestAnimationFrame(animate);
        render(vram);
    }
    requestAnimationFrame(animate);
}

(async () => {
    const main_buf = await (await fetch("../data/hl2/rom.bin")).arrayBuffer();
    startUI(main_buf);
})();
