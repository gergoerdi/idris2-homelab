let cpu = null;

// const prepareUI = async (startUI) => {
(async () => {
    const main_buf = await (await fetch("../data/hl2/rom.bin")).arrayBuffer();
    const char_buf = await (await fetch("../data/hl2/charset.bin")).arrayBuffer();

    const char_rom = new Uint8Array(char_buf.slice());

    const render = render_textbuf(document.getElementById("crt"), char_rom);
    startUI(main_buf, (vram) => () => render(vram));
})();
