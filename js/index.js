let cpu = null;

const prepareUI = async (startUI) => {
    const main_buf = await (await fetch("../data/hl2/rom.bin")).arrayBuffer();
    const char_buf = await (await fetch("../data/hl2/charset.bin")).arrayBuffer();

    startUI(main_buf)(char_buf)();
};
