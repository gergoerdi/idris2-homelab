const render_textbuf = canvas => {
    const ctx = canvas.getContext("2d");
    const buf = ctx.createImageData(canvas.width, canvas.height);
    
    const setPixel = (ptr, pixel) => {
            buf.data[ptr++] = pixel ? 0xff : 0x00;
            buf.data[ptr++] = pixel ? 0xff : 0x00;
            buf.data[ptr++] = pixel ? 0xff : 0x00;
            buf.data[ptr++] = 0xff;

            return ptr;
    };

    return (charset, vram) => {
        let ptr = 0;

        for (let row = 0; row < 25; ++row) {
            for (let subrow = 0; subrow < 8; ++subrow) {
                for (let col = 0; col < 40; ++col) {
                    const c = vram[1 + row * 40 + col];

                    let glyphLine = charset[subrow << 8 | c];
                    for (let j = 0; j < 8; ++j) {
                        let glyphPixel = glyphLine & 0x80;
                        ptr = setPixel(ptr, glyphPixel);
                        glyphLine <<= 1;
                    }
                }
            }
        }
    }
}
