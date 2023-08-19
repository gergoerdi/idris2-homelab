const keystate = {};
window.onkeyup = e => { keystate[e.code] = false; };
window.onkeydown = e => { keystate[e.code] = true; };
window.addEventListener('blur',() => { for (const key in keystate) { delete keystate[key]; } });
