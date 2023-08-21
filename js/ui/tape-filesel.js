const tape_filesel = document.getElementById("tape-filesel");

tape_filesel.addEventListener("click", (e) => {
    if (e.target != tape_filesel) return;

    const rect = e.target.getBoundingClientRect();
    const clicked_in_dialog = (
        rect.top <= e.clientY &&
            e.clientY <= rect.top + rect.height &&
            rect.left <= e.clientX &&
            e.clientX <= rect.left + rect.width
    );

    if (!clicked_in_dialog)
        tape_filesel.close();
});
