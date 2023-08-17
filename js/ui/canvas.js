(() => {
    const canvas = document.getElementById('crt');

    function resizeCanvas() {
        const container = document.getElementById('canvas-container');
        const aspectRatio = canvas.width / canvas.height;
        const containerWidth = container.offsetWidth;
        const containerHeight = container.offsetHeight;
        
        let canvasWidth, canvasHeight;
        
        if (containerWidth / containerHeight > aspectRatio) {
            canvasWidth = containerHeight * aspectRatio;
            canvasHeight = containerHeight;
        } else {
            canvasWidth = containerWidth;
            canvasHeight = containerWidth / aspectRatio;
        }
        
        canvas.style.width = `${canvasWidth}px`;
        canvas.style.height = `${canvasHeight}px`;
    }
    
    window.addEventListener('resize', resizeCanvas);
    resizeCanvas(); // Call once on page load
})();
