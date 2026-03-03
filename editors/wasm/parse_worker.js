// Lightweight Web Worker for parallel library parsing
// Multiple instances of this worker can run concurrently to parse files in parallel

import init, { parse_library_file } from './rumoca.js';

let initialized = false;

async function initialize() {
    if (initialized) return true;
    try {
        await init();
        initialized = true;
        return true;
    } catch (e) {
        console.error('[ParseWorker] Init failed:', e);
        return false;
    }
}

// Initialize immediately
initialize().then(success => {
    self.postMessage({ ready: true, success });
});

// Handle parse requests
self.onmessage = async (e) => {
    const { id, files } = e.data;

    if (!initialized) {
        self.postMessage({ id, error: 'Worker not initialized' });
        return;
    }

    // Parse all files assigned to this worker
    const results = [];
    for (let i = 0; i < files.length; i++) {
        const [filename, source] = files[i];
        try {
            const ast = parse_library_file(source, filename);
            results.push([filename, ast]);
        } catch (e) {
            // Skip files that fail to parse
            console.warn(`[ParseWorker] Failed to parse ${filename}:`, e.message);
        }

        // Report progress every 10 files
        if (i % 10 === 0) {
            self.postMessage({ id, progress: true, current: i, total: files.length });
        }
    }

    self.postMessage({ id, success: true, results });
};
