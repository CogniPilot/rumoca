// Web Worker for Rumoca WASM with rayon threading support
// This worker runs WASM functions that use Atomics.wait (not allowed on main thread)

import init, {
    wasm_init,
    get_version,
    compile_to_json,
    compile_with_libraries,
    load_libraries,
    clear_library_cache,
    get_library_count,
    lsp_diagnostics,
    lsp_hover,
    lsp_completion,
    lsp_document_symbols,
    lsp_semantic_tokens,
    lsp_semantic_token_legend,
    render_template
} from './rumoca.js';

let initialized = false;

async function initialize() {
    if (initialized) return true;

    try {
        console.log('[Worker] Loading WASM module...');
        await init();

        console.log('[Worker] Initializing thread pool...');
        const numThreads = navigator.hardwareConcurrency || 4;
        await wasm_init(numThreads);

        console.log(`[Worker] Thread pool initialized with ${numThreads} threads`);
        initialized = true;
        return true;
    } catch (e) {
        console.error('[Worker] Initialization failed:', e);
        return false;
    }
}

// Initialize and report status
initialize().then(success => {
    self.postMessage({ ready: true, success });
});

// Handle messages from main thread
self.onmessage = async (e) => {
    const { id, action, source, modelName, libraries, line, character, daeJson, template } = e.data;

    if (!initialized) {
        self.postMessage({ id, error: 'Worker not initialized' });
        return;
    }

    try {
        let result;
        switch (action) {
            case 'compile':
                result = compile_to_json(source, modelName || 'Model');
                break;
            case 'compileWithLibraries':
                result = compile_with_libraries(source, modelName || 'Model', libraries || '{}');
                break;
            case 'loadLibraries':
                result = load_libraries(libraries || '{}');
                break;
            case 'clearLibraryCache':
                clear_library_cache();
                result = 'OK';
                break;
            case 'getLibraryCount':
                result = get_library_count();
                break;
            case 'getVersion':
                result = get_version();
                break;
            case 'diagnostics':
                result = lsp_diagnostics(source);
                break;
            case 'hover':
                result = lsp_hover(source, line, character);
                break;
            case 'completion':
                result = lsp_completion(source, line, character);
                break;
            case 'documentSymbols':
                result = lsp_document_symbols(source);
                break;
            case 'semanticTokens':
                result = lsp_semantic_tokens(source);
                break;
            case 'semanticTokenLegend':
                result = lsp_semantic_token_legend();
                break;
            case 'renderTemplate':
                result = render_template(daeJson, template);
                break;
            default:
                throw new Error(`Unknown action: ${action}`);
        }
        self.postMessage({ id, success: true, result });
    } catch (e) {
        console.error('[Worker] Error:', e);
        self.postMessage({ id, error: e.message || String(e) });
    }
};
