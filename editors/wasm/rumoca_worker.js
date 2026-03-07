// Web Worker for Rumoca WASM with rayon threading support
// This worker runs WASM functions that use Atomics.wait (not allowed on main thread)

// Cache-busting query propagated from worker URL (e.g. rumoca_worker.js?v=...).
const workerUrl = new URL(self.location.href);
const cacheBust = workerUrl.searchParams.get('v') || '';
const withCacheBust = (path) =>
    cacheBust ? `${path}?v=${encodeURIComponent(cacheBust)}` : path;

let init;
let wasm_init;
let get_version;
let compile_to_json;
let compile_with_libraries;
let load_libraries;
let parse_library_file;
let merge_parsed_libraries;
let clear_library_cache;
let get_library_count;
let lsp_diagnostics;
let lsp_hover;
let lsp_completion;
let lsp_definition;
let lsp_document_symbols;
let lsp_code_actions;
let lsp_semantic_tokens;
let lsp_semantic_token_legend;
let list_classes;
let get_class_info;
let render_template;
let simulate_model = null;
let wasmModuleLoaded = false;

async function loadWasmModule() {
    if (wasmModuleLoaded) return;
    const mod = await import(withCacheBust('./rumoca.js'));
    init = mod.default;
    wasm_init = mod.wasm_init;
    get_version = mod.get_version;
    compile_to_json = mod.compile_to_json;
    compile_with_libraries = mod.compile_with_libraries;
    load_libraries = mod.load_libraries;
    parse_library_file = mod.parse_library_file;
    merge_parsed_libraries = mod.merge_parsed_libraries;
    clear_library_cache = mod.clear_library_cache;
    get_library_count = mod.get_library_count;
    lsp_diagnostics = mod.lsp_diagnostics;
    lsp_hover = mod.lsp_hover;
    lsp_completion = mod.lsp_completion;
    lsp_definition = mod.lsp_definition;
    lsp_document_symbols = mod.lsp_document_symbols;
    lsp_code_actions = mod.lsp_code_actions;
    lsp_semantic_tokens = mod.lsp_semantic_tokens;
    lsp_semantic_token_legend = mod.lsp_semantic_token_legend;
    list_classes = mod.list_classes;
    get_class_info = mod.get_class_info;
    render_template = mod.render_template;
    if (typeof mod.simulate_model === 'function') {
        simulate_model = mod.simulate_model;
    }
    wasmModuleLoaded = true;
}

let initialized = false;

// Intercept console.log to forward progress messages to main thread
const originalLog = console.log;
console.log = function(...args) {
    originalLog.apply(console, args);
    // Forward WASM progress messages to main thread
    const message = args.join(' ');
    if (message.includes('[WASM] load_libraries: parsing')) {
        // Extract progress info: "[WASM] load_libraries: parsing 50/500 (10%)"
        const match = message.match(/parsing (\d+)\/(\d+) \((\d+)%\)/);
        if (match) {
            self.postMessage({
                progress: true,
                current: parseInt(match[1]),
                total: parseInt(match[2]),
                percent: parseInt(match[3])
            });
        }
    }
};

async function initialize() {
    if (initialized) return true;

    try {
        console.log('[Worker] Loading WASM module...');
        await loadWasmModule();
        await init(withCacheBust('./rumoca_bg.wasm'));

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
    const { id, action, source, modelName, libraries, line, character, daeJson, template, tEnd, dt } = e.data;

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
            case 'parseLibraryFile':
                result = parse_library_file(source, e.data.filename);
                break;
            case 'mergeParsedLibraries':
                result = merge_parsed_libraries(e.data.definitions);
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
            case 'definition':
                result = lsp_definition(source, line, character);
                break;
            case 'documentSymbols':
                result = lsp_document_symbols(source);
                break;
            case 'codeActions':
                result = lsp_code_actions(
                    source,
                    e.data.rangeStartLine,
                    e.data.rangeStartCharacter,
                    e.data.rangeEndLine,
                    e.data.rangeEndCharacter,
                    e.data.diagnosticsJson || '[]',
                );
                break;
            case 'semanticTokens':
                result = lsp_semantic_tokens(source);
                break;
            case 'semanticTokenLegend':
                result = lsp_semantic_token_legend();
                break;
            case 'listClasses':
                result = list_classes();
                break;
            case 'getClassInfo':
                result = get_class_info(e.data.qualifiedName);
                break;
            case 'renderTemplate':
                result = render_template(daeJson, template);
                break;
            case 'simulate':
                if (!simulate_model) throw new Error('Simulation not available in this WASM build. Rebuild with rumoca-sim (diffsol feature enabled).');
                result = simulate_model(source, modelName || 'Model', tEnd || 1.0, dt || 0);
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
