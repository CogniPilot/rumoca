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
let get_simulation_models;
let compile_with_libraries;
let load_libraries;
let clear_library_cache;
let get_library_count;
let lsp_diagnostics;
let lsp_hover;
let lsp_completion;
let lsp_completion_with_timing;
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
    get_simulation_models = mod.get_simulation_models;
    compile_with_libraries = mod.compile_with_libraries;
    load_libraries = mod.load_libraries;
    clear_library_cache = mod.clear_library_cache;
    get_library_count = mod.get_library_count;
    lsp_diagnostics = mod.lsp_diagnostics;
    lsp_hover = mod.lsp_hover;
    lsp_completion = mod.lsp_completion;
    lsp_completion_with_timing = mod.lsp_completion_with_timing;
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
        await init({ module_or_path: withCacheBust('./rumoca_bg.wasm') });

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
            case 'languageCommand': {
                const command = e.data.command;
                const payload = e.data.payload || {};
                switch (command) {
                    case 'rumoca.language.getLibraryCount':
                        result = get_library_count();
                        break;
                    case 'rumoca.language.diagnostics':
                        result = lsp_diagnostics(payload.source || '');
                        break;
                    case 'rumoca.language.hover':
                        result = lsp_hover(payload.source || '', payload.line, payload.character);
                        break;
                    case 'rumoca.language.completion':
                        result = lsp_completion(payload.source || '', payload.line, payload.character);
                        break;
                    case 'rumoca.language.completionWithTiming':
                        result = lsp_completion_with_timing(payload.source || '', payload.line, payload.character);
                        break;
                    case 'rumoca.language.definition':
                        result = lsp_definition(payload.source || '', payload.line, payload.character);
                        break;
                    case 'rumoca.language.documentSymbols':
                        result = lsp_document_symbols(payload.source || '');
                        break;
                    case 'rumoca.language.codeActions':
                        result = lsp_code_actions(
                            payload.source || '',
                            payload.rangeStartLine,
                            payload.rangeStartCharacter,
                            payload.rangeEndLine,
                            payload.rangeEndCharacter,
                            payload.diagnosticsJson || '[]',
                        );
                        break;
                    case 'rumoca.language.semanticTokens':
                        result = lsp_semantic_tokens(payload.source || '');
                        break;
                    case 'rumoca.language.semanticTokenLegend':
                        result = lsp_semantic_token_legend();
                        break;
                    case 'rumoca.language.listClasses':
                        result = list_classes();
                        break;
                    case 'rumoca.language.getClassInfo':
                        result = get_class_info(payload.qualifiedName);
                        break;
                    default:
                        throw new Error(`Unknown language command: ${command}`);
                }
                break;
            }
            case 'projectCommand': {
                const command = e.data.command;
                const payload = e.data.payload || {};
                switch (command) {
                    case 'rumoca.project.getSimulationModels':
                        result = get_simulation_models(payload.source || '', payload.defaultModel || '');
                        break;
                    case 'rumoca.project.startSimulation':
                        if (!simulate_model) {
                            throw new Error('Simulation not available in this WASM build. Rebuild with rumoca-sim (diffsol feature enabled).');
                        }
                        result = simulate_model(
                            payload.source || '',
                            payload.modelName || 'Model',
                            payload.tEnd || 1.0,
                            payload.dt || 0,
                            payload.solver || 'auto',
                        );
                        break;
                    default:
                        throw new Error(`Unknown project command: ${command}`);
                }
                break;
            }
            case 'workspaceCommand': {
                const command = e.data.command;
                const payload = e.data.payload || {};
                switch (command) {
                    case 'rumoca.workspace.getVersion':
                        result = get_version();
                        break;
                    case 'rumoca.workspace.compile':
                        result = compile_to_json(payload.source || '', payload.modelName || 'Model');
                        break;
                    case 'rumoca.workspace.compileWithLibraries':
                        result = compile_with_libraries(
                            payload.source || '',
                            payload.modelName || 'Model',
                            payload.libraries || '{}',
                        );
                        break;
                    case 'rumoca.workspace.loadLibraries':
                        result = load_libraries(payload.libraries || '{}');
                        break;
                    case 'rumoca.workspace.clearLibraryCache':
                        clear_library_cache();
                        result = 'OK';
                        break;
                    case 'rumoca.workspace.renderTemplate':
                        result = render_template(payload.daeJson, payload.template);
                        break;
                    default:
                        throw new Error(`Unknown workspace command: ${command}`);
                }
                break;
            }
            default:
                throw new Error(`Unknown action: ${action}`);
        }
        if (result instanceof Uint8Array) {
            self.postMessage({ id, success: true, result }, [result.buffer]);
            return;
        }
        self.postMessage({ id, success: true, result });
    } catch (e) {
        console.error('[Worker] Error:', e);
        self.postMessage({ id, error: e.message || String(e) });
    }
};
