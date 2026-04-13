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
let get_builtin_templates;
let compile_to_json;
let compile_with_project_sources;
let sync_project_sources;
let get_source_root_statuses;
let get_simulation_models;
let compile_with_source_roots;
let load_source_roots;
let clear_source_root_cache;
let get_source_root_document_count;
let export_parsed_source_roots_binary;
let merge_parsed_source_roots_binary;
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
let simulate_model_with_project_sources = null;
let wasmModuleLoaded = false;

function canUseSharedWasmThreads() {
    return typeof self.crossOriginIsolated === 'boolean'
        && self.crossOriginIsolated
        && typeof SharedArrayBuffer !== 'undefined';
}

async function loadWasmModule() {
    if (wasmModuleLoaded) return;
    const mod = await import(withCacheBust('./rumoca.js'));
    init = mod.default;
    wasm_init = mod.wasm_init;
    get_version = mod.get_version;
    get_builtin_templates = mod.get_builtin_templates;
    compile_to_json = mod.compile_to_json;
    compile_with_project_sources = mod.compile_with_project_sources;
    sync_project_sources = mod.sync_project_sources;
    get_source_root_statuses = mod.get_source_root_statuses;
    get_simulation_models = mod.get_simulation_models;
    compile_with_source_roots = mod.compile_with_source_roots;
    load_source_roots = mod.load_source_roots;
    clear_source_root_cache = mod.clear_source_root_cache;
    get_source_root_document_count = mod.get_source_root_document_count;
    export_parsed_source_roots_binary = mod.export_parsed_source_roots_binary;
    merge_parsed_source_roots_binary = mod.merge_parsed_source_roots_binary;
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
    if (typeof mod.simulate_model_with_project_sources === 'function') {
        simulate_model_with_project_sources = mod.simulate_model_with_project_sources;
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
    if (message.includes('[WASM] load_source_roots: parsing')) {
        // Extract progress info: "[WASM] load_source_roots: parsing 50/500 (10%)"
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

        const requestedThreads = navigator.hardwareConcurrency || 4;
        const numThreads = canUseSharedWasmThreads() ? requestedThreads : 0;
        if (numThreads > 0) {
            console.log('[Worker] Initializing thread pool...');
        } else {
            console.warn('[Worker] Shared WASM threads unavailable; using single-thread mode.');
        }
        await wasm_init(numThreads);
        if (numThreads > 0) {
            console.log(`[Worker] Thread pool initialized with ${numThreads} threads`);
        }
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
    const { id, action, source, modelName, line, character, daeJson, template, tEnd, dt } = e.data;

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
                if (typeof sync_project_sources === 'function' && typeof payload.projectSources === 'string') {
                    sync_project_sources(payload.projectSources);
                }
                switch (command) {
                    case 'rumoca.language.getSourceRootDocumentCount':
                        result = get_source_root_document_count();
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
                        if (simulate_model_with_project_sources) {
                            result = simulate_model_with_project_sources(
                                payload.source || '',
                                payload.modelName || 'Model',
                                payload.projectSources || '{}',
                                payload.tEnd || 1.0,
                                payload.dt || 0,
                                payload.solver || 'auto',
                            );
                        } else {
                            result = simulate_model(
                                payload.source || '',
                                payload.modelName || 'Model',
                                payload.tEnd || 1.0,
                                payload.dt || 0,
                                payload.solver || 'auto',
                            );
                        }
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
                    case 'rumoca.workspace.getBuiltinTemplates':
                        result = get_builtin_templates();
                        break;
                    case 'rumoca.workspace.compile':
                        result = compile_to_json(payload.source || '', payload.modelName || 'Model');
                        break;
                    case 'rumoca.workspace.compileWithProjectSources':
                        result = compile_with_project_sources(
                            payload.source || '',
                            payload.modelName || 'Model',
                            payload.projectSources || '{}',
                        );
                        break;
                    case 'rumoca.workspace.compileWithSourceRoots':
                        result = compile_with_source_roots(
                            payload.source || '',
                            payload.modelName || 'Model',
                            payload.sourceRoots || '{}',
                        );
                        break;
                    case 'rumoca.workspace.loadSourceRoots':
                        result = load_source_roots(payload.sourceRoots || '{}');
                        break;
                    case 'rumoca.workspace.getSourceRootStatuses':
                        result = get_source_root_statuses();
                        break;
                    case 'rumoca.workspace.exportParsedSourceRootsBinary':
                        result = export_parsed_source_roots_binary(payload.urisJson || '[]');
                        break;
                    case 'rumoca.workspace.mergeParsedSourceRootsBinary':
                        result = merge_parsed_source_roots_binary(payload.bytes || new Uint8Array());
                        break;
                    case 'rumoca.workspace.clearSourceRootCache':
                        clear_source_root_cache();
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
