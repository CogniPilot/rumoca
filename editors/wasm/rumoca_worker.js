// Web Worker for Rumoca WASM with rayon threading support
// This worker runs WASM functions that use Atomics.wait (not allowed on main thread)

// Cache-busting query propagated from worker URL (e.g. rumoca_worker.js?v=...).
const workerUrl = new URL(self.location.href);
const cacheBust = workerUrl.searchParams.get('v') || '';
const withCacheBust = (path) =>
    cacheBust ? `${path}?v=${encodeURIComponent(cacheBust)}` : path;

// WASM project-config commands take JSON-string arguments. Accept either an
// already-stringified payload field or a structured object from the main thread.
const jsonArg = (value, emptyDefault = '') =>
    value == null ? emptyDefault : (typeof value === 'string' ? value : JSON.stringify(value));

let init;
let wasm_init;
let get_version;
let get_builtin_targets;
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
let render_target;
let project_get_simulation_config;
let project_set_simulation_preset;
let project_reset_simulation_preset;
let project_get_visualization_config;
let project_set_visualization_config;
let project_get_scenario_config;
let project_get_scenario_config_full;
let project_set_scenario_config;
let project_default_scenario_config;
let simulate_model = null;
let simulate_model_with_project_sources = null;
let lower_model_to_solve_json = null;
let wasmModuleLoaded = false;
let activeRequestId = null;

// Lazy diffsol (stiff/implicit) addon — a separate relaxed-SIMD module, loaded
// only when a BDF solve is requested, so this worker (and the package) stay
// universal. Returns the addon's exports, or rejects on browsers without
// relaxed-SIMD.
let diffsolAddonPromise = null;
function loadDiffsolAddon() {
    if (!diffsolAddonPromise) {
        diffsolAddonPromise = (async () => {
            const mod = await import(withCacheBust('./rumoca_bind_wasm_diffsol.js'));
            await mod.default();
            return mod;
        })();
        diffsolAddonPromise.catch(() => {
            diffsolAddonPromise = null;
        });
    }
    return diffsolAddonPromise;
}

// Run a stiff (BDF/diffsol) simulation: the main module lowers the model to a
// `{ solve_model, t_end, dt }` payload, the addon simulates it. Returns the
// same JSON string shape as `simulate_model`.
async function simulateViaDiffsol(source, model, tEnd, dt) {
    if (typeof lower_model_to_solve_json !== 'function') {
        throw new Error('this build cannot lower models for the diffsol addon');
    }
    let addon;
    try {
        addon = await loadDiffsolAddon();
    } catch (err) {
        throw new Error(
            'the stiff (BDF/diffsol) solver needs a browser with WebAssembly '
            + 'relaxed-SIMD (Chrome 114+, Firefox 120+, Safari 16.4+). '
            + `Use RK45 instead. (${err && err.message ? err.message : err})`,
        );
    }
    const prep = lower_model_to_solve_json(source, model, tEnd, dt);
    return addon.simulate_solve_model_diffsol(prep);
}

function canUseSharedWasmThreads() {
    return typeof self.crossOriginIsolated === 'boolean'
        && self.crossOriginIsolated
        && typeof SharedArrayBuffer !== 'undefined';
}

async function loadWasmModule() {
    if (wasmModuleLoaded) return;
    const mod = await import(withCacheBust('./rumoca_bind_wasm.js'));
    init = mod.default;
    wasm_init = mod.wasm_init;
    get_version = mod.get_version;
    get_builtin_targets = mod.get_builtin_targets;
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
    render_target = mod.render_target;
    project_get_simulation_config = mod.project_get_simulation_config;
    project_set_simulation_preset = mod.project_set_simulation_preset;
    project_reset_simulation_preset = mod.project_reset_simulation_preset;
    project_get_visualization_config = mod.project_get_visualization_config;
    project_set_visualization_config = mod.project_set_visualization_config;
    project_get_scenario_config = mod.project_get_scenario_config;
    project_get_scenario_config_full = mod.project_get_scenario_config_full;
    project_set_scenario_config = mod.project_set_scenario_config;
    project_default_scenario_config = mod.project_default_scenario_config;
    if (typeof mod.simulate_model === 'function') {
        simulate_model = mod.simulate_model;
    }
    if (typeof mod.simulate_model_with_project_sources === 'function') {
        simulate_model_with_project_sources = mod.simulate_model_with_project_sources;
    }
    if (typeof mod.lower_model_to_solve_json === 'function') {
        lower_model_to_solve_json = mod.lower_model_to_solve_json;
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
    if (message.includes('[WASM]') && message.includes('parsing')) {
        // Extract progress info: "[WASM] wasm::project: parsing 50/500 (10%)"
        const scopeMatch = message.match(/\[WASM\]\s+([^:]+(?:::[^:]+)*):\s+/);
        const match = message.match(/parsing (\d+)\/(\d+) \((\d+)%\)/);
        if (match) {
            self.postMessage({
                id: activeRequestId,
                progress: true,
                kind: 'parse',
                scope: scopeMatch ? scopeMatch[1] : '',
                message,
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
        await init({ module_or_path: withCacheBust('./rumoca_bind_wasm_bg.wasm') });

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
    const { id, action, source, modelName, line, character, daeJson, tEnd, dt } = e.data;

    if (!initialized) {
        self.postMessage({ id, error: 'Worker not initialized' });
        return;
    }

    try {
        let result;
        activeRequestId = id;
        const command = e.data.command || '';
        self.postMessage({
            id,
            progress: true,
            kind: 'request',
            phase: 'start',
            action,
            command,
        });
        switch (action) {
            case 'languageCommand': {
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
                const payload = e.data.payload || {};
                switch (command) {
                    case 'rumoca.project.getSimulationModels':
                        result = get_simulation_models(payload.source || '', payload.defaultModel || '');
                        break;
                    case 'rumoca.project.getSimulationConfig':
                        result = project_get_simulation_config(
                            jsonArg(payload.projectSources),
                            payload.model || '',
                            jsonArg(payload.fallback),
                        );
                        break;
                    case 'rumoca.project.setSimulationPreset':
                        result = project_set_simulation_preset(
                            jsonArg(payload.projectSources),
                            payload.model || '',
                            jsonArg(payload.preset, 'null'),
                        );
                        break;
                    case 'rumoca.project.resetSimulationPreset':
                        result = project_reset_simulation_preset(
                            jsonArg(payload.projectSources),
                            payload.model || '',
                        );
                        break;
                    case 'rumoca.project.getVisualizationConfig':
                        result = project_get_visualization_config(
                            jsonArg(payload.projectSources),
                            payload.model || '',
                        );
                        break;
                    case 'rumoca.project.setVisualizationConfig':
                        result = project_set_visualization_config(
                            jsonArg(payload.projectSources),
                            payload.model || '',
                            jsonArg(payload.views, 'null'),
                        );
                        break;
                    case 'rumoca.project.getScenarioConfig':
                        result = project_get_scenario_config(
                            jsonArg(payload.projectSources),
                            payload.path || payload.uri || '',
                        );
                        break;
                    case 'rumoca.project.getScenarioConfigFull':
                        result = project_get_scenario_config_full(
                            jsonArg(payload.projectSources),
                            payload.path || payload.uri || '',
                        );
                        break;
                    case 'rumoca.project.setScenarioConfig':
                        result = project_set_scenario_config(
                            payload.path || payload.uri || '',
                            jsonArg(payload.config, 'null'),
                        );
                        break;
                    case 'rumoca.project.defaultScenarioConfig':
                        result = project_default_scenario_config(
                            jsonArg(payload.projectSources),
                            payload.model || '',
                        );
                        break;
                    case 'rumoca.project.startSimulation':
                        if (!simulate_model) {
                            throw new Error('Simulation not available in this WASM build. Rebuild with rumoca-sim (diffsol feature enabled).');
                        }
                        if ((payload.solver || '') === 'bdf') {
                            // Stiff path: route to the lazy diffsol addon (the
                            // main module can't simulate bdf — it has no diffsol
                            // runtime). Project-local sources aren't supported on
                            // this path yet.
                            result = await simulateViaDiffsol(
                                payload.source || '',
                                payload.modelName || 'Model',
                                payload.tEnd || 1.0,
                                payload.dt || 0,
                            );
                        } else if (
                            simulate_model_with_project_sources
                            && typeof payload.projectSources === 'string'
                            && payload.projectSources.trim()
                            && payload.projectSources.trim() !== '{}'
                        ) {
                            result = simulate_model_with_project_sources(
                                payload.source || '',
                                payload.modelName || 'Model',
                                payload.projectSources,
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
                const payload = e.data.payload || {};
                switch (command) {
                    case 'rumoca.workspace.getVersion':
                        result = get_version();
                        break;
                    case 'rumoca.workspace.getBuiltinTargets':
                        result = get_builtin_targets();
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
                    case 'rumoca.workspace.renderTarget':
                        result = render_target(
                            payload.daeJson,
                            payload.modelName || 'Model',
                            payload.target || '',
                            payload.manifest || '',
                            payload.templates || '{}',
                        );
                        break;
                    default:
                        throw new Error(`Unknown workspace command: ${command}`);
                }
                break;
            }
            default:
                throw new Error(`Unknown action: ${action}`);
        }
        self.postMessage({
            id,
            progress: true,
            kind: 'request',
            phase: 'finish',
            action,
            command,
        });
        activeRequestId = null;
        if (result instanceof Uint8Array) {
            self.postMessage({ id, success: true, result }, [result.buffer]);
            return;
        }
        self.postMessage({ id, success: true, result });
    } catch (e) {
        activeRequestId = null;
        console.error('[Worker] Error:', e);
        self.postMessage({ id, error: e.message || String(e) });
    }
};
