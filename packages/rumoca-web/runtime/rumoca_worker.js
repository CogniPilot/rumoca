import {
    ensureParsedSourceRootCache,
    normalizeSimulationSolver,
    renderDaeTextWithRuntime,
    renderGalecFilesWithRuntime,
    simulateModelWithRuntime,
} from './rumoca_runtime.js';

// Web Worker for Rumoca WASM with rayon threading support
// This worker runs WASM functions that use Atomics.wait (not allowed on main thread)

// Cache-busting query propagated from worker URL (e.g. rumoca_worker.js?v=...).
const workerUrl = new URL(self.location.href);
const cacheBust = workerUrl.searchParams.get('v') || '';
const withCacheBust = (path) =>
    cacheBust ? `${path}?v=${encodeURIComponent(cacheBust)}` : path;

// WASM scenario-config commands take JSON-string arguments. Accept either an
// already-stringified payload field or a structured object from the main thread.
const jsonArg = (value, emptyDefault = '') =>
    value == null ? emptyDefault : (typeof value === 'string' ? value : JSON.stringify(value));

function hasWorkspaceSources(workspaceSources) {
    if (typeof workspaceSources !== 'string') {
        return false;
    }
    const trimmed = workspaceSources.trim();
    return Boolean(trimmed && trimmed !== '{}');
}

function hasSourceRoots(sourceRoots) {
    if (typeof sourceRoots !== 'string') {
        return false;
    }
    const trimmed = sourceRoots.trim();
    return Boolean(trimmed && trimmed !== '{}');
}

function syncWorkspaceSources(workspaceSources) {
    if (typeof sync_workspace_sources !== 'function') {
        throw new Error('Workspace-source simulation not available in this WASM build.');
    }
    sync_workspace_sources(workspaceSources);
}

let init;
let wasm_init;
let get_version;
let get_builtin_targets;
let compile_to_json;
let compile_with_workspace_sources;
let sync_workspace_sources;
let workspace_effective_source_roots;
let get_source_root_statuses;
let get_simulation_models;
let compile_with_source_roots;
let load_source_roots;
let clear_source_root_cache;
let get_source_root_document_count;
let export_parsed_source_roots_binary;
let merge_parsed_source_roots_binary;
let prime_source_root_completion_cache;
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
let scenario_get_simulation_config;
let scenario_set_simulation_preset;
let scenario_reset_simulation_preset;
let scenario_get_visualization_config;
let scenario_set_visualization_config;
let scenario_get_codegen_config;
let scenario_set_codegen_config;
let scenario_get_source_roots;
let scenario_set_source_roots;
let scenario_get_scenario_config;
let scenario_get_scenario_config_full;
let scenario_set_scenario_config;
let scenario_default_scenario_config;
let simulate_model = null;
let simulate_model_with_workspace_sources = null;
let lower_model_to_solve_json = null;
let model_parameter_metadata = null;
let model_parameter_metadata_with_workspace_sources = null;
let model_parameter_metadata_with_source_roots = null;
let prepare_gpu_simulation = null;
let wasmRuntimeModule = null;
let wasmModuleLoaded = false;
let activeRequestId = null;

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
    compile_with_workspace_sources = mod.compile_with_workspace_sources;
    sync_workspace_sources = mod.sync_workspace_sources;
    workspace_effective_source_roots = mod.workspace_effective_source_roots;
    get_source_root_statuses = mod.get_source_root_statuses;
    get_simulation_models = mod.get_simulation_models;
    compile_with_source_roots = mod.compile_with_source_roots;
    load_source_roots = mod.load_source_roots;
    clear_source_root_cache = mod.clear_source_root_cache;
    get_source_root_document_count = mod.get_source_root_document_count;
    export_parsed_source_roots_binary = mod.export_parsed_source_roots_binary;
    merge_parsed_source_roots_binary = mod.merge_parsed_source_roots_binary;
    prime_source_root_completion_cache = mod.prime_source_root_completion_cache;
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
    scenario_get_simulation_config = mod.scenario_get_simulation_config;
    scenario_set_simulation_preset = mod.scenario_set_simulation_preset;
    scenario_reset_simulation_preset = mod.scenario_reset_simulation_preset;
    scenario_get_visualization_config = mod.scenario_get_visualization_config;
    scenario_set_visualization_config = mod.scenario_set_visualization_config;
    scenario_get_codegen_config = mod.scenario_get_codegen_config;
    scenario_set_codegen_config = mod.scenario_set_codegen_config;
    scenario_get_source_roots = mod.scenario_get_source_roots;
    scenario_set_source_roots = mod.scenario_set_source_roots;
    scenario_get_scenario_config = mod.scenario_get_scenario_config;
    scenario_get_scenario_config_full = mod.scenario_get_scenario_config_full;
    scenario_set_scenario_config = mod.scenario_set_scenario_config;
    scenario_default_scenario_config = mod.scenario_default_scenario_config;
    if (typeof mod.simulate_model === 'function') {
        simulate_model = mod.simulate_model;
    }
    simulate_model_with_workspace_sources = mod.simulate_model_with_workspace_sources;
    if (typeof mod.lower_model_to_solve_json === 'function') {
        lower_model_to_solve_json = mod.lower_model_to_solve_json;
    }
    if (typeof mod.model_parameter_metadata === 'function') {
        model_parameter_metadata = mod.model_parameter_metadata;
    }
    if (typeof mod.model_parameter_metadata_with_workspace_sources === 'function') {
        model_parameter_metadata_with_workspace_sources = mod.model_parameter_metadata_with_workspace_sources;
    }
    if (typeof mod.model_parameter_metadata_with_source_roots === 'function') {
        model_parameter_metadata_with_source_roots = mod.model_parameter_metadata_with_source_roots;
    }
    if (typeof mod.prepare_gpu_simulation === 'function') {
        prepare_gpu_simulation = mod.prepare_gpu_simulation;
    }
    wasmRuntimeModule = {
        compile: compile_to_json,
        lower_model_to_solve_json,
        merge_parsed_source_roots_binary,
        model_parameter_metadata,
        model_parameter_metadata_with_source_roots,
        model_parameter_metadata_with_workspace_sources,
        prime_source_root_completion_cache,
        prepare_gpu_simulation,
        render_target,
        simulate_model,
    };
    wasmModuleLoaded = true;
}

let initialized = false;
const debugWorkerLogs = false;

// Intercept console.log to forward progress messages to main thread
const originalLog = console.log;
console.log = function(...args) {
    if (debugWorkerLogs) {
        originalLog.apply(console, args);
    }
    // Forward WASM progress messages to main thread
    const message = args.join(' ');
    if (message.includes('[WASM]') && message.includes('parsing')) {
        // Extract progress info: "[WASM] wasm::workspace: parsing 50/500 (10%)"
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
        if (debugWorkerLogs) console.log('[Worker] Loading WASM module...');
        await loadWasmModule();
        await init({ module_or_path: withCacheBust('./rumoca_bind_wasm_bg.wasm') });

        const requestedThreads = navigator.hardwareConcurrency || 4;
        const numThreads = canUseSharedWasmThreads() ? requestedThreads : 0;
        if (numThreads > 0) {
            if (debugWorkerLogs) console.log('[Worker] Initializing thread pool...');
        } else {
            if (debugWorkerLogs) console.warn('[Worker] Shared WASM threads unavailable; using single-thread mode.');
        }
        await wasm_init(numThreads);
        if (numThreads > 0) {
            if (debugWorkerLogs) console.log(`[Worker] Thread pool initialized with ${numThreads} threads`);
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
                if (typeof sync_workspace_sources === 'function' && typeof payload.workspaceSources === 'string') {
                    sync_workspace_sources(payload.workspaceSources);
                }
                switch (command) {
                    case 'rumoca.language.getSourceRootDocumentCount':
                        result = get_source_root_document_count();
                        break;
                    case 'rumoca.language.diagnostics':
                        await ensureParsedSourceRootCache(
                            wasmRuntimeModule,
                            payload.sourceRootCacheUrl || '',
                        );
                        result = lsp_diagnostics(payload.source || '');
                        break;
                    case 'rumoca.language.hover':
                        result = lsp_hover(payload.source || '', payload.line, payload.character);
                        break;
                    case 'rumoca.language.completion':
                        await ensureParsedSourceRootCache(
                            wasmRuntimeModule,
                            payload.sourceRootCacheUrl || '',
                        );
                        result = lsp_completion(payload.source || '', payload.line, payload.character);
                        break;
                    case 'rumoca.language.completionWithTiming':
                        await ensureParsedSourceRootCache(
                            wasmRuntimeModule,
                            payload.sourceRootCacheUrl || '',
                        );
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
            case 'scenarioCommand': {
                const payload = e.data.payload || {};
                switch (command) {
                    case 'rumoca.scenario.getSimulationModels':
                        result = get_simulation_models(payload.source || '', payload.defaultModel || '');
                        break;
                    case 'rumoca.scenario.getSimulationConfig':
                        result = scenario_get_simulation_config(
                            jsonArg(payload.workspaceSources),
                            payload.model || '',
                            jsonArg(payload.fallback),
                        );
                        break;
                    case 'rumoca.scenario.setSimulationPreset':
                        result = scenario_set_simulation_preset(
                            jsonArg(payload.workspaceSources),
                            payload.model || '',
                            jsonArg(payload.preset, 'null'),
                        );
                        break;
                    case 'rumoca.scenario.resetSimulationPreset':
                        result = scenario_reset_simulation_preset(
                            jsonArg(payload.workspaceSources),
                            payload.model || '',
                        );
                        break;
                    case 'rumoca.scenario.getVisualizationConfig':
                        result = scenario_get_visualization_config(
                            jsonArg(payload.workspaceSources),
                            payload.model || '',
                        );
                        break;
                    case 'rumoca.scenario.setVisualizationConfig':
                        result = scenario_set_visualization_config(
                            jsonArg(payload.workspaceSources),
                            payload.model || '',
                            jsonArg(payload.views, 'null'),
                        );
                        break;
                    case 'rumoca.scenario.getCodegenConfig':
                        result = scenario_get_codegen_config(
                            jsonArg(payload.workspaceSources),
                            payload.model || '',
                        );
                        break;
                    case 'rumoca.scenario.setCodegenConfig':
                        result = scenario_set_codegen_config(
                            jsonArg(payload.workspaceSources),
                            payload.model || '',
                            jsonArg(payload.config, 'null'),
                        );
                        break;
                    case 'rumoca.scenario.getSourceRoots':
                        result = scenario_get_source_roots(
                            jsonArg(payload.workspaceSources),
                            payload.model || '',
                            payload.task || '',
                        );
                        break;
                    case 'rumoca.scenario.setSourceRoots':
                        result = scenario_set_source_roots(
                            jsonArg(payload.workspaceSources),
                            payload.model || '',
                            jsonArg(payload.config, 'null'),
                        );
                        break;
                    case 'rumoca.scenario.getScenarioConfig':
                        result = scenario_get_scenario_config(
                            jsonArg(payload.workspaceSources),
                            payload.path || payload.uri || '',
                        );
                        break;
                    case 'rumoca.scenario.getScenarioConfigFull':
                        result = scenario_get_scenario_config_full(
                            jsonArg(payload.workspaceSources),
                            payload.path || payload.uri || '',
                        );
                        break;
                    case 'rumoca.scenario.setScenarioConfig':
                        result = scenario_set_scenario_config(
                            payload.path || payload.uri || '',
                            jsonArg(payload.config, 'null'),
                        );
                        break;
                    case 'rumoca.scenario.defaultScenarioConfig':
                        result = scenario_default_scenario_config(
                            jsonArg(payload.workspaceSources),
                            payload.model || '',
                            payload.task || '',
                        );
                        break;
                    case 'rumoca.scenario.startSimulation':
                        if (!simulate_model) {
                            throw new Error('Simulation not available in this WASM build. Rebuild with rumoca-sim (diffsol feature enabled).');
                        }
                        if (typeof payload.sourceRoots === 'string' && payload.sourceRoots.trim() && payload.sourceRoots.trim() !== '{}') {
                            load_source_roots(payload.sourceRoots);
                        }
                        {
                            const solver = normalizeSimulationSolver(payload.solver);
                            const useWorkspaceSources = hasWorkspaceSources(payload.workspaceSources);
                            const parameterOverrides = payload.parameterOverrides && typeof payload.parameterOverrides === 'object'
                                ? payload.parameterOverrides
                                : {};
                            if (useWorkspaceSources && solver === 'bdf') {
                                syncWorkspaceSources(payload.workspaceSources);
                                result = JSON.stringify(await simulateModelWithRuntime({
                                    wasm: wasmRuntimeModule,
                                    pkgBase: './',
                                    source: payload.source || '',
                                    modelName: payload.modelName || 'Model',
                                    tEnd: payload.tEnd || 1.0,
                                    dt: payload.dt || 0,
                                    solver,
                                    sourceRootCacheUrl: payload.sourceRootCacheUrl || '',
                                    parameterOverrides,
                                }));
                            } else if (useWorkspaceSources) {
                                if (typeof simulate_model_with_workspace_sources !== 'function') {
                                    throw new Error('Workspace-source simulation not available in this WASM build.');
                                }
                                result = simulate_model_with_workspace_sources(
                                    payload.source || '',
                                    payload.modelName || 'Model',
                                    payload.workspaceSources,
                                    payload.tEnd || 1.0,
                                    payload.dt || 0,
                                    solver,
                                    jsonArg(parameterOverrides, '{}'),
                                );
                            } else {
                                result = JSON.stringify(await simulateModelWithRuntime({
                                    wasm: wasmRuntimeModule,
                                    pkgBase: './',
                                    source: payload.source || '',
                                    modelName: payload.modelName || 'Model',
                                    tEnd: payload.tEnd || 1.0,
                                    dt: payload.dt || 0,
                                    solver,
                                    sourceRootCacheUrl: payload.sourceRootCacheUrl || '',
                                    parameterOverrides,
                                }));
                            }
                        }
                        break;
                    case 'rumoca.model.parameterMetadata':
                        if (hasSourceRoots(payload.sourceRoots)) {
                            if (typeof load_source_roots !== 'function') {
                                throw new Error('Source-root parameter metadata is not available in this WASM build.');
                            }
                            load_source_roots(payload.sourceRoots);
                        }
                        if (hasWorkspaceSources(payload.workspaceSources)) {
                            if (typeof model_parameter_metadata_with_workspace_sources !== 'function') {
                                throw new Error('Workspace parameter metadata is not available in this WASM build.');
                            }
                            result = model_parameter_metadata_with_workspace_sources(
                                payload.source || '',
                                payload.modelName || 'Model',
                                payload.workspaceSources,
                            );
                        } else if (hasSourceRoots(payload.sourceRoots)) {
                            if (typeof model_parameter_metadata_with_source_roots !== 'function') {
                                throw new Error('Source-root parameter metadata is not available in this WASM build.');
                            }
                            result = model_parameter_metadata_with_source_roots(
                                payload.source || '',
                                payload.modelName || 'Model',
                                payload.sourceRoots,
                            );
                        } else {
                            if (typeof model_parameter_metadata !== 'function') {
                                throw new Error('Parameter metadata is not available in this WASM build.');
                            }
                            result = model_parameter_metadata(
                                payload.source || '',
                                payload.modelName || 'Model',
                            );
                        }
                        break;
                    case 'rumoca.scenario.renderDaeText':
                        result = await renderDaeTextWithRuntime({
                            wasm: wasmRuntimeModule,
                            source: payload.source || '',
                            modelName: payload.modelName || 'Model',
                            sourceRootCacheUrl: payload.sourceRootCacheUrl || '',
                        });
                        break;
                    case 'rumoca.scenario.prepareGpuSimulation':
                        await ensureParsedSourceRootCache(
                            wasmRuntimeModule,
                            payload.sourceRootCacheUrl || '',
                        );
                        if (typeof payload.source !== 'string') {
                            throw new Error('prepareGpuSimulation requires source text');
                        }
                        if (typeof prepare_gpu_simulation !== 'function') {
                            throw new Error('prepare_gpu_simulation missing in this WASM build');
                        }
                        result = prepare_gpu_simulation(
                            payload.source || '',
                            payload.modelName || 'Model',
                        );
                        break;
                    default:
                        throw new Error(`Unknown scenario command: ${command}`);
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
                    case 'rumoca.workspace.compileWithWorkspaceSources':
                        result = compile_with_workspace_sources(
                            payload.source || '',
                            payload.modelName || 'Model',
                            payload.workspaceSources || '{}',
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
                    case 'rumoca.workspace.effectiveSourceRoots':
                        if (typeof workspace_effective_source_roots !== 'function') {
                            throw new Error('workspace_effective_source_roots missing in this WASM build');
                        }
                        result = workspace_effective_source_roots(
                            payload.workspaceSources || '{}',
                            payload.focusPath || payload.path || '',
                        );
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
                    case 'rumoca.workspace.renderGalec':
                        // GALEC codegen ('galec' / 'efmi' /
                        // 'galec-c') is served by the SEPARATE, lazily
                        // imported GALEC addon — never the core render_target
                        // (DAE-JSON) path, which drops the flat model the
                        // projection needs. The addon wasm loads only on this
                        // command, so the core module stays untouched.
                        result = {
                            ok: true,
                            files: await renderGalecFilesWithRuntime({
                                pkgBase: './',
                                workspaceSources: payload.workspaceSources || '{}',
                                modelName: payload.modelName || 'Model',
                                target: payload.target || '',
                            }),
                        };
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
        self.postMessage({ id, error: e.message || String(e) });
    }
};
