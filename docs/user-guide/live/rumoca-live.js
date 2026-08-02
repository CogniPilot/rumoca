// Rumoca live example runner for the mdBook guides.
//
// Upgrades fenced code blocks marked `modelica,interactive` into mini Monaco
// editor windows backed by the Rumoca WASM package: syntax highlighting,
// LSP completion/hover/diagnostics, one-click simulation with an inline plot,
// and a "Show DAE" view of the flattened system. Blocks marked
// `modelica,codegen` use a smaller GALEC code-generation widget.
//
// Loading strategy:
//   - Monaco loads from generated local vendor assets when a page contains at
//     least one interactive block. If those assets are absent, the widget falls
//     back to a plain editable textarea.
//   - The Rumoca WASM package loads lazily on first interaction (editor
//     focus or a toolbar click), so reading a page stays cheap.
//
// Page layout assumptions (see dev-guide "Docs and Pages"):
//   GitHub Pages:  /user-guide/...  with the package at /pkg/<subdir>/ and
//                  the playground modules at /src/modules/
//   local serve:   serve the repository root; books live at docs/<book>/book/
// A page can override package discovery with `window.RUMOCA_LIVE_PKG_BASE`.
(function () {
    'use strict';

    const PKG_SUBDIRS = ['release-full-web', 'release-full-web-rayon'];
    const LOCAL_PKG_SUBDIRS = ['dev-full-web'];
    const LANGUAGE_MODULE_CANDIDATES = [
        // GitHub Pages: staged runtime package next to the deployed book.
        '../pkg/release-full-web/modelica_language.js',
        '../pkg/release-full-web-rayon/modelica_language.js',
        // Local: repository root served directly, book at docs/<book>/book/.
        '../../../packages/rumoca/dist/release-full-web/modelica_language.js',
        '../../../packages/rumoca/dist/release-full-web-rayon/modelica_language.js',
        '../../../packages/rumoca-web/runtime/modelica_language.js',
    ];
    const SERIES_COLORS = [
        '#2470c2', '#d94f30', '#2c9462', '#9356c8', '#c8842c', '#3aa0ab',
    ];
    const GALEC_CODEGEN_TARGETS = new Set(['galec', 'efmi', 'galec-c']);
    const MAX_PLOT_SERIES = 6;
    const MAX_EDITOR_LINES = 28;
    const DIAGNOSTIC_DEBOUNCE_MS = 400;

    let monacoPromise = null;
    let wasmModulePromise = null;
    let wasmRequested = false;
    let languageServicesRegistered = false;
    let galecLanguageRegistered = false;
    let galecLanguageServicesRegistered = false;
    // Resolved pkg base URL (where rumoca_bind_wasm.js loaded from); the diffsol
    // driver + addon sit next to it. Cached the first time the WASM is loaded.
    let resolvedPkgBase = null;
    // Cached promise for the diffsol driver module (rumoca_diffsol.js).
    let diffsolDriverPromise = null;
    // Cached promise for the shared browser runtime module from rumoca-web.
    let runtimeDriverPromise = null;
    // Cached promise for the shared browser interactive runtime module.
    let interactiveRuntimePromise = null;
    // Cached promise for the WebGPU RK4 driver module (rumoca_gpu.js).
    let gpuDriverPromise = null;
    // Cached promise for the local Three.js bundle used by custom guide viz.
    let threeModulePromise = null;
    // Cached promise for shared browser UI helpers from rumoca-web.
    let visualizationSharedPromise = null;
    // Cached resolver for docs-staged parsed Modelica source-root caches.
    let sourceRootCacheResolverPromise = null;
    let monacoThemeObserver = null;
    const monacoEditors = new Set();
    const monacoModelWidgets = new WeakMap();
    const galecModelFileNames = new WeakMap();
    const galecDiagnosticsState = new WeakMap();
    const widgets = [];
    const scenarioConfigHostHandlers = new Map();

    window.RumocaScenarioConfigHost = {
        async request(method, payload = {}) {
            const path = trimMaybeString(payload?.path) || 'rumoca-scenario.toml';
            const handler = scenarioConfigHostHandlers.get(path);
            if (!handler) {
                throw new Error(`No scenario config host is registered for ${path}`);
            }
            return await handler(method, payload);
        },
    };

    function bookRoot() {
        // mdBook defines `path_to_root` as a top-level const in every page.
        /* global path_to_root */
        return typeof path_to_root !== 'undefined' ? path_to_root : './';
    }

    function pageUrl(relative) {
        return new URL(relative, window.location.href).href;
    }

    function isLocalPreviewHost() {
        return /^(localhost|127\.0\.0\.1|\[::1\])$/.test(window.location.hostname);
    }

    function trimMaybeString(value) {
        return typeof value === 'string' ? value.trim() : '';
    }

    function normalizeStringArray(values) {
        return Array.isArray(values)
            ? values.map(trimMaybeString).filter(Boolean)
            : [];
    }

    function uniqueStrings(values) {
        const seen = new Set();
        const result = [];
        for (const value of normalizeStringArray(values)) {
            if (!seen.has(value)) {
                seen.add(value);
                result.push(value);
            }
        }
        return result;
    }

    function encodeUrlPath(path) {
        return String(path || '').split('/').map(encodeURIComponent).join('/');
    }

    // ---------------------------------------------------------------------
    // WASM package loading
    // ---------------------------------------------------------------------

    function pkgBaseCandidates() {
        if (window.RUMOCA_LIVE_PKG_BASE) {
            return [window.RUMOCA_LIVE_PKG_BASE];
        }
        const root = bookRoot();
        const layouts = [root + '../pkg/', root + '../../../packages/rumoca/dist/'];
        const subdirs = isLocalPreviewHost() ? [...LOCAL_PKG_SUBDIRS, ...PKG_SUBDIRS] : PKG_SUBDIRS;
        const candidates = [];
        for (const layout of layouts) {
            for (const subdir of subdirs) {
                candidates.push(layout + subdir + '/');
            }
        }
        return candidates;
    }

    async function locatePkgBase() {
        for (const base of pkgBaseCandidates()) {
            const probe = pageUrl(base + 'rumoca_bind_wasm.js');
            try {
                const response = await fetch(probe, { method: 'HEAD' });
                if (response.ok) {
                    return probe.replace(/rumoca_bind_wasm\.js$/, '');
                }
            } catch (_error) {
                // Try the next layout.
            }
        }
        throw new Error(
            'Rumoca WASM package not found next to this book. ' +
            'Live examples work on the published site ' +
            '(https://cognipilot.github.io/rumoca/user-guide/) or through ' +
            'the local preview command `cargo xtask docs serve`.'
        );
    }

    function monacoBaseCandidates() {
        if (window.RUMOCA_LIVE_MONACO_BASE) {
            return [window.RUMOCA_LIVE_MONACO_BASE];
        }
        const root = bookRoot();
        return [
            root + '../vendor/monaco/',
            root + '../../../vendor/monaco/',
            root + '../../../packages/rumoca-web/vendor/monaco/',
            root + '../../../packages/playground/vendor/monaco/',
        ];
    }

    async function locateMonacoBase() {
        for (const base of monacoBaseCandidates()) {
            const probe = pageUrl(base + 'vs/loader.js');
            try {
                const response = await fetch(probe, { method: 'HEAD' });
                if (response.ok) {
                    return probe.replace(/vs\/loader\.js$/, '');
                }
            } catch (_error) {
                // Try the next layout.
            }
        }
        throw new Error('Monaco vendor assets not found');
    }

    function threeModuleCandidates() {
        if (window.RUMOCA_LIVE_THREE_MODULE) {
            return [window.RUMOCA_LIVE_THREE_MODULE];
        }
        const root = bookRoot();
        return [
            root + '../vendor/three_viewer.js',
            root + '../../../vendor/three_viewer.js',
            root + '../../../packages/rumoca-web/vendor/three_viewer.js',
        ];
    }

    async function locateThreeModule() {
        for (const candidate of threeModuleCandidates()) {
            const probe = pageUrl(candidate);
            try {
                const response = await fetch(probe, { method: 'HEAD' });
                if (response.ok) {
                    return probe;
                }
            } catch (_error) {
                // Try the next layout.
            }
        }
        throw new Error('Three.js vendor module not found');
    }

    function loadThreeModule() {
        if (!threeModulePromise) {
            threeModulePromise = locateThreeModule()
                .then((url) => import(url))
                .catch((error) => {
                    threeModulePromise = null;
                    throw error;
                });
        }
        return threeModulePromise;
    }

    function visualizationSharedCandidates() {
        const root = bookRoot();
        return [
            root + '../vendor/visualization_shared.js',
            root + '../../../vendor/visualization_shared.js',
            root + '../../../packages/rumoca-web/viz/visualization_shared.js',
            root + '../../../packages/rumoca-web/vendor/visualization_shared.js',
            root + '../../../packages/playground/vendor/visualization_shared.js',
        ];
    }

    async function locateVisualizationShared() {
        for (const candidate of visualizationSharedCandidates()) {
            const probe = pageUrl(candidate);
            try {
                const response = await fetch(probe, { method: 'HEAD' });
                if (response.ok) {
                    return probe;
                }
            } catch (_error) {
                // Try the next layout.
            }
        }
        throw new Error('Rumoca shared scenario GUI module not found');
    }

    function loadVisualizationShared() {
        if (!visualizationSharedPromise) {
            visualizationSharedPromise = locateVisualizationShared()
                .then((url) => import(url))
                .catch((error) => {
                    visualizationSharedPromise = null;
                    throw error;
                });
        }
        return visualizationSharedPromise;
    }

    function loadWasm() {
        if (!wasmModulePromise) {
            wasmRequested = true;
            broadcastStatus('Downloading Rumoca WASM (first use on this page)…');
            wasmModulePromise = (async () => {
                const base = await locatePkgBase();
                resolvedPkgBase = base;
                const module = await import(base + 'rumoca_bind_wasm.js');
                await module.default();
                return module;
            })();
            wasmModulePromise.then(
                (wasm) => {
                    broadcastStatus('');
                    registerLanguageServices(wasm);
                    for (const widget of widgets) {
                        widget.onWasmReady(wasm);
                    }
                },
                () => {
                    // Allow a retry after a failed load.
                    wasmModulePromise = null;
                    wasmRequested = false;
                    broadcastStatus('Rumoca WASM failed to load — click Simulate to retry.');
                }
            );
        }
        return wasmModulePromise;
    }

    function loadRuntimeDriver() {
        if (!runtimeDriverPromise) {
            runtimeDriverPromise = (async () => {
                const base = resolvedPkgBase || await locatePkgBase();
                const packageRuntime = base + 'rumoca_runtime.js';
                const sourceRuntime = pageUrl(bookRoot() + '../../../packages/rumoca-web/runtime/rumoca_runtime.js');
                const localPreview = isLocalPreviewHost();
                const candidates = localPreview
                    ? [sourceRuntime, packageRuntime]
                    : [packageRuntime, sourceRuntime];
                let lastError = null;
                for (const candidate of candidates) {
                    try {
                        const url = new URL(candidate, window.location.href);
                        url.searchParams.set('rumoca_live_runtime', String(Date.now()));
                        return await import(url.href);
                    } catch (error) {
                        lastError = error;
                    }
                }
                throw lastError || new Error('Rumoca runtime module not found');
            })().catch((error) => {
                runtimeDriverPromise = null;
                throw error;
            });
        }
        return runtimeDriverPromise;
    }

    function loadInteractiveRuntime() {
        if (!interactiveRuntimePromise) {
            interactiveRuntimePromise = (async () => {
                const base = resolvedPkgBase || await locatePkgBase();
                const packageRuntime = base + 'rumoca_interactive.js';
                const sourceRuntime = pageUrl(bookRoot() + '../../../packages/rumoca-web/runtime/rumoca_interactive.js');
                const localPreview = isLocalPreviewHost();
                const candidates = localPreview
                    ? [sourceRuntime, packageRuntime]
                    : [packageRuntime, sourceRuntime];
                let lastError = null;
                for (const candidate of candidates) {
                    try {
                        const url = new URL(candidate, window.location.href);
                        url.searchParams.set('rumoca_live_interactive', String(Date.now()));
                        return await import(url.href);
                    } catch (error) {
                        lastError = error;
                    }
                }
                throw lastError || new Error('Rumoca interactive runtime module not found');
            })().catch((error) => {
                interactiveRuntimePromise = null;
                throw error;
            });
        }
        return interactiveRuntimePromise;
    }

    function sourceRootManifestUrl() {
        return pageUrl(bookRoot() + 'repo-examples/source-roots/manifest.json');
    }

    async function loadSourceRootCacheResolver() {
        if (!sourceRootCacheResolverPromise) {
            sourceRootCacheResolverPromise = loadRuntimeDriver()
                .then((runtime) =>
                    runtime.createSourceRootCacheResolver({
                        manifestUrl: sourceRootManifestUrl(),
                    })
                )
                .catch((error) => {
                    sourceRootCacheResolverPromise = null;
                    throw error;
                });
        }
        return sourceRootCacheResolverPromise;
    }

    async function loadSourceRootCacheUrlForRoots(sourceRoots) {
        if (sourceRoots.length === 0) {
            return '';
        }
        const resolver = await loadSourceRootCacheResolver();
        return resolver.cacheUrlFor(sourceRoots);
    }

    function repoExamplesRootUrl() {
        return pageUrl(bookRoot() + 'repo-examples/');
    }

    function repoExamplesRelativePath(urlOrPath) {
        const root = new URL(repoExamplesRootUrl());
        const absolute = new URL(urlOrPath, window.location.href);
        if (absolute.origin !== root.origin || !absolute.pathname.startsWith(root.pathname)) {
            return '';
        }
        return decodeURIComponent(absolute.pathname.slice(root.pathname.length));
    }

    function workspaceConfigPathsForFocus(focusPath) {
        const parts = trimMaybeString(focusPath).split('/').filter(Boolean);
        if (parts.length === 0) {
            return [];
        }
        parts.pop();
        const paths = ['rumoca-workspace.toml'];
        let prefix = '';
        for (const part of parts) {
            prefix = prefix ? `${prefix}/${part}` : part;
            paths.push(`${prefix}/rumoca-workspace.toml`);
        }
        return paths;
    }

    async function fetchWorkspaceConfig(path) {
        const response = await fetch(new URL(encodeUrlPath(path), repoExamplesRootUrl()).href);
        if (response.status === 404) {
            return null;
        }
        if (!response.ok) {
            throw new Error(`Workspace config is unavailable (${response.status}): ${path}`);
        }
        return response.text();
    }

    async function loadWorkspaceSourcesForFocus(focusPath) {
        const sources = {};
        for (const path of workspaceConfigPathsForFocus(focusPath)) {
            const text = await fetchWorkspaceConfig(path);
            if (text !== null) {
                sources[path] = text;
            }
        }
        return sources;
    }

    // Lazily import the diffsol driver (rumoca_diffsol.js), which sits next to
    // the main WASM. It feature-detects relaxed-SIMD and lazy-loads the separate
    // diffsol addon. Returns null if unavailable (older package without it).
    function loadDiffsolDriver() {
        if (!diffsolDriverPromise) {
            diffsolDriverPromise = loadRuntimeDriver()
                .catch(() => {
                    diffsolDriverPromise = null;
                    return null;
                });
        }
        return diffsolDriverPromise || Promise.resolve(null);
    }

    // Lazily import the WebGPU RK4 driver (rumoca_gpu.js), the canonical
    // packaged GPU helper that sits next to the main WASM and is also published
    // as `@cognipilot/rumoca/gpu`. Exposes probeGpu/buildGpuProgram/
    // runGpuSimulation. Returns null if unavailable (older package without it).
    function loadGpuDriver() {
        if (!gpuDriverPromise && resolvedPkgBase) {
            const url = new URL(resolvedPkgBase + 'rumoca_gpu.js', window.location.href);
            url.searchParams.set('rumoca_live_gpu', String(Date.now()));
            gpuDriverPromise = import(url.href)
                .catch(() => {
                    gpuDriverPromise = null;
                    return null;
                });
        }
        return gpuDriverPromise || Promise.resolve(null);
    }

    // -----------------------------------------------------------------
    // Simulation worker
    //
    // simulate_model and DAE rendering are synchronous WASM calls that can
    // take tens of seconds on large discretized models (the NACA airfoil
    // page). Running them in a module worker keeps the page responsive so
    // the progress bar can animate. Falls back to main-thread calls when
    // workers are unavailable.
    // -----------------------------------------------------------------

    let simWorkerPromise = null;

    function buildSimWorkerSource(pkgBase) {
        return `
import init, * as rumoca from '${pkgBase}rumoca_bind_wasm.js';
import {
    ensureParsedSourceRootCache,
    prepareGpuSimulationWithRuntime,
    renderDaeTextWithRuntime,
    renderGalecFilesWithRuntime,
    simulateModelWithRuntime,
} from '${pkgBase}rumoca_runtime.js';
const GALEC_CODEGEN_TARGETS = new Set(['galec', 'efmi', 'galec-c']);
const ready = init();
self.onmessage = async (event) => {
    const { id, action, args } = event.data;
    try {
        await ready;
        let result;
        if (action === 'simulate') {
            result = JSON.stringify(await simulateModelWithRuntime({
                wasm: rumoca,
                pkgBase: '${pkgBase}',
                source: args.source,
                modelName: args.model,
                tEnd: args.tEnd || 0,
                dt: args.dt || 0,
                solver: args.solver || 'auto',
                sourceRootCacheUrl: args.sourceRootCacheUrl || '',
                parameterOverrides: args.parameterOverrides || {},
            }));
        } else if (action === 'prepare_gpu') {
            result = await prepareGpuSimulationWithRuntime({
                wasm: rumoca,
                source: args.source,
                modelName: args.model,
                sourceRootCacheUrl: args.sourceRootCacheUrl || '',
            });
        } else if (action === 'update_gpu') {
            if (typeof rumoca.update_gpu_parameters !== 'function') {
                throw new Error('update_gpu_parameters missing in this WASM build');
            }
            result = rumoca.update_gpu_parameters(args.source, args.model, args.overrides);
        } else if (action === 'dae') {
            result = await renderDaeTextWithRuntime({
                wasm: rumoca,
                source: args.source,
                modelName: args.model,
                sourceRootCacheUrl: args.sourceRootCacheUrl || '',
            });
        } else if (action === 'codegen') {
            const target = args.target || '';
            if (GALEC_CODEGEN_TARGETS.has(target)) {
                result = JSON.stringify(await renderGalecFilesWithRuntime({
                    pkgBase: '${pkgBase}',
                    workspaceSources: args.workspaceSources || '{}',
                    modelName: args.model,
                    target,
                }));
            } else {
                if (!/^[A-Za-z0-9_.-]+$/.test(target)) {
                    throw new Error('The book widget supports built-in codegen targets only.');
                }
                await ensureParsedSourceRootCache(rumoca, args.sourceRootCacheUrl || '');
                if (typeof rumoca.compile !== 'function' || typeof rumoca.render_target !== 'function') {
                    throw new Error('code generation APIs are missing in this WASM build');
                }
                const compiled = JSON.parse(rumoca.compile(args.source || '', args.model || 'Model'));
                const daeJson = JSON.stringify(compiled.dae_native || compiled.dae);
                const rendered = rumoca.render_target(
                    daeJson,
                    args.model || 'Model',
                    target,
                    '',
                    '{}',
                );
                result = JSON.stringify(Array.isArray(rendered?.files) ? rendered.files : []);
            }
        } else if (action === 'rumoca.model.parameterMetadata') {
            await ensureParsedSourceRootCache(rumoca, args.sourceRootCacheUrl || '');
            if (typeof rumoca.model_parameter_metadata !== 'function') {
                throw new Error('model_parameter_metadata missing in this WASM build');
            }
            result = rumoca.model_parameter_metadata(args.source || '', args.modelName || '');
        } else if (action === 'diagnostics') {
            await ensureParsedSourceRootCache(rumoca, args.sourceRootCacheUrl || '');
            if (typeof rumoca.lsp_diagnostics !== 'function') {
                throw new Error('lsp_diagnostics missing in this WASM build');
            }
            result = rumoca.lsp_diagnostics(args.source || '');
        } else if (action === 'completion') {
            await ensureParsedSourceRootCache(rumoca, args.sourceRootCacheUrl || '');
            if (typeof rumoca.lsp_completion !== 'function') {
                throw new Error('lsp_completion missing in this WASM build');
            }
            result = rumoca.lsp_completion(
                args.source || '',
                args.line || 0,
                args.character || 0
            );
        } else if (action === 'hover') {
            await ensureParsedSourceRootCache(rumoca, args.sourceRootCacheUrl || '');
            if (typeof rumoca.lsp_hover !== 'function') {
                throw new Error('lsp_hover missing in this WASM build');
            }
            result = rumoca.lsp_hover(
                args.source || '',
                args.line || 0,
                args.character || 0
            );
        } else {
            throw new Error('unknown action ' + action);
        }
        self.postMessage({ id, ok: true, result });
    } catch (error) {
        self.postMessage({ id, ok: false, error: String((error && error.message) || error) });
    }
};
`;
    }

    function makeAbortError(message = 'Stopped') {
        if (typeof DOMException === 'function') {
            return new DOMException(message, 'AbortError');
        }
        const error = new Error(message);
        error.name = 'AbortError';
        return error;
    }

    function isAbortError(error) {
        return error && (error.name === 'AbortError' || error.message === 'Stopped');
    }

    function throwIfAborted(signal) {
        if (signal?.aborted) {
            throw makeAbortError();
        }
    }

    function loadSimWorker() {
        if (!simWorkerPromise) {
            simWorkerPromise = (async () => {
                const base = await locatePkgBase();
                const blob = new Blob([buildSimWorkerSource(base)], { type: 'text/javascript' });
                const worker = new Worker(URL.createObjectURL(blob), { type: 'module' });
                const pending = new Map();
                let nextId = 1;
                const failAll = (failure) => {
                    for (const entry of pending.values()) {
                        if (entry.onAbort && entry.signal) {
                            entry.signal.removeEventListener('abort', entry.onAbort);
                        }
                        entry.reject(failure);
                    }
                    pending.clear();
                };
                worker.onmessage = (event) => {
                    const { id, ok, result, error } = event.data;
                    const entry = pending.get(id);
                    if (!entry) {
                        return;
                    }
                    pending.delete(id);
                    if (entry.onAbort && entry.signal) {
                        entry.signal.removeEventListener('abort', entry.onAbort);
                    }
                    if (ok) {
                        entry.resolve(result);
                    } else {
                        entry.reject(new Error(error));
                    }
                };
                worker.onerror = (event) => {
                    const failure = new Error(event.message || 'simulation worker failed');
                    failAll(failure);
                };
                return {
                    request(action, args, signal) {
                        throwIfAborted(signal);
                        return new Promise((resolve, reject) => {
                            const id = nextId++;
                            const onAbort = () => {
                                pending.delete(id);
                                worker.terminate();
                                simWorkerPromise = null;
                                reject(makeAbortError());
                                failAll(makeAbortError());
                            };
                            if (signal) {
                                signal.addEventListener('abort', onAbort, { once: true });
                            }
                            pending.set(id, { resolve, reject, signal, onAbort });
                            worker.postMessage({ id, action, args });
                        });
                    },
                    terminate() {
                        worker.terminate();
                        simWorkerPromise = null;
                        failAll(makeAbortError());
                    },
                };
            })();
            simWorkerPromise.catch(() => {
                simWorkerPromise = null;
            });
        }
        return simWorkerPromise;
    }

    // Run a heavy action in the worker, falling back to the main thread.
    async function runHeavy(action, args, mainThreadFallback, signal) {
        throwIfAborted(signal);
        try {
            const worker = await loadSimWorker();
            const result = await worker.request(action, args, signal);
            throwIfAborted(signal);
            return result;
        } catch (error) {
            if (isAbortError(error)) {
                throw error;
            }
            throwIfAborted(signal);
            console.warn(`rumoca-live: worker path failed for ${action}, using main thread:`, error);
            const result = await mainThreadFallback();
            throwIfAborted(signal);
            return result;
        }
    }

    function broadcastStatus(text) {
        for (const widget of widgets) {
            if (!widget.busy) {
                widget.setStatus(text);
            }
        }
    }

    // ---------------------------------------------------------------------
    // Monaco loading
    // ---------------------------------------------------------------------

    function injectScript(src) {
        return new Promise((resolve, reject) => {
            const script = document.createElement('script');
            script.src = src;
            script.onload = resolve;
            script.onerror = () => reject(new Error(`Failed to load ${src}`));
            document.head.appendChild(script);
        });
    }

    async function loadModelicaLanguage(monaco) {
        const root = bookRoot();
        for (const candidate of LANGUAGE_MODULE_CANDIDATES) {
            try {
                const module = await import(pageUrl(root + candidate));
                module.registerModelicaLanguage(monaco);
                return;
            } catch (_error) {
                // Try the next layout.
            }
        }
        // Minimal fallback so editors still open with comment support.
        if (!monaco.languages.getLanguages().some((lang) => lang.id === 'modelica')) {
            monaco.languages.register({ id: 'modelica' });
            monaco.languages.setLanguageConfiguration('modelica', {
                comments: { lineComment: '//', blockComment: ['/*', '*/'] },
            });
        }
    }

    function registerGalecLanguage(monaco) {
        if (galecLanguageRegistered || !monaco) {
            return;
        }
        galecLanguageRegistered = true;
        if (!monaco.languages.getLanguages().some((lang) => lang.id === 'galec')) {
            monaco.languages.register({ id: 'galec' });
        }
        monaco.languages.setLanguageConfiguration('galec', {
            comments: { blockComment: ['/*', '*/'] },
            brackets: [
                ['{', '}'],
                ['[', ']'],
                ['(', ')'],
            ],
            autoClosingPairs: [
                { open: '{', close: '}' },
                { open: '[', close: ']' },
                { open: '(', close: ')' },
                { open: "'", close: "'", notIn: ['string', 'comment'] },
            ],
        });
        monaco.languages.setMonarchTokensProvider('galec', {
            keywords: [
                'algorithm', 'and', 'block', 'constant', 'else', 'end', 'false',
                'for', 'if', 'input', 'limit', 'loop', 'method', 'not', 'or',
                'output', 'parameter', 'protected', 'public', 'self', 'signals',
                'then', 'true',
            ],
            types: ['Boolean', 'Integer', 'Real'],
            builtins: [
                'absolute', 'arccos', 'arcsin', 'arctan', 'atan2', 'cos', 'cosh',
                'exp', 'integer', 'ln', 'lg', 'max', 'min', 'real', 'sign', 'sin',
                'sinh', 'sqrt', 'tan', 'tanh',
            ],
            tokenizer: {
                root: [
                    [/\/\*/, 'comment', '@comment'],
                    [/'[^']*'/, 'string'],
                    [/[A-Za-z_][A-Za-z0-9_]*/, {
                        cases: {
                            '@keywords': 'keyword',
                            '@types': 'type',
                            '@builtins': 'predefined',
                            '@default': 'identifier',
                        },
                    }],
                    [/[{}()[\]]/, 'delimiter.bracket'],
                    [/[;,.]/, 'delimiter'],
                    [/:=|<=|>=|<>|[=<>+\-*/^:]/, 'operator'],
                    [/\d+\.\d+(?:[eE][+-]\d+)?/, 'number.float'],
                    [/\d+/, 'number'],
                ],
                comment: [
                    [/[^/*]+/, 'comment'],
                    [/\*\//, 'comment', '@pop'],
                    [/[/*]/, 'comment'],
                ],
            },
        });
    }

    function loadMonaco() {
        if (!monacoPromise) {
            monacoPromise = (async () => {
                const monacoBase = await locateMonacoBase();
                if (!window.require) {
                    await injectScript(`${monacoBase}vs/loader.js`);
                }
                window.require.config({ paths: { vs: `${monacoBase}vs` } });
                await new Promise((resolve, reject) => {
                    window.require(['vs/editor/editor.main'], resolve, reject);
                });
                const monaco = window.monaco;
                await loadModelicaLanguage(monaco);
                registerGalecLanguage(monaco);
                registerMonacoThemeSync(monaco);
                return monaco;
            })();
        }
        return monacoPromise;
    }

    function mdbookSavedTheme() {
        try {
            return trimMaybeString(localStorage.getItem('mdbook-theme'));
        } catch (_error) {
            return '';
        }
    }

    function mdbookCurrentTheme() {
        const saved = mdbookSavedTheme();
        if (saved) {
            return saved;
        }
        const classes = [
            ...document.documentElement.classList,
            ...(document.body ? [...document.body.classList] : []),
        ];
        return classes.find((name) => ['light', 'rust', 'coal', 'navy', 'ayu'].includes(name)) || '';
    }

    function monacoTheme() {
        const theme = mdbookCurrentTheme();
        const dark = ['coal', 'navy', 'ayu'].includes(theme)
            || (!theme && window.matchMedia?.('(prefers-color-scheme: dark)')?.matches);
        return dark ? 'vs-dark' : 'vs';
    }

    function registerMonacoThemeSync(monaco) {
        const apply = () => {
            monaco?.editor?.setTheme?.(monacoTheme());
            for (const editor of monacoEditors) {
                editor.layout?.();
                editor.render?.(true);
            }
        };
        apply();
        if (monacoThemeObserver) {
            return;
        }
        monacoThemeObserver = new MutationObserver(apply);
        monacoThemeObserver.observe(document.documentElement, {
            attributes: true,
            attributeFilter: ['class'],
        });
        if (document.body) {
            monacoThemeObserver.observe(document.body, {
                attributes: true,
                attributeFilter: ['class'],
            });
        }
        window.addEventListener('storage', (event) => {
            if (event.key === 'mdbook-theme') {
                apply();
            }
        });
        window.matchMedia?.('(prefers-color-scheme: dark)')?.addEventListener?.('change', apply);
    }

    // ---------------------------------------------------------------------
    // Language services (completion / hover / diagnostics) over the WASM LSP
    // ---------------------------------------------------------------------

    function lspRangeToMonaco(range) {
        const start = (range && range.start) || {};
        const end = (range && range.end) || {};
        return {
            startLineNumber: Math.max(1, Number(start.line ?? 0) + 1),
            startColumn: Math.max(1, Number(start.character ?? 0) + 1),
            endLineNumber: Math.max(1, Number(end.line ?? start.line ?? 0) + 1),
            endColumn: Math.max(1, Number(end.character ?? start.character ?? 0) + 1),
        };
    }

    function lspSeverityToMonaco(monaco, severity) {
        const severities = [
            monaco.MarkerSeverity.Error,
            monaco.MarkerSeverity.Error,
            monaco.MarkerSeverity.Warning,
            monaco.MarkerSeverity.Info,
            monaco.MarkerSeverity.Hint,
        ];
        return severities[severity] || monaco.MarkerSeverity.Error;
    }

    function lspHoverContentsToMarkdown(contents) {
        if (!contents) {
            return '';
        }
        if (typeof contents === 'string') {
            return contents;
        }
        if (Array.isArray(contents)) {
            return contents.map(lspHoverContentsToMarkdown).filter(Boolean).join('\n\n');
        }
        if (typeof contents.value === 'string') {
            if (typeof contents.language === 'string') {
                return `\`\`\`${contents.language}\n${contents.value}\n\`\`\``;
            }
            return contents.value;
        }
        return '';
    }

    function lspDefinitionLocations(monaco, model, response) {
        const locations = [];
        const addLocation = (location) => {
            const range = location?.range || location?.targetRange || location?.targetSelectionRange;
            if (!range) {
                return;
            }
            locations.push({
                uri: model.uri,
                range: lspRangeToMonaco(range),
            });
        };
        if (Array.isArray(response)) {
            response.forEach(addLocation);
        } else if (response) {
            addLocation(response);
        }
        return locations.length > 0 ? locations : null;
    }

    async function galecRuntimeRequest(method, payload) {
        const runtime = await loadRuntimeDriver();
        if (typeof runtime[method] !== 'function') {
            throw new Error(`${method} missing in this runtime build`);
        }
        return runtime[method]({
            pkgBase: resolvedPkgBase || await locatePkgBase(),
            ...payload,
        });
    }

    function registerGalecLanguageServices(monaco) {
        if (galecLanguageServicesRegistered || !monaco) {
            return;
        }
        galecLanguageServicesRegistered = true;
        registerGalecLanguage(monaco);
        monaco.languages.registerHoverProvider('galec', {
            async provideHover(model, position) {
                try {
                    const hover = await galecRuntimeRequest('galecHoverWithRuntime', {
                        source: model.getValue(),
                        fileName: galecModelFileNames.get(model) || 'generated.alg',
                        line: position.lineNumber - 1,
                        character: position.column - 1,
                    });
                    const value = lspHoverContentsToMarkdown(hover?.contents);
                    if (!value) {
                        return null;
                    }
                    return {
                        contents: [{ value }],
                        range: hover.range ? lspRangeToMonaco(hover.range) : undefined,
                    };
                } catch (error) {
                    console.warn('rumoca-live GALEC hover error:', error);
                    return null;
                }
            },
        });
        monaco.languages.registerDefinitionProvider('galec', {
            async provideDefinition(model, position) {
                try {
                    const response = await galecRuntimeRequest('galecDefinitionWithRuntime', {
                        source: model.getValue(),
                        fileName: galecModelFileNames.get(model) || 'generated.alg',
                        uri: model.uri?.toString?.() || 'file:///generated.alg',
                        line: position.lineNumber - 1,
                        character: position.column - 1,
                    });
                    return lspDefinitionLocations(monaco, model, response);
                } catch (error) {
                    console.warn('rumoca-live GALEC definition error:', error);
                    return null;
                }
            },
        });
    }

    async function updateGalecDiagnostics(monaco, model) {
        const state = galecDiagnosticsState.get(model) || { requestId: 0 };
        const requestId = state.requestId + 1;
        state.requestId = requestId;
        galecDiagnosticsState.set(model, state);
        const source = model.getValue();
        const versionId = model.getVersionId?.();
        try {
            const diagnostics = await galecRuntimeRequest('galecDiagnosticsWithRuntime', {
                source,
                fileName: galecModelFileNames.get(model) || 'generated.alg',
            });
            const current = galecDiagnosticsState.get(model);
            if (
                !current
                || current.requestId !== requestId
                || (versionId !== undefined && model.getVersionId?.() !== versionId)
            ) {
                return;
            }
            const markers = (Array.isArray(diagnostics) ? diagnostics : []).map((diagnostic) => ({
                ...lspRangeToMonaco(diagnostic.range),
                severity: lspSeverityToMonaco(monaco, diagnostic.severity),
                message: String(diagnostic.message || ''),
                source: String(diagnostic.source || 'rumoca-galec'),
                code: diagnostic.code,
            }));
            monaco.editor.setModelMarkers(model, 'rumoca-galec', markers);
        } catch (error) {
            console.warn('rumoca-live GALEC diagnostics error:', error);
        }
    }

    function activateGalecEditorLanguageServices(monaco, editor, fileName) {
        if (!monaco || !editor?.model) {
            return;
        }
        registerGalecLanguageServices(monaco);
        galecModelFileNames.set(editor.model, fileName || 'generated.alg');
        updateGalecDiagnostics(monaco, editor.model);
        let timer = null;
        editor.onChange(() => {
            clearTimeout(timer);
            timer = setTimeout(
                () => updateGalecDiagnostics(monaco, editor.model),
                DIAGNOSTIC_DEBOUNCE_MS
            );
        });
    }

    function registerLanguageServices(wasm) {
        if (languageServicesRegistered || !window.monaco) {
            return;
        }
        languageServicesRegistered = true;
        const monaco = window.monaco;

        async function sourceRootCacheUrlForModel(model) {
            const widget = monacoModelWidgets.get(model);
            return widget ? await widget.sourceRootCacheUrl(wasm) : '';
        }

        monaco.languages.registerCompletionItemProvider('modelica', {
            triggerCharacters: ['.', '(', ','],
            async provideCompletionItems(model, position) {
                try {
                    const worker = await loadSimWorker();
                    const json = await worker.request('completion', {
                        source: model.getValue(),
                        line: position.lineNumber - 1,
                        character: position.column - 1,
                        sourceRootCacheUrl: await sourceRootCacheUrlForModel(model),
                    });
                    const completions = JSON.parse(json);
                    const items = (completions && completions.items) || completions || [];
                    const kinds = monaco.languages.CompletionItemKind;
                    const kindMap = {
                        1: kinds.Text, 2: kinds.Method, 3: kinds.Function,
                        4: kinds.Constructor, 5: kinds.Field, 6: kinds.Variable,
                        7: kinds.Class, 8: kinds.Interface, 9: kinds.Module,
                        10: kinds.Property, 14: kinds.Keyword, 21: kinds.Constant,
                    };
                    return {
                        suggestions: items.map((item) => {
                            const suggestion = {
                                label: item.label,
                                kind: kindMap[item.kind] || kinds.Text,
                                insertText: item.insertText || item.label,
                                detail: item.detail,
                                documentation: item.documentation,
                            };
                            if (item.insertTextFormat === 2) {
                                suggestion.insertTextRules = monaco.languages
                                    .CompletionItemInsertTextRule.InsertAsSnippet;
                            }
                            return suggestion;
                        }),
                    };
                } catch (error) {
                    console.warn('rumoca-live completion error:', error);
                    return { suggestions: [] };
                }
            },
        });

        monaco.languages.registerHoverProvider('modelica', {
            async provideHover(model, position) {
                try {
                    const worker = await loadSimWorker();
                    const json = await worker.request('hover', {
                        source: model.getValue(),
                        line: position.lineNumber - 1,
                        character: position.column - 1,
                        sourceRootCacheUrl: await sourceRootCacheUrlForModel(model),
                    });
                    const hover = JSON.parse(json);
                    if (hover && hover.contents) {
                        const content = typeof hover.contents === 'string'
                            ? hover.contents
                            : (hover.contents.value || JSON.stringify(hover.contents));
                        return { contents: [{ value: content }] };
                    }
                } catch (error) {
                    console.warn('rumoca-live hover error:', error);
                }
                return null;
            },
        });
    }

    async function ensureWasmSourceRootsLoaded(wasm, widget) {
        const sourceRoots = await (widget?.sourceRootPaths?.(wasm) || []);
        if (sourceRoots.length === 0) {
            return;
        }
        const cacheUrl = await widget.sourceRootCacheUrl(wasm);
        const cacheKey = `cache:${cacheUrl}`;
        if (widget.sourceRootsSessionKey === cacheKey) {
            return;
        }
        const runtime = await loadRuntimeDriver();
        await runtime.ensureParsedSourceRootCache(wasm, cacheUrl);
        widget.sourceRootsSessionKey = cacheKey;
    }

    async function updateDiagnostics(wasm, monaco, model, widget) {
        const source = model.getValue();
        const versionId = model.getVersionId?.();
        const requestId = (widget.diagnosticsRequestId || 0) + 1;
        widget.diagnosticsRequestId = requestId;
        let diagnostics = [];
        try {
            const sourceRootCacheUrl = await widget.sourceRootCacheUrl(wasm);
            const worker = await loadSimWorker();
            const diagnosticsJson = await worker.request('diagnostics', {
                source,
                sourceRootCacheUrl,
            });
            if (
                widget.diagnosticsRequestId !== requestId
                || (versionId !== undefined && model.getVersionId?.() !== versionId)
            ) {
                return;
            }
            diagnostics = JSON.parse(diagnosticsJson) || [];
        } catch (error) {
            console.warn('rumoca-live diagnostics error:', error);
            return;
        }
        const markers = diagnostics.map((diagnostic) => ({
            ...lspRangeToMonaco(diagnostic.range),
            severity: lspSeverityToMonaco(monaco, diagnostic.severity),
            message: String(diagnostic.message || ''),
            source: 'rumoca',
        }));
        monaco.editor.setModelMarkers(model, 'rumoca', markers);
    }

    // ---------------------------------------------------------------------
    // Model name + plotting helpers
    // ---------------------------------------------------------------------

    function inferModelName(wasm, source) {
        try {
            const state = JSON.parse(wasm.get_simulation_models(source, ''));
            if (state && state.selected_model) {
                return state.selected_model;
            }
        } catch (_error) {
            // Fall through to the regex below on parse errors.
        }
        const match = /\b(?:model|block|class)\s+([A-Za-z_][A-Za-z0-9_]*)/.exec(source);
        return match ? match[1] : null;
    }

    function niceTicks(min, max, count) {
        if (!isFinite(min) || !isFinite(max) || min === max) {
            const value = isFinite(min) ? min : 0;
            return [value];
        }
        const span = max - min;
        const step = Math.pow(10, Math.floor(Math.log10(span / count)));
        const scaled = span / count / step;
        const niceStep = step * (scaled >= 5 ? 10 : scaled >= 2 ? 5 : scaled >= 1 ? 2 : 1);
        const start = Math.ceil(min / niceStep) * niceStep;
        const ticks = [];
        for (let v = start; v <= max + niceStep * 1e-9; v += niceStep) {
            ticks.push(v);
        }
        return ticks;
    }

    function formatTick(value) {
        if (value === 0) return '0';
        const abs = Math.abs(value);
        if (abs >= 1e4 || abs < 1e-3) return value.toExponential(1);
        return String(parseFloat(value.toPrecision(4)));
    }

    function svgEl(tag, attrs) {
        const el = document.createElementNS('http://www.w3.org/2000/svg', tag);
        for (const [key, value] of Object.entries(attrs)) {
            el.setAttribute(key, value);
        }
        return el;
    }

    // Render a time-series plot of the selected series as an inline SVG.
    function renderPlot(container, times, series) {
        const width = 640;
        const height = 320;
        const margin = { left: 56, right: 12, top: 12, bottom: 56 };
        const plotW = width - margin.left - margin.right;
        const plotH = height - margin.top - margin.bottom;

        let yMin = Infinity;
        let yMax = -Infinity;
        for (const s of series) {
            for (const v of s.values) {
                if (isFinite(v)) {
                    yMin = Math.min(yMin, v);
                    yMax = Math.max(yMax, v);
                }
            }
        }
        if (!isFinite(yMin)) { yMin = 0; yMax = 1; }
        if (yMin === yMax) { yMin -= 1; yMax += 1; }
        const pad = (yMax - yMin) * 0.05;
        yMin -= pad;
        yMax += pad;
        const xMin = times[0];
        const xMax = times[times.length - 1];

        const sx = (t) => margin.left + ((t - xMin) / (xMax - xMin || 1)) * plotW;
        const sy = (v) => margin.top + (1 - (v - yMin) / (yMax - yMin)) * plotH;

        const svg = svgEl('svg', {
            viewBox: `0 0 ${width} ${height}`,
            class: 'rumoca-live-plot-svg',
            role: 'img',
        });
        svg.appendChild(svgEl('rect', {
            x: margin.left, y: margin.top, width: plotW, height: plotH,
            class: 'rumoca-live-plot-frame',
        }));

        for (const t of niceTicks(xMin, xMax, 6)) {
            const x = sx(t);
            svg.appendChild(svgEl('line', {
                x1: x, y1: margin.top, x2: x, y2: margin.top + plotH,
                class: 'rumoca-live-plot-grid',
            }));
            const label = svgEl('text', {
                x, y: margin.top + plotH + 18, 'text-anchor': 'middle',
                class: 'rumoca-live-plot-tick',
            });
            label.textContent = formatTick(t);
            svg.appendChild(label);
        }
        for (const v of niceTicks(yMin, yMax, 5)) {
            const y = sy(v);
            svg.appendChild(svgEl('line', {
                x1: margin.left, y1: y, x2: margin.left + plotW, y2: y,
                class: 'rumoca-live-plot-grid',
            }));
            const label = svgEl('text', {
                x: margin.left - 6, y: y + 4, 'text-anchor': 'end',
                class: 'rumoca-live-plot-tick',
            });
            label.textContent = formatTick(v);
            svg.appendChild(label);
        }
        const xAxis = svgEl('text', {
            x: margin.left + plotW / 2, y: margin.top + plotH + 36,
            'text-anchor': 'middle', class: 'rumoca-live-plot-axis',
        });
        xAxis.textContent = 'time [s]';
        svg.appendChild(xAxis);

        series.forEach((s, index) => {
            const color = SERIES_COLORS[index % SERIES_COLORS.length];
            const points = times
                .map((t, i) => `${sx(t).toFixed(1)},${sy(s.values[i]).toFixed(1)}`)
                .join(' ');
            svg.appendChild(svgEl('polyline', {
                points, fill: 'none', stroke: color, 'stroke-width': 1.8,
            }));
        });

        const legend = document.createElement('div');
        legend.className = 'rumoca-live-legend';
        series.forEach((s, index) => {
            const item = document.createElement('span');
            item.className = 'rumoca-live-legend-item';
            const swatch = document.createElement('span');
            swatch.className = 'rumoca-live-legend-swatch';
            swatch.style.background = SERIES_COLORS[index % SERIES_COLORS.length];
            item.appendChild(swatch);
            item.appendChild(document.createTextNode(s.name));
            legend.appendChild(item);
        });

        container.replaceChildren(svg, legend);
    }

    function pickPlotSeries(payload, requestedNames) {
        const names = payload.names || [];
        const allData = payload.allData || [];
        const data = allData.slice(1);
        const requested = Array.isArray(requestedNames) ? requestedNames : [];
        const pickedByName = requested
            .map((name) => names.indexOf(name))
            .filter((index) => index >= 0 && Array.isArray(data[index]));
        if (pickedByName.length > 0) {
            return pickedByName.map((index) => ({ name: names[index], values: data[index] }));
        }
        const nStates = payload.nStates || names.length;
        // Rank states by dynamic range so flat series (e.g. clamped boundary
        // cells of a discretized field) do not crowd out the real dynamics.
        const candidates = [];
        for (let i = 0; i < Math.min(nStates, names.length); i++) {
            if (!Array.isArray(data[i])) {
                continue;
            }
            let lo = Infinity;
            let hi = -Infinity;
            for (const v of data[i]) {
                if (isFinite(v)) {
                    lo = Math.min(lo, v);
                    hi = Math.max(hi, v);
                }
            }
            candidates.push({ index: i, range: isFinite(hi - lo) ? hi - lo : 0 });
        }
        candidates.sort((a, b) => (b.range - a.range) || (a.index - b.index));
        const picked = candidates.slice(0, MAX_PLOT_SERIES);
        if (picked.length === 0 && names.length > 0 && Array.isArray(data[0])) {
            picked.push({ index: 0 });
        }
        picked.sort((a, b) => a.index - b.index);
        return picked.map(({ index }) => ({ name: names[index], values: data[index] }));
    }

    // ---------------------------------------------------------------------
    // Radial field visualization (`viz-radial` blocks)
    //
    // Interprets an array state such as T[1..N] as concentric shells of a
    // sphere and animates a colored cross-section over the simulation time.
    // ---------------------------------------------------------------------

    function findRadialField(payload) {
        const names = payload.names || [];
        const data = (payload.allData || []).slice(1);
        const nStates = payload.nStates || names.length;
        const groups = new Map();
        for (let i = 0; i < Math.min(nStates, names.length); i++) {
            const match = /^(.+)\[(\d+)\]$/.exec(names[i]);
            if (!match || !Array.isArray(data[i])) {
                continue;
            }
            const base = match[1];
            if (!groups.has(base)) {
                groups.set(base, []);
            }
            groups.get(base).push({ index: Number(match[2]), values: data[i] });
        }
        let best = null;
        for (const [base, members] of groups) {
            if (!best || members.length > best.members.length) {
                best = { base, members };
            }
        }
        if (!best || best.members.length < 2) {
            return null;
        }
        best.members.sort((a, b) => a.index - b.index);
        return best;
    }

    // Group 2-D array states such as u[i,j] into a matrix field.
    function findMatrixField(payload) {
        const names = payload.names || [];
        const data = (payload.allData || []).slice(1);
        const nStates = payload.nStates || names.length;
        const groups = new Map();
        for (let i = 0; i < Math.min(nStates, names.length); i++) {
            const match = /^(.+)\[(\d+)\s*,\s*(\d+)\]$/.exec(names[i]);
            if (!match || !Array.isArray(data[i])) {
                continue;
            }
            const base = match[1];
            if (!groups.has(base)) {
                groups.set(base, []);
            }
            groups.get(base).push({
                row: Number(match[2]),
                col: Number(match[3]),
                values: data[i],
            });
        }
        let best = null;
        for (const [base, members] of groups) {
            if (!best || members.length > best.members.length) {
                best = { base, members };
            }
        }
        if (!best || best.members.length < 4) {
            return null;
        }
        let rows = 0;
        let cols = 0;
        const byCell = new Map();
        for (const member of best.members) {
            rows = Math.max(rows, member.row);
            cols = Math.max(cols, member.col);
            byCell.set(`${member.row},${member.col}`, member.values);
        }
        return {
            base: best.base,
            rows,
            cols,
            members: best.members,
            at: (row, col) => byCell.get(`${row},${col}`) || null,
        };
    }

    function heatColor(fraction) {
        const f = Math.max(0, Math.min(1, fraction));
        // Blue (cold) through yellow to red (hot).
        const hue = 240 * (1 - f);
        return `hsl(${hue.toFixed(0)}, 85%, ${(35 + 20 * f).toFixed(0)}%)`;
    }

    function formatClock(seconds) {
        if (seconds >= 3600) {
            const h = Math.floor(seconds / 3600);
            const m = Math.floor((seconds % 3600) / 60);
            return `${h} h ${String(m).padStart(2, '0')} min`;
        }
        if (seconds >= 60) {
            const m = Math.floor(seconds / 60);
            const s = Math.floor(seconds % 60);
            return `${m} min ${String(s).padStart(2, '0')} s`;
        }
        return `${formatTick(seconds)} s`;
    }

    function makeCanvas(container, width, height) {
        const canvas = document.createElement('canvas');
        canvas.width = width;
        canvas.height = height;
        canvas.className = 'rumoca-live-radial-canvas';
        container.appendChild(canvas);
        return { canvas, ctx2d: canvas.getContext('2d') };
    }

    // Play button + scrubber + time label driving `drawFrame(frameIndex)`.
    // The string returned by drawFrame becomes the label text.
    function addAnimation(container, times, drawFrame, playDurationMs = 10000, options = {}) {
        if (options.live) {
            const clock = document.createElement('div');
            clock.className = 'rumoca-live-status';
            container.appendChild(clock);
            const draw = (frame) => {
                const clamped = Math.max(0, Math.min(times.length - 1, Math.round(frame)));
                clock.textContent = drawFrame(clamped) || '';
            };
            draw(times.length - 1);
            const anim = { redraw: draw };
            if (Array.isArray(options.liveAnimations)) {
                options.liveAnimations.push(anim);
            }
            return anim;
        }

        const controls = document.createElement('div');
        controls.className = 'rumoca-live-radial-controls';
        const playBtn = document.createElement('button');
        playBtn.type = 'button';
        playBtn.className = 'rumoca-live-button';
        playBtn.textContent = '▶';
        playBtn.setAttribute('aria-label', 'Play animation');
        const slider = document.createElement('input');
        slider.type = 'range';
        slider.min = '0';
        slider.max = String(times.length - 1);
        slider.value = '0';
        slider.className = 'rumoca-live-radial-slider';
        const clock = document.createElement('span');
        clock.className = 'rumoca-live-status';
        controls.append(playBtn, slider, clock);
        container.appendChild(controls);

        let playing = false;
        let rafId = null;
        const stop = () => {
            playing = false;
            playBtn.textContent = '▶';
            if (rafId !== null) {
                cancelAnimationFrame(rafId);
                rafId = null;
            }
        };
        const clampFrame = (frame) =>
            Math.max(0, Math.min(times.length - 1, Math.round(frame)));
        // A drawFrame exception must stop playback visibly, not freeze the
        // loop while the button still reads "playing".
        const draw = (frame) => {
            try {
                clock.textContent = drawFrame(clampFrame(frame)) || '';
            } catch (error) {
                stop();
                clock.textContent = `draw error: ${error.message || error}`;
                console.error('rumoca-live animation draw error:', error);
            }
            // Ratchet the label width up so the flexed slider next to it
            // does not resize (oscillate) as digit counts change.
            const width = clock.scrollWidth;
            if (width > (parseFloat(clock.style.minWidth) || 0)) {
                clock.style.minWidth = `${width}px`;
            }
        };

        playBtn.addEventListener('click', () => {
            if (playing) {
                stop();
                return;
            }
            playing = true;
            playBtn.textContent = '⏸';
            // Delta-time accumulation: robust to rAF timestamp skew and to
            // pauses, and resumes from the current slider position.
            let framePos = Number(slider.value) >= times.length - 1
                ? 0 : Number(slider.value);
            let lastTick = null;
            const framesPerMs = (times.length - 1) / playDurationMs;
            const step = (now) => {
                if (!playing) {
                    return;
                }
                if (lastTick !== null) {
                    const dt = Math.max(0, now - lastTick);
                    framePos += dt * framesPerMs;
                }
                lastTick = now;
                const frame = clampFrame(framePos);
                slider.value = String(frame);
                draw(frame);
                if (frame >= times.length - 1) {
                    stop();
                    return;
                }
                rafId = requestAnimationFrame(step);
            };
            rafId = requestAnimationFrame(step);
        });
        slider.addEventListener('input', () => {
            stop();
            draw(Number(slider.value));
        });
        draw(0);
        return { redraw: draw };
    }

    function addColorbar(container, lo, hi, colorFn) {
        const colorbar = document.createElement('div');
        colorbar.className = 'rumoca-live-radial-colorbar';
        const gradient = document.createElement('span');
        gradient.className = 'rumoca-live-radial-gradient';
        const stops = [];
        for (let i = 0; i <= 10; i++) {
            stops.push(colorFn(i / 10));
        }
        gradient.style.background = `linear-gradient(to right, ${stops.join(', ')})`;
        const loLabel = document.createElement('span');
        loLabel.textContent = formatTick(lo);
        const hiLabel = document.createElement('span');
        hiLabel.textContent = formatTick(hi);
        colorbar.append(loLabel, gradient, hiLabel);
        container.appendChild(colorbar);
    }

    function valueRange(members) {
        let vMin = Infinity;
        let vMax = -Infinity;
        for (const member of members) {
            for (const v of member.values) {
                if (isFinite(v)) {
                    vMin = Math.min(vMin, v);
                    vMax = Math.max(vMax, v);
                }
            }
        }
        if (!isFinite(vMin)) { vMin = 0; vMax = 1; }
        if (vMin === vMax) { vMax = vMin + 1; }
        return { vMin, vMax };
    }

    function enableLiveInputModelSource(source) {
        let next = source.replace(
            /(\bparameter\s+Boolean\s+interactive\s*=\s*)false\b/,
            '$1true',
        );
        next = next.replace(
            /\n  parameter Real sc\[NX, NY\] = \{[\s\S]*?\n  \} "Chordwise coordinate in the pitched airfoil frame";/,
            '\n  Real sc[NX, NY] "Chordwise coordinate in the pitched airfoil frame";',
        );
        next = next.replace(
            /\n  parameter Real nc\[NX, NY\] = \{[\s\S]*?\n  \} "Chord-normal coordinate in the pitched airfoil frame";/,
            '\n  Real nc[NX, NY] "Chord-normal coordinate in the pitched airfoil frame";',
        );
        next = next.replace(
            /\n  parameter Real sig\[NX, NY\] = \{[\s\S]*?\n  \} "Solid mask \(1 inside the airfoil\)";/,
            '\n  Real sig[NX, NY] "Solid mask (1 inside the airfoil)";',
        );
        return next;
    }

    // The API handed to editable `js,rumoca-viz` blocks (and used by the
    // built-in `viz-radial` mode). Documented in the user guide.
    function makeVizApi(payload, container, widget, options = {}) {
        const names = payload.names || [];
        const data = (payload.allData || []).slice(1);
        const initialOverrides = widget
            ? { ...widget.structuralParams, ...widget.paramOverrides }
            : {};
        const sourceText = widget?.getSource?.() || '';
        const sourceNumericParameter = (name, fallback) => {
            const escaped = String(name).replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
            const re = new RegExp(
                '\\bparameter\\s+(?:Real|Integer)\\s+' + escaped
                + '\\s*=\\s*(-?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?)\\b',
            );
            const match = re.exec(sourceText);
            const value = match ? Number(match[1]) : NaN;
            return Number.isFinite(value) ? value : fallback;
        };
        return {
            overrides: initialOverrides,
            parameter(name, fallback = NaN) {
                const liveOverrides = widget
                    ? { ...widget.structuralParams, ...widget.paramOverrides }
                    : initialOverrides;
                const override = Number(liveOverrides[name]);
                if (Number.isFinite(override)) {
                    return override;
                }
                return sourceNumericParameter(name, fallback);
            },
            // Slider bound to a scalar parameter. Two modes:
            //  - default: a tunable parameter; changing it re-settles the
            //    prepared vectors and re-runs (fast GPU path) without recompiling.
            //  - { recompile: true }: a STRUCTURAL parameter (e.g. one marked
            //    annotation(Evaluate=true)). A runtime override of such a
            //    parameter is rejected by the compiler, so the slider rewrites
            //    the source literal `<name> = <value>` in the editor and forces a
            //    full recompile + re-run.
            //  - { interactiveInput: "u" }: when the widget's Interactive toggle
            //    is on, slider movement writes the named model input before
            //    each interactive interval; when it is off, the slider behaves
            //    like a normal parameter tuner.
            // Locked while a simulation is in flight.
            addTuner(name, opts = {}) {
                if (!widget) {
                    return;
                }
                const structural = !!opts.recompile;
                const interactiveInput = typeof opts.interactiveInput === 'string'
                    && opts.interactiveInput
                    ? opts.interactiveInput
                    : null;
                const store = structural ? widget.structuralParams : widget.paramOverrides;
                const row = document.createElement('div');
                row.className = 'rumoca-live-tuner';
                const label = document.createElement('span');
                label.textContent = opts.label || name;
                const slider = document.createElement('input');
                slider.type = 'range';
                slider.min = String(opts.min ?? 0);
                slider.max = String(opts.max ?? 1);
                slider.step = String(opts.step ?? 0.1);
                slider.value = String(store[name] ?? opts.value ?? opts.min ?? 0);
                const readout = document.createElement('span');
                readout.className = 'rumoca-live-status';
                readout.textContent = slider.value;
                if (interactiveInput) {
                    slider.dataset.rumocaLiveInput = interactiveInput;
                    widget.liveInputs[interactiveInput] = Number(slider.value);
                }
                slider.addEventListener('input', () => {
                    readout.textContent = slider.value;
                    const value = Number(slider.value);
                    if (Number.isFinite(value)) {
                        if (!structural) {
                            widget.paramOverrides[name] = value;
                        }
                        if (interactiveInput) {
                            widget.liveInputs[interactiveInput] = value;
                        }
                    }
                });
                // Input events keep parameter overrides current immediately.
                // The release event only decides whether to rerun now.
                slider.addEventListener('change', () => {
                    const value = Number(slider.value);
                    if (Number.isFinite(value)) {
                        if (!structural) {
                            widget.paramOverrides[name] = value;
                        }
                        if (interactiveInput) {
                            widget.liveInputs[interactiveInput] = value;
                        }
                    }
                    if (liveCheck.checked && !structural) {
                        return;
                    }
                    if (widget.busy) {
                        widget.restartRun();
                        return;
                    }
                    if (structural) {
                        // Structural parameter: rewrite the source literal and
                        // force a full recompile (a runtime override is rejected).
                        widget.structuralParams[name] = value;
                        const src = widget.getSource();
                        const re = new RegExp(
                            '(parameter\\s+\\w+\\s+' + name + '\\s*=\\s*)'
                            + '-?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?');
                        const next = src.replace(re, '$1' + value);
                        if (next !== src) {
                            widget.gpuPrep = null;   // invalidate the compiled cache
                            widget.setSource(next);  // surface the new value in source
                        }
                    } else {
                        widget.paramOverrides[name] = value;
                    }
                    widget.requestRun();
                });
                widget.tunerInputs.push(slider);
                row.append(label, slider, readout);
                container.appendChild(row);
            },
            arrayField: () => findRadialField(payload),
            matrixField: () => findMatrixField(payload),
            series: (name) => {
                const index = names.indexOf(name);
                return index >= 0 ? data[index] : null;
            },
            valueRange,
            heatColor,
            formatTick,
            formatClock,
            loadThree: loadThreeModule,
            hideDefaultPlot: () => {
                if (widget) {
                    widget.hideDefaultPlot = true;
                }
            },
            plotSeries: (seriesNames) => {
                if (widget) {
                    widget.plotSeries = Array.isArray(seriesNames) ? seriesNames.slice() : [];
                }
            },
            makeCanvas: (width, height) => makeCanvas(container, width, height),
            addAnimation: (times, drawFrame, durationMs) =>
                addAnimation(container, times, drawFrame, durationMs, options),
            addColorbar: (lo, hi, colorFn) =>
                addColorbar(container, lo, hi, colorFn || heatColor),
        };
    }

    // Built-in cross-section animation for `viz-radial` blocks without an
    // attached editable script.
    function renderRadialViz(container, times, payload) {
        const field = findRadialField(payload);
        if (!field) {
            return false;
        }
        const api = makeVizApi(payload, container, null);
        const { vMin, vMax } = valueRange(field.members);
        const size = 280;
        const { ctx2d } = api.makeCanvas(size, size);
        const n = field.members.length;
        api.addAnimation(times, (frame) => {
            ctx2d.clearRect(0, 0, size, size);
            const maxR = size / 2 - 6;
            // Draw shells outermost-first so inner shells paint on top.
            for (let i = n - 1; i >= 0; i--) {
                const fraction = (field.members[i].values[frame] - vMin) / (vMax - vMin);
                ctx2d.beginPath();
                ctx2d.arc(size / 2, size / 2, maxR * ((i + 1) / n), 0, 2 * Math.PI);
                ctx2d.fillStyle = heatColor(fraction);
                ctx2d.fill();
            }
            ctx2d.beginPath();
            ctx2d.arc(size / 2, size / 2, maxR, 0, 2 * Math.PI);
            ctx2d.strokeStyle = '#555';
            ctx2d.lineWidth = 1.5;
            ctx2d.stroke();
            return `t = ${formatClock(times[frame])} · `
                + `${field.base}[1] = ${formatTick(field.members[0].values[frame])} · `
                + `${field.base}[${n}] = ${formatTick(field.members[n - 1].values[frame])}`;
        });
        api.addColorbar(vMin, vMax, heatColor);
        return true;
    }

    async function runCustomViz(container, payload, times, code, widget, options = {}) {
        const api = makeVizApi(payload, container, widget, options);
        const render = new Function(
            '{ payload, times, names, data, container, api }',
            `return (async () => {\n${code}\n})();`
        );
        await render({
            payload,
            times,
            names: payload.names || [],
            data: (payload.allData || []).slice(1),
            container,
            api,
        });
    }

    function describeRun(result) {
        const payload = result.payload || {};
        const details = payload.simDetails || {};
        const actual = details.actual || {};
        const requested = details.requested || {};
        const metrics = result.metrics || {};
        const parts = [];
        if (requested.solver) parts.push(`solver ${requested.solver}`);
        if (actual.points) parts.push(`${actual.points} points`);
        if (typeof actual.t_end === 'number') {
            parts.push(`t = ${formatTick(actual.t_start || 0)}…${formatTick(actual.t_end)} s`);
        }
        if (typeof metrics.simulateSeconds === 'number') {
            parts.push(`${(metrics.simulateSeconds * 1000).toFixed(1)} ms`);
        }
        return parts.join(' · ');
    }

    // ---------------------------------------------------------------------
    // Editor backends: Monaco (preferred) and a plain textarea fallback
    // ---------------------------------------------------------------------

    function createMonacoEditor(monaco, host, source, language = 'modelica', options = {}) {
        const lineHeight = 19;
        const verticalPadding = 12;
        const heightFor = (text) => {
            const maxLines = Number.isFinite(options.maxLines) ? options.maxLines : MAX_EDITOR_LINES;
            const lines = Math.min(text.split('\n').length + 1, maxLines);
            return Math.max(lines, 4) * lineHeight + verticalPadding;
        };
        host.style.height = `${heightFor(source)}px`;
        const editor = monaco.editor.create(host, {
            value: source,
            language,
            theme: monacoTheme(),
            minimap: { enabled: false },
            fontSize: 13,
            lineNumbers: 'on',
            lineNumbersMinChars: 3,
            automaticLayout: true,
            readOnly: Boolean(options.readOnly),
            domReadOnly: Boolean(options.readOnly),
            quickSuggestions: false,
            suggestOnTriggerCharacters: true,
            glyphMargin: false,
            folding: false,
            scrollBeyondLastLine: false,
            overviewRulerLanes: 0,
            renderLineHighlight: 'none',
            scrollbar: { alwaysConsumeMouseWheel: false },
        });
        const model = editor.getModel();
        editor.onDidChangeModelContent(() => {
            host.style.height = `${heightFor(editor.getValue())}px`;
        });
        monacoEditors.add(editor);
        return {
            getValue: () => editor.getValue(),
            setValue: (text) => editor.setValue(text),
            onChange: (cb) => editor.onDidChangeModelContent(cb),
            onFocus: (cb) => editor.onDidFocusEditorText(cb),
            layout: () => editor.layout(),
            model,
            dispose: () => {
                monacoEditors.delete(editor);
                editor.dispose();
                model?.dispose?.();
            },
        };
    }

    function createTextareaEditor(host, source) {
        const editor = document.createElement('textarea');
        editor.className = 'rumoca-live-editor';
        editor.spellcheck = false;
        editor.value = source;
        editor.setAttribute('aria-label', 'Editable Modelica example');
        const resize = () => {
            editor.rows = Math.max(editor.value.split('\n').length + 1, 4);
        };
        resize();
        editor.addEventListener('input', resize);
        editor.addEventListener('keydown', (event) => {
            if (event.key === 'Tab') {
                event.preventDefault();
                const { selectionStart, selectionEnd, value } = editor;
                editor.value = value.slice(0, selectionStart) + '  ' + value.slice(selectionEnd);
                editor.selectionStart = editor.selectionEnd = selectionStart + 2;
            }
        });
        host.appendChild(editor);
        return {
            getValue: () => editor.value,
            setValue: (text) => { editor.value = text; resize(); },
            onChange: (cb) => editor.addEventListener('input', cb),
            onFocus: (cb) => editor.addEventListener('focus', cb),
            model: null,
        };
    }

    function parseLiveDirectives(source) {
        const directives = {};
        const pattern = /^\s*\/\/\s*rumoca-live-([a-z-]+):\s*(.*?)\s*$/gm;
        for (const match of source.matchAll(pattern)) {
            directives[match[1]] = match[2];
        }
        return directives;
    }

    function modelFileNameForInlineScenario(model) {
        const safe = String(model || 'Model')
            .trim()
            .replace(/[^A-Za-z0-9_.-]+/g, '_')
            .replace(/^_+|_+$/g, '');
        return `${safe || 'Model'}.mo`;
    }

    function experimentNumber(source, key) {
        const pattern = new RegExp(`\\b${key}\\s*=\\s*([-+]?\\d+(?:\\.\\d+)?(?:[eE][-+]?\\d+)?)`);
        const match = pattern.exec(source || '');
        const value = match ? Number(match[1]) : NaN;
        return Number.isFinite(value) && value > 0 ? value : undefined;
    }

    function experimentSolver(source) {
        const match = /\bSolver\s*=\s*"([^"]+)"/.exec(source || '');
        const solver = trimMaybeString(match?.[1]).toLowerCase();
        return solver === 'bdf' || solver === 'rk-like' ? solver : 'auto';
    }

    // ---------------------------------------------------------------------
    // Widget
    // ---------------------------------------------------------------------

    // Turn a `js,rumoca-viz` block into an editable visualization script
    // attached to the preceding interactive widget.
    function buildVizEditor(codeEl, monaco) {
        const pre = codeEl.parentElement;
        const originalSource = codeEl.textContent.replace(/\n$/, '');

        const details = document.createElement('details');
        details.className = 'rumoca-live-viz';
        const summary = document.createElement('summary');
        summary.textContent = 'Visualization script (JavaScript — editable)';
        const host = document.createElement('div');
        host.className = 'rumoca-live-editor-host';
        details.append(summary, host);
        pre.replaceWith(details);

        const editor = monaco
            ? createMonacoEditor(monaco, host, originalSource, 'javascript')
            : createTextareaEditor(host, originalSource);
        return {
            getValue: () => editor.getValue(),
            reset: () => editor.setValue(originalSource),
        };
    }

    function buildGalecCodegenWidget(codeEl, monaco) {
        const pre = codeEl.parentElement;
        let originalSource = codeEl.textContent.replace(/\n$/, '');
        const liveDirectives = parseLiveDirectives(originalSource);
        let sourceUrl = liveDirectives.source || '';
        const scenarioUrl = liveDirectives.scenario || '';
        let modelOverride = liveDirectives.model || '';
        let scenarioText = '';
        let scenarioConfig = null;
        let scenarioDescriptor = null;

        const workspaceFocusPath = () =>
            repoExamplesRelativePath(scenarioUrl || sourceUrl);
        const scenarioWorkspacePath = () =>
            repoExamplesRelativePath(scenarioUrl) || 'rumoca-scenario.toml';

        function scenarioWorkspaceSources(text = scenarioText) {
            return JSON.stringify({ [scenarioWorkspacePath()]: text });
        }

        async function parseScenarioTextWithWasm(text) {
            const wasm = await loadWasm();
            if (typeof wasm.scenario_get_scenario_config_full !== 'function') {
                throw new Error('scenario_get_scenario_config_full missing in this WASM build');
            }
            const parsed = JSON.parse(wasm.scenario_get_scenario_config_full(
                scenarioWorkspaceSources(text),
                scenarioWorkspacePath()
            ));
            if (!parsed?.ok) {
                throw new Error(parsed?.error || 'failed to parse scenario config');
            }
            return parsed;
        }

        function codegenTarget() {
            return trimMaybeString(scenarioConfig?.codegen?.target);
        }

        function modelWorkspacePath(model) {
            return repoExamplesRelativePath(sourceUrl) || modelFileNameForInlineScenario(model);
        }

        function workspaceSourcesJson(source, model) {
            return JSON.stringify({ [modelWorkspacePath(model)]: source });
        }

        const container = document.createElement('div');
        container.className = 'rumoca-live rumoca-live-codegen-widget';
        const editorHost = document.createElement('div');
        editorHost.className = 'rumoca-live-editor-host';

        const toolbar = document.createElement('div');
        toolbar.className = 'rumoca-live-toolbar';
        const generateAlgBtn = document.createElement('button');
        generateAlgBtn.type = 'button';
        generateAlgBtn.className = 'rumoca-live-button rumoca-live-run';
        generateAlgBtn.textContent = 'Generate .alg';
        generateAlgBtn.title = 'Project the Modelica source into GALEC Algorithm Code';
        const resetBtn = document.createElement('button');
        resetBtn.type = 'button';
        resetBtn.className = 'rumoca-live-button';
        resetBtn.textContent = 'Reset';
        const status = document.createElement('span');
        status.className = 'rumoca-live-status';
        toolbar.append(generateAlgBtn, resetBtn, status);

        const progress = document.createElement('div');
        progress.className = 'rumoca-live-progress';
        progress.hidden = true;
        const progressFill = document.createElement('div');
        progressFill.className = 'rumoca-live-progress-fill rumoca-live-progress-indeterminate';
        progress.appendChild(progressFill);

        const output = document.createElement('div');
        output.className = 'rumoca-live-output';
        output.hidden = true;
        container.append(editorHost, toolbar, progress, output);
        pre.replaceWith(container);

        const editor = monaco
            ? createMonacoEditor(monaco, editorHost, originalSource)
            : createTextareaEditor(editorHost, originalSource);

        const widget = {
            busy: false,
            generatedEditors: [],
            languageServicesActive: false,
            diagnosticsTimer: null,
            diagnosticsSubscription: null,
            diagnosticsRequestId: 0,
            sourceRootCacheUrlPromise: null,
            sourceRootsSessionKey: null,
            workspaceSourceRootsPromise: null,
            scenarioSourceRootPaths() {
                return normalizeStringArray(scenarioConfig?.source_roots);
            },
            async workspaceSourceRootPaths(wasm) {
                const focusPath = workspaceFocusPath();
                if (!focusPath) {
                    return [];
                }
                if (!widget.workspaceSourceRootsPromise) {
                    widget.workspaceSourceRootsPromise = (async () => {
                        const workspaceSources = await loadWorkspaceSourcesForFocus(focusPath);
                        if (Object.keys(workspaceSources).length === 0) {
                            return [];
                        }
                        if (!wasm || typeof wasm.workspace_effective_source_roots !== 'function') {
                            throw new Error('workspace_effective_source_roots missing in this WASM build');
                        }
                        return normalizeStringArray(JSON.parse(wasm.workspace_effective_source_roots(
                            JSON.stringify(workspaceSources),
                            focusPath
                        )));
                    })().catch((error) => {
                        widget.workspaceSourceRootsPromise = null;
                        throw error;
                    });
                }
                return widget.workspaceSourceRootsPromise;
            },
            async sourceRootPaths(wasm) {
                const sourceRoots = uniqueStrings([
                    ...(await widget.workspaceSourceRootPaths(wasm)),
                    ...widget.scenarioSourceRootPaths(),
                ]);
                if (widget.scenarioSourceRootPaths().length > 0 && sourceRoots.length === 0) {
                    throw new Error(
                        `No effective Modelica source roots were resolved for ${workspaceFocusPath()}. ` +
                        'Check the scenario source_roots and the staged source-root cache.'
                    );
                }
                return sourceRoots;
            },
            sourceRootCacheUrl(wasm) {
                if (!widget.sourceRootCacheUrlPromise) {
                    widget.sourceRootCacheUrlPromise = widget.sourceRootPaths(wasm)
                        .then(loadSourceRootCacheUrlForRoots)
                        .catch((error) => {
                            widget.sourceRootCacheUrlPromise = null;
                            throw error;
                        });
                }
                return widget.sourceRootCacheUrlPromise;
            },
            setStatus(text) { status.textContent = text; },
            onWasmReady(wasm) {
                if (widget.languageServicesActive) {
                    activateEditorLanguageServices(wasm);
                }
            },
        };
        widgets.push(widget);
        if (editor.model) {
            monacoModelWidgets.set(editor.model, widget);
        }

        function scheduleDiagnostics(wasm) {
            if (!monaco || !editor.model) {
                return;
            }
            clearTimeout(widget.diagnosticsTimer);
            widget.diagnosticsTimer = setTimeout(
                () => updateDiagnostics(wasm, monaco, editor.model, widget),
                DIAGNOSTIC_DEBOUNCE_MS
            );
        }

        function activateEditorLanguageServices(wasm) {
            widget.languageServicesActive = true;
            if (!monaco || !editor.model) {
                return;
            }
            updateDiagnostics(wasm, monaco, editor.model, widget);
            if (!widget.diagnosticsSubscription) {
                widget.diagnosticsSubscription = editor.onChange(() => scheduleDiagnostics(wasm));
            }
        }

        function clearGeneratedEditors() {
            for (const generatedEditor of widget.generatedEditors) {
                generatedEditor.dispose?.();
            }
            widget.generatedEditors = [];
        }

        function showError(error) {
            clearGeneratedEditors();
            const message = error instanceof Error ? error.message : String(error);
            const errBox = document.createElement('pre');
            errBox.className = 'rumoca-live-error';
            errBox.textContent = message;
            output.replaceChildren(errBox);
            output.hidden = false;
            widget.setStatus('Failed');
        }

        function beginProgress(label) {
            progress.hidden = false;
            progressFill.style.width = '100%';
            progressFill.classList.add('rumoca-live-progress-indeterminate');
            widget.setStatus(label);
        }

        function endProgress() {
            progress.hidden = true;
            progressFill.classList.remove('rumoca-live-progress-indeterminate');
        }

        async function loadScenarioSource() {
            const response = await fetch(pageUrl(scenarioUrl));
            if (!response.ok) {
                throw new Error(`${response.status} ${response.statusText}`);
            }
            scenarioText = (await response.text()).replace(/\n$/, '');
            const parsed = await parseScenarioTextWithWasm(scenarioText);
            scenarioConfig = parsed.config || {};
            scenarioDescriptor = parsed.descriptor || {};
            const task = trimMaybeString(scenarioConfig?.rumoca?.task).toLowerCase();
            if (task && task !== 'codegen') {
                throw new Error(`GALEC codegen widget expected task = "codegen", found "${task}".`);
            }
            const target = codegenTarget();
            if (!GALEC_CODEGEN_TARGETS.has(target)) {
                throw new Error(`GALEC codegen widget does not support target "${target || '(missing)'}".`);
            }
            modelOverride = trimMaybeString(scenarioDescriptor.model)
                || trimMaybeString(scenarioConfig.model?.name)
                || modelOverride;
            sourceUrl = new URL(trimMaybeString(scenarioConfig.model?.file), pageUrl(scenarioUrl)).href;
            widget.sourceRootCacheUrlPromise = null;
            widget.sourceRootsSessionKey = null;
            widget.workspaceSourceRootsPromise = null;
        }

        async function loadExternalSource() {
            if (!sourceUrl && !scenarioUrl) {
                return;
            }
            generateAlgBtn.disabled = true;
            resetBtn.disabled = true;
            widget.setStatus(`Loading ${scenarioUrl || sourceUrl}…`);
            try {
                if (scenarioUrl) {
                    await loadScenarioSource();
                }
                const response = await fetch(sourceUrl.startsWith('http') ? sourceUrl : pageUrl(sourceUrl));
                if (!response.ok) {
                    throw new Error(`${response.status} ${response.statusText}`);
                }
                originalSource = (await response.text()).replace(/\n$/, '');
                editor.setValue(originalSource);
                generateAlgBtn.disabled = false;
                resetBtn.disabled = false;
                widget.setStatus('');
                if (widget.languageServicesActive && wasmModulePromise && monaco && editor.model) {
                    wasmModulePromise.then((wasm) => updateDiagnostics(wasm, monaco, editor.model, widget));
                }
            } catch (error) {
                const message = error instanceof Error ? error.message : String(error);
                widget.setStatus(`Failed to load ${scenarioUrl || sourceUrl}: ${message}`);
                resetBtn.disabled = false;
            }
        }

        async function renderGalecAlgFiles(wasm, source, model, signal) {
            const workspaceSources = workspaceSourcesJson(source, model);
            const filesJson = await runHeavy('codegen', {
                source,
                model,
                target: 'galec',
                workspaceSources,
                sourceRootCacheUrl: '',
            }, async () => {
                const runtime = await loadRuntimeDriver();
                const files = await runtime.renderGalecFilesWithRuntime({
                    pkgBase: resolvedPkgBase || await locatePkgBase(),
                    workspaceSources,
                    modelName: model,
                    target: 'galec',
                });
                return JSON.stringify(files);
            }, signal);
            throwIfAborted(signal);
            const files = JSON.parse(filesJson);
            if (!Array.isArray(files) || files.length === 0) {
                throw new Error('GALEC generation did not render any files.');
            }
            return files;
        }

        function codegenLanguageForPath(path) {
            const lower = String(path || '').toLowerCase();
            if (lower.endsWith('.alg')) return 'galec';
            if (lower.endsWith('.c') || lower.endsWith('.h')) return 'c';
            return 'plaintext';
        }

        function codegenStageLabel(path) {
            const lower = String(path || '').toLowerCase();
            if (lower.endsWith('.alg')) return 'GALEC Algorithm Code';
            if (lower.endsWith('.h')) return 'C header';
            if (lower.endsWith('.c')) return 'C source';
            return 'Generated file';
        }

        function modelIdentifierFromAlgPath(path) {
            const leaf = String(path || 'model.alg').split('/').pop() || 'model.alg';
            const stem = leaf.replace(/\.alg$/i, '') || 'model';
            return stem.replace(/[^A-Za-z0-9_.-]+/g, '_').replace(/^_+|_+$/g, '') || 'model';
        }

        function galecCGenerationTarget(target) {
            return target === 'efmi' || target === 'galec-c'
                ? target
                : 'galec-c';
        }

        async function generateCFromAlg(generatedEditor, algPath, configuredTarget, button) {
            const target = galecCGenerationTarget(configuredTarget);
            const algSource = generatedEditor.getValue();
            const modelName = modelIdentifierFromAlgPath(algPath);
            button.disabled = true;
            widget.setStatus(`Generating C/H from ${algPath}…`);
            try {
                const runtime = await loadRuntimeDriver();
                const cFiles = await runtime.renderGalecCFromAlgWithRuntime({
                    pkgBase: resolvedPkgBase || await locatePkgBase(),
                    algSource,
                    fileName: algPath,
                    modelName,
                    target,
                });
                renderCodegenResult([{ path: algPath, content: algSource }, ...cFiles], target);
                widget.setStatus(`Generated C/H from ${algPath}`);
            } catch (error) {
                showError(error);
            } finally {
                button.disabled = false;
            }
        }

        function appendCodegenPipeline(shell, files) {
            const hasAlg = files.some((file) => String(file.path || '').toLowerCase().endsWith('.alg'));
            const hasC = files.some((file) => /\.(?:c|h)$/i.test(String(file.path || '')));
            const stages = ['Modelica source'];
            if (hasAlg) stages.push('GALEC .alg');
            if (hasC) stages.push('C .h/.c');
            const pipeline = document.createElement('div');
            pipeline.className = 'rumoca-live-codegen-pipeline';
            for (const [index, label] of stages.entries()) {
                const stage = document.createElement('span');
                stage.className = 'rumoca-live-codegen-stage';
                stage.textContent = label;
                pipeline.appendChild(stage);
                if (index + 1 < stages.length) {
                    const arrow = document.createElement('span');
                    arrow.className = 'rumoca-live-codegen-arrow';
                    arrow.textContent = '→';
                    pipeline.appendChild(arrow);
                }
            }
            shell.appendChild(pipeline);
        }

        function appendCodegenFile(shell, file, index, target) {
            const path = trimMaybeString(file.path) || `file-${index + 1}`;
            const content = String(file.content ?? '');
            const language = codegenLanguageForPath(path);
            const details = document.createElement('details');
            details.open = index === 0;
            const summary = document.createElement('summary');
            const summaryMain = document.createElement('span');
            summaryMain.className = 'rumoca-live-codegen-summary-main';
            const stage = document.createElement('span');
            stage.className = 'rumoca-live-codegen-kind';
            stage.textContent = codegenStageLabel(path);
            const name = document.createElement('span');
            name.className = 'rumoca-live-codegen-name';
            name.textContent = path;
            summaryMain.append(stage, name);
            summary.appendChild(summaryMain);
            details.appendChild(summary);
            shell.appendChild(details);

            if (language === 'galec') {
                const host = document.createElement('div');
                host.className = 'rumoca-live-codegen-editor';
                details.appendChild(host);
                const generatedEditor = monaco
                    ? createMonacoEditor(monaco, host, content, language, { maxLines: 34 })
                    : createTextareaEditor(host, content);
                widget.generatedEditors.push(generatedEditor);
                details.addEventListener('toggle', () => {
                    if (details.open) {
                        generatedEditor.layout?.();
                    }
                });
                if (monaco) {
                    activateGalecEditorLanguageServices(monaco, generatedEditor, path);
                }
                const actions = document.createElement('span');
                actions.className = 'rumoca-live-codegen-actions';
                const generateCButton = document.createElement('button');
                generateCButton.type = 'button';
                generateCButton.className = 'rumoca-live-button rumoca-live-codegen-action';
                generateCButton.textContent = 'Generate C/H';
                generateCButton.title = 'Generate C header/source from the current GALEC .alg editor text';
                generateCButton.addEventListener('click', (event) => {
                    event.preventDefault();
                    event.stopPropagation();
                    void generateCFromAlg(generatedEditor, path, target, generateCButton);
                });
                actions.appendChild(generateCButton);
                summary.appendChild(actions);
                return;
            }

            if (monaco) {
                const host = document.createElement('div');
                host.className = 'rumoca-live-codegen-editor';
                details.appendChild(host);
                const generatedEditor = createMonacoEditor(
                    monaco,
                    host,
                    content,
                    language,
                    { readOnly: true },
                );
                widget.generatedEditors.push(generatedEditor);
                details.addEventListener('toggle', () => {
                    if (details.open) {
                        generatedEditor.layout?.();
                    }
                });
                return;
            }

            const pre = document.createElement('pre');
            pre.className = 'rumoca-live-dae rumoca-live-codegen-pre';
            pre.textContent = content;
            details.appendChild(pre);
        }

        function renderCodegenResult(files, target) {
            clearGeneratedEditors();
            const shell = document.createElement('div');
            shell.className = 'rumoca-live-codegen';
            output.replaceChildren(shell);
            output.hidden = false;
            appendCodegenPipeline(shell, files);
            files.forEach((file, index) => appendCodegenFile(shell, file, index, target));
            const hasOnlyAlg = files.length === 1
                && String(files[0]?.path || '').toLowerCase().endsWith('.alg');
            widget.setStatus(hasOnlyAlg
                ? 'Generated GALEC .alg; use Generate C/H in the .alg panel for the next step'
                : `Generated ${files.length} ${target} file(s)`);
        }

        async function runGalecCodegen(wasm, source, model, signal) {
            const target = codegenTarget();
            if (!GALEC_CODEGEN_TARGETS.has(target)) {
                throw new Error(`GALEC codegen widget does not support target "${target || '(missing)'}".`);
            }
            await ensureWasmSourceRootsLoaded(wasm, widget);
            const files = await renderGalecAlgFiles(wasm, source, model, signal);
            renderCodegenResult(files, target);
        }

        async function withWasm(action) {
            if (widget.busy) {
                return;
            }
            const run = new AbortController();
            widget.busy = true;
            generateAlgBtn.disabled = true;
            resetBtn.disabled = true;
            clearGeneratedEditors();
            beginProgress('Generating GALEC .alg…');
            let succeeded = false;
            try {
                const wasm = await loadWasm();
                throwIfAborted(run.signal);
                const source = editor.getValue();
                const model = modelOverride || inferModelName(wasm, source);
                if (!model) {
                    throw new Error('No model/block/class found in this example.');
                }
                activateEditorLanguageServices(wasm);
                await action(wasm, source, model, run.signal);
                throwIfAborted(run.signal);
                succeeded = true;
            } catch (error) {
                if (isAbortError(error)) {
                    widget.setStatus('Stopped');
                } else {
                    showError(error);
                }
            } finally {
                endProgress();
                widget.busy = false;
                generateAlgBtn.disabled = false;
                resetBtn.disabled = false;
                if (!succeeded && output.hidden) {
                    widget.setStatus('Failed');
                }
            }
        }

        generateAlgBtn.addEventListener('click', () => {
            void withWasm(runGalecCodegen);
        });

        resetBtn.addEventListener('click', () => {
            clearGeneratedEditors();
            editor.setValue(originalSource);
            output.hidden = true;
            output.replaceChildren();
            widget.setStatus('');
        });

        editor.onFocus(() => {
            if (!wasmRequested) {
                loadWasm()
                    .then(activateEditorLanguageServices)
                    .catch(() => { /* surfaced via status */ });
            } else if (wasmModulePromise) {
                wasmModulePromise
                    .then(activateEditorLanguageServices)
                    .catch(() => { /* surfaced via status */ });
            }
        });

        if (sourceUrl || scenarioUrl) {
            void loadExternalSource();
        }

        return widget;
    }

    function buildWidget(codeEl, monaco) {
        const pre = codeEl.parentElement;
        let originalSource = codeEl.textContent.replace(/\n$/, '');
        const liveDirectives = parseLiveDirectives(originalSource);
        let sourceUrl = liveDirectives.source || '';
        const scenarioUrl = liveDirectives.scenario || '';
        let modelOverride = liveDirectives.model || '';
        const directiveSourceOnly = liveDirectives.mode === 'source';
        let sourceOnly = directiveSourceOnly;
        let scenarioText = '';
        let scenarioConfig = null;
        let scenarioDescriptor = null;
        const wantsRadialViz = /\bviz-radial\b/.test(codeEl.className || '');
        const gpuDefault = /\bgpu\b/.test(codeEl.className || '');
        const workspaceFocusPath = () =>
            repoExamplesRelativePath(scenarioUrl || sourceUrl);
        const scenarioWorkspacePath = () =>
            repoExamplesRelativePath(scenarioUrl) || 'rumoca-scenario.toml';

        function scenarioWorkspaceSources(text = scenarioText) {
            return JSON.stringify({ [scenarioWorkspacePath()]: text });
        }

        async function parseScenarioTextWithWasm(text) {
            const wasm = await loadWasm();
            if (typeof wasm.scenario_get_scenario_config_full !== 'function') {
                throw new Error('scenario_get_scenario_config_full missing in this WASM build');
            }
            const parsed = JSON.parse(wasm.scenario_get_scenario_config_full(
                scenarioWorkspaceSources(text),
                scenarioWorkspacePath()
            ));
            if (!parsed?.ok) {
                throw new Error(parsed?.error || 'failed to parse scenario config');
            }
            return parsed;
        }

        async function renderScenarioConfigWithWasm(config) {
            const wasm = await loadWasm();
            if (typeof wasm.scenario_set_scenario_config !== 'function') {
                throw new Error('scenario_set_scenario_config missing in this WASM build');
            }
            const response = JSON.parse(wasm.scenario_set_scenario_config(
                scenarioWorkspacePath(),
                JSON.stringify(config)
            ));
            const write = Array.isArray(response?.writes) ? response.writes[0] : null;
            if (!write || typeof write.content !== 'string') {
                throw new Error('scenario config render did not return TOML content');
            }
            return write.content.replace(/\n$/, '');
        }

        function inlineScenarioConfig(source, model) {
            const script = widget?.vizEditor?.getValue?.() || '';
            const views = [];
            if (script) {
                widget.viewerScriptsByPath.set('viewer.js', script);
                views.push({
                    id: 'viewer',
                    title: 'Viewer',
                    type: '3d',
                    scriptPath: 'viewer.js',
                });
            }
            views.push({
                id: 'states_time',
                title: 'States vs Time',
                type: 'timeseries',
                x: 'time',
                y: ['*states'],
            });
            const dt = experimentNumber(source, 'Interval');
            return {
                rumoca: { version: '1', task: 'simulate' },
                model: {
                    file: sourceUrl ? repoExamplesRelativePath(sourceUrl) || sourceUrl : modelFileNameForInlineScenario(model),
                    name: model,
                },
                sim: {
                    solver: experimentSolver(source),
                    t_end: experimentNumber(source, 'StopTime') || 10,
                    ...(dt ? { dt } : {}),
                },
                viewer: { mode: 'results_panel' },
                plot: { views },
            };
        }

        async function ensureScenarioConfigForSettings() {
            if (scenarioConfig) {
                return;
            }
            const wasm = await loadWasm();
            const source = editor.getValue();
            const model = modelOverride || inferModelName(wasm, source);
            if (!model) {
                throw new Error('No model/block/class found in this example.');
            }
            await applyScenarioText(
                await renderScenarioConfigWithWasm(inlineScenarioConfig(source, model)),
                { rerenderSettings: false }
            );
        }

        const container = document.createElement('div');
        container.className = 'rumoca-live';
        const editorHost = document.createElement('div');
        editorHost.className = 'rumoca-live-editor-host';

        const toolbar = document.createElement('div');
        toolbar.className = 'rumoca-live-toolbar';
        const runBtn = document.createElement('button');
        runBtn.type = 'button';
        runBtn.className = 'rumoca-live-button rumoca-live-run';
        runBtn.textContent = '▶ Simulate';
        const stopBtn = document.createElement('button');
        stopBtn.type = 'button';
        stopBtn.className = 'rumoca-live-button';
        stopBtn.textContent = 'Stop';
        stopBtn.disabled = true;
        const daeBtn = document.createElement('button');
        daeBtn.type = 'button';
        daeBtn.className = 'rumoca-live-button';
        daeBtn.textContent = 'Show DAE';
        const resetBtn = document.createElement('button');
        resetBtn.type = 'button';
        resetBtn.className = 'rumoca-live-button';
        resetBtn.textContent = 'Reset';
        const settingsBtn = document.createElement('button');
        settingsBtn.type = 'button';
        settingsBtn.className = 'rumoca-live-button';
        settingsBtn.textContent = 'Settings';
        const solverLabel = document.createElement('label');
        solverLabel.className = 'rumoca-live-solver';
        const solverSelect = document.createElement('select');
        solverSelect.className = 'rumoca-live-solver-select';
        let bdfOption = null;
        for (const { value, label } of [
            { value: 'auto', label: 'Auto' },
            { value: 'rk-like', label: 'RK45 (explicit)' },
            { value: 'bdf', label: 'BDF (stiff)' },
        ]) {
            const opt = document.createElement('option');
            opt.value = value;
            opt.textContent = label;
            if (value === 'bdf') {
                // Enabled later iff the browser supports the diffsol addon.
                opt.disabled = true;
                opt.title = 'Checking browser support…';
                bdfOption = opt;
            }
            solverSelect.appendChild(opt);
        }
        solverLabel.append(document.createTextNode('Solver '), solverSelect);
        const gpuLabel = document.createElement('label');
        gpuLabel.className = 'rumoca-live-gpu';
        const gpuCheck = document.createElement('input');
        gpuCheck.type = 'checkbox';
        gpuCheck.checked = gpuDefault;
        gpuLabel.append(gpuCheck, document.createTextNode(' GPU'));
        gpuLabel.title = 'Run on WebGPU (wgsl-solve backend; experimental)';
        const liveLabel = document.createElement('label');
        liveLabel.className = 'rumoca-live-gpu';
        const liveCheck = document.createElement('input');
        liveCheck.type = 'checkbox';
        liveCheck.checked = false;
        liveLabel.append(liveCheck, document.createTextNode(' Interactive'));
        liveLabel.title = 'Keep registered input sliders live during WasmSimulationSession runs';
        const scenarioControlsPending = Boolean(scenarioUrl);
        solverLabel.hidden = scenarioControlsPending;
        gpuLabel.hidden = scenarioControlsPending;
        liveLabel.hidden = scenarioControlsPending;
        daeBtn.hidden = scenarioControlsPending;
        const status = document.createElement('span');
        status.className = 'rumoca-live-status';
        toolbar.append(runBtn, stopBtn, daeBtn, resetBtn, settingsBtn, solverLabel, gpuLabel, liveLabel, status);
        // Enable the stiff (BDF/diffsol) option only when the browser can load
        // the relaxed-SIMD addon; otherwise leave it greyed out with a reason.
        async function refreshBdfAvailability() {
            if (!bdfOption) {
                return;
            }
            const driver = await loadDiffsolDriver();
            const ok = driver && typeof driver.diffsolAvailable === 'function'
                ? await driver.diffsolAvailable(resolvedPkgBase)
                : false;
            if (ok) {
                bdfOption.disabled = false;
                bdfOption.title = 'Stiff/implicit solver (diffsol)';
            } else {
                bdfOption.disabled = true;
                bdfOption.title =
                    'The stiff (diffsol) solver needs a browser with WebAssembly '
                    + 'relaxed-SIMD (Chrome 114+, Firefox 120+, Safari 16.4+).';
                if (solverSelect.value === 'bdf') {
                    solverSelect.value = 'auto';
                }
            }
        }

        const progress = document.createElement('div');
        progress.className = 'rumoca-live-progress';
        progress.hidden = true;
        const progressFill = document.createElement('div');
        progressFill.className = 'rumoca-live-progress-fill';
        progress.appendChild(progressFill);

        const output = document.createElement('div');
        output.className = 'rumoca-live-output';
        output.hidden = true;
        const settingsPanel = document.createElement('div');
        settingsPanel.className = 'rumoca-live-settings';
        settingsPanel.hidden = true;

        container.append(editorHost, toolbar, settingsPanel, progress, output);
        pre.replaceWith(container);

        const editor = monaco
            ? createMonacoEditor(monaco, editorHost, originalSource)
            : createTextareaEditor(editorHost, originalSource);
        if (editor.model) {
            monacoModelWidgets.set(editor.model, null);
        }
        if (sourceUrl || scenarioUrl) {
            runBtn.disabled = true;
            daeBtn.disabled = true;
            resetBtn.disabled = true;
            settingsBtn.disabled = true;
            status.textContent = `Loading ${scenarioUrl || sourceUrl}…`;
        }

        const lastRunMs = {};
        let progressTimer = null;
        // When a run reports its own phases (the GPU path), `phase`
        // overrides the elapsed-time estimate: a fraction renders a real
        // progress fill, null renders the indeterminate stripe.
        let phase = null;
        const setPhase = (label, fraction) => {
            phase = { label, fraction, since: performance.now() };
            renderProgressNow();
        };
        let renderProgressNow = () => {};
        const beginProgress = (key, label) => {
            const started = performance.now();
            const expected = lastRunMs[key];
            phase = null;
            progress.hidden = false;
            const tick = () => {
                const elapsed = performance.now() - started;
                if (phase) {
                    const determinate = phase.fraction !== null
                        && phase.fraction !== undefined;
                    progressFill.classList.toggle(
                        'rumoca-live-progress-indeterminate', !determinate);
                    if (determinate) {
                        progressFill.style.width =
                            `${(Math.min(1, phase.fraction) * 100).toFixed(1)}%`;
                    } else {
                        progressFill.style.width = '100%';
                    }
                    const pct = determinate
                        ? ` ${(phase.fraction * 100).toFixed(0)}% ·` : '';
                    status.textContent =
                        `${phase.label} ·${pct} ${(elapsed / 1000).toFixed(0)} s`;
                    return;
                }
                progressFill.classList.toggle(
                    'rumoca-live-progress-indeterminate', !expected);
                if (expected) {
                    const pct = Math.min(97, (elapsed / expected) * 100);
                    progressFill.style.width = `${pct.toFixed(1)}%`;
                } else {
                    progressFill.style.width = '100%';
                }
                const suffix = expected
                    ? ` ${(elapsed / 1000).toFixed(0)} / ~${(expected / 1000).toFixed(0)} s`
                    : ` ${(elapsed / 1000).toFixed(0)} s (first run — no estimate yet)`;
                status.textContent = label + suffix;
            };
            renderProgressNow = tick;
            tick();
            progressTimer = setInterval(tick, 250);
            return started;
        };
        const endProgress = (key, started, succeeded) => {
            clearInterval(progressTimer);
            progressTimer = null;
            progress.hidden = true;
            phase = null;
            renderProgressNow = () => {};
            if (succeeded) {
                lastRunMs[key] = performance.now() - started;
            }
        };

        const widget = {
            busy: false,
            vizEditor: null,
            viewerScriptsByPath: new Map(),
            gpuPrep: null,
            interactiveRunner: null,
            generatedEditors: [],
            activeRun: null,
            rerunAfterStop: false,
            paramOverrides: {},
            structuralParams: {},
            liveInputs: {},
            plotSeries: null,
            tunerInputs: [],
            languageServicesActive: false,
            diagnosticsTimer: null,
            diagnosticsSubscription: null,
            diagnosticsRequestId: 0,
            sourceRootCacheUrlPromise: null,
            sourceRootsSessionKey: null,
            workspaceSourceRootsPromise: null,
            scenarioSourceRootPaths() {
                return normalizeStringArray(scenarioConfig?.source_roots);
            },
            scenarioParameterOverrides() {
                const parameters = scenarioConfig?.parameters;
                if (!parameters || typeof parameters !== 'object' || Array.isArray(parameters)) {
                    return {};
                }
                const overrides = {};
                for (const [name, value] of Object.entries(parameters)) {
                    const number = Number(value);
                    if (name && Number.isFinite(number)) {
                        overrides[name] = number;
                    }
                }
                return overrides;
            },
            async workspaceSourceRootPaths(wasm) {
                const focusPath = workspaceFocusPath();
                if (!focusPath) {
                    return [];
                }
                if (!widget.workspaceSourceRootsPromise) {
                    widget.workspaceSourceRootsPromise = (async () => {
                        const workspaceSources = await loadWorkspaceSourcesForFocus(focusPath);
                        if (Object.keys(workspaceSources).length === 0) {
                            return [];
                        }
                        if (!wasm || typeof wasm.workspace_effective_source_roots !== 'function') {
                            throw new Error('workspace_effective_source_roots missing in this WASM build');
                        }
                        return normalizeStringArray(JSON.parse(wasm.workspace_effective_source_roots(
                            JSON.stringify(workspaceSources),
                            focusPath
                        )));
                    })().catch((error) => {
                        widget.workspaceSourceRootsPromise = null;
                        throw error;
                    });
                }
                return widget.workspaceSourceRootsPromise;
            },
            async sourceRootPaths(wasm) {
                const sourceRoots = uniqueStrings([
                    ...(await widget.workspaceSourceRootPaths(wasm)),
                    ...widget.scenarioSourceRootPaths(),
                ]);
                if (widget.scenarioSourceRootPaths().length > 0 && sourceRoots.length === 0) {
                    throw new Error(
                        `No effective Modelica source roots were resolved for ${workspaceFocusPath()}. ` +
                        'Check the scenario source_roots and the staged source-root cache.'
                    );
                }
                return sourceRoots;
            },
            sourceRootCacheUrl(wasm) {
                if (!widget.sourceRootCacheUrlPromise) {
                    widget.sourceRootCacheUrlPromise = widget.sourceRootPaths(wasm)
                        .then(loadSourceRootCacheUrlForRoots)
                        .catch((error) => {
                            widget.sourceRootCacheUrlPromise = null;
                            throw error;
                        });
                }
                return widget.sourceRootCacheUrlPromise;
            },
            async refreshSourceRootStatus(wasm) {
                if (!workspaceFocusPath() || widget.busy) {
                    return;
                }
                try {
                    const roots = await widget.sourceRootPaths(wasm);
                    if (roots.length > 0 && !widget.busy) {
                        widget.setStatus(`${roots.length} Modelica source roots loaded from rumoca-workspace.toml.`);
                    } else if (!widget.busy) {
                        widget.setStatus('');
                    }
                } catch (error) {
                    if (!widget.busy) {
                        widget.setStatus(error instanceof Error ? error.message : String(error));
                    }
                    throw error;
                }
            },
            requestRun() {
                if (!widget.busy) {
                    runBtn.click();
                }
            },
            restartRun() {
                if (widget.busy) {
                    widget.rerunAfterStop = true;
                    stopCurrentRun();
                } else {
                    runBtn.click();
                }
            },
            getSource() { return editor.getValue(); },
            setSource(text) { editor.setValue(text); },
            setStatus(text) { status.textContent = text; },
            onWasmReady(wasm) {
                if (widget.languageServicesActive) {
                    activateEditorLanguageServices(wasm);
                }
            },
        };
        widgets.push(widget);
        if (editor.model) {
            monacoModelWidgets.set(editor.model, widget);
        }

        function scheduleDiagnostics(wasm) {
            if (!monaco || !editor.model) {
                return;
            }
            clearTimeout(widget.diagnosticsTimer);
            widget.diagnosticsTimer = setTimeout(
                () => updateDiagnostics(wasm, monaco, editor.model, widget),
                DIAGNOSTIC_DEBOUNCE_MS
            );
        }

        function activateEditorLanguageServices(wasm) {
            refreshBdfAvailability();
            widget.languageServicesActive = true;
            if (!monaco || !editor.model) {
                widget.refreshSourceRootStatus(wasm).catch(() => { /* shown in status */ });
                return;
            }
            updateDiagnostics(wasm, monaco, editor.model, widget);
            widget.refreshSourceRootStatus(wasm).catch(() => { /* shown in status */ });
            if (!widget.diagnosticsSubscription) {
                widget.diagnosticsSubscription = editor.onChange(() => scheduleDiagnostics(wasm));
            }
        }

        function hasSourceRoots() {
            return widget.scenarioSourceRootPaths().length > 0;
        }

        function stopInteractiveRunner() {
            if (widget.interactiveRunner) {
                widget.interactiveRunner.dispose();
                widget.interactiveRunner = null;
            }
            if (!widget.busy) {
                stopBtn.disabled = true;
            }
        }

        function stopCurrentRun() {
            stopInteractiveRunner();
            if (widget.activeRun && !widget.activeRun.signal.aborted) {
                widget.activeRun.abort();
            }
        }

        function scenarioWantsInputRuntime(runtime) {
            if (!scenarioConfig) {
                return false;
            }
            if (runtime?.scenarioUsesInputRuntime) {
                return runtime.scenarioUsesInputRuntime(scenarioConfig);
            }
            return Boolean(scenarioConfig.input);
        }

        function scenarioViewerMode() {
            return trimMaybeString(scenarioConfig?.viewer?.mode).toLowerCase();
        }

        function scenarioTask() {
            return trimMaybeString(scenarioConfig?.rumoca?.task).toLowerCase();
        }

        function isCodegenScenario() {
            return scenarioTask() === 'codegen';
        }

        function codegenTarget() {
            return trimMaybeString(scenarioConfig?.codegen?.target);
        }

        function modelWorkspacePath(model) {
            return repoExamplesRelativePath(sourceUrl) || modelFileNameForInlineScenario(model);
        }

        function workspaceSourcesJson(source, model) {
            return JSON.stringify({ [modelWorkspacePath(model)]: source });
        }

        function scenarioRelativeUrl(path) {
            const base = scenarioUrl || sourceUrl || window.location.href;
            return new URL(path, pageUrl(base)).href;
        }

        function scenarioAssetBaseUrl() {
            const assetDir = trimMaybeString(scenarioConfig?.transport?.http?.asset_dir);
            if (!assetDir) {
                return '';
            }
            const url = scenarioRelativeUrl(assetDir.endsWith('/') ? assetDir : `${assetDir}/`);
            return url.endsWith('/') ? url : `${url}/`;
        }

        async function scenarioInteractiveScript() {
            const scriptPath = trimMaybeString(scenarioConfig?.transport?.http?.scene);
            if (scriptPath) {
                const response = await fetch(scenarioRelativeUrl(scriptPath));
                if (!response.ok) {
                    throw new Error(`${scriptPath}: ${response.status} ${response.statusText}`);
                }
                return response.text();
            }
            return '';
        }

        async function startExternalInteractiveViewer({
            wasm,
            THREE,
            source,
            model,
            interactiveRuntime,
            sourceRootCacheUrl,
            scriptText,
        }) {
            const title = `${model} - Rumoca interactive viewer`;
            const external = window.open('', `rumoca-${model}-interactive`);
            if (!external) {
                throw new Error('External viewer window was blocked. Allow popups for this page and try again.');
            }
            external.document.open();
            external.document.write(`<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>Rumoca interactive viewer</title>
<style>
html, body { margin: 0; width: 100%; height: 100%; overflow: hidden; background: #071825; color: #d8e6f3; font-family: system-ui, sans-serif; }
#viewer { position: fixed; inset: 0; outline: none; }
#status { position: fixed; top: 0.75rem; left: 0.75rem; z-index: 4; padding: 0.35rem 0.55rem; border-radius: 5px; background: rgba(5, 10, 16, 0.72); font: 12px monospace; }
.rumoca-interactive-canvas { display: block; width: 100%; height: 100%; touch-action: none; }
.rumoca-interactive-flight-hud { position: absolute; inset: 0; z-index: 2; pointer-events: none; }
.rumoca-interactive-controls { position: absolute; left: 0.75rem; right: 0.75rem; bottom: 0.75rem; z-index: 3; display: flex; gap: 0.4rem; pointer-events: auto; }
.rumoca-interactive-controls button { border: 1px solid rgba(255,255,255,0.3); border-radius: 5px; background: rgba(5,10,16,0.78); color: #f4f7fb; font: 13px system-ui, sans-serif; min-height: 2.3em; padding: 0.45em 0.65em; touch-action: none; }
.rumoca-interactive-controls.is-capturing .rumoca-interactive-capture-toggle { border-color: #7fc7ff; background: rgba(16,103,153,0.9); }
</style>
</head>
<body>
<div id="viewer" tabindex="0"></div>
<div id="status">starting</div>
</body>
</html>`);
            external.document.close();
            external.document.title = title;
            const host = external.document.getElementById('viewer');
            const statusNode = external.document.getElementById('status');
            const runner = await interactiveRuntime.createInteractiveSimulation({
                wasm,
                THREE,
                source,
                modelName: model,
                config: scenarioConfig,
                sourceRootCacheUrl,
                container: host,
                scriptText,
                assetBaseUrl: scenarioAssetBaseUrl(),
                onStatus: (text) => {
                    widget.setStatus(text);
                    statusNode.textContent = `${text || 'running'} · Esc releases capture · Q stops`;
                },
            });
            external.addEventListener('beforeunload', () => {
                if (widget.interactiveRunner?.runner === runner) {
                    widget.interactiveRunner = null;
                }
                runner.dispose();
            }, { once: true });
            runner.start();
            statusNode.textContent = 'running · Esc releases capture · Q stops';
            return {
                runner,
                reset() {
                    runner.reset();
                    statusNode.textContent = 'reset · Esc releases capture · Q stops';
                },
                dispose() {
                    runner.dispose();
                    if (!external.closed) {
                        external.close();
                    }
                },
            };
        }

        function refreshExecutionAvailability(loading = false) {
            const codegen = isCodegenScenario();
            const galecCodegen = codegen && GALEC_CODEGEN_TARGETS.has(codegenTarget());
            runBtn.textContent = galecCodegen ? 'Generate .alg' : codegen ? 'Generate Code' : '▶ Simulate';
            runBtn.title = codegen
                ? 'Render the code generation target configured by this scenario'
                : '';
            if (codegen) {
                gpuCheck.checked = false;
                liveCheck.checked = false;
            }
            solverLabel.hidden = codegen;
            gpuLabel.hidden = codegen;
            liveLabel.hidden = codegen;
            daeBtn.hidden = codegen;
            const unavailable = sourceOnly;
            runBtn.disabled = loading || unavailable;
            daeBtn.disabled = codegen || loading || unavailable;
            if (loading) {
                return;
            }
            if (unavailable) {
                status.textContent = 'Source only; run the scenario command below.';
            } else if (hasSourceRoots()) {
                status.textContent = 'Source roots configured; dependencies load when editing.';
            } else {
                status.textContent = '';
            }
        }

        function syncScenarioControls() {
            if (!scenarioConfig) return;
            const sim = scenarioConfig.sim || {};
            const solver = trimMaybeString(sim.solver).toLowerCase();
            if (solver === 'rk-like' || solver === 'bdf') {
                solverSelect.value = solver;
            } else {
                solverSelect.value = 'auto';
            }
            const views = Array.isArray(scenarioConfig.plot?.views)
                ? scenarioConfig.plot.views
                : [];
            const firstSeries = views.find((view) => Array.isArray(view.y) && view.y.length > 0);
            if (firstSeries) {
                widget.plotSeries = firstSeries.y.filter((name) => name !== '*states');
            }
        }

        async function renderScenarioSettingsFrame() {
            await ensureScenarioConfigForSettings();
            if (!scenarioConfig) {
                settingsPanel.replaceChildren();
                return;
            }
            try {
                const shared = await loadVisualizationShared();
                const wasm = await loadWasm();
                const effectiveSourceRootPaths = await widget.sourceRootPaths(wasm);
                const parameterMetadata = await loadScenarioParameterMetadata(wasm, effectiveSourceRootPaths);
                const frame = document.createElement('iframe');
                frame.className = 'rumoca-live-settings-frame';
                frame.title = 'Rumoca Scenario Config';
                frame.srcdoc = shared.buildScenarioConfigDocument({
                    path: scenarioWorkspacePath(),
                    config: scenarioConfig,
                    descriptor: scenarioDescriptor,
                    effectiveSourceRootPaths,
                    parameterMetadata,
                });
                settingsPanel.replaceChildren(frame);
            } catch (error) {
                settingsPanel.replaceChildren();
                throw error;
            }
        }

        async function loadScenarioParameterMetadata(wasm, effectiveSourceRootPaths, selection = {}) {
            const source = editor.getValue();
            const modelName = trimMaybeString(selection.modelName)
                || trimMaybeString(scenarioConfig?.model?.name)
                || modelOverride
                || inferModelName(wasm, source);
            if (!source || !modelName) {
                return [];
            }
            const sourceRootCacheUrl = await widget.sourceRootCacheUrl(wasm);
            const payload = {
                path: scenarioWorkspacePath(),
                source,
                modelName,
                fallback: { sourceRootPaths: effectiveSourceRootPaths },
                sourceRootCacheUrl,
                timeoutMs: 8000,
            };
            try {
                const raw = await runHeavy('rumoca.model.parameterMetadata', payload, async () => {
                    await ensureWasmSourceRootsLoaded(wasm, widget);
                    if (typeof wasm.model_parameter_metadata !== 'function') {
                        throw new Error('model_parameter_metadata missing in this WASM build');
                    }
                    return wasm.model_parameter_metadata(source, modelName);
                });
                const parsed = JSON.parse(raw);
                return Array.isArray(parsed) ? parsed : [];
            } catch {
                return [];
            }
        }

        function renderScenarioRawSettings() {
            settingsPanel.innerHTML = `
                <label class="rumoca-live-settings-raw">rumoca-scenario.toml
                  <textarea data-setting="raw" rows="16"></textarea>
                </label>
                <div class="rumoca-live-settings-actions">
                  <button type="button" class="rumoca-live-button" data-action="apply-toml">Apply TOML</button>
                  <button type="button" class="rumoca-live-button" data-action="show-gui">GUI</button>
                </div>`;
            settingsPanel.querySelector('[data-setting="raw"]').value = scenarioText;
        }

        async function saveScenarioConfigEdits(edits) {
            const shared = await loadVisualizationShared();
            const config = shared.applyScenarioConfigEdits(scenarioConfig || {}, edits || []);
            await applyScenarioText(await renderScenarioConfigWithWasm(config));
            widget.setStatus('Scenario settings saved');
            return { ok: true };
        }

        async function handleScenarioConfigRequest(method, payload = {}) {
            if (method === 'parameterMetadata') {
                const wasm = await loadWasm();
                const effectiveSourceRootPaths = await widget.sourceRootPaths(wasm);
                return await loadScenarioParameterMetadata(wasm, effectiveSourceRootPaths, payload);
            }
            if (method === 'save') {
                return await saveScenarioConfigEdits(payload?.edits);
            }
            if (method === 'run') {
                await saveScenarioConfigEdits(payload?.edits);
                runBtn.click();
                return { ok: true, message: 'Scenario run requested' };
            }
            if (method === 'toggleRaw') {
                renderScenarioRawSettings();
                return { ok: true };
            }
            throw new Error(`Unknown scenario config request: ${method}`);
        }

        async function applyScenarioText(nextText, options = {}) {
            const rerenderSettings = options?.rerenderSettings !== false;
            const parsed = await parseScenarioTextWithWasm(nextText);
            scenarioText = nextText;
            scenarioConfig = parsed.config || {};
            scenarioDescriptor = parsed.descriptor || {};
            widget.sourceRootCacheUrlPromise = null;
            widget.sourceRootsSessionKey = null;
            modelOverride = trimMaybeString(scenarioDescriptor.model)
                || trimMaybeString(scenarioConfig.model?.name)
                || modelOverride;
            syncScenarioControls();
            if (rerenderSettings && !settingsPanel.hidden) {
                await renderScenarioSettingsFrame();
            }
            refreshExecutionAvailability();
        }

        scenarioConfigHostHandlers.set(scenarioWorkspacePath(), handleScenarioConfigRequest);

        async function loadScenarioSource() {
            const response = await fetch(pageUrl(scenarioUrl));
            if (!response.ok) {
                throw new Error(`${response.status} ${response.statusText}`);
            }
            await applyScenarioText((await response.text()).replace(/\n$/, ''));
            sourceOnly = directiveSourceOnly;
            sourceUrl = new URL(trimMaybeString(scenarioConfig.model?.file), pageUrl(scenarioUrl)).href;
        }

        async function loadExternalSource() {
            if (!sourceUrl && !scenarioUrl) {
                return;
            }
            try {
                if (scenarioUrl) {
                    await loadScenarioSource();
                }
                const response = await fetch(sourceUrl.startsWith('http') ? sourceUrl : pageUrl(sourceUrl));
                if (!response.ok) {
                    throw new Error(`${response.status} ${response.statusText}`);
                }
                originalSource = (await response.text()).replace(/\n$/, '');
                editor.setValue(originalSource);
                resetBtn.disabled = false;
                settingsBtn.disabled = false;
                refreshExecutionAvailability();
                if (widget.languageServicesActive && wasmModulePromise && monaco && editor.model) {
                    wasmModulePromise.then((wasm) => {
                        updateDiagnostics(wasm, monaco, editor.model, widget);
                        widget.refreshSourceRootStatus(wasm).catch(() => { /* shown in status */ });
                    });
                }
            } catch (error) {
                const message = error instanceof Error ? error.message : String(error);
                status.textContent = `Failed to load ${scenarioUrl || sourceUrl}: ${message}`;
                resetBtn.disabled = false;
                settingsBtn.disabled = false;
            }
        }
        loadExternalSource();

        // Editing intent: start the WASM download so diagnostics and
        // completion are ready by the time they are wanted.
        editor.onFocus(() => {
            if (!wasmRequested) {
                loadWasm()
                    .then(activateEditorLanguageServices)
                    .catch(() => { /* surfaced via status */ });
            } else if (wasmModulePromise) {
                wasmModulePromise
                    .then(activateEditorLanguageServices)
                    .catch(() => { /* surfaced via status */ });
            }
        });

        const showError = (error) => {
            clearGeneratedEditors();
            const message = error instanceof Error ? error.message : String(error);
            const errBox = document.createElement('pre');
            errBox.className = 'rumoca-live-error';
            errBox.textContent = message;
            output.replaceChildren(errBox);
            output.hidden = false;
            widget.setStatus('Failed');
        };

        const withWasm = async (key, busyLabel, action) => {
            if (widget.busy) {
                return;
            }
            const run = new AbortController();
            widget.activeRun = run;
            runBtn.disabled = true;
            stopBtn.disabled = false;
            daeBtn.disabled = true;
            widget.busy = true;
            clearGeneratedEditors();
            // Parameters are frozen during a simulation: lock ordinary tuners.
            // Registered input tuners stay enabled in Interactive mode because
            // the WasmSimulationSession path reads their current values before each advance.
            for (const input of widget.tunerInputs) {
                input.disabled = !(liveCheck.checked && input.dataset.rumocaLiveInput);
            }
            let started = null;
            let succeeded = false;
            try {
                const wasm = await loadWasm();
                throwIfAborted(run.signal);
                started = beginProgress(key, busyLabel);
                const source = editor.getValue();
                const model = modelOverride || inferModelName(wasm, source);
                if (!model) {
                    throw new Error('No model/block/class found in this example.');
                }
                activateEditorLanguageServices(wasm);
                await action(wasm, source, model, run.signal);
                throwIfAborted(run.signal);
                succeeded = true;
            } catch (error) {
                if (isAbortError(error)) {
                    widget.setStatus('Stopped');
                } else {
                    showError(error);
                }
            } finally {
                if (started !== null) {
                    endProgress(key, started, succeeded);
                }
                if (widget.activeRun === run) {
                    widget.activeRun = null;
                    widget.busy = false;
                    runBtn.disabled = false;
                    stopBtn.disabled = !widget.interactiveRunner;
                    daeBtn.disabled = isCodegenScenario();
                    for (const input of widget.tunerInputs) {
                        input.disabled = false;
                    }
                    if (widget.rerunAfterStop) {
                        widget.rerunAfterStop = false;
                        setTimeout(() => runBtn.click(), 0);
                    }
                }
            }
        };

        async function renderCodegenFiles(wasm, source, model, target, sourceRootCacheUrl, signal) {
            const workspaceSources = workspaceSourcesJson(source, model);
            const filesJson = await runHeavy('codegen', {
                source,
                model,
                target,
                workspaceSources,
                sourceRootCacheUrl,
            }, async () => {
                const runtime = await loadRuntimeDriver();
                if (GALEC_CODEGEN_TARGETS.has(target)) {
                    return JSON.stringify(await runtime.renderGalecFilesWithRuntime({
                        pkgBase: resolvedPkgBase || await locatePkgBase(),
                        workspaceSources,
                        modelName: model,
                        target,
                    }));
                }
                if (!/^[A-Za-z0-9_.-]+$/.test(target)) {
                    throw new Error('The book widget supports built-in codegen targets only.');
                }
                await runtime.ensureParsedSourceRootCache(wasm, sourceRootCacheUrl);
                const compiled = JSON.parse(wasm.compile(source, model));
                const daeJson = JSON.stringify(compiled.dae_native || compiled.dae);
                const rendered = wasm.render_target(daeJson, model, target, '', '{}');
                return JSON.stringify(Array.isArray(rendered?.files) ? rendered.files : []);
            }, signal);
            throwIfAborted(signal);
            const files = JSON.parse(filesJson);
            if (!Array.isArray(files) || files.length === 0) {
                throw new Error(`Target '${target}' did not render any files.`);
            }
            return files;
        }

        async function runCodegenScenario(wasm, source, model, signal) {
            const target = codegenTarget();
            if (!target) {
                throw new Error('Codegen scenario is missing [codegen].target.');
            }
            const renderTarget = GALEC_CODEGEN_TARGETS.has(target) ? 'galec' : target;
            const sourceRootCacheUrl = await widget.sourceRootCacheUrl(wasm);
            if (sourceRootCacheUrl && !GALEC_CODEGEN_TARGETS.has(renderTarget)) {
                setPhase('Loading source-root cache', null);
                await ensureWasmSourceRootsLoaded(wasm, widget);
            }
            setPhase(`Rendering ${renderTarget}`, null);
            const files = await renderCodegenFiles(
                wasm,
                source,
                model,
                renderTarget,
                sourceRootCacheUrl,
                signal,
            );
            throwIfAborted(signal);
            renderCodegenResult(files, target);
        }

        function clearGeneratedEditors() {
            for (const generatedEditor of widget.generatedEditors) {
                generatedEditor.dispose?.();
            }
            widget.generatedEditors = [];
        }

        function codegenLanguageForPath(path) {
            const lower = String(path || '').toLowerCase();
            if (lower.endsWith('.alg')) return 'galec';
            if (lower.endsWith('.c') || lower.endsWith('.h')) return 'c';
            if (lower.endsWith('.json')) return 'json';
            if (lower.endsWith('.xml')) return 'xml';
            if (lower.endsWith('.toml')) return 'toml';
            return 'plaintext';
        }

        function codegenStageLabel(path) {
            const lower = String(path || '').toLowerCase();
            if (lower.endsWith('.alg')) return 'GALEC Algorithm Code';
            if (lower.endsWith('.h')) return 'C header';
            if (lower.endsWith('.c')) return 'C source';
            return 'Generated file';
        }

        function modelIdentifierFromAlgPath(path) {
            const leaf = String(path || 'model.alg').split('/').pop() || 'model.alg';
            const stem = leaf.replace(/\.alg$/i, '') || 'model';
            return stem.replace(/[^A-Za-z0-9_.-]+/g, '_').replace(/^_+|_+$/g, '') || 'model';
        }

        function galecCGenerationTarget(target) {
            return target === 'efmi' || target === 'galec-c'
                ? target
                : 'galec-c';
        }

        async function generateCFromAlg(generatedEditor, algPath, configuredTarget, button) {
            const target = galecCGenerationTarget(configuredTarget);
            const algSource = generatedEditor.getValue();
            const modelName = modelIdentifierFromAlgPath(algPath);
            button.disabled = true;
            widget.setStatus(`Generating C/H from ${algPath}…`);
            try {
                const runtime = await loadRuntimeDriver();
                const cFiles = await runtime.renderGalecCFromAlgWithRuntime({
                    pkgBase: resolvedPkgBase || await locatePkgBase(),
                    algSource,
                    fileName: algPath,
                    modelName,
                    target,
                });
                renderCodegenResult(
                    [{ path: algPath, content: algSource }, ...cFiles],
                    target,
                );
                widget.setStatus(`Generated C/H from ${algPath}`);
            } catch (error) {
                showError(error);
            } finally {
                button.disabled = false;
            }
        }

        function appendCodegenPipeline(shell, files) {
            const hasAlg = files.some((file) => String(file.path || '').toLowerCase().endsWith('.alg'));
            const hasC = files.some((file) => /\.(?:c|h)$/i.test(String(file.path || '')));
            const stages = ['Modelica source'];
            if (hasAlg) stages.push('GALEC .alg');
            if (hasC) stages.push('C .h/.c');
            const pipeline = document.createElement('div');
            pipeline.className = 'rumoca-live-codegen-pipeline';
            for (const [index, label] of stages.entries()) {
                const stage = document.createElement('span');
                stage.className = 'rumoca-live-codegen-stage';
                stage.textContent = label;
                pipeline.appendChild(stage);
                if (index + 1 < stages.length) {
                    const arrow = document.createElement('span');
                    arrow.className = 'rumoca-live-codegen-arrow';
                    arrow.textContent = '→';
                    pipeline.appendChild(arrow);
                }
            }
            shell.appendChild(pipeline);
        }

        function appendCodegenFile(shell, file, index, target) {
            const path = trimMaybeString(file.path) || `file-${index + 1}`;
            const content = String(file.content ?? '');
            const language = codegenLanguageForPath(path);
            const details = document.createElement('details');
            details.open = index === 0;
            const summary = document.createElement('summary');
            const summaryMain = document.createElement('span');
            summaryMain.className = 'rumoca-live-codegen-summary-main';
            const stage = document.createElement('span');
            stage.className = 'rumoca-live-codegen-kind';
            stage.textContent = codegenStageLabel(path);
            const name = document.createElement('span');
            name.className = 'rumoca-live-codegen-name';
            name.textContent = path;
            summaryMain.append(stage, name);
            summary.appendChild(summaryMain);
            details.appendChild(summary);
            shell.appendChild(details);
            if (monaco) {
                const host = document.createElement('div');
                host.className = 'rumoca-live-codegen-editor';
                details.appendChild(host);
                const generatedEditor = createMonacoEditor(
                    monaco,
                    host,
                    content,
                    language,
                    {
                        maxLines: language === 'galec' ? 34 : MAX_EDITOR_LINES,
                        readOnly: language !== 'galec',
                    },
                );
                widget.generatedEditors.push(generatedEditor);
                details.addEventListener('toggle', () => {
                    if (details.open) {
                        generatedEditor.layout?.();
                    }
                });
                if (language === 'galec') {
                    activateGalecEditorLanguageServices(monaco, generatedEditor, path);
                    const actions = document.createElement('span');
                    actions.className = 'rumoca-live-codegen-actions';
                    const generateCButton = document.createElement('button');
                    generateCButton.type = 'button';
                    generateCButton.className = 'rumoca-live-button rumoca-live-codegen-action';
                    generateCButton.textContent = 'Generate C/H';
                    generateCButton.title = 'Generate C header/source from the current GALEC .alg editor text';
                    generateCButton.addEventListener('click', (event) => {
                        event.preventDefault();
                        event.stopPropagation();
                        void generateCFromAlg(generatedEditor, path, target, generateCButton);
                    });
                    actions.appendChild(generateCButton);
                    summary.appendChild(actions);
                }
            } else {
                const pre = document.createElement('pre');
                pre.className = 'rumoca-live-dae rumoca-live-codegen-pre';
                pre.textContent = content;
                details.appendChild(pre);
            }
        }

        function renderCodegenResult(files, target) {
            clearGeneratedEditors();
            const shell = document.createElement('div');
            shell.className = 'rumoca-live-codegen';
            output.replaceChildren(shell);
            output.hidden = false;
            appendCodegenPipeline(shell, files);
            files.forEach((file, index) => appendCodegenFile(shell, file, index, target));
            const hasOnlyAlg = files.length === 1
                && String(files[0]?.path || '').toLowerCase().endsWith('.alg');
            widget.setStatus(hasOnlyAlg
                ? 'Generated GALEC .alg; use Generate C/H in the .alg panel for the next step'
                : `Generated ${files.length} ${target} file(s)`);
        }

        runBtn.addEventListener('click', () => {
            if (isCodegenScenario()) {
                return withWasm('codegen', 'Generating code…', runCodegenScenario);
            }
            return withWasm('simulate', 'Compiling & simulating…', async (wasm, source, model, signal) => {
            stopInteractiveRunner();
            const sim = scenarioConfig?.sim || {};
            const solver = trimMaybeString(sim.solver).toLowerCase() || solverSelect.value || 'auto';
            const tEnd = Number.isFinite(sim.t_end) ? sim.t_end : 0;
            const dt = Number.isFinite(sim.dt) ? sim.dt : 0;
            const sourceRootCacheUrl = await widget.sourceRootCacheUrl(wasm);
            if (sourceRootCacheUrl) {
                setPhase('Loading source-root cache', null);
            }
            await ensureWasmSourceRootsLoaded(wasm, widget);
            throwIfAborted(signal);
            if (scenarioWantsInputRuntime(null)) {
                const interactiveRuntime = await loadInteractiveRuntime();
                const THREE = await loadThreeModule();
                const scriptText = await scenarioInteractiveScript();
                if (scenarioViewerMode() === 'external_web') {
                    setPhase('Opening external interactive viewer', null);
                    const runner = await startExternalInteractiveViewer({
                        wasm,
                        THREE,
                        source,
                        model,
                        interactiveRuntime,
                        sourceRootCacheUrl,
                        scriptText,
                    });
                    if (signal.aborted) {
                        runner?.dispose?.();
                        throw makeAbortError();
                    }
                    widget.interactiveRunner = runner;
                    output.replaceChildren(Object.assign(document.createElement('div'), {
                        className: 'rumoca-live-note',
                        textContent: 'External interactive viewer opened in a separate browser window. Press Esc there to release input capture; Q stops the simulation.',
                    }));
                    output.hidden = false;
                    widget.setStatus('External interactive simulation running');
                    return;
                }
                const shell = document.createElement('div');
                shell.className = 'rumoca-live-interactive';
                const host = document.createElement('div');
                host.className = 'rumoca-live-interactive-host';
                const help = document.createElement('div');
                help.className = 'rumoca-live-interactive-help';
                const keyboardHelp = Array.isArray(scenarioConfig?.viewer?.controls?.keyboard)
                    ? scenarioConfig.viewer.controls.keyboard
                        .map((control) => {
                            const keys = trimMaybeString(control?.keys);
                            const action = trimMaybeString(control?.action);
                            return keys && action ? `${keys}: ${action}` : '';
                        })
                        .filter(Boolean)
                        .join(', ')
                    : '';
                help.textContent = `Press Capture to enable mouse and configured keys${keyboardHelp ? ` (${keyboardHelp})` : ''}. Move to orbit/tilt, middle or right drag to pan, wheel to zoom, and Esc to release.`;
                shell.append(host, help);
                output.replaceChildren(shell);
                output.hidden = false;
                setPhase('Compiling interactive simulation session', null);
                const runner = await interactiveRuntime.createInteractiveSimulation({
                    wasm,
                    THREE,
                    source,
                    modelName: model,
                    config: scenarioConfig,
                    sourceRootCacheUrl,
                    container: host,
                    scriptText,
                    assetBaseUrl: scenarioAssetBaseUrl(),
                    onStatus: (text) => widget.setStatus(text),
                });
                if (signal.aborted) {
                    runner?.dispose?.();
                    throw makeAbortError();
                }
                widget.interactiveRunner = runner;
                widget.interactiveRunner.start();
                widget.setStatus('Interactive simulation running');
                return;
            }
            if (liveCheck.checked) {
                const liveSource = enableLiveInputModelSource(source);
                if (typeof wasm.prepare_gpu_simulation !== 'function') {
                    throw new Error('Interactive timing metadata needs prepare_gpu_simulation in this WASM build.');
                }
                const gpu = await loadGpuDriver();
                if (!gpu || typeof gpu.probeGpu !== 'function') {
                    throw new Error(
                        'Interactive airfoil stepping needs the WebGPU driver; '
                        + 'rebuild the package or use the non-interactive path.'
                    );
                }
                const adapter = await gpu.probeGpu();
                setPhase('Preparing interactive GPU session', null);
                const prep = JSON.parse(await runHeavy(
                    'prepare_gpu',
                    { source: liveSource, model, sourceRootCacheUrl },
                    async () => {
                        const runtime = await loadRuntimeDriver();
                        return runtime.prepareGpuSimulationWithRuntime({
                            wasm,
                            source: liveSource,
                            modelName: model,
                            sourceRootCacheUrl,
                        });
                    },
                    signal
                ));
                const variableNames = Array.isArray(prep.state_names)
                    ? prep.state_names.slice()
                    : [];
                if (variableNames.length === 0) {
                    throw new Error('Interactive GPU prep did not expose state names.');
                }
                const t0 = Number(prep.t_start) || 0;
                const outputDt = Number(prep.dt) || 0.05;
                const stepDt = Number(prep.internal_dt) || outputDt;
                const times = [];
                const rows = variableNames.map(() => []);

                let currentY = Array.isArray(prep.y0) ? prep.y0.slice() : [];
                let currentP = Array.isArray(prep.p0) ? prep.p0.slice() : [];
                let overrideKey = null;
                let t = t0;
                let completedSteps = 0;
                const bindings = prep.var_layout?.bindings || {};
                const pIndex = (name) => {
                    const index = bindings[name]?.P?.index;
                    return Number.isSafeInteger(index) ? index : null;
                };
                const pValue = (name, fallback = 0) => {
                    const index = pIndex(name);
                    const value = index === null ? NaN : Number(currentP[index]);
                    return Number.isFinite(value) ? value : fallback;
                };
                const setP = (name, value) => {
                    const index = pIndex(name);
                    if (index !== null && Number.isFinite(value)) {
                        currentP[index] = value;
                    }
                };
                const commandInput = (inputName, overrideName = inputName, fallback = 0) => {
                    const live = Number(widget.liveInputs[inputName]);
                    if (Number.isFinite(live)) {
                        return live;
                    }
                    const override = Number(widget.paramOverrides[overrideName]);
                    if (Number.isFinite(override)) {
                        return override;
                    }
                    return pValue(inputName, fallback);
                };
                const syncLiveInputs = () => {
                    setP('aoa_cmd', commandInput('aoa_cmd', 'aoa', pValue('aoa', 0)));
                    setP('mc', commandInput('mc', 'mc', pValue('mc', pValue('mc0', 0.02))));
                    setP('pc', commandInput('pc', 'pc', pValue('pc', pValue('pc0', 0.4))));
                    setP('tk', commandInput('tk', 'tk', pValue('tk', pValue('tk0', 0.12))));
                };
                const refreshStaticParametersIfNeeded = async () => {
                    const liveInputs = new Set(['aoa', 'aoa_cmd', 'mc', 'pc', 'tk']);
                    const nextKey = JSON.stringify(
                        Object.fromEntries(Object.entries(widget.paramOverrides)
                            .filter(([name]) => !liveInputs.has(name)))
                    );
                    if (nextKey === overrideKey) {
                        return;
                    }
                    overrideKey = nextKey;
                    const updated = JSON.parse(await runHeavy(
                        'update_gpu',
                        { source: liveSource, model, overrides: nextKey },
                        () => wasm.update_gpu_parameters(liveSource, model, nextKey),
                        signal
                    ));
                    currentP = Array.isArray(updated.p0) ? updated.p0.slice() : currentP;
                    if (completedSteps === 0 && Array.isArray(updated.y0)) {
                        currentY = updated.y0.slice();
                    }
                    syncLiveInputs();
                };
                const pushSample = () => {
                    times.push(t);
                    for (let i = 0; i < variableNames.length; i++) {
                        rows[i].push(Number(currentY[i]) || 0);
                    }
                };
                syncLiveInputs();
                await refreshStaticParametersIfNeeded();
                pushSample();
                const liveHost = document.createElement('div');
                liveHost.className = 'rumoca-live-radial';
                output.replaceChildren(liveHost);
                output.hidden = false;
                const livePayload = {
                    names: variableNames,
                    allData: [times, ...rows],
                    nStates: Number(prep.n_states) || 0,
                    simDetails: {
                        actual: {
                            t_start: t0,
                            t_end: t0,
                            points: 1,
                            variables: variableNames.length,
                        },
                        requested: {
                            solver: 'wgsl-solve interactive',
                            t_start: t0,
                            dt: outputDt,
                            internal_dt: stepDt,
                        },
                    },
                };
                const liveAnimations = [];
                if (widget.vizEditor) {
                    await runCustomViz(
                        liveHost,
                        livePayload,
                        times,
                        widget.vizEditor.getValue(),
                        widget,
                        { live: true, liveAnimations },
                    );
                } else if (wantsRadialViz) {
                    renderRadialViz(liveHost, times, livePayload);
                }
                let disposed = false;
                let timer = null;
                const runner = {
                    dispose() {
                        disposed = true;
                        if (timer !== null) {
                            clearTimeout(timer);
                            timer = null;
                        }
                    },
                };
                widget.interactiveRunner = runner;
                let nextTickWall = performance.now();
                const redrawLive = () => {
                    const frame = times.length - 1;
                    for (const anim of liveAnimations) {
                        anim.redraw(frame);
                    }
                    livePayload.simDetails.actual.t_end = times[frame];
                    livePayload.simDetails.actual.points = times.length;
                    widget.setStatus(
                        `Interactive t = ${times[frame].toFixed(2)} s`
                        + ` · ${completedSteps} steps`
                    );
                };
                const scheduleTick = () => {
                    const delay = Math.max(0, nextTickWall - performance.now());
                    timer = setTimeout(tick, delay);
                };
                const tick = async () => {
                    if (disposed) {
                        return;
                    }
                    try {
                        if (signal.aborted) {
                            throw makeAbortError();
                        }
                        await refreshStaticParametersIfNeeded();
                        syncLiveInputs();
                        const intervalPrep = {
                            ...prep,
                            y0: currentY,
                            p0: currentP,
                            t_start: t,
                            t_end: t + outputDt,
                            dt: outputDt,
                            internal_dt: stepDt,
                        };
                        const result = await gpu.runGpuSimulation(
                            adapter,
                            intervalPrep,
                            () => {},
                            widget.gpu || (widget.gpu = {}),
                            { signal },
                        );
                        const allData = result.payload?.allData || [];
                        const frameCount = Array.isArray(allData[0]) ? allData[0].length : 0;
                        if (frameCount < 2) {
                            throw new Error('Interactive GPU interval produced no output sample.');
                        }
                        const last = frameCount - 1;
                        t = Number(allData[0][last]) || (t + outputDt);
                        currentY = variableNames.map((_, index) => Number(allData[index + 1]?.[last]) || 0);
                        completedSteps += Math.max(1, Math.round(outputDt / stepDt));
                        pushSample();
                        redrawLive();
                        if (!disposed) {
                            nextTickWall = Math.max(
                                nextTickWall + outputDt * 1000,
                                performance.now(),
                            );
                            scheduleTick();
                        }
                    } catch (error) {
                        runner.dispose();
                        if (isAbortError(error)) {
                            widget.setStatus('Stopped');
                        } else {
                            showError(error);
                        }
                    }
                };
                redrawLive();
                scheduleTick();
                widget.setStatus('Interactive simulation running');
                return;
            }
            if (gpuCheck.checked) {
                const gpu = await loadGpuDriver();
                if (!gpu || typeof gpu.probeGpu !== 'function') {
                    throw new Error(
                        'GPU driver (rumoca_gpu.js) not found in this package; '
                        + 'rebuild it (cargo xtask playground build) or uncheck GPU to '
                        + 'simulate on the CPU (WASM) path.'
                    );
                }
                const adapter = await gpu.probeGpu();
                if (typeof wasm.prepare_gpu_simulation !== 'function') {
                    throw new Error(
                        'This WASM build predates the wgsl-solve backend; '
                        + 'rebuild the package (cargo xtask playground build) or '
                        + 'uncheck GPU to simulate on the CPU (WASM) path.'
                    );
                }
                let prep;
                if (widget.gpuPrep && widget.gpuPrep.source === source) {
                    prep = widget.gpuPrep.prep;
                } else {
                    setPhase('Compiling model (Modelica → Solve IR → WGSL)', null);
                    prep = JSON.parse(await runHeavy(
                        'prepare_gpu',
                        { source, model, sourceRootCacheUrl },
                        async () => {
                            const runtime = await loadRuntimeDriver();
                            return runtime.prepareGpuSimulationWithRuntime({
                                wasm,
                                source,
                                modelName: model,
                                sourceRootCacheUrl,
                            });
                        },
                        signal
                    ));
                    widget.gpuPrep = { source, prep };
                }
                if (Object.keys(widget.paramOverrides).length > 0) {
                    // Parameter-only change: re-settle the prepared vectors
                    // in milliseconds instead of re-lowering the model. The
                    // worker keeps the lowered model from the prepare call.
                    setPhase('Updating parameters', null);
                    const overrides = JSON.stringify(widget.paramOverrides);
                    const updated = JSON.parse(await runHeavy(
                        'update_gpu',
                        { source, model, overrides },
                        () => wasm.update_gpu_parameters(source, model, overrides),
                        signal
                    ));
                    prep = { ...prep, y0: updated.y0, p0: updated.p0 };
                }
                // Per-widget GPU program cache: a parameter-only re-run reuses
                // the compiled shader + pipelines and just re-uploads y0/p0.
                widget.gpu = widget.gpu || {};
                const result = await gpu.runGpuSimulation(adapter, prep, setPhase, widget.gpu, { signal });
                throwIfAborted(signal);
                await renderRunResult(result);
                return;
            }
            if (Object.keys(widget.paramOverrides).length > 0) {
                throw new Error(
                    'Parameter sliders drive the GPU fast path; enable the '
                    + 'GPU checkbox, or edit the parameter in the source and '
                    + 're-run on the CPU.'
                );
            }
            if (solver === 'bdf') {
                // Stiff path: the diffsol addon (separate relaxed-SIMD module)
                // runs on the main thread, like the GPU path. The main module
                // lowers the model and the addon simulates it. t_end/dt = 0
                // defer to the model's experiment annotation.
                const runtime = await loadRuntimeDriver();
                setPhase('Compiling & simulating (stiff · diffsol)', null);
                throwIfAborted(signal);
                const result = await runtime.simulateModelWithRuntime({
                    wasm,
                    pkgBase: resolvedPkgBase,
                    source,
                    modelName: model,
                    tEnd,
                    dt,
                    solver,
                    sourceRootCacheUrl,
                    parameterOverrides: widget.scenarioParameterOverrides(),
                });
                throwIfAborted(signal);
                await renderRunResult(result);
                return;
            }
            // t_end = 0 / dt = 0 defer to the model's experiment annotation,
            // falling back to runtime defaults. `solver` is "" (auto/annotation)
            // or "rk-like". Runs in a worker so the page stays live.
            const args = { source, model, tEnd, dt, solver, sourceRootCacheUrl };
            const raw = await runHeavy('simulate', args, async () => {
                setPhase('Loading source-root cache', null);
                const runtime = await loadRuntimeDriver();
                return JSON.stringify(await runtime.simulateModelWithRuntime({
                    wasm,
                    pkgBase: resolvedPkgBase,
                    source,
                    modelName: model,
                    tEnd,
                    dt,
                    solver,
                    sourceRootCacheUrl,
                    parameterOverrides: widget.scenarioParameterOverrides(),
                }));
            }, signal);
            throwIfAborted(signal);
            await renderRunResult(JSON.parse(raw));
            });
        });

        stopBtn.addEventListener('click', () => {
            widget.rerunAfterStop = false;
            const hadActiveRun = !!widget.activeRun;
            stopCurrentRun();
            stopBtn.disabled = true;
            widget.setStatus(hadActiveRun ? 'Stopping…' : 'Stopped');
        });

        async function renderRunResult(result) {
            const payload = result.payload || {};
            const allData = payload.allData || [];
            if (allData.length < 2) {
                throw new Error('Simulation produced no plottable variables.');
            }
            const views = [];
            widget.hideDefaultPlot = false;
            const configuredViews = Array.isArray(scenarioConfig?.plot?.views)
                ? scenarioConfig.plot.views
                : [];
            if (configuredViews.length > 0) {
                widget.hideDefaultPlot = true;
                for (const view of configuredViews) {
                    const type = trimMaybeString(view?.type).toLowerCase() || 'timeseries';
                    if (type === '3d') {
                        const custom = document.createElement('div');
                        custom.className = 'rumoca-live-radial';
                        try {
                            const script = await configuredViewScript(view);
                            if (!script) {
                                throw new Error('3D viewer panel has no script_path.');
                            }
                            await runCustomViz(custom, payload, allData[0], script, widget);
                            views.push(custom);
                        } catch (error) {
                            views.push(visualizationErrorBox(error));
                        }
                        continue;
                    }
                    const plot = document.createElement('div');
                    plot.className = 'rumoca-live-plot';
                    renderPlot(plot, allData[0], pickPlotSeries(payload, Array.isArray(view?.y) ? view.y : []));
                    views.push(plot);
                }
            } else if (widget.vizEditor) {
                const custom = document.createElement('div');
                custom.className = 'rumoca-live-radial';
                try {
                    await runCustomViz(custom, payload, allData[0], widget.vizEditor.getValue(), widget);
                    views.push(custom);
                } catch (error) {
                    views.push(visualizationErrorBox(error));
                }
            } else if (wantsRadialViz) {
                const radial = document.createElement('div');
                radial.className = 'rumoca-live-radial';
                if (renderRadialViz(radial, allData[0], payload)) {
                    views.push(radial);
                }
            }
            if (!widget.hideDefaultPlot) {
                const plot = document.createElement('div');
                plot.className = 'rumoca-live-plot';
                renderPlot(plot, allData[0], pickPlotSeries(payload, widget.plotSeries));
                views.push(plot);
            }
            output.replaceChildren(...views);
            output.hidden = false;
            widget.setStatus(describeRun(result));
        }

        function visualizationErrorBox(error) {
            const errBox = document.createElement('pre');
            errBox.className = 'rumoca-live-error';
            errBox.textContent = `Visualization script error: ${error.message || error}`;
            return errBox;
        }

        async function configuredViewScript(view) {
            const scriptPath = trimMaybeString(view?.script_path) || trimMaybeString(view?.scriptPath);
            if (!scriptPath) {
                return '';
            }
            const embedded = widget.viewerScriptsByPath.get(scriptPath);
            if (embedded) {
                return embedded;
            }
            const base = scenarioUrl || sourceUrl || window.location.href;
            const response = await fetch(new URL(scriptPath, pageUrl(base)).href);
            if (!response.ok) {
                throw new Error(`${scriptPath}: ${response.status} ${response.statusText}`);
            }
            return response.text();
        }

        daeBtn.addEventListener('click', () => withWasm('dae', 'Compiling to DAE…', async (wasm, source, model, signal) => {
            const sourceRootCacheUrl = await widget.sourceRootCacheUrl(wasm);
            if (sourceRootCacheUrl) {
                setPhase('Loading source-root cache', null);
            }
            await ensureWasmSourceRootsLoaded(wasm, widget);
            const text = await runHeavy('dae', { source, model, sourceRootCacheUrl }, async () => {
                const runtime = await loadRuntimeDriver();
                return runtime.renderDaeTextWithRuntime({
                    wasm,
                    source,
                    modelName: model,
                    sourceRootCacheUrl,
                });
            }, signal);
            throwIfAborted(signal);
            const daeBox = document.createElement('pre');
            daeBox.className = 'rumoca-live-dae';
            daeBox.textContent = text || '(empty render)';
            output.replaceChildren(daeBox);
            output.hidden = false;
            widget.setStatus('Flattened DAE (the form the solver integrates)');
        }));

        resetBtn.addEventListener('click', () => {
            clearGeneratedEditors();
            widget.rerunAfterStop = false;
            if (widget.interactiveRunner && typeof widget.interactiveRunner.reset === 'function') {
                widget.interactiveRunner.reset();
                widget.setStatus('Interactive simulation reset');
                return;
            }
            stopCurrentRun();
            editor.setValue(originalSource);
            if (widget.vizEditor) {
                widget.vizEditor.reset();
            }
            if (!settingsPanel.hidden) {
                void renderScenarioSettingsFrame().catch(showError);
            }
            output.hidden = true;
            output.replaceChildren();
            widget.setStatus('');
        });

        settingsBtn.addEventListener('click', () => {
            settingsPanel.hidden = !settingsPanel.hidden;
            if (!settingsPanel.hidden) {
                void renderScenarioSettingsFrame().catch(showError);
            }
        });

        settingsPanel.addEventListener('click', async (event) => {
            const action = event.target?.dataset?.action;
            if (!action || !scenarioConfig) return;
            if (action === 'apply-toml') {
                try {
                    await applyScenarioText(settingsPanel.querySelector('[data-setting="raw"]').value);
                    widget.setStatus('Settings applied from TOML');
                } catch (error) {
                    showError(error);
                    widget.setStatus('Scenario TOML has errors');
                }
                return;
            }
            if (action === 'show-gui') {
                try {
                    await renderScenarioSettingsFrame();
                } catch (error) {
                    showError(error);
                }
            }
        });

        return widget;
    }

    async function init() {
        const liveBlocks = [...document.querySelectorAll('pre > code')].filter((code) => {
            const cls = code.className || '';
            const isInteractive = cls.includes('language-modelica') && /\binteractive\b/.test(cls);
            const isCodegen = cls.includes('language-modelica') && /\b(?:codegen|galec-codegen)\b/.test(cls);
            const isViz = cls.includes('language-js') && /\brumoca-viz\b/.test(cls);
            return isInteractive || isCodegen || isViz;
        });
        if (liveBlocks.length === 0) {
            return;
        }
        let monaco = null;
        try {
            monaco = await loadMonaco();
        } catch (error) {
            console.warn('rumoca-live: Monaco unavailable, using plain editors:', error);
        }
        // Document order pairs each `js,rumoca-viz` script with the
        // interactive widget that precedes it.
        let lastWidget = null;
        for (const code of liveBlocks) {
            const cls = code.className || '';
            if (/\brumoca-viz\b/.test(cls)) {
                if (lastWidget) {
                    lastWidget.vizEditor = buildVizEditor(code, monaco);
                } else {
                    console.warn('rumoca-live: viz script with no preceding interactive block');
                }
            } else if (/\b(?:codegen|galec-codegen)\b/.test(cls)) {
                buildGalecCodegenWidget(code, monaco);
                lastWidget = null;
            } else {
                lastWidget = buildWidget(code, monaco);
            }
        }
    }

    // Debug/test surface (used by the book smoke tests). runGpuSimulation is a
    // thin wrapper over the lazily-imported packaged GPU driver (rumoca_gpu.js).
    window.rumocaLive = {
        loadWasm,
        runGpuSimulation: async (adapter, prep, onPhase, cache) => {
            const gpu = await loadGpuDriver();
            if (!gpu || typeof gpu.runGpuSimulation !== 'function') {
                throw new Error('GPU driver (rumoca_gpu.js) unavailable in this package.');
            }
            return gpu.runGpuSimulation(adapter, prep, onPhase, cache);
        },
    };

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', init);
    } else {
        init();
    }
})();
