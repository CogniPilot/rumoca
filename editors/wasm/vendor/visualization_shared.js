(function (root, factory) {
    if (typeof module === 'object' && module.exports) {
        module.exports = factory();
        return;
    }
    root.RumocaVisualizationShared = factory();
}(typeof globalThis !== 'undefined' ? globalThis : this, function () {
    function trimMaybeString(value) {
        return typeof value === 'string' ? value.trim() : '';
    }

    function normalizeStringArray(values) {
        if (!Array.isArray(values)) {
            return [];
        }
        return values.map(trimMaybeString).filter(Boolean);
    }

    const RESULTS_ROOT = '.rumoca/results';
    const RESULTS_RUNS_ROOT = `${RESULTS_ROOT}/runs`;
    const MODEL_BY_ID_ROOT = '.rumoca/models/by-id';

    function sanitizeIdentifier(input) {
        const text = String(input || '');
        let out = '';
        for (const ch of text) {
            if (/[A-Za-z0-9_]/.test(ch)) {
                out += ch.toLowerCase();
            } else if (/\s|-|\./.test(ch)) {
                out += '_';
            }
        }
        return out || 'model';
    }

    function sanitizeResultIdentifier(input) {
        let out = '';
        for (const ch of String(input || '')) {
            if (/[A-Za-z0-9_]/.test(ch)) {
                out += ch.toLowerCase();
            } else if (/\s|-/.test(ch)) {
                out += '_';
            }
        }
        return out || 'panel';
    }

    function fnv1aHash(text) {
        let hash = 0x811c9dc5;
        for (const ch of String(text || '')) {
            hash ^= ch.charCodeAt(0);
            hash = Math.imul(hash, 0x01000193) >>> 0;
        }
        return hash.toString(16).padStart(8, '0');
    }

    function sanitizeResultsPathSegment(input) {
        const cleaned = String(input || '')
            .trim()
            .replace(/[^a-zA-Z0-9._-]+/g, '_')
            .replace(/^_+|_+$/g, '');
        return cleaned.length > 0 ? cleaned : 'view';
    }

    function stableModelUuid(model) {
        return `${sanitizeIdentifier(model)}_${fnv1aHash(model)}`;
    }

    function stableResultStem(model) {
        return `${sanitizeResultIdentifier(model)}_${fnv1aHash(model)}`;
    }

    function escapeHtml(text) {
        return String(text || '')
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;')
            .replace(/'/g, '&#39;');
    }

    function escapeInlineScriptJson(raw) {
        return String(raw || '')
            .replace(/</g, '\\u003c')
            .replace(/>/g, '\\u003e')
            .replace(/&/g, '\\u0026')
            .replace(/\u2028/g, '\\u2028')
            .replace(/\u2029/g, '\\u2029');
    }

    function modelScopedViewerScriptRelativePath(uuid, viewId) {
        return `${MODEL_BY_ID_ROOT}/${uuid}/${sanitizeResultsPathSegment(viewId)}.js`;
    }

    function preferredViewerScriptPathForModel(model, viewId) {
        return modelScopedViewerScriptRelativePath(stableModelUuid(model), viewId);
    }

    function lastSimulationResultPath(model) {
        return `${RESULTS_ROOT}/${stableResultStem(model)}.json`;
    }

    function nextSimulationRunLocation(model, pathExists) {
        const now = Date.now();
        const slug = stableResultStem(model);
        let runId = `${now}_${slug}`;
        let runPath = `${RESULTS_RUNS_ROOT}/${runId}.json`;
        for (let suffix = 1; typeof pathExists === 'function' && pathExists(runPath); suffix += 1) {
            runId = `${now}_${slug}_${suffix}`;
            runPath = `${RESULTS_RUNS_ROOT}/${runId}.json`;
        }
        return { runId, runPath, savedAtUnixMs: now };
    }

    function defaultThreeDimensionalViewerScript() {
        return `// Default Rumoca 3D preset.
// Geometry is defined here so each model can fully customize visuals.
ctx.onInit = (api) => {
  if (typeof api.enableDefaultViewerRuntime === "function") {
    api.enableDefaultViewerRuntime({ selectedObjectName: "ball", followSelected: false });
  }
  const { THREE, state } = api;
  if (!THREE || !state || !state.scene) return;

  state.scene.background = new THREE.Color(0x101010);

  const keyLight = new THREE.DirectionalLight(0xffffff, 1.0);
  keyLight.position.set(2, 4, 3);
  state.scene.add(keyLight);
  state.scene.add(new THREE.AmbientLight(0x404040, 0.9));
  state.scene.add(new THREE.GridHelper(12, 24, 0x2f4f63, 0x2a2a2a));

  const floor = new THREE.Mesh(
    new THREE.BoxGeometry(8, 0.1, 8),
    new THREE.MeshStandardMaterial({ color: 0x444444 })
  );
  floor.position.set(0, -0.05, 0);
  floor.name = "floor";
  state.scene.add(floor);

  const ball = new THREE.Mesh(
    new THREE.SphereGeometry(0.2, 32, 24),
    new THREE.MeshStandardMaterial({ color: 0x3cb4ff })
  );
  ball.name = "ball";
  state.scene.add(ball);
  state.ball = ball;
};

ctx.onFrame = (api) => {
  const ball = api.state ? api.state.ball : null;
  if (ball) {
    const x = Number(api.getValue("x", api.sampleIndex));
    const y = Number(api.getValue("y", api.sampleIndex));
    const z = Number(api.getValue("z", api.sampleIndex));
    ball.position.set(
      Number.isFinite(y) ? y : 0,
      Number.isFinite(x) ? x : 0,
      Number.isFinite(z) ? z : 0
    );
  }
};`;
    }

    function nextViewId(index) {
        return `view_${index + 1}`;
    }

    function normalizeScatterSeries(raw, fallbackX, fallbackY) {
        const series = [];
        for (const entry of Array.isArray(raw) ? raw : []) {
            if (!entry || typeof entry !== 'object') {
                continue;
            }
            const x = trimMaybeString(entry.x);
            const y = trimMaybeString(entry.y);
            if (!x || !y) {
                continue;
            }
            const name = trimMaybeString(entry.name);
            series.push({
                name: name || `${y} vs ${x}`,
                x,
                y,
            });
        }
        if (series.length > 0) {
            return series;
        }
        if (fallbackX && fallbackY) {
            return [{
                name: `${fallbackY} vs ${fallbackX}`,
                x: fallbackX,
                y: fallbackY,
            }];
        }
        return undefined;
    }

    function defaultVisualizationViews() {
        return [
            {
                id: 'states_time',
                title: 'States vs Time',
                type: 'timeseries',
                x: 'time',
                y: ['*states'],
            },
        ];
    }

    function normalizeVisualizationViews(raw) {
        if (!Array.isArray(raw)) {
            return [];
        }
        const out = [];
        for (const entry of raw) {
            if (!entry || typeof entry !== 'object') {
                continue;
            }
            const typeRaw = trimMaybeString(entry.type).toLowerCase();
            const type = typeRaw === 'scatter' || typeRaw === '3d' ? typeRaw : 'timeseries';
            const x = trimMaybeString(entry.x) || undefined;
            const y = normalizeStringArray(entry.y);
            const script = trimMaybeString(entry.script) || undefined;
            const scriptPath = trimMaybeString(entry.scriptPath) || undefined;
            const fallbackX = x || 'time';
            const fallbackY = y.length > 0 ? y[0] : '';
            const scatterSeries = type === 'scatter'
                ? normalizeScatterSeries(entry.scatterSeries, fallbackX, fallbackY)
                : undefined;
            out.push({
                id: trimMaybeString(entry.id) || nextViewId(out.length),
                title: trimMaybeString(entry.title) || `View ${out.length + 1}`,
                type,
                x,
                y: type === '3d' ? y.slice(0, 2) : y,
                ...(scatterSeries ? { scatterSeries } : {}),
                ...(script ? { script } : {}),
                ...(scriptPath ? { scriptPath } : {}),
            });
        }
        return out;
    }

    function seriesIndexByName(result) {
        const lookup = new Map();
        const names = Array.isArray(result?.names) ? result.names : [];
        names.forEach((name, index) => lookup.set(String(name), index));
        return lookup;
    }

    function availableStateNames(result) {
        const names = Array.isArray(result?.names) ? result.names : [];
        const stateCount = Number.isFinite(result?.nStates) ? Math.max(0, result.nStates) : 0;
        return names.slice(0, stateCount).map(String);
    }

    function expandRequestedSeries(result, requested) {
        const names = Array.isArray(result?.names) ? result.names.map(String) : [];
        const expanded = [];
        for (const rawName of Array.isArray(requested) ? requested : []) {
            const name = trimMaybeString(rawName);
            if (!name) {
                continue;
            }
            if (name === '*states') {
                expanded.push(...availableStateNames(result));
                continue;
            }
            if (names.includes(name)) {
                expanded.push(name);
            }
        }
        return [...new Set(expanded)];
    }

    function resolveSeries(result, expr, fallback) {
        const key = trimMaybeString(expr) || trimMaybeString(fallback);
        if (!key) {
            return null;
        }
        if (key === 'time') {
            return {
                name: 'time',
                values: Array.isArray(result?.allData?.[0]) ? result.allData[0].map(Number) : [],
            };
        }
        const lookup = seriesIndexByName(result);
        const index = lookup.get(key);
        if (index === undefined) {
            return null;
        }
        const source = Array.isArray(result?.allData) ? result.allData[index + 1] : null;
        return {
            name: key,
            values: Array.isArray(source) ? source.map(Number) : [],
        };
    }

    function seriesColor(index) {
        const palette = [
            '#4ec9b0', '#569cd6', '#ce9178', '#dcdcaa', '#c586c0',
            '#9cdcfe', '#d7ba7d', '#608b4e', '#d16969', '#b5cea8',
        ];
        return palette[index % palette.length];
    }

    function normalizeSimulationRunMetrics(raw) {
        if (!raw || typeof raw !== 'object') {
            return undefined;
        }
        const obj = raw;
        const compileSeconds = Number(obj.compileSeconds);
        const simulateSeconds = Number(obj.simulateSeconds);
        const points = Number(obj.points);
        const variables = Number(obj.variables);
        if (!Number.isFinite(simulateSeconds)
            || !Number.isFinite(points)
            || !Number.isFinite(variables)) {
            return undefined;
        }

        const compilePhaseRaw = obj.compilePhaseSeconds;
        let compilePhaseSeconds;
        if (compilePhaseRaw && typeof compilePhaseRaw === 'object') {
            const instantiate = Number(compilePhaseRaw.instantiate);
            const typecheck = Number(compilePhaseRaw.typecheck);
            const flatten = Number(compilePhaseRaw.flatten);
            const todae = Number(compilePhaseRaw.todae);
            if (Number.isFinite(instantiate)
                && Number.isFinite(typecheck)
                && Number.isFinite(flatten)
                && Number.isFinite(todae)) {
                compilePhaseSeconds = {
                    instantiate,
                    typecheck,
                    flatten,
                    todae,
                };
            }
        }

        return {
            ...(Number.isFinite(compileSeconds) ? { compileSeconds } : {}),
            simulateSeconds,
            points,
            variables,
            ...(compilePhaseSeconds ? { compilePhaseSeconds } : {}),
        };
    }

    function normalizeSimulationPayload(raw) {
        if (!raw || typeof raw !== 'object') {
            return undefined;
        }
        const obj = raw;
        const names = Array.isArray(obj.names)
            ? obj.names.filter(function(entry) { return typeof entry === 'string'; })
            : [];
        const allData = Array.isArray(obj.allData)
            ? obj.allData.map(function(column) {
                return Array.isArray(column) ? column.map(Number) : [];
            })
            : [];
        const nStates = Number(obj.nStates);
        if (!Number.isFinite(nStates) || names.length === 0 || allData.length === 0) {
            return undefined;
        }
        return {
            version: Number.isFinite(Number(obj.version)) ? Number(obj.version) : undefined,
            names,
            allData,
            nStates,
            variableMeta: Array.isArray(obj.variableMeta) ? obj.variableMeta : [],
            simDetails: obj.simDetails || {},
        };
    }

    function normalizeRunId(raw) {
        const id = trimMaybeString(raw);
        if (!id || !/^[A-Za-z0-9._-]+$/.test(id)) {
            return undefined;
        }
        return id;
    }

    function simulationRunDocumentPath(runId) {
        const normalized = normalizeRunId(runId);
        return normalized ? `${RESULTS_RUNS_ROOT}/${normalized}.json` : undefined;
    }

    function normalizeHostedResultsModelRef(raw, fallbackWorkspaceRoot) {
        if (!raw || typeof raw !== 'object') {
            return undefined;
        }
        const candidate = raw;
        const model = trimMaybeString(candidate.model);
        if (!model) {
            return undefined;
        }
        return {
            model,
            workspaceRoot: trimMaybeString(candidate.workspaceRoot) || trimMaybeString(fallbackWorkspaceRoot) || undefined,
            runId: trimMaybeString(candidate.runId) || undefined,
            title: trimMaybeString(candidate.title) || undefined,
        };
    }

    function normalizeHostedResultsPanelState(raw, fallbackWorkspaceRoot) {
        const modelRef = normalizeHostedResultsModelRef(raw, fallbackWorkspaceRoot);
        if (!modelRef || !modelRef.runId) {
            return undefined;
        }
        return {
            version: 1,
            runId: modelRef.runId,
            model: modelRef.model,
            workspaceRoot: modelRef.workspaceRoot,
            title: modelRef.title,
            activeViewId: trimMaybeString(raw && raw.activeViewId) || undefined,
        };
    }

    function buildHostedResultsPanelState(args) {
        return normalizeHostedResultsPanelState(args, args && args.fallbackWorkspaceRoot);
    }

    function buildHostedResultsPanelTitle(args) {
        const title = trimMaybeString(args && args.title);
        if (title) {
            return title;
        }
        if (args && args.unavailable) {
            return 'Rumoca Results (Unavailable)';
        }
        const model = trimMaybeString(args && args.model);
        if (!model) {
            return 'Rumoca Results';
        }
        if (args && args.missingRun) {
            return `Rumoca Results: ${model} (Missing Run)`;
        }
        const timestamp = trimMaybeString(args && args.timestamp);
        return timestamp
            ? `Rumoca Results: ${model} (${timestamp})`
            : `Rumoca Results: ${model}`;
    }

    function cloneJson(value) {
        return JSON.parse(JSON.stringify(value));
    }

    function cloneView(view) {
        return {
            ...view,
            y: Array.isArray(view && view.y) ? [...view.y] : [],
            scatterSeries: Array.isArray(view && view.scatterSeries)
                ? view.scatterSeries.map(function(series) { return { ...series }; })
                : undefined,
        };
    }

    async function hydrateVisualizationViewsForModel(args) {
        const model = trimMaybeString(args && args.model);
        const fallbackScript = typeof args?.defaultViewerScript === 'function'
            ? args.defaultViewerScript
            : function() { return defaultThreeDimensionalViewerScript(); };
        const resolveViewerScriptPath = typeof args?.resolveViewerScriptPath === 'function'
            ? args.resolveViewerScriptPath
            : function(nextModel, viewId) {
                return preferredViewerScriptPathForModel(nextModel, viewId);
            };
        const readTextFile = typeof args?.readTextFile === 'function' ? args.readTextFile : null;
        const writeMissingTextFile = typeof args?.writeMissingTextFile === 'function'
            ? args.writeMissingTextFile
            : null;
        const out = [];
        for (const [index, view] of normalizeVisualizationViews(args?.views).entries()) {
            const next = cloneView(view);
            if (next.type !== '3d') {
                next.script = undefined;
                next.scriptPath = undefined;
                out.push(next);
                continue;
            }
            const viewId = trimMaybeString(next.id) || `viewer_${index + 1}`;
            const scriptPath = trimMaybeString(next.scriptPath)
                || trimMaybeString(await resolveViewerScriptPath(model, viewId))
                || preferredViewerScriptPathForModel(model, viewId);
            let script = trimMaybeString(next.script);
            if (!script && readTextFile) {
                try {
                    script = trimMaybeString(await readTextFile(scriptPath));
                } catch {
                    script = '';
                }
            }
            if (!script) {
                script = fallbackScript();
                if (writeMissingTextFile) {
                    try {
                        await writeMissingTextFile(scriptPath, script);
                    } catch {
                        // Best effort only; keep in-memory fallback.
                    }
                }
            }
            next.scriptPath = scriptPath;
            next.script = script;
            out.push(next);
        }
        return out;
    }

    async function persistVisualizationViewsForModel(args) {
        const model = trimMaybeString(args && args.model);
        const fallbackScript = typeof args?.defaultViewerScript === 'function'
            ? args.defaultViewerScript
            : function() { return defaultThreeDimensionalViewerScript(); };
        const resolveViewerScriptPath = typeof args?.resolveViewerScriptPath === 'function'
            ? args.resolveViewerScriptPath
            : function(nextModel, viewId) {
                return preferredViewerScriptPathForModel(nextModel, viewId);
            };
        const readTextFile = typeof args?.readTextFile === 'function' ? args.readTextFile : null;
        const writeTextFile = typeof args?.writeTextFile === 'function' ? args.writeTextFile : null;
        const out = [];
        for (const [index, view] of normalizeVisualizationViews(args?.views).entries()) {
            const next = cloneView(view);
            if (next.type !== '3d') {
                next.script = undefined;
                next.scriptPath = undefined;
                out.push(next);
                continue;
            }
            const viewId = trimMaybeString(next.id) || `viewer_${index + 1}`;
            const scriptPath = trimMaybeString(next.scriptPath)
                || trimMaybeString(await resolveViewerScriptPath(model, viewId))
                || preferredViewerScriptPathForModel(model, viewId);
            let existing = '';
            if (readTextFile) {
                try {
                    existing = trimMaybeString(await readTextFile(scriptPath));
                } catch {
                    existing = '';
                }
            }
            const script = trimMaybeString(next.script) || existing || fallbackScript();
            if (writeTextFile) {
                await writeTextFile(scriptPath, script);
            }
            next.script = undefined;
            next.scriptPath = scriptPath;
            out.push(next);
        }
        return out;
    }

    async function removeVisualizationScriptFilesForViews(args) {
        const removeTextFile = typeof args?.removeTextFile === 'function' ? args.removeTextFile : null;
        if (!removeTextFile) {
            return;
        }
        for (const view of Array.isArray(args?.views) ? args.views : []) {
            const scriptPath = trimMaybeString(view && view.scriptPath);
            if (scriptPath) {
                await removeTextFile(scriptPath);
            }
        }
    }

    async function removeStaleVisualizationScriptFiles(args) {
        const nextPaths = new Set(
            (Array.isArray(args?.nextViews) ? args.nextViews : [])
                .map(function(view) { return trimMaybeString(view && view.scriptPath); })
                .filter(Boolean),
        );
        await removeVisualizationScriptFilesForViews({
            views: (Array.isArray(args?.previousViews) ? args.previousViews : []).filter(function(view) {
                const scriptPath = trimMaybeString(view && view.scriptPath);
                return scriptPath && !nextPaths.has(scriptPath);
            }),
            removeTextFile: args?.removeTextFile,
        });
    }

    function buildVisualizationViewStorageHandlers(args) {
        const fallbackScript = typeof args?.defaultViewerScript === 'function'
            ? args.defaultViewerScript
            : function() { return defaultThreeDimensionalViewerScript(); };
        const resolveViewerScriptPath = typeof args?.resolveViewerScriptPath === 'function'
            ? args.resolveViewerScriptPath
            : function(model, viewId) {
                return preferredViewerScriptPathForModel(model, viewId);
            };
        const readTextFile = typeof args?.readTextFile === 'function' ? args.readTextFile : null;
        const writeTextFile = typeof args?.writeTextFile === 'function' ? args.writeTextFile : null;
        const removeTextFile = typeof args?.removeTextFile === 'function' ? args.removeTextFile : null;

        async function writeMissingTextFile(scriptPath, content) {
            if (!writeTextFile) {
                return;
            }
            if (readTextFile) {
                try {
                    await readTextFile(scriptPath);
                    return;
                } catch {
                    // Fall through to create the missing file.
                }
            }
            await writeTextFile(scriptPath, content);
        }

        return {
            async hydrateViews(input) {
                return await hydrateVisualizationViewsForModel({
                    views: input?.views,
                    model: input?.model,
                    resolveViewerScriptPath,
                    readTextFile,
                    writeMissingTextFile: writeTextFile ? writeMissingTextFile : null,
                    defaultViewerScript: fallbackScript,
                });
            },
            async persistViews(input) {
                return await persistVisualizationViewsForModel({
                    views: input?.views,
                    model: input?.model,
                    resolveViewerScriptPath,
                    readTextFile,
                    writeTextFile,
                    defaultViewerScript: fallbackScript,
                });
            },
            async removeViews(input) {
                await removeVisualizationScriptFilesForViews({
                    views: input?.views,
                    removeTextFile,
                });
            },
            async removeStaleViews(input) {
                await removeStaleVisualizationScriptFiles({
                    previousViews: input?.previousViews,
                    nextViews: input?.nextViews,
                    removeTextFile,
                });
            },
        };
    }

    async function writePersistedSimulationRunDocument(args) {
        if (!args || !args.payload || typeof args.writeTextFile !== 'function') {
            return undefined;
        }
        const location = nextSimulationRunLocation(args.model, args.pathExists);
        const runDoc = buildSimulationRunDocument({
            runId: location.runId,
            model: args.model,
            savedAtUnixMs: location.savedAtUnixMs,
            payload: args.payload,
            metrics: args.metrics,
            views: args.views,
        });
        if (!runDoc) {
            return undefined;
        }
        await args.writeTextFile(location.runPath, JSON.stringify(runDoc, null, 2));
        return {
            runId: location.runId,
            runPath: location.runPath,
            savedAtUnixMs: location.savedAtUnixMs,
            runDoc,
        };
    }

    async function readPersistedSimulationRunDocument(args) {
        const runPath = simulationRunDocumentPath(args && args.runId);
        if (!runPath || typeof args?.readTextFile !== 'function') {
            return undefined;
        }
        let text;
        try {
            text = await args.readTextFile(runPath);
        } catch {
            return undefined;
        }
        let parsed;
        try {
            parsed = JSON.parse(String(text || ''));
        } catch {
            return undefined;
        }
        const normalized = normalizePersistedSimulationRun(parsed);
        if (!normalized || normalized.runId !== args.runId) {
            return undefined;
        }
        return normalized;
    }

    function buildLastSimulationResultDocument(args) {
        return {
            version: 1,
            model: trimMaybeString(args && args.model),
            savedAtUnixMs: Math.max(0, Number(args && args.savedAtUnixMs) || 0),
            payload: cloneJson(args && args.payload ? args.payload : null),
            metrics: cloneJson(args && args.metrics ? args.metrics : null),
        };
    }

    async function writeLastSimulationResultDocument(args) {
        if (!args || typeof args.writeTextFile !== 'function') {
            return undefined;
        }
        const path = lastSimulationResultPath(args.model);
        const doc = buildLastSimulationResultDocument({
            model: args.model,
            savedAtUnixMs: args.savedAtUnixMs,
            payload: args.payload,
            metrics: args.metrics,
        });
        await args.writeTextFile(path, JSON.stringify(doc, null, 2));
        return { path, document: doc };
    }

    async function readLastSimulationResultDocument(args) {
        if (!args || typeof args.readTextFile !== 'function') {
            return null;
        }
        let text;
        try {
            text = await args.readTextFile(lastSimulationResultPath(args.model));
        } catch {
            return null;
        }
        let parsed;
        try {
            parsed = JSON.parse(String(text || ''));
        } catch {
            return null;
        }
        const payload = normalizeSimulationPayload(parsed && parsed.payload);
        if (!payload) {
            return null;
        }
        const metrics = normalizeSimulationRunMetrics(parsed && parsed.metrics);
        return {
            payload,
            ...(metrics ? { metrics } : {}),
        };
    }

    async function persistHostedSimulationRun(args) {
        const model = trimMaybeString(args && args.model);
        if (!model) {
            return undefined;
        }
        const payload = normalizeSimulationPayload(args && args.payload);
        const metrics = normalizeSimulationRunMetrics(args && args.metrics);
        const views = normalizeVisualizationViews(
            typeof args?.hydrateViews === 'function'
                ? await args.hydrateViews({
                    model,
                    views: normalizeVisualizationViews(args && args.views),
                })
                : args && args.views,
        );
        let persisted;
        if (payload && typeof args?.writeTextFile === 'function') {
            persisted = await writePersistedSimulationRunDocument({
                model,
                payload,
                metrics,
                views,
                pathExists: args.pathExists,
                writeTextFile: args.writeTextFile,
            });
        }
        if (typeof args?.writeLastResultTextFile === 'function') {
            await writeLastSimulationResultDocument({
                model,
                payload,
                metrics,
                writeTextFile: args.writeLastResultTextFile,
            });
        }
        if (!persisted) {
            return undefined;
        }
        return {
            runId: persisted.runId,
            runPath: persisted.runPath,
            savedAtUnixMs: persisted.savedAtUnixMs,
            views,
        };
    }

    async function persistHostedSimulationRunWithViews(args) {
        const model = trimMaybeString(args && args.model);
        if (!model || typeof args?.loadConfiguredViews !== 'function') {
            return undefined;
        }
        const configuredViews = normalizeVisualizationViews(
            await args.loadConfiguredViews({
                model,
                workspaceRoot: args?.workspaceRoot,
            }),
        );
        const views = configuredViews.length > 0
            ? configuredViews
            : normalizeVisualizationViews(args?.defaultViews);
        return await persistHostedSimulationRun({
            model,
            payload: args?.payload,
            metrics: args?.metrics,
            views,
            hydrateViews: args?.hydrateViews
                ? function(input) {
                    return args.hydrateViews({
                        model: input.model,
                        workspaceRoot: args?.workspaceRoot,
                        views: input.views,
                    });
                }
                : null,
            pathExists: args?.pathExists,
            writeTextFile: args?.writeTextFile,
            writeLastResultTextFile: args?.writeLastResultTextFile,
        });
    }

    async function loadHostedSimulationRun(args) {
        if (!args || typeof args.readTextFile !== 'function') {
            return undefined;
        }
        const runId = trimMaybeString(args.runId);
        if (runId) {
            return await readPersistedSimulationRunDocument({
                runId,
                readTextFile: args.readTextFile,
            });
        }
        const model = trimMaybeString(args.model);
        if (!model) {
            return undefined;
        }
        return await readLastSimulationResultDocument({
            model,
            readTextFile: args.readTextFile,
        });
    }

    async function loadHostedSimulationRunWithViews(args) {
        const model = trimMaybeString(args && args.model);
        if (!model || typeof args?.loadConfiguredViews !== 'function') {
            return undefined;
        }
        const run = await loadHostedSimulationRun({
            model,
            runId: args?.runId,
            readTextFile: args?.readTextFile,
        });
        const configuredViews = normalizeVisualizationViews(
            await args.loadConfiguredViews({
                model,
                workspaceRoot: args?.workspaceRoot,
            }),
        );
        const baseViews = configuredViews.length > 0
            ? configuredViews
            : normalizeVisualizationViews(args?.defaultViews);
        const views = typeof args?.hydrateViews === 'function'
            ? normalizeVisualizationViews(
                await args.hydrateViews({
                    model,
                    workspaceRoot: args?.workspaceRoot,
                    views: baseViews,
                }),
            )
            : baseViews;
        return { run, views };
    }

    function normalizeHostedSimulationSettingsFeatures(value) {
        const features = value && typeof value === 'object' ? value : {};
        return {
            addLibraryPath: features.addLibraryPath !== false,
            prepareModels: features.prepareModels !== false,
            resyncSidecars: features.resyncSidecars !== false,
            workspaceSettings: features.workspaceSettings !== false,
            userSettings: features.userSettings !== false,
            openViewScript: features.openViewScript !== false,
        };
    }

    function normalizeHostedSimulationSettingsCurrent(value) {
        const current = value && typeof value === 'object' ? value : {};
        const libraryOverrides = current.libraryOverrides !== undefined
            ? current.libraryOverrides
            : current.modelicaPath;
        const tEndRaw = current.tEnd === undefined || current.tEnd === null
            ? ''
            : String(current.tEnd).trim();
        const dtRaw = current.dt === undefined || current.dt === null
            ? ''
            : String(current.dt).trim();
        const tEnd = Number(tEndRaw);
        const dt = Number(dtRaw);
        return {
            solver: trimMaybeString(current.solver) || 'auto',
            tEnd: Number.isFinite(tEnd) && tEnd > 0 ? tEnd : 10,
            dt: Number.isFinite(dt) && dt > 0 ? dt : null,
            outputDir: trimMaybeString(current.outputDir),
            libraryOverrides: normalizeStringArray(libraryOverrides),
        };
    }

    function normalizeHostedSimulationSettingsState(args) {
        const activeModel = trimMaybeString(args && args.activeModel) || 'Model';
        const availableModels = normalizeStringArray(args && args.availableModels);
        return {
            activeModel,
            availableModels: availableModels.length > 0 ? availableModels : [activeModel],
            current: normalizeHostedSimulationSettingsCurrent(args && args.current),
            views: normalizeVisualizationViews(args && args.views),
            features: normalizeHostedSimulationSettingsFeatures(args && args.features),
        };
    }

    function buildHostedSimulationSettingsState(args) {
        const configuredViews = normalizeVisualizationViews(args && args.views);
        return normalizeHostedSimulationSettingsState({
            activeModel: args && args.activeModel,
            availableModels: args && args.availableModels,
            current: (args && args.current) ?? (args && args.fallbackCurrent),
            views: configuredViews.length > 0
                ? configuredViews
                : normalizeVisualizationViews(args && args.defaultViews),
            features: args && args.features,
        });
    }

    function normalizeHostedSimulationSettingsSavePayload(payload) {
        const tEnd = Number(payload && payload.tEnd);
        if (!Number.isFinite(tEnd) || tEnd <= 0) {
            throw new Error('t_end must be a positive number.');
        }
        const solver = trimMaybeString(payload && payload.solver).toLowerCase() || 'auto';
        if (solver !== 'auto' && solver !== 'bdf' && solver !== 'rk-like') {
            throw new Error('Invalid solver mode.');
        }
        const dtRaw = trimMaybeString(payload && payload.dt);
        const dt = dtRaw.length === 0 ? null : Number(dtRaw);
        if (dt !== null && (!Number.isFinite(dt) || dt <= 0)) {
            throw new Error('dt must be empty or a positive number.');
        }
        return {
            solver,
            tEnd,
            dt,
            outputDir: trimMaybeString(payload && payload.outputDir),
            libraryOverrides: normalizeStringArray(payload && payload.modelicaPath),
            views: normalizeVisualizationViews(payload && payload.views),
        };
    }

    function buildHostedSimulationSettingsResetValue(raw) {
        const source = raw && typeof raw === 'object' ? raw : {};
        const current = normalizeHostedSimulationSettingsCurrent(
            source.current && typeof source.current === 'object' ? source.current : source,
        );
        return {
            solver: current.solver,
            tEnd: current.tEnd,
            dt: current.dt,
            outputDir: current.outputDir,
            modelicaPath: [...current.libraryOverrides],
            views: normalizeVisualizationViews(source.views),
        };
    }

    async function saveHostedProjectSimulationSettings(args) {
        if (typeof args?.loadViews !== 'function') {
            throw new Error('Missing settings loadViews handler.');
        }
        if (typeof args?.persistViews !== 'function') {
            throw new Error('Missing settings persistViews handler.');
        }
        if (typeof args?.writeViews !== 'function') {
            throw new Error('Missing settings writeViews handler.');
        }
        if (typeof args?.writePreset !== 'function') {
            throw new Error('Missing settings writePreset handler.');
        }
        const model = trimMaybeString(args?.model) || 'Model';
        const preset = normalizeProjectSimulationPreset(args?.preset);
        const views = normalizeVisualizationViews(args?.views);
        const previousViews = normalizeVisualizationViews(await args.loadViews({ model }));
        const persistedViews = normalizeVisualizationViews(
            await args.persistViews({ model, views, previousViews }),
        );
        if (typeof args?.removeStaleViews === 'function') {
            await args.removeStaleViews({
                model,
                previousViews,
                nextViews: persistedViews,
            });
        }
        const viewsSaved = await args.writeViews({
            model,
            views: persistedViews,
            previousViews,
        });
        if (viewsSaved === false) {
            throw new Error(
                trimMaybeString(args?.writeViewsError) || 'Failed to save visualization settings.',
            );
        }
        const presetSaved = await args.writePreset({
            model,
            preset,
            views: persistedViews,
            previousViews,
        });
        if (presetSaved === false) {
            throw new Error(
                trimMaybeString(args?.writePresetError) || 'Failed to save simulation preset.',
            );
        }
        if (typeof args?.afterSave === 'function') {
            await args.afterSave({
                model,
                preset,
                previousViews,
                views: persistedViews,
            });
        }
        return { ok: true, views: persistedViews };
    }

    async function resetHostedProjectSimulationSettings(args) {
        if (typeof args?.loadViews !== 'function') {
            throw new Error('Missing settings loadViews handler.');
        }
        if (typeof args?.resetPreset !== 'function') {
            throw new Error('Missing settings resetPreset handler.');
        }
        if (typeof args?.writeViews !== 'function') {
            throw new Error('Missing settings writeViews handler.');
        }
        if (typeof args?.readCurrent !== 'function') {
            throw new Error('Missing settings readCurrent handler.');
        }
        if (typeof args?.readViews !== 'function') {
            throw new Error('Missing settings readViews handler.');
        }
        const model = trimMaybeString(args?.model) || 'Model';
        const previousViews = normalizeVisualizationViews(await args.loadViews({ model }));
        if (typeof args?.removeViews === 'function') {
            await args.removeViews({ model, views: previousViews });
        }
        const presetReset = await args.resetPreset({ model, previousViews });
        if (presetReset === false) {
            throw new Error(
                trimMaybeString(args?.resetPresetError) || 'Failed to reset simulation preset.',
            );
        }
        const viewsReset = await args.writeViews({
            model,
            views: [],
            previousViews,
        });
        if (viewsReset === false) {
            throw new Error(
                trimMaybeString(args?.writeViewsError) || 'Failed to reset visualization settings.',
            );
        }
        const current = normalizeHostedSimulationSettingsCurrent(
            await args.readCurrent({ model }),
        );
        const views = normalizeVisualizationViews(await args.readViews({ model }));
        const result = {
            current: {
                solver: current.solver,
                tEnd: current.tEnd,
                dt: current.dt,
                outputDir: current.outputDir,
                modelicaPath: [...current.libraryOverrides],
            },
            views: views.length > 0
                ? views
                : normalizeVisualizationViews(args?.defaultViews),
        };
        if (typeof args?.afterReset === 'function') {
            await args.afterReset({
                model,
                previousViews,
                current: result.current,
                views: result.views,
            });
        }
        return result;
    }

    function normalizeSettingsOpenViewScriptResult(value) {
        if (value === undefined || value === null) {
            return { ok: true };
        }
        if (typeof value === 'string') {
            return { path: value };
        }
        return value;
    }

    function normalizeSettingsPickLibraryPathResult(value) {
        if (value === undefined || value === null || value === '') {
            return { path: undefined };
        }
        if (typeof value === 'string') {
            return { path: value };
        }
        return value;
    }

    function normalizeSettingsResyncSidecarsResult(value) {
        if (value && typeof value === 'object' && trimMaybeString(value.message)) {
            return value;
        }
        if (typeof value === 'string') {
            return { message: value };
        }
        if (value && typeof value === 'object') {
            const remapped = Number(
                value.remapped_models ?? value.remappedModels ?? 0,
            );
            const parseFailures = Number(
                value.parse_failures ?? value.parseFailures ?? 0,
            );
            return {
                message: `Resync complete: remapped=${remapped}, parseFailures=${parseFailures}`,
            };
        }
        return { message: 'Resync completed.' };
    }

    function normalizeSettingsPrepareModelsResult(value) {
        if (value && typeof value === 'object' && trimMaybeString(value.message)) {
            return value;
        }
        if (typeof value === 'string') {
            return { message: value };
        }
        const preparedModels = Array.isArray(value?.preparedModels) ? value.preparedModels : [];
        const failures = Array.isArray(value?.failures) ? value.failures : [];
        const preparedCount = preparedModels.length || Number(value?.preparedCount ?? 0);
        const failedCount = failures.length || Number(value?.failedCount ?? 0);
        const totalCount = Number(
            value?.totalModels
                ?? value?.requestedModels
                ?? value?.modelCount
                ?? preparedCount + failedCount,
        );
        if (totalCount > 0) {
            const failureSummary = failedCount > 0 ? ` (${failedCount} failed)` : '';
            return {
                message: `Prepared ${preparedCount}/${totalCount} simulation models${failureSummary}.`,
            };
        }
        return { message: 'Prepare completed.' };
    }

    function parseHostedBinaryExportRequest(payload, kind) {
        const rawPayload = payload && typeof payload === 'object' ? payload : {};
        const defaultStem = kind === 'webm' ? 'rumoca_viewer.webm' : 'rumoca_plot.png';
        const defaultExtension = kind === 'webm' ? '.webm' : '.png';
        const dataUrl = String(rawPayload.dataUrl ?? '');
        const defaultNameRaw = String(rawPayload.defaultName ?? defaultStem);
        const defaultName = defaultNameRaw
            .replace(/[^a-zA-Z0-9._-]+/g, '_')
            .replace(/^_+|_+$/g, '') || defaultStem;
        const match = kind === 'webm'
            ? dataUrl.match(/^data:video\/webm[^,]*;base64,(.+)$/)
            : dataUrl.match(/^data:image\/png;base64,(.+)$/);
        if (!match) {
            throw new Error(
                kind === 'webm'
                    ? 'Invalid WebM payload from results webview.'
                    : 'Invalid PNG payload from results webview.',
            );
        }
        return {
            base64: match[1],
            defaultName: defaultName.endsWith(defaultExtension)
                ? defaultName
                : `${defaultName}${defaultExtension}`,
        };
    }

    function normalizeHostedPngExportRequest(payload) {
        return parseHostedBinaryExportRequest(payload, 'png');
    }

    function normalizeHostedWebmExportRequest(payload) {
        return parseHostedBinaryExportRequest(payload, 'webm');
    }

    function normalizeHostedResultsNotifyPayload(payload) {
        const rawPayload = payload && typeof payload === 'object' ? payload : {};
        return {
            message: String(rawPayload.message ?? '').trim(),
        };
    }

    function buildHostedSimulationSettingsHandlers(args) {
        const getActiveModel = typeof args?.getActiveModel === 'function'
            ? args.getActiveModel
            : () => trimMaybeString(args && args.activeModel);
        const currentModel = () => trimMaybeString(getActiveModel()) || 'Model';
        const handlers = {
            async save({ payload }) {
                if (typeof args?.save !== 'function') {
                    throw new Error('Missing settings save handler.');
                }
                const normalized = normalizeHostedSimulationSettingsSavePayload(payload);
                const value = await args.save({
                    model: currentModel(),
                    preset: {
                        solver: normalized.solver,
                        tEnd: normalized.tEnd,
                        dt: normalized.dt,
                        outputDir: normalized.outputDir,
                        libraryOverrides: [...normalized.libraryOverrides],
                    },
                    current: {
                        solver: normalized.solver,
                        tEnd: normalized.tEnd,
                        dt: normalized.dt,
                        outputDir: normalized.outputDir,
                        modelicaPath: [...normalized.libraryOverrides],
                    },
                    views: normalized.views,
                    payload,
                });
                return value === undefined ? { ok: true } : value;
            },
            async reset() {
                if (typeof args?.reset !== 'function') {
                    throw new Error('Missing settings reset handler.');
                }
                return buildHostedSimulationSettingsResetValue(
                    await args.reset({ model: currentModel() }),
                );
            },
            async openModel({ payload }) {
                const model = trimMaybeString(payload && payload.model);
                if (!model) {
                    throw new Error('Missing model for settings.openModel');
                }
                if (typeof args?.openModel === 'function') {
                    return await args.openModel({ model, payload });
                }
                if (typeof args?.selectModel !== 'function') {
                    throw new Error('Missing settings openModel handler.');
                }
                const selection = await args.selectModel({ model, payload });
                const selectedModel = trimMaybeString(
                    selection && typeof selection === 'object'
                        ? (selection.model ?? selection.selectedModel)
                        : selection,
                ) || model;
                if (typeof args?.afterOpenModel === 'function') {
                    await args.afterOpenModel({
                        model: selectedModel,
                        selection,
                        payload,
                    });
                }
                return { ok: true, model: selectedModel };
            },
            async openViewScript({ payload }) {
                if (typeof args?.openViewScript !== 'function') {
                    throw new Error('Missing settings openViewScript handler.');
                }
                return normalizeSettingsOpenViewScriptResult(
                    await args.openViewScript({
                        model: currentModel(),
                        viewId: trimMaybeString(payload && payload.viewId),
                        payload,
                    }),
                );
            },
        };
        if (typeof args?.pickLibraryPath === 'function') {
            handlers.pickLibraryPath = async ({ payload }) =>
                normalizeSettingsPickLibraryPathResult(
                    await args.pickLibraryPath({ model: currentModel(), payload }),
                );
        }
        if (typeof args?.prepareModels === 'function') {
            handlers.prepareModels = async ({ payload }) =>
                normalizeSettingsPrepareModelsResult(
                    await args.prepareModels({ model: currentModel(), payload }),
                );
        }
        if (typeof args?.resyncSidecars === 'function') {
            handlers.resyncSidecars = async ({ payload }) =>
                normalizeSettingsResyncSidecarsResult(
                    await args.resyncSidecars({ model: currentModel(), payload }),
                );
        }
        for (const key of ['openWorkspaceSettings', 'openUserSettings']) {
            if (typeof args?.[key] === 'function') {
                handlers[key] = async ({ payload }) => await args[key]({ model: currentModel(), payload });
            }
        }
        return handlers;
    }

    function buildHostedSimulationSettingsDocument(args) {
        const state = normalizeHostedSimulationSettingsState(args);
        const activeModel = state.activeModel;
        const availableModels = state.availableModels;
        const current = state.current;
        const features = state.features;
        const addLibraryPathAttrs = features.addLibraryPath ? '' : ' style="display:none;"';
        const prepareModelsAttrs = features.prepareModels ? '' : ' style="display:none;"';
        const resyncSidecarsAttrs = features.resyncSidecars ? '' : ' style="display:none;"';
        const workspaceSettingsAttrs = features.workspaceSettings ? '' : ' style="display:none;"';
        const userSettingsAttrs = features.userSettings ? '' : ' style="display:none;"';
        const openViewScriptAttrs = features.openViewScript ? '' : ' style="display:none;"';
        const initialState = {
            activeModel,
            availableModels,
            current,
            views: state.views,
        };
        const initialStateJson = escapeInlineScriptJson(JSON.stringify(initialState));
        return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Rumoca Settings: ${escapeHtml(activeModel)}</title>
  <style>
    :root {
      --pad: 16px;
      --radius: 8px;
      --page-bg: var(--vscode-editor-background, #1e1e1e);
      --page-fg: var(--vscode-foreground, #d4d4d4);
      --card-bg: var(--vscode-sideBar-background, #252526);
      --panel-bg: var(--vscode-editorWidget-background, #252526);
      --input-bg: var(--vscode-input-background, #252526);
      --input-fg: var(--vscode-input-foreground, #d4d4d4);
      --input-border: var(--vscode-input-border, #3c3c3c);
      --button-bg: var(--vscode-button-background, #0e639c);
      --button-fg: var(--vscode-button-foreground, #ffffff);
      --button-secondary-bg: var(--vscode-button-secondaryBackground, #3a3d41);
      --button-secondary-fg: var(--vscode-button-secondaryForeground, #f0f0f0);
      --card-border: var(--vscode-panel-border, var(--input-border));
      --muted: var(--vscode-descriptionForeground, #a0a0a0);
      --ok: var(--vscode-testing-iconPassed, #89d185);
      --error: var(--vscode-errorForeground, #f48771);
    }
    html, body { height: 100%; }
    body {
      margin: 0;
      font-family: var(--vscode-font-family, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif);
      color: var(--page-fg);
      background: var(--page-bg);
    }
    .page {
      padding: var(--pad);
      display: grid;
      gap: 12px;
      min-height: 100%;
      grid-template-rows: auto auto auto auto 1fr auto;
    }
    .header {
      display: flex;
      align-items: flex-start;
      justify-content: space-between;
      gap: 12px;
    }
    .title {
      font-size: 18px;
      font-weight: 700;
      margin: 0;
      letter-spacing: 0.01em;
    }
    .subtitle {
      margin-top: 4px;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.4;
    }
    .card {
      border: 1px solid var(--card-border);
      border-radius: var(--radius);
      padding: 12px;
      background: var(--card-bg);
    }
    .card h3 {
      margin: 0 0 10px 0;
      font-size: 12px;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.04em;
      color: var(--muted);
    }
    .grid {
      display: grid;
      grid-template-columns: repeat(3, minmax(0, 1fr));
      gap: 10px;
    }
    .field { display: grid; gap: 4px; }
    label { font-size: 12px; font-weight: 600; }
    input, textarea, select {
      width: 100%;
      box-sizing: border-box;
      background: var(--input-bg);
      color: var(--input-fg);
      border: 1px solid var(--input-border);
      border-radius: 6px;
      padding: 7px 8px;
      font-size: 12px;
      min-height: 30px;
    }
    textarea { min-height: 140px; resize: vertical; font-family: var(--vscode-editor-font-family, monospace); }
    .hint { color: var(--muted); font-size: 11px; line-height: 1.35; }
    .row { display: flex; gap: 8px; align-items: center; }
    .row .grow { flex: 1; }
    .toolbar { display: flex; gap: 8px; margin-bottom: 8px; flex-wrap: wrap; }
    .results-layout {
      display: grid;
      grid-template-columns: minmax(220px, 0.9fr) minmax(320px, 1.6fr);
      gap: 10px;
      min-height: 280px;
    }
    .results-list-pane, .results-editor-pane {
      border: 1px solid var(--card-border);
      border-radius: 6px;
      padding: 10px;
      background: var(--page-bg);
    }
    #viewList { min-height: 180px; height: 100%; }
    .stack { display: grid; gap: 8px; }
    .mono { font-family: var(--vscode-editor-font-family, monospace); }
    #viewScriptRow { display: none; }
    @media (max-width: 980px) {
      .results-layout { grid-template-columns: 1fr; }
      #viewList { min-height: 120px; }
    }
    button {
      border: 0;
      border-radius: 6px;
      padding: 6px 10px;
      font-size: 12px;
      cursor: pointer;
      background: var(--button-bg);
      color: var(--button-fg);
    }
    button.secondary {
      background: var(--button-secondary-bg);
      color: var(--button-secondary-fg);
    }
    button.ghost {
      background: transparent;
      border: 1px solid var(--card-border);
      color: var(--page-fg);
    }
    .footer {
      position: sticky;
      bottom: 0;
      padding-top: 8px;
      background: linear-gradient(to bottom, transparent, var(--page-bg) 22%);
    }
    .actions {
      display: flex;
      gap: 8px;
      flex-wrap: wrap;
      align-items: center;
      border: 1px solid var(--card-border);
      border-radius: var(--radius);
      padding: 10px;
      background: var(--panel-bg);
    }
    .actions-hint {
      width: 100%;
      margin-top: 2px;
      font-size: 12px;
      color: var(--muted);
    }
    .status {
      margin-left: auto;
      font-size: 12px;
      color: var(--muted);
      min-height: 16px;
    }
    .status.ok { color: var(--ok); }
    .status.error { color: var(--error); }
    @media (max-width: 900px) {
      .grid { grid-template-columns: 1fr; }
      .status { width: 100%; margin-left: 0; }
    }
  </style>
</head>
<body>
  <div class="page">
    <div class="header">
      <div>
        <h1 class="title">Simulation Preset</h1>
        <div class="subtitle">One-click Play uses these values for this model.</div>
      </div>
      <div class="stack" style="min-width: 240px;">
        <div class="field">
          <label for="modelSelect">Model</label>
          <select id="modelSelect"></select>
        </div>
      </div>
    </div>

    <div class="card">
      <h3>Solver</h3>
      <div class="grid">
        <div class="field">
          <label for="solver">Method</label>
          <select id="solver">
            <option value="auto">auto</option>
            <option value="bdf">bdf</option>
            <option value="rk-like">rk-like</option>
          </select>
        </div>
        <div class="field">
          <label for="tEnd">End Time (t_end)</label>
          <input id="tEnd" type="number" step="0.01">
        </div>
        <div class="field">
          <label for="dt">Fixed Step (dt, optional)</label>
          <input id="dt" type="number" step="0.0001" placeholder="auto">
        </div>
      </div>
    </div>

    <div class="card">
      <h3>Libraries</h3>
      <div class="toolbar">
        <button id="addLib" class="ghost"${addLibraryPathAttrs}>Add Library Directory...</button>
        <button id="clearLibs" class="ghost">Clear</button>
      </div>
      <div class="field">
        <label for="modelicaPath">Additional Library Paths For This Model</label>
        <textarea id="modelicaPath" placeholder="/path/to/ModelicaStandardLibrary"></textarea>
        <div class="hint">If set, these are appended to the default library paths for this model.</div>
      </div>
    </div>

    <div class="card">
      <h3>Results Panels</h3>
      <div class="results-layout">
        <div class="results-list-pane">
          <div class="field">
            <label for="viewList">Configured Panels</label>
            <select id="viewList" size="9" class="mono"></select>
          </div>
          <div class="toolbar" style="margin-top: 8px;">
            <button id="addView" class="ghost">+ Add Panel</button>
            <button id="removeView" class="ghost">Remove</button>
          </div>
        </div>
        <div class="results-editor-pane stack">
          <div class="field">
            <label for="viewTitle">Panel Title</label>
            <input id="viewTitle" placeholder="States vs Time">
          </div>
          <div class="field">
            <label for="viewType">Type</label>
            <select id="viewType">
              <option value="timeseries">timeseries</option>
              <option value="scatter">scatter</option>
              <option value="3d">3d</option>
            </select>
          </div>
          <div class="field" id="viewXRow">
            <label id="viewXLabel" for="viewX">X Expression</label>
            <input id="viewX" placeholder="time">
          </div>
          <div class="field" id="viewYSeriesRow">
            <label for="viewY">Series / Variables</label>
            <textarea id="viewY" class="mono" style="min-height: 110px;" placeholder="*states"></textarea>
            <div id="viewYHint" class="hint">Use one variable per line. Special token: <code>*states</code>.</div>
          </div>
          <div class="field" id="viewYScatterRow" style="display:none;">
            <label for="viewScatterSeriesList">Scatter Series</label>
            <div class="results-layout" style="grid-template-columns: minmax(200px, 0.95fr) minmax(220px, 1.05fr); min-height: 220px;">
              <div class="results-list-pane" style="padding:8px;">
                <select id="viewScatterSeriesList" size="7" class="mono" style="min-height: 150px;"></select>
                <div class="toolbar" style="margin-top: 8px;">
                  <button id="addScatterSeries" class="ghost">+ Add Series</button>
                  <button id="removeScatterSeries" class="ghost">Remove Series</button>
                </div>
              </div>
              <div class="results-editor-pane stack" style="padding:8px;">
                <div class="field">
                  <label for="viewScatterSeriesName">Series Name</label>
                  <input id="viewScatterSeriesName" placeholder="Actual trajectory">
                </div>
                <div class="field">
                  <label for="viewScatterSeriesX">X Expression</label>
                  <input id="viewScatterSeriesX" placeholder="x">
                </div>
                <div class="field">
                  <label for="viewScatterSeriesY">Y Expression</label>
                  <input id="viewScatterSeriesY" placeholder="y">
                </div>
              </div>
            </div>
          </div>
          <div id="viewScriptRow" class="field">
            <label for="viewScriptPath">3D Script File</label>
            <div class="row">
              <input id="viewScriptPath" class="grow mono" placeholder=".rumoca/models/by-id/&lt;uuid&gt;/viewer_3d.js" readonly>
              <button id="openViewScript" class="ghost"${openViewScriptAttrs}>Open Script File</button>
            </div>
            <div class="hint">Rumoca stores 3D scripts next to the model sidecars under <code>.rumoca/models/by-id/&lt;uuid&gt;/</code>.</div>
          </div>
        </div>
      </div>
      <div class="hint" style="margin-top: 8px;">Play opens one <code>Rumoca Results</code> window with these configured subpanels.</div>
    </div>

    <div class="footer">
      <div class="actions">
        <button id="prepareModels" class="secondary"${prepareModelsAttrs}>Prepare All Models In File</button>
        <button id="reset" class="secondary">Reset Preset</button>
        <button id="resyncSidecars" class="secondary"${resyncSidecarsAttrs}>Resync Sidecars</button>
        <button id="workspaceSettings" class="secondary"${workspaceSettingsAttrs}>Workspace Settings</button>
        <button id="userSettings" class="secondary"${userSettingsAttrs}>User Settings</button>
        <div id="status" class="status"></div>
        <div class="actions-hint">Autosaves per-model preset for <code id="activeModelHint"></code> in <code>.rumoca/models/by-id/&lt;uuid&gt;/simulation.toml</code> and panel layout in <code>.rumoca/models/by-id/&lt;uuid&gt;/views.toml</code>. Play uses these values.</div>
      </div>
    </div>
  </div>

  <script>
    const vscodeApi = typeof acquireVsCodeApi === 'function' ? acquireVsCodeApi() : null;
    const browserHost = (() => {
      if (
        globalThis.RumocaSimulationSettingsHost
        && typeof globalThis.RumocaSimulationSettingsHost.request === 'function'
      ) {
        return globalThis.RumocaSimulationSettingsHost;
      }
      try {
        if (
          globalThis.parent
          && globalThis.parent !== globalThis
          && globalThis.parent.RumocaSimulationSettingsHost
          && typeof globalThis.parent.RumocaSimulationSettingsHost.request === 'function'
        ) {
          return globalThis.parent.RumocaSimulationSettingsHost;
        }
      } catch (_error) {
        return null;
      }
      return null;
    })();
    const initialState = ${initialStateJson};
    const solverInput = document.getElementById('solver');
    const tEndInput = document.getElementById('tEnd');
    const dtInput = document.getElementById('dt');
    const modelSelectInput = document.getElementById('modelSelect');
    const modelicaPathInput = document.getElementById('modelicaPath');
    const clearLibsBtn = document.getElementById('clearLibs');
    const statusEl = document.getElementById('status');
    const viewListInput = document.getElementById('viewList');
    const viewTitleInput = document.getElementById('viewTitle');
    const viewTypeInput = document.getElementById('viewType');
    const viewXRow = document.getElementById('viewXRow');
    const viewXLabelEl = document.getElementById('viewXLabel');
    const viewXInput = document.getElementById('viewX');
    const viewYInput = document.getElementById('viewY');
    const viewYSeriesRow = document.getElementById('viewYSeriesRow');
    const viewYScatterRow = document.getElementById('viewYScatterRow');
    const viewScatterSeriesListInput = document.getElementById('viewScatterSeriesList');
    const addScatterSeriesBtn = document.getElementById('addScatterSeries');
    const removeScatterSeriesBtn = document.getElementById('removeScatterSeries');
    const viewScatterSeriesNameInput = document.getElementById('viewScatterSeriesName');
    const viewScatterSeriesXInput = document.getElementById('viewScatterSeriesX');
    const viewScatterSeriesYInput = document.getElementById('viewScatterSeriesY');
    const viewYHintEl = document.getElementById('viewYHint');
    const viewScriptPathInput = document.getElementById('viewScriptPath');
    const viewScriptRow = document.getElementById('viewScriptRow');
    const openViewScriptBtn = document.getElementById('openViewScript');
    const pageEl = document.querySelector('.page');
    const activeModelHint = document.getElementById('activeModelHint');
    const pendingRequests = new Map();
    let nextRequestId = 1;
    const current = initialState && initialState.current ? initialState.current : {};
    const activeModelName = String(initialState && initialState.activeModel ? initialState.activeModel : '');
    const availableModels = Array.isArray(initialState && initialState.availableModels)
      ? initialState.availableModels.map((entry) => String(entry || '').trim()).filter(Boolean)
      : [activeModelName];
    const preservedOutputDir = String(current.outputDir || '');
    let views = Array.isArray(initialState && initialState.views) && initialState.views.length > 0
      ? initialState.views
      : [{ id: 'states_time', title: 'States vs Time', type: 'timeseries', x: 'time', y: ['*states'] }];
    let selectedViewIndex = views.length > 0 ? 0 : -1;
    let selectedScatterSeriesIndex = -1;
    let autoSaveTimer = null;
    let isResettingFromHost = false;

    function setStatus(text, level) {
      statusEl.textContent = text || '';
      statusEl.classList.remove('ok', 'error');
      if (level) statusEl.classList.add(level);
    }

    function requestHost(method, payload) {
      if (browserHost && typeof browserHost.request === 'function') {
        return Promise.resolve(browserHost.request(method, payload));
      }
      if (!vscodeApi) {
        return Promise.reject(new Error('VS Code webview API unavailable'));
      }
      const requestId = String(nextRequestId++);
      return new Promise((resolve, reject) => {
        pendingRequests.set(requestId, { resolve, reject });
        vscodeApi.postMessage({
          command: 'settings.request',
          requestId,
          method,
          payload,
        });
      });
    }

    window.addEventListener('message', (event) => {
      const message = event && event.data ? event.data : {};
      if (!message || message.command !== 'settings.response') {
        return;
      }
      const requestId = String(message.requestId || '');
      const pending = pendingRequests.get(requestId);
      if (!pending) {
        return;
      }
      pendingRequests.delete(requestId);
      if (message.ok === false) {
        pending.reject(new Error(String(message.error || 'Settings host request failed')));
        return;
      }
      pending.resolve(message.value);
    });

    function renderModelSelect() {
      modelSelectInput.innerHTML = '';
      for (const modelName of availableModels) {
        const option = document.createElement('option');
        option.value = String(modelName);
        option.textContent = String(modelName);
        if (String(modelName) === activeModelName) {
          option.selected = true;
        }
        modelSelectInput.appendChild(option);
      }
      if (activeModelHint) {
        activeModelHint.textContent = activeModelName;
      }
    }

    function parseSeriesList(text) {
      return String(text || '')
        .split(/\r?\n|,/)
        .map((v) => v.trim())
        .filter(Boolean);
    }

    function stringifySeries(list) {
      return (Array.isArray(list) ? list : []).map((v) => String(v).trim()).filter(Boolean).join('\n');
    }

    function makeUniqueViewId(prefix) {
      const base = (prefix || 'view').replace(/[^a-zA-Z0-9_]+/g, '_').toLowerCase();
      const existing = new Set(views.map((view) => String(view.id || '')));
      let candidate = base + '_' + String(views.length + 1);
      let counter = 1;
      while (existing.has(candidate)) {
        counter += 1;
        candidate = base + '_' + String(views.length + counter);
      }
      return candidate;
    }

    function getActiveView() {
      if (selectedViewIndex < 0 || selectedViewIndex >= views.length) {
        return null;
      }
      return views[selectedViewIndex];
    }

    function ensureScatterSeries(view) {
      if (!view || view.type !== 'scatter') {
        return [];
      }
      if (!Array.isArray(view.scatterSeries)) {
        view.scatterSeries = [];
      }
      if (view.scatterSeries.length === 0) {
        const fallbackX = String(view.x || 'time').trim() || 'time';
        const fallbackY = Array.isArray(view.y) && view.y.length > 0 ? String(view.y[0] || '').trim() : '';
        if (fallbackY.length > 0) {
          view.scatterSeries.push({
            name: fallbackY + ' vs ' + fallbackX,
            x: fallbackX,
            y: fallbackY,
          });
        }
      }
      view.scatterSeries = view.scatterSeries
        .map((entry) => {
          const name = String(entry && entry.name !== undefined ? entry.name : '').trim();
          const x = String(entry && entry.x !== undefined ? entry.x : '').trim();
          const y = String(entry && entry.y !== undefined ? entry.y : '').trim();
          if (x.length === 0 || y.length === 0) {
            return null;
          }
          return {
            name: name.length > 0 ? name : (y + ' vs ' + x),
            x,
            y,
          };
        })
        .filter(Boolean);
      return view.scatterSeries;
    }

    function setScatterSeriesEditorEnabled(enabled) {
      viewScatterSeriesListInput.disabled = !enabled;
      addScatterSeriesBtn.disabled = !enabled;
      removeScatterSeriesBtn.disabled = !enabled;
      viewScatterSeriesNameInput.disabled = !enabled;
      viewScatterSeriesXInput.disabled = !enabled;
      viewScatterSeriesYInput.disabled = !enabled;
    }

    function renderScatterSeriesEditor() {
      const view = getActiveView();
      if (!view || view.type !== 'scatter') {
        viewScatterSeriesListInput.innerHTML = '';
        viewScatterSeriesNameInput.value = '';
        viewScatterSeriesXInput.value = '';
        viewScatterSeriesYInput.value = '';
        selectedScatterSeriesIndex = -1;
        setScatterSeriesEditorEnabled(false);
        return;
      }
      const scatterSeries = ensureScatterSeries(view);
      viewScatterSeriesListInput.innerHTML = '';
      for (let index = 0; index < scatterSeries.length; index += 1) {
        const series = scatterSeries[index];
        const option = document.createElement('option');
        option.value = String(index);
        option.textContent = String(series.name || ('Series ' + String(index + 1)));
        viewScatterSeriesListInput.appendChild(option);
      }
      if (scatterSeries.length === 0) {
        selectedScatterSeriesIndex = -1;
      } else if (selectedScatterSeriesIndex < 0 || selectedScatterSeriesIndex >= scatterSeries.length) {
        selectedScatterSeriesIndex = 0;
      }
      setScatterSeriesEditorEnabled(true);
      if (selectedScatterSeriesIndex >= 0) {
        viewScatterSeriesListInput.value = String(selectedScatterSeriesIndex);
        const selectedSeries = scatterSeries[selectedScatterSeriesIndex];
        viewScatterSeriesNameInput.value = String(selectedSeries.name || '');
        viewScatterSeriesXInput.value = String(selectedSeries.x || '');
        viewScatterSeriesYInput.value = String(selectedSeries.y || '');
      } else {
        viewScatterSeriesNameInput.value = '';
        viewScatterSeriesXInput.value = '';
        viewScatterSeriesYInput.value = '';
      }
    }

    function commitScatterSeriesEditor() {
      const view = getActiveView();
      if (!view || view.type !== 'scatter') {
        return;
      }
      const scatterSeries = ensureScatterSeries(view);
      if (selectedScatterSeriesIndex < 0 || selectedScatterSeriesIndex >= scatterSeries.length) {
        return;
      }
      const currentSeries = scatterSeries[selectedScatterSeriesIndex];
      const nextX = String(viewScatterSeriesXInput.value || '').trim();
      const nextY = String(viewScatterSeriesYInput.value || '').trim();
      currentSeries.name = String(viewScatterSeriesNameInput.value || '').trim();
      currentSeries.x = nextX;
      currentSeries.y = nextY;
      if (!currentSeries.name) {
        currentSeries.name = nextY.length > 0 && nextX.length > 0
          ? (nextY + ' vs ' + nextX)
          : ('Series ' + String(selectedScatterSeriesIndex + 1));
      }
      ensureScatterSeries(view);
    }

    function setViewEditorEnabled(enabled) {
      viewTitleInput.disabled = !enabled;
      viewTypeInput.disabled = !enabled;
      viewXInput.disabled = !enabled;
      viewYInput.disabled = !enabled;
      viewScriptPathInput.disabled = !enabled;
      openViewScriptBtn.disabled = !enabled;
      document.getElementById('removeView').disabled = !enabled;
      if (!enabled) {
        setScatterSeriesEditorEnabled(false);
      }
    }

    function applyTypeHint(typeValue) {
      const type = String(typeValue || 'timeseries');
      if (type === '3d') {
        viewXRow.style.display = 'none';
        viewYSeriesRow.style.display = 'none';
        viewYScatterRow.style.display = 'none';
        viewScriptRow.style.display = 'grid';
        viewYHintEl.textContent = '';
      } else if (type === 'scatter') {
        viewXRow.style.display = 'none';
        viewXLabelEl.textContent = 'X Expression';
        viewYSeriesRow.style.display = 'none';
        viewYScatterRow.style.display = 'block';
        viewScriptRow.style.display = 'none';
        viewYHintEl.textContent = '';
      } else {
        viewXRow.style.display = 'block';
        viewXLabelEl.textContent = 'X Expression';
        viewYSeriesRow.style.display = 'block';
        viewYScatterRow.style.display = 'none';
        viewScriptRow.style.display = 'none';
        viewYHintEl.textContent = 'Use one variable per line. Special token: *states.';
      }
    }

    function renderViewEditor() {
      const valid = selectedViewIndex >= 0 && selectedViewIndex < views.length;
      setViewEditorEnabled(valid);
      if (!valid) {
        viewTitleInput.value = '';
        viewTypeInput.value = 'timeseries';
        viewXInput.value = '';
        viewYInput.value = '';
        viewScriptPathInput.value = '';
        selectedScatterSeriesIndex = -1;
        renderScatterSeriesEditor();
        applyTypeHint('timeseries');
        return;
      }
      const view = views[selectedViewIndex];
      viewTitleInput.value = String(view.title || '');
      viewTypeInput.value = String(view.type || 'timeseries');
      viewXInput.value = String(view.x || '');
      viewYInput.value = stringifySeries(view.y);
      viewScriptPathInput.value = String(view.scriptPath || '').trim();
      if (view.type === 'scatter') {
        ensureScatterSeries(view);
      } else {
        selectedScatterSeriesIndex = -1;
      }
      applyTypeHint(view.type);
      renderScatterSeriesEditor();
    }

    function renderViewList() {
      viewListInput.innerHTML = '';
      for (let index = 0; index < views.length; index += 1) {
        const view = views[index];
        const option = document.createElement('option');
        const title = String(view.title || view.id || ('View ' + String(index + 1)));
        option.value = String(index);
        option.textContent = title + ' [' + String(view.type || 'timeseries') + ']';
        viewListInput.appendChild(option);
      }
      if (views.length === 0) {
        selectedViewIndex = -1;
      } else if (selectedViewIndex < 0 || selectedViewIndex >= views.length) {
        selectedViewIndex = 0;
      }
      if (selectedViewIndex >= 0) {
        viewListInput.value = String(selectedViewIndex);
      }
      renderViewEditor();
    }

    function commitViewEditor() {
      if (selectedViewIndex < 0 || selectedViewIndex >= views.length) {
        return;
      }
      const view = views[selectedViewIndex];
      const type = String(viewTypeInput.value || 'timeseries');
      view.type = (type === '3d' || type === 'scatter') ? type : 'timeseries';
      view.title = String(viewTitleInput.value || '').trim();
      if (view.type === '3d') {
        view.x = undefined;
        view.y = [];
        view.scatterSeries = undefined;
      } else if (view.type === 'scatter') {
        view.x = String(viewXInput.value || '').trim();
        if (view.x.length === 0) {
          view.x = undefined;
        }
        commitScatterSeriesEditor();
        const scatterSeries = ensureScatterSeries(view);
        view.scatterSeries = scatterSeries;
        if (scatterSeries.length > 0) {
          view.x = scatterSeries[0].x;
          view.y = [scatterSeries[0].y];
        } else {
          view.y = [];
        }
      } else {
        view.x = String(viewXInput.value || '').trim();
        if (view.x.length === 0) {
          view.x = undefined;
        }
        view.y = parseSeriesList(viewYInput.value);
        view.scatterSeries = undefined;
      }
      if (view.type !== '3d') {
        view.script = undefined;
        view.scriptPath = undefined;
      }
      if (!view.id || String(view.id).trim().length === 0) {
        view.id = makeUniqueViewId(view.type);
      }
      if (!view.title || String(view.title).trim().length === 0) {
        view.title = 'View ' + String(selectedViewIndex + 1);
      }
    }

    function addView(type) {
      commitViewEditor();
      const nextType = type === '3d' || type === 'scatter' ? type : 'timeseries';
      views.push({
        id: makeUniqueViewId(nextType),
        title: 'New Panel',
        type: nextType,
        x: nextType === 'timeseries' ? 'time' : undefined,
        y: nextType === 'timeseries' ? ['*states'] : [],
        scatterSeries: nextType === 'scatter' ? [{ name: 'Series 1', x: 'x', y: 'y' }] : undefined,
        scriptPath: undefined,
      });
      selectedViewIndex = views.length - 1;
      renderViewList();
    }

    function collectSavePayload() {
      commitViewEditor();
      const libs = modelicaPathInput.value
        .split(/\r?\n/)
        .map((value) => value.trim())
        .filter(Boolean);
      const normalizedViews = views.map((view, index) => {
        const type = String(view.type || 'timeseries');
        const normalizedType = type === '3d' || type === 'scatter' ? type : 'timeseries';
        const yValues = Array.isArray(view.y) ? view.y.map((value) => String(value).trim()).filter(Boolean) : [];
        const xValue = view.x === undefined || view.x === null ? undefined : String(view.x).trim();
        const scatterSeriesValues = Array.isArray(view.scatterSeries)
          ? view.scatterSeries
              .map((series) => ({
                name: String(series && series.name !== undefined ? series.name : '').trim(),
                x: String(series && series.x !== undefined ? series.x : '').trim(),
                y: String(series && series.y !== undefined ? series.y : '').trim(),
              }))
              .filter((series) => series.x.length > 0 && series.y.length > 0)
          : [];
        const scriptPathValue = view.scriptPath === undefined || view.scriptPath === null
          ? undefined
          : String(view.scriptPath).trim();
        const normalizedX = normalizedType === '3d'
          ? undefined
          : xValue && xValue.length > 0
            ? xValue
            : undefined;
        const normalizedY = normalizedType === '3d' ? [] : yValues;
        return {
          id: String(view.id || ('view_' + String(index + 1))).trim(),
          title: String(view.title || ('View ' + String(index + 1))).trim(),
          type: normalizedType,
          x: normalizedX,
          y: normalizedY,
          scatterSeries: normalizedType === 'scatter' ? scatterSeriesValues : undefined,
          scriptPath: normalizedType === '3d' && scriptPathValue && scriptPathValue.length > 0 ? scriptPathValue : undefined,
        };
      });
      return {
        solver: solverInput.value,
        tEnd: Number(tEndInput.value),
        dt: dtInput.value.trim(),
        outputDir: preservedOutputDir,
        modelicaPath: libs,
        views: normalizedViews,
      };
    }

    async function saveNow() {
      if (isResettingFromHost) {
        return;
      }
      setStatus('Saving…');
      try {
        await requestHost('save', collectSavePayload());
        setStatus('Saved.', 'ok');
      } catch (error) {
        setStatus(String(error && error.message ? error.message : error), 'error');
      }
    }

    function scheduleAutoSave() {
      if (isResettingFromHost) {
        return;
      }
      if (autoSaveTimer) {
        clearTimeout(autoSaveTimer);
      }
      setStatus('Saving…');
      autoSaveTimer = setTimeout(() => {
        autoSaveTimer = null;
        void saveNow();
      }, 500);
    }

    function shouldDelegateAutoSave(target) {
      if (!(target instanceof HTMLElement)) return false;
      if (target.closest('#reset, #workspaceSettings, #userSettings, #openViewScript, #addLib, #prepareModels, #resyncSidecars')) {
        return false;
      }
      return !!target.closest('input, textarea, select');
    }

    if (pageEl) {
      pageEl.addEventListener('input', (event) => {
        if (shouldDelegateAutoSave(event.target)) {
          scheduleAutoSave();
        }
      }, true);
      pageEl.addEventListener('change', (event) => {
        if (shouldDelegateAutoSave(event.target)) {
          scheduleAutoSave();
        }
      }, true);
    }

    document.getElementById('addView').addEventListener('click', () => {
      const requestedType = String(viewTypeInput.value || 'timeseries');
      addView(requestedType);
      scheduleAutoSave();
    });
    document.getElementById('removeView').addEventListener('click', () => {
      if (selectedViewIndex < 0 || selectedViewIndex >= views.length) return;
      views.splice(selectedViewIndex, 1);
      if (selectedViewIndex >= views.length) {
        selectedViewIndex = views.length - 1;
      }
      renderViewList();
      scheduleAutoSave();
    });

    viewListInput.addEventListener('change', () => {
      commitViewEditor();
      const parsed = Number(viewListInput.value);
      selectedViewIndex = Number.isFinite(parsed) ? parsed : -1;
      renderViewEditor();
    });

    viewTypeInput.addEventListener('change', () => {
      const nextType = String(viewTypeInput.value || 'timeseries');
      applyTypeHint(nextType);
      commitViewEditor();
      renderViewList();
    });

    openViewScriptBtn.addEventListener('click', async () => {
      commitViewEditor();
      if (selectedViewIndex < 0 || selectedViewIndex >= views.length) {
        return;
      }
      const view = views[selectedViewIndex];
      if (view.type !== '3d') {
        setStatus('Selected panel is not a 3D viewer.', 'error');
        return;
      }
      try {
        const result = await requestHost('openViewScript', { viewId: view.id });
        if (result && result.path) {
          setStatus('Opened script: ' + result.path, 'ok');
        }
      } catch (error) {
        setStatus(String(error && error.message ? error.message : error), 'error');
      }
    });

    viewTitleInput.addEventListener('blur', () => {
      commitViewEditor();
      renderViewList();
    });

    function addScatterSeriesFromPrompt() {
      const view = getActiveView();
      if (!view || view.type !== 'scatter') {
        return;
      }
      const nameRaw = window.prompt('Scatter series name:', 'Series ' + String((view.scatterSeries?.length ?? 0) + 1));
      if (nameRaw === null) return;
      const xRaw = window.prompt('Scatter series X expression:', 'x');
      if (xRaw === null) return;
      const yRaw = window.prompt('Scatter series Y expression:', 'y');
      if (yRaw === null) return;
      const x = String(xRaw || '').trim();
      const y = String(yRaw || '').trim();
      if (x.length === 0 || y.length === 0) {
        setStatus('Scatter series requires both X and Y expressions.', 'error');
        return;
      }
      const scatterSeries = ensureScatterSeries(view);
      const nextName = String(nameRaw || '').trim();
      scatterSeries.push({
        name: nextName.length > 0 ? nextName : (y + ' vs ' + x),
        x,
        y,
      });
      selectedScatterSeriesIndex = scatterSeries.length - 1;
      renderScatterSeriesEditor();
      commitViewEditor();
      renderViewList();
      scheduleAutoSave();
    }

    addScatterSeriesBtn.addEventListener('click', () => {
      addScatterSeriesFromPrompt();
    });
    removeScatterSeriesBtn.addEventListener('click', () => {
      const view = getActiveView();
      if (!view || view.type !== 'scatter') return;
      const scatterSeries = ensureScatterSeries(view);
      if (selectedScatterSeriesIndex < 0 || selectedScatterSeriesIndex >= scatterSeries.length) return;
      scatterSeries.splice(selectedScatterSeriesIndex, 1);
      if (selectedScatterSeriesIndex >= scatterSeries.length) {
        selectedScatterSeriesIndex = scatterSeries.length - 1;
      }
      renderScatterSeriesEditor();
      commitViewEditor();
      renderViewList();
      scheduleAutoSave();
    });
    viewScatterSeriesListInput.addEventListener('change', () => {
      commitScatterSeriesEditor();
      const parsed = Number(viewScatterSeriesListInput.value);
      selectedScatterSeriesIndex = Number.isFinite(parsed) ? parsed : -1;
      renderScatterSeriesEditor();
      commitViewEditor();
      renderViewList();
      scheduleAutoSave();
    });
    viewScatterSeriesNameInput.addEventListener('blur', () => {
      commitScatterSeriesEditor();
      renderScatterSeriesEditor();
      commitViewEditor();
      renderViewList();
      scheduleAutoSave();
    });
    viewScatterSeriesXInput.addEventListener('blur', () => {
      commitScatterSeriesEditor();
      renderScatterSeriesEditor();
      commitViewEditor();
      renderViewList();
      scheduleAutoSave();
    });
    viewScatterSeriesYInput.addEventListener('blur', () => {
      commitScatterSeriesEditor();
      renderScatterSeriesEditor();
      commitViewEditor();
      renderViewList();
      scheduleAutoSave();
    });

    document.getElementById('reset').addEventListener('click', async () => {
      setStatus('Resetting…');
      try {
        const reset = await requestHost('reset', {});
        isResettingFromHost = true;
        if (autoSaveTimer) {
          clearTimeout(autoSaveTimer);
          autoSaveTimer = null;
        }
        solverInput.value = String(reset && reset.solver ? reset.solver : 'auto');
        tEndInput.value = String(reset && reset.tEnd !== undefined ? reset.tEnd : 10);
        dtInput.value = String(reset && reset.dt ? reset.dt : '');
        modelicaPathInput.value = Array.isArray(reset && reset.modelicaPath) ? reset.modelicaPath.join('\n') : '';
        views = Array.isArray(reset && reset.views) && reset.views.length > 0
          ? reset.views
          : [{ id: 'states_time', title: 'States vs Time', type: 'timeseries', x: 'time', y: ['*states'] }];
        selectedViewIndex = views.length > 0 ? 0 : -1;
        renderViewList();
        setStatus('Reset to global defaults.', 'ok');
      } catch (error) {
        setStatus(String(error && error.message ? error.message : error), 'error');
      } finally {
        isResettingFromHost = false;
      }
    });

    document.getElementById('prepareModels').addEventListener('click', async () => {
      setStatus('Preparing models…');
      try {
        const result = await requestHost('prepareModels', {});
        setStatus(result && result.message ? result.message : 'Prepare completed.', 'ok');
      } catch (error) {
        setStatus(String(error && error.message ? error.message : error), 'error');
      }
    });

    document.getElementById('resyncSidecars').addEventListener('click', async () => {
      setStatus('Resyncing sidecars…');
      try {
        const result = await requestHost('resyncSidecars', {});
        setStatus(result && result.message ? result.message : 'Resync completed.', 'ok');
      } catch (error) {
        setStatus(String(error && error.message ? error.message : error), 'error');
      }
    });

    modelSelectInput.addEventListener('change', async () => {
      const nextModel = String(modelSelectInput.value || '').trim();
      if (!nextModel || nextModel === activeModelName) {
        return;
      }
      setStatus('Opening model settings…');
      try {
        await requestHost('openModel', { model: nextModel });
      } catch (error) {
        setStatus(String(error && error.message ? error.message : error), 'error');
      }
    });

    document.getElementById('addLib').addEventListener('click', async () => {
      try {
        const result = await requestHost('pickLibraryPath', {});
        if (result && result.path) {
          const lines = modelicaPathInput.value
            .split(/\r?\n/)
            .map((value) => value.trim())
            .filter(Boolean);
          if (!lines.includes(result.path)) {
            lines.push(result.path);
            modelicaPathInput.value = lines.join('\n');
            scheduleAutoSave();
          }
        }
      } catch (error) {
        setStatus(String(error && error.message ? error.message : error), 'error');
      }
    });

    clearLibsBtn.addEventListener('click', () => {
      modelicaPathInput.value = '';
      scheduleAutoSave();
    });

    document.getElementById('workspaceSettings').addEventListener('click', async () => {
      try {
        await requestHost('openWorkspaceSettings', {});
      } catch (error) {
        setStatus(String(error && error.message ? error.message : error), 'error');
      }
    });

    document.getElementById('userSettings').addEventListener('click', async () => {
      try {
        await requestHost('openUserSettings', {});
      } catch (error) {
        setStatus(String(error && error.message ? error.message : error), 'error');
      }
    });

    solverInput.value = String(current.solver || 'auto');
    tEndInput.value = String(current.tEnd || 10);
    dtInput.value = current.dt === null || current.dt === undefined ? '' : String(current.dt);
    modelicaPathInput.value = Array.isArray(current.libraryOverrides) ? current.libraryOverrides.join('\n') : '';
    renderModelSelect();
    renderViewList();
  </script>
</body>
</html>`;
    }

    async function handleHostedSimulationSettingsRequest(args) {
        const message = args && args.message;
        if (!message || typeof message !== 'object') {
            return false;
        }
        const command = trimMaybeString(message.command);
        if (command !== 'settings.request') {
            return false;
        }
        const requestId = trimMaybeString(message.requestId);
        const method = trimMaybeString(message.method);
        if (!requestId || !method) {
            return true;
        }
        const payload = message.payload;
        const handlers = args && args.handlers ? args.handlers : {};
        const postMessage = typeof args?.postMessage === 'function' ? args.postMessage : null;

        async function respond(ok, value, error) {
            if (!postMessage) {
                return;
            }
            await postMessage({
                command: 'settings.response',
                requestId,
                ok,
                value,
                error,
            });
        }

        const handler = handlers[method];
        if (typeof handler !== 'function') {
            const detail = `Unknown settings request: ${method}`;
            if (typeof args?.onError === 'function') {
                args.onError({ method, error: new Error(detail) });
            }
            await respond(false, undefined, detail);
            return true;
        }

        try {
            const value = await handler({ method, payload });
            await respond(true, value);
        } catch (error) {
            const detail = String(error && error.message ? error.message : error);
            if (typeof args?.onError === 'function') {
                args.onError({ method, error });
            }
            await respond(false, undefined, detail);
        }
        return true;
    }

    function buildHostedResultsDocument(args) {
        const safeViews = normalizeVisualizationViews(args && args.views).length > 0
            ? normalizeVisualizationViews(args && args.views)
            : defaultVisualizationViews();
        const viewsJson = escapeInlineScriptJson(JSON.stringify(safeViews));
        const payloadJson = escapeInlineScriptJson(JSON.stringify(args && args.payload ? args.payload : null));
        const metricsJson = escapeInlineScriptJson(JSON.stringify(args && args.metrics ? args.metrics : null));
        const modelName = trimMaybeString(args && args.model) || 'Rumoca Results';
        const modelJson = escapeInlineScriptJson(JSON.stringify(modelName));
        const panelStateJson = escapeInlineScriptJson(JSON.stringify(
            buildHostedResultsPanelState(args && args.panelState) || null,
        ));
        const assets = args && args.assets ? args.assets : {};
        return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Rumoca Results: ${escapeHtml(modelName)}</title>
  <link rel="stylesheet" href="${escapeHtml(assets.uplotCss || '')}">
  <link rel="stylesheet" href="${escapeHtml(assets.resultsAppCss || '')}">
</head>
<body>
  <div id="resultsRoot" style="position:fixed;inset:0;"></div>
  <script src="${escapeHtml(assets.uplotJs || '')}"></script>
  <script src="${escapeHtml(assets.threeJs || '')}"></script>
  <script src="${escapeHtml(assets.visualizationSharedJs || '')}"></script>
  <script src="${escapeHtml(assets.resultsAppJs || '')}"></script>
  <script>
    const configuredViews = ${viewsJson};
    const runPayload = ${payloadJson};
    const runMetrics = ${metricsJson};
    const modelName = ${modelJson};
    const panelState = ${panelStateJson};
    const vscodeApi = typeof acquireVsCodeApi === 'function' ? acquireVsCodeApi() : null;
    const root = document.getElementById('resultsRoot');
    const pendingRequests = new Map();
    let nextRequestId = 1;

    function readPanelState() {
      if (!vscodeApi) {
        return panelState || {};
      }
      try {
        return vscodeApi.getState() || panelState || {};
      } catch (_) {
        return panelState || {};
      }
    }

    function writePanelState(patch) {
      if (!vscodeApi) {
        return panelState || {};
      }
      const nextState = Object.assign({}, panelState || {}, readPanelState(), patch || {});
      try {
        vscodeApi.setState(nextState);
      } catch (_) {
        // ignore panel-state persistence errors
      }
      return nextState;
    }

    if (vscodeApi) {
      writePanelState({});
    }

    function requestHost(method, payload) {
      if (!vscodeApi) {
        return Promise.reject(new Error('VS Code webview API unavailable'));
      }
      const requestId = String(nextRequestId++);
      return new Promise((resolve, reject) => {
        pendingRequests.set(requestId, { resolve, reject });
        vscodeApi.postMessage({
          command: 'results.request',
          requestId,
          method,
          payload,
        });
      });
    }

    window.addEventListener('message', (event) => {
      const message = event && event.data ? event.data : {};
      if (!message || message.command !== 'results.response') {
        return;
      }
      const requestId = String(message.requestId || '');
      const pending = pendingRequests.get(requestId);
      if (!pending) {
        return;
      }
      pendingRequests.delete(requestId);
      if (message.ok === false) {
        pending.reject(new Error(String(message.error || 'Results host request failed')));
        return;
      }
      pending.resolve(message.value);
    });

    const baseState = readPanelState();
    const modelRef = {
      model: modelName,
      workspaceRoot: typeof baseState.workspaceRoot === 'string' ? baseState.workspaceRoot : undefined,
      runId: typeof baseState.runId === 'string' ? baseState.runId : undefined,
      title: typeof baseState.title === 'string' ? baseState.title : undefined,
    };

    const bridge = vscodeApi
      ? {
          loadViews: () => requestHost('loadViews', { modelRef }),
          saveViews: (_ignored, nextViews) => requestHost('saveViews', { modelRef, views: nextViews }),
          resetViews: () => requestHost('resetViews', { modelRef }),
          savePng: (payload) => { void requestHost('savePng', Object.assign({ modelRef }, payload || {})); },
          saveWebm: (payload) => { void requestHost('saveWebm', Object.assign({ modelRef }, payload || {})); },
          notify: (message) => { void requestHost('notify', { modelRef, message }).catch(() => undefined); },
          persistState: (nextState) => { writePanelState(nextState); },
        }
      : {
          persistState: () => {},
        };

    const app = RumocaResultsApp.createResultsApp({
      root,
      model: modelName,
      modelRef,
      payload: runPayload,
      views: configuredViews,
      metrics: runMetrics,
      activeViewId:
        typeof baseState.activeViewId === 'string' && baseState.activeViewId.length > 0
          ? baseState.activeViewId
          : undefined,
      bridge,
    });

    window.addEventListener('beforeunload', () => {
      if (app && typeof app.dispose === 'function') {
        app.dispose();
      }
    });
  </script>
</body>
</html>`;
    }

    async function loadHostedProjectResultsViews(args) {
        if (typeof args?.loadConfiguredViews !== 'function') {
            throw new Error('Missing results loadConfiguredViews handler.');
        }
        if (typeof args?.hydrateViews !== 'function') {
            throw new Error('Missing results hydrateViews handler.');
        }
        const model = trimMaybeString(args?.model);
        if (!model) {
            throw new Error('Results request missing model reference.');
        }
        const workspaceRoot = trimMaybeString(args?.workspaceRoot) || undefined;
        const configuredViews = normalizeVisualizationViews(
            await args.loadConfiguredViews({ model, workspaceRoot }),
        );
        const baseViews = configuredViews.length > 0
            ? configuredViews
            : normalizeVisualizationViews(args?.defaultViews);
        return {
            views: normalizeVisualizationViews(
                await args.hydrateViews({ model, workspaceRoot, views: baseViews }),
            ),
        };
    }

    async function saveHostedProjectResultsViews(args) {
        if (typeof args?.loadConfiguredViews !== 'function') {
            throw new Error('Missing results loadConfiguredViews handler.');
        }
        if (typeof args?.persistViews !== 'function') {
            throw new Error('Missing results persistViews handler.');
        }
        if (typeof args?.writeConfiguredViews !== 'function') {
            throw new Error('Missing results writeConfiguredViews handler.');
        }
        if (typeof args?.hydrateViews !== 'function') {
            throw new Error('Missing results hydrateViews handler.');
        }
        const model = trimMaybeString(args?.model);
        if (!model) {
            throw new Error('Results request missing model reference.');
        }
        const workspaceRoot = trimMaybeString(args?.workspaceRoot) || undefined;
        const previousViews = normalizeVisualizationViews(
            await args.loadConfiguredViews({ model, workspaceRoot }),
        );
        const nextViews = normalizeVisualizationViews(args?.views);
        const persistedViews = normalizeVisualizationViews(
            await args.persistViews({
                model,
                workspaceRoot,
                views: nextViews,
                previousViews,
            }),
        );
        const saved = await args.writeConfiguredViews({
            model,
            workspaceRoot,
            views: persistedViews,
            previousViews,
        });
        if (saved === false) {
            throw new Error(
                trimMaybeString(args?.writeViewsError) || 'Failed to save visualization settings.',
            );
        }
        return {
            views: normalizeVisualizationViews(
                await args.hydrateViews({ model, workspaceRoot, views: persistedViews }),
            ),
        };
    }

    async function resetHostedProjectResultsViews(args) {
        if (typeof args?.loadConfiguredViews !== 'function') {
            throw new Error('Missing results loadConfiguredViews handler.');
        }
        if (typeof args?.writeConfiguredViews !== 'function') {
            throw new Error('Missing results writeConfiguredViews handler.');
        }
        const model = trimMaybeString(args?.model);
        if (!model) {
            throw new Error('Results request missing model reference.');
        }
        const workspaceRoot = trimMaybeString(args?.workspaceRoot) || undefined;
        const previousViews = normalizeVisualizationViews(
            await args.loadConfiguredViews({ model, workspaceRoot }),
        );
        if (typeof args?.removeViews === 'function') {
            await args.removeViews({ model, workspaceRoot, views: previousViews });
        }
        const reset = await args.writeConfiguredViews({
            model,
            workspaceRoot,
            views: [],
            previousViews,
        });
        if (reset === false) {
            throw new Error(
                trimMaybeString(args?.writeViewsError) || 'Failed to reset visualization settings.',
            );
        }
        return await loadHostedProjectResultsViews({
            model,
            workspaceRoot,
            loadConfiguredViews: args.loadConfiguredViews,
            hydrateViews: args.hydrateViews,
            defaultViews: args.defaultViews,
        });
    }

    async function handleHostedResultsRequest(args) {
        const message = args && args.message;
        if (!message || typeof message !== 'object') {
            return false;
        }
        const command = trimMaybeString(message.command);
        if (command !== 'results.request') {
            return false;
        }
        const requestId = trimMaybeString(message.requestId);
        const method = trimMaybeString(message.method);
        if (!requestId || !method) {
            return true;
        }
        const payload = message.payload;
        const handlers = args && args.handlers ? args.handlers : {};
        const postMessage = typeof args?.postMessage === 'function' ? args.postMessage : null;
        const fallbackWorkspaceRoot =
            typeof args?.fallbackWorkspaceRoot === 'function'
                ? args.fallbackWorkspaceRoot()
                : args?.fallbackWorkspaceRoot;
        const modelRef = normalizeHostedResultsModelRef(
            payload && typeof payload === 'object' ? payload.modelRef : undefined,
            fallbackWorkspaceRoot,
        );

        async function respond(ok, value, error) {
            if (!postMessage) {
                return;
            }
            await postMessage({
                command: 'results.response',
                requestId,
                ok,
                value,
                error,
            });
        }

        const handler = handlers[method];
        if (typeof handler !== 'function') {
            const detail = `Unknown results request: ${method}`;
            if (typeof args?.onError === 'function') {
                args.onError({ method, error: new Error(detail) });
            }
            await respond(false, undefined, detail);
            return true;
        }

        try {
            const value = await handler({ method, payload, modelRef });
            await respond(true, value);
        } catch (error) {
            const detail = String(error && error.message ? error.message : error);
            if (typeof args?.onError === 'function') {
                args.onError({ method, error });
            }
            await respond(false, undefined, detail);
        }
        return true;
    }

    function buildSimulationRunDocument(args) {
        if (!args || typeof args !== 'object') {
            return undefined;
        }
        const runId = normalizeRunId(args.runId);
        const model = trimMaybeString(args.model);
        const payload = normalizeSimulationPayload(args.payload);
        if (!runId || !model || !payload) {
            return undefined;
        }
        const savedAtUnixMs = Number(args.savedAtUnixMs);
        const metrics = normalizeSimulationRunMetrics(args.metrics);
        const views = normalizeVisualizationViews(args.views);
        return {
            version: 1,
            runId,
            model,
            savedAtUnixMs: Number.isFinite(savedAtUnixMs) ? savedAtUnixMs : undefined,
            payload,
            metrics: metrics || null,
            views,
        };
    }

    function normalizePersistedSimulationRun(raw) {
        if (!raw || typeof raw !== 'object') {
            return undefined;
        }
        const doc = buildSimulationRunDocument(raw);
        if (!doc) {
            return undefined;
        }
        return {
            runId: doc.runId,
            model: doc.model,
            payload: doc.payload,
            metrics: doc.metrics || undefined,
            views: doc.views,
            savedAtUnixMs: doc.savedAtUnixMs,
        };
    }

    function buildVisualizationModel(result, rawView) {
        const view = rawView && typeof rawView === 'object' ? rawView : defaultVisualizationViews()[0];
        const type = trimMaybeString(view.type).toLowerCase();
        const normalizedType = type === 'scatter' || type === '3d' ? type : 'timeseries';
        if (normalizedType === '3d') {
            const xSeries = resolveSeries(result, view.x, 'time');
            const yNames = expandRequestedSeries(result, view.y);
            const ySeries = resolveSeries(result, yNames[0], null);
            const zSeries = resolveSeries(result, yNames[1], null);
            const values = [];
            const count = Math.min(
                xSeries?.values?.length ?? 0,
                ySeries?.values?.length ?? 0,
                zSeries?.values?.length ?? 0,
            );
            for (let index = 0; index < count; index += 1) {
                values.push({
                    x: Number(xSeries.values[index]),
                    y: Number(ySeries.values[index]),
                    z: Number(zSeries.values[index]),
                });
            }
            return {
                type: '3d',
                title: trimMaybeString(view.title) || '3D View',
                points: values,
                script: trimMaybeString(view.script) || undefined,
                scriptPath: trimMaybeString(view.scriptPath) || undefined,
                labels: {
                    x: xSeries?.name || 'x',
                    y: ySeries?.name || 'y',
                    z: zSeries?.name || 'z',
                },
                times: Array.isArray(result?.allData?.[0]) ? result.allData[0].map(Number) : [],
            };
        }

        const xSeries = resolveSeries(result, view.x, 'time');
        const ySeries = expandRequestedSeries(result, view.y)
            .map((name) => resolveSeries(result, name, null))
            .filter(Boolean)
            .map((series, index) => ({
                name: series.name,
                values: series.values,
                color: seriesColor(index),
            }));
        return {
            type: normalizedType,
            title: trimMaybeString(view.title) || 'View',
            x: xSeries,
            y: ySeries,
        };
    }


    const PROJECT_CONFIG_PATH = '.rumoca/project.toml';
    const MODEL_IDENTITY_FILE = 'identity.toml';
    const MODEL_SIMULATION_FILE = 'simulation.toml';
    const MODEL_VIEWS_FILE = 'views.toml';

    function stripTomlComment(line) {
        let inString = false;
        let escaped = false;
        let out = '';
        for (const ch of String(line || '')) {
            if (escaped) {
                out += ch;
                escaped = false;
                continue;
            }
            if (ch === '\\' && inString) {
                out += ch;
                escaped = true;
                continue;
            }
            if (ch === '"') {
                inString = !inString;
                out += ch;
                continue;
            }
            if (ch === '#' && !inString) {
                break;
            }
            out += ch;
        }
        return out.trim();
    }

    function splitTomlArrayItems(inner) {
        const items = [];
        let current = '';
        let inString = false;
        let escaped = false;
        for (const ch of inner) {
            if (escaped) {
                current += ch;
                escaped = false;
                continue;
            }
            if (ch === '\\' && inString) {
                current += ch;
                escaped = true;
                continue;
            }
            if (ch === '"') {
                inString = !inString;
                current += ch;
                continue;
            }
            if (ch === ',' && !inString) {
                items.push(current.trim());
                current = '';
                continue;
            }
            current += ch;
        }
        if (current.trim()) {
            items.push(current.trim());
        }
        return items;
    }

    function parseTomlValue(raw) {
        const value = String(raw || '').trim();
        if (!value) return null;
        if (value.startsWith('"') && value.endsWith('"')) {
            return JSON.parse(value);
        }
        if (value.startsWith('[') && value.endsWith(']')) {
            const inner = value.slice(1, -1).trim();
            if (!inner) return [];
            return splitTomlArrayItems(inner).map(parseTomlValue);
        }
        if (/^(true|false)$/i.test(value)) {
            return value.toLowerCase() === 'true';
        }
        if (/^[+-]?\d+(?:\.\d+)?$/.test(value)) {
            return Number(value);
        }
        return value;
    }

    function parseSimpleTomlObject(text, allowedSections = []) {
        const result = {};
        let currentSection = '';
        for (const rawLine of String(text || '').split(/\r?\n/)) {
            const line = stripTomlComment(rawLine);
            if (!line) continue;
            const sectionMatch = line.match(/^\[([^\]]+)\]$/);
            if (sectionMatch) {
                currentSection = sectionMatch[1].trim();
                continue;
            }
            const kvMatch = line.match(/^([A-Za-z0-9_.-]+)\s*=\s*(.+)$/);
            if (!kvMatch) continue;
            if (currentSection && !allowedSections.includes(currentSection)) continue;
            const key = kvMatch[1].trim();
            const value = parseTomlValue(kvMatch[2]);
            const target = currentSection
                ? (result[currentSection] = result[currentSection] || {})
                : result;
            target[key] = value;
        }
        return result;
    }

    function parseViewsToml(text) {
        const views = [];
        let current = null;
        for (const rawLine of String(text || '').split(/\r?\n/)) {
            const line = stripTomlComment(rawLine);
            if (!line) continue;
            if (line === '[[views]]') {
                current = {};
                views.push(current);
                continue;
            }
            if (!current) continue;
            const kvMatch = line.match(/^([A-Za-z0-9_.-]+)\s*=\s*(.+)$/);
            if (!kvMatch) continue;
            current[kvMatch[1].trim()] = parseTomlValue(kvMatch[2]);
        }
        return normalizeVisualizationViews(views.map((view) => {
            const next = { ...view };
            if (Object.prototype.hasOwnProperty.call(next, 'script_path')) {
                next.scriptPath = next.script_path;
                delete next.script_path;
            }
            return next;
        }));
    }

    function tomlString(value) {
        return JSON.stringify(String(value ?? ''));
    }

    function tomlNumberLine(key, value) {
        return Number.isFinite(value) ? `${key} = ${value}` : null;
    }

    function tomlStringArray(values) {
        return `[${values.map((value) => tomlString(value)).join(', ')}]`;
    }

    function classNameFromQualifiedName(model) {
        return String(model || '').split('.').filter(Boolean).pop() || 'Model';
    }

    function renderProjectToml(existingText, defaults, projectName) {
        const current = parseSimpleTomlObject(existingText, ['project', 'simulation.defaults']);
        const nextName = trimMaybeString(projectName) || trimMaybeString(current.project?.name);
        const nextDefaults = {
            solver: trimMaybeString(defaults?.solver) || trimMaybeString(current['simulation.defaults']?.solver),
            tEnd: Number.isFinite(defaults?.tEnd) ? defaults.tEnd : current['simulation.defaults']?.t_end,
            dt: Number.isFinite(defaults?.dt) ? defaults.dt : current['simulation.defaults']?.dt,
            outputDir: trimMaybeString(defaults?.outputDir) || trimMaybeString(current['simulation.defaults']?.output_dir),
        };

        const lines = ['version = 1'];
        if (nextName) {
            lines.push('', '[project]', `name = ${tomlString(nextName)}`);
        }
        const hasDefaults = nextDefaults.solver
            || Number.isFinite(nextDefaults.tEnd)
            || Number.isFinite(nextDefaults.dt)
            || nextDefaults.outputDir;
        if (hasDefaults) {
            lines.push('', '[simulation.defaults]');
            if (nextDefaults.solver) lines.push(`solver = ${tomlString(nextDefaults.solver)}`);
            const tEndLine = tomlNumberLine('t_end', nextDefaults.tEnd);
            if (tEndLine) lines.push(tEndLine);
            const dtLine = tomlNumberLine('dt', nextDefaults.dt);
            if (dtLine) lines.push(dtLine);
            if (nextDefaults.outputDir) lines.push(`output_dir = ${tomlString(nextDefaults.outputDir)}`);
        }
        return `${lines.join('\n')}\n`;
    }

    function renderIdentityToml(model, uuid, nowMs) {
        return [
            'version = 1',
            `uuid = ${tomlString(uuid)}`,
            `qualified_name = ${tomlString(model)}`,
            `class_name = ${tomlString(classNameFromQualifiedName(model))}`,
            'aliases = []',
            'previous_source_files = []',
            `last_seen_unix_ms = ${Math.max(0, Number(nowMs) || 0)}`,
            '',
        ].join('\n');
    }

    function normalizeProjectSimulationPreset(preset) {
        if (!preset || typeof preset !== 'object') {
            return null;
        }
        const solver = trimMaybeString(preset.solver).toLowerCase();
        const normalizedSolver = solver === 'auto' || solver === 'bdf' || solver === 'rk-like'
            ? solver
            : '';
        const tEnd = Number.isFinite(preset.tEnd) && preset.tEnd > 0 ? Number(preset.tEnd) : null;
        const dt = Number.isFinite(preset.dt) && preset.dt > 0 ? Number(preset.dt) : null;
        const outputDir = trimMaybeString(preset.outputDir) || null;
        const libraryOverrides = normalizeStringArray(preset.libraryOverrides);
        if (!normalizedSolver && tEnd === null && dt === null && !outputDir && libraryOverrides.length === 0) {
            return null;
        }
        return {
            ...(normalizedSolver ? { solver: normalizedSolver } : {}),
            ...(tEnd !== null ? { tEnd } : {}),
            ...(dt !== null ? { dt } : {}),
            ...(outputDir ? { outputDir } : {}),
            ...(libraryOverrides.length > 0 ? { libraryOverrides } : {}),
        };
    }

    function normalizeProjectSimulationSettings(value, fallback) {
        const normalizedFallback = fallback && typeof fallback === 'object' ? fallback : {};
        const solver = trimMaybeString(value?.solver).toLowerCase()
            || trimMaybeString(normalizedFallback.solver).toLowerCase()
            || 'auto';
        const normalizedSolver = solver === 'bdf' || solver === 'rk-like' ? solver : 'auto';
        const tEnd = Number.isFinite(value?.tEnd)
            ? Number(value.tEnd)
            : Number.isFinite(normalizedFallback.tEnd)
                ? Number(normalizedFallback.tEnd)
                : 10.0;
        const dt = Number.isFinite(value?.dt)
            ? Number(value.dt)
            : Number.isFinite(normalizedFallback.dt)
                ? Number(normalizedFallback.dt)
                : null;
        const outputDir = trimMaybeString(value?.outputDir) || trimMaybeString(normalizedFallback.outputDir) || '';
        const modelicaPath = normalizeStringArray(value?.modelicaPath).length > 0
            ? normalizeStringArray(value?.modelicaPath)
            : normalizeStringArray(normalizedFallback.modelicaPath);
        return { solver: normalizedSolver, tEnd, dt, outputDir, modelicaPath };
    }

    function simulationSettingsJson(settings) {
        return {
            solver: settings.solver,
            tEnd: settings.tEnd,
            dt: settings.dt,
            outputDir: settings.outputDir,
            modelicaPath: [...settings.modelicaPath],
        };
    }

    function renderSimulationToml(preset) {
        const lines = [];
        const solver = trimMaybeString(preset?.solver);
        const outputDir = trimMaybeString(preset?.outputDir);
        if (solver) lines.push(`solver = ${tomlString(solver)}`);
        const tEndLine = tomlNumberLine('t_end', preset?.tEnd);
        if (tEndLine) lines.push(tEndLine);
        const dtLine = tomlNumberLine('dt', preset?.dt);
        if (dtLine) lines.push(dtLine);
        if (outputDir) lines.push(`output_dir = ${tomlString(outputDir)}`);
        const libraryOverrides = normalizeStringArray(preset?.libraryOverrides);
        if (libraryOverrides.length > 0) {
            lines.push(`library_overrides = ${tomlStringArray(libraryOverrides)}`);
        }
        return lines.length > 0 ? `${lines.join('\n')}\n` : '';
    }

    function renderViewsToml(views) {
        const lines = [];
        for (const view of normalizeVisualizationViews(views)) {
            lines.push('[[views]]');
            if (trimMaybeString(view.id)) lines.push(`id = ${tomlString(view.id)}`);
            if (trimMaybeString(view.title)) lines.push(`title = ${tomlString(view.title)}`);
            if (trimMaybeString(view.type)) lines.push(`type = ${tomlString(view.type)}`);
            if (trimMaybeString(view.x)) lines.push(`x = ${tomlString(view.x)}`);
            const y = normalizeStringArray(view.y);
            if (y.length > 0) lines.push(`y = ${tomlStringArray(y)}`);
            if (trimMaybeString(view.script)) lines.push(`script = ${tomlString(view.script)}`);
            if (trimMaybeString(view.scriptPath)) lines.push(`script_path = ${tomlString(view.scriptPath)}`);
            lines.push('');
        }
        return lines.join('\n');
    }

    function extractUuidFromPath(path, fileName) {
        const marker = `${MODEL_BY_ID_ROOT}/`;
        const prefix = String(path || '');
        const idx = prefix.indexOf(marker);
        if (idx < 0 || !prefix.endsWith(`/${fileName}`)) {
            return null;
        }
        const remainder = prefix.slice(idx + marker.length, prefix.length - fileName.length - 1);
        return remainder.split('/').filter(Boolean)[0] || null;
    }

    function hostedProjectIdentityPath(uuid) {
        return `${MODEL_BY_ID_ROOT}/${uuid}/${MODEL_IDENTITY_FILE}`;
    }

    function hostedProjectSimulationPath(uuid) {
        return `${MODEL_BY_ID_ROOT}/${uuid}/${MODEL_SIMULATION_FILE}`;
    }

    function hostedProjectViewsPath(uuid) {
        return `${MODEL_BY_ID_ROOT}/${uuid}/${MODEL_VIEWS_FILE}`;
    }

    function hostedProjectSnapshotState(snapshot) {
        const files = Array.isArray(snapshot?.files)
            ? snapshot.files.filter((file) => trimMaybeString(file?.path).startsWith('.rumoca/'))
            : [];
        const rootToml = files.find((file) => trimMaybeString(file.path) === PROJECT_CONFIG_PATH)?.content || '';
        const root = parseSimpleTomlObject(rootToml, ['project', 'simulation.defaults']);
        const defaults = normalizeProjectSimulationPreset({
            solver: root['simulation.defaults']?.solver,
            tEnd: root['simulation.defaults']?.t_end,
            dt: root['simulation.defaults']?.dt,
            outputDir: root['simulation.defaults']?.output_dir,
        }) || {};
        const identitiesByUuid = new Map();
        const identitiesByModel = new Map();
        const simulationModels = new Map();
        const visualizationModels = new Map();

        for (const file of files) {
            if (!trimMaybeString(file.path).endsWith(`/${MODEL_IDENTITY_FILE}`)) {
                continue;
            }
            const uuid = extractUuidFromPath(file.path, MODEL_IDENTITY_FILE);
            if (!uuid) {
                continue;
            }
            const identity = parseSimpleTomlObject(file.content);
            const qualifiedName = trimMaybeString(identity.qualified_name);
            if (!qualifiedName) {
                continue;
            }
            const record = {
                uuid,
                qualifiedName,
                className: trimMaybeString(identity.class_name) || classNameFromQualifiedName(qualifiedName),
            };
            identitiesByUuid.set(uuid, record);
            identitiesByModel.set(qualifiedName, record);
        }

        for (const file of files) {
            if (trimMaybeString(file.path).endsWith(`/${MODEL_SIMULATION_FILE}`)) {
                const uuid = extractUuidFromPath(file.path, MODEL_SIMULATION_FILE);
                const identity = uuid ? identitiesByUuid.get(uuid) : null;
                if (!identity) {
                    continue;
                }
                const parsed = parseSimpleTomlObject(file.content);
                const preset = normalizeProjectSimulationPreset({
                    solver: parsed.solver,
                    tEnd: parsed.t_end,
                    dt: parsed.dt,
                    outputDir: parsed.output_dir,
                    libraryOverrides: parsed.library_overrides,
                });
                if (preset) {
                    simulationModels.set(identity.qualifiedName, preset);
                }
                continue;
            }
            if (trimMaybeString(file.path).endsWith(`/${MODEL_VIEWS_FILE}`)) {
                const uuid = extractUuidFromPath(file.path, MODEL_VIEWS_FILE);
                const identity = uuid ? identitiesByUuid.get(uuid) : null;
                if (!identity) {
                    continue;
                }
                const views = parseViewsToml(file.content);
                if (views.length > 0) {
                    visualizationModels.set(identity.qualifiedName, views);
                }
            }
        }

        return {
            rootToml,
            files,
            projectName: trimMaybeString(root.project?.name) || null,
            defaults,
            identitiesByModel,
            simulationModels,
            visualizationModels,
            editorState: cloneJson(snapshot?.editorState || {}),
        };
    }

    function pruneHostedProjectRemoves(files, liveUuids, removes) {
        for (const file of files) {
            const path = trimMaybeString(file?.path);
            if (!path.startsWith(`${MODEL_BY_ID_ROOT}/`)) {
                continue;
            }
            const parts = path.slice(MODEL_BY_ID_ROOT.length + 1).split('/');
            const uuid = parts[0];
            if (uuid && !liveUuids.has(uuid) && !removes.includes(path)) {
                removes.push(path);
            }
        }
    }

    function buildHostedProjectSimulationConfig(state, model, fallback) {
        const defaults = normalizeProjectSimulationSettings({
            solver: state.defaults.solver,
            tEnd: state.defaults.tEnd,
            dt: state.defaults.dt,
            outputDir: state.defaults.outputDir,
            modelicaPath: [],
        }, fallback);
        const preset = normalizeProjectSimulationPreset(state.simulationModels.get(model));
        const effective = normalizeProjectSimulationSettings({
            ...defaults,
            ...(preset || {}),
            modelicaPath: normalizeStringArray(preset?.libraryOverrides).length > 0
                ? preset.libraryOverrides
                : defaults.modelicaPath,
        }, defaults);
        return {
            preset: preset
                ? {
                    solver: effective.solver,
                    tEnd: effective.tEnd,
                    dt: effective.dt,
                    outputDir: effective.outputDir,
                    libraryOverrides: normalizeStringArray(preset.libraryOverrides).length > 0
                        ? [...preset.libraryOverrides]
                        : [...defaults.modelicaPath],
                }
                : null,
            defaults: simulationSettingsJson(defaults),
            effective: simulationSettingsJson(effective),
            diagnostics: [],
        };
    }

    function buildHostedProjectResult(result, patch) {
        return {
            result,
            writes: patch?.writes || [],
            removes: patch?.removes || [],
            ...(patch && Object.prototype.hasOwnProperty.call(patch, 'editorState')
                ? { editorState: patch.editorState }
                : {}),
        };
    }

    function executeHostedProjectSidecarCommand(command, snapshot, payload) {
        const state = hostedProjectSnapshotState(snapshot);
        const patch = { writes: [], removes: [] };
        const model = trimMaybeString(payload?.model);

        switch (command) {
            case 'rumoca.project.getSimulationConfig':
                return buildHostedProjectResult(
                    buildHostedProjectSimulationConfig(state, model, payload?.fallback),
                    patch,
                );
            case 'rumoca.project.getVisualizationConfig':
                return buildHostedProjectResult(
                    { views: cloneJson(state.visualizationModels.get(model) || []) },
                    patch,
                );
            case 'rumoca.project.setSelectedSimulationModel': {
                const nextEditorState = cloneJson(state.editorState || {}) || {};
                const selectedModel = trimMaybeString(payload?.model);
                if (selectedModel) {
                    nextEditorState.selectedSimulationModel = selectedModel;
                } else {
                    delete nextEditorState.selectedSimulationModel;
                }
                patch.editorState = nextEditorState;
                return buildHostedProjectResult({ ok: true, selectedModel: selectedModel || null }, patch);
            }
            case 'rumoca.project.resetSimulationPreset':
                return executeHostedProjectSidecarCommand('rumoca.project.setSimulationPreset', snapshot, {
                    model,
                    preset: null,
                });
            case 'rumoca.project.setSimulationPreset': {
                const preset = normalizeProjectSimulationPreset(payload?.preset);
                const identity = state.identitiesByModel.get(model) || {
                    uuid: stableModelUuid(model),
                    qualifiedName: model,
                };
                const liveUuids = new Set(
                    Array.from(state.identitiesByModel.values()).map((entry) => entry.uuid),
                );
                if (preset || state.visualizationModels.has(model)) {
                    liveUuids.add(identity.uuid);
                    patch.writes.push({
                        path: hostedProjectIdentityPath(identity.uuid),
                        content: renderIdentityToml(model, identity.uuid, Date.now()),
                    });
                } else {
                    patch.removes.push(hostedProjectIdentityPath(identity.uuid));
                    liveUuids.delete(identity.uuid);
                }
                if (!state.rootToml) {
                    patch.writes.push({
                        path: PROJECT_CONFIG_PATH,
                        content: renderProjectToml('', state.defaults, state.projectName),
                    });
                }
                if (preset) {
                    patch.writes.push({
                        path: hostedProjectSimulationPath(identity.uuid),
                        content: renderSimulationToml(preset),
                    });
                } else {
                    patch.removes.push(hostedProjectSimulationPath(identity.uuid));
                }
                pruneHostedProjectRemoves(state.files, liveUuids, patch.removes);
                return buildHostedProjectResult({ ok: true }, patch);
            }
            case 'rumoca.project.setVisualizationConfig': {
                const views = normalizeVisualizationViews(payload?.views);
                const identity = state.identitiesByModel.get(model) || {
                    uuid: stableModelUuid(model),
                    qualifiedName: model,
                };
                const liveUuids = new Set(
                    Array.from(state.identitiesByModel.values()).map((entry) => entry.uuid),
                );
                if (views.length > 0 || state.simulationModels.has(model)) {
                    liveUuids.add(identity.uuid);
                    patch.writes.push({
                        path: hostedProjectIdentityPath(identity.uuid),
                        content: renderIdentityToml(model, identity.uuid, Date.now()),
                    });
                } else {
                    patch.removes.push(hostedProjectIdentityPath(identity.uuid));
                    liveUuids.delete(identity.uuid);
                }
                if (!state.rootToml) {
                    patch.writes.push({
                        path: PROJECT_CONFIG_PATH,
                        content: renderProjectToml('', state.defaults, state.projectName),
                    });
                }
                if (views.length > 0) {
                    patch.writes.push({
                        path: hostedProjectViewsPath(identity.uuid),
                        content: renderViewsToml(views),
                    });
                } else {
                    patch.removes.push(hostedProjectViewsPath(identity.uuid));
                }
                pruneHostedProjectRemoves(state.files, liveUuids, patch.removes);
                return buildHostedProjectResult({ ok: true }, patch);
            }
            default:
                return buildHostedProjectResult(null, patch);
        }
    }

    return {
        buildHostedResultsPanelState,
        buildHostedResultsPanelTitle,
        buildVisualizationModel,
        buildSimulationRunDocument,
        defaultThreeDimensionalViewerScript,
        defaultVisualizationViews,
        buildLastSimulationResultDocument,
        buildHostedResultsDocument,
        buildHostedSimulationSettingsHandlers,
        buildHostedSimulationSettingsDocument,
        buildHostedSimulationSettingsState,
        buildHostedSimulationSettingsResetValue,
        buildVisualizationViewStorageHandlers,
        handleHostedResultsRequest,
        handleHostedSimulationSettingsRequest,
        lastSimulationResultPath,
        loadHostedSimulationRun,
        loadHostedSimulationRunWithViews,
        loadHostedProjectResultsViews,
        modelScopedViewerScriptRelativePath,
        nextSimulationRunLocation,
        normalizeHostedSimulationSettingsCurrent,
        normalizeHostedPngExportRequest,
        normalizeHostedResultsPanelState,
        normalizeHostedResultsModelRef,
        normalizeHostedResultsNotifyPayload,
        normalizeHostedSimulationSettingsSavePayload,
        normalizeHostedSimulationSettingsState,
        normalizeVisualizationViews,
        normalizePersistedSimulationRun,
        normalizeSimulationPayload,
        normalizeSimulationRunMetrics,
        normalizeHostedWebmExportRequest,
        persistHostedSimulationRun,
        persistHostedSimulationRunWithViews,
        preferredViewerScriptPathForModel,
        resetHostedProjectResultsViews,
        resetHostedProjectSimulationSettings,
        executeHostedProjectSidecarCommand,
        hydrateVisualizationViewsForModel,
        persistVisualizationViewsForModel,
        readLastSimulationResultDocument,
        readPersistedSimulationRunDocument,
        removeStaleVisualizationScriptFiles,
        removeVisualizationScriptFilesForViews,
        sanitizeResultsPathSegment,
        saveHostedProjectResultsViews,
        saveHostedProjectSimulationSettings,
        simulationRunDocumentPath,
        writeLastSimulationResultDocument,
        writePersistedSimulationRunDocument,
    };
}));
