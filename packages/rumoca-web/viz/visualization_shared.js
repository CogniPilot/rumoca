    function trimMaybeString(value) {
        return typeof value === 'string' ? value.trim() : '';
    }

    function normalizeStringArray(values) {
        if (!Array.isArray(values)) {
            return [];
        }
        return values.map(trimMaybeString).filter(Boolean);
    }

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

    function sanitizeResultsPathSegment(input) {
        const cleaned = String(input || '')
            .trim()
            .replace(/[^a-zA-Z0-9._-]+/g, '_')
            .replace(/^_+|_+$/g, '');
        return cleaned.length > 0 ? cleaned : 'view';
    }

    function stableResultStem(model) {
        return sanitizeResultIdentifier(model);
    }

    function resultTimestamp(date = new Date()) {
        return date.toISOString().replace(/[-:]/g, '').replace(/\.\d{3}Z$/, 'Z');
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

    function modelScopedViewerScriptRelativePath(_model, viewId) {
        return `${sanitizeResultsPathSegment(viewId)}.js`;
    }

    function preferredViewerScriptPathForModel(model, viewId) {
        return modelScopedViewerScriptRelativePath(model, viewId);
    }

    function normalizeResultDirectory(raw) {
        return String(raw || '')
            .replace(/\\/g, '/')
            .replace(/^\/+/, '')
            .replace(/\/+/g, '/')
            .trim()
            .split('/')
            .filter((part) => part && part !== '.')
            .join('/');
    }

    function nextSimulationRunLocation(model, pathExists, resultDirectory = '') {
        const date = new Date();
        const timestamp = resultTimestamp(date);
        const slug = stableResultStem(model);
        let runId = `${timestamp}.${slug}`;
        let runPath = simulationRunDocumentPath(runId, resultDirectory);
        for (let suffix = 1; typeof pathExists === 'function' && pathExists(runPath); suffix += 1) {
            runId = `${timestamp}.${slug}.${suffix}`;
            runPath = simulationRunDocumentPath(runId, resultDirectory);
        }
        return { runId, runPath, savedAtUnixMs: date.getTime() };
    }

    function defaultThreeDimensionalViewerScript() {
        return `// Default Rumoca 3D preset.
// Geometry is defined here so each model can fully customize visuals.
ctx.onInit = (api) => {
  if (typeof api.enableDefaultViewerRuntime === "function") {
    api.enableDefaultViewerRuntime({ selectedObjectName: "ball", followSelected: true });
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
    const height = Number(api.getValue("x", api.sampleIndex));
    ball.position.set(0, Number.isFinite(height) ? height : 0, 0);
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
            const scriptPath = trimMaybeString(entry.scriptPath)
                || trimMaybeString(entry.script_path)
                || undefined;
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

    function simulationRunDocumentPath(runId, resultDirectory = '') {
        const normalized = normalizeRunId(runId);
        if (!normalized) {
            return undefined;
        }
        const directory = normalizeResultDirectory(resultDirectory);
        const fileName = `rumoca-result.${normalized}.json`;
        return directory ? `${directory}/${fileName}` : fileName;
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
            runPath: trimMaybeString(candidate.runPath) || undefined,
            title: trimMaybeString(candidate.title) || undefined,
        };
    }

    function normalizeHostedResultsPanelState(raw, fallbackWorkspaceRoot) {
        const modelRef = normalizeHostedResultsModelRef(raw, fallbackWorkspaceRoot);
        if (!modelRef || (!modelRef.runId && !modelRef.runPath)) {
            return undefined;
        }
        const state = {
            version: 1,
            runId: modelRef.runId,
            model: modelRef.model,
            workspaceRoot: modelRef.workspaceRoot,
            title: modelRef.title,
            activeViewId: trimMaybeString(raw && raw.activeViewId) || undefined,
        };
        if (modelRef.runPath) {
            state.runPath = modelRef.runPath;
        }
        return state;
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
        const location = nextSimulationRunLocation(args.model, args.pathExists, args.resultDirectory);
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
        const runPath = simulationRunDocumentPath(args && args.runId, args && args.resultDirectory);
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
                resultDirectory: args.resultDirectory,
                pathExists: args.pathExists,
                writeTextFile: args.writeTextFile,
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
            readTextFile: args?.readTextFile,
            writeTextFile: args?.writeTextFile,
            resultDirectory: args?.resultDirectory,
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
                resultDirectory: args.resultDirectory,
                readTextFile: args.readTextFile,
            });
        }
        return undefined;
    }

    async function loadHostedSimulationRunWithViews(args) {
        const model = trimMaybeString(args && args.model);
        if (!model || typeof args?.loadConfiguredViews !== 'function') {
            return undefined;
        }
        const run = await loadHostedSimulationRun({
            model,
            runId: args?.runId,
            resultDirectory: args?.resultDirectory,
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
        const resultsAppModuleJson = escapeInlineScriptJson(JSON.stringify(
            trimMaybeString(assets.resultsAppJs) || './results_app.js',
        ));
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
  <script type="module">
    const { createResultsApp } = await import(${resultsAppModuleJson});
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

    const app = createResultsApp({
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


    function isRumocaScenarioPath(path) {
        const filePath = trimMaybeString(path);
        if (!filePath) return false;
        const fileName = filePath.split('/').pop() || '';
        return fileName === 'rumoca-scenario.toml'
            || (fileName.startsWith('rumoca-scenario.') && fileName.endsWith('.toml'));
    }

    // --- Scenario config form model (scenario TOML dual-view GUI) ------------
    // Pure transforms between a scenario-as-JSON config tree (from
    // scenario_get_scenario_config_full) and a flat list of editable leaf fields,
    // shared by the config form in both editors. Scalars (string/number/boolean)
    // are editable leaves; nested tables recurse so the form can group by
    // top-level section; arrays and other non-scalars are edited as raw JSON
    // leaves in v1 and refined into richer widgets later.

    function scenarioConfigLeafKind(value) {
        if (typeof value === 'boolean') return 'boolean';
        if (typeof value === 'number') return 'number';
        if (typeof value === 'string') return 'string';
        return 'json';
    }

    function flattenScenarioConfig(tree, basePath = []) {
        const fields = [];
        if (!tree || typeof tree !== 'object' || Array.isArray(tree)) {
            return fields;
        }
        for (const key of Object.keys(tree)) {
            const value = tree[key];
            const path = [...basePath, key];
            if (value && typeof value === 'object' && !Array.isArray(value)) {
                fields.push(...flattenScenarioConfig(value, path));
            } else {
                fields.push({
                    path,
                    section: path[0],
                    key,
                    label: path.join('.'),
                    kind: scenarioConfigLeafKind(value),
                    value,
                });
            }
        }
        return fields;
    }

    function setScenarioConfigValue(tree, path, value) {
        const next = tree && typeof tree === 'object' && !Array.isArray(tree)
            ? { ...tree }
            : {};
        if (!Array.isArray(path) || path.length === 0) {
            return next;
        }
        const [head, ...rest] = path;
        if (rest.length === 0) {
            if (value === undefined) {
                delete next[head];
            } else {
                next[head] = value;
            }
        } else {
            next[head] = setScenarioConfigValue(next[head], rest, value);
        }
        return next;
    }

    function applyScenarioConfigEdits(tree, edits) {
        let next = tree && typeof tree === 'object' && !Array.isArray(tree) ? tree : {};
        for (const edit of Array.isArray(edits) ? edits : []) {
            if (edit && Array.isArray(edit.path)) {
                next = setScenarioConfigValue(next, edit.path, edit.value);
            }
        }
        return next;
    }

    const SCENARIO_CONFIG_SECTION_ORDER = [
        'rumoca',
        'model',
        'sim',
        'parameters',
        'codegen',
        'viewer',
        'transport',
        'locals',
        'input',
        'signals',
        'derive',
        'reset',
    ];

    function scenarioConfigSectionRank(section) {
        const index = SCENARIO_CONFIG_SECTION_ORDER.indexOf(section);
        return index === -1 ? SCENARIO_CONFIG_SECTION_ORDER.length : index;
    }

    const SCENARIO_SOLVER_OPTIONS = [
        ['auto', 'Auto'],
        ['rk-like', 'RK-like (explicit)'],
        ['bdf', 'BDF (stiff systems)'],
    ];
    const SCENARIO_SIM_MODE_OPTIONS = [
        ['', 'Default'],
        ['as_fast_as_possible', 'As fast as possible'],
        ['realtime', 'Realtime'],
        ['lockstep', 'Lockstep'],
    ];
    const SCENARIO_TASK_OPTIONS = [
        ['simulate', 'Simulation'],
        ['codegen', 'Code generation'],
    ];
    const SCENARIO_VIEWER_MODE_OPTIONS = [
        ['results_panel', 'Results panel'],
        ['external_web', 'External browser'],
    ];
    const SCENARIO_PLOT_VIEW_TYPE_OPTIONS = [
        ['timeseries', 'timeseries'],
        ['scatter', 'scatter'],
        ['3d', '3d'],
    ];
    const SCENARIO_CODEGEN_TARGET_OPTIONS = [
        ['sympy', 'sympy'],
        ['jax', 'jax'],
        ['casadi-sx', 'casadi-sx'],
        ['casadi-mx', 'casadi-mx'],
        ['onnx', 'onnx'],
        ['fmi2', 'fmi2'],
        ['fmi3', 'fmi3'],
    ];
    const SCENARIO_INPUT_MODE_OPTIONS = [
        ['auto', 'auto'],
        ['keyboard', 'keyboard'],
        ['gamepad', 'gamepad'],
    ];
    const SCENARIO_KEY_ACTION_OPTIONS = [
        ['set', 'set local value'],
        ['toggle', 'toggle local flag'],
        ['signal', 'emit signal'],
    ];
    const SCENARIO_GAMEPAD_AXIS_OPTIONS = [
        ['LeftStickX', 'LeftStickX'],
        ['LeftStickY', 'LeftStickY'],
        ['RightStickX', 'RightStickX'],
        ['RightStickY', 'RightStickY'],
        ['LeftZ', 'LeftZ'],
        ['RightZ', 'RightZ'],
        ['DPadX', 'DPadX'],
        ['DPadY', 'DPadY'],
    ];
    const SCENARIO_GAMEPAD_BUTTON_OPTIONS = [
        ['South', 'South'],
        ['East', 'East'],
        ['North', 'North'],
        ['West', 'West'],
        ['Start', 'Start'],
        ['Select', 'Select'],
        ['LeftTrigger', 'LeftTrigger'],
        ['RightTrigger', 'RightTrigger'],
        ['LeftThumb', 'LeftThumb'],
        ['RightThumb', 'RightThumb'],
        ['DPadUp', 'DPadUp'],
        ['DPadDown', 'DPadDown'],
        ['DPadLeft', 'DPadLeft'],
        ['DPadRight', 'DPadRight'],
        ['Mode', 'Mode'],
    ];
    const SCENARIO_LOCAL_TYPE_OPTIONS = [
        ['float', 'float'],
        ['bool', 'bool'],
    ];
    function scenarioPathKey(path) {
        return Array.isArray(path) ? path.join('.') : '';
    }

    function scenarioListText(values) {
        return normalizeStringArray(values).join('\n');
    }

    function scenarioFieldValue(config, path, fallback = '') {
        let value = config;
        for (const segment of path) {
            if (!value || typeof value !== 'object') {
                return fallback;
            }
            value = value[segment];
        }
        return value === undefined || value === null ? fallback : value;
    }

    function scenarioSelectOptions(options, selected) {
        const selectedText = trimMaybeString(selected);
        return options
            .map(([value, label]) => {
                const selectedAttr = value === selectedText ? ' selected' : '';
                return `<option value="${escapeHtml(value)}"${selectedAttr}>${escapeHtml(label)}</option>`;
            })
            .join('');
    }

    function scenarioOptionLabel(options, value) {
        const selectedText = trimMaybeString(value);
        return options.find(([optionValue]) => optionValue === selectedText)?.[1]
            || selectedText
            || 'Default';
    }

    function scenarioConfigControlMarkup(field) {
        const idAttr = `field_${field.index}`;
        const value = field.value === undefined || field.value === null ? '' : field.value;
        const fieldJson = escapeHtml(JSON.stringify(field.path));
        const optionalAttr = field.optional ? ' data-optional="true"' : '';
        const placeholder = field.placeholder ? ` placeholder="${escapeHtml(field.placeholder)}"` : '';
        const common = `id="${idAttr}" data-field="${field.index}" data-kind="${escapeHtml(field.kind)}" data-path="${fieldJson}"${optionalAttr}`;
        if (field.kind === 'select') {
            return `<select ${common}>${scenarioSelectOptions(field.options || [], value)}</select>`;
        }
        if (field.kind === 'boolean') {
            const checked = value ? ' checked' : '';
            return `<input type="checkbox" ${common}${checked}>`;
        }
        if (field.kind === 'number' || field.kind === 'optionalNumber') {
            return `<input type="number" step="any" ${common} value="${escapeHtml(String(value))}"${placeholder}>`;
        }
        if (field.kind === 'stringList') {
            const rows = normalizeStringArray(value)
                .map((item) => sourceRootRowMarkup(item))
                .join('');
            return `
              <div ${common} class="path-list">
                <div class="path-list-rows" data-path-list-rows>${rows}</div>
                <button type="button" class="ghost add-path" data-add-path>+ Add Library Folder</button>
              </div>`;
        }
        if (field.kind === 'json') {
            return `<textarea ${common} rows="3">${escapeHtml(JSON.stringify(value ?? null))}</textarea>`;
        }
        return `<input type="text" ${common} value="${escapeHtml(String(value))}"${placeholder}>`;
    }

    function scenarioConfigFieldMarkup(field) {
        const idAttr = `field_${field.index}`;
        const hint = field.hint ? `<div class="hint">${escapeHtml(field.hint)}</div>` : '';
        const classes = [
            'field',
            field.kind === 'boolean' ? 'inline-field' : '',
            field.kind === 'stringList' ? 'wide-field' : '',
        ].filter(Boolean).join(' ');
        return `
        <div class="${classes}">
          <label for="${idAttr}">${escapeHtml(field.label)}</label>
          ${scenarioConfigControlMarkup(field)}
          ${hint}
        </div>`;
    }

    function typedScenarioConfigFields(config, task) {
        const fields = [
            {
                section: 'rumoca',
                label: 'Workflow',
                path: ['rumoca', 'task'],
                kind: 'select',
                value: scenarioFieldValue(config, ['rumoca', 'task'], task || 'simulate'),
                options: SCENARIO_TASK_OPTIONS,
                hint: 'Choose what the scenario does when you press the primary action.',
            },
            {
                section: 'model',
                label: 'Model file',
                path: ['model', 'file'],
                kind: 'string',
                value: scenarioFieldValue(config, ['model', 'file']),
                hint: 'Path to the Modelica source, relative to this scenario file.',
            },
            {
                section: 'model',
                label: 'Model name',
                path: ['model', 'name'],
                kind: 'string',
                value: scenarioFieldValue(config, ['model', 'name']),
            },
            {
                section: 'sim',
                label: 'Solver',
                path: ['sim', 'solver'],
                kind: 'select',
                value: scenarioFieldValue(config, ['sim', 'solver'], 'auto'),
                options: SCENARIO_SOLVER_OPTIONS,
                hint: 'Auto chooses a solver. RK-like is explicit; BDF is for stiff systems.',
            },
            {
                section: 'sim',
                label: 'Pacing',
                path: ['sim', 'mode'],
                kind: 'select',
                optional: true,
                value: scenarioFieldValue(config, ['sim', 'mode']),
                options: SCENARIO_SIM_MODE_OPTIONS,
                hint: 'Simulation speed policy. Realtime follows wall-clock time; lockstep waits for each step.',
            },
            {
                section: 'sim',
                label: 'End time',
                path: ['sim', 't_end'],
                kind: 'number',
                value: scenarioFieldValue(config, ['sim', 't_end'], 10),
                hint: 'Simulation stop time. Must be positive.',
            },
            {
                section: 'sim',
                label: 'Step size',
                path: ['sim', 'dt'],
                kind: 'optionalNumber',
                optional: true,
                value: scenarioFieldValue(config, ['sim', 'dt']),
                hint: 'Optional fixed step. Leave blank for automatic stepping.',
            },
            {
                section: 'sim',
                label: 'Output directory',
                path: ['sim', 'output_dir'],
                kind: 'string',
                optional: true,
                value: scenarioFieldValue(config, ['sim', 'output_dir']),
                hint: 'Optional directory for generated run artifacts.',
            },
            {
                section: 'viewer',
                label: 'Results display',
                path: ['viewer', 'mode'],
                kind: 'select',
                value: scenarioFieldValue(config, ['viewer', 'mode'], 'results_panel'),
                options: SCENARIO_VIEWER_MODE_OPTIONS,
                hint: 'Presentation only: show results inside the editor or in an external browser.',
            },
            {
                section: 'codegen',
                label: 'Target',
                path: ['codegen', 'target'],
                kind: 'select',
                value: scenarioFieldValue(config, ['codegen', 'target'], 'sympy'),
                options: SCENARIO_CODEGEN_TARGET_OPTIONS,
                hint: 'Built-in renderer used when task is codegen.',
            },
            {
                section: 'codegen',
                label: 'Output directory',
                path: ['codegen', 'output_dir'],
                kind: 'string',
                optional: true,
                value: scenarioFieldValue(config, ['codegen', 'output_dir']),
                hint: 'Directory where generated files are materialized.',
            },
            {
                section: 'source_roots',
                label: 'Scenario library folders',
                path: ['source_roots'],
                kind: 'stringList',
                optional: true,
                value: scenarioFieldValue(config, ['source_roots'], []),
                hint: 'Extra Modelica library folders for this scenario. Workspace libraries stay in rumoca-workspace.toml.',
            },
        ];
        return fields;
    }

    function sourceRootRowMarkup(value = '') {
        return `
          <div class="path-row" data-path-row>
            <input type="text" data-list-item value="${escapeHtml(value)}" placeholder="../libraries/Modelica">
            <button type="button" class="ghost icon-button" data-browse-path title="Browse for a folder">...</button>
            <button type="button" class="ghost icon-button" data-remove-path title="Remove library folder">-</button>
          </div>`;
    }

    function scenarioPlotViews(config) {
        const plot = config && typeof config === 'object' ? config.plot : null;
        return normalizeVisualizationViews(plot && Array.isArray(plot.views) ? plot.views : []);
    }

    function scenarioParameterOverrides(config) {
        const parameters = config && typeof config === 'object' ? config.parameters : null;
        return parameters && typeof parameters === 'object' && !Array.isArray(parameters)
            ? parameters
            : {};
    }

    function scenarioParameterMetadata(metadata, overrides) {
        const overrideMap = overrides && typeof overrides === 'object' && !Array.isArray(overrides)
            ? overrides
            : {};
        const rows = Array.isArray(metadata)
            ? metadata
                .filter((entry) => entry && typeof entry === 'object' && trimMaybeString(entry.name))
                .map((entry) => {
                    const name = trimMaybeString(entry.name);
                    const hasOverride = Object.prototype.hasOwnProperty.call(overrideMap, name);
                    return {
                        name,
                        defaultValue: entry.defaultValue,
                        unit: trimMaybeString(entry.unit),
                        start: trimMaybeString(entry.start),
                        min: trimMaybeString(entry.min),
                        max: trimMaybeString(entry.max),
                        nominal: trimMaybeString(entry.nominal),
                        minValue: entry.minValue === null || entry.minValue === undefined
                            ? NaN
                            : Number(entry.minValue),
                        maxValue: entry.maxValue === null || entry.maxValue === undefined
                            ? NaN
                            : Number(entry.maxValue),
                        fixed: entry.fixed,
                        description: trimMaybeString(entry.description),
                        hasOverride,
                        overrideValue: hasOverride ? overrideMap[name] : '',
                    };
                })
            : [];
        const known = new Set(rows.map((entry) => entry.name));
        for (const [name, value] of Object.entries(overrideMap).sort(([a], [b]) => a.localeCompare(b))) {
            if (known.has(name)) continue;
            rows.push({
                name,
                defaultValue: '',
                unit: '',
                start: '',
                min: '',
                max: '',
                nominal: '',
                minValue: NaN,
                maxValue: NaN,
                fixed: undefined,
                description: 'Override preserved from TOML; metadata was not available for this parameter.',
                hasOverride: true,
                overrideValue: value,
            });
        }
        rows.sort((a, b) => a.name.localeCompare(b.name));
        return rows;
    }

    function sortedObjectEntries(value) {
        if (!value || typeof value !== 'object' || Array.isArray(value)) {
            return [];
        }
        return Object.keys(value).sort().map((key) => [key, value[key]]);
    }

    function scenarioInputMappings(config) {
        const input = config && typeof config === 'object' && config.input && typeof config.input === 'object'
            ? config.input
            : {};
        const keyboard = input.keyboard && typeof input.keyboard === 'object' ? input.keyboard : {};
        const gamepad = input.gamepad && typeof input.gamepad === 'object' ? input.gamepad : {};
        const signals = config && typeof config === 'object' && config.signals && typeof config.signals === 'object'
            ? config.signals
            : {};
        return {
            mode: trimMaybeString(input.mode),
            enabled: Boolean(trimMaybeString(input.mode)
                || sortedObjectEntries(config && config.locals).length
                || sortedObjectEntries(keyboard.keys).length
                || sortedObjectEntries(keyboard.integrators).length
                || sortedObjectEntries(gamepad.axes).length
                || sortedObjectEntries(gamepad.integrators).length
                || sortedObjectEntries(gamepad.buttons).length
                || sortedObjectEntries(signals.stepper_inputs).length),
            locals: sortedObjectEntries(config && config.locals).map(([name, local]) => ({
                name,
                type: trimMaybeString(local && local.type) || 'float',
                defaultValue: local && local.default !== undefined ? String(local.default) : '',
            })),
            keyboardKeys: sortedObjectEntries(keyboard.keys).map(([key, binding]) => ({
                key,
                action: trimMaybeString(binding && binding.action) || 'set',
                target: trimMaybeString(binding && binding.target),
                value: binding && binding.value !== undefined ? String(binding.value) : '',
                state: trimMaybeString(binding && binding.state),
                signal: trimMaybeString(binding && binding.signal),
                debounceMs: binding && binding.debounce_ms !== undefined ? String(binding.debounce_ms) : '',
                precondition: trimMaybeString(binding && binding.precondition),
            })),
            keyboardIntegrators: sortedObjectEntries(keyboard.integrators).map(([name, integrator]) => scenarioInputIntegrator(name, integrator)),
            gamepadAxes: sortedObjectEntries(gamepad.axes).map(([name, axis]) => ({
                name,
                source: trimMaybeString(axis && axis.source) || 'LeftStickY',
                write: trimMaybeString(axis && axis.write),
                scale: axis && axis.scale !== undefined ? String(axis.scale) : '',
                invert: Boolean(axis && axis.invert),
            })),
            gamepadIntegrators: sortedObjectEntries(gamepad.integrators).map(([name, integrator]) => scenarioInputIntegrator(name, integrator)),
            gamepadButtons: sortedObjectEntries(gamepad.buttons).map(([name, button]) => ({
                name,
                source: trimMaybeString(button && button.source) || 'South',
                action: trimMaybeString(button && button.action) || 'toggle',
                state: trimMaybeString(button && button.state),
                signal: trimMaybeString(button && button.signal),
                debounceMs: button && button.debounce_ms !== undefined ? String(button.debounce_ms) : '',
                precondition: trimMaybeString(button && button.precondition),
            })),
            stepperInputs: sortedObjectEntries(signals.stepper_inputs).map(([name, source]) => ({
                name,
                source: typeof source === 'string' ? source : JSON.stringify(source ?? ''),
            })),
        };
    }

    function scenarioInputIntegrator(name, integrator) {
        const clamp = Array.isArray(integrator && integrator.clamp) ? integrator.clamp : [];
        return {
            name,
            source: trimMaybeString(integrator && integrator.source),
            write: trimMaybeString(integrator && integrator.write),
            deadband: integrator && integrator.deadband !== undefined ? String(integrator.deadband) : '',
            rate: integrator && integrator.rate !== undefined ? String(integrator.rate) : '',
            clampMin: clamp[0] !== undefined ? String(clamp[0]) : '',
            clampMax: clamp[1] !== undefined ? String(clamp[1]) : '',
        };
    }

    function scenarioPlotViewMarkup(view, index) {
        const viewType = trimMaybeString(view.type).toLowerCase() || 'timeseries';
        return `
        <div class="view-row" data-view-index="${index}" data-view-id="${escapeHtml(view.id || '')}">
          <div class="view-title-row">
            <strong>${escapeHtml(view.title || view.id || `View ${index + 1}`)}</strong>
            <button type="button" class="ghost remove-view" data-remove-view="${index}">Remove</button>
          </div>
          <div class="grid">
            <div class="field">
              <label for="view_${index}_title">title</label>
              <input id="view_${index}_title" data-view-field="title" value="${escapeHtml(view.title || '')}">
            </div>
            <div class="field">
              <label for="view_${index}_type">type</label>
              <select id="view_${index}_type" data-view-field="type">${scenarioSelectOptions(SCENARIO_PLOT_VIEW_TYPE_OPTIONS, viewType)}</select>
            </div>
            <div class="field" data-view-kind="series"${viewType === '3d' ? ' hidden' : ''}>
              <label for="view_${index}_x">x</label>
              <input id="view_${index}_x" data-view-field="x" value="${escapeHtml(view.x || 'time')}">
            </div>
            <div class="field" data-view-kind="series"${viewType === '3d' ? ' hidden' : ''}>
              <label for="view_${index}_y">y series</label>
              <textarea id="view_${index}_y" data-view-field="y" rows="3" placeholder="*states">${escapeHtml(scenarioListText(view.y || ['*states']))}</textarea>
            </div>
            <div class="field" data-view-kind="script3d"${viewType === '3d' ? '' : ' hidden'}>
              <label for="view_${index}_script_path">viewer script path</label>
              <input id="view_${index}_script_path" data-view-field="scriptPath" value="${escapeHtml(view.scriptPath || '')}" placeholder="viewer.js">
            </div>
          </div>
        </div>`;
    }

    function scenarioPlotSectionMarkup(views) {
        const viewMarkup = views.length > 0
            ? views.map((view, index) => scenarioPlotViewMarkup(view, index)).join('')
            : '<div class="empty">No plot panels configured. Add a panel to save simulation results with a reusable view.</div>';
        return `
      <section class="card" id="plotViewsCard" data-scenario-section="plot">
        <details open>
          <summary>
            <span>Plot Panels</span>
            <span class="summary-meta">${views.length} configured</span>
          </summary>
          <div class="section-header">
            <span class="hint">Reusable result views for this simulation.</span>
            <button type="button" id="addPlotView" class="ghost">Add Panel</button>
          </div>
          <div id="plotViewsList">${viewMarkup}</div>
        </details>
      </section>`;
    }

    function scenarioInputSummary(inputMappings) {
        const parts = [];
        const locals = inputMappings.locals.length;
        const keyboard = inputMappings.keyboardKeys.length + inputMappings.keyboardIntegrators.length;
        const gamepad = inputMappings.gamepadAxes.length + inputMappings.gamepadIntegrators.length + inputMappings.gamepadButtons.length;
        const stepper = inputMappings.stepperInputs.length;
        if (locals) parts.push(`${locals} local${locals === 1 ? '' : 's'}`);
        if (keyboard) parts.push(`${keyboard} keyboard`);
        if (gamepad) parts.push(`${gamepad} gamepad`);
        if (stepper) parts.push(`${stepper} model input${stepper === 1 ? '' : 's'}`);
        return parts.length ? parts.join(' · ') : 'No routes configured';
    }

    function scenarioCountLabel(count, singular, plural = `${singular}s`) {
        return count === 1 ? `1 ${singular}` : `${count} ${plural}`;
    }

    function scenarioMappingHeader(labels, rowClass) {
        return `<div class="mapping-header ${escapeHtml(rowClass || '')}">${labels.map((label) => `<span>${escapeHtml(label)}</span>`).join('')}<span></span></div>`;
    }

    function scenarioMappingRows(rows, emptyText) {
        return rows || `<div class="empty mapping-empty">${escapeHtml(emptyText)}</div>`;
    }

    function scenarioInputSubsectionMarkup(args) {
        const button = args.addButton
            ? `<button type="button" class="ghost" ${args.addButton.attr}>${escapeHtml(args.addButton.label)}</button>`
            : '';
        const actions = button ? `<div class="section-header">${button}</div>` : '';
        const header = args.header ? scenarioMappingHeader(args.header, args.rowClass) : '';
        const hint = args.hint ? `<div class="hint">${escapeHtml(args.hint)}</div>` : '';
        return `
            <details class="mapping-group input-subsection">
              <summary>
                <span>${escapeHtml(args.title)}</span>
                <span class="summary-meta">${escapeHtml(args.summary)}</span>
              </summary>
              ${actions}
              ${header}
              <div ${args.listAttr}>${scenarioMappingRows(args.rows, args.emptyText)}</div>
              ${hint}
            </details>`;
    }

    function scenarioInputSectionMarkup(inputMappings) {
        const mode = trimMaybeString(inputMappings.mode);
        const enabled = Boolean(inputMappings.enabled);
        const summary = scenarioInputSummary(inputMappings);
        const localRows = inputMappings.locals.map((row, index) => scenarioLocalRowMarkup(row, index)).join('');
        const keyboardRows = inputMappings.keyboardKeys.map((row, index) => scenarioKeyboardKeyRowMarkup(row, index)).join('');
        const keyboardIntegratorRows = inputMappings.keyboardIntegrators.map((row, index) => scenarioIntegratorRowMarkup(row, index, 'keyboard')).join('');
        const gamepadAxisRows = inputMappings.gamepadAxes.map((row, index) => scenarioGamepadAxisRowMarkup(row, index)).join('');
        const gamepadIntegratorRows = inputMappings.gamepadIntegrators.map((row, index) => scenarioIntegratorRowMarkup(row, index, 'gamepad')).join('');
        const gamepadButtonRows = inputMappings.gamepadButtons.map((row, index) => scenarioGamepadButtonRowMarkup(row, index)).join('');
        const stepperInputRows = inputMappings.stepperInputs.map((row, index) => scenarioStepperInputRowMarkup(row, index)).join('');
        return `
      <section class="card" id="inputMappingsCard" data-scenario-section="input">
        <details open>
          <summary>
            <span>Input Mapping</span>
            <span class="summary-meta">${escapeHtml(enabled ? summary : 'disabled')}</span>
          </summary>
          <div class="grid">
            <div class="field inline-field">
              <label for="input_enabled">enabled</label>
              <input id="input_enabled" data-input-enabled type="checkbox"${enabled ? ' checked' : ''}>
              <div class="hint">Enable user input routing for keyboard, gamepad, or browser controls.</div>
            </div>
            <div class="field" data-input-enabled-field>
              <label for="input_mode">input mode</label>
              <select id="input_mode" data-input-mode>${scenarioSelectOptions(SCENARIO_INPUT_MODE_OPTIONS, mode || 'auto')}</select>
              <div class="hint">auto tries gamepad first and falls back to keyboard.</div>
            </div>
          </div>
          <details class="advanced-details" data-input-enabled-field>
            <summary>
              <span>Advanced Input Routing</span>
              <span class="summary-meta">${escapeHtml(summary)}</span>
            </summary>
            ${scenarioInputSubsectionMarkup({
                title: 'Locals',
                summary: scenarioCountLabel(inputMappings.locals.length, 'local'),
                addButton: { attr: 'data-add-local', label: '+ Add Local' },
                header: ['Name', 'Type', 'Default'],
                rowClass: 'local-row',
                listAttr: 'data-local-list',
                rows: localRows,
                emptyText: 'No local input state configured.',
            })}
            ${scenarioInputSubsectionMarkup({
                title: 'Keyboard',
                summary: scenarioCountLabel(inputMappings.keyboardKeys.length, 'key', 'keys'),
                addButton: { attr: 'data-add-keyboard-key', label: '+ Add Key' },
                header: ['Key', 'Action', 'Target', 'Value', 'State', 'Signal', 'Debounce', 'Condition'],
                rowClass: 'keyboard-key-row',
                listAttr: 'data-keyboard-key-list',
                rows: keyboardRows,
                emptyText: 'No keyboard bindings configured.',
            })}
            ${scenarioInputSubsectionMarkup({
                title: 'Keyboard Integrators',
                summary: scenarioCountLabel(inputMappings.keyboardIntegrators.length, 'integrator'),
                addButton: { attr: 'data-add-keyboard-integrator', label: '+ Add Integrator' },
                header: ['Name', 'Source', 'Writes', 'Rate', 'Deadband', 'Min', 'Max'],
                rowClass: 'integrator-row',
                listAttr: 'data-keyboard-integrator-list',
                rows: keyboardIntegratorRows,
                emptyText: 'No keyboard integrators configured.',
            })}
            ${scenarioInputSubsectionMarkup({
                title: 'Gamepad Axes',
                summary: scenarioCountLabel(inputMappings.gamepadAxes.length, 'axis', 'axes'),
                addButton: { attr: 'data-add-gamepad-axis', label: '+ Add Axis' },
                header: ['Name', 'Axis', 'Writes', 'Scale', 'Invert'],
                rowClass: 'gamepad-axis-row',
                listAttr: 'data-gamepad-axis-list',
                rows: gamepadAxisRows,
                emptyText: 'No gamepad axes configured.',
            })}
            ${scenarioInputSubsectionMarkup({
                title: 'Gamepad Integrators',
                summary: scenarioCountLabel(inputMappings.gamepadIntegrators.length, 'integrator'),
                addButton: { attr: 'data-add-gamepad-integrator', label: '+ Add Integrator' },
                header: ['Name', 'Axis', 'Writes', 'Rate', 'Deadband', 'Min', 'Max'],
                rowClass: 'integrator-row',
                listAttr: 'data-gamepad-integrator-list',
                rows: gamepadIntegratorRows,
                emptyText: 'No gamepad integrators configured.',
            })}
            ${scenarioInputSubsectionMarkup({
                title: 'Gamepad Buttons',
                summary: scenarioCountLabel(inputMappings.gamepadButtons.length, 'button'),
                addButton: { attr: 'data-add-gamepad-button', label: '+ Add Button' },
                header: ['Name', 'Button', 'Action', 'State', 'Signal', 'Debounce', 'Condition'],
                rowClass: 'gamepad-button-row',
                listAttr: 'data-gamepad-button-list',
                rows: gamepadButtonRows,
                emptyText: 'No gamepad buttons configured.',
            })}
            ${scenarioInputSubsectionMarkup({
                title: 'Model Inputs',
                summary: scenarioCountLabel(inputMappings.stepperInputs.length, 'model input'),
                addButton: { attr: 'data-add-stepper-input', label: '+ Add Model Input' },
                header: ['Model input', 'Source route'],
                rowClass: 'stepper-input-row',
                listAttr: 'data-stepper-input-list',
                rows: stepperInputRows,
                emptyText: 'No Modelica inputs are connected to user input state.',
                hint: 'Routes local/runtime values into Modelica input variables each step, for example throttle = local:throttle.',
            })}
          </details>
        </details>
      </section>`;
    }

    function scenarioIntegratorRowMarkup(row, index, device) {
        const prefix = device === 'gamepad' ? 'gamepad-integrator' : 'keyboard-integrator';
        return `
        <div class="mapping-row integrator-row" data-${prefix}-row="${index}">
          <input data-integrator-field="name" value="${escapeHtml(row.name || '')}" placeholder="throttle">
          <input data-integrator-field="source" value="${escapeHtml(row.source || '')}" placeholder="${device === 'gamepad' ? 'LeftStickY' : 'local:throttle_input'}">
          <input data-integrator-field="write" value="${escapeHtml(row.write || '')}" placeholder="local target">
          <input data-integrator-field="rate" type="number" step="any" value="${escapeHtml(row.rate || '')}" placeholder="0.7">
          <input data-integrator-field="deadband" type="number" step="any" value="${escapeHtml(row.deadband || '')}" placeholder="0.1">
          <input data-integrator-field="clampMin" type="number" step="any" value="${escapeHtml(row.clampMin || '')}" placeholder="0.0">
          <input data-integrator-field="clampMax" type="number" step="any" value="${escapeHtml(row.clampMax || '')}" placeholder="1.0">
          <button type="button" class="ghost icon-button" data-remove-mapping>-</button>
        </div>`;
    }

    function scenarioLocalRowMarkup(row, index) {
        return `
        <div class="mapping-row local-row" data-local-row="${index}">
          <input data-local-field="name" value="${escapeHtml(row.name || '')}" placeholder="throttle">
          <select data-local-field="type">${scenarioSelectOptions(SCENARIO_LOCAL_TYPE_OPTIONS, row.type || 'float')}</select>
          <input data-local-field="defaultValue" value="${escapeHtml(row.defaultValue || '')}" placeholder="0.0">
          <button type="button" class="ghost icon-button" data-remove-mapping>-</button>
        </div>`;
    }

    function scenarioKeyboardKeyRowMarkup(row, index) {
        return `
        <div class="mapping-row keyboard-key-row" data-keyboard-key-row="${index}">
          <input data-key-field="key" value="${escapeHtml(row.key || '')}" placeholder="ArrowUp">
          <select data-key-field="action">${scenarioSelectOptions(SCENARIO_KEY_ACTION_OPTIONS, row.action || 'set')}</select>
          <input data-key-field="target" data-action-kind="set" value="${escapeHtml(row.target || '')}" placeholder="local target">
          <input data-key-field="value" data-action-kind="set" type="number" step="any" value="${escapeHtml(row.value || '')}" placeholder="1.0">
          <input data-key-field="state" data-action-kind="toggle" value="${escapeHtml(row.state || '')}" placeholder="bool local">
          <input data-key-field="signal" data-action-kind="signal" value="${escapeHtml(row.signal || '')}" placeholder="reset">
          <input data-key-field="debounceMs" data-action-kind="toggle signal" type="number" step="1" min="0" value="${escapeHtml(row.debounceMs || '')}" placeholder="debounce ms">
          <input data-key-field="precondition" value="${escapeHtml(row.precondition || '')}" placeholder="throttle <= 0.05">
          <button type="button" class="ghost icon-button" data-remove-mapping>-</button>
        </div>`;
    }

    function scenarioGamepadAxisRowMarkup(row, index) {
        return `
        <div class="mapping-row gamepad-axis-row" data-gamepad-axis-row="${index}">
          <input data-axis-field="name" value="${escapeHtml(row.name || '')}" placeholder="throttle">
          <select data-axis-field="source">${scenarioSelectOptions(SCENARIO_GAMEPAD_AXIS_OPTIONS, row.source || 'LeftStickY')}</select>
          <input data-axis-field="write" value="${escapeHtml(row.write || '')}" placeholder="local target">
          <input data-axis-field="scale" type="number" step="any" value="${escapeHtml(row.scale || '')}" placeholder="1.0">
          <label class="check-field"><input data-axis-field="invert" type="checkbox"${row.invert ? ' checked' : ''}> invert</label>
          <button type="button" class="ghost icon-button" data-remove-mapping>-</button>
        </div>`;
    }

    function scenarioGamepadButtonRowMarkup(row, index) {
        return `
        <div class="mapping-row gamepad-button-row" data-gamepad-button-row="${index}">
          <input data-button-field="name" value="${escapeHtml(row.name || '')}" placeholder="arm">
          <select data-button-field="source">${scenarioSelectOptions(SCENARIO_GAMEPAD_BUTTON_OPTIONS, row.source || 'South')}</select>
          <select data-button-field="action">${scenarioSelectOptions(SCENARIO_KEY_ACTION_OPTIONS.filter(([value]) => value !== 'set'), row.action || 'toggle')}</select>
          <input data-button-field="state" data-action-kind="toggle" value="${escapeHtml(row.state || '')}" placeholder="bool local">
          <input data-button-field="signal" data-action-kind="signal" value="${escapeHtml(row.signal || '')}" placeholder="reset">
          <input data-button-field="debounceMs" type="number" step="1" min="0" value="${escapeHtml(row.debounceMs || '')}" placeholder="debounce ms">
          <input data-button-field="precondition" value="${escapeHtml(row.precondition || '')}" placeholder="throttle <= 0.05">
          <button type="button" class="ghost icon-button" data-remove-mapping>-</button>
        </div>`;
    }

    function scenarioStepperInputRowMarkup(row, index) {
        return `
        <div class="mapping-row stepper-input-row" data-stepper-input-row="${index}">
          <input data-stepper-field="name" value="${escapeHtml(row.name || '')}" placeholder="stick_throttle">
          <input data-stepper-field="source" value="${escapeHtml(row.source || '')}" placeholder="local:throttle">
          <button type="button" class="ghost icon-button" data-remove-mapping>-</button>
        </div>`;
    }

    function scenarioFieldByPath(fields, path) {
        const key = scenarioPathKey(path);
        return fields.find((field) => scenarioPathKey(field.path) === key);
    }

    function scenarioSectionTitle(section) {
        return {
            rumoca: 'Scenario',
            model: 'Model',
            sim: 'Simulation',
            parameters: 'Parameters',
            codegen: 'Code Generation',
            viewer: 'Results Display',
            source_roots: 'Modelica Libraries',
        }[section] || section.replace(/_/g, ' ');
    }

    function scenarioSectionMeta(section, fields) {
        if (section === 'rumoca') {
            const task = scenarioFieldByPath(fields, ['rumoca', 'task'])?.value || 'simulate';
            return scenarioOptionLabel(SCENARIO_TASK_OPTIONS, task);
        }
        if (section === 'model') {
            return trimMaybeString(scenarioFieldByPath(fields, ['model', 'name'])?.value)
                || trimMaybeString(scenarioFieldByPath(fields, ['model', 'file'])?.value)
                || 'No model selected';
        }
        if (section === 'sim') {
            const solver = scenarioOptionLabel(SCENARIO_SOLVER_OPTIONS, scenarioFieldByPath(fields, ['sim', 'solver'])?.value || 'auto');
            const pacing = scenarioOptionLabel(SCENARIO_SIM_MODE_OPTIONS, scenarioFieldByPath(fields, ['sim', 'mode'])?.value || '');
            const endTime = scenarioFieldByPath(fields, ['sim', 't_end'])?.value;
            return `${solver} · ${pacing} · end ${endTime || 10}`;
        }
        if (section === 'parameters') {
            return '';
        }
        if (section === 'viewer') {
            return scenarioOptionLabel(SCENARIO_VIEWER_MODE_OPTIONS, scenarioFieldByPath(fields, ['viewer', 'mode'])?.value || 'results_panel');
        }
        if (section === 'codegen') {
            return scenarioFieldByPath(fields, ['codegen', 'target'])?.value || 'sympy';
        }
        if (section === 'source_roots') {
            const roots = normalizeStringArray(scenarioFieldByPath(fields, ['source_roots'])?.value);
            return roots.length ? `${roots.length} scenario folder${roots.length === 1 ? '' : 's'}` : 'Workspace libraries only';
        }
        return '';
    }

    function scenarioSectionOpen(section) {
        return ['rumoca', 'model', 'sim'].includes(section);
    }

    function scenarioSectionMarkup(section, fields) {
        const rows = fields.map((field) => scenarioConfigFieldMarkup(field)).join('');
        const openAttr = scenarioSectionOpen(section) ? ' open' : '';
        const meta = scenarioSectionMeta(section, fields);
        return `
      <section class="card" data-scenario-section="${escapeHtml(section)}">
        <details${openAttr}>
          <summary>
            <span>${escapeHtml(scenarioSectionTitle(section))}</span>
            ${meta ? `<span class="summary-meta">${escapeHtml(meta)}</span>` : ''}
          </summary>
          <div class="grid">${rows}</div>
        </details>
      </section>`;
    }

    function scenarioFieldsBySection(fields) {
        const sections = new Map();
        for (const field of fields) {
            if (!sections.has(field.section)) {
                sections.set(field.section, []);
            }
            sections.get(field.section).push(field);
        }
        return sections;
    }

    function effectiveSourceRootMarkup(sourceRoots) {
        const roots = normalizeStringArray(sourceRoots);
        if (roots.length === 0) {
            return '';
        }
        const rootItems = roots.map((root) => `<li>${escapeHtml(root)}</li>`).join('');
        return `
      <section class="card" data-scenario-section="effective_source_roots">
        <details>
          <summary>
            <span>Workspace Source Roots</span>
            <span class="summary-meta">${roots.length} resolved</span>
          </summary>
          <ul class="readonly-list">${rootItems}</ul>
          <div class="hint">Resolved from rumoca-workspace.toml. Scenario files only store scenario-specific roots.</div>
        </details>
      </section>`;
    }

    function scenarioParameterSectionMarkup(parameters) {
        const rows = Array.isArray(parameters) && parameters.length
            ? parameters.map((parameter) => {
                const defaultValue = parameter.defaultValue === undefined || parameter.defaultValue === null || parameter.defaultValue === ''
                    ? ''
                    : String(parameter.defaultValue);
                const overrideValue = parameter.hasOverride ? String(parameter.overrideValue) : '';
                const rowMinAttr = Number.isFinite(parameter.minValue) ? ` data-parameter-min="${escapeHtml(String(parameter.minValue))}"` : '';
                const rowMaxAttr = Number.isFinite(parameter.maxValue) ? ` data-parameter-max="${escapeHtml(String(parameter.maxValue))}"` : '';
                const inputMinAttr = Number.isFinite(parameter.minValue) ? ` min="${escapeHtml(String(parameter.minValue))}"` : '';
                const inputMaxAttr = Number.isFinite(parameter.maxValue) ? ` max="${escapeHtml(String(parameter.maxValue))}"` : '';
                const unit = parameter.unit ? `<span>${escapeHtml(parameter.unit)}</span>` : '';
                const bounds = [
                    parameter.min ? `min ${parameter.min}` : '',
                    parameter.max ? `max ${parameter.max}` : '',
                    parameter.nominal ? `nominal ${parameter.nominal}` : '',
                ].filter(Boolean).join(' · ');
                const meta = [
                    defaultValue ? `default ${defaultValue}` : '',
                    bounds,
                    parameter.fixed !== undefined ? `fixed ${parameter.fixed ? 'true' : 'false'}` : '',
                ].filter(Boolean).join(' · ');
                return `
          <div class="parameter-row" data-parameter-row data-parameter-name="${escapeHtml(parameter.name)}"${rowMinAttr}${rowMaxAttr}>
            <div class="parameter-details">
              <strong>${escapeHtml(parameter.name)}</strong>
              ${unit}
              ${parameter.description ? `<div class="hint">${escapeHtml(parameter.description)}</div>` : ''}
              ${meta ? `<div class="hint">${escapeHtml(meta)}</div>` : ''}
            </div>
            <div class="parameter-control">
              <label>Override</label>
              <input type="number" step="any"${inputMinAttr}${inputMaxAttr} data-parameter-field="override" value="${escapeHtml(overrideValue)}" placeholder="${escapeHtml(defaultValue)}">
              <button type="button" class="ghost icon-button" data-clear-parameter title="Clear override">-</button>
            </div>
          </div>`;
            }).join('')
            : '<div class="empty">Compile the selected model to discover tunable scalar parameters.</div>';
        const meta = Array.isArray(parameters) && parameters.length
            ? `${parameters.length} tunable`
            : 'None discovered';
        return `
      <section class="card" id="parametersCard" data-scenario-section="parameters">
        <details>
          <summary>
            <span>Parameters</span>
            <span class="summary-meta">${escapeHtml(meta)}</span>
          </summary>
          <div class="parameter-list">${rows}</div>
        </details>
      </section>`;
    }

    function scenarioSectionsMarkup(fields, plotViews, inputMappings, parameters, advancedFields, effectiveSourceRoots) {
        const sections = scenarioFieldsBySection(fields);
        const mainMarkup = [...sections.keys()]
            .sort((a, b) => scenarioConfigSectionRank(a) - scenarioConfigSectionRank(b) || a.localeCompare(b))
            .map((section) => scenarioSectionMarkup(section, sections.get(section)))
            .join('');
        const advancedMarkup = advancedFields.length === 0 ? '' : `
      <section class="card">
        <details class="advanced-details">
          <summary>Advanced TOML Fields</summary>
          <div class="grid advanced-grid">${advancedFields.map((field) => scenarioConfigFieldMarkup(field)).join('')}</div>
        </details>
      </section>`;
        const taskField = fields.find((field) => scenarioPathKey(field.path) === 'rumoca.task');
        const task = trimMaybeString(taskField?.value) || 'simulate';
        const parameterMarkup = task === 'codegen' ? '' : scenarioParameterSectionMarkup(parameters);
        const plotMarkup = task === 'codegen' ? '' : scenarioPlotSectionMarkup(plotViews);
        const inputMarkup = task === 'codegen' ? '' : scenarioInputSectionMarkup(inputMappings);
        return `${mainMarkup}${parameterMarkup}${effectiveSourceRootMarkup(effectiveSourceRoots)}${inputMarkup}${plotMarkup}${advancedMarkup}`;
    }

    // Build the scenario config GUI document (iframe srcdoc). This is the one
    // scenario authoring GUI shared by VS Code, playground, and mdBook hosts.
    // Hosts provide file transport only; the editor sends typed edits back to
    // Rust scenario TOML rendering through the existing scenario command APIs.
    function buildScenarioConfigDocument(scenario) {
        const data = scenario && typeof scenario === 'object' ? scenario : {};
        const config = data.config && typeof data.config === 'object' ? data.config : {};
        const descriptor = data.descriptor && typeof data.descriptor === 'object' ? data.descriptor : {};
        const path = trimMaybeString(data.path) || 'rumoca-scenario.toml';
        const model = trimMaybeString(descriptor.model)
            || trimMaybeString((config.model || {}).name)
            || trimMaybeString(data.model);
        const task = trimMaybeString(descriptor.task) || trimMaybeString((config.rumoca || {}).task) || 'simulate';
        const theme = trimMaybeString(data.theme);
        const effectiveSourceRootPaths = normalizeStringArray(data.effectiveSourceRootPaths);
        const fields = typedScenarioConfigFields(config, task);
        const plotViews = scenarioPlotViews(config);
        const inputMappings = scenarioInputMappings(config);
        const parameterOverrides = scenarioParameterOverrides(config);
        const parameters = scenarioParameterMetadata(data.parameterMetadata, parameterOverrides);
        fields.forEach((field, index) => {
            field.index = index;
        });
        const sectionMarkup = scenarioSectionsMarkup(fields, plotViews, inputMappings, parameters, [], effectiveSourceRootPaths);
        const taskLabel = scenarioOptionLabel(SCENARIO_TASK_OPTIONS, task);
        const runLabel = task === 'codegen' ? 'Save & Generate Code' : 'Save & Simulate';
        const initialStateJson = escapeInlineScriptJson(JSON.stringify({
            path,
            fields,
            plotViews,
            inputMappings,
            parameters,
            runLabel,
        }));
        return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Rumoca Config: ${escapeHtml(model || path)}</title>
  <style>
    :root { --pad: 16px; --radius: 8px; --muted: var(--vscode-descriptionForeground, #9da5b4); --ok: var(--vscode-testing-iconPassed, #73c991); --error: var(--vscode-errorForeground, #f48771); }
    body[data-theme="midnight"] { --vscode-font-family: Inter, ui-sans-serif, system-ui, sans-serif; --vscode-foreground: #d6dee8; --vscode-editor-background: #0f1419; --vscode-sideBar-background: #151e27; --vscode-panel-border: #263341; --vscode-input-background: #18222d; --vscode-input-foreground: #d6dee8; --vscode-input-border: #334155; --vscode-descriptionForeground: #8b9aac; --vscode-button-background: #0891b2; --vscode-button-foreground: #ffffff; --vscode-button-border: transparent; --vscode-testing-iconPassed: #73c991; --vscode-errorForeground: #ff8a8a; --muted: #8b9aac; --ok: #73c991; --error: #ff8a8a; }
    body[data-theme="graphite"] { --vscode-font-family: Inter, ui-sans-serif, system-ui, sans-serif; --vscode-foreground: #e4e4e7; --vscode-editor-background: #161616; --vscode-sideBar-background: #232326; --vscode-panel-border: #37373d; --vscode-input-background: #2b2b30; --vscode-input-foreground: #e4e4e7; --vscode-input-border: #45454d; --vscode-descriptionForeground: #a1a1aa; --vscode-button-background: #0284c7; --vscode-button-foreground: #ffffff; --vscode-button-border: transparent; --vscode-testing-iconPassed: #73c991; --vscode-errorForeground: #fca5a5; --muted: #a1a1aa; --ok: #73c991; --error: #fca5a5; }
    body[data-theme="paper"] { --vscode-font-family: Inter, ui-sans-serif, system-ui, sans-serif; --vscode-foreground: #1f2937; --vscode-editor-background: #fbfaf7; --vscode-sideBar-background: #f3f0ea; --vscode-panel-border: #d2cbc0; --vscode-input-background: #ffffff; --vscode-input-foreground: #1f2937; --vscode-input-border: #c8c1b6; --vscode-descriptionForeground: #697386; --vscode-button-background: #0369a1; --vscode-button-foreground: #ffffff; --vscode-button-border: transparent; --vscode-testing-iconPassed: #047857; --vscode-errorForeground: #b91c1c; --muted: #697386; --ok: #047857; --error: #b91c1c; }
    html, body { height: 100%; }
    body { margin: 0; font-family: var(--vscode-font-family, system-ui, sans-serif); color: var(--vscode-foreground, #d4d4d4); background: var(--vscode-editor-background, #1e1e1e); }
    .page { padding: var(--pad); display: grid; gap: 12px; }
    .header { display: flex; align-items: center; justify-content: space-between; gap: 12px; }
    .title { font-size: 16px; font-weight: 700; margin: 0; }
    .subtitle { color: var(--muted); font-size: 12px; }
    .actions { display: flex; gap: 8px; }
    button { font: inherit; padding: 6px 12px; border-radius: 6px; border: 1px solid var(--vscode-button-border, transparent); background: var(--vscode-button-background, #0e639c); color: var(--vscode-button-foreground, #fff); cursor: pointer; }
    button.ghost { background: transparent; color: var(--vscode-foreground, #d4d4d4); border-color: var(--vscode-input-border, #3c3c3c); }
    .card { border: 1px solid var(--vscode-panel-border, #3c3c3c); border-radius: var(--radius); padding: 12px; background: var(--vscode-sideBar-background, #252526); }
    .card details { display: grid; gap: 10px; }
    .card summary { cursor: pointer; display: flex; align-items: center; justify-content: space-between; gap: 10px; margin: 0; font-size: 12px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.04em; color: var(--muted); }
    .card summary:hover { color: var(--vscode-foreground, #d4d4d4); }
    .card summary::before { content: ""; flex: 0 0 auto; width: 0; height: 0; border-top: 4px solid transparent; border-bottom: 4px solid transparent; border-left: 5px solid var(--muted); transform-origin: 2px 4px; }
    .card details[open] > summary::before { transform: rotate(90deg); }
    .card summary::-webkit-details-marker { display: none; }
    .card details[open] > summary { margin-bottom: 10px; }
    .summary-meta { margin-left: auto; font-size: 11px; font-weight: 500; text-transform: none; color: var(--muted); letter-spacing: 0; }
    .advanced-details { margin-top: 12px; padding-top: 10px; border-top: 1px solid var(--vscode-panel-border, #3c3c3c); }
    .section-header, .view-title-row { display: flex; align-items: center; justify-content: space-between; gap: 8px; }
    .grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 10px; }
    .field { display: grid; gap: 4px; }
    .wide-field { grid-column: 1 / -1; }
    label { font-size: 12px; font-weight: 600; }
    input, textarea, select { width: 100%; box-sizing: border-box; background: var(--vscode-input-background, #313131); color: var(--vscode-input-foreground, #ccc); border: 1px solid var(--vscode-input-border, #3c3c3c); border-radius: 6px; padding: 6px; }
    input.invalid, textarea.invalid, select.invalid { border-color: var(--error); }
    input[type="checkbox"] { width: auto; }
    .path-list { display: grid; gap: 8px; }
    .path-list-rows { display: grid; gap: 6px; }
    .path-row { display: grid; grid-template-columns: minmax(0, 1fr) auto auto; gap: 6px; align-items: center; }
    .icon-button { min-width: 32px; padding-inline: 8px; }
    .add-path { justify-self: start; }
    .mapping-group { display: grid; gap: 8px; margin-top: 14px; }
    .advanced-details > .mapping-group:first-of-type { margin-top: 0; }
    .input-subsection { border-top: 1px solid var(--vscode-panel-border, #3c3c3c); padding-top: 10px; }
    .input-subsection > summary { text-transform: none; letter-spacing: 0; }
    .input-subsection > .section-header { justify-content: flex-end; }
    .mapping-group h4 { margin: 0; font-size: 12px; }
    .mapping-row { display: grid; gap: 6px; align-items: center; margin: 6px 0; }
    .mapping-header { display: grid; gap: 6px; align-items: center; margin: 8px 0 2px; color: var(--muted); font-size: 11px; font-weight: 600; }
    .mapping-empty { margin: 8px 0; }
    .local-row { grid-template-columns: minmax(120px, 1fr) 120px minmax(100px, 1fr) auto; }
    .keyboard-key-row { grid-template-columns: minmax(88px, 0.7fr) minmax(130px, 1fr) repeat(6, minmax(88px, 1fr)) auto; }
    .gamepad-axis-row { grid-template-columns: minmax(100px, 1fr) minmax(120px, 1fr) minmax(100px, 1fr) minmax(80px, 0.7fr) auto auto; }
    .gamepad-button-row { grid-template-columns: minmax(90px, 1fr) minmax(120px, 1fr) minmax(120px, 1fr) repeat(4, minmax(90px, 1fr)) auto; }
    .integrator-row { grid-template-columns: minmax(100px, 1fr) minmax(130px, 1.2fr) minmax(100px, 1fr) repeat(4, minmax(80px, 0.8fr)) auto; }
    .stepper-input-row { grid-template-columns: minmax(120px, 1fr) minmax(160px, 2fr) auto; }
    .check-field { display: inline-flex; align-items: center; gap: 6px; }
    .hint { color: var(--muted); font-size: 11px; line-height: 1.35; }
    .readonly-list { margin: 0; padding-left: 18px; display: grid; gap: 4px; font-size: 12px; }
    .status { font-size: 12px; min-height: 16px; }
    .status.ok { color: var(--ok); } .status.error { color: var(--error); }
    .empty { color: var(--muted); font-size: 13px; }
    .view-row { display: grid; gap: 10px; padding: 10px 0; border-top: 1px solid var(--vscode-panel-border, #3c3c3c); }
    .view-row:first-of-type { border-top: 0; padding-top: 0; }
    .parameter-list { display: grid; gap: 10px; }
    .parameter-row { display: grid; grid-template-columns: minmax(0, 1.4fr) minmax(220px, 0.6fr); gap: 12px; align-items: start; padding: 10px 0; border-top: 1px solid var(--vscode-panel-border, #3c3c3c); }
    .parameter-row:first-child { border-top: 0; padding-top: 0; }
    .parameter-details { display: grid; gap: 3px; }
    .parameter-details > strong { font-size: 13px; }
    .parameter-control { display: grid; grid-template-columns: 1fr auto; gap: 6px; align-items: end; }
    .parameter-control label { grid-column: 1 / -1; }
    @media (max-width: 720px) {
      :root { --pad: 14px; }
      body { font-size: 15px; }
      .page { gap: 14px; }
      .header {
        align-items: stretch;
        flex-direction: column;
      }
      .title { font-size: 18px; }
      .subtitle,
      .hint,
      .status,
      .readonly-list,
      .summary-meta {
        font-size: 13px;
      }
      .actions {
        flex-wrap: wrap;
      }
      .actions button {
        flex: 1 1 120px;
      }
      button,
      input,
      textarea,
      select {
        min-height: 42px;
        font-size: 16px;
        padding: 8px 10px;
      }
      input[type="checkbox"] {
        min-width: 24px;
        min-height: 24px;
      }
      label,
      .card summary,
      .mapping-group h4 {
        font-size: 13px;
      }
      .card {
        padding: 14px;
      }
      .grid,
      .path-row,
      .parameter-row,
      .parameter-control,
      .mapping-row,
      .mapping-header,
      .local-row,
      .keyboard-key-row,
      .gamepad-axis-row,
      .gamepad-button-row,
      .integrator-row,
      .stepper-input-row {
        grid-template-columns: 1fr;
      }
      .icon-button {
        min-width: 42px;
      }
    }
  </style>
</head>
<body data-theme="${escapeHtml(theme)}">
  <div class="page">
    <div class="header">
      <div>
        <p class="title">${escapeHtml(model || 'Scenario')} <span id="taskSubtitle" class="subtitle">(${escapeHtml(taskLabel)})</span></p>
        <div class="subtitle">${escapeHtml(path)}</div>
      </div>
      <div class="actions">
        <button id="runBtn">${escapeHtml(runLabel)}</button>
        <button id="saveBtn">Save</button>
        <button id="rawBtn" class="ghost">Raw TOML</button>
      </div>
    </div>
    <div id="status" class="status"></div>
    ${sectionMarkup || '<div class="empty">This scenario has no editable fields yet. Use the lightning-bolt action on a model, or switch to the raw TOML view.</div>'}
    <div class="hint">Advanced or host-specific sections are preserved. Switch to Raw TOML only when the typed controls do not expose a setting yet.</div>
  </div>
  <script>
    const initialState = ${initialStateJson};
    const fields = Array.isArray(initialState.fields) ? initialState.fields : [];
    let plotViews = Array.isArray(initialState.plotViews) ? initialState.plotViews.slice() : [];
    let parameters = Array.isArray(initialState.parameters) ? initialState.parameters.slice() : [];
    const path = String(initialState.path || 'rumoca-scenario.toml');
    let runLabel = String(initialState.runLabel || 'Run');
    const statusEl = document.getElementById('status');
    const vscodeApi = typeof acquireVsCodeApi === 'function' ? acquireVsCodeApi() : null;
    const pendingRequests = new Map();
    let nextRequestId = 1;
    const browserHost = (() => {
      const own = globalThis.RumocaScenarioConfigHost;
      if (own && typeof own.request === 'function') return own;
      try {
        const parentHost = globalThis.parent && globalThis.parent !== globalThis
          ? globalThis.parent.RumocaScenarioConfigHost : null;
        if (parentHost && typeof parentHost.request === 'function') return parentHost;
      } catch (_error) { return null; }
      return null;
    })();

    function requestHost(method, payload) {
      if (browserHost) return Promise.resolve(browserHost.request(method, payload));
      if (!vscodeApi) return Promise.reject(new Error('config host unavailable'));
      const requestId = String(nextRequestId++);
      return new Promise((resolve, reject) => {
        pendingRequests.set(requestId, { resolve, reject });
        vscodeApi.postMessage({ command: 'scenarioConfig.request', requestId, method, payload });
      });
    }

    window.addEventListener('message', (event) => {
      const message = event && event.data ? event.data : {};
      if (message.command !== 'scenarioConfig.response') return;
      const pending = pendingRequests.get(String(message.requestId || ''));
      if (!pending) return;
      pendingRequests.delete(String(message.requestId));
      if (message.ok === false) pending.reject(new Error(String(message.error || 'config host request failed')));
      else pending.resolve(message.value);
    });

    function setStatus(text, level) {
      statusEl.textContent = text || '';
      statusEl.classList.remove('ok', 'error');
      if (level) statusEl.classList.add(level);
    }

    function escapeText(value) {
      return String(value ?? '').replace(/[&<>"']/g, (char) => ({
        '&': '&amp;',
        '<': '&lt;',
        '>': '&gt;',
        '"': '&quot;',
        "'": '&#39;',
      }[char]));
    }

    function fail(message, el) {
      if (el && el.classList) el.classList.add('invalid');
      throw new Error(message);
    }

    function parseLines(value) {
      return String(value || '').split(/\\r?\\n|,/).map((item) => item.trim()).filter(Boolean);
    }

    function sourceRootRowMarkup(value) {
      return [
        '<div class="path-row" data-path-row>',
        '<input type="text" data-list-item value="' + escapeText(value || '') + '" placeholder="../libraries/Modelica">',
        '<button type="button" class="ghost icon-button" data-browse-path title="Browse for a folder">...</button>',
        '<button type="button" class="ghost icon-button" data-remove-path title="Remove library folder">-</button>',
        '</div>',
      ].join('');
    }

    function sanitizeId(value, fallback) {
      const cleaned = String(value || '').trim().toLowerCase().replace(/[^a-z0-9_]+/g, '_').replace(/^_+|_+$/g, '');
      return cleaned || fallback;
    }

    function collectFieldValue(field, el) {
      if (!el) return undefined;
      el.classList.remove('invalid');
      if (field.kind === 'boolean') return el.checked;
      if (field.kind === 'stringList') {
        return Array.from(el.querySelectorAll('[data-list-item]'))
          .map((input) => String(input.value || '').trim())
          .filter(Boolean);
      }
      if (field.kind === 'select') {
        const allowed = new Set((field.options || []).map((option) => String(option[0])));
        const value = String(el.value || '').trim();
        if (!allowed.has(value)) fail('Choose a listed value for ' + field.label + '.', el);
        if (!value && field.optional) return undefined;
        return value;
      }
      if (field.kind === 'number' || field.kind === 'optionalNumber') {
        const raw = String(el.value || '').trim();
        if (!raw && field.optional) return undefined;
        const value = Number(raw);
        if (!Number.isFinite(value) || value <= 0) fail(field.label + ' must be a positive number.', el);
        return value;
      }
      const text = String(el.value || '').trim();
      if (!text && field.optional) return undefined;
      if (!text && (field.path.join('.') === 'model.name' || field.path.join('.') === 'model.file')) {
        fail(field.label + ' is required.', el);
      }
      return text;
    }

    function defaultView(type, index) {
      const suffix = String(index + 1);
      if (type === 'scatter') {
        return { id: 'scatter_' + suffix, title: 'Scatter', type: 'scatter', x: 'time', y: ['x'] };
      }
      if (type === '3d') {
        return { id: 'viewer_' + suffix, title: '3D View', type: '3d', x: 'time', y: ['x', 'y'], scriptPath: 'viewer.js' };
      }
      return { id: index === 0 ? 'states_time' : 'timeseries_' + suffix, title: index === 0 ? 'States vs Time' : 'Time Series', type: 'timeseries', x: 'time', y: ['*states'] };
    }

    function optionMarkup(value, label, selected) {
      return '<option value="' + escapeText(value) + '"' + (value === selected ? ' selected' : '') + '>' + escapeText(label) + '</option>';
    }

    function selectMarkup(values, selected) {
      return values.map(([value, label]) => optionMarkup(value, label, selected)).join('');
    }

    function optionLabel(values, selected, fallback) {
      const text = String(selected || '').trim();
      const match = values.find(([value]) => value === text);
      return match ? match[1] : (text || fallback);
    }

    function localRowMarkup(row, index) {
      const current = row || {};
      return [
        '<div class="mapping-row local-row" data-local-row="' + index + '">',
        '<input data-local-field="name" value="' + escapeText(current.name || '') + '" placeholder="throttle">',
        '<select data-local-field="type">' + selectMarkup([['float', 'float'], ['bool', 'bool']], current.type || 'float') + '</select>',
        '<input data-local-field="defaultValue" value="' + escapeText(current.defaultValue || '') + '" placeholder="0.0">',
        '<button type="button" class="ghost icon-button" data-remove-mapping>-</button>',
        '</div>',
      ].join('');
    }

    function keyboardKeyRowMarkup(row, index) {
      const current = row || {};
      return [
        '<div class="mapping-row keyboard-key-row" data-keyboard-key-row="' + index + '">',
        '<input data-key-field="key" value="' + escapeText(current.key || '') + '" placeholder="ArrowUp">',
        '<select data-key-field="action">' + selectMarkup([['set', 'set local value'], ['toggle', 'toggle local flag'], ['signal', 'emit signal']], current.action || 'set') + '</select>',
        '<input data-key-field="target" data-action-kind="set" value="' + escapeText(current.target || '') + '" placeholder="local target">',
        '<input data-key-field="value" data-action-kind="set" type="number" step="any" value="' + escapeText(current.value || '') + '" placeholder="1.0">',
        '<input data-key-field="state" data-action-kind="toggle" value="' + escapeText(current.state || '') + '" placeholder="bool local">',
        '<input data-key-field="signal" data-action-kind="signal" value="' + escapeText(current.signal || '') + '" placeholder="reset">',
        '<input data-key-field="debounceMs" data-action-kind="toggle signal" type="number" step="1" min="0" value="' + escapeText(current.debounceMs || '') + '" placeholder="debounce ms">',
        '<input data-key-field="precondition" value="' + escapeText(current.precondition || '') + '" placeholder="throttle <= 0.05">',
        '<button type="button" class="ghost icon-button" data-remove-mapping>-</button>',
        '</div>',
      ].join('');
    }

    function integratorRowMarkup(row, index, device) {
      const current = row || {};
      const prefix = device === 'gamepad' ? 'gamepad-integrator' : 'keyboard-integrator';
      return [
        '<div class="mapping-row integrator-row" data-' + prefix + '-row="' + index + '">',
        '<input data-integrator-field="name" value="' + escapeText(current.name || '') + '" placeholder="throttle">',
        '<input data-integrator-field="source" value="' + escapeText(current.source || '') + '" placeholder="' + (device === 'gamepad' ? 'LeftStickY' : 'local:throttle_input') + '">',
        '<input data-integrator-field="write" value="' + escapeText(current.write || '') + '" placeholder="local target">',
        '<input data-integrator-field="rate" type="number" step="any" value="' + escapeText(current.rate || '') + '" placeholder="0.7">',
        '<input data-integrator-field="deadband" type="number" step="any" value="' + escapeText(current.deadband || '') + '" placeholder="0.1">',
        '<input data-integrator-field="clampMin" type="number" step="any" value="' + escapeText(current.clampMin || '') + '" placeholder="0.0">',
        '<input data-integrator-field="clampMax" type="number" step="any" value="' + escapeText(current.clampMax || '') + '" placeholder="1.0">',
        '<button type="button" class="ghost icon-button" data-remove-mapping>-</button>',
        '</div>',
      ].join('');
    }

    function gamepadAxisRowMarkup(row, index) {
      const current = row || {};
      const sources = ['LeftStickX', 'LeftStickY', 'RightStickX', 'RightStickY', 'LeftZ', 'RightZ', 'DPadX', 'DPadY'].map((item) => [item, item]);
      return [
        '<div class="mapping-row gamepad-axis-row" data-gamepad-axis-row="' + index + '">',
        '<input data-axis-field="name" value="' + escapeText(current.name || '') + '" placeholder="throttle">',
        '<select data-axis-field="source">' + selectMarkup(sources, current.source || 'LeftStickY') + '</select>',
        '<input data-axis-field="write" value="' + escapeText(current.write || '') + '" placeholder="local target">',
        '<input data-axis-field="scale" type="number" step="any" value="' + escapeText(current.scale || '') + '" placeholder="1.0">',
        '<label class="check-field"><input data-axis-field="invert" type="checkbox"' + (current.invert ? ' checked' : '') + '> invert</label>',
        '<button type="button" class="ghost icon-button" data-remove-mapping>-</button>',
        '</div>',
      ].join('');
    }

    function gamepadButtonRowMarkup(row, index) {
      const current = row || {};
      const sources = ['South', 'East', 'North', 'West', 'Start', 'Select', 'LeftTrigger', 'RightTrigger', 'LeftThumb', 'RightThumb', 'DPadUp', 'DPadDown', 'DPadLeft', 'DPadRight', 'Mode'].map((item) => [item, item]);
      return [
        '<div class="mapping-row gamepad-button-row" data-gamepad-button-row="' + index + '">',
        '<input data-button-field="name" value="' + escapeText(current.name || '') + '" placeholder="arm">',
        '<select data-button-field="source">' + selectMarkup(sources, current.source || 'South') + '</select>',
        '<select data-button-field="action">' + selectMarkup([['toggle', 'toggle local flag'], ['signal', 'emit signal']], current.action || 'toggle') + '</select>',
        '<input data-button-field="state" data-action-kind="toggle" value="' + escapeText(current.state || '') + '" placeholder="bool local">',
        '<input data-button-field="signal" data-action-kind="signal" value="' + escapeText(current.signal || '') + '" placeholder="reset">',
        '<input data-button-field="debounceMs" type="number" step="1" min="0" value="' + escapeText(current.debounceMs || '') + '" placeholder="debounce ms">',
        '<input data-button-field="precondition" value="' + escapeText(current.precondition || '') + '" placeholder="throttle <= 0.05">',
        '<button type="button" class="ghost icon-button" data-remove-mapping>-</button>',
        '</div>',
      ].join('');
    }

    function stepperInputRowMarkup(row, index) {
      const current = row || {};
      return [
        '<div class="mapping-row stepper-input-row" data-stepper-input-row="' + index + '">',
        '<input data-stepper-field="name" value="' + escapeText(current.name || '') + '" placeholder="stick_throttle">',
        '<input data-stepper-field="source" value="' + escapeText(current.source || '') + '" placeholder="local:throttle">',
        '<button type="button" class="ghost icon-button" data-remove-mapping>-</button>',
        '</div>',
      ].join('');
    }

    function viewRowMarkup(view, index) {
      const current = view || defaultView('timeseries', index);
      const type = String(current.type || 'timeseries');
      const yText = Array.isArray(current.y) ? current.y.join('\\n') : '';
      const typeOptions = ['timeseries', 'scatter', '3d'].map((item) => optionMarkup(item, item, type)).join('');
      return [
        '<div class="view-row" data-view-index="' + index + '" data-view-id="' + escapeText(current.id || '') + '">',
        '<div class="view-title-row"><strong>' + escapeText(current.title || current.id || ('View ' + (index + 1))) + '</strong>',
        '<button type="button" class="ghost remove-view" data-remove-view="' + index + '">Remove</button></div>',
        '<div class="grid">',
        '<div class="field"><label for="view_' + index + '_title">title</label><input id="view_' + index + '_title" data-view-field="title" value="' + escapeText(current.title || '') + '"></div>',
        '<div class="field"><label for="view_' + index + '_type">type</label><select id="view_' + index + '_type" data-view-field="type">' + typeOptions + '</select></div>',
        '<div class="field" data-view-kind="series"><label for="view_' + index + '_x">x</label><input id="view_' + index + '_x" data-view-field="x" value="' + escapeText(current.x || 'time') + '"></div>',
        '<div class="field" data-view-kind="series"><label for="view_' + index + '_y">y series</label><textarea id="view_' + index + '_y" data-view-field="y" rows="3" placeholder="*states">' + escapeText(yText) + '</textarea></div>',
        '<div class="field" data-view-kind="script3d"' + (type === '3d' ? '' : ' hidden') + '><label for="view_' + index + '_script_path">viewer script path</label><input id="view_' + index + '_script_path" data-view-field="scriptPath" value="' + escapeText(current.scriptPath || '') + '" placeholder="viewer.js"></div>',
        '</div></div>',
      ].join('');
    }

    function renderPlotViews() {
      const container = document.getElementById('plotViewsList');
      if (!container) return;
      container.innerHTML = plotViews.length > 0
        ? plotViews.map((view, index) => viewRowMarkup(view, index)).join('')
        : '<div class="empty">No plot panels configured. Add a panel to save simulation results with a reusable view.</div>';
      syncPlotViewVisibility();
    }

    function syncPlotViewVisibility() {
      for (const row of document.querySelectorAll('[data-view-index]')) {
        const type = String(row.querySelector('[data-view-field="type"]')?.value || 'timeseries');
        row.querySelectorAll('[data-view-kind="series"]').forEach((field) => { field.hidden = type === '3d'; });
        row.querySelectorAll('[data-view-kind="script3d"]').forEach((field) => { field.hidden = type !== '3d'; });
      }
    }

    function syncActionVisibility(row) {
      const action = String(row.querySelector('[data-key-field="action"], [data-button-field="action"]')?.value || 'set');
      row.querySelectorAll('[data-action-kind]').forEach((field) => {
        field.hidden = !String(field.getAttribute('data-action-kind') || '').split(/\\s+/).includes(action);
      });
    }

    function syncInputVisibility() {
      const enabled = inputMappingEnabled();
      document.querySelectorAll('[data-input-enabled-field]').forEach((field) => {
        field.hidden = !enabled;
      });
      document.querySelectorAll('[data-keyboard-key-row], [data-gamepad-button-row]').forEach(syncActionVisibility);
    }

    function collectPlotViews() {
      const views = [];
      for (const row of document.querySelectorAll('[data-view-index]')) {
        const index = Number(row.getAttribute('data-view-index'));
        const current = plotViews[index] || defaultView('timeseries', index);
        const preservedId = String(row.getAttribute('data-view-id') || current.id || '');
        const typeEl = row.querySelector('[data-view-field="type"]');
        const titleEl = row.querySelector('[data-view-field="title"]');
        const xEl = row.querySelector('[data-view-field="x"]');
        const yEl = row.querySelector('[data-view-field="y"]');
        const scriptPathEl = row.querySelector('[data-view-field="scriptPath"]');
        for (const el of [typeEl, titleEl, xEl, yEl, scriptPathEl]) el?.classList.remove('invalid');
        const type = String(typeEl?.value || 'timeseries');
        if (!['timeseries', 'scatter', '3d'].includes(type)) fail('Choose a listed plot type.', typeEl);
        const title = String(titleEl?.value || '').trim() || (type === '3d' ? '3D View' : type === 'scatter' ? 'Scatter' : 'Time Series');
        const x = String(xEl?.value || '').trim() || 'time';
        const y = parseLines(yEl?.value);
        if (type !== '3d' && y.length === 0) fail('Add at least one y series for each plot panel.', yEl);
        if (type === '3d' && y.length > 0 && y.length < 2) fail('3D panels need two y entries for y/z data, or leave the field blank for script-driven views.', yEl);
        const view = {
          id: sanitizeId(preservedId || title, 'view_' + String(index + 1)),
          title,
          type,
          x,
          y: type === '3d' && y.length === 0 ? [] : y,
        };
        const scriptPath = String(scriptPathEl?.value || '').trim();
        if (type === '3d' && scriptPath) view.scriptPath = scriptPath;
        views.push(view);
      }
      return views;
    }

    function inputMappingEnabled() {
      return Boolean(document.querySelector('[data-input-enabled]')?.checked);
    }

    function cleanName(value, label, el) {
      const text = String(value || '').trim();
      if (!text) fail(label + ' is required.', el);
      return text;
    }

    function validateSignalSource(value, label, el) {
      const source = cleanName(value, label, el);
      if (
        !source.startsWith('stepper:')
        && !source.startsWith('local:')
        && !source.startsWith('runtime:')
      ) {
        fail(label + ' must start with stepper:, local:, or runtime:.', el);
      }
      return source;
    }

    function optionalNumber(value, label, el) {
      const raw = String(value || '').trim();
      if (!raw) return undefined;
      const number = Number(raw);
      if (!Number.isFinite(number)) fail(label + ' must be a number.', el);
      return number;
    }

    function optionalInteger(value, label, el) {
      const number = optionalNumber(value, label, el);
      if (number === undefined) return undefined;
      if (!Number.isInteger(number) || number < 0) fail(label + ' must be a non-negative integer.', el);
      return number;
    }

    function parseLocalDefault(raw, type, el) {
      const text = String(raw || '').trim();
      if (!text) return undefined;
      if (type === 'bool') {
        if (text === 'true' || text === '1') return true;
        if (text === 'false' || text === '0') return false;
        fail('Bool local defaults must be true or false.', el);
      }
      const number = Number(text);
      if (!Number.isFinite(number)) fail('Float local defaults must be numbers.', el);
      return number;
    }

    function assignUnique(target, key, value, el, label) {
      if (Object.prototype.hasOwnProperty.call(target, key)) fail(label + ' names must be unique.', el);
      target[key] = value;
    }

    function parseSignalRouteObject(route, el) {
      if (!route || typeof route !== 'object' || Array.isArray(route)) {
        fail('Structured model input routes must be objects.', el);
      }
      if (Object.prototype.hasOwnProperty.call(route, 'const')) {
        return { const: route.const };
      }
      const normalized = { from: validateSignalSource(route.from, 'Model input source', el) };
      const hasConditional = Object.prototype.hasOwnProperty.call(route, 'when_true')
        || Object.prototype.hasOwnProperty.call(route, 'when_false');
      const hasDefault = Object.prototype.hasOwnProperty.call(route, 'default');
      if (hasConditional) {
        if (
          !Object.prototype.hasOwnProperty.call(route, 'when_true')
          || !Object.prototype.hasOwnProperty.call(route, 'when_false')
        ) {
          fail('Conditional model inputs need both when_true and when_false.', el);
        }
        normalized.when_true = route.when_true;
        normalized.when_false = route.when_false;
        return normalized;
      }
      if (hasDefault) {
        normalized.default = route.default;
        return normalized;
      }
      return normalized.from;
    }

    function parseSignalRouteValue(value, el) {
      const text = cleanName(value, 'Model input source', el);
      if (!text.startsWith('{')) {
        return validateSignalSource(text, 'Model input source', el);
      }
      try {
        return parseSignalRouteObject(JSON.parse(text), el);
      } catch (error) {
        if (!(error instanceof SyntaxError)) {
          throw error;
        }
        fail('Structured model input source must be JSON, for example {"from":"local:armed","when_true":1,"when_false":0}.', el);
      }
      return undefined;
    }

    function collectLocals() {
      const locals = {};
      for (const row of document.querySelectorAll('[data-local-row]')) {
        const nameEl = row.querySelector('[data-local-field="name"]');
        const typeEl = row.querySelector('[data-local-field="type"]');
        const defaultEl = row.querySelector('[data-local-field="defaultValue"]');
        [nameEl, typeEl, defaultEl].forEach((el) => el?.classList.remove('invalid'));
        const name = cleanName(nameEl?.value, 'Local name', nameEl);
        const type = String(typeEl?.value || 'float');
        if (!['float', 'bool'].includes(type)) fail('Choose a listed local type.', typeEl);
        const local = { type };
        const defaultValue = parseLocalDefault(defaultEl?.value, type, defaultEl);
        if (defaultValue !== undefined) local.default = defaultValue;
        assignUnique(locals, name, local, nameEl, 'Local');
      }
      return Object.keys(locals).length ? locals : undefined;
    }

    function addOptionalActionFields(target, row, prefix) {
      const debounceEl = row.querySelector('[' + prefix + '="debounceMs"]');
      const preconditionEl = row.querySelector('[' + prefix + '="precondition"]');
      const debounceMs = optionalInteger(debounceEl?.value, 'debounce_ms', debounceEl);
      if (debounceMs !== undefined) target.debounce_ms = debounceMs;
      const precondition = String(preconditionEl?.value || '').trim();
      if (precondition) target.precondition = precondition;
    }

    function collectKeyboardKeys() {
      const keys = {};
      for (const row of document.querySelectorAll('[data-keyboard-key-row]')) {
        const keyEl = row.querySelector('[data-key-field="key"]');
        const actionEl = row.querySelector('[data-key-field="action"]');
        const targetEl = row.querySelector('[data-key-field="target"]');
        const valueEl = row.querySelector('[data-key-field="value"]');
        const stateEl = row.querySelector('[data-key-field="state"]');
        const signalEl = row.querySelector('[data-key-field="signal"]');
        row.querySelectorAll('input, select').forEach((el) => el.classList.remove('invalid'));
        const key = cleanName(keyEl?.value, 'Keyboard key', keyEl);
        const action = String(actionEl?.value || 'set');
        const binding = { action };
        if (action === 'set') {
          binding.target = cleanName(targetEl?.value, 'Key target', targetEl);
          const value = optionalNumber(valueEl?.value, 'Key value', valueEl);
          if (value === undefined) fail('Key set actions require a value.', valueEl);
          binding.value = value;
        } else if (action === 'toggle') {
          binding.state = cleanName(stateEl?.value, 'Toggle state', stateEl);
          addOptionalActionFields(binding, row, 'data-key-field');
        } else if (action === 'signal') {
          binding.signal = cleanName(signalEl?.value, 'Signal name', signalEl);
          addOptionalActionFields(binding, row, 'data-key-field');
        } else {
          fail('Choose a listed keyboard action.', actionEl);
        }
        assignUnique(keys, key, binding, keyEl, 'Keyboard key');
      }
      return Object.keys(keys).length ? keys : undefined;
    }

    function collectIntegrators(selector, label) {
      const integrators = {};
      for (const row of document.querySelectorAll(selector)) {
        const nameEl = row.querySelector('[data-integrator-field="name"]');
        const sourceEl = row.querySelector('[data-integrator-field="source"]');
        const writeEl = row.querySelector('[data-integrator-field="write"]');
        const rateEl = row.querySelector('[data-integrator-field="rate"]');
        const deadbandEl = row.querySelector('[data-integrator-field="deadband"]');
        const clampMinEl = row.querySelector('[data-integrator-field="clampMin"]');
        const clampMaxEl = row.querySelector('[data-integrator-field="clampMax"]');
        row.querySelectorAll('input').forEach((el) => el.classList.remove('invalid'));
        const rate = optionalNumber(rateEl?.value, label + ' rate', rateEl);
        const clampMin = optionalNumber(clampMinEl?.value, label + ' clamp minimum', clampMinEl);
        const clampMax = optionalNumber(clampMaxEl?.value, label + ' clamp maximum', clampMaxEl);
        if (rate === undefined) fail(label + ' rate is required.', rateEl);
        if (clampMin === undefined) fail(label + ' clamp minimum is required.', clampMinEl);
        if (clampMax === undefined) fail(label + ' clamp maximum is required.', clampMaxEl);
        if (clampMin > clampMax) fail(label + ' clamp minimum must be <= maximum.', clampMinEl);
        const integrator = {
          source: cleanName(sourceEl?.value, label + ' source', sourceEl),
          write: cleanName(writeEl?.value, label + ' target', writeEl),
          rate,
          clamp: [clampMin, clampMax],
        };
        const deadband = optionalNumber(deadbandEl?.value, label + ' deadband', deadbandEl);
        if (deadband !== undefined) integrator.deadband = deadband;
        assignUnique(integrators, cleanName(nameEl?.value, label + ' name', nameEl), integrator, nameEl, label);
      }
      return Object.keys(integrators).length ? integrators : undefined;
    }

    function collectGamepadAxes() {
      const axes = {};
      for (const row of document.querySelectorAll('[data-gamepad-axis-row]')) {
        const nameEl = row.querySelector('[data-axis-field="name"]');
        const sourceEl = row.querySelector('[data-axis-field="source"]');
        const writeEl = row.querySelector('[data-axis-field="write"]');
        const scaleEl = row.querySelector('[data-axis-field="scale"]');
        row.querySelectorAll('input, select').forEach((el) => el.classList.remove('invalid'));
        const axis = {
          source: cleanName(sourceEl?.value, 'Gamepad axis source', sourceEl),
          write: cleanName(writeEl?.value, 'Gamepad axis target', writeEl),
        };
        const scale = optionalNumber(scaleEl?.value, 'Gamepad axis scale', scaleEl);
        if (scale !== undefined) axis.scale = scale;
        if (row.querySelector('[data-axis-field="invert"]')?.checked) axis.invert = true;
        assignUnique(axes, cleanName(nameEl?.value, 'Gamepad axis name', nameEl), axis, nameEl, 'Gamepad axis');
      }
      return Object.keys(axes).length ? axes : undefined;
    }

    function collectGamepadButtons() {
      const buttons = {};
      for (const row of document.querySelectorAll('[data-gamepad-button-row]')) {
        const nameEl = row.querySelector('[data-button-field="name"]');
        const sourceEl = row.querySelector('[data-button-field="source"]');
        const actionEl = row.querySelector('[data-button-field="action"]');
        const stateEl = row.querySelector('[data-button-field="state"]');
        const signalEl = row.querySelector('[data-button-field="signal"]');
        row.querySelectorAll('input, select').forEach((el) => el.classList.remove('invalid'));
        const action = String(actionEl?.value || 'toggle');
        const button = {
          source: cleanName(sourceEl?.value, 'Gamepad button source', sourceEl),
          action,
        };
        if (action === 'toggle') {
          button.state = cleanName(stateEl?.value, 'Button toggle state', stateEl);
        } else if (action === 'signal') {
          button.signal = cleanName(signalEl?.value, 'Button signal', signalEl);
        } else {
          fail('Choose a listed gamepad button action.', actionEl);
        }
        addOptionalActionFields(button, row, 'data-button-field');
        assignUnique(buttons, cleanName(nameEl?.value, 'Gamepad button name', nameEl), button, nameEl, 'Gamepad button');
      }
      return Object.keys(buttons).length ? buttons : undefined;
    }

    function collectStepperInputs() {
      const inputs = {};
      for (const row of document.querySelectorAll('[data-stepper-input-row]')) {
        const nameEl = row.querySelector('[data-stepper-field="name"]');
        const sourceEl = row.querySelector('[data-stepper-field="source"]');
        row.querySelectorAll('input').forEach((el) => el.classList.remove('invalid'));
        const name = cleanName(nameEl?.value, 'Model input name', nameEl);
        const source = parseSignalRouteValue(sourceEl?.value, sourceEl);
        assignUnique(inputs, name, source, nameEl, 'Model input');
      }
      return Object.keys(inputs).length ? inputs : undefined;
    }

    function collectInputEdits() {
      if (!document.getElementById('inputMappingsCard') || !inputMappingEnabled()) {
        return [
          { path: ['input'], value: undefined },
          { path: ['signals', 'stepper_inputs'], value: undefined },
        ];
      }
      const keyboardKeys = collectKeyboardKeys();
      const keyboardIntegrators = collectIntegrators('[data-keyboard-integrator-row]', 'Keyboard integrator');
      const gamepadAxes = collectGamepadAxes();
      const gamepadIntegrators = collectIntegrators('[data-gamepad-integrator-row]', 'Gamepad integrator');
      const gamepadButtons = collectGamepadButtons();
      const input = { mode: String(document.querySelector('[data-input-mode]')?.value || 'auto') };
      if (keyboardKeys || keyboardIntegrators) input.keyboard = {
        ...(keyboardKeys ? { keys: keyboardKeys } : {}),
        ...(keyboardIntegrators ? { integrators: keyboardIntegrators } : {}),
      };
      if (gamepadAxes || gamepadIntegrators || gamepadButtons) input.gamepad = {
        ...(gamepadAxes ? { axes: gamepadAxes } : {}),
        ...(gamepadIntegrators ? { integrators: gamepadIntegrators } : {}),
        ...(gamepadButtons ? { buttons: gamepadButtons } : {}),
      };
      return [
        { path: ['input'], value: input },
        { path: ['locals'], value: collectLocals() },
        { path: ['signals', 'stepper_inputs'], value: collectStepperInputs() },
      ];
    }

    function collectParameterOverrides() {
      const overrides = {};
      const card = document.getElementById('parametersCard');
      if (!card) return undefined;
      for (const row of card.querySelectorAll('[data-parameter-row]')) {
        const name = String(row.getAttribute('data-parameter-name') || '').trim();
        const input = row.querySelector('[data-parameter-field="override"]');
        input?.classList.remove('invalid');
        const raw = String(input?.value || '').trim();
        if (!raw) continue;
        const value = Number(raw);
        if (!Number.isFinite(value)) fail('Parameter override for ' + name + ' must be numeric.', input);
        const minText = row.getAttribute('data-parameter-min');
        const maxText = row.getAttribute('data-parameter-max');
        const min = minText === null ? NaN : Number(minText);
        const max = maxText === null ? NaN : Number(maxText);
        if (Number.isFinite(min) && value < min) fail('Parameter override for ' + name + ' must be >= ' + min + '.', input);
        if (Number.isFinite(max) && value > max) fail('Parameter override for ' + name + ' must be <= ' + max + '.', input);
        overrides[name] = value;
      }
      return Object.keys(overrides).length ? overrides : undefined;
    }

    function collectEdits() {
      const edits = [];
      for (const field of fields) {
        const el = document.querySelector('[data-field="' + field.index + '"]');
        if (el && !el.closest('[data-scenario-section]')?.hidden) {
          edits.push({ path: field.path, value: collectFieldValue(field, el) });
        }
      }
      if (!document.getElementById('plotViewsCard')?.hidden) {
        edits.push({ path: ['plot', 'views'], value: collectPlotViews() });
      }
      if (!document.getElementById('inputMappingsCard')?.hidden) {
        edits.push(...collectInputEdits());
      }
      if (!document.getElementById('parametersCard')?.hidden) {
        edits.push({ path: ['parameters'], value: collectParameterOverrides() });
      }
      return edits;
    }

    function selectedTask() {
      const taskField = fields.find((field) => Array.isArray(field.path) && field.path.join('.') === 'rumoca.task');
      const el = taskField ? document.querySelector('[data-field="' + taskField.index + '"]') : null;
      return String(el?.value || 'simulate');
    }

    function runLabelForTask(task) {
      return task === 'codegen' ? 'Save & Generate Code' : 'Save & Simulate';
    }

    function taskLabelForTask(task) {
      return optionLabel([['simulate', 'Simulation'], ['codegen', 'Code generation']], task, 'Simulation');
    }

    function syncTaskVisibility() {
      const task = selectedTask();
      runLabel = runLabelForTask(task);
      const runBtn = document.getElementById('runBtn');
      if (runBtn) runBtn.textContent = runLabel;
      const taskSubtitle = document.getElementById('taskSubtitle');
      if (taskSubtitle) taskSubtitle.textContent = '(' + taskLabelForTask(task) + ')';
      document.querySelectorAll('[data-scenario-section="sim"], [data-scenario-section="parameters"], [data-scenario-section="viewer"], [data-scenario-section="input"], [data-scenario-section="plot"]')
        .forEach((section) => { section.hidden = task === 'codegen'; });
      document.querySelectorAll('[data-scenario-section="codegen"]')
        .forEach((section) => { section.hidden = task !== 'codegen'; });
      syncInputVisibility();
    }

    for (const field of fields) {
      if (Array.isArray(field.path) && field.path.join('.') === 'rumoca.task') {
        document.querySelector('[data-field="' + field.index + '"]')?.addEventListener('change', syncTaskVisibility);
      }
    }
    document.getElementById('plotViewsList')?.addEventListener('change', (event) => {
      if (event.target?.matches?.('[data-view-field="type"]')) {
        syncPlotViewVisibility();
      }
    });
    document.getElementById('inputMappingsCard')?.addEventListener('change', (event) => {
      if (event.target?.matches?.('[data-input-enabled], [data-key-field="action"], [data-button-field="action"]')) {
        syncInputVisibility();
      }
    });
    document.addEventListener('click', async (event) => {
      const addButton = event.target?.closest?.('[data-add-path]');
      if (addButton) {
        const list = addButton.closest('[data-field]');
        list?.querySelector('[data-path-list-rows]')?.insertAdjacentHTML('beforeend', sourceRootRowMarkup(''));
        const rows = list?.querySelectorAll('[data-list-item]');
        rows?.[rows.length - 1]?.focus();
        return;
      }
      const removeButton = event.target?.closest?.('[data-remove-path]');
      if (removeButton) {
        removeButton.closest('[data-path-row]')?.remove();
        return;
      }
      const browseButton = event.target?.closest?.('[data-browse-path]');
      if (browseButton) {
        const row = browseButton.closest('[data-path-row]');
        const input = row?.querySelector('[data-list-item]');
        try {
          const selected = await requestHost('browseSourceRoot', { path, current: input?.value || '' });
          const value = String(selected?.path || selected || '').trim();
          if (value && input) input.value = value;
        } catch (error) {
          setStatus('Folder picker is not available in this host.', 'error');
        }
      }
      const removeMapping = event.target?.closest?.('[data-remove-mapping]');
      if (removeMapping) {
        removeMapping.closest('.mapping-row')?.remove();
        return;
      }
      if (event.target?.closest?.('[data-add-local]')) {
        const list = document.querySelector('[data-local-list]');
        const index = list?.querySelectorAll('[data-local-row]').length || 0;
        list?.insertAdjacentHTML('beforeend', localRowMarkup({}, index));
        return;
      }
      if (event.target?.closest?.('[data-add-keyboard-key]')) {
        const list = document.querySelector('[data-keyboard-key-list]');
        const index = list?.querySelectorAll('[data-keyboard-key-row]').length || 0;
        list?.insertAdjacentHTML('beforeend', keyboardKeyRowMarkup({}, index));
        syncInputVisibility();
        return;
      }
      if (event.target?.closest?.('[data-add-keyboard-integrator]')) {
        const list = document.querySelector('[data-keyboard-integrator-list]');
        const index = list?.querySelectorAll('[data-keyboard-integrator-row]').length || 0;
        list?.insertAdjacentHTML('beforeend', integratorRowMarkup({}, index, 'keyboard'));
        return;
      }
      if (event.target?.closest?.('[data-add-gamepad-axis]')) {
        const list = document.querySelector('[data-gamepad-axis-list]');
        const index = list?.querySelectorAll('[data-gamepad-axis-row]').length || 0;
        list?.insertAdjacentHTML('beforeend', gamepadAxisRowMarkup({}, index));
        return;
      }
      if (event.target?.closest?.('[data-add-gamepad-integrator]')) {
        const list = document.querySelector('[data-gamepad-integrator-list]');
        const index = list?.querySelectorAll('[data-gamepad-integrator-row]').length || 0;
        list?.insertAdjacentHTML('beforeend', integratorRowMarkup({}, index, 'gamepad'));
        return;
      }
      if (event.target?.closest?.('[data-add-gamepad-button]')) {
        const list = document.querySelector('[data-gamepad-button-list]');
        const index = list?.querySelectorAll('[data-gamepad-button-row]').length || 0;
        list?.insertAdjacentHTML('beforeend', gamepadButtonRowMarkup({}, index));
        syncInputVisibility();
        return;
      }
      if (event.target?.closest?.('[data-add-stepper-input]')) {
        const list = document.querySelector('[data-stepper-input-list]');
        const index = list?.querySelectorAll('[data-stepper-input-row]').length || 0;
        list?.insertAdjacentHTML('beforeend', stepperInputRowMarkup({}, index));
      }
      const clearParameter = event.target?.closest?.('[data-clear-parameter]');
      if (clearParameter) {
        const input = clearParameter.closest('[data-parameter-row]')?.querySelector('[data-parameter-field="override"]');
        if (input) input.value = '';
      }
    });
    syncTaskVisibility();
    syncPlotViewVisibility();
    syncInputVisibility();

    document.getElementById('addPlotView')?.addEventListener('click', () => {
      plotViews = collectPlotViews();
      plotViews.push(defaultView('timeseries', plotViews.length));
      renderPlotViews();
    });
    document.getElementById('plotViewsList')?.addEventListener('click', (event) => {
      const button = event.target && event.target.closest ? event.target.closest('[data-remove-view]') : null;
      if (!button) return;
      const index = Number(button.getAttribute('data-remove-view'));
      plotViews = collectPlotViews().filter((_view, viewIndex) => viewIndex !== index);
      renderPlotViews();
    });

    document.getElementById('saveBtn').addEventListener('click', async () => {
      try {
        setStatus('Saving…');
        await requestHost('save', { path, edits: collectEdits() });
        setStatus('Saved', 'ok');
      } catch (error) {
        setStatus(String(error && error.message ? error.message : error), 'error');
      }
    });
    document.getElementById('runBtn').addEventListener('click', async () => {
      try {
        setStatus(runLabel + '…');
        const response = await requestHost('run', { path, edits: collectEdits() });
        const message = response && typeof response.message === 'string'
          ? response.message : runLabel + ' requested';
        setStatus(message, 'ok');
      } catch (error) {
        setStatus(String(error && error.message ? error.message : error), 'error');
      }
    });
    document.getElementById('rawBtn').addEventListener('click', () => {
      requestHost('toggleRaw', { path }).catch(() => {});
    });
  </script>
</body>
</html>`;
    }

const VisualizationShared = {
        buildHostedResultsPanelState,
        flattenScenarioConfig,
        setScenarioConfigValue,
        applyScenarioConfigEdits,
        buildScenarioConfigDocument,
        buildHostedResultsPanelTitle,
        buildVisualizationModel,
        buildSimulationRunDocument,
        defaultThreeDimensionalViewerScript,
        defaultVisualizationViews,
        buildHostedResultsDocument,
        buildVisualizationViewStorageHandlers,
        handleHostedResultsRequest,
        isRumocaScenarioPath,
        loadHostedSimulationRun,
        loadHostedSimulationRunWithViews,
        modelScopedViewerScriptRelativePath,
        nextSimulationRunLocation,
        normalizeHostedPngExportRequest,
        normalizeHostedResultsPanelState,
        normalizeHostedResultsModelRef,
        normalizeHostedResultsNotifyPayload,
        normalizeVisualizationViews,
        normalizePersistedSimulationRun,
        normalizeSimulationPayload,
        normalizeSimulationRunMetrics,
        normalizeHostedWebmExportRequest,
        persistHostedSimulationRun,
        persistHostedSimulationRunWithViews,
        preferredViewerScriptPathForModel,
        hydrateVisualizationViewsForModel,
        persistVisualizationViewsForModel,
        readPersistedSimulationRunDocument,
        removeStaleVisualizationScriptFiles,
        removeVisualizationScriptFilesForViews,
        sanitizeResultsPathSegment,
        simulationRunDocumentPath,
        writePersistedSimulationRunDocument,
    };

export {
        buildHostedResultsPanelState,
        flattenScenarioConfig,
        setScenarioConfigValue,
        applyScenarioConfigEdits,
        buildScenarioConfigDocument,
        buildHostedResultsPanelTitle,
        buildVisualizationModel,
        buildSimulationRunDocument,
        defaultThreeDimensionalViewerScript,
        defaultVisualizationViews,
        buildHostedResultsDocument,
        buildVisualizationViewStorageHandlers,
        handleHostedResultsRequest,
        isRumocaScenarioPath,
        loadHostedSimulationRun,
        loadHostedSimulationRunWithViews,
        modelScopedViewerScriptRelativePath,
        nextSimulationRunLocation,
        normalizeHostedPngExportRequest,
        normalizeHostedResultsPanelState,
        normalizeHostedResultsModelRef,
        normalizeHostedResultsNotifyPayload,
        normalizeVisualizationViews,
        normalizePersistedSimulationRun,
        normalizeSimulationPayload,
        normalizeSimulationRunMetrics,
        normalizeHostedWebmExportRequest,
        persistHostedSimulationRun,
        persistHostedSimulationRunWithViews,
        preferredViewerScriptPathForModel,
        hydrateVisualizationViewsForModel,
        persistVisualizationViewsForModel,
        readPersistedSimulationRunDocument,
        removeStaleVisualizationScriptFiles,
        removeVisualizationScriptFilesForViews,
        sanitizeResultsPathSegment,
        simulationRunDocumentPath,
        writePersistedSimulationRunDocument,
};

export default VisualizationShared;
