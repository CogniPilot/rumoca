import * as shared from '../../vendor/visualization_shared.js';

function cloneJson(value) {
    return value === undefined ? undefined : JSON.parse(JSON.stringify(value));
}

function trimMaybeString(value) {
    return typeof value === 'string' ? value.trim() : '';
}

function normalizeSimulationSettingsCurrent(value) {
    const current = value && typeof value === 'object' ? value : {};
    const tEnd = Number(current.tEnd);
    const dt = Number(current.dt);
    return {
        solver: trimMaybeString(current.solver) || 'auto',
        tEnd: Number.isFinite(tEnd) && tEnd > 0 ? tEnd : 10,
        dt: Number.isFinite(dt) && dt > 0 ? dt : null,
        outputDir: trimMaybeString(current.outputDir),
        sourceRootPaths: Array.isArray(current.sourceRootPaths)
            ? current.sourceRootPaths.map(trimMaybeString).filter(Boolean)
            : [],
    };
}

function isWorkspaceConfigPath(path) {
    const normalized = String(path || '').replace(/\\/g, '/').replace(/^\/+/, '');
    return normalized.split('/').pop()?.toLowerCase() === 'rumoca-workspace.toml';
}

function normalizeWorkspacePath(path) {
    return String(path || '')
        .replace(/\\/g, '/')
        .replace(/^\/+/, '')
        .replace(/\/+/g, '/')
        .trim()
        .split('/')
        .filter((part) => part && part !== '.')
        .join('/');
}

function isInsideSourceRoot(filePath, sourceRoot) {
    return filePath === sourceRoot || filePath.startsWith(`${sourceRoot}/`);
}

function captureWorkspaceSourcesJson(workspaceFs) {
    const sources = {};
    for (const file of workspaceFs.listFiles()) {
        const filePath = String(file.path || '');
        if (typeof file.content !== 'string') {
            continue;
        }
        if (
            shared.isRumocaScenarioPath(filePath)
            || isWorkspaceConfigPath(filePath)
            || filePath.toLowerCase().endsWith('.mo')
        ) {
            sources[filePath] = file.content;
        }
    }
    return JSON.stringify(sources);
}

function captureWorkspaceSourceRootsJson(workspaceFs, sourceRootPaths) {
    const roots = Array.isArray(sourceRootPaths)
        ? sourceRootPaths.map(normalizeWorkspacePath).filter(Boolean)
        : [];
    if (roots.length === 0) {
        return '{}';
    }
    const sources = {};
    for (const file of workspaceFs.listFiles()) {
        const filePath = normalizeWorkspacePath(file.path);
        if (!filePath || !filePath.toLowerCase().endsWith('.mo')) {
            continue;
        }
        if (typeof file.content !== 'string') {
            continue;
        }
        if (roots.some((root) => isInsideSourceRoot(filePath, root))) {
            sources[filePath] = file.content;
        }
    }
    return JSON.stringify(sources);
}

function applyScenarioPatch(workspaceFs, response) {
    const parsed = response && typeof response === 'object' ? response : {};
    const writes = Array.isArray(parsed.writes) ? parsed.writes : [];
    const removes = Array.isArray(parsed.removes) ? parsed.removes : [];

    for (const write of writes) {
        const path = trimMaybeString(write?.path);
        if (!path) {
            continue;
        }
        workspaceFs.setFile(path, typeof write?.content === 'string' ? write.content : '');
    }
    for (const path of removes) {
        const normalized = trimMaybeString(path);
        if (normalized) {
            workspaceFs.removeFile(normalized);
        }
    }
    if (Object.prototype.hasOwnProperty.call(parsed, 'editorState')) {
        workspaceFs.setEditorState(cloneJson(parsed.editorState || {}));
    }
    return parsed.result ?? parsed;
}

export function preferredViewerScriptPathForModel(model, viewId) {
    return shared.preferredViewerScriptPathForModel(model, viewId);
}

export function createScenarioInterface({ workspaceFs, runtimeBridge = null, onWorkspaceMutation = null }) {
    let selectedSimulationModel = '';

    function readSelectedSimulationModel() {
        return selectedSimulationModel;
    }

    async function requestRuntime(action, payload = {}, timeoutMs) {
        if (!runtimeBridge || typeof runtimeBridge.request !== 'function') {
            throw new Error(`Runtime bridge unavailable for ${action}`);
        }
        return await runtimeBridge.request(action, payload, timeoutMs);
    }

    async function requestScenarioCommand(command, payload = {}, timeoutMs) {
        return await requestRuntime(
            'scenarioCommand',
            {
                command,
                payload,
            },
            timeoutMs,
        );
    }

    async function executeScenarioCommand(command, payload = {}, timeoutMs) {
        const raw = await requestScenarioCommand(
            command,
            {
                ...payload,
                workspaceSources: captureWorkspaceSourcesJson(workspaceFs),
            },
            timeoutMs,
        );
        const parsed = typeof raw === 'string' ? JSON.parse(raw) : raw;
        const result = applyScenarioPatch(workspaceFs, parsed);
        onWorkspaceMutation?.();
        return result;
    }

    async function getSimulationModels({ source, defaultModel }) {
        const preferredModel = readSelectedSimulationModel() || trimMaybeString(defaultModel);
        const raw = await requestScenarioCommand('rumoca.scenario.getSimulationModels', {
            source,
            defaultModel: preferredModel,
        });
        const parsed = typeof raw === 'string' ? JSON.parse(raw) : raw;
        const models = Array.isArray(parsed?.models)
            ? parsed.models.map((entry) => trimMaybeString(entry)).filter(Boolean)
            : [];
        const selectedModel = trimMaybeString(parsed?.selectedModel)
            || (preferredModel && models.includes(preferredModel) ? preferredModel : '')
            || models[0]
            || '';
        setSelectedSimulationModel(selectedModel);
        return {
            ok: parsed?.ok !== false,
            models,
            selectedModel: selectedModel || null,
            error: trimMaybeString(parsed?.error) || null,
        };
    }

    async function getScenarioParameterOverrides(path) {
        const scenarioPath = trimMaybeString(path);
        if (!scenarioPath || !shared.isRumocaScenarioPath(scenarioPath)) {
            return {};
        }
        const full = await executeScenarioCommand('rumoca.scenario.getScenarioConfigFull', {
            path: scenarioPath,
        });
        const parameters = full?.config?.parameters;
        return parameters && typeof parameters === 'object' && !Array.isArray(parameters)
            ? parameters
            : {};
    }

    async function getParameterMetadata({
        path,
        source,
        sourcePath,
        modelName,
        fallback,
        timeoutMs,
        workspaceSources = null,
    }) {
        const selectedModel = trimMaybeString(modelName) || readSelectedSimulationModel();
        if (!selectedModel || typeof source !== 'string') {
            return [];
        }
        const simulationConfig = await executeScenarioCommand(
            'rumoca.scenario.getSimulationConfig',
            {
                model: selectedModel,
                fallback,
                ...(trimMaybeString(path) ? { path: trimMaybeString(path) } : {}),
            },
        );
        const effective = simulationConfig?.effective || normalizeSimulationSettingsCurrent(fallback);
        const requestPayload = {
            source,
            modelName: selectedModel,
        };
        const sourceRoots = captureWorkspaceSourceRootsJson(workspaceFs, effective?.sourceRootPaths);
        if (sourceRoots !== '{}') {
            requestPayload.sourceRoots = sourceRoots;
        }
        if (typeof workspaceSources === 'string') {
            requestPayload.workspaceSources = workspaceSources;
        } else {
            requestPayload.workspaceSources = shared.workspaceModelicaSourcesJson(workspaceFs.listFiles(), {
                excludePath: sourcePath,
                excludeSourceRootPaths: effective?.sourceRootPaths,
            });
        }
        const raw = await requestScenarioCommand(
            'rumoca.model.parameterMetadata',
            requestPayload,
            timeoutMs,
        );
        const parsed = typeof raw === 'string' ? JSON.parse(raw) : raw;
        if (Array.isArray(parsed)) {
            return parsed;
        }
        return Array.isArray(parsed?.parameters) ? parsed.parameters : [];
    }

    async function startSimulation({ source, model, fallback, timeoutMs, workspaceSources = null, scenarioPath = '' }) {
        const selectedModel = trimMaybeString(model) || readSelectedSimulationModel();
        if (!selectedModel) {
            throw new Error('No simulation model selected');
        }
        setSelectedSimulationModel(selectedModel);
        const simulationConfig = await executeScenarioCommand(
            'rumoca.scenario.getSimulationConfig',
            {
                model: selectedModel,
                fallback,
                ...(trimMaybeString(scenarioPath) ? { path: trimMaybeString(scenarioPath) } : {}),
            },
        );
        const effective = simulationConfig?.effective || normalizeSimulationSettingsCurrent(fallback);
        const resolvedScenarioPath = trimMaybeString(scenarioPath) || trimMaybeString(simulationConfig?.scenarioPath);
        const parameterOverrides = await getScenarioParameterOverrides(resolvedScenarioPath);
        const requestPayload = {
            source,
            modelName: selectedModel,
            solver: trimMaybeString(effective?.solver) || 'auto',
            tEnd: Number(effective?.tEnd) || 1.0,
            dt: Number(effective?.dt) || 0,
        };
        if (Object.keys(parameterOverrides).length > 0) {
            requestPayload.parameterOverrides = parameterOverrides;
        }
        const sourceRoots = captureWorkspaceSourceRootsJson(workspaceFs, effective?.sourceRootPaths);
        if (sourceRoots !== '{}') {
            requestPayload.sourceRoots = sourceRoots;
        }
        if (typeof workspaceSources === 'string') {
            requestPayload.workspaceSources = workspaceSources;
        }
        const raw = await requestScenarioCommand(
            'rumoca.scenario.startSimulation',
            requestPayload,
            timeoutMs,
        );
        const parsed = typeof raw === 'string' ? JSON.parse(raw) : raw;
        const payload = shared.normalizeSimulationPayload(parsed?.payload);
        if (!payload) {
            throw new Error('Simulation runtime returned an invalid results payload');
        }
        const metrics = shared.normalizeSimulationRunMetrics(parsed?.metrics);
        return {
            ok: true,
            model: selectedModel,
            effective,
            scenarioPath: resolvedScenarioPath,
            payload,
            ...(metrics ? { metrics } : {}),
        };
    }

    function setSelectedSimulationModel(model) {
        selectedSimulationModel = trimMaybeString(model);
        onWorkspaceMutation?.();
        return { ok: true, selectedModel: selectedSimulationModel || null };
    }

    async function execute(command, payload = {}) {
        switch (command) {
            case 'rumoca.scenario.getSimulationConfig':
            case 'rumoca.scenario.getScenarioConfig':
            case 'rumoca.scenario.getScenarioConfigFull':
            case 'rumoca.scenario.setScenarioConfig':
            case 'rumoca.scenario.setSimulationPreset':
            case 'rumoca.scenario.resetSimulationPreset':
            case 'rumoca.scenario.getVisualizationConfig':
            case 'rumoca.scenario.setVisualizationConfig':
            case 'rumoca.scenario.getCodegenConfig':
            case 'rumoca.scenario.setCodegenConfig':
            case 'rumoca.scenario.getSourceRoots':
            case 'rumoca.scenario.setSourceRoots':
                return await executeScenarioCommand(command, payload);
            case 'rumoca.scenario.defaultScenarioConfig': {
                const raw = await requestScenarioCommand(command, {
                    ...payload,
                    workspaceSources: captureWorkspaceSourcesJson(workspaceFs),
                });
                return typeof raw === 'string' ? JSON.parse(raw) : raw;
            }
            case 'rumoca.scenario.setSelectedSimulationModel':
                return setSelectedSimulationModel(payload?.model);
            case 'rumoca.scenario.getSimulationModels':
                return await getSimulationModels(payload);
            case 'rumoca.model.parameterMetadata':
                return await getParameterMetadata(payload);
            case 'rumoca.scenario.startSimulation':
                return await startSimulation(payload);
            default:
                return null;
        }
    }

    return {
        execute,
    };
}
