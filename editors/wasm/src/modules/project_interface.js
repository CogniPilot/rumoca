function sharedVisualization() {
    const shared = globalThis.RumocaVisualizationShared;
    if (!shared) {
        throw new Error('RumocaVisualizationShared not loaded');
    }
    return shared;
}

function cloneJson(value) {
    return value === undefined ? undefined : JSON.parse(JSON.stringify(value));
}

function trimMaybeString(value) {
    return typeof value === 'string' ? value.trim() : '';
}

function captureHostedProjectSnapshot(projectFs) {
    return {
        files: projectFs.listFiles()
            .filter((file) => String(file.path || '').startsWith('.rumoca/') && typeof file.content === 'string')
            .map((file) => ({
                path: file.path,
                content: file.content,
            })),
        editorState: cloneJson(projectFs.getEditorState() || {}),
    };
}

function applyHostedProjectPatch(projectFs, response) {
    const parsed = response && typeof response === 'object' ? response : {};
    const writes = Array.isArray(parsed.writes) ? parsed.writes : [];
    const removes = Array.isArray(parsed.removes) ? parsed.removes : [];

    for (const write of writes) {
        const path = trimMaybeString(write?.path);
        if (!path) {
            continue;
        }
        projectFs.setFile(path, typeof write?.content === 'string' ? write.content : '');
    }
    for (const path of removes) {
        const normalized = trimMaybeString(path);
        if (normalized) {
            projectFs.removeFile(normalized);
        }
    }
    if (Object.prototype.hasOwnProperty.call(parsed, 'editorState')) {
        projectFs.setEditorState(cloneJson(parsed.editorState || {}));
    }
    return parsed.result ?? parsed;
}

export function preferredViewerScriptPathForModel(model, viewId) {
    return sharedVisualization().preferredViewerScriptPathForModel(model, viewId);
}

export function createProjectInterface({ projectFs, runtimeBridge = null, onProjectMutation = null }) {
    function readSelectedSimulationModel() {
        return trimMaybeString(projectFs.getEditorState()?.selectedSimulationModel);
    }

    function executeHostedProjectSidecarCommand(command, payload = {}) {
        const result = applyHostedProjectPatch(
            projectFs,
            sharedVisualization().executeHostedProjectSidecarCommand(
                command,
                captureHostedProjectSnapshot(projectFs),
                payload,
            ),
        );
        onProjectMutation?.();
        return result;
    }

    async function requestRuntime(action, payload = {}, timeoutMs) {
        if (!runtimeBridge || typeof runtimeBridge.request !== 'function') {
            throw new Error(`Runtime bridge unavailable for ${action}`);
        }
        return await runtimeBridge.request(action, payload, timeoutMs);
    }

    async function requestProjectCommand(command, payload = {}, timeoutMs) {
        return await requestRuntime(
            'projectCommand',
            {
                command,
                payload,
            },
            timeoutMs,
        );
    }

    async function getSimulationModels({ source, defaultModel }) {
        const preferredModel = readSelectedSimulationModel() || trimMaybeString(defaultModel);
        const raw = await requestProjectCommand('rumoca.project.getSimulationModels', {
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
        executeHostedProjectSidecarCommand('rumoca.project.setSelectedSimulationModel', {
            model: selectedModel,
        });
        return {
            ok: parsed?.ok !== false,
            models,
            selectedModel: selectedModel || null,
            error: trimMaybeString(parsed?.error) || null,
        };
    }

    async function startSimulation({ source, model, fallback, timeoutMs, projectSources = '{}' }) {
        const selectedModel = trimMaybeString(model) || readSelectedSimulationModel();
        if (!selectedModel) {
            throw new Error('No simulation model selected');
        }
        executeHostedProjectSidecarCommand('rumoca.project.setSelectedSimulationModel', {
            model: selectedModel,
        });
        const simulationConfig = executeHostedProjectSidecarCommand(
            'rumoca.project.getSimulationConfig',
            {
                model: selectedModel,
                fallback,
            },
        );
        const effective = simulationConfig?.effective || sharedVisualization().normalizeHostedSimulationSettingsCurrent(fallback);
        const raw = await requestProjectCommand(
            'rumoca.project.startSimulation',
            {
                source,
                modelName: selectedModel,
                projectSources,
                solver: trimMaybeString(effective?.solver) || 'auto',
                tEnd: Number(effective?.tEnd) || 1.0,
                dt: Number(effective?.dt) || 0,
            },
            timeoutMs,
        );
        const parsed = typeof raw === 'string' ? JSON.parse(raw) : raw;
        const payload = sharedVisualization().normalizeSimulationPayload(parsed?.payload);
        if (!payload) {
            throw new Error('Simulation runtime returned an invalid results payload');
        }
        const metrics = sharedVisualization().normalizeSimulationRunMetrics(parsed?.metrics);
        return {
            ok: true,
            model: selectedModel,
            effective,
            payload,
            ...(metrics ? { metrics } : {}),
        };
    }

    function execute(command, payload = {}) {
        switch (command) {
            case 'rumoca.project.getSimulationConfig':
            case 'rumoca.project.setSimulationPreset':
            case 'rumoca.project.resetSimulationPreset':
            case 'rumoca.project.getVisualizationConfig':
            case 'rumoca.project.setVisualizationConfig':
            case 'rumoca.project.setSelectedSimulationModel':
                return executeHostedProjectSidecarCommand(command, payload);
            case 'rumoca.project.getSimulationModels':
                return getSimulationModels(payload);
            case 'rumoca.project.startSimulation':
                return startSimulation(payload);
            default:
                return null;
        }
    }

    return {
        execute,
    };
}
