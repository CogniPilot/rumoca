import { preferredViewerScriptPathForModel } from './project_interface.js';

function sharedVisualization() {
    const shared = globalThis.RumocaVisualizationShared;
    if (!shared) {
        throw new Error('RumocaVisualizationShared not loaded');
    }
    return shared;
}

function sharedResultsApp() {
    const app = globalThis.RumocaResultsApp;
    if (!app) {
        throw new Error('RumocaResultsApp not loaded');
    }
    return app;
}

function trimMaybeString(value) {
    return typeof value === 'string' ? value.trim() : '';
}

function cloneJson(value) {
    return JSON.parse(JSON.stringify(value));
}

export function buildProjectVisualizationViewStorage({
    projectFs,
    defaultViewerScript,
}) {
    const shared = sharedVisualization();
    return shared.buildVisualizationViewStorageHandlers({
        resolveViewerScriptPath: (nextModel, viewId) => preferredViewerScriptPathForModel(nextModel, viewId),
        readTextFile: (scriptPath) => projectFs.getFileContent(scriptPath),
        writeTextFile: (scriptPath, content) => {
            projectFs.setFile(scriptPath, content);
        },
        removeTextFile: (scriptPath) => {
            projectFs.removeFile(scriptPath);
        },
        defaultViewerScript: typeof defaultViewerScript === 'function'
            ? defaultViewerScript
            : () => shared.defaultThreeDimensionalViewerScript(),
    });
}

export function createResultsPanelController({
    root,
    tabsRoot,
    projectFs,
    projectInterface,
    onStatus,
    onActivateRun = null,
    readPanelState,
    writePanelState,
}) {
    const shared = sharedVisualization();
    const appFactory = sharedResultsApp();
    const visualizationStorage = buildProjectVisualizationViewStorage({ projectFs });
    const resultsByModel = new Map();
    const LAST_RUN_ID = 'last_run';
    let activeModel = '';
    let renderVersion = 0;
    let mountedApp = null;
    let runSequence = 0;

    function setStatus(message, tone) {
        if (typeof onStatus === 'function') {
            onStatus(String(message || ''), tone);
        }
    }

    function currentActiveViewId() {
        const state = typeof readPanelState === 'function' ? readPanelState() : {};
        return trimMaybeString(state?.activeViewId) || undefined;
    }

    function readPersistedState() {
        const state = typeof readPanelState === 'function' ? readPanelState() : {};
        return {
            activeViewId: trimMaybeString(state?.activeViewId) || null,
            activeRunIdByModel:
                state?.activeRunIdByModel && typeof state.activeRunIdByModel === 'object'
                    ? { ...state.activeRunIdByModel }
                    : {},
            activeViewIdByRun:
                state?.activeViewIdByRun && typeof state.activeViewIdByRun === 'object'
                    ? { ...state.activeViewIdByRun }
                    : {},
        };
    }

    function persistPanelState(nextState) {
        const prev = typeof readPanelState === 'function' ? readPanelState() : {};
        if (typeof writePanelState === 'function') {
            writePanelState({
                ...prev,
                ...(nextState || {}),
            });
        }
    }

    function runStateKey(model, runId) {
        const nextModel = trimMaybeString(model);
        const nextRunId = trimMaybeString(runId);
        return nextModel && nextRunId ? `${nextModel}::${nextRunId}` : '';
    }

    function persistRunSelection(model, runId) {
        const nextModel = trimMaybeString(model);
        const nextRunId = trimMaybeString(runId);
        if (!nextModel) {
            return;
        }
        const nextState = readPersistedState();
        if (nextRunId) {
            nextState.activeRunIdByModel[nextModel] = nextRunId;
        } else {
            delete nextState.activeRunIdByModel[nextModel];
        }
        persistPanelState(nextState);
    }

    function persistRunViewState(model, runId, activeViewId) {
        const nextState = readPersistedState();
        const nextViewId = trimMaybeString(activeViewId) || null;
        nextState.activeViewId = nextViewId;
        const key = runStateKey(model, runId);
        if (key && nextViewId) {
            nextState.activeViewIdByRun[key] = nextViewId;
        } else if (key) {
            delete nextState.activeViewIdByRun[key];
        }
        persistPanelState(nextState);
    }

    function forgetRunState(model, runId) {
        const nextModel = trimMaybeString(model);
        const nextRunId = trimMaybeString(runId);
        if (!nextModel || !nextRunId) {
            return;
        }
        const nextState = readPersistedState();
        delete nextState.activeViewIdByRun[runStateKey(nextModel, nextRunId)];
        if (trimMaybeString(nextState.activeRunIdByModel[nextModel]) === nextRunId) {
            delete nextState.activeRunIdByModel[nextModel];
        }
        persistPanelState(nextState);
    }

    function createModelRunState() {
        return {
            runs: [],
            activeRunId: '',
            hydrated: false,
        };
    }

    function ensureModelRunState(model) {
        const nextModel = trimMaybeString(model);
        if (!nextModel) {
            return null;
        }
        let state = resultsByModel.get(nextModel);
        if (!state) {
            state = createModelRunState();
            resultsByModel.set(nextModel, state);
        }
        return state;
    }

    function chooseActiveRunId(model, modelState) {
        if (!modelState || !Array.isArray(modelState.runs) || modelState.runs.length === 0) {
            return '';
        }
        const persistedRunId = trimMaybeString(readPersistedState().activeRunIdByModel[model]);
        const candidates = [
            trimMaybeString(modelState.activeRunId),
            persistedRunId,
            trimMaybeString(modelState.runs.at(-1)?.id),
        ];
        for (const candidate of candidates) {
            if (candidate && modelState.runs.some((run) => run.id === candidate)) {
                return candidate;
            }
        }
        return '';
    }

    function currentRun(model, modelState) {
        const activeRunId = chooseActiveRunId(model, modelState);
        return modelState?.runs.find((run) => run.id === activeRunId) || null;
    }

    function formatRunLabel(model, createdAt) {
        const timeLabel = new Date(createdAt).toLocaleTimeString([], {
            hour: '2-digit',
            minute: '2-digit',
            second: '2-digit',
        });
        return `${trimMaybeString(model) || 'Model'} Results - ${timeLabel}`;
    }

    function createRunRecord(model, run, options = {}) {
        const createdAt = trimMaybeString(options.createdAt) || new Date().toISOString();
        return {
            id: trimMaybeString(options.id) || `run_${Date.now()}_${++runSequence}`,
            label: trimMaybeString(options.label) || formatRunLabel(model, createdAt),
            createdAt,
            payload: run?.payload ? cloneJson(run.payload) : null,
            metrics: run?.metrics ? cloneJson(run.metrics) : null,
            activeViewId: trimMaybeString(options.activeViewId || run?.activeViewId) || undefined,
        };
    }

    function appendEmptyRunTabs(message) {
        if (!tabsRoot) {
            return;
        }
        tabsRoot.innerHTML = '';
        const empty = document.createElement('div');
        empty.className = 'results-run-empty';
        empty.textContent = message;
        tabsRoot.appendChild(empty);
    }

    function closeRun(model, runId) {
        const nextModel = trimMaybeString(model);
        const nextRunId = trimMaybeString(runId);
        const modelState = resultsByModel.get(nextModel);
        if (!modelState || !nextRunId) {
            return;
        }
        const runIndex = modelState.runs.findIndex((run) => run.id === nextRunId);
        if (runIndex < 0) {
            return;
        }
        modelState.runs.splice(runIndex, 1);
        forgetRunState(nextModel, nextRunId);
        if (trimMaybeString(modelState.activeRunId) === nextRunId) {
            const fallbackRun = modelState.runs[runIndex] || modelState.runs[runIndex - 1] || null;
            modelState.activeRunId = fallbackRun?.id || '';
            persistRunSelection(nextModel, modelState.activeRunId);
        }
        if (nextModel === activeModel) {
            void render(nextModel);
        }
    }

    function renderRunTabs() {
        if (!tabsRoot) {
            return;
        }
        const nextModel = trimMaybeString(activeModel);
        const modelState = nextModel ? resultsByModel.get(nextModel) : null;
        const runs = Array.isArray(modelState?.runs) ? modelState.runs : [];
        if (!nextModel) {
            appendEmptyRunTabs('Run a simulation to open results tabs.');
            return;
        }
        if (runs.length === 0) {
            appendEmptyRunTabs('Run a simulation to open results tabs.');
            return;
        }

        const activeRunId = chooseActiveRunId(nextModel, modelState);
        tabsRoot.innerHTML = '';
        for (const run of runs) {
            const shell = document.createElement('div');
            shell.className = `results-run-tab-shell${run.id === activeRunId ? ' active' : ''}`;

            const tab = document.createElement('button');
            tab.type = 'button';
            tab.className = `results-run-tab${run.id === activeRunId ? ' active' : ''}`;
            tab.setAttribute('role', 'tab');
            tab.setAttribute('aria-selected', run.id === activeRunId ? 'true' : 'false');
            tab.title = run.label;
            tab.textContent = run.label;
            tab.addEventListener('click', () => {
                modelState.activeRunId = run.id;
                persistRunSelection(nextModel, run.id);
                onActivateRun?.('simulate', nextModel, run.id);
                void render(nextModel);
            });
            shell.appendChild(tab);

            const close = document.createElement('button');
            close.type = 'button';
            close.className = 'results-run-close';
            close.title = `Close ${run.label}`;
            close.setAttribute('aria-label', `Close ${run.label}`);
            close.textContent = '×';
            close.addEventListener('click', (event) => {
                event.preventDefault();
                event.stopPropagation();
                closeRun(nextModel, run.id);
            });
            shell.appendChild(close);
            tabsRoot.appendChild(shell);
        }
    }

    function disposeMountedApp() {
        if (mountedApp && typeof mountedApp.dispose === 'function') {
            mountedApp.dispose();
        }
        mountedApp = null;
    }

    function createBridge(model, runId) {
        const baseBridge = {
            notify(message) {
                setStatus(message, 'ok');
            },
            persistState(nextState) {
                const nextViewId = trimMaybeString(nextState?.activeViewId) || null;
                if (trimMaybeString(model) && trimMaybeString(runId)) {
                    const modelState = ensureModelRunState(model);
                    const run = modelState?.runs.find((candidate) => candidate.id === runId);
                    if (run) {
                        run.activeViewId = nextViewId || undefined;
                    }
                    persistRunViewState(model, runId, nextViewId);
                    return;
                }
                persistPanelState({
                    ...readPersistedState(),
                    activeViewId: nextViewId,
                });
            },
        };
        if (!trimMaybeString(model)) {
            return baseBridge;
        }

        return {
            ...baseBridge,
            async loadViews() {
                return await shared.loadHostedProjectResultsViews({
                    model,
                    loadConfiguredViews: ({ model: nextModel }) =>
                        projectInterface.execute('rumoca.project.getVisualizationConfig', {
                            model: nextModel,
                        }).views,
                    hydrateViews: async ({ model: nextModel, views }) =>
                        await visualizationStorage.hydrateViews({
                            views,
                            model: nextModel,
                        }),
                    defaultViews: shared.defaultVisualizationViews(),
                });
            },
            async saveViews(_ignored, nextViews) {
                return await shared.saveHostedProjectResultsViews({
                    model,
                    views: nextViews,
                    loadConfiguredViews: ({ model: nextModel }) =>
                        projectInterface.execute('rumoca.project.getVisualizationConfig', {
                            model: nextModel,
                        }).views,
                    persistViews: async ({ model: nextModel, views, previousViews }) => {
                        const persisted = await visualizationStorage.persistViews({
                            views,
                            model: nextModel,
                        });
                        await visualizationStorage.removeStaleViews({
                            previousViews,
                            nextViews: persisted,
                        });
                        return persisted;
                    },
                    writeConfiguredViews: ({ model: nextModel, views }) => {
                        projectInterface.execute('rumoca.project.setVisualizationConfig', {
                            model: nextModel,
                            views,
                        });
                        return true;
                    },
                    hydrateViews: async ({ model: nextModel, views }) =>
                        await visualizationStorage.hydrateViews({
                            views,
                            model: nextModel,
                        }),
                });
            },
            async resetViews() {
                return await shared.resetHostedProjectResultsViews({
                    model,
                    loadConfiguredViews: ({ model: nextModel }) =>
                        projectInterface.execute('rumoca.project.getVisualizationConfig', {
                            model: nextModel,
                        }).views,
                    removeViews: async ({ views }) =>
                        await visualizationStorage.removeViews({
                            views,
                        }),
                    writeConfiguredViews: ({ model: nextModel, views }) => {
                        projectInterface.execute('rumoca.project.setVisualizationConfig', {
                            model: nextModel,
                            views,
                        });
                        return true;
                    },
                    hydrateViews: async ({ model: nextModel, views }) =>
                        await visualizationStorage.hydrateViews({
                            views,
                            model: nextModel,
                        }),
                    defaultViews: shared.defaultVisualizationViews(),
                });
            },
        };
    }

    async function render(model) {
        const renderId = ++renderVersion;
        const nextModel = trimMaybeString(model);
        activeModel = nextModel;
        const modelState = nextModel ? ensureModelRunState(nextModel) : null;
        const restored = nextModel
            ? await shared.loadHostedSimulationRunWithViews({
                model: nextModel,
                readTextFile: (resultPath) => projectFs.getFileContent(resultPath),
                loadConfiguredViews: ({ model: targetModel }) =>
                    projectInterface.execute('rumoca.project.getVisualizationConfig', { model: targetModel }).views,
                hydrateViews: ({ model: targetModel, views }) =>
                    visualizationStorage.hydrateViews({
                        model: targetModel,
                        views,
                    }),
                defaultViews: shared.defaultVisualizationViews(),
            })
            : null;
        if (nextModel && modelState && !modelState.hydrated) {
            const persistedRun = restored?.run ?? null;
            if (persistedRun) {
                modelState.runs.push(createRunRecord(nextModel, persistedRun, {
                    id: LAST_RUN_ID,
                    label: `${nextModel} (last run)`,
                    activeViewId: currentActiveViewId(),
                }));
            }
            modelState.hydrated = true;
        }
        const hydratedViews = nextModel
            ? restored?.views ?? shared.defaultVisualizationViews()
            : shared.defaultVisualizationViews();

        if (renderId !== renderVersion) {
            return;
        }

        disposeMountedApp();
        const run = nextModel && modelState ? currentRun(nextModel, modelState) : null;
        if (nextModel && modelState) {
            modelState.activeRunId = trimMaybeString(run?.id);
            persistRunSelection(nextModel, modelState.activeRunId);
        }
        renderRunTabs();
        mountedApp = appFactory.createResultsApp({
            root,
            model: nextModel || 'Rumoca Results',
            modelRef: nextModel ? { model: nextModel } : undefined,
            payload: run?.payload ?? null,
            metrics: run?.metrics ?? null,
            views: hydratedViews,
            activeViewId: run?.activeViewId
                || (nextModel && run ? trimMaybeString(readPersistedState().activeViewIdByRun[runStateKey(nextModel, run.id)]) : '')
                || currentActiveViewId(),
            bridge: createBridge(nextModel, run?.id || ''),
        });
    }

    return {
        clear() {
            resultsByModel.clear();
            activeModel = '';
            disposeMountedApp();
            renderRunTabs();
        },
        dispose() {
            disposeMountedApp();
        },
        getSimulationRun(model) {
            const nextModel = trimMaybeString(model);
            const modelState = nextModel ? resultsByModel.get(nextModel) : null;
            return nextModel && modelState ? currentRun(nextModel, modelState) : null;
        },
        closeActiveRun(model) {
            const nextModel = trimMaybeString(model);
            const modelState = nextModel ? resultsByModel.get(nextModel) : null;
            const run = nextModel && modelState ? currentRun(nextModel, modelState) : null;
            if (!nextModel || !run) {
                return false;
            }
            closeRun(nextModel, run.id);
            return true;
        },
        async renderModel(model) {
            await render(model);
        },
        async reset() {
            resultsByModel.clear();
            renderRunTabs();
            await render('');
        },
        async setSimulationRun(model, run) {
            const nextModel = trimMaybeString(model);
            if (nextModel) {
                const modelState = ensureModelRunState(nextModel);
                const nextRun = createRunRecord(nextModel, run);
                modelState.hydrated = true;
                modelState.runs.push(nextRun);
                modelState.activeRunId = nextRun.id;
                persistRunSelection(nextModel, nextRun.id);
                await shared.persistHostedSimulationRunWithViews({
                    model: nextModel,
                    payload: nextRun.payload,
                    metrics: nextRun.metrics,
                    loadConfiguredViews: ({ model: targetModel }) =>
                        projectInterface.execute('rumoca.project.getVisualizationConfig', { model: targetModel }).views,
                    hydrateViews: ({ model: targetModel, views }) =>
                        visualizationStorage.hydrateViews({
                            views,
                            model: targetModel,
                        }),
                    defaultViews: shared.defaultVisualizationViews(),
                    pathExists: (candidatePath) => projectFs.getFileContent(candidatePath) !== null,
                    readTextFile: (candidatePath) => projectFs.getFileContent(candidatePath),
                    writeTextFile: (runPath, content) => {
                        projectFs.setFile(runPath, `${content}\n`);
                    },
                    writeLastResultTextFile: (resultPath, content) => {
                        projectFs.setFile(resultPath, `${content}\n`);
                    },
                });
            }
            await render(model);
        },
    };
}
