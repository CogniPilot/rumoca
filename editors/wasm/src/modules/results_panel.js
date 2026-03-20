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
    projectFs,
    projectInterface,
    onStatus,
    readPanelState,
    writePanelState,
}) {
    const shared = sharedVisualization();
    const appFactory = sharedResultsApp();
    const visualizationStorage = buildProjectVisualizationViewStorage({ projectFs });
    const resultsByModel = new Map();
    let activeModel = '';
    let renderVersion = 0;
    let mountedApp = null;

    function setStatus(message, tone) {
        if (typeof onStatus === 'function') {
            onStatus(String(message || ''), tone);
        }
    }

    function currentActiveViewId() {
        const state = typeof readPanelState === 'function' ? readPanelState() : {};
        return trimMaybeString(state?.activeViewId) || undefined;
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

    function disposeMountedApp() {
        if (mountedApp && typeof mountedApp.dispose === 'function') {
            mountedApp.dispose();
        }
        mountedApp = null;
    }

    function createBridge(model) {
        const baseBridge = {
            notify(message) {
                setStatus(message, 'ok');
            },
            persistState(nextState) {
                persistPanelState(nextState);
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
        const persistedRun = restored?.run ?? null;
        const run = nextModel ? resultsByModel.get(nextModel) || persistedRun || null : null;
        if (nextModel && run && !resultsByModel.has(nextModel)) {
            resultsByModel.set(nextModel, cloneJson(run));
        }
        const hydratedViews = nextModel
            ? restored?.views ?? shared.defaultVisualizationViews()
            : shared.defaultVisualizationViews();

        if (renderId !== renderVersion) {
            return;
        }

        disposeMountedApp();
        mountedApp = appFactory.createResultsApp({
            root,
            model: nextModel || 'Rumoca Results',
            modelRef: nextModel ? { model: nextModel } : undefined,
            payload: run?.payload ?? null,
            metrics: run?.metrics ?? null,
            views: hydratedViews,
            activeViewId: currentActiveViewId(),
            bridge: createBridge(nextModel),
        });
    }

    return {
        clear() {
            resultsByModel.clear();
            activeModel = '';
            disposeMountedApp();
        },
        dispose() {
            disposeMountedApp();
        },
        getSimulationRun(model) {
            return resultsByModel.get(model) || null;
        },
        async renderModel(model) {
            await render(model);
        },
        async reset() {
            resultsByModel.clear();
            await render('');
        },
        async setSimulationRun(model, run) {
            if (trimMaybeString(model)) {
                resultsByModel.set(model, cloneJson(run));
                await shared.persistHostedSimulationRunWithViews({
                    model,
                    payload: run?.payload,
                    metrics: run?.metrics,
                    loadConfiguredViews: ({ model: targetModel }) =>
                        projectInterface.execute('rumoca.project.getVisualizationConfig', { model: targetModel }).views,
                    hydrateViews: ({ model: targetModel, views }) =>
                        visualizationStorage.hydrateViews({
                            views,
                            model: targetModel,
                        }),
                    defaultViews: shared.defaultVisualizationViews(),
                    pathExists: (candidatePath) => projectFs.getFileContent(candidatePath) !== null,
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
