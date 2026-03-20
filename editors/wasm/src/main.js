import { setupCommandPalette } from './modules/command_palette.js';
import { createSourceBreadcrumbs } from './modules/breadcrumbs.js';
import { createLibraryDocsController } from './modules/library_docs.js';
import { createDiagnosticsController } from './modules/diagnostics_panel.js';
import { installFileActions } from './modules/file_actions.js';
import { setupMonacoWorkspace } from './modules/monaco_setup.js';
import { createProjectInterface } from './modules/project_interface.js';
import {
    buildProjectVisualizationViewStorage,
    createResultsPanelController,
} from './modules/results_panel.js';
import {
    createProjectFilesystem,
    inferModelicaFileName,
} from './modules/project_fs.js';

let editor;
const projectFs = createProjectFilesystem();
const projectInterface = createProjectInterface({
    projectFs,
    runtimeBridge: {
        request(action, payload = {}, timeoutMs) {
            return sendRequest(action, payload, timeoutMs);
        },
    },
});
let simResultsPanelState = { activeViewId: null };
let suspendWorkspaceObservers = false;
let defaultProjectSeed = null;
const simulationSettingsModal = document.getElementById('simulationSettingsModal');
const simulationSettingsFrame = document.getElementById('simulationSettingsFrame');
const simulationSettingsCloseBtn = document.getElementById('simulationSettingsCloseBtn');
const projectVisualizationStorage = buildProjectVisualizationViewStorage({ projectFs });

function trimMaybeString(value) {
    return typeof value === 'string' ? value.trim() : '';
}

function sharedVisualization() {
    const shared = globalThis.RumocaVisualizationShared;
    if (!shared) {
        throw new Error('RumocaVisualizationShared not loaded');
    }
    return shared;
}

const resultsPanelController = createResultsPanelController({
    root: document.getElementById('simPlot'),
    projectFs,
    projectInterface,
    onStatus(message, tone) {
        const statusEl = document.getElementById('simStatus');
        if (!statusEl) return;
        statusEl.textContent = String(message || '');
        if (tone === 'error') {
            statusEl.style.color = '#c9184a';
        } else if (tone === 'ok') {
            statusEl.style.color = '#2d6a4f';
        } else {
            statusEl.style.color = '#888';
        }
    },
    readPanelState() {
        return simResultsPanelState;
    },
    writePanelState(nextState) {
        simResultsPanelState = {
            ...simResultsPanelState,
            ...(nextState || {}),
        };
    },
});
void resultsPanelController.renderModel('');

// Panel references
const resizeHandleH = document.getElementById('resizeHandleH');
const leftArea = document.querySelector('.left-area');
let isResizingH = false;

function isNarrowLayout() {
    return window.innerWidth <= 980;
}

// Horizontal resize between left area and right panel
resizeHandleH.addEventListener('mousedown', (e) => {
    if (isNarrowLayout()) return;
    isResizingH = true;
    document.body.style.cursor = 'ew-resize';
    document.body.style.userSelect = 'none';
});

const layoutAllEditors = () => {
    if (window.editor) window.editor.layout();
    if (window.templateEditor) window.templateEditor.layout();
    if (window.outputEditor) window.outputEditor.layout();
    if (window.codegenOutputEditor) window.codegenOutputEditor.layout();
};

function syncResponsiveLayoutState() {
    if (!leftArea) return;
    if (isNarrowLayout()) {
        leftArea.style.width = '';
        leftArea.style.flex = '1 1 auto';
    } else if (leftArea.style.flex === '1 1 auto') {
        leftArea.style.flex = '1';
    }
}

window.addEventListener('resize', syncResponsiveLayoutState);
syncResponsiveLayoutState();

document.addEventListener('mousemove', (e) => {
    if (isResizingH) {
        if (isNarrowLayout()) {
            isResizingH = false;
            document.body.style.cursor = '';
            document.body.style.userSelect = '';
            return;
        }
        const containerRect = document.querySelector('.main-container').getBoundingClientRect();
        const newWidth = e.clientX - containerRect.left;
        leftArea.style.flex = 'none';
        leftArea.style.width = Math.max(200, newWidth) + 'px';
        layoutAllEditors();
    }
    if (isResizingV) {
        const leftAreaEl = document.querySelector('.left-area');
        const leftAreaRect = leftAreaEl.getBoundingClientRect();
        const newHeight = leftAreaRect.bottom - e.clientY;
        const clampedHeight = Math.max(100, Math.min(newHeight, window.innerHeight * 0.5));
        bottomPanel.style.height = clampedHeight + 'px';
        if (window.editor) window.editor.layout();
    }
});

document.addEventListener('mouseup', () => {
    if (isResizingH || isResizingV) {
        isResizingH = false;
        isResizingV = false;
        document.body.style.cursor = '';
        document.body.style.userSelect = '';
        layoutAllEditors();
    }
});

// Bottom panel resize (vertical)
const resizeHandleV = document.getElementById('resizeHandleV');
const bottomPanel = document.getElementById('bottomPanel');
let isResizingV = false;

resizeHandleV.addEventListener('mousedown', (e) => {
    isResizingV = true;
    document.body.style.cursor = 'ns-resize';
    document.body.style.userSelect = 'none';
});

// Active right tab state
window.activeRightTab = 'simulate';
window.activeBottomTab = 'output';

function normalizeTabName(tabName, allowed, fallback) {
    return allowed.includes(tabName) ? tabName : fallback;
}

function setBottomPanelCollapsed(collapsed) {
    bottomPanel.classList.toggle('collapsed', Boolean(collapsed));
    const arrow = document.getElementById('bottomArrow');
    if (!arrow) return;
    arrow.innerHTML = bottomPanel.classList.contains('collapsed') ? '&#9650;' : '&#9660;';
}

// Switch between right panel tabs (Simulate / DAE / Codegen)
window.switchRightTab = function(tabName) {
    window.activeRightTab = tabName;
    // Update tab buttons
    document.querySelectorAll('.right-tab-bar .right-tab').forEach(tab => {
        tab.classList.toggle('active', tab.dataset.tab === tabName);
    });
    // Update tab content
    document.getElementById('simTab').classList.toggle('active', tabName === 'simulate');
    document.getElementById('daeTab').classList.toggle('active', tabName === 'dae');
    document.getElementById('codegenTab').classList.toggle('active', tabName === 'codegen');
    // Refresh editors in the newly visible tab
    setTimeout(() => {
        layoutAllEditors();
        // If switching to DAE or Codegen, refresh the output
        const modelName = document.getElementById('modelSelect').value;
        if (modelName && window.compiledModels && window.compiledModels[modelName]) {
            if (tabName === 'dae') {
                displayDaeOutput(modelName);
            } else if (tabName === 'codegen') {
                displayCodegenOutput(modelName);
            }
        }
        if (tabName === 'simulate') {
            void resultsPanelController.renderModel(window.selectedModel || '');
        }
    }, 0);
};

// Toggle bottom panel collapse/expand (collapses down)
window.toggleBottomPanel = function() {
    setBottomPanelCollapsed(!bottomPanel.classList.contains('collapsed'));
    if (window.editor) window.editor.layout();
};

// Switch between bottom panel tabs
window.switchBottomTab = function(tabName) {
    window.activeBottomTab = tabName;
    // Update tab buttons (only within bottom panel)
    document.querySelectorAll('.panel-header-bottom .bottom-tab').forEach(tab => {
        tab.classList.toggle('active', tab.dataset.tab === tabName);
    });
    // Update sections
    document.getElementById('outputSection').classList.toggle('active', tabName === 'output');
    document.getElementById('errorsSection').classList.toggle('active', tabName === 'errors');
    document.getElementById('librariesSection').classList.toggle('active', tabName === 'libraries');
    document.getElementById('packagesSection').classList.toggle('active', tabName === 'packages');
    if (tabName === 'packages' && typeof window.refreshPackageViewer === 'function') {
        window.refreshPackageViewer();
    }
};

function collectProjectEditorState() {
    return {
        rightTab: String(window.activeRightTab || 'simulate'),
        bottomTab: String(window.activeBottomTab || 'output'),
        bottomPanelCollapsed: bottomPanel.classList.contains('collapsed'),
        template: window.templateEditor?.getValue?.() || '',
        simResultsActiveViewId: trimMaybeString(simResultsPanelState.activeViewId) || null,
    };
}

function applyProjectEditorState(editorState) {
    const fallbackState = defaultProjectSeed?.editorState || {
        rightTab: 'simulate',
        bottomTab: 'output',
        bottomPanelCollapsed: false,
        template: '',
    };
    const nextState = editorState && typeof editorState === 'object'
        ? {
            ...fallbackState,
            ...editorState,
        }
        : fallbackState;
    const rightTab = normalizeTabName(
        String(nextState.rightTab || fallbackState.rightTab),
        ['simulate', 'dae', 'codegen'],
        'simulate',
    );
    const bottomTab = normalizeTabName(
        String(nextState.bottomTab || fallbackState.bottomTab),
        ['output', 'errors', 'libraries', 'packages'],
        'output',
    );

    if (window.templateEditor?.setValue) {
        const template = String(nextState.template || fallbackState.template || '');
        if (window.templateEditor.getValue() !== template) {
            window.templateEditor.setValue(template);
        }
    }

    simResultsPanelState = {
        activeViewId: trimMaybeString(nextState.simResultsActiveViewId)
            || trimMaybeString(fallbackState.simResultsActiveViewId)
            || null,
    };

    setBottomPanelCollapsed(Boolean(nextState.bottomPanelCollapsed));
    window.switchRightTab(rightTab);
    window.switchBottomTab(bottomTab);
}

function buildNewProjectState() {
    const seed = defaultProjectSeed || {
        activeDocumentPath: 'Main.mo',
        activeDocumentContent: 'model Main\nend Main;\n',
        editorState: {
            rightTab: 'simulate',
            bottomTab: 'output',
            bottomPanelCollapsed: false,
            template: '',
            simResultsActiveViewId: null,
        },
    };
    projectFs.clearProject();
    projectFs.setActiveDocument(seed.activeDocumentPath, seed.activeDocumentContent);
    projectFs.setEditorState(seed.editorState);
    return {
        activeDocumentPath: projectFs.getActiveDocumentPath(),
        activeDocumentContent: projectFs.getActiveDocumentContent(),
        editorState: projectFs.getEditorState(),
        libraryArchives: [],
        fileCount: projectFs.listFiles().length,
    };
}

function projectSimulationFallback() {
    return {
        solver: 'auto',
        tEnd: 10.0,
        dt: null,
        outputDir: '',
        modelicaPath: [],
    };
}

async function getSimulationModelState(source, defaultModel = '') {
    const state = await projectInterface.execute('rumoca.project.getSimulationModels', {
        source,
        defaultModel,
    });
    if (!state || state.ok === false) {
        return {
            ok: false,
            models: [],
            selectedModel: null,
            error: state?.error || 'Failed to discover simulation models',
        };
    }
    return state;
}

function listSimulationModels() {
    const modelSelect = document.getElementById('modelSelect');
    if (!modelSelect) {
        return [];
    }
    return Array.from(modelSelect.options)
        .map((option) => trimMaybeString(option.value))
        .filter(Boolean);
}

function currentSimulationModel() {
    return trimMaybeString(document.getElementById('modelSelect')?.value || '');
}

function simulationSettingsFeatures() {
    return {
        addLibraryPath: false,
        prepareModels: false,
        resyncSidecars: false,
        workspaceSettings: false,
        userSettings: false,
        openViewScript: false,
    };
}

function simulationViewsForModel(model) {
    const config = projectInterface.execute('rumoca.project.getVisualizationConfig', { model });
    return Array.isArray(config?.views) ? config.views : [];
}

function buildSimulationSettingsDocument(model) {
    const config = projectInterface.execute('rumoca.project.getSimulationConfig', {
        model,
        fallback: projectSimulationFallback(),
    });
    return sharedVisualization().buildHostedSimulationSettingsDocument(
        sharedVisualization().buildHostedSimulationSettingsState({
            activeModel: model,
            availableModels: listSimulationModels(),
            current: config?.effective,
            fallbackCurrent: projectSimulationFallback(),
            views: simulationViewsForModel(model),
            defaultViews: [],
            features: simulationSettingsFeatures(),
        }),
    );
}

function isSimulationSettingsModalOpen() {
    return Boolean(simulationSettingsModal) && !simulationSettingsModal.hidden;
}

async function renderSimulationSettingsModal(preferredModel = '') {
    if (!simulationSettingsFrame) {
        return;
    }
    const model = trimMaybeString(preferredModel)
        || currentSimulationModel()
        || listSimulationModels()[0]
        || 'Model';
    simulationSettingsFrame.srcdoc = buildSimulationSettingsDocument(model);
}

function refreshSimulationSettingsModalIfOpen() {
    if (!isSimulationSettingsModalOpen()) {
        return;
    }
    void renderSimulationSettingsModal();
}

async function saveSimulationSettingsForModel(model, preset, views) {
    return await sharedVisualization().saveHostedProjectSimulationSettings({
        model,
        preset,
        views,
        loadViews: ({ model: nextModel }) => simulationViewsForModel(nextModel),
        persistViews: async ({ model: nextModel, views: nextViews }) =>
            await projectVisualizationStorage.persistViews({
                views: nextViews,
                model: nextModel,
            }),
        removeStaleViews: async ({ previousViews, nextViews }) =>
            await projectVisualizationStorage.removeStaleViews({
                previousViews,
                nextViews,
            }),
        writeViews: ({ model: nextModel, views: nextViews }) => {
            projectInterface.execute('rumoca.project.setVisualizationConfig', {
                model: nextModel,
                views: nextViews,
            });
            return true;
        },
        writePreset: ({ model: nextModel, preset: nextPreset }) => {
            projectInterface.execute('rumoca.project.setSimulationPreset', {
                model: nextModel,
                preset: nextPreset,
            });
            return true;
        },
        afterSave: async () => {
            await resultsPanelController.renderModel(window.selectedModel || '');
        },
    });
}

async function resetSimulationSettingsForModel(model) {
    return await sharedVisualization().resetHostedProjectSimulationSettings({
        model,
        loadViews: ({ model: nextModel }) => simulationViewsForModel(nextModel),
        removeViews: async ({ views }) =>
            await projectVisualizationStorage.removeViews({
                views,
            }),
        resetPreset: ({ model: nextModel }) => {
            projectInterface.execute('rumoca.project.resetSimulationPreset', { model: nextModel });
            return true;
        },
        writeViews: ({ model: nextModel, views }) => {
            projectInterface.execute('rumoca.project.setVisualizationConfig', {
                model: nextModel,
                views,
            });
            return true;
        },
        readCurrent: ({ model: nextModel }) =>
            projectInterface.execute('rumoca.project.getSimulationConfig', {
                model: nextModel,
                fallback: projectSimulationFallback(),
            })?.effective,
        readViews: () => [],
        afterReset: async () => {
            await resultsPanelController.renderModel(window.selectedModel || '');
        },
    });
}

function openSimulationSettingsModal() {
    if (!simulationSettingsModal) {
        return;
    }
    simulationSettingsModal.hidden = false;
    void renderSimulationSettingsModal();
}

function closeSimulationSettingsModal() {
    if (!simulationSettingsModal) {
        return;
    }
    simulationSettingsModal.hidden = true;
    if (simulationSettingsFrame) {
        simulationSettingsFrame.srcdoc = 'about:blank';
    }
}

window.openSimulationSettingsModal = openSimulationSettingsModal;

const simulationSettingsHandlers = sharedVisualization().buildHostedSimulationSettingsHandlers({
    getActiveModel: () => currentSimulationModel() || listSimulationModels()[0] || 'Model',
    save: async ({ model, preset, views }) =>
        await saveSimulationSettingsForModel(model, preset, views),
    reset: async ({ model }) =>
        await resetSimulationSettingsForModel(model),
    selectModel: async ({ model }) => {
        const modelSelect = document.getElementById('modelSelect');
        if (modelSelect) {
            modelSelect.value = model;
        }
        if (typeof window.updateSelectedModel === 'function') {
            window.updateSelectedModel();
        }
        return { model };
    },
    afterOpenModel: async ({ model }) => {
        await renderSimulationSettingsModal(model);
    },
});

globalThis.RumocaSimulationSettingsHost = {
    async request(method, payload = {}) {
        const handler = simulationSettingsHandlers[method];
        if (typeof handler !== 'function') {
            throw new Error(`Unsupported settings request: ${method}`);
        }
        return await handler({ method, payload });
    },
};

simulationSettingsCloseBtn?.addEventListener('click', () => {
    closeSimulationSettingsModal();
});
simulationSettingsModal?.addEventListener('click', (event) => {
    if (event.target === simulationSettingsModal) {
        closeSimulationSettingsModal();
    }
});

function parseBadgeNumber(text) {
    const match = String(text || '').match(/\d+/);
    return match ? Number(match[0]) : 0;
}

function updateGlobalStatusBar() {
    const modelSelect = document.getElementById('modelSelect');
    const diagnosticsBadge = document.getElementById('diagnosticsCount');
    const compileStatus = document.getElementById('autoCompileStatus');
    const libraryBadge = document.getElementById('libCount');

    const modelLabel = document.getElementById('statusBarModel');
    const problemsLabel = document.getElementById('statusBarProblems');
    const compileLabel = document.getElementById('statusBarCompile');
    const librariesLabel = document.getElementById('statusBarLibraries');
    if (!modelLabel || !problemsLabel || !compileLabel || !librariesLabel) return;

    const selectedModel = String(modelSelect?.value || '').trim();
    const problemText = String(diagnosticsBadge?.textContent || '').trim();
    const compileText = String(compileStatus?.textContent || '').trim();
    const libraryText = String(libraryBadge?.textContent || '').trim();

    modelLabel.textContent = `Model: ${selectedModel || '--'}`;
    problemsLabel.textContent = `Problems: ${problemText || parseBadgeNumber(problemText)}`;
    compileLabel.textContent = `Compile: ${compileText || 'idle'}`;
    librariesLabel.textContent = `Libraries: ${parseBadgeNumber(libraryText)}`;
}

function bindStatusBarObservers() {
    const idsToWatch = ['diagnosticsCount', 'autoCompileStatus', 'libCount'];
    const observer = new MutationObserver(() => updateGlobalStatusBar());
    for (const id of idsToWatch) {
        const element = document.getElementById(id);
        if (!element) continue;
        observer.observe(element, {
            childList: true,
            subtree: true,
            characterData: true,
            attributes: true,
        });
    }

    const modelSelect = document.getElementById('modelSelect');
    if (modelSelect) {
        modelSelect.addEventListener('change', () => updateGlobalStatusBar());
    }
}

bindStatusBarObservers();
window.updateGlobalStatusBar = updateGlobalStatusBar;
setTimeout(() => updateGlobalStatusBar(), 0);

// DAE format state (Pretty vs JSON in DAE tab)
window.daeFormat = 'pretty';

// Update DAE format toggle
window.updateDaeFormat = function() {
    window.daeFormat = document.getElementById('daeFormatSelect').value;
    const modelName = document.getElementById('modelSelect').value;
    if (modelName && window.compiledModels && window.compiledModels[modelName]) {
        displayDaeOutput(modelName);
    }
};

window.runSimulation = async function() {
    const modelName = document.getElementById('modelSelect').value;
    if (!modelName) {
        document.getElementById('simStatus').textContent = 'No model selected';
        return;
    }
    const simulationConfig = projectInterface.execute('rumoca.project.getSimulationConfig', {
        model: modelName,
        fallback: projectSimulationFallback(),
    });
    const tEnd = Number(simulationConfig.effective?.tEnd) || 1.0;
    const dt = Number(simulationConfig.effective?.dt) || 0;
    const source = window.editor ? window.editor.getValue() : '';
    const btn = document.getElementById('simRunBtn');
    const status = document.getElementById('simStatus');

    btn.disabled = true;
    status.textContent = 'Simulating...';
    status.style.color = '#9a6700';

    try {
        const result = await projectInterface.execute('rumoca.project.startSimulation', {
            source,
            model: modelName,
            fallback: projectSimulationFallback(),
            timeoutMs: 60000,
        });
        const simulateMs = Math.round((Number(result.metrics?.simulateSeconds) || 0) * 1000);
        status.textContent =
            `${result.metrics?.points ?? 0} pts, ${result.metrics?.variables ?? 0} vars (${simulateMs}ms)`;
        status.style.color = '#2d6a4f';
        await resultsPanelController.setSimulationRun(modelName, {
            payload: result.payload,
            ...(result.metrics ? { metrics: result.metrics } : {}),
        });
    } catch (e) {
        status.textContent = e.message || 'Simulation failed';
        status.style.color = '#c9184a';
    } finally {
        btn.disabled = false;
    }
};

// Display output for the active tab
function displayModelOutput(modelName) {
    if (window.activeRightTab === 'dae') {
        displayDaeOutput(modelName);
    } else if (window.activeRightTab === 'codegen') {
        displayCodegenOutput(modelName);
    }
}

// Display DAE output (Pretty or JSON)
function displayDaeOutput(modelName) {
    const result = window.compiledModels[modelName];
    if (!result) { setDaeOutput(`No compilation result for ${modelName}`); return; }
    if (result.error) { setDaeOutput(String(result.error || 'compile error')); return; }
    if (!result.dae) { setDaeOutput(`No DAE available for ${modelName}`); return; }

    if (window.daeFormat === 'pretty') {
        const prettyOutput = result.pretty || '';
        setDaeOutput(prettyOutput || JSON.stringify(result.dae, null, 2));
    } else {
        if (result.dae_native) {
            setDaeOutput(JSON.stringify(result.dae_native, null, 2), 'json');
        } else {
            setDaeOutput('DAE JSON not available');
        }
    }
}

// Display codegen output (render template)
async function displayCodegenOutput(modelName) {
    const result = window.compiledModels[modelName];
    if (!result || result.error || !result.dae) {
        setCodegenOutput(result?.error ? '' : 'No DAE available');
        return;
    }
    const template = window.templateEditor ? window.templateEditor.getValue() : '';
    if (!template.trim()) {
        setCodegenOutput('Enter or select a template to render output.');
        clearTemplateErrors();
        return;
    }
    if (!result.dae_native) {
        setCodegenOutput('Native DAE not available for template rendering.');
        clearTemplateErrors();
        return;
    }
    try {
        const daeJson = JSON.stringify(result.dae_native);
        const rendered = await sendWorkspaceCommand('rumoca.workspace.renderTemplate', {
            daeJson,
            template,
        });
        setCodegenOutput(rendered);
        clearTemplateErrors();
    } catch (e) {
        setCodegenOutput('');
        showTemplateError(e.message);
    }
}

// ANSI to HTML converter
function ansiToHtml(text) {
    text = text.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    const colorMap = {
        '30': 'ansi-black', '31': 'ansi-red', '32': 'ansi-green', '33': 'ansi-yellow',
        '34': 'ansi-blue', '35': 'ansi-magenta', '36': 'ansi-cyan', '37': 'ansi-white',
        '90': 'ansi-bright-black', '91': 'ansi-bright-red', '92': 'ansi-bright-green',
        '93': 'ansi-bright-yellow', '94': 'ansi-bright-blue', '95': 'ansi-bright-magenta',
        '96': 'ansi-bright-cyan', '97': 'ansi-bright-white'
    };
    let result = '';
    let openSpans = 0;
    const regex = /\x1b\[([0-9;]*)m|\[([0-9;]*)m/g;
    let lastIndex = 0;
    let match;
    while ((match = regex.exec(text)) !== null) {
        result += text.substring(lastIndex, match.index);
        lastIndex = regex.lastIndex;
        const codes = (match[1] || match[2] || '0').split(';');
        for (const code of codes) {
            if (code === '0' || code === '39' || code === '') {
                while (openSpans > 0) { result += '</span>'; openSpans--; }
            } else if (code === '1') {
                result += '<span class="ansi-bold">'; openSpans++;
            } else if (colorMap[code]) {
                result += `<span class="${colorMap[code]}">`; openSpans++;
            }
        }
    }
    result += text.substring(lastIndex);
    while (openSpans > 0) { result += '</span>'; openSpans--; }
    return result;
}

// Set DAE output (DAE tab) - uses Monaco editor
function setDaeOutput(text, language) {
    if (window.outputEditor) {
        const lang = language || (window.daeFormat === 'json' ? 'json' : 'plaintext');
        const model = window.outputEditor.getModel();
        if (model) {
            monaco.editor.setModelLanguage(model, lang);
        }
        window.outputEditor.setValue(text || '');
    }
}

// Set codegen output (Codegen tab) - uses Monaco editor
function setCodegenOutput(text, language) {
    if (window.codegenOutputEditor) {
        const lang = language || 'plaintext';
        const model = window.codegenOutputEditor.getModel();
        if (model) {
            monaco.editor.setModelLanguage(model, lang);
        }
        window.codegenOutputEditor.setValue(text || '');
    }
}

// Set terminal output (bottom panel Output tab)
function setTerminalOutput(text) {
    document.getElementById('terminalOutput').innerHTML = ansiToHtml(text);
}

let diagnosticsController = null;

function updateCompileErrors(errors) {
    if (!diagnosticsController) return;
    diagnosticsController.updateCompileErrors(errors);
}

function showTemplateError(message) {
    if (!diagnosticsController) return;
    diagnosticsController.showTemplateError(message);
}

function clearTemplateErrors() {
    if (!diagnosticsController) return;
    diagnosticsController.clearTemplateErrors();
}

function updateDiagnostics(diagnostics) {
    if (!diagnosticsController) return;
    diagnosticsController.updateModelicaDiagnostics(diagnostics);
}

function normalizeDiagnosticsPayload(payload, sourceText) {
    if (!diagnosticsController) return [];
    return diagnosticsController.normalizeDiagnosticsPayload(payload, sourceText);
}

function diagnosticCodeString(diagnostic) {
    if (!diagnosticsController) return '';
    return diagnosticsController.diagnosticCodeString(diagnostic);
}

function navigateProblems(step) {
    if (!diagnosticsController) return;
    diagnosticsController.navigateProblems(step);
}

// Pretty print DAE IR for human-readable display
function formatExpr(expr) {
    if (!expr) return '?';
    if (typeof expr === 'number') return String(expr);
    if (typeof expr === 'string') return expr;
    if (expr.Real !== undefined) return String(expr.Real);
    if (expr.Integer !== undefined) return String(expr.Integer);
    if (expr.Boolean !== undefined) return expr.Boolean ? 'true' : 'false';
    if (expr.Ref) return expr.Ref;
    if (expr.Neg) return `-${formatExpr(expr.Neg)}`;
    if (expr.Add) return `(${formatExpr(expr.Add[0])} + ${formatExpr(expr.Add[1])})`;
    if (expr.Sub) return `(${formatExpr(expr.Sub[0])} - ${formatExpr(expr.Sub[1])})`;
    if (expr.Mul) return `(${formatExpr(expr.Mul[0])} * ${formatExpr(expr.Mul[1])})`;
    if (expr.Div) return `(${formatExpr(expr.Div[0])} / ${formatExpr(expr.Div[1])})`;
    if (expr.Pow) return `(${formatExpr(expr.Pow[0])} ^ ${formatExpr(expr.Pow[1])})`;
    if (expr.Der) return `der(${formatExpr(expr.Der)})`;
    if (expr.Sin) return `sin(${formatExpr(expr.Sin)})`;
    if (expr.Cos) return `cos(${formatExpr(expr.Cos)})`;
    if (expr.Sqrt) return `sqrt(${formatExpr(expr.Sqrt)})`;
    if (expr.Exp) return `exp(${formatExpr(expr.Exp)})`;
    if (expr.Log) return `log(${formatExpr(expr.Log)})`;
    if (expr.Abs) return `abs(${formatExpr(expr.Abs)})`;
    if (expr.Sign) return `sign(${formatExpr(expr.Sign)})`;
    if (expr.Gt) return `(${formatExpr(expr.Gt[0])} > ${formatExpr(expr.Gt[1])})`;
    if (expr.Lt) return `(${formatExpr(expr.Lt[0])} < ${formatExpr(expr.Lt[1])})`;
    if (expr.Ge) return `(${formatExpr(expr.Ge[0])} >= ${formatExpr(expr.Ge[1])})`;
    if (expr.Le) return `(${formatExpr(expr.Le[0])} <= ${formatExpr(expr.Le[1])})`;
    if (expr.Eq) return `(${formatExpr(expr.Eq[0])} == ${formatExpr(expr.Eq[1])})`;
    if (expr.And) return `(${formatExpr(expr.And[0])} and ${formatExpr(expr.And[1])})`;
    if (expr.Or) return `(${formatExpr(expr.Or[0])} or ${formatExpr(expr.Or[1])})`;
    if (expr.Not) return `not ${formatExpr(expr.Not)}`;
    if (expr.IfExpr) return `if ${formatExpr(expr.IfExpr.cond)} then ${formatExpr(expr.IfExpr.then_expr)} else ${formatExpr(expr.IfExpr.else_expr)}`;
    if (expr.Pre) return `pre(${formatExpr(expr.Pre)})`;
    if (expr.call) return `${expr.call}(${(expr.args || []).map(formatExpr).join(', ')})`;
    return JSON.stringify(expr);
}

// Format equation to string
function formatEq(eq) {
    if (!eq) return '?';
    // Handle Simple equation variant
    if (eq.Simple) {
        return `${formatExpr(eq.Simple.lhs)} = ${formatExpr(eq.Simple.rhs)}`;
    }
    // Handle direct lhs/rhs format
    if (eq.lhs !== undefined && eq.rhs !== undefined) {
        return `${formatExpr(eq.lhs)} = ${formatExpr(eq.rhs)}`;
    }
    // Handle For equation variant
    if (eq.For) {
        const indices = eq.For.indices || [];
        const idxStr = indices.map(i => `${i.ident?.text || '?'} in ${formatExpr(i.range)}`).join(', ');
        const eqsStr = (eq.For.equations || []).map(formatEq).join('; ');
        return `for ${idxStr} loop ${eqsStr} end for`;
    }
    // Handle Connect equation variant
    if (eq.Connect) {
        return `connect(${formatExpr(eq.Connect.from)}, ${formatExpr(eq.Connect.to)})`;
    }
    return JSON.stringify(eq);
}

// Format component to string
function formatComp(name, comp) {
    if (!comp) return `  ${name}: ?`;
    const type = comp.type_name || 'Real';
    const shape = comp.shape && comp.shape.length > 0 ? `[${comp.shape.join(', ')}]` : '';
    const start = comp.start && comp.start !== 'Empty' ? ` = ${formatExpr(comp.start)}` : '';
    return `  ${name}: ${type}${shape}${start}`;
}

// Format statement to string
function formatStmt(stmt) {
    if (!stmt) return '?';
    if (stmt.Assignment) {
        return `${formatExpr(stmt.Assignment.comp)} := ${formatExpr(stmt.Assignment.value)}`;
    }
    if (stmt.Return) return 'return';
    if (stmt.Break) return 'break';
    if (stmt.For) {
        const indices = stmt.For.indices || [];
        const idxStr = indices.map(i => `${i.ident?.text || '?'} in ${formatExpr(i.range)}`).join(', ');
        return `for ${idxStr} loop ... end for`;
    }
    if (stmt.When) {
        return 'when ... end when';
    }
    if (stmt.If) {
        return 'if ... end if';
    }
    return JSON.stringify(stmt);
}

function prettyPrintDae(dae) {
    if (!dae) return 'No DAE';
    let out = [];

    out.push(`=== ${dae.model_name || 'Model'} ===`);
    if (dae.rumoca_version) out.push(`Rumoca: ${dae.rumoca_version}`);
    out.push('');

    // Parameters (p)
    if (dae.p && Object.keys(dae.p).length > 0) {
        out.push('Parameters:');
        for (const [name, comp] of Object.entries(dae.p)) {
            out.push(formatComp(name, comp));
        }
        out.push('');
    }

    // Constant parameters (cp)
    if (dae.cp && Object.keys(dae.cp).length > 0) {
        out.push('Constants:');
        for (const [name, comp] of Object.entries(dae.cp)) {
            out.push(formatComp(name, comp));
        }
        out.push('');
    }

    // Inputs (u)
    if (dae.u && Object.keys(dae.u).length > 0) {
        out.push('Inputs:');
        for (const [name, comp] of Object.entries(dae.u)) {
            out.push(formatComp(name, comp));
        }
        out.push('');
    }

    // States (x)
    if (dae.x && Object.keys(dae.x).length > 0) {
        out.push('States (x):');
        for (const [name, comp] of Object.entries(dae.x)) {
            out.push(formatComp(name, comp));
        }
        out.push('');
    }

    // Algebraic variables (y)
    if (dae.y && Object.keys(dae.y).length > 0) {
        out.push('Algebraics (y):');
        for (const [name, comp] of Object.entries(dae.y)) {
            out.push(formatComp(name, comp));
        }
        out.push('');
    }

    // Discrete Real (z)
    if (dae.z && Object.keys(dae.z).length > 0) {
        out.push('Discrete Real (z):');
        for (const [name, comp] of Object.entries(dae.z)) {
            out.push(formatComp(name, comp));
        }
        out.push('');
    }

    // Discrete-valued (m)
    if (dae.m && Object.keys(dae.m).length > 0) {
        out.push('Discrete (m):');
        for (const [name, comp] of Object.entries(dae.m)) {
            out.push(formatComp(name, comp));
        }
        out.push('');
    }

    // Conditions (c)
    if (dae.c && Object.keys(dae.c).length > 0) {
        out.push('Conditions (c):');
        for (const [name, comp] of Object.entries(dae.c)) {
            out.push(formatComp(name, comp));
        }
        out.push('');
    }

    // Continuous equations (fx)
    if (dae.fx && dae.fx.length > 0) {
        out.push('Equations (fx):');
        dae.fx.forEach(eq => out.push(`  ${formatEq(eq)};`));
        out.push('');
    }

    // Initial equations (fx_init)
    if (dae.fx_init && dae.fx_init.length > 0) {
        out.push('Initial Equations (fx_init):');
        dae.fx_init.forEach(eq => out.push(`  ${formatEq(eq)};`));
        out.push('');
    }

    // Algebraic equations (fz)
    if (dae.fz && dae.fz.length > 0) {
        out.push('Algebraic Equations (fz):');
        dae.fz.forEach(eq => out.push(`  ${formatEq(eq)};`));
        out.push('');
    }

    // Discrete update equations (fm)
    if (dae.fm && dae.fm.length > 0) {
        out.push('Discrete Equations (fm):');
        dae.fm.forEach(eq => out.push(`  ${formatEq(eq)};`));
        out.push('');
    }

    // Reset statements (fr)
    if (dae.fr && Object.keys(dae.fr).length > 0) {
        out.push('Reset Statements (fr):');
        for (const [cond, stmt] of Object.entries(dae.fr)) {
            out.push(`  when ${cond}: ${formatStmt(stmt)}`);
        }
        out.push('');
    }

    // Condition updates (fc)
    if (dae.fc && Object.keys(dae.fc).length > 0) {
        out.push('Condition Updates (fc):');
        for (const [cond, expr] of Object.entries(dae.fc)) {
            out.push(`  ${cond} := ${formatExpr(expr)}`);
        }
        out.push('');
    }

    // Summary
    const numStates = dae.x ? Object.keys(dae.x).length : 0;
    const numAlg = dae.y ? Object.keys(dae.y).length : 0;
    const numFx = dae.fx ? dae.fx.length : 0;
    const numFz = dae.fz ? dae.fz.length : 0;

    out.push('Summary:');
    out.push(`  States: ${numStates}`);
    out.push(`  Algebraics: ${numAlg}`);
    out.push(`  Equations: ${numFx} (continuous) + ${numFz} (algebraic)`);

    return out.join('\n');
}

const libraryDocsController = createLibraryDocsController({
    sendLanguageCommand,
    sendWorkspaceCommand,
    setTerminalOutput,
    isWorkerReady: () => workerReady,
    projectFs,
});
libraryDocsController.bindWindowApi();

// Store compiled results for all models: { modelName: { dae, balance } }
window.compiledModels = {};
window.selectedModel = null;

function resetCompiledWorkspaceState() {
    window.compiledModels = {};
    window.selectedModel = null;
    window.currentDaeForCompletions = null;
    simResultsPanelState = { activeViewId: null };
    resultsPanelController.clear();
    const modelSelect = document.getElementById('modelSelect');
    if (modelSelect) {
        modelSelect.innerHTML = '<option value="">-- No models --</option>';
        modelSelect.value = '';
    }
    projectInterface.execute('rumoca.project.setSelectedSimulationModel', { model: '' });
    const statusSpan = document.getElementById('autoCompileStatus');
    if (statusSpan) {
        statusSpan.textContent = '';
        statusSpan.style.color = '#888';
    }
    setDaeOutput('No model selected');
    displayCodegenOutput('');
    void resultsPanelController.renderModel('');
}

async function applyImportedProject(projectState) {
    resetCompiledWorkspaceState();
    suspendWorkspaceObservers = true;
    try {
        if (editor?.setValue) {
            editor.setValue(projectState.activeDocumentContent || '');
        }
        applyProjectEditorState(projectState.editorState);
        projectFs.updateActiveDocumentContent(editor?.getValue?.() || projectState.activeDocumentContent || '');
    } finally {
        suspendWorkspaceObservers = false;
    }
    await libraryDocsController.restoreProjectLibraries();
    updateSourceBreadcrumbs();
    updateGlobalStatusBar();
    await resultsPanelController.renderModel(window.selectedModel || '');
    refreshSimulationSettingsModalIfOpen();
    if (!isRumocaSmokeMode() && typeof window.triggerCompileNow === 'function') {
        window.triggerCompileNow();
    }
}

installFileActions({
    getEditor: () => window.editor,
    getCompiledModels: () => window.compiledModels,
    getDaeFormat: () => window.daeFormat,
    getCodegenOutputEditor: () => window.codegenOutputEditor,
    projectFs,
    setTerminalOutput,
    beforeProjectExport: async () => {
        projectFs.setEditorState(collectProjectEditorState());
    },
    onCreateNewProject: async () => {
        await applyImportedProject(buildNewProjectState());
    },
    onProjectLoaded: async (projectState) => {
        await applyImportedProject(projectState);
    },
});

// Update display when model selection changes
window.updateSelectedModel = function() {
    const modelName = document.getElementById('modelSelect').value;
    window.selectedModel = modelName;
    projectInterface.execute('rumoca.project.setSelectedSimulationModel', { model: modelName });
    void resultsPanelController.renderModel(modelName || '');
    if (modelName && window.compiledModels[modelName]) {
        const result = window.compiledModels[modelName];
        // Refresh the active tab's output
        if (window.activeRightTab === 'dae') displayDaeOutput(modelName);
        else if (window.activeRightTab === 'codegen') displayCodegenOutput(modelName);
        // Update DAE for template autocompletion
        if (result.dae_native) {
            window.currentDaeForCompletions = result.dae_native;
        }
        // Update balance status
        const statusSpan = document.getElementById('autoCompileStatus');
        const b = result.balance;
        if (b) {
            const balanceStatus = b.is_balanced ? 'BALANCED' :
                (typeof b.status === 'string' ? b.status.toUpperCase() :
                (b.status.CompileError ? 'ERROR' : 'UNBALANCED'));
            statusSpan.textContent = b.is_balanced ? `${modelName}: Balanced` : `${modelName}: ${balanceStatus}`;
            statusSpan.style.color = b.is_balanced ? '#2d6a4f' : '#c9184a';
        }
    } else if (!modelName) {
        setDaeOutput('No model selected');
    }
    updateSourceBreadcrumbs();
    updateGlobalStatusBar();
    refreshSimulationSettingsModalIfOpen();
};

const sourceBreadcrumbs = createSourceBreadcrumbs();

function updateSourceBreadcrumbs() {
    sourceBreadcrumbs.update();
}

// Web Worker setup (cache-busted to avoid stale JS/WASM bundles during local iteration)
const workerCacheBust = String(Date.now());
const workerUrl = new URL('../../pkg/rumoca_worker.js', window.location.href);
workerUrl.searchParams.set('v', workerCacheBust);
const worker = new Worker(workerUrl, { type: 'module' });
let requestId = 0;
const pendingRequests = new Map();
let workerReady = false;

worker.onmessage = (e) => {
    const { id, ready, success, result, error, progress, current, total, percent } = e.data;

    // Handle progress updates for library loading
    if (progress) {
        libraryDocsController.handleWorkerProgress({ current, total, percent });
        return;
    }

    if (ready !== undefined) {
        const status = document.getElementById('status');
        if (success) {
            status.textContent = 'Ready';
            status.className = 'status ready';
            workerReady = true;
            if (window.refreshModelicaSemanticTokens) {
                window.refreshModelicaSemanticTokens();
            }
            // Fetch version and show welcome message
            sendWorkspaceCommand('rumoca.workspace.getVersion', {}).then(version => {
                setTerminalOutput(`Rumoca v${version} - Modelica Compiler\nHover over tabs/buttons for help.`);
            }).catch(() => {
                setTerminalOutput('WASM initialized! Edit code to compile.');
            });
        } else {
            status.textContent = 'Error';
            status.className = 'status error';
            setTerminalOutput('Failed to initialize WASM worker.');
        }
        return;
    }
    const resolver = pendingRequests.get(id);
    if (resolver) {
        pendingRequests.delete(id);
        if (error) resolver.reject(new Error(error));
        else resolver.resolve(result);
    }
};

worker.onerror = (e) => {
    console.error('Worker error:', e);
    document.getElementById('status').textContent = 'Worker Error';
    document.getElementById('status').className = 'status error';
};

function sendRequest(action, params = {}, timeout = 30000) {
    const id = ++requestId;
    return new Promise((resolve, reject) => {
        const timeoutId = setTimeout(() => {
            if (pendingRequests.has(id)) {
                pendingRequests.delete(id);
                console.error(`[sendRequest] timeout for action '${action}' (id=${id})`);
                reject(new Error(`Request timeout for ${action}`));
            }
        }, timeout);

        pendingRequests.set(id, {
            resolve: (result) => {
                clearTimeout(timeoutId);
                resolve(result);
            },
            reject: (error) => {
                clearTimeout(timeoutId);
                reject(error);
            }
        });
        worker.postMessage({ id, action, ...params });
    });
}

function sendLanguageCommand(command, payload = {}, timeout = 30000) {
    return sendRequest(
        'languageCommand',
        {
            command,
            payload,
        },
        timeout,
    );
}

function sendWorkspaceCommand(command, payload = {}, timeout = 30000) {
    return sendRequest(
        'workspaceCommand',
        {
            command,
            payload,
        },
        timeout,
    );
}

function readRumocaSmokeConfig() {
    const params = new URLSearchParams(window.location.search);
    if (params.get('rumoca_smoke') !== '1') {
        return null;
    }
    return {
        modelName: params.get('smoke_model') || '',
        sourceUrl: params.get('smoke_source_url') || '',
        libraryZipUrl: params.get('smoke_library_zip_url') || '',
        callbackPort: Number.parseInt(params.get('smoke_callback_port') || '0', 10),
        readyTimeoutMs: Number.parseInt(params.get('smoke_ready_timeout_ms') || '20000', 10),
        compileTimeoutMs: Number.parseInt(params.get('smoke_compile_timeout_ms') || '60000', 10),
        completionTimeoutMs: Number.parseInt(params.get('smoke_completion_timeout_ms') || '20000', 10),
    };
}

function isRumocaSmokeMode() {
    return new URLSearchParams(window.location.search).get('rumoca_smoke') === '1';
}

function ensureRumocaSmokeResultNode() {
    let node = document.getElementById('rumocaSmokeResult');
    if (node) {
        return node;
    }
    node = document.createElement('pre');
    node.id = 'rumocaSmokeResult';
    node.hidden = true;
    document.body.appendChild(node);
    return node;
}

function setRumocaSmokeResult(status, payload) {
    document.body.dataset.rumocaSmokeStatus = status;
    ensureRumocaSmokeResultNode().textContent = JSON.stringify(payload, null, 2);
}

async function notifyRumocaSmokeDone(config, status, payload) {
    if (!Number.isFinite(config.callbackPort) || config.callbackPort <= 0) {
        return;
    }
    try {
        await fetch(`http://127.0.0.1:${config.callbackPort}/smoke-done`, {
            method: 'POST',
            mode: 'no-cors',
            keepalive: true,
            body: JSON.stringify({ status, payload }),
        });
    } catch (error) {
        console.warn('[rumoca-smoke] failed to notify callback', error);
    }
}

async function waitForRumocaSmoke(label, predicate, timeoutMs) {
    const started = performance.now();
    while ((performance.now() - started) < timeoutMs) {
        const value = await predicate();
        if (value) {
            return value;
        }
        await new Promise(resolve => setTimeout(resolve, 100));
    }
    throw new Error(`${label} timed out after ${timeoutMs}ms`);
}

async function loadRumocaSmokeLibraryArchive(libraryZipUrl) {
    const response = await fetch(libraryZipUrl, { cache: 'no-store' });
    if (!response.ok) {
        throw new Error(`Failed to fetch smoke library archive: ${response.status} ${response.statusText}`);
    }
    const bytes = await response.arrayBuffer();
    const fileName = libraryZipUrl.split('/').pop() || 'smoke-library.zip';
    const file = new File([bytes], fileName, { type: 'application/zip' });
    if (typeof window.stageLibraryArchiveFile !== 'function') {
        throw new Error('library staging unavailable for smoke archive load');
    }
    rumocaSmokeLibrariesImported = false;
    return await window.stageLibraryArchiveFile(file);
}

function normalizeCompletionItems(rawCompletion) {
    const items = rawCompletion?.items || rawCompletion || [];
    return Array.isArray(items) ? items : [];
}

const EXPECTED_MSL_COMPLETION_LABEL = 'Electrical';
const EXPECTED_MSL_NAVIGATION_LABEL = 'Ground';
const ACTIVE_WASM_URI = 'file:///input.mo';
let rumocaSmokeLibrariesImported = false;

function completionLabel(item) {
    if (typeof item?.label === 'string') {
        return item.label;
    }
    if (item?.label && typeof item.label.label === 'string') {
        return item.label.label;
    }
    return String(item?.label ?? '');
}

function hoverContentsParts(contents) {
    if (typeof contents === 'string') {
        return [contents];
    }
    if (Array.isArray(contents)) {
        return contents.flatMap(part => hoverContentsParts(part));
    }
    if (contents && typeof contents === 'object' && typeof contents.value === 'string') {
        return [contents.value];
    }
    return [];
}

function normalizeUri(value) {
    if (typeof value === 'string') {
        return value;
    }
    if (value && typeof value === 'object' && typeof value.toString === 'function') {
        const rendered = value.toString();
        if (rendered && rendered !== '[object Object]') {
            return rendered;
        }
    }
    return null;
}

function findRumocaSmokeNavigationProbe(source) {
    const preferredPath = 'Modelica.Electrical.Analog.Basic.Ground';
    const preferredOffset = source.indexOf(preferredPath);
    if (preferredOffset >= 0) {
        return {
            label: EXPECTED_MSL_NAVIGATION_LABEL,
            offset: preferredOffset + preferredPath.lastIndexOf(EXPECTED_MSL_NAVIGATION_LABEL),
        };
    }

    const fallback = source.match(/\bModelica(?:\.[A-Z][A-Za-z0-9_]*)+\b/);
    if (!fallback?.[0]) {
        throw new Error('smoke source missing a qualified Modelica symbol for navigation');
    }
    const label = fallback[0].split('.').at(-1);
    if (!label) {
        throw new Error('qualified Modelica navigation probe is missing a terminal label');
    }
    return {
        label,
        offset: fallback.index + fallback[0].lastIndexOf(label),
    };
}

async function measureRumocaSmokeHover(source, timeoutMs) {
    const navigationProbe = findRumocaSmokeNavigationProbe(source);
    const model = window.editor?.getModel();
    if (!model) {
        throw new Error('editor unavailable for smoke hover measurement');
    }
    const position = model.getPositionAt(navigationProbe.offset);
    const started = performance.now();
    const rawHover = await sendLanguageCommand(
        'rumoca.language.hover',
        {
            source,
            line: position.lineNumber - 1,
            character: position.column - 1,
        },
        timeoutMs,
    );
    const hoverMs = Math.round(performance.now() - started);
    const hover = JSON.parse(rawHover);
    const hoverText = hoverContentsParts(hover?.contents).join('\n');
    return {
        hoverMs,
        hoverCount: hover ? 1 : 0,
        expectedHoverPresent: hoverText.includes(navigationProbe.label),
    };
}

async function measureRumocaSmokeDefinition(source, timeoutMs) {
    const navigationProbe = findRumocaSmokeNavigationProbe(source);
    const model = window.editor?.getModel();
    if (!model) {
        throw new Error('editor unavailable for smoke definition measurement');
    }
    const position = model.getPositionAt(navigationProbe.offset);
    const started = performance.now();
    const rawDefinition = await sendLanguageCommand(
        'rumoca.language.definition',
        {
            source,
            line: position.lineNumber - 1,
            character: position.column - 1,
        },
        timeoutMs,
    );
    const definitionMs = Math.round(performance.now() - started);
    const parsedDefinition = JSON.parse(rawDefinition);
    const definitionEntries = Array.isArray(parsedDefinition)
        ? parsedDefinition
        : parsedDefinition
            ? [parsedDefinition]
            : [];
    const definitionUris = definitionEntries
        .map(item => normalizeUri(item?.targetUri ?? item?.uri ?? null))
        .filter(Boolean);
    return {
        definitionMs,
        definitionCount: definitionUris.length,
        expectedDefinitionPresent: definitionUris.length > 0,
        crossFileDefinitionPresent: definitionUris.some(uri => uri !== ACTIVE_WASM_URI),
    };
}

async function measureRumocaSmokeCompletion(source, timeoutMs, options = {}) {
    const { ensureLibraryImport = false } = options;
    if (!window.editor || !window.editor.getModel) {
        throw new Error('editor unavailable for smoke completion measurement');
    }

    if (window.editor.getValue() !== source) {
        window.editor.setValue(source);
    }
    const model = window.editor.getModel();
    const preferredProbe = 'Modelica.Electrical.Analog.Basic.Ground';
    const preferredOffset = source.indexOf(preferredProbe);
    const probe = 'Modelica.';
    const offset = preferredOffset >= 0 ? preferredOffset : source.indexOf(probe);
    if (offset < 0) {
        throw new Error(`completion probe source missing '${probe}'`);
    }
    const position = model.getPositionAt(offset + probe.length);
    window.editor.focus();
    window.editor.setPosition(position);
    window.editor.revealPositionInCenter(position);

    const started = performance.now();
    let libraryImportMs = 0;
    if (ensureLibraryImport && !rumocaSmokeLibrariesImported) {
        if (typeof window.importLoadedLibrariesForSmoke !== 'function') {
            throw new Error('library import unavailable for smoke completion measurement');
        }
        const imported = await window.importLoadedLibrariesForSmoke();
        libraryImportMs = Math.max(0, Number(imported?.libraryImportMs) || 0);
        rumocaSmokeLibrariesImported = true;
    }
    const rawCompletion = await sendLanguageCommand(
        'rumoca.language.completionWithTiming',
        {
            source,
            line: position.lineNumber - 1,
            character: position.column - 1,
        },
        timeoutMs,
    );
    const completionMs = Math.round(performance.now() - started);
    const parsedCompletion = JSON.parse(rawCompletion);
    const items = normalizeCompletionItems(parsedCompletion?.items || parsedCompletion);
    return {
        completionMs,
        libraryImportMs,
        completionCount: items.length,
        expectedCompletionPresent: items.some(
            item => completionLabel(item) === EXPECTED_MSL_COMPLETION_LABEL,
        ),
        stageTimings: parsedCompletion?.timing || null,
    };
}

async function measureRumocaSmokeCodeLenses() {
    if (typeof window.provideModelicaCodeLensesForSmoke !== 'function') {
        throw new Error('code lens provider unavailable for smoke measurement');
    }
    const started = performance.now();
    const provided = await Promise.resolve(window.provideModelicaCodeLensesForSmoke());
    const codeLensMs = Math.round(performance.now() - started);
    const lenses = Array.isArray(provided?.lenses) ? provided.lenses : [];
    return {
        codeLensMs,
        codeLensCount: lenses.length,
    };
}

async function runRumocaBrowserSmoke(config) {
    const result = {
        modelName: config.modelName || null,
        libraryCount: 0,
        statusText: '',
    };
    setRumocaSmokeResult('running', result);

    try {
        await waitForRumocaSmoke(
            'worker ready',
            () => workerReady && window.editor && window.loadLibraryArchiveFile,
            config.readyTimeoutMs,
        );

        if (config.libraryZipUrl) {
            const stagedArchive = await loadRumocaSmokeLibraryArchive(config.libraryZipUrl);
            result.archiveLoadMs = Math.max(0, Number(stagedArchive?.archivePrepMs) || 0);
            result.libraryCount = await waitForRumocaSmoke(
                'loaded library count',
                () => {
                    const count = parseBadgeNumber(document.getElementById('libCount')?.textContent || '0');
                    return count > 0 ? count : null;
                },
                config.readyTimeoutMs,
            );
        }

        let sourceText = window.editor.getValue();
        let discoveredModels = null;
        if (config.sourceUrl) {
            const response = await fetch(config.sourceUrl, { cache: 'no-store' });
            if (!response.ok) {
                throw new Error(`Failed to fetch smoke source: ${response.status} ${response.statusText}`);
            }
            sourceText = await response.text();
            const openStart = performance.now();
            window.compiledModels = {};
            window.editor.setValue(sourceText);
            discoveredModels = await waitForRumocaSmoke(
                'smoke source model discovery',
                async () => {
                    const state = await getSimulationModelState(window.editor.getValue(), config.modelName || '');
                    const models = Array.isArray(state.models) ? state.models : [];
                    if (!config.modelName) {
                        return models.length > 0 ? models : null;
                    }
                    return models.includes(config.modelName) ? models : null;
                },
                config.readyTimeoutMs,
            );
            result.openMs = Math.round(performance.now() - openStart);
        }

        if (!discoveredModels) {
            discoveredModels = await waitForRumocaSmoke(
                'smoke source model discovery',
                async () => {
                    const state = await getSimulationModelState(window.editor.getValue(), config.modelName || '');
                    const models = Array.isArray(state.models) ? state.models : [];
                    if (!config.modelName) {
                        return models.length > 0 ? models : null;
                    }
                    return models.includes(config.modelName) ? models : null;
                },
                config.readyTimeoutMs,
            );
            result.openMs = result.openMs ?? 0;
        }

        const smokeModelName = config.modelName || discoveredModels[0];
        const completionProbeSource = sourceText;
        const codeLenses = await measureRumocaSmokeCodeLenses();
        result.codeLensMs = codeLenses.codeLensMs;
        result.codeLensCount = codeLenses.codeLensCount;
        const libraryLoad = await measureRumocaSmokeCompletion(
            completionProbeSource,
            config.completionTimeoutMs,
            { ensureLibraryImport: true },
        );
        result.libraryLoadMs = libraryLoad.completionMs;
        result.libraryLoadCompletionCount = libraryLoad.completionCount;
        result.libraryExpectedCompletionPresent = libraryLoad.expectedCompletionPresent;
        result.libraryStageTimings = libraryLoad.stageTimings;
        if (!result.libraryExpectedCompletionPresent) {
            throw new Error(
                `Initial smoke completion items did not include Modelica.${EXPECTED_MSL_COMPLETION_LABEL}`,
            );
        }

        const firstCompletion = await measureRumocaSmokeCompletion(
            completionProbeSource,
            config.completionTimeoutMs,
        );
        const warmCompletion = await measureRumocaSmokeCompletion(
            completionProbeSource,
            config.completionTimeoutMs,
        );
        result.completionMs = firstCompletion.completionMs;
        result.completionCount = firstCompletion.completionCount;
        result.expectedCompletionPresent = firstCompletion.expectedCompletionPresent;
        result.coldStageTimings = firstCompletion.stageTimings;
        result.warmCompletionMs = warmCompletion.completionMs;
        result.warmCompletionCount = warmCompletion.completionCount;
        result.warmExpectedCompletionPresent = warmCompletion.expectedCompletionPresent;
        result.warmStageTimings = warmCompletion.stageTimings;
        if (!result.expectedCompletionPresent) {
            throw new Error(
                `Smoke completion items did not include Modelica.${EXPECTED_MSL_COMPLETION_LABEL}`,
            );
        }
        if (!result.warmExpectedCompletionPresent) {
            throw new Error(
                `Warm smoke completion items did not include Modelica.${EXPECTED_MSL_COMPLETION_LABEL}`,
            );
        }

        const hover = await measureRumocaSmokeHover(
            completionProbeSource,
            config.completionTimeoutMs,
        );
        result.hoverMs = hover.hoverMs;
        result.hoverCount = hover.hoverCount;
        result.expectedHoverPresent = hover.expectedHoverPresent;
        if (!result.expectedHoverPresent) {
            throw new Error(`Smoke hover did not include ${EXPECTED_MSL_NAVIGATION_LABEL}`);
        }

        const definition = await measureRumocaSmokeDefinition(
            completionProbeSource,
            config.completionTimeoutMs,
        );
        result.definitionMs = definition.definitionMs;
        result.definitionCount = definition.definitionCount;
        result.expectedDefinitionPresent = definition.expectedDefinitionPresent;
        result.crossFileDefinitionPresent = definition.crossFileDefinitionPresent;
        if (!result.expectedDefinitionPresent) {
            throw new Error(`Smoke definition did not resolve ${EXPECTED_MSL_NAVIGATION_LABEL}`);
        }
        if (!result.crossFileDefinitionPresent) {
            throw new Error(`Smoke definition did not leave the active document for ${EXPECTED_MSL_NAVIGATION_LABEL}`);
        }

        const compileStart = performance.now();
        const compileJson = await sendWorkspaceCommand(
            result.libraryCount > 0
                ? 'rumoca.workspace.compileWithLibraries'
                : 'rumoca.workspace.compile',
            {
                source: window.editor.getValue(),
                modelName: smokeModelName,
                libraries: '{}',
            },
            config.compileTimeoutMs,
        );
        const compileResult = JSON.parse(compileJson);
        result.compileMs = Math.round(performance.now() - compileStart);
        result.modelName = smokeModelName;
        result.libraryCount = parseBadgeNumber(document.getElementById('libCount')?.textContent || '0');
        result.statusText = String(document.getElementById('autoCompileStatus')?.textContent || '');
        window.compiledModels = window.compiledModels || {};
        window.compiledModels[smokeModelName] = {
            dae: compileResult.dae,
            dae_native: compileResult.dae_native,
            balance: compileResult.balance,
            pretty: compileResult.pretty,
            error: null,
        };

        if (compileResult.balance?.is_balanced !== true) {
            throw new Error(`Smoke model did not balance: ${JSON.stringify(compileResult.balance ?? null)}`);
        }

        setRumocaSmokeResult('pass', result);
        await notifyRumocaSmokeDone(config, 'pass', result);
    } catch (error) {
        result.error = String(error?.message || error);
        setRumocaSmokeResult('fail', result);
        await notifyRumocaSmokeDone(config, 'fail', result);
        throw error;
    }
}

function jumpToModelicaLocation(lineNumber, column) {
    if (!editor) return;
    const safeLine = Math.max(1, Number(lineNumber) || 1);
    const safeColumn = Math.max(1, Number(column) || 1);
    const position = { lineNumber: safeLine, column: safeColumn };
    editor.focus();
    editor.setPosition(position);
    editor.revealPositionInCenter(position);
}

async function triggerModelicaQuickFixAt(lineNumber, column) {
    if (!editor) return;
    jumpToModelicaLocation(lineNumber, column);
    const action = editor.getAction ? editor.getAction('editor.action.quickFix') : null;
    if (!action || typeof action.run !== 'function') return;
    await action.run();
}

function flattenClassTreeNodes(nodes, sink) {
    if (!Array.isArray(nodes)) return sink;
    for (const node of nodes) {
        if (!node || typeof node !== 'object') continue;
        if (typeof node.qualified_name === 'string' && node.qualified_name.length > 0) {
            sink.push(node);
        }
        flattenClassTreeNodes(node.children, sink);
    }
    return sink;
}

async function buildQuickOpenItems() {
    const items = [];
    const modelSelect = document.getElementById('modelSelect');
    const seenModelNames = new Set();

    if (modelSelect) {
        for (const option of Array.from(modelSelect.options)) {
            const modelName = String(option.value || '').trim();
            if (!modelName || seenModelNames.has(modelName)) continue;
            seenModelNames.add(modelName);
            items.push({
                label: `Model: ${modelName}`,
                detail: 'Select model in right panel',
                tags: ['model', 'open'],
                run: () => {
                    modelSelect.value = modelName;
                    if (typeof window.updateSelectedModel === 'function') {
                        window.updateSelectedModel();
                    }
                    if (typeof window.switchRightTab === 'function') {
                        window.switchRightTab('dae');
                    }
                    if (editor) editor.focus();
                },
            });
        }
    }

    if (!workerReady) return items;

    try {
        const json = await sendLanguageCommand('rumoca.language.listClasses', {});
        const parsed = JSON.parse(json);
        const classNodes = flattenClassTreeNodes(parsed?.classes, []);
        for (const node of classNodes) {
            const qualifiedName = String(node.qualified_name || '').trim();
            if (!qualifiedName) continue;
            const classType = String(node.class_type || 'class');
            const partialSuffix = node.partial ? ' partial' : '';
            items.push({
                label: `Class: ${qualifiedName}`,
                detail: `${classType}${partialSuffix}`,
                tags: ['class', 'documentation', classType.toLowerCase()],
                run: async () => {
                    if (typeof window.switchBottomTab === 'function') {
                        window.switchBottomTab('packages');
                    }
                    if (typeof window.selectClass === 'function') {
                        await window.selectClass(encodeURIComponent(qualifiedName));
                    }
                },
            });
        }
    } catch (error) {
        console.warn('Quick-open class list failed:', error);
    }

    return items;
}

function symbolStartPosition(symbol) {
    const selectionRange = symbol?.selectionRange || symbol?.selection_range || null;
    const range = symbol?.range || symbol?.location?.range || null;
    const start = selectionRange?.start || range?.start || null;
    return {
        lineNumber: Math.max(1, Number(start?.line ?? 0) + 1),
        column: Math.max(1, Number(start?.character ?? 0) + 1),
    };
}

function normalizeDocumentSymbols(payload) {
    if (!payload) return { nested: [], flat: [] };
    if (Array.isArray(payload)) return { nested: payload, flat: [] };
    const nested = Array.isArray(payload.Nested)
        ? payload.Nested
        : (Array.isArray(payload.nested) ? payload.nested : []);
    const flat = Array.isArray(payload.Flat)
        ? payload.Flat
        : (Array.isArray(payload.flat) ? payload.flat : []);
    return { nested, flat };
}

function flattenNestedDocumentSymbols(symbols, sink, parentPath = '') {
    if (!Array.isArray(symbols)) return sink;
    for (const symbol of symbols) {
        if (!symbol || typeof symbol !== 'object') continue;
        const name = String(symbol.name || '').trim();
        const fullName = parentPath && name ? `${parentPath}.${name}` : name || parentPath;
        if (fullName) {
            const pos = symbolStartPosition(symbol);
            sink.push({
                label: fullName,
                detail: String(symbol.detail || ''),
                tags: ['symbol', 'outline'],
                run: () => jumpToModelicaLocation(pos.lineNumber, pos.column),
            });
        }
        flattenNestedDocumentSymbols(symbol.children, sink, fullName);
    }
    return sink;
}

function flattenFlatDocumentSymbols(symbols, sink) {
    if (!Array.isArray(symbols)) return sink;
    for (const symbol of symbols) {
        if (!symbol || typeof symbol !== 'object') continue;
        const name = String(symbol.name || '').trim();
        if (!name) continue;
        const containerName = String(symbol.containerName || symbol.container_name || '').trim();
        const fullName = containerName ? `${containerName}.${name}` : name;
        const pos = symbolStartPosition(symbol);
        sink.push({
            label: fullName,
            detail: 'Symbol',
            tags: ['symbol', 'outline'],
            run: () => jumpToModelicaLocation(pos.lineNumber, pos.column),
        });
    }
    return sink;
}

async function buildDocumentSymbolItems() {
    if (!editor || !workerReady) return [];
    try {
        const source = editor.getValue();
        const json = await sendLanguageCommand('rumoca.language.documentSymbols', { source });
        const parsed = JSON.parse(json);
        const symbols = normalizeDocumentSymbols(parsed);
        const items = [];
        flattenNestedDocumentSymbols(symbols.nested, items);
        flattenFlatDocumentSymbols(symbols.flat, items);
        return items;
    } catch (error) {
        console.warn('Symbol search failed:', error);
        return [];
    }
}

setupCommandPalette({
    getQuickOpenItems: buildQuickOpenItems,
    getSymbolItems: buildDocumentSymbolItems,
});

// Monaco Editor
require.config({ paths: { 'vs': 'https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.45.0/min/vs' }});
require(['vs/editor/editor.main'], function() {
    const monacoState = setupMonacoWorkspace({ monaco, sendLanguageCommand, layoutAllEditors });
    editor = monacoState.editor;
    const templateEditor = monacoState.templateEditor;
    projectFs.setActiveDocument(
        inferModelicaFileName(editor.getValue(), 'Ball.mo'),
        editor.getValue(),
    );
    defaultProjectSeed = {
        activeDocumentPath: projectFs.getActiveDocumentPath(),
        activeDocumentContent: editor.getValue(),
        editorState: collectProjectEditorState(),
    };
    window.triggerQuickFixAtCursor = async function() {
        if (!editor) return;
        const position = editor.getPosition ? editor.getPosition() : null;
        const line = position ? position.lineNumber : 1;
        const col = position ? position.column : 1;
        await triggerModelicaQuickFixAt(line, col);
    };

    function debounce(fn, delay) {
        let timer = null;
        return function(...args) {
            if (timer) clearTimeout(timer);
            timer = setTimeout(() => fn.apply(this, args), delay);
        };
    }

    diagnosticsController = createDiagnosticsController({
        monaco,
        getModelEditor: () => editor,
        getTemplateEditor: () => window.templateEditor,
        switchToErrorsTab: () => window.switchBottomTab('errors'),
        triggerModelicaQuickFix: triggerModelicaQuickFixAt,
    });
    window.renderAllDiagnostics = () => diagnosticsController.renderAllDiagnostics();

    let liveCheckGeneration = 0;
    const runLiveChecks = debounce(async () => {
        if (!workerReady) return;
        const runGeneration = ++liveCheckGeneration;
        const isStaleRun = () => runGeneration !== liveCheckGeneration;

        const source = editor.getValue();
        const statusSpan = document.getElementById('autoCompileStatus');
        const modelSelect = document.getElementById('modelSelect');
        const startTime = performance.now();

        statusSpan.textContent = 'Compiling...';
        statusSpan.style.color = '#9a6700';
        updateCompileErrors([]);

        try {
        let diagnostics = [];
        // Run diagnostics
        try {
            const diagJson = await sendLanguageCommand('rumoca.language.diagnostics', { source });
            if (isStaleRun()) return;
            diagnostics = normalizeDiagnosticsPayload(JSON.parse(diagJson), source);
            if (isStaleRun()) return;
            updateDiagnostics(diagnostics);
        } catch (e) {
            if (isStaleRun()) return;
            console.warn('Live diagnostics error:', e);
            // Clear old diagnostics on error to avoid showing stale errors
            updateDiagnostics([]);
            diagnostics = [];
        }

        const previousSelection = modelSelect.value;
        const modelState = await getSimulationModelState(source, previousSelection);
        if (isStaleRun()) return;
        const models = Array.isArray(modelState.models) ? modelState.models : [];
        modelSelect.innerHTML = models.length === 0
            ? '<option value="">-- No models --</option>'
            : models.map((model) => `<option value="${model}">${model}</option>`).join('');
        modelSelect.value = modelState.selectedModel || '';
        if (isStaleRun()) return;

        // Clear old compiled results
        window.compiledModels = {};

        const hasParseErrors = diagnostics.some(d => {
            if (d?.severity !== 1) return false;
            const code = diagnosticCodeString(d);
            return code.startsWith('EP')
                || code === 'syntax-error'
                || /\bEP\d{3}\b/.test(String(d?.message ?? ''));
        });
        const hasErrorDiagnostics = diagnostics.some(d => d?.severity === 1);
        if (hasErrorDiagnostics) {
            updateCompileErrors([]);
            const elapsed = (performance.now() - startTime).toFixed(0);
            statusSpan.textContent = hasParseErrors
                ? `Syntax Error (${elapsed}ms)`
                : `Error (${elapsed}ms)`;
            statusSpan.style.color = '#c9184a';
            if (models.length > 0) {
                const selectedModel = modelSelect.value || models[0];
                window.selectedModel = selectedModel;
                setDaeOutput(hasParseErrors
                    ? 'Compilation skipped due to parse errors.'
                    : 'Compilation skipped due to diagnostics errors.');
            } else {
                setDaeOutput('No models found in source');
            }
            if (window.refreshCodeLens) window.refreshCodeLens();
            window.switchBottomTab('errors');
            if (document.getElementById('packagesSection').classList.contains('active')) {
                libraryDocsController.refreshPackageViewer(true);
            }
            return;
        }

        // Compile all models
        let cachedCount = 0;
        try { cachedCount = await sendLanguageCommand('rumoca.language.getLibraryCount', {}); } catch (e) {}

        let successCount = 0;
        const compileErrors = [];

        for (const modelName of models) {
            if (isStaleRun()) return;
            try {
                let json;
                console.log('[compile] compiling model:', modelName);
                if (cachedCount > 0) {
                    json = await sendWorkspaceCommand('rumoca.workspace.compileWithLibraries', {
                        source,
                        modelName,
                        libraries: '{}',
                    });
                } else {
                    json = await sendWorkspaceCommand('rumoca.workspace.compile', {
                        source,
                        modelName,
                    });
                }
                if (isStaleRun()) return;
                const result = JSON.parse(json);
                console.log('[compile] got result for', modelName, '- pretty length:', result.pretty?.length, 'dae keys:', Object.keys(result.dae || {}));
                window.compiledModels[modelName] = {
                    dae: result.dae,
                    dae_native: result.dae_native,
                    balance: result.balance,
                    pretty: result.pretty
                };
                // Update DAE for template autocompletion (use dae_native for actual field names)
                if (result.dae_native && (modelName === modelSelect.value || modelSelect.value === '')) {
                    window.currentDaeForCompletions = result.dae_native;
                    console.log('[compile] updated currentDaeForCompletions for', modelName);
                }
                successCount++;
            } catch (e) {
                if (isStaleRun()) return;
                console.log('[compile] error for', modelName, ':', e.message);
                window.compiledModels[modelName] = {
                    dae: null,
                    balance: null,
                    error: e.message
                };
                compileErrors.push({ model: modelName, message: e.message });
            }
        }

        const elapsed = (performance.now() - startTime).toFixed(0);
        if (isStaleRun()) return;
        updateCompileErrors(compileErrors);

        // Update display with selected model
        let selectedModel = modelSelect.value;
        console.log('[runLiveChecks] selectedModel:', selectedModel, 'models:', models);

        // If no selection but we have models, select the first one
        if (!selectedModel && models.length > 0) {
            selectedModel = models[0];
            modelSelect.value = selectedModel;
            console.log('[runLiveChecks] auto-selected:', selectedModel);
        }
        window.selectedModel = selectedModel;

        // Refresh active tab output after compilation
        if (selectedModel && window.compiledModels[selectedModel]) {
            const result = window.compiledModels[selectedModel];
            // Refresh the active tab
            if (window.activeRightTab === 'dae') displayDaeOutput(selectedModel);
            else if (window.activeRightTab === 'codegen') displayCodegenOutput(selectedModel);

            if (result.error) {
                statusSpan.textContent = `Error (${elapsed}ms)`;
                statusSpan.style.color = '#c9184a';
                window.switchBottomTab('errors');
            } else {
                const b = result.balance;
                if (b) {
                    const balanceStatus = b.is_balanced ? 'BALANCED' :
                        (typeof b.status === 'string' ? b.status.toUpperCase() :
                        (b.status.CompileError ? 'ERROR' : 'UNBALANCED'));
                    const modelCountStr = models.length > 1 ? ` [${successCount}/${models.length}]` : '';
                    statusSpan.textContent = b.is_balanced
                        ? `Balanced (${elapsed}ms)${modelCountStr}`
                        : `${balanceStatus} (${elapsed}ms)${modelCountStr}`;
                    statusSpan.style.color = b.is_balanced ? '#2d6a4f' : '#c9184a';
                }
            }
        } else if (selectedModel && models.includes(selectedModel)) {
            setDaeOutput(`Waiting for compilation of ${selectedModel}...`);
            statusSpan.textContent = `${elapsed}ms`;
            statusSpan.style.color = '#888';
        } else {
            setDaeOutput(models.length === 0 ? 'No models found in source' : 'Select a model');
            statusSpan.textContent = models.length === 0 ? 'No models' : `${elapsed}ms`;
            statusSpan.style.color = '#888';
        }

        // Refresh CodeLens to show balance for all models
        if (window.refreshCodeLens) window.refreshCodeLens();
        if (document.getElementById('packagesSection').classList.contains('active')) {
            libraryDocsController.refreshPackageViewer(true);
        }
        refreshSimulationSettingsModalIfOpen();
        } catch (unexpectedError) {
            if (isStaleRun()) return;
            // Catch any unexpected errors to ensure status is always updated
            console.error('[runLiveChecks] Unexpected error:', unexpectedError);
            statusSpan.textContent = 'Error';
            statusSpan.style.color = '#c9184a';
            updateCompileErrors([{ model: 'live-check', message: String(unexpectedError?.message || unexpectedError) }]);
        } finally {
            updateGlobalStatusBar();
        }
    }, 500);
    window.triggerCompileNow = () => {
        runLiveChecks();
    };

    editor.onDidChangeModelContent(() => {
        projectFs.updateActiveDocumentContent(editor.getValue());
        if (window.refreshModelicaSemanticTokens) window.refreshModelicaSemanticTokens();
        updateSourceBreadcrumbs();
        if (suspendWorkspaceObservers) return;
        if (isRumocaSmokeMode()) return;
        runLiveChecks();
    });

    editor.onDidChangeCursorPosition(() => {
        updateSourceBreadcrumbs();
    });

    window.nextProblem = () => navigateProblems(1);
    window.previousProblem = () => navigateProblems(-1);

    window.addEventListener('keydown', event => {
        if (event.key !== 'F8') return;
        if (event.ctrlKey || event.metaKey || event.altKey) return;
        const target = event.target;
        if (
            target instanceof Element
            && (target.isContentEditable
                || ['INPUT', 'TEXTAREA', 'SELECT'].includes(target.tagName))
            && !(editor && typeof editor.hasTextFocus === 'function' && editor.hasTextFocus())
        ) {
            return;
        }
        event.preventDefault();
        navigateProblems(event.shiftKey ? -1 : 1);
    });

    setTimeout(() => {
        updateSourceBreadcrumbs();
        if (isRumocaSmokeMode()) return;
        runLiveChecks();
    }, 1000);

    // Template editor change listener - re-render when template changes (if codegen tab is active)
    let templateDebounceTimer = null;
    templateEditor.onDidChangeModelContent(() => {
        if (suspendWorkspaceObservers) return;
        if (window.activeRightTab !== 'codegen') return;
        if (templateDebounceTimer) clearTimeout(templateDebounceTimer);
        templateDebounceTimer = setTimeout(() => {
            const modelName = document.getElementById('modelSelect').value;
            if (modelName && window.compiledModels[modelName]) {
                displayCodegenOutput(modelName);
            }
        }, 300);
    });

    const smokeConfig = readRumocaSmokeConfig();
    if (smokeConfig) {
        void runRumocaBrowserSmoke(smokeConfig).catch(error => {
            console.error('[rumoca-smoke] browser smoke failed:', error);
        });
    }
});
