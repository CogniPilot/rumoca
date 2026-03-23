import { setupCommandPalette } from './modules/command_palette.js';
import { createSourceBreadcrumbs } from './modules/breadcrumbs.js';
import { createPackageArchiveController } from './modules/package_archive_controller.js';
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
    normalizePath,
} from './modules/project_fs.js';

let editor;
const projectFs = createProjectFilesystem();
const WASM_PROJECT_PERSIST_DB = 'rumoca-wasm-editor';
const WASM_PROJECT_PERSIST_STORE = 'autosave';
const WASM_PROJECT_PERSIST_KEY = 'active-project';
let projectPersistenceTimer = null;
let projectPersistenceInFlight = Promise.resolve();
let projectPersistenceReady = false;
const projectInterface = createProjectInterface({
    projectFs,
    runtimeBridge: {
        request(action, payload = {}, timeoutMs) {
            return sendRequest(action, payload, timeoutMs);
        },
    },
    onProjectMutation() {
        scheduleProjectPersistence();
    },
});
let simResultsPanelState = emptySimResultsPanelState();
let suspendWorkspaceObservers = false;
let defaultProjectSeed = null;
let openDocumentPaths = [];
const simulationSettingsModal = document.getElementById('simulationSettingsModal');
const simulationSettingsFrame = document.getElementById('simulationSettingsFrame');
const simulationSettingsCloseBtn = document.getElementById('simulationSettingsCloseBtn');
const codegenTemplateSummary = document.getElementById('codegenTemplateSummary');
const simRunTabs = document.getElementById('simRunTabs');
const outputTabsRoot = document.getElementById('outputTabs');
const codegenRunTabs = document.getElementById('codegenRunTabs');
const resultsSettingsBtn = document.getElementById('resultsSettingsBtn');
const resultsCloseBtn = document.getElementById('resultsCloseBtn');
const fileMenuButton = document.getElementById('fileMenuButton');
const fileMenuPanel = document.getElementById('fileMenuPanel');
const packageArchiveInput = document.getElementById('packageArchiveInput');
const sidebarBackdrop = document.getElementById('sidebarBackdrop');
const mobileSidebarBtn = document.getElementById('mobileSidebarBtn');
const mobileProjectBtn = document.getElementById('mobileProjectBtn');
const mobileAppbarTitle = document.getElementById('mobileAppbarTitle');
const workbenchSidepanel = document.getElementById('workbenchSidepanel');
const sidebarExplorerBtn = document.getElementById('sidebarExplorerBtn');
const sidebarProjectBtn = document.getElementById('sidebarProjectBtn');
const sidebarEditorPaneBtn = document.getElementById('sidebarEditorPaneBtn');
const sidebarResultsPaneBtn = document.getElementById('sidebarResultsPaneBtn');
const sidebarCodegenPaneBtn = document.getElementById('sidebarCodegenPaneBtn');
const workbenchSidebar = document.getElementById('workbenchSidebar');
const explorerSidebarPanel = document.getElementById('explorerSidebarPanel');
const projectSidebarPanel = document.getElementById('projectSidebarPanel');
const explorerSection = document.getElementById('explorerSection');
const outlineSection = document.getElementById('outlineSection');
const explorerSectionToggle = document.getElementById('explorerSectionToggle');
const outlineSectionToggle = document.getElementById('outlineSectionToggle');
const explorerSectionArrow = document.getElementById('explorerSectionArrow');
const outlineSectionArrow = document.getElementById('outlineSectionArrow');
const explorerNewFileBtn = document.getElementById('explorerNewFileBtn');
const explorerNewFolderBtn = document.getElementById('explorerNewFolderBtn');
const explorerTreeToggleBtn = document.getElementById('explorerTreeToggleBtn');
const outlineTreeToggleBtn = document.getElementById('outlineTreeToggleBtn');
const resizeHandleSidebar = document.getElementById('resizeHandleSidebar');
const resizeHandleSidebarV = document.getElementById('resizeHandleSidebarV');
const explorerTree = document.getElementById('explorerTree');
const outlineTree = document.getElementById('outlineTree');
const editorWorkbench = document.getElementById('editorWorkbench');
const editorPaneArea = document.getElementById('editorPaneArea');
const primaryEditorStack = document.getElementById('primaryEditorStack');
const secondaryEditorStack = document.getElementById('secondaryEditorStack');
const primaryEditorEmpty = document.getElementById('primaryEditorEmpty');
const secondaryEditorEmpty = document.getElementById('secondaryEditorEmpty');
const editorTabsRoot = document.getElementById('editorTabs');
const secondaryEditorTabsRoot = document.getElementById('secondaryEditorTabs');
const editorSplitHandle = document.getElementById('editorSplitHandle');
const editorDropOverlay = document.getElementById('editorDropOverlay');
const editorDropZones = Array.from(document.querySelectorAll('.editor-drop-zone'));
const editorRunButtons = Array.from(document.querySelectorAll('[data-editor-run-btn]'));
const editorSettingsButtons = Array.from(document.querySelectorAll('[data-editor-settings-btn]'));
const editorCloseButtons = Array.from(document.querySelectorAll('[data-editor-close-btn]'));
const rightPanel = document.getElementById('rightPanel');
const sidebarContextMenu = document.getElementById('sidebarContextMenu');
const sidebarContextActionBtn = document.getElementById('sidebarContextActionBtn');
const projectVisualizationStorage = buildProjectVisualizationViewStorage({ projectFs });
const editorViewStates = new Map();
const editorPanes = {
    primary: { id: 'primary', stackEl: primaryEditorStack, tabsEl: editorTabsRoot, editorElId: 'editor', paths: [], activePath: '', editor: null },
    secondary: { id: 'secondary', stackEl: secondaryEditorStack, tabsEl: secondaryEditorTabsRoot, editorElId: 'secondaryEditor', paths: [], activePath: '', editor: null },
};
let activeEditorPaneId = 'primary';
let editorPaneSplit = 'single';
let editorPaneVisible = true;
let isResizingEditorSplit = false;
let dragEditorTabState = null;
let monacoApi = null;
let createSourceEditorFactory = null;
let bindPaneEditorToWorkspace = null;
let outlineRenderVersion = 0;
let outlineRefreshTimer = null;
const explorerCollapsedNodes = new Set();
const outlineCollapsedNodes = new Set();
let explorerBranchKeys = [];
let outlineBranchKeys = [];
let selectedExplorerPath = '';
let sidebarContextAction = null;
const EXPLORER_ROOT_SELECTION = '.';
const DEFAULT_CODEGEN_TEMPLATE_ID = 'sympy.py.jinja';
const LEGACY_CUSTOM_TEMPLATE_PATH = '.rumoca/templates/custom_codegen.jinja';
let builtInCodegenTemplates = [];
let builtInCodegenTemplatesLoaded = false;
let codegenSettings = defaultCodegenSettings();
let codegenRuns = [];
let activeCodegenRunId = '';
let codegenRunSequence = 0;

function trimMaybeString(value) {
    return typeof value === 'string' ? value.trim() : '';
}

function defaultCodegenSettings() {
    return {
        mode: 'builtin',
        builtinTemplateId: DEFAULT_CODEGEN_TEMPLATE_ID,
        customTemplatePath: '',
    };
}

function normalizeCodegenSettings(value) {
    const next = value && typeof value === 'object' ? value : {};
    return {
        mode: next.mode === 'custom' ? 'custom' : 'builtin',
        builtinTemplateId: trimMaybeString(next.builtinTemplateId) || DEFAULT_CODEGEN_TEMPLATE_ID,
        customTemplatePath: trimMaybeString(next.customTemplatePath),
    };
}

function inferCodegenLanguage(templateId) {
    const nextId = trimMaybeString(templateId).toLowerCase();
    if (nextId.endsWith('.py.jinja')) return 'python';
    if (nextId.endsWith('.jl.jinja')) return 'julia';
    if (nextId.endsWith('.c.jinja') || nextId.endsWith('.h.jinja')) return 'c';
    if (nextId.endsWith('.xml.jinja')) return 'xml';
    if (nextId.endsWith('.mo.jinja')) return 'modelica';
    if (nextId.endsWith('.json.jinja')) return 'json';
    if (nextId.endsWith('.html.jinja')) return 'html';
    return 'plaintext';
}

function findBuiltInCodegenTemplate(templateId) {
    const nextId = trimMaybeString(templateId);
    if (!nextId) {
        return builtInCodegenTemplates[0] || null;
    }
    return builtInCodegenTemplates.find((template) => template.id === nextId) || builtInCodegenTemplates[0] || null;
}

async function ensureBuiltInCodegenTemplatesLoaded() {
    if (builtInCodegenTemplatesLoaded) {
        return builtInCodegenTemplates;
    }
    const raw = await sendWorkspaceCommand('rumoca.workspace.getBuiltinTemplates', {});
    const templates = Array.isArray(raw) ? raw : [];
    builtInCodegenTemplates = templates
        .map((entry) => ({
            id: trimMaybeString(entry?.id),
            label: trimMaybeString(entry?.label),
            language: trimMaybeString(entry?.language) || 'plaintext',
            source: typeof entry?.source === 'string' ? entry.source : '',
        }))
        .filter((entry) => entry.id && entry.label && entry.source);
    builtInCodegenTemplatesLoaded = true;
    if (!findBuiltInCodegenTemplate(codegenSettings.builtinTemplateId) && builtInCodegenTemplates[0]) {
        codegenSettings = {
            ...codegenSettings,
            builtinTemplateId: builtInCodegenTemplates[0].id,
        };
    }
    refreshCodegenTemplateSummary();
    return builtInCodegenTemplates;
}

function refreshCodegenTemplateSummary() {
    if (!codegenTemplateSummary) {
        return;
    }
    const activeRun = activeCodegenRun();
    if (activeRun) {
        codegenTemplateSummary.textContent = activeRun.templateLabel
            ? `Rendered with: ${activeRun.templateLabel}`
            : 'Rendered output snapshot';
        return;
    }
    if (codegenSettings.mode === 'custom') {
        codegenTemplateSummary.textContent = codegenSettings.customTemplatePath
            ? `Custom template: ${codegenSettings.customTemplatePath}`
            : 'Custom template: choose a workspace file';
        return;
    }
    const selectedBuiltin = findBuiltInCodegenTemplate(codegenSettings.builtinTemplateId);
    codegenTemplateSummary.textContent = selectedBuiltin
        ? `Built-in template: ${selectedBuiltin.label}`
        : 'Built-in template: loading...';
}

function currentCodegenModel() {
    return trimMaybeString(window.selectedModel || currentSimulationModel());
}

function formatCodegenRunLabel(modelName, createdAt) {
    const timeLabel = new Date(createdAt).toLocaleTimeString([], {
        hour: '2-digit',
        minute: '2-digit',
        second: '2-digit',
    });
    return `${trimMaybeString(modelName) || 'Model'} Codegen - ${timeLabel}`;
}

function activeCodegenRun() {
    return codegenRuns.find((run) => run.id === activeCodegenRunId) || null;
}

function showActiveCodegenRun() {
    const run = activeCodegenRun();
    if (!run) {
        setCodegenOutput('', 'plaintext');
        refreshCodegenTemplateSummary();
        return;
    }
    setCodegenOutput(run.text || '', run.language || 'plaintext');
    refreshCodegenTemplateSummary();
}

function renderCodegenRunTabs() {
    if (!codegenRunTabs) {
        return;
    }
    codegenRunTabs.innerHTML = '';
    for (const run of codegenRuns) {
        const shell = document.createElement('div');
        shell.className = `results-run-tab-shell${run.id === activeCodegenRunId ? ' active' : ''}`;

        const tab = document.createElement('button');
        tab.type = 'button';
        tab.className = `results-run-tab${run.id === activeCodegenRunId ? ' active' : ''}`;
        tab.setAttribute('role', 'tab');
        tab.setAttribute('aria-selected', String(run.id === activeCodegenRunId));
        tab.title = run.label;
        tab.textContent = run.label;
        tab.addEventListener('click', () => {
            activeCodegenRunId = run.id;
            window.switchRightTab('codegen');
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
            closeCodegenRun(run.id);
        });
        shell.appendChild(close);
        codegenRunTabs.appendChild(shell);
    }
}

async function applyCodegenSettings(nextSettings, { rerender = true } = {}) {
    if (codegenSettings.mode === nextSettings.mode
        && codegenSettings.builtinTemplateId === nextSettings.builtinTemplateId
        && codegenSettings.customTemplatePath === nextSettings.customTemplatePath) {
        refreshCodegenTemplateSummary();
        return;
    }
    codegenSettings = normalizeCodegenSettings(nextSettings);
    refreshCodegenTemplateSummary();
    scheduleProjectPersistence();
    void rerender;
}

function migrateLegacyCodegenTemplateState(nextState) {
    if (!nextState || typeof nextState !== 'object') {
        return null;
    }
    if (nextState.codegenSettings && typeof nextState.codegenSettings === 'object') {
        return null;
    }
    const legacyTemplate = typeof nextState.template === 'string' ? nextState.template : '';
    if (!trimMaybeString(legacyTemplate)) {
        return null;
    }
    if (projectFs.getFileContent(LEGACY_CUSTOM_TEMPLATE_PATH) !== legacyTemplate) {
        projectFs.setFile(LEGACY_CUSTOM_TEMPLATE_PATH, legacyTemplate);
    }
    return {
        mode: 'custom',
        builtinTemplateId: DEFAULT_CODEGEN_TEMPLATE_ID,
        customTemplatePath: LEGACY_CUSTOM_TEMPLATE_PATH,
    };
}

async function resolveCodegenTemplateSelection() {
    if (codegenSettings.mode === 'custom') {
        const templatePath = trimMaybeString(codegenSettings.customTemplatePath);
        if (!templatePath) {
            throw new Error('Choose a custom template file in Rumoca settings.');
        }
        const templateSource = projectFs.getFileContent(templatePath);
        if (typeof templateSource !== 'string') {
            throw new Error(`Template file not found: ${templatePath}`);
        }
        return {
            source: templateSource,
            label: templatePath,
            language: inferCodegenLanguage(templatePath),
        };
    }
    const selectedBuiltin = findBuiltInCodegenTemplate(codegenSettings.builtinTemplateId);
    if (!selectedBuiltin) {
        throw new Error('No built-in codegen templates are available.');
    }
    return {
        source: selectedBuiltin.source,
        label: selectedBuiltin.label,
        language: selectedBuiltin.language || inferCodegenLanguage(selectedBuiltin.id),
    };
}

function normalizeStringMap(value) {
    const next = {};
    if (!value || typeof value !== 'object' || Array.isArray(value)) {
        return next;
    }
    for (const [key, entry] of Object.entries(value)) {
        const nextKey = trimMaybeString(key);
        const nextValue = trimMaybeString(entry);
        if (nextKey && nextValue) {
            next[nextKey] = nextValue;
        }
    }
    return next;
}

function parseBadgeNumber(text) {
    const match = String(text || '').match(/\d+/);
    return match ? Number(match[0]) : 0;
}

function isProjectPersistenceEnabled() {
    return !isRumocaSmokeMode()
        && typeof window !== 'undefined'
        && typeof window.indexedDB !== 'undefined';
}

function indexedDbRequest(request) {
    return new Promise((resolve, reject) => {
        request.onsuccess = () => resolve(request.result);
        request.onerror = () => reject(request.error || new Error('IndexedDB request failed'));
    });
}

function indexedDbTransactionDone(transaction) {
    return new Promise((resolve, reject) => {
        transaction.oncomplete = () => resolve();
        transaction.onerror = () => reject(transaction.error || new Error('IndexedDB transaction failed'));
        transaction.onabort = () => reject(transaction.error || new Error('IndexedDB transaction aborted'));
    });
}

function openProjectPersistenceDb() {
    return new Promise((resolve, reject) => {
        const request = window.indexedDB.open(WASM_PROJECT_PERSIST_DB, 1);
        request.onupgradeneeded = () => {
            const db = request.result;
            if (!db.objectStoreNames.contains(WASM_PROJECT_PERSIST_STORE)) {
                db.createObjectStore(WASM_PROJECT_PERSIST_STORE);
            }
        };
        request.onsuccess = () => resolve(request.result);
        request.onerror = () => reject(request.error || new Error('Failed to open IndexedDB'));
    });
}

async function loadPersistedProjectEntries() {
    if (!isProjectPersistenceEnabled()) {
        return null;
    }
    const db = await openProjectPersistenceDb();
    try {
        const transaction = db.transaction(WASM_PROJECT_PERSIST_STORE, 'readonly');
        const store = transaction.objectStore(WASM_PROJECT_PERSIST_STORE);
        const record = await indexedDbRequest(store.get(WASM_PROJECT_PERSIST_KEY));
        await indexedDbTransactionDone(transaction);
        return Array.isArray(record?.entries) ? record.entries : null;
    } finally {
        db.close();
    }
}

async function savePersistedProjectEntries(entries) {
    if (!isProjectPersistenceEnabled()) {
        return;
    }
    const db = await openProjectPersistenceDb();
    try {
        const transaction = db.transaction(WASM_PROJECT_PERSIST_STORE, 'readwrite');
        const store = transaction.objectStore(WASM_PROJECT_PERSIST_STORE);
        await indexedDbRequest(store.put({
            schemaVersion: 1,
            savedAt: Date.now(),
            entries,
        }, WASM_PROJECT_PERSIST_KEY));
        await indexedDbTransactionDone(transaction);
    } finally {
        db.close();
    }
}

function collectPersistedProjectEntries() {
    projectFs.setEditorState(collectProjectEditorState());
    if (editor?.getValue) {
        persistActivePaneDocument();
    }
    return projectFs.snapshotArchiveEntries({ includeCacheFiles: true });
}

async function persistProjectToBrowserStorage() {
    if (!isProjectPersistenceEnabled()) {
        return;
    }
    try {
        await savePersistedProjectEntries(collectPersistedProjectEntries());
    } catch (error) {
        console.warn('Failed to persist WASM project state:', error);
    }
}

function enqueueProjectPersistence() {
    projectPersistenceInFlight = projectPersistenceInFlight
        .catch(() => {})
        .then(() => persistProjectToBrowserStorage());
    return projectPersistenceInFlight;
}

function scheduleProjectPersistence(delayMs = 250) {
    if (!isProjectPersistenceEnabled() || !projectPersistenceReady) {
        return;
    }
    if (projectPersistenceTimer) {
        clearTimeout(projectPersistenceTimer);
    }
    projectPersistenceTimer = setTimeout(() => {
        projectPersistenceTimer = null;
        void enqueueProjectPersistence();
    }, delayMs);
}

function flushProjectPersistence() {
    if (!isProjectPersistenceEnabled() || !projectPersistenceReady) {
        return Promise.resolve();
    }
    if (projectPersistenceTimer) {
        clearTimeout(projectPersistenceTimer);
        projectPersistenceTimer = null;
        void enqueueProjectPersistence();
    }
    return projectPersistenceInFlight.catch(() => {});
}

function emptySimResultsPanelState() {
    return {
        activeViewId: null,
        activeRunIdByModel: {},
        activeViewIdByRun: {},
    };
}

function normalizeSimResultsPanelState(value, fallbackActiveViewId = null) {
    const next = emptySimResultsPanelState();
    const legacyActiveViewId = trimMaybeString(fallbackActiveViewId) || null;
    if (!value || typeof value !== 'object' || Array.isArray(value)) {
        next.activeViewId = legacyActiveViewId;
        return next;
    }
    next.activeViewId = trimMaybeString(value.activeViewId) || legacyActiveViewId;
    next.activeRunIdByModel = normalizeStringMap(value.activeRunIdByModel);
    next.activeViewIdByRun = normalizeStringMap(value.activeViewIdByRun);
    return next;
}

function pathParts(path) {
    return String(path || '').split('/').filter(Boolean);
}

function baseName(path) {
    return pathParts(path).at(-1) || '';
}

function parentDirectory(path) {
    const parts = pathParts(path);
    return parts.length > 1 ? parts.slice(0, -1).join('/') : '';
}

function isExplorerTextEntry(entry) {
    return (
        entry?.sourceKind === 'workspace'
        || entry?.sourceKind === 'packageArchive'
        || entry?.sourceKind === 'sidecar'
    ) && entry?.isText === true;
}

function normalizeOpenDocumentPaths(paths, fallbackPath = '') {
    const next = [];
    const seen = new Set();
    const pushPath = (candidate) => {
        const normalized = trimMaybeString(candidate);
        if (!normalized || seen.has(normalized)) {
            return;
        }
        if (typeof projectFs.getFileContent(normalized) !== 'string') {
            return;
        }
        seen.add(normalized);
        next.push(normalized);
    };
    if (Array.isArray(paths)) {
        for (const path of paths) {
            pushPath(path);
        }
    }
    pushPath(fallbackPath);
    return next;
}

function otherEditorPaneId(paneId) {
    return paneId === 'secondary' ? 'primary' : 'secondary';
}

function normalizeEditorPaneId(paneId) {
    return paneId === 'secondary' ? 'secondary' : 'primary';
}

function getEditorPane(paneId = activeEditorPaneId) {
    return editorPanes[normalizeEditorPaneId(paneId)];
}

function rebuildOpenDocumentPaths() {
    openDocumentPaths = normalizeOpenDocumentPaths([
        ...editorPanes.primary.paths,
        ...editorPanes.secondary.paths,
    ]);
}

function syncOpenDocuments(nextPaths = openDocumentPaths, fallbackPath = '') {
    editorPanes.primary.paths = normalizeOpenDocumentPaths(nextPaths, fallbackPath);
    editorPanes.primary.activePath = trimMaybeString(fallbackPath)
        || editorPanes.primary.paths.at(-1)
        || '';
    editorPanes.secondary.paths = [];
    editorPanes.secondary.activePath = '';
    editorPaneSplit = 'single';
    rebuildOpenDocumentPaths();
}

function setPanePaths(paneId, nextPaths, fallbackPath = '') {
    const pane = getEditorPane(paneId);
    pane.paths = normalizeOpenDocumentPaths(nextPaths, fallbackPath);
    pane.activePath = trimMaybeString(fallbackPath)
        || pane.paths.at(-1)
        || '';
    rebuildOpenDocumentPaths();
}

function removePathFromOtherEditorPane(path, keepPaneId) {
    const nextPath = trimMaybeString(path);
    const keepId = normalizeEditorPaneId(keepPaneId);
    for (const [paneId, pane] of Object.entries(editorPanes)) {
        if (paneId === keepId) {
            continue;
        }
        if (!pane.paths.includes(nextPath)) {
            continue;
        }
        pane.paths = pane.paths.filter((candidate) => candidate !== nextPath);
        if (pane.activePath === nextPath) {
            pane.activePath = pane.paths.at(-1) || '';
        }
    }
    rebuildOpenDocumentPaths();
}

function hasSecondaryEditorPane() {
    return editorPaneSplit !== 'single';
}

function sharedVisualization() {
    const shared = globalThis.RumocaVisualizationShared;
    if (!shared) {
        throw new Error('RumocaVisualizationShared not loaded');
    }
    return shared;
}

function setFileMenuOpen(isOpen) {
    if (!fileMenuButton || !fileMenuPanel) {
        return;
    }
    fileMenuButton.setAttribute('aria-expanded', String(Boolean(isOpen)));
    fileMenuPanel.classList.toggle('open', Boolean(isOpen));
}

function currentSidebarMode() {
    return workbenchSidebar?.dataset.mode === 'project' ? 'project' : 'explorer';
}

function syncWorkbenchPaneButtons() {
    if (sidebarEditorPaneBtn) {
        const active = editorPaneVisible && hasOpenEditorPane();
        sidebarEditorPaneBtn.classList.toggle('active', active);
        sidebarEditorPaneBtn.setAttribute('aria-pressed', String(active));
    }
    if (sidebarResultsPaneBtn) {
        const active = window.activeRightTab === 'simulate';
        sidebarResultsPaneBtn.classList.toggle('active', active);
        sidebarResultsPaneBtn.setAttribute('aria-pressed', String(active));
    }
    if (sidebarCodegenPaneBtn) {
        const active = window.activeRightTab === 'codegen';
        sidebarCodegenPaneBtn.classList.toggle('active', active);
        sidebarCodegenPaneBtn.setAttribute('aria-pressed', String(active));
    }
}

function updateMobileAppbarTitle() {
    if (!mobileAppbarTitle) {
        return;
    }
    const activePath = trimMaybeString(projectFs.getActiveDocumentPath());
    const title = currentSidebarMode() === 'project'
        ? 'Project'
        : activePath
            ? baseName(activePath) || activePath
            : 'Rumoca';
    mobileAppbarTitle.textContent = title;
}

function syncSidebarBackdrop() {
    if (!sidebarBackdrop) {
        return;
    }
    const open = isNarrowLayout() && !workbenchSidepanel?.classList.contains('collapsed');
    sidebarBackdrop.hidden = !open;
}

function syncSidebarModeButtons() {
    const collapsed = workbenchSidepanel?.classList.contains('collapsed');
    const mode = currentSidebarMode();
    if (sidebarExplorerBtn) {
        const active = !collapsed && mode === 'explorer';
        const expanded = !collapsed && mode === 'explorer';
        sidebarExplorerBtn.classList.toggle('active', active);
        sidebarExplorerBtn.setAttribute('aria-expanded', String(expanded));
        sidebarExplorerBtn.setAttribute('aria-pressed', String(active));
        sidebarExplorerBtn.title = active ? 'Hide Explorer sidebar' : 'Show Explorer sidebar';
        sidebarExplorerBtn.setAttribute('aria-label', sidebarExplorerBtn.title);
    }
    if (sidebarProjectBtn) {
        const active = !collapsed && mode === 'project';
        const expanded = !collapsed && mode === 'project';
        sidebarProjectBtn.classList.toggle('active', active);
        sidebarProjectBtn.setAttribute('aria-expanded', String(expanded));
        sidebarProjectBtn.setAttribute('aria-pressed', String(active));
        sidebarProjectBtn.title = active ? 'Hide Project sidebar' : 'Show Project sidebar';
        sidebarProjectBtn.setAttribute('aria-label', sidebarProjectBtn.title);
    }
    if (mobileSidebarBtn) {
        const active = !collapsed && mode === 'explorer';
        mobileSidebarBtn.classList.toggle('active', active);
        mobileSidebarBtn.setAttribute('aria-expanded', String(active));
    }
    if (mobileProjectBtn) {
        const active = !collapsed && mode === 'project';
        mobileProjectBtn.classList.toggle('active', active);
        mobileProjectBtn.setAttribute('aria-expanded', String(active));
    }
    updateMobileAppbarTitle();
}

function setSidebarMode(mode, { persist = true } = {}) {
    const nextMode = mode === 'project' ? 'project' : 'explorer';
    if (workbenchSidebar) {
        workbenchSidebar.dataset.mode = nextMode;
    }
    explorerSidebarPanel?.classList.toggle('pane-hidden', nextMode !== 'explorer');
    projectSidebarPanel?.classList.toggle('pane-hidden', nextMode !== 'project');
    syncSidebarModeButtons();
    if (persist) {
        scheduleProjectPersistence();
    }
}

function setSidebarCollapsed(collapsed) {
    if (!workbenchSidepanel) {
        return;
    }
    const nextCollapsed = Boolean(collapsed);
    workbenchSidepanel.classList.toggle('collapsed', nextCollapsed);
    syncSidebarModeButtons();
    syncSidebarBackdrop();
    updateSidebarSplitHandleVisibility();
    scheduleProjectPersistence();
}

function toggleSidebarCollapsed() {
    setSidebarCollapsed(!workbenchSidepanel?.classList.contains('collapsed'));
    layoutAllEditors();
}

function setSidebarSectionCollapsed(sectionName, collapsed) {
    const normalized = sectionName === 'outline' ? 'outline' : 'explorer';
    const section = normalized === 'outline' ? outlineSection : explorerSection;
    const toggle = normalized === 'outline' ? outlineSectionToggle : explorerSectionToggle;
    const arrow = normalized === 'outline' ? outlineSectionArrow : explorerSectionArrow;
    if (!section || !toggle || !arrow) {
        return;
    }
    const nextCollapsed = Boolean(collapsed);
    section.classList.toggle('collapsed', nextCollapsed);
    toggle.setAttribute('aria-expanded', String(!nextCollapsed));
    arrow.textContent = nextCollapsed ? '▸' : '▾';
    scheduleProjectPersistence();
}

function toggleSidebarSection(sectionName) {
    const section = sectionName === 'outline' ? outlineSection : explorerSection;
    if (!section) {
        return;
    }
    setSidebarSectionCollapsed(sectionName, !section.classList.contains('collapsed'));
    updateSidebarSplitHandleVisibility();
}

function closeSidebarContextMenu() {
    sidebarContextAction = null;
    if (!sidebarContextMenu || !sidebarContextActionBtn) {
        return;
    }
    sidebarContextMenu.classList.remove('open');
    sidebarContextMenu.style.left = '';
    sidebarContextMenu.style.top = '';
    sidebarContextActionBtn.textContent = '';
}

function openSidebarContextMenu(clientX, clientY, action) {
    if (!sidebarContextMenu || !sidebarContextActionBtn || !action) {
        return;
    }
    sidebarContextAction = action;
    sidebarContextActionBtn.textContent = action.label || 'Delete';
    sidebarContextActionBtn.classList.toggle('danger', action.danger !== false);
    sidebarContextMenu.classList.add('open');
    sidebarContextMenu.style.left = `${clientX}px`;
    sidebarContextMenu.style.top = `${clientY}px`;
    const rect = sidebarContextMenu.getBoundingClientRect();
    const nextLeft = Math.max(8, Math.min(clientX, window.innerWidth - rect.width - 8));
    const nextTop = Math.max(8, Math.min(clientY, window.innerHeight - rect.height - 8));
    sidebarContextMenu.style.left = `${nextLeft}px`;
    sidebarContextMenu.style.top = `${nextTop}px`;
}

function workspaceCreationBaseDirectory() {
    const selectedPath = trimMaybeString(selectedExplorerPath);
    if (selectedPath === EXPLORER_ROOT_SELECTION) {
        return '';
    }
    if (selectedPath) {
        const selectedEntry = projectFs.getFileEntry(selectedPath);
        if (selectedEntry?.sourceKind === 'workspace') {
            return parentDirectory(selectedPath);
        }
        const hasFolder = projectFs.listFolders().includes(selectedPath)
            || projectFs.listFileEntries().some((entry) => entry.path.startsWith(`${selectedPath}/`));
        if (hasFolder) {
            return selectedPath;
        }
    }
    const activePath = projectFs.getActiveDocumentPath();
    const activeEntry = projectFs.getFileEntry(activePath);
    if (activeEntry?.sourceKind === 'workspace') {
        return parentDirectory(activePath);
    }
    return '';
}

function defaultNewWorkspaceFilePath() {
    const baseDir = workspaceCreationBaseDirectory();
    return baseDir ? `${baseDir}/NewFile.mo` : 'NewFile.mo';
}

function defaultNewWorkspaceFolderPath() {
    const baseDir = workspaceCreationBaseDirectory();
    return baseDir ? `${baseDir}/NewFolder` : 'NewFolder';
}

function resolveWorkspaceCreationPath(inputPath) {
    const requestedPath = trimMaybeString(inputPath);
    if (!requestedPath) {
        return '';
    }
    const normalizedRequestedPath = normalizePath(requestedPath);
    if (!normalizedRequestedPath) {
        return '';
    }
    if (normalizedRequestedPath.includes('/')) {
        return normalizedRequestedPath;
    }
    const baseDir = workspaceCreationBaseDirectory();
    return baseDir ? `${baseDir}/${normalizedRequestedPath}` : normalizedRequestedPath;
}

function defaultWorkspaceFileContent(path) {
    if (!String(path || '').endsWith('.mo')) {
        return '';
    }
    if (baseName(path) === 'package.mo') {
        const packagePath = parentDirectory(path);
        const packageName = baseName(packagePath);
        if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(packageName)) {
            return '';
        }
        const enclosingPath = parentDirectory(packagePath);
        const withinLine = enclosingPath ? `within ${enclosingPath};\n` : 'within ;\n';
        return `${withinLine}package ${packageName}\nend ${packageName};\n`;
    }
    const stem = baseName(path).replace(/\.[^.]+$/, '');
    if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(stem)) {
        return '';
    }
    return `model ${stem}\nend ${stem};\n`;
}

function currentExplorerSelectionPath() {
    const selectedPath = trimMaybeString(selectedExplorerPath);
    if (selectedPath === EXPLORER_ROOT_SELECTION) {
        return '';
    }
    return selectedPath
        || (hasOpenEditorPane() ? projectFs.getActiveDocumentPath() : '');
}

function setSelectedExplorerPath(path) {
    selectedExplorerPath = trimMaybeString(path);
    updateExplorerActiveSelection(currentExplorerSelectionPath());
}

function expandWorkspaceExplorerPath(path, includeLeaf = false) {
    const parts = pathParts(path);
    let prefix = '';
    const limit = includeLeaf ? parts.length : Math.max(0, parts.length - 1);
    for (let index = 0; index < limit; index += 1) {
        prefix = prefix ? `${prefix}/${parts[index]}` : parts[index];
        explorerCollapsedNodes.delete(`explorer:${prefix}`);
    }
}

function pruneOpenDocuments(predicate, fallbackPath = projectFs.getActiveDocumentPath()) {
    syncOpenDocuments(openDocumentPaths.filter((path) => !predicate(path)), fallbackPath);
}

function closeTitlebarMenu() {
    setFileMenuOpen(false);
}

function toggleFileMenu() {
    if (!fileMenuPanel) {
        return;
    }
    setFileMenuOpen(!fileMenuPanel.classList.contains('open'));
}

window.closeTitlebarMenu = closeTitlebarMenu;
window.openPackageArchivePicker = function() {
    closeTitlebarMenu();
    packageArchiveInput?.click();
};
window.loadPackageArchiveInput = async function(input) {
    const file = input?.files?.[0];
    if (!file) {
        return;
    }
    try {
        if (typeof window.loadPackageArchiveFile !== 'function') {
            throw new Error('Package archive loader is unavailable.');
        }
        setTerminalOutput(`Loading package archive ${file.name}...`);
        await window.loadPackageArchiveFile(file);
    } catch (error) {
        const message = error?.message || String(error);
        setTerminalOutput(`Failed to load package archive: ${message}`);
        alert(`Failed to load package archive: ${message}`);
    } finally {
        if (input) {
            input.value = '';
        }
    }
};

const resultsPanelController = createResultsPanelController({
    root: document.getElementById('simPlot'),
    tabsRoot: document.getElementById('simRunTabs'),
    projectFs,
    projectInterface,
    onActivateRun(kind) {
        if (kind === 'simulate') {
            window.switchRightTab('simulate');
        }
    },
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
        scheduleProjectPersistence();
    },
});

function currentResultsModel() {
    return trimMaybeString(window.selectedModel || currentSimulationModel());
}

function hasOpenEditorPane() {
    return openDocumentPaths.length > 0;
}

function hasSimulationOutputPane() {
    return Boolean(resultsPanelController.getSimulationRun(currentResultsModel()));
}

function hasCodegenOutputPane() {
    return Boolean(activeCodegenRun());
}

function setActiveRightTabState(tabName) {
    window.activeRightTab = tabName === 'codegen' ? 'codegen' : tabName === 'simulate' ? 'simulate' : '';
    document.getElementById('simTab').classList.toggle('active', window.activeRightTab === 'simulate');
    document.getElementById('codegenTab').classList.toggle('active', window.activeRightTab === 'codegen');
}

function updateWorkbenchPaneVisibility() {
    const nextLeftArea = document.querySelector('.left-area');
    const nextResizeHandleH = document.getElementById('resizeHandleH');
    const nextRightPanel = document.getElementById('rightPanel');
    const nextWorkbenchTop = document.querySelector('.workbench-top') || document.querySelector('.main-container');
    if (!nextLeftArea || !nextResizeHandleH || !nextRightPanel || !nextWorkbenchTop) {
        return;
    }
    const showEditor = editorPaneVisible && hasOpenEditorPane();
    const showOutput = Boolean(window.activeRightTab);
    nextLeftArea.classList.toggle('pane-hidden', !showEditor);
    nextRightPanel.classList.toggle('pane-hidden', !showOutput);
    nextResizeHandleH.classList.toggle('pane-hidden', !(showEditor && showOutput));
    nextWorkbenchTop.dataset.layout = showEditor && showOutput
        ? 'split'
        : showEditor
            ? 'editor'
            : showOutput
            ? 'output'
                : 'empty';
    syncWorkbenchPaneButtons();
}

function activeResultsSettingsTrigger() {
    return document.querySelector('#simPlot .rumoca-results-header-button');
}

function refreshResultsWindowChrome() {
    const hasRun = Boolean(resultsPanelController.getSimulationRun(currentResultsModel()));
    const hasCodegenRun = Boolean(activeCodegenRun());
    if (outputTabsRoot) {
        outputTabsRoot.dataset.activePane = window.activeRightTab === 'codegen'
            ? 'codegen'
            : window.activeRightTab === 'simulate'
                ? 'simulate'
                : '';
    }
    renderCodegenRunTabs();
    if (resultsSettingsBtn) {
        resultsSettingsBtn.hidden = window.activeRightTab !== 'simulate';
        resultsSettingsBtn.disabled = !hasRun;
        resultsSettingsBtn.setAttribute('aria-disabled', String(!hasRun));
    }
    if (resultsCloseBtn) {
        const canClose = window.activeRightTab === 'codegen' ? hasCodegenRun : hasRun;
        resultsCloseBtn.hidden = !window.activeRightTab;
        resultsCloseBtn.disabled = !canClose;
        resultsCloseBtn.setAttribute('aria-disabled', String(!canClose));
        resultsCloseBtn.title = window.activeRightTab === 'codegen'
            ? 'Close active code generation output'
            : 'Close active results window';
        resultsCloseBtn.setAttribute(
            'aria-label',
            window.activeRightTab === 'codegen'
                ? 'Close active code generation output'
                : 'Close active results window',
        );
    }
    updateWorkbenchPaneVisibility();
}

window.openActiveResultsSettings = function() {
    const trigger = activeResultsSettingsTrigger();
    if (trigger instanceof HTMLElement) {
        trigger.click();
    }
};

window.closeActiveResultsRun = function() {
    if (window.activeRightTab === 'codegen') {
        closeCodegenRun(activeCodegenRunId);
        return;
    }
    resultsPanelController.closeActiveRun(currentResultsModel());
    refreshResultsWindowChrome();
};

if (simRunTabs) {
    const runTabsObserver = new MutationObserver(() => {
        refreshResultsWindowChrome();
    });
    runTabsObserver.observe(simRunTabs, {
        childList: true,
        subtree: true,
        attributes: true,
    });
}

void resultsPanelController.renderModel('');
refreshResultsWindowChrome();
refreshFileRunSettingsButton();

function addCodegenRun(run) {
    codegenRuns.push(run);
    activeCodegenRunId = run.id;
    renderCodegenRunTabs();
    showActiveCodegenRun();
}

function closeCodegenRun(runId) {
    const nextRunId = trimMaybeString(runId);
    if (!nextRunId) {
        return;
    }
    const closedIndex = codegenRuns.findIndex((run) => run.id === nextRunId);
    if (closedIndex < 0) {
        return;
    }
    codegenRuns.splice(closedIndex, 1);
    if (activeCodegenRunId === nextRunId) {
        const fallback = codegenRuns[Math.max(0, closedIndex - 1)] || codegenRuns[closedIndex] || null;
        activeCodegenRunId = fallback?.id || '';
    }
    if (!activeCodegenRunId) {
        setCodegenOutput('', 'plaintext');
    } else if (window.activeRightTab === 'codegen') {
        showActiveCodegenRun();
    }
    refreshCodegenTemplateSummary();
    refreshFileRunSettingsButton();
    refreshResultsWindowChrome();
    scheduleProjectPersistence();
}

function clearCodegenRuns() {
    codegenRuns = [];
    activeCodegenRunId = '';
    setCodegenOutput('', 'plaintext');
    refreshCodegenTemplateSummary();
    refreshResultsWindowChrome();
}

function editorPaneEmptyElement(paneId) {
    return normalizeEditorPaneId(paneId) === 'secondary' ? secondaryEditorEmpty : primaryEditorEmpty;
}

function paneFallbackPathAfterClosing(paneId, path) {
    const pane = getEditorPane(paneId);
    const nextPath = trimMaybeString(path);
    const remaining = pane.paths.filter((candidate) => candidate !== nextPath);
    if (remaining.length === 0) {
        return '';
    }
    const closedIndex = Math.max(0, pane.paths.indexOf(nextPath));
    return remaining[Math.min(closedIndex, remaining.length - 1)] || '';
}

function setEditorPaneSplit(split) {
    editorPaneSplit = split === 'horizontal' ? 'horizontal' : split === 'vertical' ? 'vertical' : 'single';
    if (editorPaneArea) {
        editorPaneArea.dataset.split = editorPaneSplit;
    }
    const showSecondary = editorPaneSplit !== 'single';
    secondaryEditorStack?.classList.toggle('pane-hidden', !showSecondary);
    editorSplitHandle?.classList.toggle('pane-hidden', !showSecondary);
    if (!showSecondary) {
        primaryEditorStack.style.width = '';
        primaryEditorStack.style.height = '';
    } else if (editorPaneSplit === 'horizontal') {
        primaryEditorStack.style.width = '';
    } else {
        primaryEditorStack.style.height = '';
    }
    primaryEditorStack?.classList.toggle('active', activeEditorPaneId === 'primary');
    secondaryEditorStack?.classList.toggle('active', activeEditorPaneId === 'secondary');
}

function refreshEditorPaneEmptyState(paneId) {
    const pane = getEditorPane(paneId);
    const emptyEl = editorPaneEmptyElement(paneId);
    if (!pane?.editor || !emptyEl) {
        return;
    }
    const isEmpty = pane.paths.length === 0 || !trimMaybeString(pane.activePath);
    pane.stackEl?.classList.toggle('empty', isEmpty);
    emptyEl.hidden = !isEmpty;
    const container = document.getElementById(pane.editorElId);
    if (container) {
        container.hidden = isEmpty;
    }
}

function persistActivePaneDocument() {
    const pane = getEditorPane(activeEditorPaneId);
    const activePath = trimMaybeString(pane?.activePath);
    if (!activePath || typeof pane?.editor?.getValue !== 'function') {
        return;
    }
    if (typeof pane.editor.saveViewState === 'function') {
        editorViewStates.set(activePath, pane.editor.saveViewState());
    }
    projectFs.setFile(activePath, pane.editor.getValue());
    projectFs.activateDocument(activePath);
}

function setActiveEditorPane(
    paneId,
    { focusEditor = false, refreshNavigation = true, outlineDelayMs = 0, persistCurrent = true } = {},
) {
    const nextPane = getEditorPane(paneId);
    if (!nextPane?.editor) {
        return;
    }
    if (persistCurrent) {
        persistActivePaneDocument();
    }
    activeEditorPaneId = nextPane.id;
    editor = nextPane.editor;
    window.editor = nextPane.editor;
    if (trimMaybeString(nextPane.activePath)) {
        projectFs.activateDocument(nextPane.activePath);
    }
    setEditorPaneSplit(editorPaneSplit);
    if (refreshNavigation) {
        refreshActiveDocumentNavigation({ outlineDelayMs });
    }
    applyActiveEditorLockState();
    if (focusEditor && typeof nextPane.editor.focus === 'function') {
        nextPane.editor.focus();
    }
}

function canCloseProjectDocument(path = projectFs.getActiveDocumentPath(), paneId = activeEditorPaneId) {
    const pane = getEditorPane(paneId);
    const nextPath = trimMaybeString(path);
    return Boolean(nextPath && pane?.paths.includes(nextPath));
}

function collapseSingleEmptyPaneIfPossible() {
    const primaryHasFiles = editorPanes.primary.paths.length > 0;
    const secondaryHasFiles = editorPanes.secondary.paths.length > 0;
    if (primaryHasFiles || secondaryHasFiles) {
        return;
    }
    setEditorPaneSplit('single');
    activeEditorPaneId = 'primary';
}

async function closeProjectDocument(path = projectFs.getActiveDocumentPath(), paneId = activeEditorPaneId) {
    const pane = getEditorPane(paneId);
    const nextPath = trimMaybeString(path);
    if (!pane || !nextPath || !pane.paths.includes(nextPath)) {
        return;
    }
    const fallbackPath = paneFallbackPathAfterClosing(pane.id, nextPath);
    editorViewStates.delete(nextPath);
    if (pane.id === activeEditorPaneId && pane.activePath === nextPath) {
        persistActivePaneDocument();
    }
    pane.paths = pane.paths.filter((candidate) => candidate !== nextPath);
    pane.activePath = fallbackPath;
    rebuildOpenDocumentPaths();
    if (fallbackPath) {
        await openProjectDocument(fallbackPath, {
            paneId: pane.id,
            focusEditor: pane.id === activeEditorPaneId,
            forceReload: true,
        });
    } else {
        refreshEditorPaneEmptyState(pane.id);
        collapseSingleEmptyPaneIfPossible();
        renderEditorTabs();
        refreshWorkbenchNavigation();
        scheduleProjectPersistence();
    }
}

window.closeActiveProjectDocument = () => closeProjectDocument(projectFs.getActiveDocumentPath(), activeEditorPaneId);
window.closeActivePaneDocument = (paneId) => closeProjectDocument(getEditorPane(paneId)?.activePath || '', paneId);

function activeDocumentEntry(path = projectFs.getActiveDocumentPath()) {
    const nextPath = trimMaybeString(path);
    if (!nextPath) {
        return null;
    }
    return projectFs.getFileEntry(nextPath);
}

function readDocumentLockStates() {
    const raw = projectFs.getEditorState()?.documentLockStates;
    if (!raw || typeof raw !== 'object' || Array.isArray(raw)) {
        return {};
    }
    const next = {};
    for (const [path, locked] of Object.entries(raw)) {
        const normalizedPath = trimMaybeString(path);
        if (!normalizedPath || typeof locked !== 'boolean') {
            continue;
        }
        next[normalizedPath] = locked;
    }
    return next;
}

function readFolderLockStates() {
    const raw = projectFs.getEditorState()?.folderLockStates;
    if (!raw || typeof raw !== 'object' || Array.isArray(raw)) {
        return {};
    }
    const next = {};
    for (const [path, locked] of Object.entries(raw)) {
        const normalizedPath = trimMaybeString(path);
        if (!normalizedPath || typeof locked !== 'boolean') {
            continue;
        }
        next[normalizedPath] = locked;
    }
    return next;
}

function defaultLockStateForEntry(entry) {
    return entry?.sourceKind === 'packageArchive';
}

function inheritedFolderLock(path) {
    const nextPath = trimMaybeString(path);
    if (!nextPath) {
        return null;
    }
    const folderLockStates = readFolderLockStates();
    let bestPath = '';
    let bestValue = null;
    for (const [folderPath, locked] of Object.entries(folderLockStates)) {
        if (nextPath === folderPath || nextPath.startsWith(`${folderPath}/`)) {
            if (folderPath.length > bestPath.length) {
                bestPath = folderPath;
                bestValue = locked;
            }
        }
    }
    return typeof bestValue === 'boolean' ? bestValue : null;
}

function isDocumentLocked(path = projectFs.getActiveDocumentPath()) {
    const nextPath = trimMaybeString(path);
    const entry = activeDocumentEntry(nextPath);
    const lockStates = readDocumentLockStates();
    if (Object.prototype.hasOwnProperty.call(lockStates, nextPath)) {
        return Boolean(lockStates[nextPath]);
    }
    const inherited = inheritedFolderLock(nextPath);
    if (typeof inherited === 'boolean') {
        return inherited;
    }
    return defaultLockStateForEntry(entry);
}

function setDocumentLocked(path, locked) {
    const nextPath = trimMaybeString(path);
    if (!nextPath) {
        return;
    }
    const existingState = projectFs.getEditorState() || {};
    projectFs.setEditorState({
        ...existingState,
        documentLockStates: {
            ...readDocumentLockStates(),
            [nextPath]: Boolean(locked),
        },
    });
}

function isFolderLocked(path, entries = projectFs.listFileEntries()) {
    const nextPath = trimMaybeString(path);
    if (!nextPath) {
        return false;
    }
    const folderLockStates = readFolderLockStates();
    if (Object.prototype.hasOwnProperty.call(folderLockStates, nextPath)) {
        return Boolean(folderLockStates[nextPath]);
    }
    const descendants = entries.filter(
        (entry) => entry.path === nextPath || entry.path.startsWith(`${nextPath}/`),
    );
    if (descendants.length === 0) {
        return false;
    }
    return descendants.every((entry) => isDocumentLocked(entry.path));
}

function setFolderLocked(path, locked) {
    const nextPath = trimMaybeString(path);
    if (!nextPath) {
        return;
    }
    const existingState = projectFs.getEditorState() || {};
    const documentLockStates = readDocumentLockStates();
    for (const candidatePath of Object.keys(documentLockStates)) {
        if (candidatePath === nextPath || candidatePath.startsWith(`${nextPath}/`)) {
            delete documentLockStates[candidatePath];
        }
    }
    const folderLockStates = readFolderLockStates();
    for (const candidatePath of Object.keys(folderLockStates)) {
        if (candidatePath.startsWith(`${nextPath}/`)) {
            delete folderLockStates[candidatePath];
        }
    }
    folderLockStates[nextPath] = Boolean(locked);
    projectFs.setEditorState({
        ...existingState,
        documentLockStates,
        folderLockStates,
    });
}

function applyActiveEditorLockState() {
    if (!editor) {
        return;
    }
    const entry = activeDocumentEntry();
    const locked = isDocumentLocked(entry?.path || '');
    if (typeof editor.updateOptions === 'function') {
        editor.updateOptions({ readOnly: locked });
    }
}

function toggleDocumentLock(path = projectFs.getActiveDocumentPath()) {
    const nextPath = trimMaybeString(path);
    if (!nextPath) {
        return;
    }
    setDocumentLocked(nextPath, !isDocumentLocked(nextPath));
    applyActiveEditorLockState();
    refreshWorkbenchNavigation();
    scheduleProjectPersistence();
}

function toggleFolderLock(path) {
    const nextPath = trimMaybeString(path);
    if (!nextPath) {
        return;
    }
    setFolderLocked(nextPath, !isFolderLocked(nextPath));
    applyActiveEditorLockState();
    refreshWorkbenchNavigation();
    scheduleProjectPersistence();
}

window.toggleActiveEditorLock = function() {
    toggleDocumentLock(projectFs.getActiveDocumentPath());
};

function appendSidebarEmpty(container, message) {
    container.innerHTML = '';
    const empty = document.createElement('div');
    empty.className = 'sidebar-empty';
    empty.textContent = message;
    container.appendChild(empty);
}

function appendSidebarRow(container, {
    nodeId = '',
    ancestorIds = [],
    kind = 'file',
    label,
    path = '',
    depth = 0,
    meta = '',
    active = false,
    onClick = null,
    sourceKind = 'workspace',
    sourceBadge = '',
    hasChildren = false,
    collapsed = false,
    onContextMenu = null,
    action = null,
    hidden = false,
}) {
    const shell = document.createElement('div');
    shell.className = `sidebar-row-shell${active ? ' active' : ''}`;
    if (nodeId) {
        shell.dataset.nodeId = nodeId;
    }
    if (Array.isArray(ancestorIds) && ancestorIds.length > 0) {
        shell.dataset.ancestorIds = ancestorIds.join('|');
    }
    shell.dataset.hasChildren = String(Boolean(hasChildren));
    shell.hidden = Boolean(hidden);
    if (path) {
        shell.dataset.path = path;
    }

    const row = document.createElement('button');
    row.type = 'button';
    row.className = `sidebar-row${kind === 'dir' ? ' dir' : ''}${sourceKind === 'packageArchive' ? ' package-archive' : ''}${active ? ' active' : ''}`;
    row.style.paddingLeft = `${12 + depth * 14}px`;
    if (path) {
        row.dataset.path = path;
    }
    if (typeof onClick === 'function') {
        row.addEventListener('click', onClick);
    } else {
        row.setAttribute('aria-disabled', 'true');
    }
    if (typeof onContextMenu === 'function') {
        row.addEventListener('contextmenu', (event) => {
            event.preventDefault();
            event.stopPropagation();
            onContextMenu(event);
        });
    }

    const expander = document.createElement('span');
    expander.className = 'expander';
    expander.textContent = hasChildren ? (collapsed ? '▸' : '▾') : '';
    row.appendChild(expander);

    const icon = document.createElement('span');
    icon.className = 'icon';
    row.appendChild(icon);

    const text = document.createElement('span');
    text.className = 'label';
    text.textContent = label;
    row.appendChild(text);

    if (meta) {
        const metaNode = document.createElement('span');
        metaNode.className = 'meta';
        metaNode.textContent = meta;
        row.appendChild(metaNode);
    }

    if (sourceBadge) {
        const badgeNode = document.createElement('span');
        badgeNode.className = 'source-badge';
        badgeNode.textContent = sourceBadge;
        row.appendChild(badgeNode);
    }

    shell.appendChild(row);

    if (action && typeof action.run === 'function') {
        const actionBtn = document.createElement('button');
        actionBtn.type = 'button';
        actionBtn.className = `sidebar-row-action${action.locked ? ' locked' : ' unlocked'}`;
        actionBtn.textContent = action.icon || (action.locked ? '🔒' : '🔓');
        actionBtn.title = action.title || '';
        actionBtn.setAttribute('aria-label', action.ariaLabel || action.title || '');
        actionBtn.setAttribute('aria-pressed', String(Boolean(action.locked)));
        actionBtn.addEventListener('click', (event) => {
            event.preventDefault();
            event.stopPropagation();
            action.run();
        });
        shell.appendChild(actionBtn);
    }

    container.appendChild(shell);
}

function createSidebarNode(id, label, {
    kind = 'dir',
    path = '',
    meta = '',
    sourceKind = 'workspace',
    sourceBadge = '',
    run = null,
    active = false,
    contextAction = null,
    action = null,
} = {}) {
    return {
        id,
        label,
        kind,
        path,
        meta,
        sourceKind,
        sourceBadge,
        run,
        active,
        contextAction,
        action,
        children: [],
    };
}

function ensureSidebarBranch(nodes, id, label, options = {}) {
    let existing = nodes.find((node) => node.id === id);
    if (existing) {
        if (options.meta) {
            existing.meta = options.meta;
        }
        if (options.sourceKind) {
            existing.sourceKind = options.sourceKind;
        }
        if (options.sourceBadge) {
            existing.sourceBadge = options.sourceBadge;
        }
        if (options.contextAction) {
            existing.contextAction = options.contextAction;
        }
        if (options.action) {
            existing.action = options.action;
        }
        return existing;
    }
    existing = createSidebarNode(id, label, { ...options, kind: 'dir' });
    nodes.push(existing);
    return existing;
}

function collectBranchKeys(nodes, sink = []) {
    for (const node of nodes) {
        if (Array.isArray(node.children) && node.children.length > 0) {
            sink.push(node.id);
            collectBranchKeys(node.children, sink);
        }
    }
    return sink;
}

function allBranchesCollapsed(branchKeys, collapsedKeys) {
    return Array.isArray(branchKeys)
        && branchKeys.length > 0
        && branchKeys.every((key) => collapsedKeys.has(key));
}

function updateTreeToggleButton(button, branchKeys, collapsedKeys, label) {
    if (!button) {
        return;
    }
    const hasBranches = Array.isArray(branchKeys) && branchKeys.length > 0;
    const expandMode = hasBranches && allBranchesCollapsed(branchKeys, collapsedKeys);
    const nextLabel = hasBranches
        ? `${expandMode ? 'Expand' : 'Collapse'} all`
        : `${label} empty`;
    button.textContent = '';
    button.dataset.mode = expandMode ? 'expand' : 'collapse';
    button.disabled = !hasBranches;
    if (!hasBranches) {
        button.title = `No ${label.toLowerCase()} items to toggle`;
        button.setAttribute('aria-label', button.title);
        return;
    }
    button.title = `${nextLabel} ${label.toLowerCase()} items`;
    button.setAttribute('aria-label', button.title);
    button.dataset.mode = expandMode ? 'expand' : 'collapse';
}

function renderSidebarTreeNodes(container, nodes, {
    depth = 0,
    collapsedKeys = new Set(),
    onToggle = null,
    ancestorIds = [],
} = {}) {
    for (const node of nodes) {
        const hasChildren = Array.isArray(node.children) && node.children.length > 0;
        const isCollapsed = hasChildren && collapsedKeys.has(node.id);
        const hidden = ancestorIds.some((ancestorId) => collapsedKeys.has(ancestorId));
        appendSidebarRow(container, {
            nodeId: node.id,
            ancestorIds,
            kind: node.kind || (hasChildren ? 'dir' : 'file'),
            label: node.label,
            path: node.path || '',
            depth,
            meta: node.meta,
            active: Boolean(node.active),
            onClick: node.kind === 'dir'
                ? () => {
                    if (node.path) {
                        setSelectedExplorerPath(node.path);
                    }
                    if (hasChildren) {
                        onToggle?.(node.id);
                    }
                }
                : (typeof node.run === 'function'
                    ? () => {
                        if (node.path) {
                            setSelectedExplorerPath(node.path);
                        }
                        node.run();
                    }
                    : null),
            sourceKind: node.sourceKind || 'workspace',
            sourceBadge: node.sourceBadge || '',
            hasChildren,
            collapsed: isCollapsed,
            onContextMenu: node.contextAction
                ? (event) => {
                    openSidebarContextMenu(event.clientX, event.clientY, node.contextAction);
                }
                : null,
            action: node.action || null,
            hidden,
        });
        if (hasChildren) {
            renderSidebarTreeNodes(container, node.children, {
                depth: depth + 1,
                collapsedKeys,
                onToggle,
                ancestorIds: ancestorIds.concat(node.id),
            });
        }
    }
}

function updateRenderedTreeVisibility(container, collapsedKeys = new Set()) {
    if (!container) {
        return;
    }
    for (const shell of container.querySelectorAll('.sidebar-row-shell')) {
        const ancestorIds = String(shell.dataset.ancestorIds || '')
            .split('|')
            .map((value) => value.trim())
            .filter(Boolean);
        shell.hidden = ancestorIds.some((ancestorId) => collapsedKeys.has(ancestorId));
        if (shell.dataset.hasChildren !== 'true') {
            continue;
        }
        const expander = shell.querySelector('.expander');
        if (!expander) {
            continue;
        }
        expander.textContent = collapsedKeys.has(shell.dataset.nodeId) ? '▸' : '▾';
    }
}

async function deleteExplorerFolder(path) {
    closeSidebarContextMenu();
    if (!window.confirm(`Delete folder "${path}"?`)) {
        return;
    }
    const removedPackageArchiveContent = projectFs.listFileEntries().some(
        (entry) => entry.sourceKind === 'packageArchive'
            && (entry.path === path || entry.path.startsWith(`${path}/`)),
    );
    const previousActivePath = projectFs.getActiveDocumentPath();
    const removed = projectFs.removeFolder(path);
    if (!removed) {
        return;
    }
    if (selectedExplorerPath === path || selectedExplorerPath.startsWith(`${path}/`)) {
        selectedExplorerPath = '';
    }
    if (removedPackageArchiveContent) {
        await packageArchiveController.restoreProjectPackageArchives();
    }
    pruneOpenDocuments(
        (candidate) => candidate === path || candidate.startsWith(`${path}/`),
        projectFs.getActiveDocumentPath(),
    );
    const nextActivePath = projectFs.getActiveDocumentPath();
    if (previousActivePath !== nextActivePath || !projectFs.getFileContent(previousActivePath)) {
        await openProjectDocument(nextActivePath, { focusEditor: false, forceReload: true });
        return;
    }
    refreshWorkbenchNavigation();
    scheduleProjectPersistence();
}

async function deleteExplorerFile(path) {
    closeSidebarContextMenu();
    if (!window.confirm(`Delete file "${path}"?`)) {
        return;
    }
    const removedPackageArchiveContent = projectFs.listFileEntries().some(
        (entry) => entry.sourceKind === 'packageArchive' && entry.path === path,
    );
    const previousActivePath = projectFs.getActiveDocumentPath();
    const removed = projectFs.removeFile(path);
    if (!removed) {
        return;
    }
    if (selectedExplorerPath === path) {
        selectedExplorerPath = '';
    }
    if (removedPackageArchiveContent) {
        await packageArchiveController.restoreProjectPackageArchives();
    }
    pruneOpenDocuments((candidate) => candidate === path, projectFs.getActiveDocumentPath());
    const nextActivePath = projectFs.getActiveDocumentPath();
    if (previousActivePath !== nextActivePath || !projectFs.getFileContent(previousActivePath)) {
        await openProjectDocument(nextActivePath, { focusEditor: false, forceReload: true });
        return;
    }
    refreshWorkbenchNavigation();
    scheduleProjectPersistence();
}

function createExplorerFolderAction(path) {
    return {
        label: 'Delete Folder',
        danger: true,
        run: async () => {
            await deleteExplorerFolder(path);
        },
    };
}

function createExplorerFileAction(path) {
    return {
        label: 'Delete File',
        danger: true,
        run: async () => {
            await deleteExplorerFile(path);
        },
    };
}

function explorerLockAction(path, sourceKind) {
    const locked = isDocumentLocked(path);
    const imported = sourceKind === 'packageArchive';
    return {
        locked,
        title: imported
            ? (locked
                ? 'Imported file is locked. Click to unlock editing for this file.'
                : 'Imported file is unlocked. Click to lock editing for this file.')
            : (locked
                ? 'File is locked. Click to unlock editing for this file.'
                : 'File is unlocked. Click to lock editing for this file.'),
        ariaLabel: imported
            ? (locked ? 'Unlock imported file' : 'Lock imported file')
            : (locked ? 'Unlock file' : 'Lock file'),
        run: () => {
            toggleDocumentLock(path);
        },
    };
}

function explorerFolderLockAction(path, sourceKind, locked) {
    const imported = sourceKind === 'packageArchive';
    return {
        locked: Boolean(locked),
        title: imported
            ? (locked
                ? 'Imported folder is locked. Click to unlock editing for files in this folder.'
                : 'Imported folder is unlocked. Click to lock editing for files in this folder.')
            : (locked
                ? 'Folder is locked. Click to unlock editing for files in this folder.'
                : 'Folder is unlocked. Click to lock editing for files in this folder.'),
        ariaLabel: imported
            ? (locked ? 'Unlock imported folder' : 'Lock imported folder')
            : (locked ? 'Unlock folder' : 'Lock folder'),
        run: () => {
            toggleFolderLock(path);
        },
    };
}

async function createWorkspaceFileFromExplorer() {
    closeSidebarContextMenu();
    const defaultPath = defaultNewWorkspaceFilePath();
    const requestedPath = resolveWorkspaceCreationPath(
        window.prompt('New file path', defaultPath) || '',
    );
    if (!requestedPath) {
        return;
    }
    try {
        const existingContent = projectFs.getFileContent(requestedPath);
        if (typeof existingContent === 'string') {
            expandWorkspaceExplorerPath(requestedPath);
            revealExplorerPath(requestedPath);
            await openProjectDocument(requestedPath, { forceReload: true });
            return;
        }
        const createdPath = projectFs.setFile(
            requestedPath,
            defaultWorkspaceFileContent(requestedPath),
        );
        expandWorkspaceExplorerPath(createdPath);
        revealExplorerPath(createdPath);
        await openProjectDocument(createdPath, { forceReload: true });
        scheduleProjectPersistence(0);
    } catch (error) {
        window.alert(error instanceof Error ? error.message : String(error));
    }
}

function createWorkspaceFolderFromExplorer() {
    closeSidebarContextMenu();
    const defaultPath = defaultNewWorkspaceFolderPath();
    const requestedPath = resolveWorkspaceCreationPath(
        window.prompt('New folder path', defaultPath) || '',
    );
    if (!requestedPath) {
        return;
    }
    try {
        if (projectFs.listFolders().includes(requestedPath)) {
            expandWorkspaceExplorerPath(requestedPath, true);
            revealExplorerPath(requestedPath);
            return;
        }
        const createdPath = projectFs.setFolder(requestedPath);
        expandWorkspaceExplorerPath(createdPath, true);
        revealExplorerPath(createdPath);
        scheduleProjectPersistence(0);
    } catch (error) {
        window.alert(error instanceof Error ? error.message : String(error));
    }
}

function collapseImportedExplorerBranches() {
    const entries = projectFs.listFileEntries().filter(
        (entry) => entry.sourceKind === 'packageArchive' && isExplorerTextEntry(entry),
    );
    for (const entry of entries) {
        const parts = pathParts(parentDirectory(entry.path));
        let prefix = '';
        for (const part of parts) {
            prefix = prefix ? `${prefix}/${part}` : part;
            explorerCollapsedNodes.add(`explorer:${prefix}`);
        }
    }
}

function buildExplorerNodes(folderPaths, entries, activePath) {
    const rootNodes = [];
    const folderStats = new Map();
    const ensureFolderStats = (path) => {
        const normalized = trimMaybeString(path);
        if (!normalized) {
            return null;
        }
        if (!folderStats.has(normalized)) {
            folderStats.set(normalized, {
                totalFiles: 0,
                packageArchiveFiles: 0,
                lockedFiles: 0,
            });
        }
        return folderStats.get(normalized);
    };
    const addFolderStats = (path, sourceKind, locked) => {
        const stats = ensureFolderStats(path);
        if (!stats) {
            return;
        }
        stats.totalFiles += 1;
        if (sourceKind === 'packageArchive') {
            stats.packageArchiveFiles += 1;
        }
        if (locked) {
            stats.lockedFiles += 1;
        }
    };
    const registerFolderPath = (path) => {
        let prefix = '';
        for (const part of pathParts(path)) {
            prefix = prefix ? `${prefix}/${part}` : part;
            ensureFolderStats(prefix);
        }
    };
    for (const folderPath of folderPaths) {
        registerFolderPath(folderPath);
    }
    for (const entry of entries) {
        let prefix = '';
        for (const part of pathParts(parentDirectory(entry.path))) {
            prefix = prefix ? `${prefix}/${part}` : part;
            addFolderStats(prefix, entry.sourceKind, isDocumentLocked(entry.path));
        }
    }
    const folderSourceKind = (path) => {
        const stats = folderStats.get(path);
        return stats && stats.totalFiles > 0 && stats.packageArchiveFiles === stats.totalFiles
            ? 'packageArchive'
            : 'workspace';
    };
    const folderLocked = (path) => {
        const stats = folderStats.get(path);
        return Boolean(stats && stats.totalFiles > 0 && stats.lockedFiles === stats.totalFiles);
    }
    const ensureDirectoryPath = (path) => {
        const parts = pathParts(path);
        let currentNodes = rootNodes;
        let prefix = '';
        for (const part of parts) {
            prefix = prefix ? `${prefix}/${part}` : part;
            const sourceKind = folderSourceKind(prefix);
            const branch = ensureSidebarBranch(currentNodes, `explorer:${prefix}`, part, {
                path: prefix,
                sourceKind,
                active: prefix === activePath,
                contextAction: createExplorerFolderAction(prefix),
                action: explorerFolderLockAction(prefix, sourceKind, folderLocked(prefix)),
            });
            currentNodes = branch.children;
        }
        return currentNodes;
    };

    for (const folderPath of folderPaths) {
        ensureDirectoryPath(folderPath);
    }

    for (const entry of entries) {
        const parts = pathParts(entry.path);
        if (parts.length === 0) {
            continue;
        }
        const currentNodes = ensureDirectoryPath(parentDirectory(entry.path));
        currentNodes.push(createSidebarNode(`explorer-file:${entry.path}`, parts.at(-1) || entry.path, {
            kind: 'file',
            path: entry.path,
            active: entry.path === activePath,
            run: () => {
                void openProjectDocument(entry.path);
            },
            contextAction: createExplorerFileAction(entry.path),
            action: explorerLockAction(entry.path, entry.sourceKind),
        }));
    }
    return rootNodes;
}

function renderEditorTabsForPane(paneId) {
    const pane = getEditorPane(paneId);
    if (!pane?.tabsEl) {
        return;
    }
    pane.tabsEl.innerHTML = '';
    if (pane.paths.length === 0) {
        appendSidebarEmpty(pane.tabsEl, 'No open files.');
        refreshEditorPaneEmptyState(pane.id);
        return;
    }
    for (const path of pane.paths) {
        const tab = document.createElement('div');
        tab.className = `editor-tab${path === pane.activePath ? ' active' : ''}`;
        tab.title = path;
        tab.setAttribute('role', 'tab');
        tab.setAttribute('aria-selected', path === pane.activePath ? 'true' : 'false');
        tab.draggable = true;
        tab.dataset.path = path;
        tab.dataset.paneId = pane.id;

        const tabButton = document.createElement('button');
        tabButton.type = 'button';
        tabButton.className = 'editor-tab-button';
        tabButton.title = path;

        const name = document.createElement('span');
        name.className = 'tab-name';
        name.textContent = baseName(path) || path;
        tabButton.appendChild(name);

        const dir = parentDirectory(path);
        if (dir) {
            const pathNode = document.createElement('span');
            pathNode.className = 'tab-path';
            pathNode.textContent = dir;
            tabButton.appendChild(pathNode);
        }

        tabButton.addEventListener('click', () => {
            void openProjectDocument(path, { paneId: pane.id });
        });
        tab.appendChild(tabButton);

        const closeButton = document.createElement('button');
        closeButton.type = 'button';
        closeButton.className = 'editor-tab-close';
        closeButton.title = 'Close file';
        closeButton.setAttribute('aria-label', `Close ${baseName(path) || path}`);
        closeButton.textContent = '✕';
        closeButton.addEventListener('click', (event) => {
            event.preventDefault();
            event.stopPropagation();
            void closeProjectDocument(path, pane.id);
        });
        tab.appendChild(closeButton);

        tab.addEventListener('dragstart', (event) => {
            dragEditorTabState = { path, sourcePaneId: pane.id };
            tab.classList.add('dragging');
            event.dataTransfer?.setData('text/plain', JSON.stringify(dragEditorTabState));
            event.dataTransfer?.setData('application/x-rumoca-editor-tab', JSON.stringify(dragEditorTabState));
            event.dataTransfer.effectAllowed = 'move';
            if (editorDropOverlay) {
                editorDropOverlay.hidden = false;
            }
        });
        tab.addEventListener('dragend', () => {
            tab.classList.remove('dragging');
            dragEditorTabState = null;
            if (editorDropOverlay) {
                editorDropOverlay.hidden = true;
            }
            for (const zone of editorDropZones) {
                zone.classList.remove('active');
            }
        });
        pane.tabsEl.appendChild(tab);
    }
    if (!pane.tabsEl.dataset.dragBound) {
        pane.tabsEl.addEventListener('dragover', (event) => {
            if (!dragEditorTabState) {
                return;
            }
            event.preventDefault();
            event.dataTransfer.dropEffect = 'move';
        });
        pane.tabsEl.addEventListener('drop', async (event) => {
            if (!dragEditorTabState) {
                return;
            }
            event.preventDefault();
            await moveEditorTabToPane(dragEditorTabState.path, dragEditorTabState.sourcePaneId, pane.id);
        });
        pane.tabsEl.dataset.dragBound = 'true';
    }
    refreshEditorPaneEmptyState(pane.id);
}

function renderEditorTabs() {
    setEditorPaneSplit(editorPaneSplit);
    renderEditorTabsForPane('primary');
    renderEditorTabsForPane('secondary');
    for (const button of editorCloseButtons) {
        const paneId = button.dataset.paneId || activeEditorPaneId;
        const pane = getEditorPane(paneId);
        const closable = canCloseProjectDocument(pane?.activePath || '', paneId);
        button.disabled = !closable;
        button.setAttribute('aria-disabled', String(!closable));
        button.title = closable ? 'Close active file' : 'No open file';
    }
}

function updateExplorerActiveSelection(activePath = '') {
    if (!explorerTree) {
        return;
    }
    const nextPath = trimMaybeString(activePath);
    for (const shell of explorerTree.querySelectorAll('.sidebar-row-shell[data-path]')) {
        shell.classList.toggle('active', nextPath && shell.dataset.path === nextPath);
    }
    for (const row of explorerTree.querySelectorAll('.sidebar-row[data-path]')) {
        const isActive = nextPath && row.dataset.path === nextPath;
        row.classList.toggle('active', isActive);
        row.setAttribute('aria-current', isActive ? 'true' : 'false');
    }
}

function findExplorerRowByPath(path = '') {
    const nextPath = trimMaybeString(path);
    if (!explorerTree || !nextPath) {
        return null;
    }
    for (const row of explorerTree.querySelectorAll('.sidebar-row[data-path]')) {
        if (row.dataset.path === nextPath) {
            return row;
        }
    }
    return null;
}

function revealExplorerPath(path = '') {
    const nextPath = trimMaybeString(path);
    if (!nextPath) {
        return;
    }
    setSidebarMode('explorer', { persist: false });
    setSidebarCollapsed(false);
    setSelectedExplorerPath(nextPath);
    renderExplorerPane();
    updateWorkbenchPaneVisibility();
    updateMobileAppbarTitle();
    requestAnimationFrame(() => {
        let row = findExplorerRowByPath(nextPath);
        if (!row) {
            renderExplorerPane();
            row = findExplorerRowByPath(nextPath);
        }
        row?.scrollIntoView({
            block: 'nearest',
            inline: 'nearest',
        });
    });
}

function renderExplorerPane() {
    if (!explorerTree) {
        return;
    }
    const fileEntries = projectFs.listFileEntries().filter(isExplorerTextEntry);
    const folderEntries = projectFs.listFolders();
    if (fileEntries.length === 0 && folderEntries.length === 0) {
        appendSidebarEmpty(explorerTree, 'Project files will appear here.');
        explorerBranchKeys = [];
        updateTreeToggleButton(explorerTreeToggleBtn, explorerBranchKeys, explorerCollapsedNodes, 'Explorer');
        return;
    }

    const activePath = currentExplorerSelectionPath();
    explorerTree.innerHTML = '';
    const nodes = buildExplorerNodes(folderEntries, fileEntries, activePath);
    explorerBranchKeys = collectBranchKeys(nodes, []);
    updateTreeToggleButton(explorerTreeToggleBtn, explorerBranchKeys, explorerCollapsedNodes, 'Explorer');
    renderSidebarTreeNodes(explorerTree, nodes, {
        collapsedKeys: explorerCollapsedNodes,
        onToggle(nodeId) {
            if (explorerCollapsedNodes.has(nodeId)) {
                explorerCollapsedNodes.delete(nodeId);
            } else {
                explorerCollapsedNodes.add(nodeId);
            }
            updateRenderedTreeVisibility(explorerTree, explorerCollapsedNodes);
            updateTreeToggleButton(explorerTreeToggleBtn, explorerBranchKeys, explorerCollapsedNodes, 'Explorer');
            scheduleProjectPersistence();
        },
    });
    updateExplorerActiveSelection(activePath);
}

explorerTree?.addEventListener('click', (event) => {
    const target = event.target;
    if (!(target instanceof Element)) {
        return;
    }
    if (target.closest('.sidebar-row') || target.closest('.sidebar-row-action')) {
        return;
    }
    setSelectedExplorerPath(EXPLORER_ROOT_SELECTION);
    scheduleProjectPersistence();
});

function refreshActiveDocumentNavigation({ includeOutline = true, outlineDelayMs = 0 } = {}) {
    renderEditorTabs();
    updateExplorerActiveSelection(currentExplorerSelectionPath());
    updateWorkbenchPaneVisibility();
    updateMobileAppbarTitle();
    if (!includeOutline) {
        return;
    }
    if (outlineDelayMs > 0) {
        scheduleOutlineRefresh(outlineDelayMs);
        return;
    }
    void renderOutlinePane();
}

async function renderOutlinePane() {
    if (!outlineTree) {
        return;
    }
    const renderId = ++outlineRenderVersion;
    const activePath = hasOpenEditorPane() ? projectFs.getActiveDocumentPath() : '';
    if (!activePath || !editor) {
        appendSidebarEmpty(outlineTree, 'Open a Modelica file to view symbols.');
        outlineBranchKeys = [];
        updateTreeToggleButton(outlineTreeToggleBtn, outlineBranchKeys, outlineCollapsedNodes, 'Outline');
        return;
    }
    if (!workerReady) {
        appendSidebarEmpty(outlineTree, 'Waiting for language worker...');
        outlineBranchKeys = [];
        updateTreeToggleButton(outlineTreeToggleBtn, outlineBranchKeys, outlineCollapsedNodes, 'Outline');
        return;
    }

    const items = await buildDocumentSymbolTreeNodes();
    if (renderId !== outlineRenderVersion) {
        return;
    }
    if (items.length === 0) {
        appendSidebarEmpty(outlineTree, 'No symbols found in the active document.');
        outlineBranchKeys = [];
        updateTreeToggleButton(outlineTreeToggleBtn, outlineBranchKeys, outlineCollapsedNodes, 'Outline');
        return;
    }

    outlineTree.innerHTML = '';
    outlineBranchKeys = collectBranchKeys(items, []);
    updateTreeToggleButton(outlineTreeToggleBtn, outlineBranchKeys, outlineCollapsedNodes, 'Outline');
    renderSidebarTreeNodes(outlineTree, items, {
        collapsedKeys: outlineCollapsedNodes,
        onToggle(nodeId) {
            if (outlineCollapsedNodes.has(nodeId)) {
                outlineCollapsedNodes.delete(nodeId);
            } else {
                outlineCollapsedNodes.add(nodeId);
            }
            updateRenderedTreeVisibility(outlineTree, outlineCollapsedNodes);
            updateTreeToggleButton(outlineTreeToggleBtn, outlineBranchKeys, outlineCollapsedNodes, 'Outline');
            scheduleProjectPersistence();
        },
    });
}

function scheduleOutlineRefresh(delayMs = 120) {
    if (outlineRefreshTimer) {
        clearTimeout(outlineRefreshTimer);
    }
    outlineRefreshTimer = setTimeout(() => {
        outlineRefreshTimer = null;
        void renderOutlinePane();
    }, delayMs);
}

function refreshWorkbenchNavigation({ includeOutline = true, outlineDelayMs = 0 } = {}) {
    renderEditorTabs();
    renderExplorerPane();
    updateWorkbenchPaneVisibility();
    updateMobileAppbarTitle();
    if (!includeOutline) {
        return;
    }
    if (outlineDelayMs > 0) {
        scheduleOutlineRefresh(outlineDelayMs);
        return;
    }
    void renderOutlinePane();
}

function inferSourceEditorLanguage(path) {
    const nextPath = trimMaybeString(path).toLowerCase();
    if (nextPath.endsWith('.mo')) return 'modelica';
    if (nextPath.endsWith('.jinja') || nextPath.endsWith('.jinja2')) return 'jinja2';
    if (nextPath.endsWith('.js') || nextPath.endsWith('.mjs') || nextPath.endsWith('.cjs')) {
        return 'javascript';
    }
    if (nextPath.endsWith('.toml')) return 'toml';
    if (nextPath.endsWith('.py')) return 'python';
    if (nextPath.endsWith('.jl')) return 'julia';
    if (nextPath.endsWith('.json')) return 'json';
    if (nextPath.endsWith('.xml')) return 'xml';
    if (nextPath.endsWith('.html')) return 'html';
    if (nextPath.endsWith('.c') || nextPath.endsWith('.h')) return 'c';
    return 'plaintext';
}

function resolveSupportedEditorLanguage(languageId) {
    const nextId = trimMaybeString(languageId) || 'plaintext';
    const getLanguages = monacoApi?.languages?.getLanguages;
    if (typeof getLanguages !== 'function') {
        return nextId === 'toml' ? 'ini' : nextId;
    }
    const supported = getLanguages.call(monacoApi.languages)
        .some((entry) => trimMaybeString(entry?.id) === nextId);
    if (supported) {
        return nextId;
    }
    if (nextId === 'toml') {
        return 'ini';
    }
    return 'plaintext';
}

// Panel references
const resizeHandleH = document.getElementById('resizeHandleH');
const leftArea = document.querySelector('.left-area');
const workbenchTop = document.querySelector('.workbench-top') || document.querySelector('.main-container');
const workbenchMain = document.querySelector('.workbench-main') || document.querySelector('.main-container');
let isResizingH = false;
let isResizingSidebar = false;
let isResizingSidebarSplit = false;

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
editorSplitHandle?.addEventListener('mousedown', () => {
    if (editorPaneSplit === 'single') return;
    isResizingEditorSplit = true;
    document.body.style.cursor = editorPaneSplit === 'horizontal' ? 'ns-resize' : 'ew-resize';
    document.body.style.userSelect = 'none';
});

const layoutAllEditors = () => {
    if (window.editor) window.editor.layout();
    if (editorPanes.secondary.editor) editorPanes.secondary.editor.layout();
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
    syncSidebarBackdrop();
}

window.addEventListener('resize', syncResponsiveLayoutState);
window.addEventListener('resize', updateSidebarSplitHandleVisibility);
syncResponsiveLayoutState();
setSidebarMode('explorer', { persist: false });
setSidebarCollapsed(isNarrowLayout());
setSidebarSectionCollapsed('explorer', false);
setSidebarSectionCollapsed('outline', false);
updateSidebarSplitHandleVisibility();

sidebarExplorerBtn?.addEventListener('click', () => {
    const collapsed = workbenchSidepanel?.classList.contains('collapsed');
    const isExplorer = currentSidebarMode() === 'explorer';
    if (collapsed || !isExplorer) {
        setSidebarMode('explorer');
        setSidebarCollapsed(false);
    } else {
        setSidebarCollapsed(true);
    }
});
mobileSidebarBtn?.addEventListener('click', () => {
    const collapsed = workbenchSidepanel?.classList.contains('collapsed');
    const isExplorer = currentSidebarMode() === 'explorer';
    if (collapsed || !isExplorer) {
        setSidebarMode('explorer');
        setSidebarCollapsed(false);
    } else {
        setSidebarCollapsed(true);
    }
});
sidebarProjectBtn?.addEventListener('click', () => {
    const collapsed = workbenchSidepanel?.classList.contains('collapsed');
    const isProject = currentSidebarMode() === 'project';
    if (collapsed || !isProject) {
        setSidebarMode('project');
        setSidebarCollapsed(false);
    } else {
        setSidebarCollapsed(true);
    }
});
mobileProjectBtn?.addEventListener('click', () => {
    const collapsed = workbenchSidepanel?.classList.contains('collapsed');
    const isProject = currentSidebarMode() === 'project';
    if (collapsed || !isProject) {
        setSidebarMode('project');
        setSidebarCollapsed(false);
    } else {
        setSidebarCollapsed(true);
    }
});
sidebarBackdrop?.addEventListener('click', () => {
    setSidebarCollapsed(true);
});
sidebarEditorPaneBtn?.addEventListener('click', () => {
    editorPaneVisible = !editorPaneVisible;
    updateWorkbenchPaneVisibility();
    scheduleProjectPersistence();
});
sidebarResultsPaneBtn?.addEventListener('click', () => {
    window.switchRightTab(window.activeRightTab === 'simulate' ? '' : 'simulate');
});
sidebarCodegenPaneBtn?.addEventListener('click', () => {
    window.switchRightTab(window.activeRightTab === 'codegen' ? '' : 'codegen');
});
explorerNewFileBtn?.addEventListener('click', () => {
    void createWorkspaceFileFromExplorer();
});
explorerNewFolderBtn?.addEventListener('click', () => {
    createWorkspaceFolderFromExplorer();
});
explorerSectionToggle?.addEventListener('click', () => {
    toggleSidebarSection('explorer');
});
outlineSectionToggle?.addEventListener('click', () => {
    toggleSidebarSection('outline');
});
for (const zone of editorDropZones) {
    zone.addEventListener('dragover', (event) => {
        if (!dragEditorTabState) {
            return;
        }
        event.preventDefault();
        zone.classList.add('active');
        event.dataTransfer.dropEffect = 'move';
    });
    zone.addEventListener('dragleave', () => {
        zone.classList.remove('active');
    });
    zone.addEventListener('drop', async (event) => {
        if (!dragEditorTabState) {
            return;
        }
        event.preventDefault();
        zone.classList.remove('active');
        await splitEditorPaneWithTab(
            dragEditorTabState.path,
            dragEditorTabState.sourcePaneId,
            zone.dataset.dropPosition || 'right',
        );
        dragEditorTabState = null;
        if (editorDropOverlay) {
            editorDropOverlay.hidden = true;
        }
    });
}
explorerTreeToggleBtn?.addEventListener('click', () => {
    if (allBranchesCollapsed(explorerBranchKeys, explorerCollapsedNodes)) {
        explorerCollapsedNodes.clear();
    } else {
        explorerCollapsedNodes.clear();
        for (const nodeId of explorerBranchKeys) {
            explorerCollapsedNodes.add(nodeId);
        }
    }
    updateRenderedTreeVisibility(explorerTree, explorerCollapsedNodes);
    updateTreeToggleButton(explorerTreeToggleBtn, explorerBranchKeys, explorerCollapsedNodes, 'Explorer');
    scheduleProjectPersistence();
});
outlineTreeToggleBtn?.addEventListener('click', () => {
    if (allBranchesCollapsed(outlineBranchKeys, outlineCollapsedNodes)) {
        outlineCollapsedNodes.clear();
    } else {
        outlineCollapsedNodes.clear();
        for (const nodeId of outlineBranchKeys) {
            outlineCollapsedNodes.add(nodeId);
        }
    }
    updateRenderedTreeVisibility(outlineTree, outlineCollapsedNodes);
    updateTreeToggleButton(outlineTreeToggleBtn, outlineBranchKeys, outlineCollapsedNodes, 'Outline');
    scheduleProjectPersistence();
});

function updateSidebarSplitHandleVisibility() {
    if (!resizeHandleSidebarV) {
        return;
    }
    const hidden = isNarrowLayout()
        || workbenchSidepanel?.classList.contains('collapsed')
        || currentSidebarMode() !== 'explorer'
        || explorerSection?.classList.contains('collapsed')
        || outlineSection?.classList.contains('collapsed');
    resizeHandleSidebarV.style.display = hidden ? 'none' : '';
}

resizeHandleSidebar?.addEventListener('mousedown', () => {
    if (isNarrowLayout() || workbenchSidepanel?.classList.contains('collapsed')) return;
    isResizingSidebar = true;
    document.body.style.cursor = 'ew-resize';
    document.body.style.userSelect = 'none';
});

resizeHandleSidebarV?.addEventListener('mousedown', () => {
    if (isNarrowLayout()) return;
    if (workbenchSidepanel?.classList.contains('collapsed')) return;
    if (currentSidebarMode() !== 'explorer') return;
    if (explorerSection?.classList.contains('collapsed') || outlineSection?.classList.contains('collapsed')) return;
    isResizingSidebarSplit = true;
    document.body.style.cursor = 'ns-resize';
    document.body.style.userSelect = 'none';
});

function setRuntimeStatusBar(message, tone = 'loading') {
    const runtimeLabel = document.getElementById('outputRuntimeStatus');
    if (!runtimeLabel) return;
    runtimeLabel.textContent = `Runtime: ${String(message || '').trim() || 'Unknown'}`;
    runtimeLabel.dataset.tone = tone;
}

function setCompileStatusBadge(message, tone = 'loading') {
    const compileLabel = document.getElementById('outputCompileStatus');
    if (!compileLabel) return;
    compileLabel.textContent = `Compile: ${String(message || '').trim() || 'Unknown'}`;
    compileLabel.dataset.tone = tone;
}

setRuntimeStatusBar('Loading...', 'loading');
setCompileStatusBadge('Waiting...', 'loading');

let startupCompileRequested = false;

function requestStartupCompileIfReady() {
    if (startupCompileRequested) {
        return;
    }
    if (!workerReady) {
        return;
    }
    if (typeof window.triggerCompileNow !== 'function') {
        return;
    }
    if (isRumocaSmokeMode()) {
        return;
    }
    if (!hasOpenEditorPane()) {
        return;
    }
    if (!projectFs.getActiveDocumentPath()?.endsWith('.mo')) {
        return;
    }
    startupCompileRequested = true;
    window.triggerCompileNow();
}

function paneEditor(paneId) {
    return getEditorPane(paneId)?.editor || null;
}

function applyEditorLanguage(paneId, path) {
    const nextEditor = paneEditor(paneId);
    const model = nextEditor?.getModel?.();
    if (!nextEditor || !model || !monacoApi?.editor?.setModelLanguage) {
        return;
    }
    const languageId = resolveSupportedEditorLanguage(inferSourceEditorLanguage(path));
    monacoApi.editor.setModelLanguage(model, languageId);
    if (typeof window.refreshCodeLens === 'function') {
        window.refreshCodeLens();
    }
}

function ensureSecondarySourceEditor() {
    const pane = getEditorPane('secondary');
    if (pane.editor || !createSourceEditorFactory) {
        return pane.editor;
    }
    pane.editor = createSourceEditorFactory('secondaryEditor');
    if (pane.editor && typeof bindPaneEditorToWorkspace === 'function') {
        bindPaneEditorToWorkspace('secondary', pane.editor);
    }
    return pane.editor;
}

async function openProjectDocument(path, { paneId = activeEditorPaneId, focusEditor = true, forceReload = false } = {}) {
    const nextPath = trimMaybeString(path);
    const targetPane = getEditorPane(paneId);
    const targetEditor = normalizeEditorPaneId(paneId) === 'secondary'
        ? ensureSecondarySourceEditor()
        : paneEditor('primary');
    if (!nextPath || !targetPane || !targetEditor) {
        return;
    }
    const nextContent = projectFs.getFileContent(nextPath);
    if (typeof nextContent !== 'string') {
        return;
    }
    editorPaneVisible = true;
    updateWorkbenchPaneVisibility();

    removePathFromOtherEditorPane(nextPath, targetPane.id);
    const currentPath = trimMaybeString(targetPane.activePath);
    if (currentPath === nextPath && !forceReload) {
        setPanePaths(targetPane.id, [...targetPane.paths, nextPath], nextPath);
        setActiveEditorPane(targetPane.id, { focusEditor, refreshNavigation: false });
        refreshActiveDocumentNavigation({ outlineDelayMs: 80 });
        scheduleProjectPersistence(1200);
        return;
    }

    if (currentPath && typeof targetEditor.saveViewState === 'function') {
        editorViewStates.set(currentPath, targetEditor.saveViewState());
    }
    if (currentPath && typeof targetEditor.getValue === 'function') {
        projectFs.setFile(currentPath, targetEditor.getValue());
    }

    targetPane.paths = normalizeOpenDocumentPaths([...targetPane.paths, nextPath], currentPath || nextPath);
    rebuildOpenDocumentPaths();

    suspendWorkspaceObservers = true;
    try {
        targetEditor.setValue(nextContent);
        applyEditorLanguage(targetPane.id, nextPath);
    } finally {
        suspendWorkspaceObservers = false;
    }

    if (typeof targetEditor.restoreViewState === 'function') {
        const viewState = editorViewStates.get(nextPath);
        if (viewState) {
            targetEditor.restoreViewState(viewState);
        }
    }

    targetPane.activePath = nextPath;
    projectFs.setActiveDocument(nextPath, nextContent);
    selectedExplorerPath = nextPath;
    setActiveEditorPane(targetPane.id, {
        focusEditor,
        refreshNavigation: false,
        persistCurrent: false,
    });
    updateSourceBreadcrumbs();
    refreshSimulationSettingsModalIfOpen();
    refreshActiveDocumentNavigation({ outlineDelayMs: 80 });
    updateMobileAppbarTitle();
    if (isNarrowLayout()) {
        setSidebarCollapsed(true);
    }
    scheduleProjectPersistence(1200);
}

async function moveEditorTabToPane(path, sourcePaneId, targetPaneId) {
    const nextPath = trimMaybeString(path);
    const sourcePane = getEditorPane(sourcePaneId);
    const targetPane = getEditorPane(targetPaneId);
    if (!nextPath || !sourcePane || !targetPane) {
        return;
    }
    if (sourcePane.activePath === nextPath && typeof sourcePane.editor?.getValue === 'function') {
        projectFs.setFile(nextPath, sourcePane.editor.getValue());
    }
    if (!targetPane.paths.includes(nextPath)) {
        targetPane.paths = normalizeOpenDocumentPaths([...targetPane.paths, nextPath], nextPath);
    } else {
        targetPane.activePath = nextPath;
    }
    if (sourcePane.id !== targetPane.id) {
        sourcePane.paths = sourcePane.paths.filter((candidate) => candidate !== nextPath);
        if (sourcePane.activePath === nextPath) {
            sourcePane.activePath = sourcePane.paths.at(-1) || '';
        }
    }
    rebuildOpenDocumentPaths();
    await openProjectDocument(nextPath, { paneId: targetPane.id, focusEditor: true, forceReload: true });
    collapseSingleEmptyPaneIfPossible();
}

async function splitEditorPaneWithTab(path, sourcePaneId, position) {
    const nextPath = trimMaybeString(path);
    if (!nextPath) {
        return;
    }
    ensureSecondarySourceEditor();
    const sourcePane = getEditorPane(sourcePaneId);
    if (sourcePane?.activePath === nextPath && typeof sourcePane.editor?.getValue === 'function') {
        projectFs.setFile(nextPath, sourcePane.editor.getValue());
    }
    const primaryPane = getEditorPane('primary');
    const secondaryPane = getEditorPane('secondary');
    if (editorPaneSplit === 'single') {
        const sourcePaths = normalizeOpenDocumentPaths(sourcePane.paths, sourcePane.activePath);
        const remaining = sourcePaths.filter((candidate) => candidate !== nextPath);
        if (position === 'left') {
            primaryPane.paths = [nextPath];
            primaryPane.activePath = nextPath;
            secondaryPane.paths = remaining;
            secondaryPane.activePath = remaining.at(-1) || '';
            setEditorPaneSplit('vertical');
            await openProjectDocument(nextPath, { paneId: 'primary', focusEditor: true, forceReload: true });
            if (secondaryPane.activePath) {
                await openProjectDocument(secondaryPane.activePath, { paneId: 'secondary', focusEditor: false, forceReload: true });
            } else {
                refreshEditorPaneEmptyState('secondary');
            }
            return;
        }
        primaryPane.paths = remaining;
        primaryPane.activePath = remaining.at(-1) || '';
        secondaryPane.paths = [nextPath];
        secondaryPane.activePath = nextPath;
        setEditorPaneSplit(position === 'bottom' ? 'horizontal' : 'vertical');
        if (primaryPane.activePath) {
            await openProjectDocument(primaryPane.activePath, { paneId: 'primary', focusEditor: false, forceReload: true });
        } else {
            refreshEditorPaneEmptyState('primary');
        }
        await openProjectDocument(nextPath, { paneId: 'secondary', focusEditor: true, forceReload: true });
        return;
    }
    setEditorPaneSplit(position === 'bottom' ? 'horizontal' : 'vertical');
    const targetPaneId = position === 'left' ? 'primary' : 'secondary';
    await moveEditorTabToPane(nextPath, sourcePaneId, targetPaneId);
}

document.addEventListener('mousemove', (e) => {
    if (isResizingSidebar) {
        if (isNarrowLayout()) {
            isResizingSidebar = false;
            document.body.style.cursor = '';
            document.body.style.userSelect = '';
            return;
        }
        const panelRect = workbenchSidepanel.getBoundingClientRect();
        const newWidth = e.clientX - panelRect.left - 6;
        if (workbenchSidebar) {
            workbenchSidebar.style.width = `${Math.max(180, Math.min(newWidth, 480))}px`;
        }
        layoutAllEditors();
        return;
    }
    if (isResizingH) {
        if (isNarrowLayout()) {
            isResizingH = false;
            document.body.style.cursor = '';
            document.body.style.userSelect = '';
            return;
        }
        const containerRect = workbenchTop.getBoundingClientRect();
        const newWidth = e.clientX - containerRect.left;
        leftArea.style.flex = 'none';
        leftArea.style.width = Math.max(200, newWidth) + 'px';
        layoutAllEditors();
    }
    if (isResizingSidebarSplit) {
        const sidebarRect = workbenchSidebar?.getBoundingClientRect();
        if (!sidebarRect || !explorerSection || !outlineSection) {
            return;
        }
        const explorerHeaderHeight = explorerSection.querySelector('.sidebar-section-header')?.getBoundingClientRect().height || 0;
        const handleHeight = resizeHandleSidebarV?.getBoundingClientRect().height || 0;
        const outlineHeaderHeight = outlineSection.querySelector('.sidebar-section-header')?.getBoundingClientRect().height || 0;
        const outlineMinHeight = 90 + outlineHeaderHeight;
        const nextHeight = e.clientY - sidebarRect.top - explorerHeaderHeight;
        const maxHeight = Math.max(140, sidebarRect.height - handleHeight - outlineMinHeight);
        explorerSection.style.flex = 'none';
        explorerSection.style.height = `${Math.max(140, Math.min(nextHeight, maxHeight))}px`;
        return;
    }
    if (isResizingV) {
        const workbenchRect = workbenchMain.getBoundingClientRect();
        const newHeight = workbenchRect.bottom - e.clientY;
        const clampedHeight = Math.max(100, Math.min(newHeight, window.innerHeight * 0.5));
        bottomPanel.style.height = clampedHeight + 'px';
        if (window.editor) window.editor.layout();
        return;
    }
    if (isResizingEditorSplit) {
        const areaRect = editorPaneArea?.getBoundingClientRect();
        if (!areaRect) {
            return;
        }
        if (editorPaneSplit === 'horizontal') {
            const newHeight = Math.max(120, Math.min(e.clientY - areaRect.top, areaRect.height - 120));
            primaryEditorStack.style.flex = 'none';
            primaryEditorStack.style.height = `${newHeight}px`;
            primaryEditorStack.style.width = '';
        } else {
            const newWidth = Math.max(200, Math.min(e.clientX - areaRect.left, areaRect.width - 200));
            primaryEditorStack.style.flex = 'none';
            primaryEditorStack.style.width = `${newWidth}px`;
            primaryEditorStack.style.height = '';
        }
        layoutAllEditors();
    }
});

document.addEventListener('mouseup', () => {
    if (isResizingH || isResizingV || isResizingSidebar || isResizingSidebarSplit || isResizingEditorSplit) {
        isResizingH = false;
        isResizingV = false;
        isResizingSidebar = false;
        isResizingSidebarSplit = false;
        isResizingEditorSplit = false;
        document.body.style.cursor = '';
        document.body.style.userSelect = '';
        layoutAllEditors();
        scheduleProjectPersistence();
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
window.activeRightTab = '';
window.activeBottomTab = 'output';

function normalizeTabName(tabName, allowed, fallback) {
    return allowed.includes(tabName) ? tabName : fallback;
}

function setBottomPanelCollapsed(collapsed) {
    bottomPanel.classList.toggle('collapsed', Boolean(collapsed));
    const arrow = document.getElementById('bottomArrow');
    if (!arrow) return;
    arrow.innerHTML = bottomPanel.classList.contains('collapsed') ? '&#9650;' : '&#9660;';
    scheduleProjectPersistence();
}

function refreshFileRunSettingsButton() {
    if (editorSettingsButtons.length === 0) {
        return;
    }
    for (const button of editorSettingsButtons) {
        button.title = 'Simulation and codegen settings';
        button.setAttribute('aria-label', 'Simulation and codegen settings');
    }
}

window.openCodegenTab = function() {
    const modelName = currentCodegenModel();
    if (modelName && window.compiledModels?.[modelName]) {
        void createCodegenRunForModel(modelName);
        return;
    }
    showTemplateError('Compile a model before rendering code generation output.');
};

window.openCodegenTabForPane = function(paneId) {
    setActiveEditorPane(paneId);
    window.openCodegenTab();
};

window.openFileRunSettings = function() {
    openSimulationSettingsModal();
};

window.openFileRunSettingsForPane = function(paneId) {
    setActiveEditorPane(paneId);
    window.openFileRunSettings();
};

// Switch between right panel tabs (Simulate / Codegen)
window.switchRightTab = function(tabName) {
    const nextTab = tabName === 'codegen' ? 'codegen' : tabName === 'simulate' ? 'simulate' : '';
    setActiveRightTabState(nextTab);
    refreshFileRunSettingsButton();
    refreshResultsWindowChrome();
    // Refresh editors in the newly visible tab
    setTimeout(() => {
        layoutAllEditors();
        if (nextTab === 'codegen') {
            showActiveCodegenRun();
        }
        if (nextTab === 'simulate') {
            void resultsPanelController.renderModel(window.selectedModel || '');
        }
    }, 0);
    scheduleProjectPersistence();
};

// Toggle bottom panel collapse/expand (collapses down)
window.toggleBottomPanel = function() {
    setBottomPanelCollapsed(!bottomPanel.classList.contains('collapsed'));
    if (window.editor) window.editor.layout();
};

// Switch between bottom panel tabs
window.switchBottomTab = function(tabName) {
    const nextTab = tabName === 'errors' ? 'errors' : 'output';
    window.activeBottomTab = nextTab;
    // Update tab buttons (only within bottom panel)
    document.querySelectorAll('.panel-header-bottom .bottom-tab').forEach(tab => {
        tab.classList.toggle('active', tab.dataset.tab === nextTab);
    });
    // Update sections
    document.getElementById('outputSection').classList.toggle('active', nextTab === 'output');
    document.getElementById('errorsSection').classList.toggle('active', nextTab === 'errors');
    scheduleProjectPersistence();
};

function collectProjectEditorState() {
    const existingState = projectFs.getEditorState() || {};
    return {
        ...existingState,
        rightTab: typeof window.activeRightTab === 'string' ? window.activeRightTab : '',
        bottomTab: String(window.activeBottomTab || 'output'),
        bottomPanelCollapsed: bottomPanel.classList.contains('collapsed'),
        sidebarCollapsed: workbenchSidepanel?.classList.contains('collapsed') || false,
        sidebarMode: currentSidebarMode(),
        explorerSectionCollapsed: explorerSection?.classList.contains('collapsed') || false,
        outlineSectionCollapsed: outlineSection?.classList.contains('collapsed') || false,
        leftAreaWidth: String(leftArea?.style.width || existingState.leftAreaWidth || ''),
        sidebarWidth: String(workbenchSidebar?.style.width || existingState.sidebarWidth || ''),
        bottomPanelHeight: String(bottomPanel?.style.height || existingState.bottomPanelHeight || ''),
        explorerSectionHeight: String(explorerSection?.style.height || existingState.explorerSectionHeight || ''),
        explorerCollapsedNodeIds: [...explorerCollapsedNodes],
        outlineCollapsedNodeIds: [...outlineCollapsedNodes],
        selectedExplorerPath: trimMaybeString(selectedExplorerPath),
        editorPaneVisible,
        openDocuments: [...openDocumentPaths],
        codegenSettings: { ...codegenSettings },
        simResultsPanelState: normalizeSimResultsPanelState(simResultsPanelState),
    };
}

function applyProjectEditorState(editorState) {
    const fallbackState = defaultProjectSeed?.editorState || {
        rightTab: '',
        bottomTab: 'output',
        bottomPanelCollapsed: false,
        sidebarCollapsed: false,
        sidebarMode: 'explorer',
        explorerSectionCollapsed: false,
        outlineSectionCollapsed: false,
        leftAreaWidth: '',
        sidebarWidth: '',
        bottomPanelHeight: '',
        explorerSectionHeight: '',
        explorerCollapsedNodeIds: [],
        outlineCollapsedNodeIds: [],
        selectedExplorerPath: '',
        editorPaneVisible: true,
        openDocuments: [],
        codegenSettings: defaultCodegenSettings(),
        simResultsPanelState: emptySimResultsPanelState(),
    };
    const nextState = editorState && typeof editorState === 'object'
        ? {
            ...fallbackState,
            ...editorState,
        }
        : fallbackState;
    const rightTab = normalizeTabName(
        String(nextState.rightTab || fallbackState.rightTab),
        ['simulate', 'codegen', ''],
        '',
    );
    const bottomTab = normalizeTabName(
        String(nextState.bottomTab || fallbackState.bottomTab),
        ['output', 'errors'],
        'output',
    );
    const sidebarMode = normalizeTabName(
        String(nextState.sidebarMode || fallbackState.sidebarMode || 'explorer'),
        ['explorer', 'project'],
        'explorer',
    );

    codegenSettings = normalizeCodegenSettings(
        migrateLegacyCodegenTemplateState(nextState)
        || nextState.codegenSettings
        || fallbackState.codegenSettings,
    );
    refreshCodegenTemplateSummary();

    simResultsPanelState = normalizeSimResultsPanelState(
        nextState.simResultsPanelState ?? fallbackState.simResultsPanelState,
        nextState.simResultsActiveViewId ?? fallbackState.simResultsActiveViewId,
    );
    explorerCollapsedNodes.clear();
    for (const nodeId of Array.isArray(nextState.explorerCollapsedNodeIds)
        ? nextState.explorerCollapsedNodeIds
        : fallbackState.explorerCollapsedNodeIds || []) {
        const nextNodeId = trimMaybeString(nodeId);
        if (nextNodeId) {
            explorerCollapsedNodes.add(nextNodeId);
        }
    }
    outlineCollapsedNodes.clear();
    for (const nodeId of Array.isArray(nextState.outlineCollapsedNodeIds)
        ? nextState.outlineCollapsedNodeIds
        : fallbackState.outlineCollapsedNodeIds || []) {
        const nextNodeId = trimMaybeString(nodeId);
        if (nextNodeId) {
            outlineCollapsedNodes.add(nextNodeId);
        }
    }
    const restoredActivePath = trimMaybeString(projectFs.getActiveDocumentPath());
    if (Array.isArray(nextState.openDocuments)) {
        syncOpenDocuments(nextState.openDocuments, restoredActivePath);
    } else {
        syncOpenDocuments(fallbackState.openDocuments, restoredActivePath);
    }
    selectedExplorerPath = trimMaybeString(nextState.selectedExplorerPath)
        || restoredActivePath;
    editorPaneVisible = nextState.editorPaneVisible !== false;
    setSidebarMode(sidebarMode, { persist: false });
    setBottomPanelCollapsed(Boolean(nextState.bottomPanelCollapsed));
    setSidebarCollapsed(Boolean(nextState.sidebarCollapsed));
    setSidebarSectionCollapsed('explorer', Boolean(nextState.explorerSectionCollapsed));
    setSidebarSectionCollapsed('outline', Boolean(nextState.outlineSectionCollapsed));
    const savedLeftAreaWidth = trimMaybeString(nextState.leftAreaWidth);
    if (savedLeftAreaWidth && /^\d+px$/.test(savedLeftAreaWidth) && !isNarrowLayout()) {
        leftArea.style.flex = 'none';
        leftArea.style.width = savedLeftAreaWidth;
    } else {
        syncResponsiveLayoutState();
    }
    const savedSidebarWidth = trimMaybeString(nextState.sidebarWidth);
    if (savedSidebarWidth && /^\d+px$/.test(savedSidebarWidth) && !isNarrowLayout() && workbenchSidebar) {
        workbenchSidebar.style.width = savedSidebarWidth;
    }
    const savedBottomPanelHeight = trimMaybeString(nextState.bottomPanelHeight);
    if (savedBottomPanelHeight && /^\d+px$/.test(savedBottomPanelHeight)) {
        bottomPanel.style.height = savedBottomPanelHeight;
    }
    const savedExplorerHeight = trimMaybeString(nextState.explorerSectionHeight);
    if (savedExplorerHeight && /^\d+px$/.test(savedExplorerHeight) && !isNarrowLayout() && explorerSection) {
        explorerSection.style.flex = 'none';
        explorerSection.style.height = savedExplorerHeight;
    } else if (explorerSection) {
        explorerSection.style.flex = '';
        explorerSection.style.height = '';
    }
    updateSidebarSplitHandleVisibility();
    window.switchRightTab(rightTab);
    window.switchBottomTab(bottomTab);
    refreshWorkbenchNavigation();
}

function buildNewProjectState() {
    const seed = defaultProjectSeed || {
        activeDocumentPath: 'Main.mo',
        activeDocumentContent: 'model Main\nend Main;\n',
        editorState: {
            rightTab: '',
            bottomTab: 'output',
            bottomPanelCollapsed: false,
            sidebarCollapsed: false,
            sidebarMode: 'explorer',
            explorerSectionCollapsed: false,
            outlineSectionCollapsed: false,
            leftAreaWidth: '',
            sidebarWidth: '',
            bottomPanelHeight: '',
            explorerSectionHeight: '',
            explorerCollapsedNodeIds: [],
            outlineCollapsedNodeIds: [],
            openDocuments: [projectFs.getActiveDocumentPath()],
            codegenSettings: defaultCodegenSettings(),
            simResultsPanelState: emptySimResultsPanelState(),
        },
    };
    projectFs.clearProject();
    projectFs.setActiveDocument(seed.activeDocumentPath, seed.activeDocumentContent);
    projectFs.setEditorState(seed.editorState);
    return {
        activeDocumentPath: projectFs.getActiveDocumentPath(),
        activeDocumentContent: projectFs.getActiveDocumentContent(),
        editorState: projectFs.getEditorState(),
        packageArchives: [],
        fileCount: projectFs.listFileEntries().length,
    };
}

function projectSimulationFallback() {
    return {
        solver: 'auto',
        tEnd: 10.0,
        dt: null,
        outputDir: '',
        sourceRootPaths: [],
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
    return trimMaybeString(
        document.getElementById('modelSelect')?.value
        || window.selectedModel
        || projectFs.getEditorState()?.selectedSimulationModel
        || '',
    );
}

function collectWorkspaceModelicaSources(excludePath = projectFs.getActiveDocumentPath()) {
    const excluded = normalizePath(excludePath);
    const sources = {};
    for (const entry of projectFs.listFiles()) {
        const path = normalizePath(entry?.path);
        if (!path || path === excluded || !path.endsWith('.mo')) {
            continue;
        }
        if (String(path).startsWith('.rumoca/') || entry?.sourceKind === 'packageArchive') {
            continue;
        }
        if (typeof entry?.content !== 'string') {
            continue;
        }
        sources[path] = entry.content;
    }
    return sources;
}

function collectWorkspaceModelicaSourcesJson(excludePath = projectFs.getActiveDocumentPath()) {
    return JSON.stringify(collectWorkspaceModelicaSources(excludePath));
}

function simulationModelPreference(preferredModel = '') {
    return trimMaybeString(preferredModel)
        || trimMaybeString(window.selectedModel)
        || trimMaybeString(projectFs.getEditorState()?.selectedSimulationModel)
        || trimMaybeString(document.getElementById('modelSelect')?.value || '');
}

function updateSimulationModelOptions(models, selectedModel = '') {
    const modelSelect = document.getElementById('modelSelect');
    if (!modelSelect) {
        return {
            models: [],
            selectedModel: '',
        };
    }
    const uniqueModels = Array.from(new Set(
        (Array.isArray(models) ? models : [])
            .map((entry) => trimMaybeString(entry))
            .filter(Boolean),
    ));
    const preferred = simulationModelPreference(selectedModel);
    modelSelect.innerHTML = uniqueModels.length === 0
        ? '<option value="">-- No models --</option>'
        : uniqueModels.map((model) => `<option value="${model}">${model}</option>`).join('');
    const resolvedSelection = preferred && uniqueModels.includes(preferred)
        ? preferred
        : uniqueModels[0] || '';
    modelSelect.value = resolvedSelection;
    if (resolvedSelection) {
        projectInterface.execute('rumoca.project.setSelectedSimulationModel', { model: resolvedSelection });
    }
    return {
        models: uniqueModels,
        selectedModel: resolvedSelection,
    };
}

async function resolveSimulationModels(preferredModel = '') {
    const preferred = simulationModelPreference(preferredModel);
    const knownModels = Array.from(new Set([
        ...listSimulationModels(),
        ...Object.keys(window.compiledModels || {}).map((entry) => trimMaybeString(entry)),
        preferred,
    ].filter(Boolean)));
    if (knownModels.length > 0) {
        return updateSimulationModelOptions(knownModels, preferred);
    }
    if (!projectFs.getActiveDocumentPath().endsWith('.mo') || !editor) {
        return updateSimulationModelOptions([], preferred);
    }
    const modelState = await getSimulationModelState(editor.getValue(), preferred);
    return updateSimulationModelOptions(modelState.models, modelState.selectedModel || preferred);
}

function simulationSettingsFeatures() {
    return {
        addSourceRootPath: false,
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

async function buildSimulationSettingsDocument(model, availableModels = listSimulationModels()) {
    const config = projectInterface.execute('rumoca.project.getSimulationConfig', {
        model,
        fallback: projectSimulationFallback(),
    });
    const templates = await ensureBuiltInCodegenTemplatesLoaded();
    return sharedVisualization().buildHostedSimulationSettingsDocument(
        sharedVisualization().buildHostedSimulationSettingsState({
            activeModel: model,
            availableModels,
            current: config?.effective,
            fallbackCurrent: projectSimulationFallback(),
            codegen: codegenSettings,
            fallbackCodegen: defaultCodegenSettings(),
            codegenTemplates: templates,
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
    const resolvedModels = await resolveSimulationModels(preferredModel);
    const model = trimMaybeString(preferredModel)
        || trimMaybeString(resolvedModels.selectedModel)
        || resolvedModels.models[0]
        || 'Model';
    simulationSettingsFrame.srcdoc = await buildSimulationSettingsDocument(model, resolvedModels.models);
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
            scheduleProjectPersistence();
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
            scheduleProjectPersistence();
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
    save: async ({ model, preset, codegenSettings: nextCodegenSettings, views }) => {
        const saved = await saveSimulationSettingsForModel(model, preset, views);
        await applyCodegenSettings(nextCodegenSettings, { rerender: false });
        return saved;
    },
    reset: async ({ model }) => {
        const resetCodegen = defaultCodegenSettings();
        const resetState = await resetSimulationSettingsForModel(model);
        await applyCodegenSettings(resetCodegen, { rerender: false });
        return {
            ...resetState,
            codegen: resetCodegen,
        };
    },
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
sidebarContextMenu?.addEventListener('click', (event) => {
    event.stopPropagation();
});
sidebarContextActionBtn?.addEventListener('click', async (event) => {
    event.stopPropagation();
    const action = sidebarContextAction;
    closeSidebarContextMenu();
    if (typeof action?.run === 'function') {
        await action.run();
    }
});
fileMenuButton?.addEventListener('click', (event) => {
    event.stopPropagation();
    toggleFileMenu();
});
fileMenuPanel?.addEventListener('click', (event) => {
    event.stopPropagation();
});
document.addEventListener('click', () => {
    closeTitlebarMenu();
    closeSidebarContextMenu();
});
document.addEventListener('keydown', (event) => {
    if (event.key === 'Escape') {
        closeTitlebarMenu();
        closeSidebarContextMenu();
        closeSimulationSettingsModal();
    }
});
document.addEventListener('visibilitychange', () => {
    if (document.visibilityState === 'hidden') {
        void flushProjectPersistence();
    }
});
window.addEventListener('resize', () => {
    closeSidebarContextMenu();
});
window.addEventListener('pagehide', () => {
    void flushProjectPersistence();
});

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
    const runButtons = editorRunButtons;
    const status = document.getElementById('simStatus');

    for (const button of runButtons) {
        button.disabled = true;
    }
    status.textContent = 'Simulating...';
    status.style.color = '#9a6700';

    try {
        const result = await projectInterface.execute('rumoca.project.startSimulation', {
            source,
            model: modelName,
            fallback: projectSimulationFallback(),
            projectSources: collectWorkspaceModelicaSourcesJson(projectFs.getActiveDocumentPath()),
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
        window.switchRightTab('simulate');
    } catch (e) {
        status.textContent = e.message || 'Simulation failed';
        status.style.color = '#c9184a';
    } finally {
        for (const button of runButtons) {
            button.disabled = false;
        }
    }
};

window.runSimulationForPane = function(paneId) {
    setActiveEditorPane(paneId);
    return window.runSimulation();
};

// Display output for the active tab
function displayModelOutput(modelName) {
    if (window.activeRightTab === 'codegen') {
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
    if (!trimMaybeString(modelName)) {
        setCodegenOutput('No model selected');
        clearTemplateErrors();
        return;
    }
    const result = window.compiledModels[modelName];
    if (!result || result.error || !result.dae) {
        setCodegenOutput(result?.error ? '' : 'No DAE available');
        return;
    }
    if (!result.dae_native) {
        setCodegenOutput('Native DAE not available for template rendering.');
        clearTemplateErrors();
        return;
    }
    try {
        await ensureBuiltInCodegenTemplatesLoaded();
        const templateSelection = await resolveCodegenTemplateSelection();
        const daeJson = JSON.stringify(result.dae_native);
        const rendered = await sendWorkspaceCommand('rumoca.workspace.renderTemplate', {
            daeJson,
            template: templateSelection.source,
        });
        setCodegenOutput(rendered, templateSelection.language);
        clearTemplateErrors();
    } catch (e) {
        setCodegenOutput('');
        showTemplateError(e.message);
    }
}

async function createCodegenRunForModel(modelName) {
    const nextModel = trimMaybeString(modelName);
    if (!nextModel) {
        showTemplateError('No model selected for code generation output.');
        return;
    }
    const result = window.compiledModels[nextModel];
    if (!result || result.error || !result.dae_native) {
        showTemplateError('Compile a model before rendering code generation output.');
        return;
    }
    try {
        await ensureBuiltInCodegenTemplatesLoaded();
        const templateSelection = await resolveCodegenTemplateSelection();
        const daeJson = JSON.stringify(result.dae_native);
        const rendered = await sendWorkspaceCommand('rumoca.workspace.renderTemplate', {
            daeJson,
            template: templateSelection.source,
        });
        const createdAt = new Date().toISOString();
        addCodegenRun({
            id: `codegen_${Date.now()}_${++codegenRunSequence}`,
            label: formatCodegenRunLabel(nextModel, createdAt),
            createdAt,
            modelName: nextModel,
            text: rendered,
            language: templateSelection.language,
            templateLabel: templateSelection.label,
        });
        clearTemplateErrors();
        window.switchRightTab('codegen');
    } catch (e) {
        showTemplateError(e.message || 'Failed to render code generation output.');
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

const packageArchiveController = createPackageArchiveController({
    sendLanguageCommand,
    sendWorkspaceCommand,
    setTerminalOutput,
    isWorkerReady: () => workerReady,
    projectFs,
    onPackageArchivesChanged: () => {
        collapseImportedExplorerBranches();
        refreshWorkbenchNavigation({ includeOutline: false });
        scheduleProjectPersistence(2000);
    },
});
packageArchiveController.bindWindowApi();

// Store compiled results for all models: { modelName: { dae, balance } }
window.compiledModels = {};
window.selectedModel = null;

function resetCompiledWorkspaceState() {
    window.compiledModels = {};
    window.selectedModel = null;
    window.currentDaeForCompletions = null;
    simResultsPanelState = emptySimResultsPanelState();
    resultsPanelController.clear();
    const modelSelect = document.getElementById('modelSelect');
    if (modelSelect) {
        modelSelect.innerHTML = '<option value="">-- No models --</option>';
        modelSelect.value = '';
    }
    projectInterface.execute('rumoca.project.setSelectedSimulationModel', { model: '' });
    setCompileStatusBadge('Waiting...', 'loading');
    displayCodegenOutput('');
    void resultsPanelController.renderModel('');
    refreshResultsWindowChrome();
}

async function applyImportedProject(projectState) {
    resetCompiledWorkspaceState();
    startupCompileRequested = false;
    suspendWorkspaceObservers = true;
    try {
        if (editor?.setValue) {
            editor.setValue(projectState.activeDocumentContent || '');
        }
        applyProjectEditorState(projectState.editorState);
        const activePath = trimMaybeString(projectFs.getActiveDocumentPath());
        if (activePath) {
            projectFs.activateDocument(activePath);
        }
    } finally {
        suspendWorkspaceObservers = false;
    }
    await packageArchiveController.restoreProjectPackageArchives();
    updateSourceBreadcrumbs();
    refreshWorkbenchNavigation();
    await resultsPanelController.renderModel(window.selectedModel || '');
    refreshSimulationSettingsModalIfOpen();
    scheduleProjectPersistence(0);
    requestStartupCompileIfReady();
}

async function restorePersistedProjectIfAvailable() {
    const entries = await loadPersistedProjectEntries();
    if (!Array.isArray(entries) || entries.length === 0) {
        return false;
    }
    const projectState = projectFs.loadArchiveEntries(entries);
    await applyImportedProject(projectState);
    return true;
}

installFileActions({
    getEditor: () => window.editor,
    getCompiledModels: () => window.compiledModels,
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
        if (window.activeRightTab === 'codegen') displayCodegenOutput(modelName);
        // Update DAE for template autocompletion
        if (result.dae_native) {
            window.currentDaeForCompletions = result.dae_native;
        }
    }
    refreshResultsWindowChrome();
    updateSourceBreadcrumbs();
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

    // Handle progress updates for package-archive loading
    if (progress) {
        packageArchiveController.handleWorkerProgress({ current, total, percent });
        return;
    }

    if (ready !== undefined) {
        if (success) {
            setRuntimeStatusBar('Ready', 'ready');
            workerReady = true;
            if (window.refreshModelicaSemanticTokens) {
                window.refreshModelicaSemanticTokens();
            }
            refreshWorkbenchNavigation();
            requestStartupCompileIfReady();
            // Fetch version and show welcome message
            sendWorkspaceCommand('rumoca.workspace.getVersion', {}).then(version => {
                setTerminalOutput(`Rumoca v${version} - Modelica Compiler\nHover over tabs/buttons for help.`);
            }).catch(() => {
                setTerminalOutput('WASM initialized! Edit code to compile.');
            });
        } else {
            setRuntimeStatusBar('Error', 'error');
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
    setRuntimeStatusBar('Worker Error', 'error');
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

function languageCommandNeedsProjectSources(command) {
    return command === 'rumoca.language.diagnostics'
        || command === 'rumoca.language.hover'
        || command === 'rumoca.language.completion'
        || command === 'rumoca.language.completionWithTiming'
        || command === 'rumoca.language.definition'
        || command === 'rumoca.language.documentSymbols';
}

function augmentLanguagePayload(command, payload = {}) {
    if (!languageCommandNeedsProjectSources(command)) {
        return payload;
    }
    const activePath = projectFs?.getActiveDocumentPath?.() || '';
    return {
        ...payload,
        projectSources: collectWorkspaceModelicaSourcesJson(activePath),
    };
}

function sendLanguageCommand(command, payload = {}, timeout = 30000) {
    return sendRequest(
        'languageCommand',
        {
            command,
            payload: augmentLanguagePayload(command, payload),
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
        packageArchiveUrl: params.get('smoke_package_archive_url') || '',
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

async function loadRumocaSmokePackageArchive(packageArchiveUrl) {
    const response = await fetch(packageArchiveUrl, { cache: 'no-store' });
    if (!response.ok) {
        throw new Error(`Failed to fetch smoke package archive: ${response.status} ${response.statusText}`);
    }
    const bytes = await response.arrayBuffer();
    const fileName = packageArchiveUrl.split('/').pop() || 'smoke-package-archive.zip';
    const file = new File([bytes], fileName, { type: 'application/zip' });
    if (typeof window.stagePackageArchiveFile !== 'function') {
        throw new Error('package-archive staging unavailable for smoke archive load');
    }
    rumocaSmokeSourceRootsImported = false;
    return await window.stagePackageArchiveFile(file);
}

function normalizeCompletionItems(rawCompletion) {
    const items = rawCompletion?.items || rawCompletion || [];
    return Array.isArray(items) ? items : [];
}

const EXPECTED_MSL_COMPLETION_LABEL = 'Electrical';
const EXPECTED_MSL_NAVIGATION_LABEL = 'Ground';
const ACTIVE_WASM_URI = 'file:///input.mo';
let rumocaSmokeSourceRootsImported = false;

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
    const { ensureSourceRootImport = false } = options;
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
    let sourceRootImportMs = 0;
    if (ensureSourceRootImport && !rumocaSmokeSourceRootsImported) {
        if (typeof window.importLoadedPackageArchivesForSmoke !== 'function') {
            throw new Error('source-root import unavailable for smoke completion measurement');
        }
        const imported = await window.importLoadedPackageArchivesForSmoke();
        sourceRootImportMs = Math.max(0, Number(imported?.sourceRootImportMs) || 0);
        rumocaSmokeSourceRootsImported = true;
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
        sourceRootImportMs,
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
        sourceRootCount: 0,
        statusText: '',
    };
    setRumocaSmokeResult('running', result);

    try {
        await waitForRumocaSmoke(
            'worker ready',
            () => workerReady && window.editor && window.loadPackageArchiveFile,
            config.readyTimeoutMs,
        );

        if (config.packageArchiveUrl) {
            const stagedArchive = await loadRumocaSmokePackageArchive(config.packageArchiveUrl);
            result.archiveLoadMs = Math.max(0, Number(stagedArchive?.archivePrepMs) || 0);
            result.sourceRootCount = await waitForRumocaSmoke(
                'loaded source-root count',
                () => {
                    const count = parseBadgeNumber(document.getElementById('packageArchiveCount')?.textContent || '0');
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
        const sourceRootLoad = await measureRumocaSmokeCompletion(
            completionProbeSource,
            config.completionTimeoutMs,
            { ensureSourceRootImport: true },
        );
        result.sourceRootLoadMs = sourceRootLoad.completionMs;
        result.sourceRootImportMs = sourceRootLoad.sourceRootImportMs;
        result.sourceRootLoadCompletionCount = sourceRootLoad.completionCount;
        result.sourceRootExpectedCompletionPresent = sourceRootLoad.expectedCompletionPresent;
        result.sourceRootStageTimings = sourceRootLoad.stageTimings;
        if (!result.sourceRootExpectedCompletionPresent) {
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
        const projectSources = collectWorkspaceModelicaSourcesJson(projectFs.getActiveDocumentPath());
        const compileJson = await sendWorkspaceCommand(
            'rumoca.workspace.compileWithProjectSources',
            {
                source: window.editor.getValue(),
                modelName: smokeModelName,
                projectSources,
            },
            config.compileTimeoutMs,
        );
        const compileResult = JSON.parse(compileJson);
        result.compileMs = Math.round(performance.now() - compileStart);
        result.modelName = smokeModelName;
        result.sourceRootCount = parseBadgeNumber(document.getElementById('packageArchiveCount')?.textContent || '0');
        result.statusText = String(document.getElementById('outputCompileStatus')?.textContent || '');
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
                        window.switchRightTab('simulate');
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

function buildNestedDocumentSymbolTree(symbols, parentPath = '') {
    const nodes = [];
    if (!Array.isArray(symbols)) {
        return nodes;
    }
    for (const symbol of symbols) {
        if (!symbol || typeof symbol !== 'object') continue;
        const name = String(symbol.name || '').trim();
        if (!name) continue;
        const fullName = parentPath ? `${parentPath}.${name}` : name;
        const pos = symbolStartPosition(symbol);
        const children = buildNestedDocumentSymbolTree(symbol.children, fullName);
        nodes.push({
            id: `outline:${fullName}`,
            label: name,
            kind: children.length > 0 ? 'dir' : 'file',
            meta: String(symbol.detail || ''),
            sourceKind: 'workspace',
            run: () => jumpToModelicaLocation(pos.lineNumber, pos.column),
            children,
        });
    }
    return nodes;
}

function buildFlatDocumentSymbolTree(symbols) {
    const rootNodes = [];
    if (!Array.isArray(symbols)) {
        return rootNodes;
    }
    for (const symbol of symbols) {
        if (!symbol || typeof symbol !== 'object') continue;
        const name = String(symbol.name || '').trim();
        if (!name) continue;
        const containerName = String(symbol.containerName || symbol.container_name || '').trim();
        const pos = symbolStartPosition(symbol);
        let currentNodes = rootNodes;
        let prefix = '';
        if (containerName) {
            const parts = containerName.split('.').filter(Boolean);
            for (const part of parts) {
                prefix = prefix ? `${prefix}.${part}` : part;
                const branch = ensureSidebarBranch(currentNodes, `outline:${prefix}`, part, {
                    sourceKind: 'workspace',
                });
                currentNodes = branch.children;
            }
        }
        currentNodes.push(createSidebarNode(`outline:${containerName ? `${containerName}.` : ''}${name}`, name, {
            kind: 'file',
            meta: 'Symbol',
            sourceKind: 'workspace',
            run: () => jumpToModelicaLocation(pos.lineNumber, pos.column),
        }));
    }
    return rootNodes;
}

function flattenNestedDocumentSymbols(symbols, sink, parentPath = '', depth = 0) {
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
                depth,
                tags: ['symbol', 'outline'],
                run: () => jumpToModelicaLocation(pos.lineNumber, pos.column),
            });
        }
        flattenNestedDocumentSymbols(symbol.children, sink, fullName, depth + 1);
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
            depth: Math.max(0, containerName ? containerName.split('.').length : 0),
            tags: ['symbol', 'outline'],
            run: () => jumpToModelicaLocation(pos.lineNumber, pos.column),
        });
    }
    return sink;
}

async function buildDocumentSymbolTreeNodes() {
    if (!editor || !workerReady) return [];
    try {
        const source = editor.getValue();
        const json = await sendLanguageCommand('rumoca.language.documentSymbols', { source });
        const parsed = JSON.parse(json);
        const symbols = normalizeDocumentSymbols(parsed);
        const nestedNodes = buildNestedDocumentSymbolTree(symbols.nested);
        return nestedNodes.length > 0 ? nestedNodes : buildFlatDocumentSymbolTree(symbols.flat);
    } catch (error) {
        console.warn('Symbol tree failed:', error);
        return [];
    }
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
    monacoApi = monaco;
    createSourceEditorFactory = monacoState.createSourceEditor;
    editor = monacoState.editor;
    editorPanes.primary.editor = editor;
    projectFs.setActiveDocument(
        inferModelicaFileName(editor.getValue(), 'Ball.mo'),
        editor.getValue(),
    );
    syncOpenDocuments([projectFs.getActiveDocumentPath()], projectFs.getActiveDocumentPath());
    editorPanes.primary.activePath = projectFs.getActiveDocumentPath();
    refreshWorkbenchNavigation();
    defaultProjectSeed = {
        activeDocumentPath: projectFs.getActiveDocumentPath(),
        activeDocumentContent: editor.getValue(),
        editorState: collectProjectEditorState(),
    };
    applyActiveEditorLockState();
    window.openProjectDocument = (path) => {
        void openProjectDocument(path);
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

    const scheduleSemanticTokenRefresh = debounce(() => {
        if (window.refreshModelicaSemanticTokens) {
            window.refreshModelicaSemanticTokens();
        }
    }, 300);

    const scheduleTypingOutlineRefresh = debounce(() => {
        if (outlineSection?.classList.contains('collapsed')) {
            return;
        }
        scheduleOutlineRefresh(0);
    }, 350);

    const scheduleCodegenRefresh = debounce(() => {
        if (window.activeRightTab !== 'codegen') {
            return;
        }
        const modelName = currentSimulationModel();
        if (modelName && window.compiledModels?.[modelName]) {
            void displayCodegenOutput(modelName);
        }
    }, 350);

    diagnosticsController = createDiagnosticsController({
        monaco,
        getModelEditor: () => editor,
        getModelPath: () => projectFs.getActiveDocumentPath(),
        getTemplateEditor: () => null,
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
        const modelSelect = document.getElementById('modelSelect');
        const startTime = performance.now();
        const setCompileStatus = (text, color) => {
            const nextText = String(text || '');
            const tone = color === '#c9184a'
                ? 'error'
                : color === '#2d6a4f'
                    ? 'ready'
                    : color === '#9a6700'
                        ? 'loading'
                        : 'loading';
            setCompileStatusBadge(nextText, tone);
        };

        setCompileStatus('Compiling...', '#9a6700');
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
        updateSimulationModelOptions(models, modelState.selectedModel || previousSelection);
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
            setCompileStatus(
                hasParseErrors ? `Syntax Error (${elapsed}ms)` : `Error (${elapsed}ms)`,
                '#c9184a',
            );
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
            if (document.getElementById('classTreePanel')) {
                packageArchiveController.refreshPackageViewer(true);
            }
            return;
        }

        // Compile all models
        const projectSources = collectWorkspaceModelicaSourcesJson(projectFs.getActiveDocumentPath());
        let successCount = 0;
        const compileErrors = [];

        for (const modelName of models) {
            if (isStaleRun()) return;
            try {
                let json;
                console.log('[compile] compiling model:', modelName);
                json = await sendWorkspaceCommand('rumoca.workspace.compileWithProjectSources', {
                    source,
                    modelName,
                    projectSources,
                });
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
            if (window.activeRightTab === 'codegen') displayCodegenOutput(selectedModel);

            if (result.error) {
                setCompileStatus(`Error (${elapsed}ms)`, '#c9184a');
                window.switchBottomTab('errors');
            } else {
                const b = result.balance;
                if (b) {
                    const balanceStatus = b.is_balanced ? 'BALANCED' :
                        (typeof b.status === 'string' ? b.status.toUpperCase() :
                        (b.status.CompileError ? 'ERROR' : 'UNBALANCED'));
                    const modelCountStr = models.length > 1 ? ` [${successCount}/${models.length}]` : '';
                    setCompileStatus(
                        b.is_balanced
                            ? `Balanced (${elapsed}ms)${modelCountStr}`
                            : `${balanceStatus} (${elapsed}ms)${modelCountStr}`,
                        b.is_balanced ? '#2d6a4f' : '#c9184a',
                    );
                }
            }
        } else if (selectedModel && models.includes(selectedModel)) {
            setDaeOutput(`Waiting for compilation of ${selectedModel}...`);
            setCompileStatus(`${elapsed}ms`, '#888');
        } else {
            setDaeOutput(models.length === 0 ? 'No models found in source' : 'Select a model');
            setCompileStatus(models.length === 0 ? 'No models' : `${elapsed}ms`, '#888');
        }

        // Refresh CodeLens to show balance for all models
        if (window.refreshCodeLens) window.refreshCodeLens();
        if (document.getElementById('classTreePanel')) {
            packageArchiveController.refreshPackageViewer(true);
        }
        refreshSimulationSettingsModalIfOpen();
        } catch (unexpectedError) {
            if (isStaleRun()) return;
            // Catch any unexpected errors to ensure status is always updated
            console.error('[runLiveChecks] Unexpected error:', unexpectedError);
            setCompileStatus('Error', '#c9184a');
            updateCompileErrors([{ model: 'live-check', message: String(unexpectedError?.message || unexpectedError) }]);
        }
    }, 1200);
    window.triggerCompileNow = () => {
        runLiveChecks();
    };
    requestStartupCompileIfReady();

    bindPaneEditorToWorkspace = (paneId, paneEditor) => {
        if (!paneEditor || paneEditor.__rumocaPaneBound) {
            return;
        }
        paneEditor.__rumocaPaneBound = true;
        paneEditor.onDidFocusEditorText(() => {
            setActiveEditorPane(paneId);
        });
        paneEditor.onDidChangeModelContent(() => {
            if (suspendWorkspaceObservers) return;
            if (activeEditorPaneId !== paneId) {
                setActiveEditorPane(paneId);
            }
            const pane = getEditorPane(paneId);
            const activePath = trimMaybeString(pane?.activePath);
            if (activePath) {
                projectFs.setFile(activePath, paneEditor.getValue());
                if (activeEditorPaneId === paneId) {
                    projectFs.activateDocument(activePath);
                }
            }
            updateSourceBreadcrumbs();
            scheduleProjectPersistence();
            scheduleSemanticTokenRefresh();
            if (projectFs.getActiveDocumentPath().endsWith('.mo')) {
                scheduleTypingOutlineRefresh();
            }
            if (
                window.activeRightTab === 'codegen'
                && codegenSettings.mode === 'custom'
                && projectFs.getActiveDocumentPath() === trimMaybeString(codegenSettings.customTemplatePath)
            ) {
                scheduleCodegenRefresh();
            }
            if (isRumocaSmokeMode()) return;
            if (!projectFs.getActiveDocumentPath().endsWith('.mo')) return;
            runLiveChecks();
        });
        paneEditor.onDidChangeCursorPosition(() => {
            if (activeEditorPaneId !== paneId) {
                return;
            }
            updateSourceBreadcrumbs();
        });
    };

    bindPaneEditorToWorkspace('primary', editor);

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

    const smokeConfig = readRumocaSmokeConfig();
    void (async () => {
        let restored = false;
        try {
            restored = await restorePersistedProjectIfAvailable();
        } catch (error) {
            console.warn('Failed to restore persisted WASM project state:', error);
            await applyImportedProject(buildNewProjectState());
            restored = true;
        }

        projectPersistenceReady = true;
        scheduleProjectPersistence(0);

        if (!restored) {
            setTimeout(() => {
                updateSourceBreadcrumbs();
                if (isRumocaSmokeMode()) return;
                runLiveChecks();
            }, 1000);
        }

        if (smokeConfig) {
            void runRumocaBrowserSmoke(smokeConfig).catch(error => {
                console.error('[rumoca-smoke] browser smoke failed:', error);
            });
        }
    })();
});
