import { setupCommandPalette } from './modules/command_palette.js';
import * as shared from '../vendor/visualization_shared.js';
import { createSourceBreadcrumbs } from './modules/breadcrumbs.js';
import { createPackageArchiveController } from './modules/package_archive_controller.js';
import { createDiagnosticsController } from './modules/diagnostics_panel.js';
import { installFileActions } from './modules/file_actions.js';
import { createScenarioConfigEditorController } from './modules/scenario_config_editor.js';
import {
    createResultFileEditorController,
    interactiveRunPathForScenario,
    isInteractiveRunPath,
    isRumocaResultPath,
} from './modules/result_file_editor.js';
import { setupMonacoWorkspace } from './modules/monaco_setup.js';
import { createScenarioInterface } from './modules/scenario_interface.js';
import { buildScenarioVisualizationViewStorage } from './modules/visualization_view_storage.js';
import {
    createWorkspaceFilesystem,
    inferModelicaFileName,
    normalizePath,
} from './modules/workspace_fs.js';
import { loadDefaultWorkspaceEntries } from './modules/default_workspace.js';

let editor;
const workspaceFs = createWorkspaceFilesystem();
const WASM_WORKSPACE_PERSIST_DB = 'rumoca-wasm-editor';
const WASM_WORKSPACE_PERSIST_STORE = 'autosave';
const WASM_WORKSPACE_PERSIST_KEY = 'active-workspace';
const DEFAULT_WEB_RESULTS_OUTPUT_DIR = 'results';
let workspacePersistenceTimer = null;
let workspacePersistenceInFlight = Promise.resolve();
let workspacePersistenceReady = false;
const scenarioInterface = createScenarioInterface({
    workspaceFs,
    runtimeBridge: {
        request(action, payload = {}, timeoutMs) {
            return sendRequest(action, payload, timeoutMs);
        },
    },
    onWorkspaceMutation() {
        scheduleWorkspacePersistence();
    },
});
let suspendWorkspaceObservers = false;
let defaultWorkspaceSeed = null;
let openDocumentPaths = [];
const fileMenuButton = document.getElementById('fileMenuButton');
const fileMenuPanel = document.getElementById('fileMenuPanel');
const packageArchiveInput = document.getElementById('packageArchiveInput');
const sidebarBackdrop = document.getElementById('sidebarBackdrop');
const sidebarBrandToggle = document.getElementById('sidebarBrandToggle');
const mobileSidebarBtn = document.getElementById('mobileSidebarBtn');
const mobileAppbarTitle = document.getElementById('mobileAppbarTitle');
const themeSelects = Array.from(document.querySelectorAll('[data-theme-select]'));
const workbenchSidepanel = document.getElementById('workbenchSidepanel');
const workbenchSidebar = document.getElementById('workbenchSidebar');
const explorerSidebarPanel = document.getElementById('explorerSidebarPanel');
const projectSection = document.getElementById('projectSection');
const explorerSection = document.getElementById('explorerSection');
const outlineSection = document.getElementById('outlineSection');
const projectSectionToggle = document.getElementById('projectSectionToggle');
const explorerSectionToggle = document.getElementById('explorerSectionToggle');
const outlineSectionToggle = document.getElementById('outlineSectionToggle');
const projectSectionArrow = document.getElementById('projectSectionArrow');
const explorerSectionArrow = document.getElementById('explorerSectionArrow');
const outlineSectionArrow = document.getElementById('outlineSectionArrow');
const explorerNewFileBtn = document.getElementById('explorerNewFileBtn');
const explorerNewFolderBtn = document.getElementById('explorerNewFolderBtn');
const explorerTreeToggleBtn = document.getElementById('explorerTreeToggleBtn');
const outlineTreeToggleBtn = document.getElementById('outlineTreeToggleBtn');
const resizeHandleSidebar = document.getElementById('resizeHandleSidebar');
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
const editorCodegenButtons = Array.from(document.querySelectorAll('[data-editor-codegen-btn]'));
const editorActionGroups = Array.from(document.querySelectorAll('[data-editor-actions]'));
const editorCloseButtons = Array.from(document.querySelectorAll('[data-editor-close-btn]'));
const sidebarContextMenu = document.getElementById('sidebarContextMenu');
const scenarioVisualizationStorage = buildScenarioVisualizationViewStorage({ workspaceFs });
const editorViewStates = new Map();
const editorPanes = {
    primary: {
        id: 'primary',
        stackEl: primaryEditorStack,
        tabsEl: editorTabsRoot,
        editorElId: 'editor',
        scenarioFrameElId: 'primaryScenarioConfigFrame',
        resultViewElId: 'primaryResultFileView',
        paths: [],
        activePath: '',
        editor: null,
    },
    secondary: {
        id: 'secondary',
        stackEl: secondaryEditorStack,
        tabsEl: secondaryEditorTabsRoot,
        editorElId: 'secondaryEditor',
        scenarioFrameElId: 'secondaryScenarioConfigFrame',
        resultViewElId: 'secondaryResultFileView',
        paths: [],
        activePath: '',
        editor: null,
    },
};
let resultFileEditorController = null;
const interactiveRunContexts = new Map();
const scenarioConfigEditorController = createScenarioConfigEditorController({
    workspaceFs,
    scenarioInterface,
    getEditorPanes: () => editorPanes,
    getActiveEditorPaneId: () => activeEditorPaneId,
    applyEditorLanguage,
    withWorkspaceObserversSuspended(callback) {
        suspendWorkspaceObservers = true;
        try {
            return callback();
        } finally {
            suspendWorkspaceObservers = false;
        }
    },
    renderExplorerPane,
    scheduleWorkspacePersistence,
    shouldUseOtherCustomEditor: (pane) =>
        Boolean(resultFileEditorController?.shouldUseCustomFileEditor(pane)),
    async runScenarioPath(path) {
        const scenarioPath = normalizePath(path);
        const pane = Object.values(editorPanes).find((candidate) => candidate.activePath === scenarioPath);
        const paneId = pane?.id || activeEditorPaneId;
        if (typeof window.runScenarioForPane === 'function') {
            return await window.runScenarioForPane(paneId);
        }
        return null;
    },
});
resultFileEditorController = createResultFileEditorController({
    workspaceFs,
    getEditorPanes: () => editorPanes,
    renderExplorerPane,
    scheduleWorkspacePersistence,
    shouldUseOtherCustomEditor: (pane) =>
        Boolean(scenarioConfigEditorController.shouldUseScenarioEditor(pane)),
    resolveInteractiveRun: (path) => interactiveRunContexts.get(path) || null,
    loadInteractiveRuntime: loadPlaygroundInteractiveRuntime,
    loadWasmModule: loadMainThreadWasmModule,
    reportInteractiveRunError(run, runPath, error) {
        const message = String(error?.message || error || 'Input simulation failed');
        updateCompileErrors([{
            model: trimMaybeString(run?.modelName) || 'input simulation',
            message,
            path: trimMaybeString(run?.scenarioPath) || trimMaybeString(runPath),
        }]);
        window.switchBottomTab?.('errors');
        setCompileStatusBadge('Error', 'error');
    },
});
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
let sidebarContextActions = [];
const EXPLORER_ROOT_SELECTION = '.';
const DEFAULT_CODEGEN_TARGET_ID = 'sympy';
const PLAYGROUND_THEME_STORAGE_KEY = 'rumoca-playground-theme';
const PLAYGROUND_THEME_IDS = new Set(['system', 'light', 'rust', 'coal', 'navy', 'ayu']);
const systemThemeMedia = typeof window.matchMedia === 'function'
    ? window.matchMedia('(prefers-color-scheme: dark)')
    : null;
let builtInCodegenTemplates = [];
let builtInCodegenTemplatesLoaded = false;
let applyMonacoTheme = null;
let playgroundThemeId = normalizeThemeId(readStoredThemeId());

function trimMaybeString(value) {
    return typeof value === 'string' ? value.trim() : '';
}

function normalizeThemeId(value) {
    const themeId = trimMaybeString(value).toLowerCase();
    return PLAYGROUND_THEME_IDS.has(themeId) ? themeId : 'system';
}

function resolveThemeId(themeId) {
    const normalizedThemeId = normalizeThemeId(themeId);
    if (normalizedThemeId !== 'system') {
        return normalizedThemeId;
    }
    return systemThemeMedia?.matches ? 'navy' : 'light';
}

function readStoredThemeId() {
    try {
        return window.localStorage?.getItem(PLAYGROUND_THEME_STORAGE_KEY);
    } catch {
        return '';
    }
}

function writeStoredThemeId(themeId) {
    try {
        window.localStorage?.setItem(PLAYGROUND_THEME_STORAGE_KEY, themeId);
    } catch {
        // Ignore storage failures; workspace state still carries the selection.
    }
}

function applyPlaygroundTheme(themeId, { persist = true } = {}) {
    const nextThemeId = normalizeThemeId(themeId);
    const effectiveThemeId = resolveThemeId(nextThemeId);
    playgroundThemeId = nextThemeId;
    document.documentElement.dataset.theme = effectiveThemeId;
    document.documentElement.dataset.themePreference = nextThemeId;
    document.body.dataset.theme = effectiveThemeId;
    document.body.dataset.themePreference = nextThemeId;
    for (const select of themeSelects) {
        select.value = nextThemeId;
    }
    if (typeof applyMonacoTheme === 'function') {
        applyMonacoTheme(effectiveThemeId);
    }
    scenarioConfigEditorController?.syncAll?.();
    if (persist) {
        writeStoredThemeId(nextThemeId);
        scheduleWorkspacePersistence();
    }
}
applyPlaygroundTheme(playgroundThemeId, { persist: false });

function handleSystemThemeChange() {
    if (playgroundThemeId === 'system') {
        applyPlaygroundTheme('system', { persist: false });
    }
}

if (systemThemeMedia?.addEventListener) {
    systemThemeMedia.addEventListener('change', handleSystemThemeChange);
} else if (systemThemeMedia?.addListener) {
    systemThemeMedia.addListener(handleSystemThemeChange);
}

function defaultCodegenSettings() {
    return {
        mode: 'target',
        builtinTargetId: DEFAULT_CODEGEN_TARGET_ID,
        customTargetPath: '',
        outputDir: '',
    };
}

function normalizeCodegenSettings(value) {
    const next = value && typeof value === 'object' ? value : {};
    const mode = next.mode === 'custom-target'
            ? 'custom-target'
            : 'target';
    return {
        mode,
        builtinTargetId: trimMaybeString(next.builtinTargetId) || DEFAULT_CODEGEN_TARGET_ID,
        customTargetPath: trimMaybeString(next.customTargetPath),
        outputDir: trimMaybeString(next.outputDir),
    };
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
    const raw = await sendWorkspaceCommand('rumoca.workspace.getBuiltinTargets', {});
    const templates = Array.isArray(raw) ? raw : [];
    builtInCodegenTemplates = templates
        .map((entry) => ({
            id: trimMaybeString(entry?.id),
            label: trimMaybeString(entry?.label),
            manifest: typeof entry?.manifest === 'string' ? entry.manifest : '',
        }))
        .filter((entry) => entry.id && entry.label);
    builtInCodegenTemplatesLoaded = true;
    return builtInCodegenTemplates;
}

function codegenSettingsFromScenarioConfig(config) {
    const target = trimMaybeString(config?.target);
    if (!target) {
        return {
            ...defaultCodegenSettings(),
            outputDir: trimMaybeString(config?.outputDir),
        };
    }
    const builtin = findBuiltInCodegenTemplate(target);
    if (builtin?.id === target) {
        return {
            mode: 'target',
            builtinTargetId: target,
            customTargetPath: '',
            outputDir: trimMaybeString(config?.outputDir),
        };
    }
    return {
        mode: 'custom-target',
        builtinTargetId: DEFAULT_CODEGEN_TARGET_ID,
        customTargetPath: target,
        outputDir: trimMaybeString(config?.outputDir),
    };
}

function codegenConfigFromSettings(settings) {
    const normalized = normalizeCodegenSettings(settings);
    const target = normalized.mode === 'custom-target'
        ? normalized.customTargetPath
        : normalized.builtinTargetId;
    return {
        target: trimMaybeString(target),
        outputDir: normalized.outputDir,
    };
}

async function codegenSettingsForModel(modelName = currentCodegenModel()) {
    await ensureBuiltInCodegenTemplatesLoaded();
    const model = trimMaybeString(modelName);
    if (!model) {
        return defaultCodegenSettings();
    }
    const config = await scenarioInterface.execute('rumoca.scenario.getCodegenConfig', { model });
    return codegenSettingsFromScenarioConfig(config);
}

async function saveCodegenSettingsForModel(modelName, settings) {
    const model = trimMaybeString(modelName);
    if (!model) {
        return { ok: true };
    }
    const config = codegenConfigFromSettings(settings);
    const response = await scenarioInterface.execute('rumoca.scenario.setCodegenConfig', {
        model,
        config,
    });
    return response;
}

async function resetCodegenSettingsForModel(modelName) {
    return await saveCodegenSettingsForModel(modelName, defaultCodegenSettings());
}

function currentCodegenModel() {
    return trimMaybeString(window.selectedModel || currentSimulationModel());
}

async function resolveCodegenTemplateSelection(modelName = currentCodegenModel()) {
    const settings = await codegenSettingsForModel(modelName);
    if (settings.mode === 'custom-target') {
        const targetPath = trimMaybeString(settings.customTargetPath);
        if (!targetPath) {
            throw new Error('Choose a custom target directory in Rumoca settings.');
        }
        return {
            kind: 'target',
            target: targetPath,
            label: targetPath,
            language: 'plaintext',
            outputRoot: settings.outputDir,
        };
    }
    const selectedBuiltin = findBuiltInCodegenTemplate(settings.builtinTargetId);
    if (!selectedBuiltin) {
        throw new Error('No built-in codegen targets are available.');
    }
    return {
        kind: 'target',
        target: selectedBuiltin.id,
        label: selectedBuiltin.label,
        language: 'plaintext',
        outputRoot: settings.outputDir,
    };
}

function collectCustomCodegenTarget(targetPath) {
    const root = trimMaybeString(targetPath).replace(/^\/+|\/+$/g, '');
    if (!root) {
        throw new Error('Choose a custom target directory in Rumoca settings.');
    }
    const manifestPath = `${root}/target.toml`;
    const manifest = workspaceFs.getFileContent(manifestPath);
    if (typeof manifest !== 'string') {
        throw new Error(`Target manifest not found: ${manifestPath}`);
    }
    const prefix = `${root}/`;
    const templates = {};
    for (const file of workspaceFs.listFiles()) {
        if (!file.path.startsWith(prefix) || file.path === manifestPath) {
            continue;
        }
        templates[file.path.slice(prefix.length)] = file.content;
    }
    return { manifest, templates };
}

// GALEC codegen targets (all ir = "dae") are served by the SEPARATE, lazily
// loaded GALEC addon — never the core render_target/DAE-JSON path, which drops
// the flat model the projection needs. The worker loads the addon on demand.
const GALEC_CODEGEN_TARGETS = new Set(['galec', 'galec-production', 'embedded-c-galec']);

async function renderGalecCodegenSelection(modelName, source, target) {
    const rendered = await sendWorkspaceCommand('rumoca.workspace.renderGalec', {
        source,
        modelName,
        target,
    });
    if (!rendered || !Array.isArray(rendered.files)) {
        throw new Error('GALEC codegen renderer did not return files.');
    }
    return rendered.files;
}

async function renderCodegenSelection(modelName, daeJson, templateSelection) {
    let manifest = '';
    let templates = {};
    if (templateSelection.kind === 'custom-target') {
        ({ manifest, templates } = collectCustomCodegenTarget(templateSelection.target));
    }
    const rendered = await sendWorkspaceCommand('rumoca.workspace.renderTarget', {
        daeJson,
        modelName,
        target: templateSelection.target,
        manifest,
        templates: JSON.stringify(templates),
    });
    if (!rendered || !Array.isArray(rendered.files)) {
        throw new Error('Target renderer did not return files.');
    }
    return rendered.files;
}

function sanitizeGeneratedPathSegment(value) {
    return trimMaybeString(value)
        .replace(/[^A-Za-z0-9_.-]+/g, '_')
        .replace(/^_+|_+$/g, '')
        || 'output';
}

function defaultCodegenOutputRoot(modelName, templateSelection) {
    const modelLeaf = trimMaybeString(modelName).split('.').filter(Boolean).pop() || 'model';
    const targetLeaf = sanitizeGeneratedPathSegment(templateSelection.target || templateSelection.label || 'target');
    return `${sanitizeGeneratedPathSegment(modelLeaf)}_${targetLeaf}_out`;
}

function resolveGeneratedTargetPath(outputRoot, targetPath) {
    const root = trimMaybeString(outputRoot).replace(/^\/+|\/+$/g, '');
    const relative = trimMaybeString(targetPath).replace(/\\/g, '/').replace(/^\/+/, '');
    const parts = relative.split('/').filter(Boolean);
    if (!root || parts.length === 0 || parts.some((part) => part === '..')) {
        throw new Error(`Invalid target output path: ${targetPath}`);
    }
    return `${root}/${parts.join('/')}`;
}

function writeRenderedTargetFiles(files, outputRoot) {
    if (!Array.isArray(files) || files.length === 0) {
        throw new Error('Target renderer returned no files.');
    }
    const paths = [];
    for (const file of files) {
        const path = resolveGeneratedTargetPath(outputRoot, file?.path);
        workspaceFs.setFile(path, typeof file?.content === 'string' ? file.content : '');
        paths.push(path);
    }
    renderExplorerPane();
    scheduleWorkspacePersistence();
    return paths;
}

function parseBadgeNumber(text) {
    const match = String(text || '').match(/\d+/);
    return match ? Number(match[0]) : 0;
}

function isWorkspacePersistenceEnabled() {
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

function openWorkspacePersistenceDb() {
    return new Promise((resolve, reject) => {
        const request = window.indexedDB.open(WASM_WORKSPACE_PERSIST_DB, 1);
        request.onupgradeneeded = () => {
            const db = request.result;
            if (!db.objectStoreNames.contains(WASM_WORKSPACE_PERSIST_STORE)) {
                db.createObjectStore(WASM_WORKSPACE_PERSIST_STORE);
            }
        };
        request.onsuccess = () => resolve(request.result);
        request.onerror = () => reject(request.error || new Error('Failed to open IndexedDB'));
    });
}

async function loadPersistedWorkspaceEntries() {
    if (!isWorkspacePersistenceEnabled()) {
        return null;
    }
    const db = await openWorkspacePersistenceDb();
    try {
        const transaction = db.transaction(WASM_WORKSPACE_PERSIST_STORE, 'readonly');
        const store = transaction.objectStore(WASM_WORKSPACE_PERSIST_STORE);
        const record = await indexedDbRequest(store.get(WASM_WORKSPACE_PERSIST_KEY));
        await indexedDbTransactionDone(transaction);
        return Array.isArray(record?.entries) ? record.entries : null;
    } finally {
        db.close();
    }
}

async function savePersistedWorkspaceEntries(entries) {
    if (!isWorkspacePersistenceEnabled()) {
        return;
    }
    const db = await openWorkspacePersistenceDb();
    try {
        const transaction = db.transaction(WASM_WORKSPACE_PERSIST_STORE, 'readwrite');
        const store = transaction.objectStore(WASM_WORKSPACE_PERSIST_STORE);
        await indexedDbRequest(store.put({
            schemaVersion: 1,
            savedAt: Date.now(),
            entries,
        }, WASM_WORKSPACE_PERSIST_KEY));
        await indexedDbTransactionDone(transaction);
    } finally {
        db.close();
    }
}

function collectPersistedWorkspaceEntries() {
    workspaceFs.setEditorState(collectWorkspaceEditorState());
    if (editor?.getValue) {
        persistActivePaneDocument();
    }
    return workspaceFs.snapshotArchiveEntries({ includeCacheFiles: true });
}

async function persistWorkspaceToBrowserStorage() {
    if (!isWorkspacePersistenceEnabled()) {
        return;
    }
    try {
        await savePersistedWorkspaceEntries(collectPersistedWorkspaceEntries());
    } catch (error) {
        console.warn('Failed to persist WASM workspace state:', error);
    }
}

function enqueueWorkspacePersistence() {
    workspacePersistenceInFlight = workspacePersistenceInFlight
        .catch(() => {})
        .then(() => persistWorkspaceToBrowserStorage());
    return workspacePersistenceInFlight;
}

function scheduleWorkspacePersistence(delayMs = 250) {
    if (!isWorkspacePersistenceEnabled() || !workspacePersistenceReady) {
        return;
    }
    if (workspacePersistenceTimer) {
        clearTimeout(workspacePersistenceTimer);
    }
    workspacePersistenceTimer = setTimeout(() => {
        workspacePersistenceTimer = null;
        void enqueueWorkspacePersistence();
    }, delayMs);
}

function flushWorkspacePersistence() {
    if (!isWorkspacePersistenceEnabled() || !workspacePersistenceReady) {
        return Promise.resolve();
    }
    if (workspacePersistenceTimer) {
        clearTimeout(workspacePersistenceTimer);
        workspacePersistenceTimer = null;
        void enqueueWorkspacePersistence();
    }
    return workspacePersistenceInFlight.catch(() => {});
}

function pathParts(path) {
    return String(path || '').split('/').filter(Boolean);
}

function baseName(path) {
    return pathParts(path).at(-1) || '';
}

function friendlyEditorTabLabel(path) {
    const nextPath = normalizePath(path);
    if (!isRumocaResultPath(nextPath)) {
        return baseName(nextPath) || nextPath;
    }
    const content = workspaceFs.getFileContent(nextPath);
    if (typeof content === 'string') {
        try {
            const model = trimMaybeString(JSON.parse(content)?.model);
            if (model) {
                return `${model.split('.').pop() || model} Results`;
            }
        } catch {
            // Fall through to the compact result filename.
        }
    }
    const filename = baseName(nextPath);
    return filename.length > 24 ? 'Simulation Results' : filename;
}

function parentDirectory(path) {
    const parts = pathParts(path);
    return parts.length > 1 ? parts.slice(0, -1).join('/') : '';
}

function resolveWorkspaceRelativePath(basePath, relativePath) {
    const relative = normalizePath(relativePath);
    if (!relative) {
        return '';
    }
    const parts = pathParts(parentDirectory(basePath));
    for (const part of pathParts(relative)) {
        if (part === '.') {
            continue;
        }
        if (part === '..') {
            parts.pop();
            continue;
        }
        parts.push(part);
    }
    return parts.join('/');
}

function scenarioResultDirectory(config) {
    const effective = config?.effective || {};
    const scenarioDir = parentDirectory(normalizePath(config?.scenarioPath || workspaceFs.getActiveDocumentPath()));
    const outputDir = normalizePath(effective.outputDir || DEFAULT_WEB_RESULTS_OUTPUT_DIR);
    return scenarioDir ? normalizePath(`${scenarioDir}/${outputDir}`) : outputDir;
}

async function persistSimulationResultFile(model, result, resultDirectory) {
    return await shared.persistHostedSimulationRunWithViews({
        model,
        payload: result?.payload,
        metrics: result?.metrics || null,
        loadConfiguredViews: async ({ model: targetModel }) =>
            (await scenarioInterface.execute(
                'rumoca.scenario.getVisualizationConfig',
                { model: targetModel },
            )).views,
        hydrateViews: ({ model: targetModel, views }) =>
            scenarioVisualizationStorage.hydrateViews({
                views,
                model: targetModel,
            }),
        defaultViews: shared.defaultVisualizationViews(),
        resultDirectory,
        pathExists: (candidatePath) => workspaceFs.getFileContent(candidatePath) !== null,
        readTextFile: (candidatePath) => workspaceFs.getFileContent(candidatePath),
        writeTextFile: (runPath, content) => {
            workspaceFs.setFile(runPath, `${content}\n`);
        },
    });
}

function isPreviewableAssetPath(path) {
    const nextPath = trimMaybeString(path).toLowerCase();
    return ['.glb', '.png', '.jpg', '.jpeg', '.gif', '.webp', '.svg']
        .some((extension) => nextPath.endsWith(extension));
}

function isExplorerTextEntry(entry) {
    return (
        entry?.sourceKind === 'workspace'
        || entry?.sourceKind === 'packageArchive'
        || entry?.sourceKind === 'generated'
    ) && entry?.isText === true;
}

function isExplorerOpenableEntry(entry) {
    return isExplorerTextEntry(entry) || (
        (
            entry?.sourceKind === 'workspace'
            || entry?.sourceKind === 'packageArchive'
            || entry?.sourceKind === 'generated'
        ) && isPreviewableAssetPath(entry?.path)
    );
}

function normalizeOpenDocumentPaths(paths, fallbackPath = '') {
    const next = [];
    const seen = new Set();
    const pushPath = (candidate) => {
        const normalized = trimMaybeString(candidate);
        if (!normalized || seen.has(normalized)) {
            return;
        }
        const content = workspaceFs.getFileContent(normalized);
        const entry = workspaceFs.getFileEntry(normalized);
        const customEditor = resultFileEditorController?.isCustomFilePath(normalized, entry);
        if (typeof content !== 'string' && !customEditor) {
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

function setFileMenuOpen(isOpen) {
    if (!fileMenuButton || !fileMenuPanel) {
        return;
    }
    fileMenuButton.setAttribute('aria-expanded', String(Boolean(isOpen)));
    fileMenuPanel.classList.toggle('open', Boolean(isOpen));
}

function currentSidebarMode() {
    return 'explorer';
}

function updateMobileAppbarTitle() {
    if (!mobileAppbarTitle) {
        return;
    }
    const activePath = trimMaybeString(workspaceFs.getActiveDocumentPath());
    const title = activePath
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
    const title = collapsed ? 'Show Explorer' : 'Hide Explorer';
    if (sidebarBrandToggle) {
        sidebarBrandToggle.title = title;
        sidebarBrandToggle.setAttribute('aria-label', title);
        sidebarBrandToggle.setAttribute('aria-expanded', String(!collapsed));
    }
    if (mobileSidebarBtn) {
        const active = !collapsed;
        mobileSidebarBtn.classList.toggle('active', active);
        mobileSidebarBtn.setAttribute('aria-expanded', String(active));
        mobileSidebarBtn.title = title;
        mobileSidebarBtn.setAttribute('aria-label', title);
    }
    updateMobileAppbarTitle();
}

function setSidebarMode(mode, { persist = true } = {}) {
    if (workbenchSidebar) {
        workbenchSidebar.dataset.mode = 'explorer';
    }
    explorerSidebarPanel?.classList.remove('pane-hidden');
    syncSidebarModeButtons();
    if (persist) {
        scheduleWorkspacePersistence();
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
    scheduleWorkspacePersistence();
}

function toggleSidebarCollapsed() {
    setSidebarCollapsed(!workbenchSidepanel?.classList.contains('collapsed'));
    layoutAllEditors();
}

function setSidebarSectionCollapsed(sectionName, collapsed) {
    const normalized = sectionName === 'outline'
        ? 'outline'
        : sectionName === 'project'
            ? 'project'
            : 'explorer';
    const section = normalized === 'outline'
        ? outlineSection
        : normalized === 'project'
            ? projectSection
            : explorerSection;
    const toggle = normalized === 'outline'
        ? outlineSectionToggle
        : normalized === 'project'
            ? projectSectionToggle
            : explorerSectionToggle;
    const arrow = normalized === 'outline'
        ? outlineSectionArrow
        : normalized === 'project'
            ? projectSectionArrow
            : explorerSectionArrow;
    if (!section || !toggle || !arrow) {
        return;
    }
    const nextCollapsed = Boolean(collapsed);
    section.classList.toggle('collapsed', nextCollapsed);
    toggle.setAttribute('aria-expanded', String(!nextCollapsed));
    arrow.textContent = nextCollapsed ? '▸' : '▾';
    scheduleWorkspacePersistence();
}

function toggleSidebarSection(sectionName) {
    const section = sectionName === 'outline'
        ? outlineSection
        : sectionName === 'project'
            ? projectSection
            : explorerSection;
    if (!section) {
        return;
    }
    setSidebarSectionCollapsed(sectionName, !section.classList.contains('collapsed'));
}

function closeSidebarContextMenu() {
    sidebarContextActions = [];
    if (!sidebarContextMenu) {
        return;
    }
    sidebarContextMenu.classList.remove('open');
    sidebarContextMenu.style.left = '';
    sidebarContextMenu.style.top = '';
    sidebarContextMenu.innerHTML = '';
}

function openSidebarContextMenu(clientX, clientY, actions) {
    if (!sidebarContextMenu) {
        return;
    }
    const menuActions = (Array.isArray(actions) ? actions : [actions]).filter(Boolean);
    if (menuActions.length === 0) {
        return;
    }
    sidebarContextActions = menuActions;
    sidebarContextMenu.innerHTML = '';
    for (const action of sidebarContextActions) {
        const button = document.createElement('button');
        button.type = 'button';
        button.className = `sidebar-context-item${action.danger ? ' danger' : ''}`;
        button.textContent = action.label || 'Action';
        button.disabled = Boolean(action.disabled);
        button.title = action.title || '';
        button.addEventListener('click', async () => {
            closeSidebarContextMenu();
            if (!button.disabled && typeof action.run === 'function') {
                await action.run();
            }
        });
        sidebarContextMenu.appendChild(button);
    }
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
        const selectedEntry = workspaceFs.getFileEntry(selectedPath);
        if (selectedEntry?.sourceKind === 'workspace') {
            return parentDirectory(selectedPath);
        }
        const hasFolder = workspaceFs.listFolders().includes(selectedPath)
            || workspaceFs.listFileEntries().some((entry) => entry.path.startsWith(`${selectedPath}/`));
        if (hasFolder) {
            return selectedPath;
        }
    }
    const activePath = workspaceFs.getActiveDocumentPath();
    const activeEntry = workspaceFs.getFileEntry(activePath);
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
        || (hasOpenEditorPane() ? workspaceFs.getActiveDocumentPath() : '');
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

function pruneOpenDocuments(predicate, fallbackPath = workspaceFs.getActiveDocumentPath()) {
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

function hasOpenEditorPane() {
    return openDocumentPaths.length > 0;
}

function isModelicaDocumentPath(path) {
    return trimMaybeString(path).toLowerCase().endsWith('.mo');
}

function activeEditorDocumentPath() {
    return trimMaybeString(getEditorPane(activeEditorPaneId)?.activePath);
}

function updateWorkbenchPaneVisibility() {
    const nextLeftArea = document.querySelector('.left-area');
    const nextWorkbenchTop = document.querySelector('.workbench-top') || document.querySelector('.main-container');
    if (!nextLeftArea || !nextWorkbenchTop) {
        return;
    }
    const showEditor = editorPaneVisible;
    nextLeftArea.classList.toggle('pane-hidden', !showEditor);
    nextWorkbenchTop.dataset.layout = showEditor ? 'editor' : 'empty';
}

updateWorkbenchPaneVisibility();
refreshEditorActionButtons();

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
    scenarioConfigEditorController.syncPane(pane.id);
    resultFileEditorController?.syncPane(pane.id);
}

function persistActivePaneDocument() {
    const pane = getEditorPane(activeEditorPaneId);
    const activePath = trimMaybeString(pane?.activePath);
    if (!activePath || typeof pane?.editor?.getValue !== 'function') {
        return;
    }
    if (
        scenarioConfigEditorController.shouldUseScenarioEditor(pane)
        || resultFileEditorController?.shouldUseCustomFileEditor(pane)
    ) {
        if (!isInteractiveRunPath(activePath)) {
            workspaceFs.activateDocument(activePath);
        }
        return;
    }
    if (typeof pane.editor.saveViewState === 'function') {
        editorViewStates.set(activePath, pane.editor.saveViewState());
    }
    workspaceFs.setFile(activePath, pane.editor.getValue());
    workspaceFs.activateDocument(activePath);
}

function setActiveEditorPane(
    paneId,
    { focusEditor = false, refreshNavigation = true, outlineDelayMs = 0, persistCurrent = true } = {},
) {
    const nextPane = getEditorPane(paneId);
    if (!nextPane) {
        return;
    }
    if (persistCurrent) {
        persistActivePaneDocument();
    }
    activeEditorPaneId = nextPane.id;
    if (nextPane.editor) {
        editor = nextPane.editor;
        window.editor = nextPane.editor;
    }
    if (trimMaybeString(nextPane.activePath) && !isInteractiveRunPath(nextPane.activePath)) {
        workspaceFs.activateDocument(nextPane.activePath);
    }
    setEditorPaneSplit(editorPaneSplit);
    if (refreshNavigation) {
        refreshActiveDocumentNavigation({ outlineDelayMs });
    }
    scenarioConfigEditorController.syncPane(nextPane.id);
    resultFileEditorController?.syncPane(nextPane.id);
    if (
        focusEditor
        && !scenarioConfigEditorController.shouldUseScenarioEditor(nextPane)
        && !resultFileEditorController?.shouldUseCustomFileEditor(nextPane)
        && typeof nextPane.editor?.focus === 'function'
    ) {
        nextPane.editor.focus();
    }
}

function canCloseWorkspaceDocument(path = workspaceFs.getActiveDocumentPath(), paneId = activeEditorPaneId) {
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

async function closeWorkspaceDocument(path = workspaceFs.getActiveDocumentPath(), paneId = activeEditorPaneId) {
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
    scenarioConfigEditorController.closePath(nextPath);
    pane.paths = pane.paths.filter((candidate) => candidate !== nextPath);
    pane.activePath = fallbackPath;
    rebuildOpenDocumentPaths();
    if (fallbackPath) {
        await openWorkspaceDocument(fallbackPath, {
            paneId: pane.id,
            focusEditor: pane.id === activeEditorPaneId,
            forceReload: true,
        });
    } else {
        refreshEditorPaneEmptyState(pane.id);
        collapseSingleEmptyPaneIfPossible();
        renderEditorTabs();
        refreshWorkbenchNavigation();
        scheduleWorkspacePersistence();
    }
}

window.closeActiveWorkspaceDocument = () => closeWorkspaceDocument(workspaceFs.getActiveDocumentPath(), activeEditorPaneId);
window.closeActivePaneDocument = (paneId) => closeWorkspaceDocument(getEditorPane(paneId)?.activePath || '', paneId);
window.getActiveEditorPaneId = () => activeEditorPaneId;

async function closeOtherWorkspaceDocuments(path, paneId = activeEditorPaneId) {
    const pane = getEditorPane(paneId);
    const nextPath = trimMaybeString(path);
    if (!pane || !nextPath || !pane.paths.includes(nextPath)) {
        return;
    }
    persistActivePaneDocument();
    for (const candidate of pane.paths) {
        if (candidate !== nextPath) {
            scenarioConfigEditorController.closePath(candidate);
            editorViewStates.delete(candidate);
        }
    }
    pane.paths = [nextPath];
    pane.activePath = nextPath;
    rebuildOpenDocumentPaths();
    await openWorkspaceDocument(nextPath, {
        paneId: pane.id,
        focusEditor: pane.id === activeEditorPaneId,
        forceReload: true,
    });
}

async function closeAllWorkspaceDocuments(paneId = activeEditorPaneId) {
    const pane = getEditorPane(paneId);
    if (!pane) {
        return;
    }
    persistActivePaneDocument();
    for (const candidate of pane.paths) {
        scenarioConfigEditorController.closePath(candidate);
        editorViewStates.delete(candidate);
    }
    pane.paths = [];
    pane.activePath = '';
    rebuildOpenDocumentPaths();
    refreshEditorPaneEmptyState(pane.id);
    collapseSingleEmptyPaneIfPossible();
    renderEditorTabs();
    refreshWorkbenchNavigation();
    scheduleWorkspacePersistence();
}

function downloadWorkspacePath(path) {
    persistActivePaneDocument();
    if (typeof window.downloadWorkspaceFile === 'function') {
        window.downloadWorkspaceFile(path);
    }
}

function openEditorTabContextMenu(clientX, clientY, path, paneId = activeEditorPaneId) {
    const pane = getEditorPane(paneId);
    const nextPath = trimMaybeString(path);
    if (!pane || !nextPath || !pane.paths.includes(nextPath)) {
        return;
    }
    openSidebarContextMenu(clientX, clientY, [
        {
            label: 'Close',
            run: async () => closeWorkspaceDocument(nextPath, pane.id),
        },
        {
            label: 'Close Others',
            disabled: pane.paths.length <= 1,
            run: async () => closeOtherWorkspaceDocuments(nextPath, pane.id),
        },
        {
            label: 'Close All',
            run: async () => closeAllWorkspaceDocuments(pane.id),
        },
        {
            label: 'Download',
            run: () => downloadWorkspacePath(nextPath),
        },
    ]);
}

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
    iconKind = '',
    hasChildren = false,
    collapsed = false,
    onContextMenu = null,
    onToggle = null,
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
    if (hasChildren && typeof onToggle === 'function') {
        expander.addEventListener('click', (event) => {
            event.preventDefault();
            event.stopPropagation();
            onToggle();
        });
    }
    row.appendChild(expander);

    const icon = document.createElement('span');
    icon.className = ['icon', iconKind].filter(Boolean).join(' ');
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

    container.appendChild(shell);
}

function createSidebarNode(id, label, {
    kind = 'dir',
    path = '',
    meta = '',
    sourceKind = 'workspace',
    sourceBadge = '',
    iconKind = '',
    run = null,
    active = false,
    contextAction = null,
} = {}) {
    return {
        id,
        label,
        kind,
        path,
        meta,
        sourceKind,
        sourceBadge,
        iconKind,
        run,
        active,
        contextAction,
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
        if (options.iconKind) {
            existing.iconKind = options.iconKind;
        }
        if (typeof options.run === 'function' && typeof existing.run !== 'function') {
            existing.run = options.run;
        }
        if (options.contextAction) {
            existing.contextAction = options.contextAction;
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
    branchClick = 'toggle',
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
            onClick: () => {
                if (node.kind === 'dir' && branchClick === 'toggle') {
                    if (node.path) {
                        setSelectedExplorerPath(node.path);
                    }
                    if (hasChildren) {
                        onToggle?.(node.id);
                    }
                    return;
                }
                if (node.path) {
                    setSelectedExplorerPath(node.path);
                }
                if (typeof node.run === 'function') {
                    node.run();
                } else if (hasChildren) {
                    onToggle?.(node.id);
                }
            },
            sourceKind: node.sourceKind || 'workspace',
            sourceBadge: node.sourceBadge || '',
            iconKind: node.iconKind || '',
            hasChildren,
            collapsed: isCollapsed,
            onToggle: hasChildren ? () => onToggle?.(node.id) : null,
            onContextMenu: node.contextAction
                ? (event) => {
                    openSidebarContextMenu(event.clientX, event.clientY, node.contextAction);
                }
                : null,
            hidden,
        });
        if (hasChildren) {
            renderSidebarTreeNodes(container, node.children, {
                depth: depth + 1,
                collapsedKeys,
                onToggle,
                branchClick,
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
    const removedPackageArchiveContent = workspaceFs.listFileEntries().some(
        (entry) => entry.sourceKind === 'packageArchive'
            && (entry.path === path || entry.path.startsWith(`${path}/`)),
    );
    const previousActivePath = workspaceFs.getActiveDocumentPath();
    const removed = workspaceFs.removeFolder(path);
    if (!removed) {
        return;
    }
    if (selectedExplorerPath === path || selectedExplorerPath.startsWith(`${path}/`)) {
        selectedExplorerPath = '';
    }
    if (removedPackageArchiveContent) {
        await packageArchiveController.restoreWorkspacePackageArchives();
    }
    pruneOpenDocuments(
        (candidate) => candidate === path || candidate.startsWith(`${path}/`),
        workspaceFs.getActiveDocumentPath(),
    );
    const nextActivePath = workspaceFs.getActiveDocumentPath();
    if (previousActivePath !== nextActivePath || !workspaceFs.getFileContent(previousActivePath)) {
        await openWorkspaceDocument(nextActivePath, { focusEditor: false, forceReload: true });
        refreshWorkbenchNavigation();
        scheduleWorkspacePersistence();
        return;
    }
    refreshWorkbenchNavigation();
    scheduleWorkspacePersistence();
}

async function deleteExplorerFile(path) {
    closeSidebarContextMenu();
    if (!window.confirm(`Delete file "${path}"?`)) {
        return;
    }
    const removedPackageArchiveContent = workspaceFs.listFileEntries().some(
        (entry) => entry.sourceKind === 'packageArchive' && entry.path === path,
    );
    const previousActivePath = workspaceFs.getActiveDocumentPath();
    const removed = workspaceFs.removeFile(path);
    if (!removed) {
        return;
    }
    if (selectedExplorerPath === path) {
        selectedExplorerPath = '';
    }
    if (removedPackageArchiveContent) {
        await packageArchiveController.restoreWorkspacePackageArchives();
    }
    pruneOpenDocuments((candidate) => candidate === path, workspaceFs.getActiveDocumentPath());
    const nextActivePath = workspaceFs.getActiveDocumentPath();
    if (previousActivePath !== nextActivePath || !workspaceFs.getFileContent(previousActivePath)) {
        await openWorkspaceDocument(nextActivePath, { focusEditor: false, forceReload: true });
        refreshWorkbenchNavigation();
        scheduleWorkspacePersistence();
        return;
    }
    refreshWorkbenchNavigation();
    scheduleWorkspacePersistence();
}

function createExplorerFolderAction(path) {
    return [
        {
            label: 'Download Folder',
            disabled: true,
            title: 'Download is available for files. Use Save ZIP for folders.',
        },
        {
            label: 'Delete Folder',
            danger: true,
            run: async () => {
                await deleteExplorerFolder(path);
            },
        },
    ];
}

function createExplorerFileAction(path) {
    return [
        {
            label: 'Download',
            run: () => downloadWorkspacePath(path),
        },
        {
            label: 'Delete File',
            danger: true,
            run: async () => {
                await deleteExplorerFile(path);
            },
        },
    ];
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
        const existingContent = workspaceFs.getFileContent(requestedPath);
        if (typeof existingContent === 'string') {
            expandWorkspaceExplorerPath(requestedPath);
            revealExplorerPath(requestedPath);
            await openWorkspaceDocument(requestedPath, { forceReload: true });
            return;
        }
        const createdPath = workspaceFs.setFile(
            requestedPath,
            defaultWorkspaceFileContent(requestedPath),
        );
        expandWorkspaceExplorerPath(createdPath);
        revealExplorerPath(createdPath);
        await openWorkspaceDocument(createdPath, { forceReload: true });
        scheduleWorkspacePersistence(0);
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
        if (workspaceFs.listFolders().includes(requestedPath)) {
            expandWorkspaceExplorerPath(requestedPath, true);
            revealExplorerPath(requestedPath);
            return;
        }
        const createdPath = workspaceFs.setFolder(requestedPath);
        expandWorkspaceExplorerPath(createdPath, true);
        revealExplorerPath(createdPath);
        scheduleWorkspacePersistence(0);
    } catch (error) {
        window.alert(error instanceof Error ? error.message : String(error));
    }
}

function collapseImportedExplorerBranches() {
    const entries = workspaceFs.listFileEntries().filter(
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
            });
        }
        return folderStats.get(normalized);
    };
    const addFolderStats = (path, sourceKind) => {
        const stats = ensureFolderStats(path);
        if (!stats) {
            return;
        }
        stats.totalFiles += 1;
        if (sourceKind === 'packageArchive') {
            stats.packageArchiveFiles += 1;
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
            addFolderStats(prefix, entry.sourceKind);
        }
    }
    const folderSourceKind = (path) => {
        const stats = folderStats.get(path);
        return stats && stats.totalFiles > 0 && stats.packageArchiveFiles === stats.totalFiles
            ? 'packageArchive'
            : 'workspace';
    };
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
                void openWorkspaceDocument(entry.path, { paneId: 'primary' });
            },
            contextAction: createExplorerFileAction(entry.path),
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
        name.textContent = friendlyEditorTabLabel(path);
        tabButton.appendChild(name);

        tabButton.addEventListener('click', () => {
            void openWorkspaceDocument(path, { paneId: pane.id });
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
            void closeWorkspaceDocument(path, pane.id);
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
        tab.addEventListener('contextmenu', (event) => {
            event.preventDefault();
            event.stopPropagation();
            openEditorTabContextMenu(event.clientX, event.clientY, path, pane.id);
        });
        pane.tabsEl.appendChild(tab);
    }
    if (!pane.tabsEl.dataset.dragBound) {
        pane.tabsEl.addEventListener('contextmenu', (event) => {
            const tab = event.target?.closest?.('.editor-tab');
            if (!tab || !pane.tabsEl.contains(tab)) {
                return;
            }
            event.preventDefault();
            event.stopPropagation();
            openEditorTabContextMenu(event.clientX, event.clientY, tab.dataset.path, tab.dataset.paneId);
        });
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
        const closable = canCloseWorkspaceDocument(pane?.activePath || '', paneId);
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
    const fileEntries = workspaceFs.listFileEntries().filter(isExplorerOpenableEntry);
    const folderEntries = workspaceFs.listFolders();
    if (fileEntries.length === 0 && folderEntries.length === 0) {
        appendSidebarEmpty(explorerTree, 'Workspace files will appear here.');
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
            scheduleWorkspacePersistence();
        },
    });
    updateExplorerActiveSelection(activePath);
}

explorerTree?.addEventListener('click', (event) => {
    const target = event.target;
    if (!(target instanceof Element)) {
        return;
    }
    if (target.closest('.sidebar-row')) {
        return;
    }
    setSelectedExplorerPath(EXPLORER_ROOT_SELECTION);
    scheduleWorkspacePersistence();
});

function refreshActiveDocumentNavigation({ includeOutline = true, outlineDelayMs = 0 } = {}) {
    renderEditorTabs();
    scenarioConfigEditorController.syncAll();
    resultFileEditorController?.syncAll();
    refreshEditorActionButtons();
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
    const activePath = hasOpenEditorPane() ? workspaceFs.getActiveDocumentPath() : '';
    if (!activePath || !editor || !isModelicaDocumentPath(activePath)) {
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
        branchClick: 'run',
        onToggle(nodeId) {
            if (outlineCollapsedNodes.has(nodeId)) {
                outlineCollapsedNodes.delete(nodeId);
            } else {
                outlineCollapsedNodes.add(nodeId);
            }
            updateRenderedTreeVisibility(outlineTree, outlineCollapsedNodes);
            updateTreeToggleButton(outlineTreeToggleBtn, outlineBranchKeys, outlineCollapsedNodes, 'Outline');
            scheduleWorkspacePersistence();
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
    scenarioConfigEditorController.syncAll();
    resultFileEditorController?.syncAll();
    refreshEditorActionButtons();
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
    if (nextPath.endsWith('.md') || nextPath.endsWith('.markdown')) return 'markdown';
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
        return nextId === 'toml' ? 'plaintext' : nextId;
    }
    const supported = getLanguages.call(monacoApi.languages)
        .some((entry) => trimMaybeString(entry?.id) === nextId);
    if (supported) {
        return nextId;
    }
    if (nextId === 'toml') {
        return 'plaintext';
    }
    return 'plaintext';
}

const leftArea = document.querySelector('.left-area');
const mainContainer = document.querySelector('.main-container');
const workbenchTop = document.querySelector('.workbench-top') || document.querySelector('.main-container');
const workbenchMain = document.querySelector('.workbench-main') || document.querySelector('.main-container');
let isResizingSidebar = false;
let pendingSidebarResizeWidth = null;
let sidebarResizeFrame = 0;
const SIDEBAR_RESIZE_MIN_WIDTH = 180;
const SIDEBAR_RESIZE_MAX_WIDTH = 480;

function scheduleSidebarWidth(width) {
    pendingSidebarResizeWidth = width;
    if (sidebarResizeFrame) {
        return;
    }
    sidebarResizeFrame = requestAnimationFrame(() => {
        sidebarResizeFrame = 0;
        if (workbenchSidebar && Number.isFinite(pendingSidebarResizeWidth)) {
            workbenchSidebar.style.width = `${pendingSidebarResizeWidth}px`;
        }
        pendingSidebarResizeWidth = null;
    });
}

function flushSidebarWidth() {
    if (sidebarResizeFrame) {
        cancelAnimationFrame(sidebarResizeFrame);
        sidebarResizeFrame = 0;
    }
    if (workbenchSidebar && Number.isFinite(pendingSidebarResizeWidth)) {
        workbenchSidebar.style.width = `${pendingSidebarResizeWidth}px`;
    }
    pendingSidebarResizeWidth = null;
}

function isNarrowLayout() {
    return window.innerWidth <= 980;
}

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
syncResponsiveLayoutState();
setSidebarMode('explorer', { persist: false });
setSidebarCollapsed(isNarrowLayout());
setSidebarSectionCollapsed('explorer', false);
setSidebarSectionCollapsed('outline', false);

for (const select of themeSelects) {
    select.addEventListener('change', (event) => {
        applyPlaygroundTheme(event.target?.value);
    });
}
sidebarBrandToggle?.addEventListener('click', () => {
    toggleSidebarCollapsed();
});
mobileSidebarBtn?.addEventListener('click', () => {
    toggleSidebarCollapsed();
});
sidebarBackdrop?.addEventListener('click', () => {
    setSidebarCollapsed(true);
});
explorerNewFileBtn?.addEventListener('click', () => {
    void createWorkspaceFileFromExplorer();
});
explorerNewFolderBtn?.addEventListener('click', () => {
    createWorkspaceFolderFromExplorer();
});
projectSectionToggle?.addEventListener('click', () => {
    toggleSidebarSection('project');
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
    scheduleWorkspacePersistence();
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
    scheduleWorkspacePersistence();
});

resizeHandleSidebar?.addEventListener('mousedown', (event) => {
    if (isNarrowLayout() || workbenchSidepanel?.classList.contains('collapsed')) return;
    isResizingSidebar = true;
    document.body.classList.add('sidebar-resizing');
    document.body.style.cursor = 'ew-resize';
    document.body.style.userSelect = 'none';
    event.preventDefault();
});

resizeHandleSidebar?.addEventListener('dblclick', (event) => {
    event.preventDefault();
    if (!workbenchSidebar) {
        return;
    }
    workbenchSidebar.style.width = '';
    layoutAllEditors();
    scheduleWorkspacePersistence();
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

function yieldToBrowserPaint() {
    return new Promise((resolve) => requestAnimationFrame(() => resolve()));
}

function compileProgressLabel(progress) {
    const command = trimMaybeString(progress?.command);
    const scope = trimMaybeString(progress?.scope);
    if (progress?.kind === 'parse') {
        const current = Number(progress.current) || 0;
        const total = Number(progress.total) || 0;
        const percent = Number(progress.percent) || 0;
        const sourceLabel = scope === 'wasm::workspace'
            ? 'workspace files'
            : scope === 'wasm::bundled-source-roots'
                ? 'package files'
                : 'source files';
        return `Parsing ${sourceLabel} ${current}/${total} (${percent}%)`;
    }
    if (progress?.kind !== 'request' || progress?.phase !== 'start') {
        return '';
    }
    switch (command) {
        case 'rumoca.language.diagnostics':
            return 'Checking diagnostics...';
        case 'rumoca.scenario.getSimulationModels':
            return 'Discovering models...';
        case 'rumoca.workspace.compileWithWorkspaceSources':
            return 'Compiling with workspace files...';
        case 'rumoca.workspace.compileWithSourceRoots':
            return 'Compiling with source roots...';
        case 'rumoca.workspace.loadSourceRoots':
            return 'Loading package files...';
        case 'rumoca.workspace.effectiveSourceRoots':
            return 'Reading workspace settings...';
        case 'rumoca.workspace.mergeParsedSourceRootsBinary':
            return 'Restoring parsed package cache...';
        case 'rumoca.workspace.exportParsedSourceRootsBinary':
            return 'Saving parsed package cache...';
        default:
            return '';
    }
}

function handleWorkerProgress(progress) {
    if (progress?.kind === 'parse') {
        packageArchiveController.handleWorkerProgress(progress);
    }
    if (
        progress?.kind === 'request'
        && progress?.phase === 'finish'
        && [
            'rumoca.workspace.loadSourceRoots',
            'rumoca.workspace.mergeParsedSourceRootsBinary',
            'rumoca.workspace.exportParsedSourceRootsBinary',
            'rumoca.workspace.effectiveSourceRoots',
        ].includes(trimMaybeString(progress?.command))
    ) {
        setCompileStatusBadge('Idle', 'loading');
        return;
    }
    const label = compileProgressLabel(progress);
    if (label) {
        setCompileStatusBadge(label, 'loading');
    }
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
    if (!isModelicaDocumentPath(activeEditorDocumentPath())) {
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

async function openWorkspaceDocument(path, { paneId = activeEditorPaneId, focusEditor = true, forceReload = false } = {}) {
    const nextPath = trimMaybeString(path);
    const targetPane = getEditorPane(paneId);
    if (!nextPath || !targetPane) {
        return;
    }
    const nextContent = workspaceFs.getFileContent(nextPath);
    const nextEntry = workspaceFs.getFileEntry(nextPath);
    const useCustomFileEditor = resultFileEditorController?.isCustomFilePath(nextPath, nextEntry);
    const useVirtualFileEditor = isInteractiveRunPath(nextPath);
    const targetEditor = useCustomFileEditor
        ? targetPane.editor
        : (normalizeEditorPaneId(paneId) === 'secondary'
            ? ensureSecondarySourceEditor()
            : paneEditor('primary'));
    if (!useCustomFileEditor && !targetEditor) {
        return;
    }
    if (typeof nextContent !== 'string' && !useCustomFileEditor) {
        return;
    }
    editorPaneVisible = true;
    updateWorkbenchPaneVisibility();

    removePathFromOtherEditorPane(nextPath, targetPane.id);
    const currentPath = trimMaybeString(targetPane.activePath);
    if (currentPath === nextPath && !forceReload) {
        setPanePaths(targetPane.id, [...targetPane.paths, nextPath], nextPath);
        setActiveEditorPane(targetPane.id, { focusEditor, refreshNavigation: false });
        if (scenarioConfigEditorController.isScenarioPath(nextPath)) {
            scenarioConfigEditorController.showGui(nextPath);
        }
        scenarioConfigEditorController.syncAll?.();
        resultFileEditorController?.syncAll?.();
        renderEditorTabs();
        refreshActiveDocumentNavigation({ outlineDelayMs: 80 });
        scheduleWorkspacePersistence(1200);
        return;
    }

    if (currentPath && typeof targetEditor?.saveViewState === 'function') {
        editorViewStates.set(currentPath, targetEditor.saveViewState());
    }
    if (
        currentPath
        && !scenarioConfigEditorController.shouldUseScenarioEditor(targetPane)
        && !resultFileEditorController?.shouldUseCustomFileEditor(targetPane)
        && typeof targetEditor?.getValue === 'function'
    ) {
        workspaceFs.setFile(currentPath, targetEditor.getValue());
    }

    targetPane.paths = normalizeOpenDocumentPaths([...targetPane.paths, nextPath], currentPath || nextPath);
    rebuildOpenDocumentPaths();

    if (!useCustomFileEditor) {
        suspendWorkspaceObservers = true;
        try {
            targetEditor.setValue(typeof nextContent === 'string' ? nextContent : '');
            applyEditorLanguage(targetPane.id, nextPath);
        } finally {
            suspendWorkspaceObservers = false;
        }
    }

    if (!useCustomFileEditor && typeof targetEditor?.restoreViewState === 'function') {
        const viewState = editorViewStates.get(nextPath);
        if (viewState) {
            targetEditor.restoreViewState(viewState);
        }
    }

    targetPane.activePath = nextPath;
    if (useVirtualFileEditor) {
        // Virtual run tabs are editor surfaces, not workspace files.
    } else {
        workspaceFs.setActiveDocument(nextPath, nextContent);
        selectedExplorerPath = nextPath;
    }
    renderEditorTabs();
    if (scenarioConfigEditorController.isScenarioPath(nextPath)) {
        scenarioConfigEditorController.showGui(nextPath);
    }
    setActiveEditorPane(targetPane.id, {
        focusEditor,
        refreshNavigation: false,
        persistCurrent: false,
    });
    updateSourceBreadcrumbs();
    scenarioConfigEditorController.syncAll();
    resultFileEditorController?.syncAll();
    refreshActiveDocumentNavigation({ outlineDelayMs: 80 });
    if (!isModelicaDocumentPath(nextPath)) {
        window.invalidateModelicaLiveChecks?.();
    }
    updateMobileAppbarTitle();
    if (isNarrowLayout()) {
        setSidebarCollapsed(true);
    }
    scheduleWorkspacePersistence(1200);
}

async function moveEditorTabToPane(path, sourcePaneId, targetPaneId) {
    const nextPath = trimMaybeString(path);
    const sourcePane = getEditorPane(sourcePaneId);
    const targetPane = getEditorPane(targetPaneId);
    if (!nextPath || !sourcePane || !targetPane) {
        return;
    }
    if (
        sourcePane.activePath === nextPath
        && !scenarioConfigEditorController.shouldUseScenarioEditor(sourcePane)
        && !resultFileEditorController?.shouldUseCustomFileEditor(sourcePane)
        && typeof sourcePane.editor?.getValue === 'function'
    ) {
        workspaceFs.setFile(nextPath, sourcePane.editor.getValue());
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
    await openWorkspaceDocument(nextPath, { paneId: targetPane.id, focusEditor: true, forceReload: true });
    collapseSingleEmptyPaneIfPossible();
}

async function splitEditorPaneWithTab(path, sourcePaneId, position) {
    const nextPath = trimMaybeString(path);
    if (!nextPath) {
        return;
    }
    ensureSecondarySourceEditor();
    const sourcePane = getEditorPane(sourcePaneId);
    if (
        sourcePane?.activePath === nextPath
        && !scenarioConfigEditorController.shouldUseScenarioEditor(sourcePane)
        && !resultFileEditorController?.shouldUseCustomFileEditor(sourcePane)
        && typeof sourcePane.editor?.getValue === 'function'
    ) {
        workspaceFs.setFile(nextPath, sourcePane.editor.getValue());
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
            await openWorkspaceDocument(nextPath, { paneId: 'primary', focusEditor: true, forceReload: true });
            if (secondaryPane.activePath) {
                await openWorkspaceDocument(secondaryPane.activePath, { paneId: 'secondary', focusEditor: false, forceReload: true });
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
            await openWorkspaceDocument(primaryPane.activePath, { paneId: 'primary', focusEditor: false, forceReload: true });
        } else {
            refreshEditorPaneEmptyState('primary');
        }
        await openWorkspaceDocument(nextPath, { paneId: 'secondary', focusEditor: true, forceReload: true });
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
            document.body.classList.remove('sidebar-resizing');
            document.body.style.cursor = '';
            document.body.style.userSelect = '';
            return;
        }
        const panelRect = workbenchSidepanel.getBoundingClientRect();
        const newWidth = e.clientX - panelRect.left;
        scheduleSidebarWidth(Math.max(SIDEBAR_RESIZE_MIN_WIDTH, Math.min(newWidth, SIDEBAR_RESIZE_MAX_WIDTH)));
        return;
    }
    if (isResizingV) {
        const workbenchRect = (mainContainer || workbenchMain).getBoundingClientRect();
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
    if (isResizingV || isResizingSidebar || isResizingEditorSplit) {
        isResizingV = false;
        flushSidebarWidth();
        isResizingSidebar = false;
        isResizingEditorSplit = false;
        document.body.classList.remove('sidebar-resizing');
        document.body.style.cursor = '';
        document.body.style.userSelect = '';
        layoutAllEditors();
        scheduleWorkspacePersistence();
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
window.activeBottomTab = 'output';

function normalizeTabName(tabName, allowed, fallback) {
    return allowed.includes(tabName) ? tabName : fallback;
}

function setBottomPanelCollapsed(collapsed) {
    bottomPanel.classList.toggle('collapsed', Boolean(collapsed));
    const arrow = document.getElementById('bottomArrow');
    if (!arrow) return;
    arrow.innerHTML = bottomPanel.classList.contains('collapsed') ? '&#9650;' : '&#9660;';
    scheduleWorkspacePersistence();
}

function refreshEditorActionButtons() {
    for (const button of editorCodegenButtons) {
        const pane = getEditorPane(button.dataset.paneId);
        const isModelica = trimMaybeString(pane?.activePath).toLowerCase().endsWith('.mo');
        button.hidden = !isModelica;
        button.disabled = !isModelica;
        button.setAttribute('aria-disabled', String(!isModelica));
        button.title = isModelica ? 'Create scenario for this Modelica file' : 'Open a Modelica file to create a scenario';
        button.setAttribute('aria-label', button.title);
    }
    for (const group of editorActionGroups) {
        const visibleButton = Array.from(group.querySelectorAll('button'))
            .some((button) => !button.hidden);
        group.hidden = !visibleButton;
    }
}

function relativePathFromDirectory(baseDir, targetPath) {
    const baseParts = pathParts(baseDir);
    const targetParts = pathParts(targetPath);
    let common = 0;
    while (common < baseParts.length && common < targetParts.length && baseParts[common] === targetParts[common]) {
        common += 1;
    }
    const parts = [
        ...Array.from({ length: baseParts.length - common }, () => '..'),
        ...targetParts.slice(common),
    ];
    return parts.join('/') || baseName(targetPath);
}

function escapeTomlBasicString(value) {
    return String(value || '')
        .replace(/\\/g, '\\\\')
        .replace(/"/g, '\\"');
}

function rewriteScenarioModelFile(content, scenarioPath, modelPath) {
    const relativeModelPath = relativePathFromDirectory(parentDirectory(scenarioPath), modelPath);
    return String(content || '').replace(
        /(\[model\][\s\S]*?\n\s*file\s*=\s*)"[^"]*"/,
        `$1"${escapeTomlBasicString(relativeModelPath)}"`,
    );
}

function normalizeScenarioPromptPath(path) {
    const nextPath = normalizePath(path);
    if (!nextPath) {
        return '';
    }
    return nextPath.endsWith('.toml') ? nextPath : `${nextPath}.toml`;
}

function defaultScenarioPathForModel(modelPath, modelName, fallbackPath) {
    const modelLeaf = trimMaybeString(modelName).split('.').filter(Boolean).pop()
        || baseName(modelPath).replace(/\.mo$/i, '')
        || 'model';
    const slug = sanitizeGeneratedPathSegment(modelLeaf).toLowerCase();
    const directory = parentDirectory(modelPath);
    const preferred = `${directory ? `${directory}/` : ''}rumoca-scenario.${slug}.toml`;
    return normalizePath(preferred || fallbackPath);
}

function promptForScenarioPath({ modelPath, modelName, defaultPath }) {
    const message = [
        `Create scenario for ${baseName(modelPath) || modelName || 'this model'}`,
        '',
        'Scenario file path:',
    ].join('\n');
    return normalizeScenarioPromptPath(window.prompt(message, defaultPath) || '');
}

async function modelNameForPane(pane) {
    const activePath = trimMaybeString(pane?.activePath);
    const source = typeof pane?.editor?.getValue === 'function'
        ? pane.editor.getValue()
        : workspaceFs.getFileContent(activePath);
    if (activePath.endsWith('.mo') && typeof source === 'string') {
        const modelState = await getSimulationModelState(source, '');
        return trimMaybeString(modelState.selectedModel)
            || trimMaybeString(modelState.models?.[0])
            || trimMaybeString(inferModelicaFileName(source, baseName(activePath)).replace(/\.mo$/i, ''));
    }
    const resolvedModels = await resolveSimulationModels();
    return currentSimulationModel()
        || trimMaybeString(resolvedModels.selectedModel)
        || trimMaybeString(resolvedModels.models[0])
        || '';
}

async function openScenarioConfigForCurrentModel(task = 'simulate', { promptForPath = false } = {}) {
    persistActivePaneDocument();
    const scenarioTask = task === 'codegen' ? 'codegen' : 'simulate';
    const pane = getEditorPane(activeEditorPaneId);
    const modelPath = trimMaybeString(pane?.activePath);
    const model = await modelNameForPane(pane);
    if (!model) {
        showTemplateError('Open a Modelica model before creating a scenario config.');
        return;
    }
    const generated = await scenarioInterface.execute('rumoca.scenario.defaultScenarioConfig', {
        model,
        task: scenarioTask,
    });
    const defaultPath = modelPath.endsWith('.mo')
        ? defaultScenarioPathForModel(modelPath, model, trimMaybeString(generated?.path))
        : trimMaybeString(generated?.path);
    const requestedPath = promptForPath
        ? promptForScenarioPath({ modelPath, modelName: model, defaultPath })
        : defaultPath;
    if (!requestedPath) {
        return;
    }
    if (!shared.isRumocaScenarioPath(requestedPath)) {
        showTemplateError('Scenario files must be named rumoca-scenario.toml or rumoca-scenario.<name>.toml.');
        return;
    }
    let scenarioContent = typeof generated?.content === 'string' ? generated.content : '';
    if (!requestedPath || !scenarioContent) {
        showTemplateError('Unable to create a scenario config for the selected model.');
        return;
    }
    if (modelPath.endsWith('.mo')) {
        scenarioContent = rewriteScenarioModelFile(scenarioContent, requestedPath, modelPath);
    }
    const existingContent = workspaceFs.getFileContent(requestedPath);
    if (typeof existingContent === 'string' && promptForPath && !window.confirm(`Replace ${requestedPath}?`)) {
        return;
    }
    if (typeof existingContent !== 'string' || promptForPath) {
        workspaceFs.setFile(requestedPath, scenarioContent);
    }
    expandWorkspaceExplorerPath(requestedPath);
    revealExplorerPath(requestedPath);
    await openWorkspaceDocument(requestedPath, { forceReload: true });
    scheduleWorkspacePersistence(0);
}

window.createScenarioForModelPane = async function(paneId) {
    setActiveEditorPane(paneId);
    await openScenarioConfigForCurrentModel('simulate', { promptForPath: true });
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
    scheduleWorkspacePersistence();
};

function collectWorkspaceEditorState() {
    const existingState = workspaceFs.getEditorState() || {};
    return {
        ...existingState,
        bottomTab: String(window.activeBottomTab || 'output'),
        bottomPanelCollapsed: bottomPanel.classList.contains('collapsed'),
        sidebarCollapsed: workbenchSidepanel?.classList.contains('collapsed') || false,
        sidebarMode: currentSidebarMode(),
        projectSectionCollapsed: projectSection?.classList.contains('collapsed') || false,
        explorerSectionCollapsed: explorerSection?.classList.contains('collapsed') || false,
        outlineSectionCollapsed: outlineSection?.classList.contains('collapsed') || false,
        leftAreaWidth: String(leftArea?.style.width || existingState.leftAreaWidth || ''),
        sidebarWidth: String(workbenchSidebar?.style.width || existingState.sidebarWidth || ''),
        bottomPanelHeight: String(bottomPanel?.style.height || existingState.bottomPanelHeight || ''),
        explorerCollapsedNodeIds: [...explorerCollapsedNodes],
        outlineCollapsedNodeIds: [...outlineCollapsedNodes],
        selectedExplorerPath: trimMaybeString(selectedExplorerPath),
        editorPaneVisible,
        openDocuments: openDocumentPaths.filter((path) => !isInteractiveRunPath(path)),
    };
}

function applyWorkspaceEditorState(editorState) {
    const fallbackState = defaultWorkspaceSeed?.editorState || {
        bottomTab: 'output',
        bottomPanelCollapsed: false,
        sidebarCollapsed: false,
        sidebarMode: 'explorer',
        projectSectionCollapsed: true,
        explorerSectionCollapsed: false,
        outlineSectionCollapsed: false,
        leftAreaWidth: '',
        sidebarWidth: '',
        bottomPanelHeight: '',
        explorerCollapsedNodeIds: [],
        outlineCollapsedNodeIds: [],
        selectedExplorerPath: '',
        editorPaneVisible: true,
        openDocuments: [],
    };
    const nextState = editorState && typeof editorState === 'object'
        ? {
            ...fallbackState,
            ...editorState,
        }
        : fallbackState;
    const bottomTab = normalizeTabName(
        String(nextState.bottomTab || fallbackState.bottomTab),
        ['output', 'errors'],
        'output',
    );
    const sidebarMode = normalizeTabName(
        String(nextState.sidebarMode || fallbackState.sidebarMode || 'explorer'),
        ['explorer'],
        'explorer',
    );

    applyPlaygroundTheme(playgroundThemeId, { persist: false });

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
    const restoredActivePath = trimMaybeString(workspaceFs.getActiveDocumentPath());
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
    setSidebarCollapsed(isNarrowLayout() ? true : Boolean(nextState.sidebarCollapsed));
    setSidebarSectionCollapsed('project', Boolean(nextState.projectSectionCollapsed));
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
    if (explorerSection) {
        explorerSection.style.flex = '';
        explorerSection.style.height = '';
    }
    window.switchBottomTab(bottomTab);
    refreshWorkbenchNavigation();
}

function fallbackDefaultEditorState(activePath = workspaceFs.getActiveDocumentPath()) {
    return {
        bottomTab: 'output',
        bottomPanelCollapsed: false,
        sidebarCollapsed: false,
        sidebarMode: 'explorer',
        projectSectionCollapsed: true,
        explorerSectionCollapsed: false,
        outlineSectionCollapsed: false,
        leftAreaWidth: '',
        sidebarWidth: '',
        bottomPanelHeight: '',
        explorerCollapsedNodeIds: [],
        outlineCollapsedNodeIds: [],
        selectedExplorerPath: activePath,
        editorPaneVisible: true,
        openDocuments: activePath ? [activePath] : [],
    };
}

function fallbackDefaultWorkspaceSeed() {
    const activeDocumentPath = workspaceFs.getActiveDocumentPath() || 'Main.mo';
    const activeDocumentContent = editor?.getValue?.() || 'model Main\nend Main;\n';
    return {
        activeDocumentPath,
        activeDocumentContent,
        editorState: fallbackDefaultEditorState(activeDocumentPath),
    };
}

async function loadDefaultWorkspaceSeed() {
    const entries = await loadDefaultWorkspaceEntries();
    const seedWorkspace = createWorkspaceFilesystem();
    const workspaceState = seedWorkspace.loadFileEntries(entries);
    const editorState = seedWorkspace.getEditorState()
        || fallbackDefaultEditorState(workspaceState.activeDocumentPath);
    seedWorkspace.setEditorState(editorState);
    return {
        entries: seedWorkspace.snapshotArchiveEntries({ includeCacheFiles: true }),
        activeDocumentPath: seedWorkspace.getActiveDocumentPath(),
        activeDocumentContent: seedWorkspace.getActiveDocumentContent(),
        editorState: seedWorkspace.getEditorState(),
    };
}

function buildNewWorkspaceState() {
    workspaceFs.clearWorkspace();
    const editorState = fallbackDefaultEditorState('Main.mo');
    editorState.projectSectionCollapsed = true;
    editorState.openDocuments = ['Main.mo'];
    editorState.selectedExplorerPath = 'Main.mo';
    workspaceFs.setActiveDocument('Main.mo', '');
    workspaceFs.setEditorState(editorState);
    return {
        activeDocumentPath: workspaceFs.getActiveDocumentPath(),
        activeDocumentContent: workspaceFs.getActiveDocumentContent(),
        editorState: workspaceFs.getEditorState(),
        packageArchives: [],
        fileCount: workspaceFs.listFileEntries().length,
    };
}

function buildExamplesWorkspaceState() {
    const seed = defaultWorkspaceSeed;
    workspaceFs.clearWorkspace();
    if (seed && Array.isArray(seed.entries) && seed.entries.length > 0) {
        const workspaceState = workspaceFs.loadFileEntries(seed.entries);
        if (seed.editorState) {
            workspaceFs.setEditorState(seed.editorState);
            workspaceState.editorState = workspaceFs.getEditorState();
        }
        return workspaceState;
    }
    return buildNewWorkspaceState();
}

function scenarioSimulationFallback(sourceRootPaths = []) {
    return {
        solver: 'auto',
        tEnd: 10.0,
        dt: null,
        outputDir: DEFAULT_WEB_RESULTS_OUTPUT_DIR,
        sourceRootPaths,
    };
}

async function scenarioSimulationFallbackForFocus(focusPath = workspaceFs.getActiveDocumentPath()) {
    return scenarioSimulationFallback(await effectiveWorkspaceSourceRootPaths(focusPath));
}

async function getSimulationModelState(source, defaultModel = '') {
    const state = await scenarioInterface.execute('rumoca.scenario.getSimulationModels', {
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
        || '',
    );
}

function collectWorkspaceModelicaSources(
    excludePath = workspaceFs.getActiveDocumentPath(),
    excludeSourceRootPaths = [],
) {
    return shared.workspaceModelicaSourceMap(workspaceFs.listFiles(), {
        excludePath,
        excludeSourceRootPaths,
    });
}

function collectWorkspaceModelicaSourcesJson(
    excludePath = workspaceFs.getActiveDocumentPath(),
    excludeSourceRootPaths = [],
) {
    return shared.workspaceModelicaSourcesJson(workspaceFs.listFiles(), {
        excludePath,
        excludeSourceRootPaths,
    });
}

function normalizeSourceRootPath(path) {
    const parts = [];
    for (const part of pathParts(normalizePath(path))) {
        if (part === '..') {
            parts.pop();
        } else {
            parts.push(part);
        }
    }
    return parts.join('/');
}

function collectWorkspaceSourceRootsJson(sourceRootPaths = []) {
    return shared.workspaceSourceRootSourcesJson(workspaceFs.listFiles(), sourceRootPaths);
}

function scenarioUsesInputRuntime(config) {
    const input = config?.input;
    return Boolean(input && input.enabled !== false);
}

function workspaceRelativeUrl(path) {
    const normalized = normalizePath(path);
    if (!normalized) {
        return '';
    }
    const assetBase = new URL(window.rumocaRepoAssetBase || '../../', window.location.href);
    return new URL(normalized, assetBase).href;
}

function workspaceDirectoryUrl(path) {
    const url = workspaceRelativeUrl(path);
    return url ? (url.endsWith('/') ? url : `${url}/`) : '';
}

function scenarioRelativeFileText(scenarioPath, relativePath, label) {
    const resolvedPath = resolveWorkspaceRelativePath(scenarioPath, relativePath);
    const content = resolvedPath ? workspaceFs.getFileContent(resolvedPath) : null;
    if (typeof content !== 'string') {
        throw new Error(`${label} not found: ${relativePath}`);
    }
    return content;
}

function scenarioInteractiveScriptText(config, scenarioPath) {
    const scenePath = trimMaybeString(config?.transport?.http?.scene);
    return scenePath ? scenarioRelativeFileText(scenarioPath, scenePath, 'Interactive scene script') : '';
}

function scenarioAssetBaseUrl(config, scenarioPath) {
    const assetDir = trimMaybeString(config?.transport?.http?.asset_dir);
    if (!assetDir) {
        return '';
    }
    return workspaceDirectoryUrl(resolveWorkspaceRelativePath(scenarioPath, assetDir));
}

function scenarioLocalSourceRootPaths(config, scenarioPath) {
    return Array.isArray(config?.source_roots)
        ? config.source_roots
            .map((path) => resolveWorkspaceRelativePath(scenarioPath, path))
            .filter(Boolean)
        : [];
}

async function interactiveSourceRootPaths(context) {
    const fallback = await scenarioSimulationFallbackForFocus(context.modelPath || context.path);
    const simulationConfig = await scenarioInterface.execute('rumoca.scenario.getSimulationConfig', {
        model: context.modelName,
        fallback,
    });
    const effective = Array.isArray(simulationConfig?.effective?.sourceRootPaths)
        ? simulationConfig.effective.sourceRootPaths
        : fallback.sourceRootPaths;
    return Array.from(new Set([
        ...effective.map(normalizeSourceRootPath).filter(Boolean),
        ...scenarioLocalSourceRootPaths(context.config, context.path).map(normalizeSourceRootPath),
    ].filter(Boolean)));
}

async function storeInteractiveRunContext(context) {
    const runPath = interactiveRunPathForScenario(context.path, context.modelName);
    const sourceRootPaths = await interactiveSourceRootPaths(context);
    const sourceRoots = collectWorkspaceSourceRootsJson(sourceRootPaths);
    interactiveRunContexts.set(runPath, {
        path: runPath,
        scenarioPath: context.path,
        source: context.source,
        modelName: context.modelName,
        config: context.config,
        scriptText: scenarioInteractiveScriptText(context.config, context.path),
        assetBaseUrl: scenarioAssetBaseUrl(context.config, context.path),
        sourceRootCacheUrl: '',
        sourceRoots,
        workspaceSources: collectWorkspaceModelicaSourcesJson(
            context.modelPath || context.path,
            sourceRootPaths,
        ),
    });
    resultFileEditorController?.invalidatePath(runPath);
    return runPath;
}

async function openInteractiveRunForContext(context, paneId) {
    const status = document.getElementById('simStatus');
    if (status) {
        status.textContent = 'Starting interactive simulation...';
        status.style.color = '#9a6700';
    }
    const runPath = await storeInteractiveRunContext(context);
    await openWorkspaceDocument(runPath, {
        paneId,
        focusEditor: false,
        forceReload: true,
    });
    if (status) {
        status.textContent = 'Interactive simulation opened';
        status.style.color = '#2d6a4f';
    }
    return {
        ok: true,
        message: `Started interactive simulation for ${context.modelName}`,
        runPath,
    };
}

function isWorkspaceConfigPath(path) {
    const normalized = String(path || '').replace(/\\/g, '/').replace(/^\/+/, '');
    return normalized.split('/').pop()?.toLowerCase() === 'rumoca-workspace.toml';
}

function collectWorkspaceConfigSources() {
    const sources = {};
    for (const entry of workspaceFs.listFiles()) {
        const path = normalizePath(entry?.path);
        if (!path || !isWorkspaceConfigPath(path) || typeof entry?.content !== 'string') {
            continue;
        }
        sources[path] = entry.content;
    }
    return sources;
}

function collectWorkspaceConfigSourcesJson() {
    return JSON.stringify(collectWorkspaceConfigSources());
}

async function effectiveWorkspaceSourceRootPaths(focusPath = workspaceFs.getActiveDocumentPath()) {
    const raw = await sendWorkspaceCommand('rumoca.workspace.effectiveSourceRoots', {
        workspaceSources: collectWorkspaceConfigSourcesJson(),
        focusPath: normalizePath(focusPath),
    });
    const parsed = JSON.parse(raw);
    return Array.isArray(parsed)
        ? parsed.map((entry) => trimMaybeString(entry)).filter(Boolean)
        : [];
}

function simulationModelPreference(preferredModel = '') {
    return trimMaybeString(preferredModel)
        || trimMaybeString(window.selectedModel)
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
        scenarioInterface.execute('rumoca.scenario.setSelectedSimulationModel', { model: resolvedSelection });
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
    if (!workspaceFs.getActiveDocumentPath().endsWith('.mo') || !editor) {
        return updateSimulationModelOptions([], preferred);
    }
    const modelState = await getSimulationModelState(editor.getValue(), preferred);
    return updateSimulationModelOptions(modelState.models, modelState.selectedModel || preferred);
}

sidebarContextMenu?.addEventListener('click', (event) => {
    event.stopPropagation();
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
    }
});
document.addEventListener('visibilitychange', () => {
    if (document.visibilityState === 'hidden') {
        void flushWorkspacePersistence();
    }
});
window.addEventListener('resize', () => {
    closeSidebarContextMenu();
});
window.addEventListener('pagehide', () => {
    void flushWorkspacePersistence();
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

async function scenarioContextForPath(scenarioPath) {
    const path = normalizePath(scenarioPath);
    if (!shared.isRumocaScenarioPath(path)) {
        return null;
    }
    persistActivePaneDocument();
    const full = await scenarioInterface.execute('rumoca.scenario.getScenarioConfigFull', { path });
    if (!full || full.ok === false) {
        throw new Error(full?.error || 'Unable to load scenario config.');
    }
    const config = full.config || {};
    const modelName = trimMaybeString(config.model?.name);
    const modelFile = trimMaybeString(config.model?.file);
    const modelPath = modelFile ? resolveWorkspaceRelativePath(path, modelFile) : '';
    const modelSource = modelPath ? workspaceFs.getFileContent(modelPath) : null;
    if (modelPath && typeof modelSource !== 'string') {
        throw new Error(`Scenario model file not found: ${modelPath}`);
    }
    return {
        path,
        config,
        task: trimMaybeString(config.rumoca?.task) === 'codegen' ? 'codegen' : 'simulate',
        modelName,
        modelPath,
        source: typeof modelSource === 'string' ? modelSource : (window.editor ? window.editor.getValue() : ''),
    };
}

async function runSimulationWithContext({ modelName, source, focusPath, excludePath } = {}) {
    const selectedModel = trimMaybeString(modelName) || document.getElementById('modelSelect').value;
    if (selectedModel) {
        const modelSelect = document.getElementById('modelSelect');
        if (modelSelect && Array.from(modelSelect.options).some((option) => option.value === selectedModel)) {
            modelSelect.value = selectedModel;
        }
        window.selectedModel = selectedModel;
        scenarioInterface.execute('rumoca.scenario.setSelectedSimulationModel', { model: selectedModel });
    }
    const sourceText = typeof source === 'string' ? source : (window.editor ? window.editor.getValue() : '');
    const rootFocusPath = normalizePath(focusPath || workspaceFs.getActiveDocumentPath());
    const workspaceExcludePath = normalizePath(excludePath || rootFocusPath);
    const model = selectedModel;
    if (!model) {
        document.getElementById('simStatus').textContent = 'No model selected';
        throw new Error('No model selected.');
    }
    const simulationFallback = await scenarioSimulationFallbackForFocus(rootFocusPath);
    const simulationConfig = await scenarioInterface.execute('rumoca.scenario.getSimulationConfig', {
        model,
        fallback: simulationFallback,
    });
    const tEnd = Number(simulationConfig.effective?.tEnd) || 1.0;
    const dt = Number(simulationConfig.effective?.dt) || 0;
    const status = document.getElementById('simStatus');

    status.textContent = 'Simulating...';
    status.style.color = '#9a6700';
    setTerminalOutput(`Simulating ${model}...`);

    try {
        const simulationPayload = {
            source: sourceText,
            model,
            fallback: simulationFallback,
            timeoutMs: 60000,
        };
        if (shared.isRumocaScenarioPath(rootFocusPath)) {
            simulationPayload.scenarioPath = rootFocusPath;
        }
        simulationPayload.workspaceSources = collectWorkspaceModelicaSourcesJson(
            workspaceExcludePath,
            simulationConfig.effective?.sourceRootPaths,
        );
        workspaceSourcesSynced = true;
        const result = await scenarioInterface.execute('rumoca.scenario.startSimulation', simulationPayload);
        const simulateMs = Math.round((Number(result.metrics?.simulateSeconds) || 0) * 1000);
        status.textContent =
            `${result.metrics?.points ?? 0} pts, ${result.metrics?.variables ?? 0} vars (${simulateMs}ms)`;
        status.style.color = '#2d6a4f';
        const persistedRun = await persistSimulationResultFile(model, result, scenarioResultDirectory(result));
        if (persistedRun?.runPath) {
            renderExplorerPane();
            revealExplorerPath(persistedRun.runPath);
            await openWorkspaceDocument(persistedRun.runPath, {
                paneId: 'primary',
                focusEditor: false,
                forceReload: true,
            });
        }
        setTerminalOutput(
            persistedRun?.runPath
                ? `Saved simulation results to ${persistedRun.runPath}\n${result.metrics?.points ?? 0} points, ${result.metrics?.variables ?? 0} variables (${simulateMs}ms).`
                : `Simulated ${model}\n${result.metrics?.points ?? 0} points, ${result.metrics?.variables ?? 0} variables (${simulateMs}ms).`,
        );
        return {
            ok: true,
            message: persistedRun?.runPath
                ? `Saved results to ${persistedRun.runPath}`
                : `Simulated ${model}`,
            runPath: persistedRun?.runPath || '',
        };
    } catch (e) {
        status.textContent = e.message || 'Simulation failed';
        status.style.color = '#c9184a';
        setTerminalOutput(`Simulation failed for ${model}: ${e.message || e}`);
        throw e;
    }
}

window.runSimulation = async function() {
    const modelName = document.getElementById('modelSelect').value;
    return await runSimulationWithContext({
        modelName,
        source: window.editor ? window.editor.getValue() : '',
        focusPath: workspaceFs.getActiveDocumentPath(),
        excludePath: workspaceFs.getActiveDocumentPath(),
    });
};

window.runSimulationForPane = function(paneId) {
    setActiveEditorPane(paneId);
    return window.runSimulation();
};

window.runScenarioForPane = async function(paneId) {
    setActiveEditorPane(paneId);
    try {
        const pane = getEditorPane(paneId);
        const context = await scenarioContextForPath(pane?.activePath);
        if (!context) {
            return await window.runSimulation();
        }
        if (!context.modelName) {
            document.getElementById('simStatus').textContent = 'Scenario has no model name';
            throw new Error('Scenario has no model name.');
        }
        if (context.task === 'codegen') {
            return await createCodegenRunForModel(context.modelName);
        }
        if (scenarioUsesInputRuntime(context.config)) {
            return await openInteractiveRunForContext(context, pane.id);
        }
        return await runSimulationWithContext({
            modelName: context.modelName,
            source: context.source,
            focusPath: context.path,
            excludePath: context.modelPath || context.path,
        });
    } catch (error) {
        const status = document.getElementById('simStatus');
        if (status) {
            status.textContent = error.message || 'Scenario run failed';
            status.style.color = '#c9184a';
        }
        throw error;
    }
};

// Display output for the active tab
function displayModelOutput(modelName) {
    displayDaeOutput(modelName);
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

async function createCodegenRunForModel(modelName) {
    const nextModel = trimMaybeString(modelName);
    if (!nextModel) {
        showTemplateError('No model selected for code generation output.');
        throw new Error('No model selected for code generation output.');
    }
    const result = window.compiledModels[nextModel];
    if (!result || result.error || !result.dae_native) {
        showTemplateError('Compile a model before rendering code generation output.');
        throw new Error('Compile a model before rendering code generation output.');
    }
    try {
        await ensureBuiltInCodegenTemplatesLoaded();
        const templateSelection = await resolveCodegenTemplateSelection(nextModel);
        let renderedFiles;
        if (GALEC_CODEGEN_TARGETS.has(templateSelection.target)) {
            const source = trimMaybeString(result.source);
            if (!source) {
                throw new Error('GALEC code generation needs the Modelica source; recompile the model.');
            }
            renderedFiles = await renderGalecCodegenSelection(nextModel, source, templateSelection.target);
        } else {
            const daeJson = JSON.stringify(result.dae_native);
            renderedFiles = await renderCodegenSelection(nextModel, daeJson, templateSelection);
        }
        const configuredOutputRoot = normalizePath(templateSelection.outputRoot || '');
        const outputRoot = configuredOutputRoot || defaultCodegenOutputRoot(nextModel, templateSelection);
        const paths = writeRenderedTargetFiles(renderedFiles, outputRoot);
        await openWorkspaceDocument(paths[0], { forceReload: true });
        revealExplorerPath(paths[0]);
        setTerminalOutput(`Generated ${paths.length} file(s) in ${outputRoot}.`);
        clearTemplateErrors();
        return {
            ok: true,
            message: `Generated ${paths.length} file(s) in ${outputRoot}`,
            paths,
        };
    } catch (e) {
        showTemplateError(e.message || 'Failed to render code generation output.');
        throw e;
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

async function refreshDiagnosticsAfterCompileFailure(source) {
    try {
        const diagJson = await sendLanguageCommand('rumoca.language.diagnostics', { source });
        const diagnostics = normalizeDiagnosticsPayload(JSON.parse(diagJson), source);
        updateDiagnostics(diagnostics);
        return {
            diagnostics,
            hasErrorDiagnostics: diagnostics.some(diagnostic => diagnostic?.severity === 1),
        };
    } catch (error) {
        return { diagnostics: [], hasErrorDiagnostics: false };
    }
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
    workspaceFs,
    onPackageArchivesChanged: () => {
        collapseImportedExplorerBranches();
        refreshWorkbenchNavigation({ includeOutline: false });
        scheduleWorkspacePersistence(2000);
    },
});
packageArchiveController.bindWindowApi();

// Store compiled results for all models: { modelName: { dae, balance } }
window.compiledModels = {};
window.selectedModel = null;
let workspaceSourcesSynced = false;
let lastLiveCheckCompileKey = null;

function resetCompiledWorkspaceState() {
    window.compiledModels = {};
    window.selectedModel = null;
    workspaceSourcesSynced = false;
    lastLiveCheckCompileKey = null;
    window.currentDaeForCompletions = null;
    const modelSelect = document.getElementById('modelSelect');
    if (modelSelect) {
        modelSelect.innerHTML = '<option value="">-- No models --</option>';
        modelSelect.value = '';
    }
    scenarioInterface.execute('rumoca.scenario.setSelectedSimulationModel', { model: '' });
    setCompileStatusBadge('Waiting...', 'loading');
}

async function applyImportedWorkspace(workspaceState) {
    resetCompiledWorkspaceState();
    startupCompileRequested = false;
    suspendWorkspaceObservers = true;
    try {
        if (editor?.setValue) {
            editor.setValue(workspaceState.activeDocumentContent || '');
        }
        applyWorkspaceEditorState(workspaceState.editorState);
        const activePath = trimMaybeString(workspaceFs.getActiveDocumentPath());
        if (activePath) {
            workspaceFs.activateDocument(activePath);
        }
    } finally {
        suspendWorkspaceObservers = false;
    }
    await packageArchiveController.restoreWorkspacePackageArchives();
    updateSourceBreadcrumbs();
    refreshWorkbenchNavigation();
    scenarioConfigEditorController.syncAll();
    resultFileEditorController?.syncAll();
    scheduleWorkspacePersistence(0);
    requestStartupCompileIfReady();
}

async function restorePersistedWorkspaceIfAvailable() {
    const entries = await loadPersistedWorkspaceEntries();
    if (!Array.isArray(entries) || entries.length === 0) {
        return false;
    }
    const workspaceState = workspaceFs.loadArchiveEntries(entries);
    await applyImportedWorkspace(workspaceState);
    return true;
}

installFileActions({
    getEditor: () => window.editor,
    workspaceFs,
    setTerminalOutput,
    beforeWorkspaceExport: async () => {
        workspaceFs.setEditorState(collectWorkspaceEditorState());
    },
    onCreateNewWorkspace: async () => {
        await applyImportedWorkspace(buildNewWorkspaceState());
    },
    onLoadExamplesWorkspace: async () => {
        if (!defaultWorkspaceSeed) {
            defaultWorkspaceSeed = await loadDefaultWorkspaceSeed();
        }
        await applyImportedWorkspace(buildExamplesWorkspaceState());
    },
    onWorkspaceLoaded: async (workspaceState) => {
        await applyImportedWorkspace(workspaceState);
    },
});

// Update display when model selection changes
window.updateSelectedModel = function() {
    const modelName = document.getElementById('modelSelect').value;
    window.selectedModel = modelName;
    scenarioInterface.execute('rumoca.scenario.setSelectedSimulationModel', { model: modelName });
    if (modelName && window.compiledModels[modelName]) {
        const result = window.compiledModels[modelName];
        // Update DAE for codegen completions
        if (result.dae_native) {
            window.currentDaeForCompletions = result.dae_native;
        }
    }
    updateSourceBreadcrumbs();
    scenarioConfigEditorController.syncAll();
    resultFileEditorController?.syncAll();
};

const sourceBreadcrumbs = createSourceBreadcrumbs();

function updateSourceBreadcrumbs() {
    sourceBreadcrumbs.update();
}

// Web Worker setup (cache-busted to avoid stale JS/WASM bundles during local iteration)
const workerCacheBust = String(Date.now());
const wasmUrlParams = new URLSearchParams(window.location.search);
const defaultWasmPkgBase = window.rumocaWasmPkgBase || '../rumoca/dist';
const defaultWasmPkgSubdir = window.rumocaWasmPkgSubdir || 'release-full-web';
const wasmPkgSubdir = wasmUrlParams.get('smoke_pkg_subdir') || defaultWasmPkgSubdir;
let mainThreadWasmModulePromise = null;
let playgroundInteractiveRuntimePromise = null;
const workerUrl = new URL(`${defaultWasmPkgBase}/${wasmPkgSubdir}/rumoca_worker.js`, window.location.href);
workerUrl.searchParams.set('v', workerCacheBust);
const worker = new Worker(workerUrl, { type: 'module' });
let requestId = 0;
const pendingRequests = new Map();
let workerReady = false;
let workerInitError = null;
const workerReadyWaiters = [];

function wasmPackageBaseUrl() {
    return new URL(`${defaultWasmPkgBase}/${wasmPkgSubdir}/`, window.location.href).href;
}

async function loadMainThreadWasmModule() {
    if (!mainThreadWasmModulePromise) {
        mainThreadWasmModulePromise = (async () => {
            const base = wasmPackageBaseUrl();
            const moduleUrl = new URL('rumoca_bind_wasm.js', base);
            const wasmUrl = new URL('rumoca_bind_wasm_bg.wasm', base);
            moduleUrl.searchParams.set('v', workerCacheBust);
            wasmUrl.searchParams.set('v', workerCacheBust);
            const module = await import(moduleUrl.href);
            await module.default({ module_or_path: wasmUrl.href });
            await module.wasm_init?.(0);
            return module;
        })().catch((error) => {
            mainThreadWasmModulePromise = null;
            throw error;
        });
    }
    return mainThreadWasmModulePromise;
}

async function loadPlaygroundInteractiveRuntime() {
    if (!playgroundInteractiveRuntimePromise) {
        playgroundInteractiveRuntimePromise = (async () => {
            const runtimeUrl = new URL('rumoca_interactive.js', wasmPackageBaseUrl());
            runtimeUrl.searchParams.set('v', workerCacheBust);
            return await import(runtimeUrl.href);
        })().catch((error) => {
            playgroundInteractiveRuntimePromise = null;
            throw error;
        });
    }
    return playgroundInteractiveRuntimePromise;
}

function settleWorkerReadyWaiters(error = null) {
    while (workerReadyWaiters.length > 0) {
        const waiter = workerReadyWaiters.shift();
        clearTimeout(waiter.timeoutId);
        if (error) {
            waiter.reject(error);
        } else {
            waiter.resolve();
        }
    }
}

function waitForWorkerReady(timeout) {
    if (workerReady) {
        return Promise.resolve();
    }
    if (workerInitError) {
        return Promise.reject(workerInitError);
    }
    return new Promise((resolve, reject) => {
        const timeoutId = setTimeout(() => {
            const index = workerReadyWaiters.findIndex(waiter => waiter.timeoutId === timeoutId);
            if (index >= 0) {
                workerReadyWaiters.splice(index, 1);
            }
            reject(new Error('WASM worker initialization timed out'));
        }, timeout);
        workerReadyWaiters.push({ resolve, reject, timeoutId });
    });
}

worker.onmessage = (e) => {
    const { id, ready, success, result, error, progress } = e.data;

    if (progress) {
        const resolver = id !== null && id !== undefined ? pendingRequests.get(id) : null;
        if (resolver?.refreshTimeout) {
            resolver.refreshTimeout();
        }
        handleWorkerProgress(e.data);
        return;
    }

    if (ready !== undefined) {
        if (success) {
            setRuntimeStatusBar('Ready', 'ready');
            workerReady = true;
            settleWorkerReadyWaiters();
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
            workerInitError = new Error('WASM worker failed to initialize');
            settleWorkerReadyWaiters(workerInitError);
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
    workerInitError = new Error(e.message || 'WASM worker failed');
    settleWorkerReadyWaiters(workerInitError);
};

function sendRequest(action, params = {}, timeout = 30000) {
    const id = ++requestId;
    return new Promise((resolve, reject) => {
        waitForWorkerReady(timeout)
            .then(() => {
                let timeoutId = null;
                const onTimeout = () => {
                    if (pendingRequests.has(id)) {
                        pendingRequests.delete(id);
                        console.error(`[sendRequest] timeout for action '${action}' (id=${id})`);
                        reject(new Error(`Request timeout for ${action}`));
                    }
                };
                const refreshTimeout = () => {
                    if (timeoutId !== null) {
                        clearTimeout(timeoutId);
                    }
                    timeoutId = setTimeout(onTimeout, timeout);
                };
                refreshTimeout();

                pendingRequests.set(id, {
                    resolve: (result) => {
                        clearTimeout(timeoutId);
                        resolve(result);
                    },
                    reject: (requestError) => {
                        clearTimeout(timeoutId);
                        reject(requestError);
                    },
                    refreshTimeout,
                });
                worker.postMessage({ id, action, ...params });
            })
            .catch(reject);
    });
}

function languageCommandNeedsWorkspaceSources(command) {
    return command === 'rumoca.language.diagnostics';
}

function augmentLanguagePayload(command, payload = {}) {
    if (!languageCommandNeedsWorkspaceSources(command)) {
        return payload;
    }
    const activePath = workspaceFs?.getActiveDocumentPath?.() || '';
    return {
        ...payload,
        workspaceSources: collectWorkspaceModelicaSourcesJson(activePath),
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
            if (typeof window.importLoadedPackageArchivesForSmoke !== 'function') {
                throw new Error('source-root import unavailable for smoke archive load');
            }
            const imported = await window.importLoadedPackageArchivesForSmoke();
            result.sourceRootImportMs = Math.max(0, Number(imported?.sourceRootImportMs) || 0);
            rumocaSmokeSourceRootsImported = true;
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
        const workspaceSources = collectWorkspaceModelicaSourcesJson(workspaceFs.getActiveDocumentPath());
        const compileJson = await sendWorkspaceCommand(
            'rumoca.workspace.compileWithWorkspaceSources',
            {
                source: window.editor.getValue(),
                modelName: smokeModelName,
                workspaceSources,
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

async function openWorkspaceLocation({ path, lineNumber, column, target } = {}) {
    if (target === 'template') {
        return false;
    }

    const nextPath = trimMaybeString(path);
    if (nextPath && typeof workspaceFs.getFileContent(nextPath) === 'string') {
        await openWorkspaceDocument(nextPath, {
            paneId: 'primary',
            focusEditor: true,
        });
    }

    jumpToModelicaLocation(lineNumber, column);
    return true;
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
                detail: 'Select model',
                tags: ['model', 'open'],
                run: () => {
                    modelSelect.value = modelName;
                    if (typeof window.updateSelectedModel === 'function') {
                        window.updateSelectedModel();
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
            iconKind: 'outline-symbol',
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
                    iconKind: 'outline-symbol',
                    run: () => jumpToModelicaLocation(pos.lineNumber, pos.column),
                });
                currentNodes = branch.children;
            }
        }
        currentNodes.push(createSidebarNode(`outline:${containerName ? `${containerName}.` : ''}${name}`, name, {
            kind: 'file',
            meta: 'Symbol',
            sourceKind: 'workspace',
            iconKind: 'outline-symbol',
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
require.config({ paths: { 'vs': './vendor/monaco/vs' }});
require(['vs/editor/editor.main'], function() {
    const monacoState = setupMonacoWorkspace({ monaco, sendLanguageCommand, layoutAllEditors });
    monacoApi = monaco;
    createSourceEditorFactory = monacoState.createSourceEditor;
    applyMonacoTheme = monacoState.setEditorTheme;
    applyPlaygroundTheme(playgroundThemeId, { persist: false });
    editor = monacoState.editor;
    editorPanes.primary.editor = editor;
    workspaceFs.setActiveDocument(
        inferModelicaFileName(editor.getValue(), 'Ball.mo'),
        editor.getValue(),
    );
    syncOpenDocuments([workspaceFs.getActiveDocumentPath()], workspaceFs.getActiveDocumentPath());
    editorPanes.primary.activePath = workspaceFs.getActiveDocumentPath();
    refreshWorkbenchNavigation();
    defaultWorkspaceSeed = fallbackDefaultWorkspaceSeed();
    window.openWorkspaceDocument = (path) => {
        void openWorkspaceDocument(path);
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

    function modelicaCompileSignature(source) {
        let out = '';
        let i = 0;
        while (i < source.length) {
            const ch = source[i];
            const next = source[i + 1] || '';
            if (/\s/.test(ch)) {
                i += 1;
                continue;
            }
            if (ch === '/' && next === '/') {
                i += 2;
                while (i < source.length && source[i] !== '\n') i += 1;
                continue;
            }
            if (ch === '/' && next === '*') {
                i += 2;
                while (i < source.length && !(source[i] === '*' && source[i + 1] === '/')) {
                    i += 1;
                }
                i += i < source.length ? 2 : 0;
                continue;
            }
            if (ch === '"') {
                out += ch;
                i += 1;
                while (i < source.length) {
                    const stringChar = source[i];
                    out += stringChar;
                    i += 1;
                    if (stringChar === '\\' && i < source.length) {
                        out += source[i];
                        i += 1;
                    } else if (stringChar === '"') {
                        break;
                    }
                }
                continue;
            }
            out += ch;
            i += 1;
        }
        return out;
    }

    function liveCheckCompileKey(source, path) {
        return `${trimMaybeString(path)}\0${modelicaCompileSignature(source)}`;
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

    diagnosticsController = createDiagnosticsController({
        monaco,
        getModelEditor: () => editor,
        getModelPath: () => workspaceFs.getActiveDocumentPath(),
        getTemplateEditor: () => null,
        switchToErrorsTab: () => window.switchBottomTab('errors'),
        triggerModelicaQuickFix: triggerModelicaQuickFixAt,
        openLocation: openWorkspaceLocation,
    });
    window.renderAllDiagnostics = () => diagnosticsController.renderAllDiagnostics();

    let liveCheckGeneration = 0;
    let liveCheckInFlight = false;
    let liveCheckRerunRequested = false;
    function invalidateModelicaLiveChecks() {
        liveCheckGeneration++;
        liveCheckRerunRequested = false;
        lastLiveCheckCompileKey = null;
        updateDiagnostics([]);
        updateCompileErrors([]);
        window.compiledModels = {};
        window.selectedModel = null;
        window.currentDaeForCompletions = null;
        setCompileStatusBadge('Idle', 'loading');
    }
    window.invalidateModelicaLiveChecks = invalidateModelicaLiveChecks;

    const runLiveChecks = debounce(async (force = false) => {
        if (isRumocaSmokeMode()) return;
        if (!workerReady) return;
        const sourcePath = activeEditorDocumentPath();
        if (!isModelicaDocumentPath(sourcePath)) {
            invalidateModelicaLiveChecks();
            return;
        }
        const source = editor.getValue();
        const compileKey = liveCheckCompileKey(source, sourcePath);
        if (!force && compileKey === lastLiveCheckCompileKey) {
            return;
        }
        lastLiveCheckCompileKey = compileKey;
        if (liveCheckInFlight) {
            liveCheckRerunRequested = true;
            setCompileStatusBadge('Queued after current check...', 'loading');
            return;
        }
        liveCheckInFlight = true;
        const runGeneration = ++liveCheckGeneration;
        const isStaleRun = () => runGeneration !== liveCheckGeneration;

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
        let workspaceSyncedByDiagnostics = false;

        try {
        let diagnostics = [];
        // Run diagnostics
        try {
            setCompileStatus('Checking diagnostics...', '#9a6700');
            const diagJson = await sendLanguageCommand('rumoca.language.diagnostics', { source });
            workspaceSyncedByDiagnostics = true;
            workspaceSourcesSynced = true;
            if (isStaleRun()) return;
            diagnostics = normalizeDiagnosticsPayload(JSON.parse(diagJson), source);
            if (isStaleRun()) return;
            updateDiagnostics(diagnostics);
        } catch (e) {
            if (isStaleRun()) return;
            // Clear old diagnostics on error to avoid showing stale errors
            updateDiagnostics([]);
            diagnostics = [];
        }

        setCompileStatus('Discovering models...', '#9a6700');
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
        let workspaceSourceCount = 0;
        let workspaceSources = '{}';
        if (workspaceSyncedByDiagnostics) {
            setCompileStatus('Using synced workspace files...', '#9a6700');
        } else {
            setCompileStatus('Collecting workspace files...', '#9a6700');
            await yieldToBrowserPaint();
            const workspaceSourcesMap = collectWorkspaceModelicaSources(workspaceFs.getActiveDocumentPath());
            workspaceSourceCount = Object.keys(workspaceSourcesMap).length;
            workspaceSources = JSON.stringify(workspaceSourcesMap);
        }
        let successCount = 0;
        const compileErrors = [];

        for (const [modelIndex, modelName] of models.entries()) {
            if (isStaleRun()) return;
            try {
                let json;
                const sourceCountLabel = workspaceSourceCount > 0
                    ? `, ${workspaceSourceCount} workspace files`
                    : '';
                setCompileStatus(
                    `Compiling ${modelName} ${modelIndex + 1}/${models.length}${sourceCountLabel}`,
                    '#9a6700',
                );
                if (workspaceSyncedByDiagnostics) {
                    json = await sendWorkspaceCommand('rumoca.workspace.compile', {
                        source,
                        modelName,
                    });
                } else {
                    json = await sendWorkspaceCommand('rumoca.workspace.compileWithWorkspaceSources', {
                        source,
                        modelName,
                        workspaceSources,
                    });
                }
                if (isStaleRun()) return;
                const result = JSON.parse(json);
                window.compiledModels[modelName] = {
                    dae: result.dae,
                    dae_native: result.dae_native,
                    balance: result.balance,
                    pretty: result.pretty,
                    // GALEC codegen re-projects from source (the addon owns the
                    // flat model), so keep the source that produced this model.
                    source,
                };
                // Update DAE for codegen completions (use dae_native for actual field names)
                if (result.dae_native && (modelName === modelSelect.value || modelSelect.value === '')) {
                    window.currentDaeForCompletions = result.dae_native;
                }
                successCount++;
            } catch (e) {
                if (isStaleRun()) return;
                const refreshedDiagnostics = await refreshDiagnosticsAfterCompileFailure(source);
                if (isStaleRun()) return;
                if (refreshedDiagnostics.hasErrorDiagnostics) {
                    diagnostics = refreshedDiagnostics.diagnostics;
                    continue;
                }
                window.compiledModels[modelName] = {
                    dae: null,
                    balance: null,
                    error: e.message
                };
                compileErrors.push({
                    model: modelName,
                    message: e.message,
                    path: sourcePath,
                });
            }
        }

        const elapsed = (performance.now() - startTime).toFixed(0);
        if (isStaleRun()) return;
        updateCompileErrors(compileErrors);
        if (diagnostics.some(diagnostic => diagnostic?.severity === 1)) {
            setCompileStatus(`Error (${elapsed}ms)`, '#c9184a');
            setDaeOutput('Compilation skipped due to diagnostics errors.');
            window.switchBottomTab('errors');
            if (window.refreshCodeLens) window.refreshCodeLens();
            if (document.getElementById('classTreePanel')) {
                packageArchiveController.refreshPackageViewer(true);
            }
            return;
        }

        // Update display with selected model
        let selectedModel = modelSelect.value;

        // If no selection but we have models, select the first one
        if (!selectedModel && models.length > 0) {
            selectedModel = models[0];
            modelSelect.value = selectedModel;
        }
        window.selectedModel = selectedModel;

        if (selectedModel && window.compiledModels[selectedModel]) {
            const result = window.compiledModels[selectedModel];

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
        scenarioConfigEditorController.syncAll();
        resultFileEditorController?.syncAll();
        } catch (unexpectedError) {
            if (isStaleRun()) return;
            setCompileStatus('Error', '#c9184a');
            updateCompileErrors([{ model: 'live-check', message: String(unexpectedError?.message || unexpectedError) }]);
        } finally {
            liveCheckInFlight = false;
            if (liveCheckRerunRequested) {
                liveCheckRerunRequested = false;
                runLiveChecks();
            }
        }
    }, 1200);
    window.triggerCompileNow = () => {
        if (!isModelicaDocumentPath(activeEditorDocumentPath())) {
            invalidateModelicaLiveChecks();
            return;
        }
        runLiveChecks(true);
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
            const isModelicaDocument = isModelicaDocumentPath(activePath);
            const compileKey = isModelicaDocument
                ? liveCheckCompileKey(paneEditor.getValue(), activePath)
                : '';
            const compilerRelevantEdit = isModelicaDocument
                && compileKey !== lastLiveCheckCompileKey;
            if (activePath) {
                workspaceFs.setFile(activePath, paneEditor.getValue());
                if (activeEditorPaneId === paneId) {
                    workspaceFs.activateDocument(activePath);
                }
            }
            updateSourceBreadcrumbs();
            scheduleWorkspacePersistence();
            if (compilerRelevantEdit) {
                scheduleSemanticTokenRefresh();
                scheduleTypingOutlineRefresh();
            }
            if (isRumocaSmokeMode()) return;
            if (!compilerRelevantEdit) return;
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
            defaultWorkspaceSeed = await loadDefaultWorkspaceSeed();
        } catch (error) {
            console.warn('Failed to load default examples workspace:', error);
        }
        try {
            restored = await restorePersistedWorkspaceIfAvailable();
        } catch (error) {
            console.warn('Failed to restore persisted WASM workspace state:', error);
            await applyImportedWorkspace(buildNewWorkspaceState());
            restored = true;
        }
        if (!restored) {
            await applyImportedWorkspace(buildNewWorkspaceState());
            restored = true;
        }

        workspacePersistenceReady = true;
        scheduleWorkspacePersistence(0);

        if (smokeConfig) {
            void runRumocaBrowserSmoke(smokeConfig).catch(error => {
                console.error('[rumoca-smoke] browser smoke failed:', error);
            });
        }
    })();
});
