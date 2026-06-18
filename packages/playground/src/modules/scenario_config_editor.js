import * as shared from '../../vendor/visualization_shared.js';

function trimMaybeString(value) {
    return typeof value === 'string' ? value.trim() : '';
}

function escapeHtml(value) {
    return String(value ?? '').replace(/[&<>"']/g, (char) => ({
        '&': '&amp;',
        '<': '&lt;',
        '>': '&gt;',
        '"': '&quot;',
        "'": '&#39;',
    }[char]));
}

function scenarioShellThemeCss(theme) {
    if (theme === 'light' || theme === 'rust') {
        return 'body { color: #0f172a; background: #ffffff; } .path, .status { color: #475569; } .error { color: #b91c1c; } button { border-color: #cbd5e1; color: #0f172a; }';
    }
    return 'body { color: #d4d4d4; background: #1e1e1e; } .path, .status { color: #9da5b4; } .error { color: #f48771; } button { border-color: #3c3c3c; color: #d4d4d4; }';
}

function buildScenarioConfigErrorDocument(path, error, theme = '') {
    const stateJson = JSON.stringify({ path });
    return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Rumoca Config: ${escapeHtml(path)}</title>
  <style>
    body { margin: 0; font-family: system-ui, sans-serif; }
    ${scenarioShellThemeCss(theme)}
    .page { padding: 16px; display: grid; gap: 12px; }
    .header { display: flex; align-items: center; justify-content: space-between; gap: 12px; }
    h1 { margin: 0; font-size: 16px; }
    .path { font-size: 12px; }
    .error { white-space: pre-wrap; }
    button { font: inherit; padding: 6px 12px; border-radius: 6px; border: 1px solid; background: transparent; cursor: pointer; }
  </style>
</head>
<body>
  <div class="page">
    <div class="header">
      <div>
        <h1>Scenario config</h1>
        <div class="path">${escapeHtml(path)}</div>
      </div>
      <button id="rawBtn">Raw TOML</button>
    </div>
    <div class="error">${escapeHtml(error?.message || error || 'Unable to load scenario config')}</div>
  </div>
  <script>
    const state = ${stateJson};
    document.getElementById('rawBtn').addEventListener('click', () => {
      const host = globalThis.parent && globalThis.parent.RumocaScenarioConfigHost;
      if (host && typeof host.request === 'function') {
        host.request('toggleRaw', { path: state.path }).catch(() => {});
      }
    });
  </script>
</body>
</html>`;
}

function buildScenarioConfigLoadingDocument(path, theme = '') {
    const stateJson = JSON.stringify({ path });
    return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Rumoca Config: ${escapeHtml(path)}</title>
  <style>
    body { margin: 0; font-family: system-ui, sans-serif; }
    ${scenarioShellThemeCss(theme)}
    .page { min-height: 100vh; padding: 16px; display: grid; gap: 12px; align-content: start; }
    .header { display: flex; align-items: center; justify-content: space-between; gap: 12px; }
    h1 { margin: 0; font-size: 16px; }
    .path, .status { font-size: 12px; }
    button { font: inherit; padding: 6px 12px; border-radius: 6px; border: 1px solid; background: transparent; cursor: pointer; }
  </style>
</head>
<body>
  <div class="page">
    <div class="header">
      <div>
        <h1>Scenario config</h1>
        <div class="path">${escapeHtml(path)}</div>
      </div>
      <button id="rawBtn">Raw TOML</button>
    </div>
    <div class="status">Loading typed scenario controls...</div>
  </div>
  <script>
    const state = ${stateJson};
    document.getElementById('rawBtn').addEventListener('click', () => {
      const host = globalThis.parent && globalThis.parent.RumocaScenarioConfigHost;
      if (host && typeof host.request === 'function') {
        host.request('toggleRaw', { path: state.path }).catch(() => {});
      }
    });
  </script>
</body>
</html>`;
}

function scenarioConfigTimeout(ms) {
    return new Promise((_, reject) => {
        setTimeout(() => reject(new Error('Scenario config rendering timed out. Switch to Raw TOML and try again once the runtime is ready.')), ms);
    });
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

function parentDirectory(path) {
    const normalized = normalizeWorkspacePath(path);
    const slash = normalized.lastIndexOf('/');
    return slash === -1 ? '' : normalized.slice(0, slash);
}

function resolveWorkspaceRelativePath(basePath, relativePath) {
    const relative = trimMaybeString(relativePath).replace(/\\/g, '/');
    if (!relative) {
        return '';
    }
    if (relative.startsWith('/')) {
        return normalizeWorkspacePath(relative);
    }
    const baseDir = parentDirectory(basePath);
    const parts = `${baseDir ? `${baseDir}/` : ''}${relative}`.split('/');
    const stack = [];
    for (const part of parts) {
        if (!part || part === '.') {
            continue;
        }
        if (part === '..') {
            stack.pop();
        } else {
            stack.push(part);
        }
    }
    return stack.join('/');
}

async function loadParameterMetadata({ activePath, scenario, workspaceFs, scenarioInterface }) {
    const config = scenario && typeof scenario === 'object' ? scenario.config || {} : {};
    const modelName = trimMaybeString(config.model?.name);
    const modelFile = trimMaybeString(config.model?.file);
    const modelPath = modelFile ? resolveWorkspaceRelativePath(activePath, modelFile) : '';
    const source = modelPath ? workspaceFs.getFileContent(modelPath) : null;
    if (!modelName || typeof source !== 'string') {
        return [];
    }
    try {
        const metadata = await scenarioInterface.execute('rumoca.model.parameterMetadata', {
            path: activePath,
            source,
            modelName,
            fallback: {
                sourceRootPaths: Array.isArray(scenario?.effectiveSourceRootPaths)
                    ? scenario.effectiveSourceRootPaths
                    : [],
            },
            timeoutMs: 8000,
        });
        return Array.isArray(metadata) ? metadata : [];
    } catch (_error) {
        return [];
    }
}

export function createScenarioConfigEditorController({
    workspaceFs,
    scenarioInterface,
    getEditorPanes,
    getActiveEditorPaneId,
    applyEditorLanguage,
    withWorkspaceObserversSuspended,
    renderExplorerPane,
    scheduleWorkspacePersistence,
    runScenarioPath,
    shouldUseOtherCustomEditor,
}) {
    const rawEditorPaths = new Set();
    const renderVersions = new Map();

    function panes() {
        return getEditorPanes?.() || {};
    }

    function frameForPane(pane) {
        return pane?.scenarioFrameElId ? document.getElementById(pane.scenarioFrameElId) : null;
    }

    function shouldUseScenarioEditor(pane) {
        const activePath = trimMaybeString(pane?.activePath);
        return shared.isRumocaScenarioPath(activePath) && !rawEditorPaths.has(activePath);
    }

    function refreshOpenEditorContent(path) {
        const nextPath = trimMaybeString(path);
        const nextContent = workspaceFs.getFileContent(nextPath);
        if (!nextPath || typeof nextContent !== 'string') {
            return;
        }
        for (const pane of Object.values(panes())) {
            if (pane?.activePath !== nextPath || !pane.editor?.getValue) {
                continue;
            }
            if (pane.editor.getValue() === nextContent) {
                continue;
            }
            withWorkspaceObserversSuspended(() => {
                pane.editor.setValue(nextContent);
                applyEditorLanguage(pane.id, nextPath);
            });
        }
    }

    async function renderPane(pane) {
        const frame = frameForPane(pane);
        const activePath = trimMaybeString(pane?.activePath);
        if (!frame || !shouldUseScenarioEditor(pane)) {
            return;
        }
        const renderVersion = (renderVersions.get(pane.id) || 0) + 1;
        renderVersions.set(pane.id, renderVersion);
        const theme = document.body?.dataset?.theme || '';
        if (!frame.srcdoc || frame.dataset.renderPath !== activePath) {
            frame.srcdoc = buildScenarioConfigLoadingDocument(activePath, theme);
        }
        frame.dataset.renderPath = activePath;
        try {
            const scenario = await Promise.race([
                scenarioInterface.execute('rumoca.scenario.getScenarioConfigFull', {
                    path: activePath,
                }),
                scenarioConfigTimeout(8000),
            ]);
            if (renderVersions.get(pane.id) !== renderVersion || pane.activePath !== activePath) {
                return;
            }
            const parameterMetadata = await loadParameterMetadata({
                activePath,
                scenario,
                workspaceFs,
                scenarioInterface,
            });
            if (renderVersions.get(pane.id) !== renderVersion || pane.activePath !== activePath) {
                return;
            }
            frame.srcdoc = shared.buildScenarioConfigDocument({
                ...(scenario && typeof scenario === 'object' ? scenario : {}),
                path: activePath,
                parameterMetadata,
                theme,
            });
        } catch (error) {
            if (renderVersions.get(pane.id) === renderVersion && pane.activePath === activePath) {
                frame.srcdoc = buildScenarioConfigErrorDocument(activePath, error, theme);
            }
        }
    }

    function syncPane(paneId) {
        const pane = panes()[paneId];
        if (!pane) {
            return;
        }
        const editorEl = document.getElementById(pane.editorElId);
        const frame = frameForPane(pane);
        const isEmpty = pane.paths.length === 0 || !trimMaybeString(pane.activePath);
        const useScenarioEditor = !isEmpty && shouldUseScenarioEditor(pane);
        const useOtherCustomEditor = !isEmpty
            && !useScenarioEditor
            && typeof shouldUseOtherCustomEditor === 'function'
            && shouldUseOtherCustomEditor(pane);
        if (editorEl) {
            editorEl.hidden = isEmpty || useScenarioEditor || useOtherCustomEditor;
        }
        if (frame) {
            frame.hidden = isEmpty || !useScenarioEditor;
        }
        pane.stackEl?.classList.toggle('scenario-config-active', useScenarioEditor);
        if (useScenarioEditor) {
            void renderPane(pane);
        }
    }

    function syncAll() {
        for (const paneId of Object.keys(panes())) {
            syncPane(paneId);
        }
    }

    async function save(path, edits) {
        const scenarioPath = trimMaybeString(path);
        if (!shared.isRumocaScenarioPath(scenarioPath)) {
            throw new Error('Scenario config path is required.');
        }
        const latest = await scenarioInterface.execute('rumoca.scenario.getScenarioConfigFull', {
            path: scenarioPath,
        });
        const latestConfig = latest?.config && typeof latest.config === 'object' ? latest.config : {};
        const config = shared.applyScenarioConfigEdits(latestConfig, edits);
        const response = await scenarioInterface.execute('rumoca.scenario.setScenarioConfig', {
            path: scenarioPath,
            config,
        });
        refreshOpenEditorContent(scenarioPath);
        syncAll();
        renderExplorerPane?.();
        scheduleWorkspacePersistence?.();
        return response;
    }

    function toggleRaw(path) {
        const scenarioPath = trimMaybeString(path);
        if (!shared.isRumocaScenarioPath(scenarioPath)) {
            return { ok: true };
        }
        rawEditorPaths.add(scenarioPath);
        refreshOpenEditorContent(scenarioPath);
        syncAll();
        const activePane = panes()[getActiveEditorPaneId?.()];
        if (activePane?.activePath === scenarioPath && activePane.editor?.focus) {
            activePane.editor.focus();
        }
        return { ok: true };
    }

    function showGui(path) {
        const scenarioPath = trimMaybeString(path);
        if (!shared.isRumocaScenarioPath(scenarioPath)) {
            return false;
        }
        rawEditorPaths.delete(scenarioPath);
        syncAll();
        return true;
    }

    async function request(method, payload = {}) {
        if (method === 'save') {
            return await save(payload?.path, payload?.edits);
        }
        if (method === 'run') {
            await save(payload?.path, payload?.edits);
            if (typeof runScenarioPath === 'function') {
                const result = await runScenarioPath(payload?.path);
                return {
                    ok: true,
                    message: result?.message || 'Scenario run completed',
                    ...(result || {}),
                };
            }
            return { ok: true, message: 'Scenario saved' };
        }
        if (method === 'toggleRaw') {
            return toggleRaw(payload?.path);
        }
        throw new Error(`Unknown scenario config request: ${method}`);
    }

    globalThis.RumocaScenarioConfigHost = { request };

    return {
        closePath(path) {
            rawEditorPaths.delete(trimMaybeString(path));
        },
        isScenarioPath: shared.isRumocaScenarioPath,
        shouldUseScenarioEditor,
        showGui,
        syncAll,
        syncPane,
    };
}
