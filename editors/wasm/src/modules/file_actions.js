import {
    buildProjectArchiveBlob,
    readProjectArchiveEntries,
} from './project_fs.js';

function triggerDownload(blob, filename) {
    const url = URL.createObjectURL(blob);
    const anchor = document.createElement('a');
    anchor.href = url;
    anchor.download = filename;
    document.body.appendChild(anchor);
    anchor.click();
    document.body.removeChild(anchor);
    URL.revokeObjectURL(url);
}

function basename(path) {
    return String(path || '').split('/').filter(Boolean).pop() || 'Model.mo';
}

async function readTextFile(file) {
    return await file.text();
}

function normalizedRelativePath(path) {
    return String(path || '')
        .replace(/\\/g, '/')
        .split('/')
        .filter(Boolean)
        .join('/');
}

async function readFolderEntriesFromInput(input) {
    const files = Array.from(input?.files || []);
    if (files.length === 0) {
        throw new Error('No folder selected');
    }

    const entries = [];
    for (const file of files) {
        const path = normalizedRelativePath(file.webkitRelativePath || file.name);
        if (!path) continue;
        entries.push({
            path,
            content: await readTextFile(file),
        });
    }
    return entries.sort((lhs, rhs) => lhs.path.localeCompare(rhs.path));
}

async function readFolderEntriesFromHandle(handle, prefix = '') {
    const entries = [];
    for await (const entry of handle.values()) {
        const nextPath = prefix ? `${prefix}/${entry.name}` : entry.name;
        if (entry.kind === 'directory') {
            entries.push(...await readFolderEntriesFromHandle(entry, nextPath));
            continue;
        }
        const file = await entry.getFile();
        entries.push({
            path: normalizedRelativePath(nextPath),
            content: await readTextFile(file),
        });
    }
    return entries.sort((lhs, rhs) => lhs.path.localeCompare(rhs.path));
}

export function installFileActions({
    getEditor,
    getCompiledModels,
    getCodegenOutputEditor,
    projectFs,
    setTerminalOutput,
    beforeProjectExport,
    onCreateNewProject,
    onProjectLoaded,
}) {
    async function applyProjectEntries(entries, label) {
        if (!projectFs) {
            throw new Error('Project filesystem is unavailable.');
        }
        const projectState = projectFs.loadFileEntries(entries);
        await onProjectLoaded?.(projectState);
        setTerminalOutput?.(
            `Loaded ${label} with ${projectState.fileCount} file(s).`,
        );
    }

    window.saveFile = function() {
        const content = projectFs?.getActiveDocumentContent?.()
            || (getEditor()?.getValue?.() ?? '');
        const filename = basename(projectFs?.getActiveDocumentPath?.() || 'Model.mo');
        triggerDownload(new Blob([content], { type: 'text/plain' }), filename);
    };

    window.downloadProject = async function() {
        if (!projectFs) {
            alert('Project filesystem is unavailable.');
            return;
        }
        await beforeProjectExport?.();
        const editor = getEditor();
        if (editor?.getValue) {
            projectFs.updateActiveDocumentContent(editor.getValue());
        }
        const blob = await buildProjectArchiveBlob(projectFs);
        triggerDownload(blob, 'rumoca-project.zip');
        setTerminalOutput?.(`Project exported with ${projectFs.listFiles().length} file(s).`);
    };

    window.loadProject = function() {
        const input = document.getElementById('projectArchiveInput');
        if (input) {
            input.click();
        }
    };

    window.newProject = async function() {
        try {
            await onCreateNewProject?.();
            setTerminalOutput?.('Created a new in-browser project.');
        } catch (error) {
            const message = error?.message || String(error);
            setTerminalOutput?.(`Failed to create new project: ${message}`);
            alert(`Failed to create new project: ${message}`);
        }
    };

    window.loadProjectArchive = async function(input) {
        const file = input?.files?.[0];
        if (!file) {
            return;
        }
        if (!projectFs) {
            alert('Project filesystem is unavailable.');
            return;
        }

        try {
            setTerminalOutput?.(`Loading project archive ${file.name}...`);
            const entries = await readProjectArchiveEntries(file);
            await applyProjectEntries(entries, `project archive ${file.name}`);
        } catch (error) {
            const message = error?.message || String(error);
            setTerminalOutput?.(`Failed to load project archive: ${message}`);
            alert(`Failed to load project archive: ${message}`);
        } finally {
            input.value = '';
        }
    };

    window.loadProjectFolder = async function() {
        if (typeof window.showDirectoryPicker === 'function') {
            try {
                setTerminalOutput?.('Loading project folder...');
                const handle = await window.showDirectoryPicker();
                const entries = await readFolderEntriesFromHandle(handle);
                await applyProjectEntries(entries, `project folder ${handle.name}`);
            } catch (error) {
                if (error?.name === 'AbortError') {
                    return;
                }
                const message = error?.message || String(error);
                setTerminalOutput?.(`Failed to load project folder: ${message}`);
                alert(`Failed to load project folder: ${message}`);
            }
            return;
        }

        const input = document.getElementById('projectFolderInput');
        if (input) {
            input.click();
        }
    };

    window.loadProjectFolderInput = async function(input) {
        try {
            setTerminalOutput?.('Loading project folder...');
            const entries = await readFolderEntriesFromInput(input);
            await applyProjectEntries(entries, 'project folder import');
        } catch (error) {
            const message = error?.message || String(error);
            setTerminalOutput?.(`Failed to load project folder: ${message}`);
            alert(`Failed to load project folder: ${message}`);
        } finally {
            if (input) {
                input.value = '';
            }
        }
    };

    window.saveCodegenOutput = function() {
        const modelName = document.getElementById('modelSelect').value;
        const compiledModels = getCompiledModels();
        const codegenOutputEditor = getCodegenOutputEditor();
        const content = codegenOutputEditor && codegenOutputEditor.getValue
            ? codegenOutputEditor.getValue()
            : '';
        if (!modelName || !compiledModels[modelName]) {
            alert('No code generation output available yet. Choose a model and wait for compilation.');
            return;
        }
        if (!content || content.startsWith('No model selected') || content.startsWith('No DAE')) {
            alert('No valid codegen output to save.');
            return;
        }
        const filename = `${modelName || 'output'}.txt`;
        triggerDownload(new Blob([content], { type: 'text/plain' }), filename);
    };
}
