import {
    buildWorkspaceArchiveBlob,
    readWorkspaceArchiveEntries,
} from './workspace_fs.js';

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

function blobForContent(content) {
    if (content instanceof Uint8Array) {
        return new Blob([content]);
    }
    if (ArrayBuffer.isView(content)) {
        return new Blob([new Uint8Array(content.buffer, content.byteOffset, content.byteLength)]);
    }
    if (content instanceof ArrayBuffer) {
        return new Blob([content]);
    }
    return new Blob([String(content ?? '')], { type: 'text/plain' });
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
    workspaceFs,
    setTerminalOutput,
    beforeWorkspaceExport,
    onCreateNewWorkspace,
    onLoadExamplesWorkspace,
    onWorkspaceLoaded,
}) {
    async function applyWorkspaceEntries(entries, label) {
        if (!workspaceFs) {
            throw new Error('Workspace filesystem is unavailable.');
        }
        const workspaceState = workspaceFs.loadFileEntries(entries);
        await onWorkspaceLoaded?.(workspaceState);
        setTerminalOutput?.(
            `Loaded ${label} with ${workspaceState.fileCount} file(s).`,
        );
    }

    window.saveFile = function() {
        const content = workspaceFs?.getActiveDocumentContent?.()
            || (getEditor()?.getValue?.() ?? '');
        const filename = basename(workspaceFs?.getActiveDocumentPath?.() || 'Model.mo');
        triggerDownload(blobForContent(content), filename);
    };

    window.downloadWorkspaceFile = function(path) {
        const normalizedPath = String(path || '').trim();
        if (!workspaceFs || !normalizedPath) {
            return;
        }
        const content = workspaceFs.getFileContent(normalizedPath);
        if (content === null || content === undefined) {
            alert(`Could not find ${normalizedPath}.`);
            return;
        }
        triggerDownload(blobForContent(content), basename(normalizedPath));
    };

    window.downloadWorkspace = async function() {
        if (!workspaceFs) {
            alert('Workspace filesystem is unavailable.');
            return;
        }
        await beforeWorkspaceExport?.();
        const editor = getEditor();
        if (editor?.getValue) {
            workspaceFs.updateActiveDocumentContent(editor.getValue());
        }
        const blob = await buildWorkspaceArchiveBlob(workspaceFs);
        triggerDownload(blob, 'rumoca-workspace.zip');
        setTerminalOutput?.(`Workspace exported with ${workspaceFs.listFiles().length} file(s).`);
    };

    window.loadWorkspace = function() {
        const input = document.getElementById('workspaceArchiveInput');
        if (input) {
            input.click();
        }
    };

    window.newWorkspace = async function() {
        try {
            await onCreateNewWorkspace?.();
            setTerminalOutput?.('Created a new in-browser workspace.');
        } catch (error) {
            const message = error?.message || String(error);
            setTerminalOutput?.(`Failed to create new workspace: ${message}`);
            alert(`Failed to create new workspace: ${message}`);
        }
    };

    window.loadExamplesWorkspace = async function() {
        try {
            await onLoadExamplesWorkspace?.();
            setTerminalOutput?.('Loaded the bundled examples workspace.');
        } catch (error) {
            const message = error?.message || String(error);
            setTerminalOutput?.(`Failed to load examples workspace: ${message}`);
            alert(`Failed to load examples workspace: ${message}`);
        }
    };

    window.loadWorkspaceArchive = async function(input) {
        const file = input?.files?.[0];
        if (!file) {
            return;
        }
        if (!workspaceFs) {
            alert('Workspace filesystem is unavailable.');
            return;
        }

        try {
            setTerminalOutput?.(`Loading workspace archive ${file.name}...`);
            const entries = await readWorkspaceArchiveEntries(file);
            await applyWorkspaceEntries(entries, `workspace archive ${file.name}`);
        } catch (error) {
            const message = error?.message || String(error);
            setTerminalOutput?.(`Failed to load workspace archive: ${message}`);
            alert(`Failed to load workspace archive: ${message}`);
        } finally {
            input.value = '';
        }
    };

    window.loadWorkspaceFolder = async function() {
        if (typeof window.showDirectoryPicker === 'function') {
            try {
                setTerminalOutput?.('Loading workspace folder...');
                const handle = await window.showDirectoryPicker();
                const entries = await readFolderEntriesFromHandle(handle);
                await applyWorkspaceEntries(entries, `workspace folder ${handle.name}`);
            } catch (error) {
                if (error?.name === 'AbortError') {
                    return;
                }
                const message = error?.message || String(error);
                setTerminalOutput?.(`Failed to load workspace folder: ${message}`);
                alert(`Failed to load workspace folder: ${message}`);
            }
            return;
        }

        const input = document.getElementById('workspaceFolderInput');
        if (input) {
            input.click();
        }
    };

    window.loadWorkspaceFolderInput = async function(input) {
        try {
            setTerminalOutput?.('Loading workspace folder...');
            const entries = await readFolderEntriesFromInput(input);
            await applyWorkspaceEntries(entries, 'workspace folder import');
        } catch (error) {
            const message = error?.message || String(error);
            setTerminalOutput?.(`Failed to load workspace folder: ${message}`);
            alert(`Failed to load workspace folder: ${message}`);
        } finally {
            if (input) {
                input.value = '';
            }
        }
    };

}
