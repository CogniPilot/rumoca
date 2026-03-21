const PROJECT_METADATA_PATH = '.rumoca/project.json';
const PROJECT_EDITOR_STATE_PATH = '.rumoca/editor-state.json';
const PROJECT_CACHE_PREFIX = '.rumoca/cache/';
const PROJECT_SCHEMA_VERSION = 1;
const PROJECT_EDITOR_STATE_VERSION = 1;
const PROJECT_TEXT_FILE_EXTENSIONS = new Set([
    '.mo',
    '.m',
    '.ts',
    '.js',
    '.json',
    '.toml',
    '.txt',
    '.md',
    '.yml',
    '.yaml',
    '.html',
    '.css',
    '.svg',
    '.csv',
    '.log',
    '.in',
    '.out',
    '.c',
    '.h',
    '.cpp',
    '.hpp',
    '.rs',
]);
const PROJECT_BINARY_FILE_EXTENSIONS = new Set([
    '.bin',
    '.cache',
    '.wasm',
    '.dat',
    '.dll',
    '.so',
    '.dylib',
    '.png',
    '.jpg',
    '.jpeg',
    '.gif',
    '.webp',
    '.zip',
    '.gz',
    '.xz',
    '.bz2',
    '.7z',
    '.npy',
    '.npz',
]);
const TEXT_DECODER = typeof TextDecoder !== 'undefined'
    ? new TextDecoder('utf-8', { fatal: false })
    : null;
const UTF8_DECODER = {
    decode(value) {
        if (TEXT_DECODER) {
            return TEXT_DECODER.decode(value);
        }
        if (value && typeof Buffer !== 'undefined' && typeof Buffer.from === 'function') {
            return Buffer.from(value).toString('utf8');
        }
        return String(value || '');
    },
};

function fileExtension(path) {
    const text = String(path || '');
    const dot = text.lastIndexOf('.');
    if (dot === -1) {
        return '';
    }
    return text.slice(dot).toLowerCase();
}

function fileRecordContentFromRaw(content) {
    if (content === null || content === undefined) {
        return '';
    }
    if (typeof content === 'string') {
        return content;
    }
    if (content instanceof Uint8Array) {
        return content;
    }
    if (ArrayBuffer.isView(content)) {
        return new Uint8Array(content.buffer, content.byteOffset, content.byteLength);
    }
    if (content instanceof ArrayBuffer) {
        return new Uint8Array(content);
    }
    return String(content);
}

function isBinaryLike(content) {
    if (!(content instanceof Uint8Array)) {
        return false;
    }
    let controlCount = 0;
    const sampleSize = Math.min(1024, content.length);
    for (let index = 0; index < sampleSize; index += 1) {
        const byte = content[index];
        if (byte === 0 || (byte < 9 || (byte > 13 && byte < 32))) {
            controlCount += 1;
        }
    }
    return controlCount > Math.max(1, Math.floor(sampleSize * 0.2));
}

function normalizeArchiveFileContent(path, content) {
    const resolved = fileRecordContentFromRaw(content);
    if (typeof resolved === 'string') {
        return resolved;
    }
    const extension = fileExtension(path);
    if (PROJECT_BINARY_FILE_EXTENSIONS.has(extension)) {
        return resolved;
    }
    if (PROJECT_TEXT_FILE_EXTENSIONS.has(extension)) {
        return UTF8_DECODER.decode(resolved);
    }
    if (!isBinaryLike(resolved)) {
        return UTF8_DECODER.decode(resolved);
    }
    return resolved;
}

function normalizeTextContent(content) {
    return typeof content === 'string' ? content : '';
}

export function normalizePath(path) {
    const trimmed = String(path || '')
        .replace(/\\/g, '/')
        .replace(/^\/+/, '')
        .replace(/\/+/g, '/')
        .trim();
    if (!trimmed || trimmed === '.') {
        return '';
    }
    return trimmed
        .split('/')
        .filter((part) => part && part !== '.')
        .join('/');
}

function isProjectMetadataPath(path) {
    return normalizePath(path) === PROJECT_METADATA_PATH;
}

function isProjectEditorStatePath(path) {
    return normalizePath(path) === PROJECT_EDITOR_STATE_PATH;
}

function isManagedProjectPath(path) {
    return isProjectMetadataPath(path) || isProjectEditorStatePath(path);
}

function isSidecarPath(path) {
    const normalized = normalizePath(path);
    return normalized.startsWith('.rumoca/');
}

function isCachePath(path) {
    return normalizePath(path).startsWith(PROJECT_CACHE_PREFIX);
}

function fileRecord(content, sourceKind, archiveId = null) {
    return {
        content: fileRecordContentFromRaw(content),
        sourceKind,
        archiveId,
    };
}

function firstMatchingMo(entries, excludedPaths = new Set()) {
    for (const [path] of entries) {
        if (excludedPaths.has(path)) continue;
        if (path.endsWith('.mo')) {
            return path;
        }
    }
    return null;
}

export function inferModelicaFileName(source, fallback = 'Main.mo') {
    const match = String(source || '').match(
        /(?:model|class|block|connector|record|function)\s+([A-Za-z_][A-Za-z0-9_]*)/,
    );
    if (!match?.[1]) {
        return fallback;
    }
    return `${match[1]}.mo`;
}

function cloneJson(value) {
    if (value === null || value === undefined) {
        return null;
    }
    return JSON.parse(JSON.stringify(value));
}

function serializeProjectMetadata(activeDocumentPath, packageArchives) {
    return `${JSON.stringify({
        schemaVersion: PROJECT_SCHEMA_VERSION,
        activeDocument: activeDocumentPath,
        packageArchives: packageArchives.map((archive) => ({
            archiveId: archive.archiveId,
            fileName: archive.fileName,
            fileCount: archive.fileCount,
            paths: archive.paths,
        })),
        folders: [],
    }, null, 2)}\n`;
}

function serializeEditorState(editorState) {
    if (!editorState || typeof editorState !== 'object') {
        return null;
    }
    return `${JSON.stringify({
        schemaVersion: PROJECT_EDITOR_STATE_VERSION,
        ...editorState,
    }, null, 2)}\n`;
}

function parseProjectMetadata(raw) {
    if (!raw) {
        return null;
    }
    try {
        const parsed = JSON.parse(raw);
        if (!parsed || typeof parsed !== 'object') {
            return null;
        }
        return parsed;
    } catch {
        return null;
    }
}

function parseEditorState(raw) {
    if (!raw) {
        return null;
    }
    try {
        const parsed = JSON.parse(raw);
        if (!parsed || typeof parsed !== 'object') {
            return null;
        }
        return parsed;
    } catch {
        return null;
    }
}

export function createProjectFilesystem() {
    const files = new Map();
    const cacheFiles = new Map();
    const packageArchives = new Map();
    const folders = new Set();
    let activeDocumentPath = 'Main.mo';
    let editorState = null;

    function prunePackageArchivesForPaths(removedPaths) {
        if (!Array.isArray(removedPaths) || removedPaths.length === 0) {
            return;
        }
        const removed = new Set(
            removedPaths
                .map((path) => normalizePath(path))
                .filter(Boolean),
        );
        for (const [archiveId, archive] of Array.from(packageArchives.entries())) {
            const nextPaths = archive.paths.filter((path) => !removed.has(path));
            if (nextPaths.length === archive.paths.length) {
                continue;
            }
            if (nextPaths.length === 0) {
                packageArchives.delete(archiveId);
                continue;
            }
            packageArchives.set(archiveId, {
                ...archive,
                fileCount: nextPaths.length,
                paths: nextPaths,
            });
        }
    }

    function ensureParentFolders(path) {
        const parts = normalizePath(path).split('/').filter(Boolean);
        let prefix = '';
        for (let index = 0; index < Math.max(0, parts.length - 1); index += 1) {
            prefix = prefix ? `${prefix}/${parts[index]}` : parts[index];
            if (prefix && !isManagedProjectPath(prefix) && !isSidecarPath(prefix)) {
                folders.add(prefix);
            }
        }
    }

    function listFolders() {
        return Array.from(folders).sort((lhs, rhs) => lhs.localeCompare(rhs));
    }

    function listPackageArchives() {
        return Array.from(packageArchives.values()).map((archive) => ({
            archiveId: archive.archiveId,
            fileName: archive.fileName,
            fileCount: archive.fileCount,
            paths: [...archive.paths],
        }));
    }

    function setActiveDocument(path, content) {
        const normalizedPath = normalizePath(path) || 'Main.mo';
        const existing = files.get(normalizedPath);
        activeDocumentPath = normalizedPath;
        ensureParentFolders(normalizedPath);
        files.set(
            normalizedPath,
            fileRecord(
                content,
                existing?.sourceKind || 'workspace',
                existing?.archiveId || null,
            ),
        );
        return normalizedPath;
    }

    function activateDocument(path) {
        const normalizedPath = normalizePath(path) || 'Main.mo';
        activeDocumentPath = normalizedPath;
        if (!files.has(normalizedPath)) {
            ensureParentFolders(normalizedPath);
            files.set(normalizedPath, fileRecord('', 'workspace'));
        }
        return normalizedPath;
    }

    function updateActiveDocumentContent(content) {
        setActiveDocument(activeDocumentPath, content);
    }

    function ensureActiveDocumentFromSource(source, fallback = 'Main.mo') {
        if (!files.has(activeDocumentPath)) {
            setActiveDocument(inferModelicaFileName(source, fallback), source);
            return;
        }
        updateActiveDocumentContent(source);
    }

    function replacePackageArchive(archiveId, fileName, archiveFiles) {
        const normalizedArchiveId = String(archiveId || '').trim();
        if (!normalizedArchiveId) {
            throw new Error('package archive id is required');
        }

        removePackageArchive(normalizedArchiveId);

        const normalizedEntries = Object.entries(archiveFiles || {})
            .map((
                [path, content],
            ) => [
                normalizePath(path),
                normalizeArchiveFileContent(path, content),
            ])
            .filter(([path]) => Boolean(path))
            .sort(([lhs], [rhs]) => lhs.localeCompare(rhs));

        for (const [path, content] of normalizedEntries) {
            files.set(
                path,
                fileRecord(content, 'packageArchive', normalizedArchiveId),
            );
        }
        const paths = normalizedEntries.map(([path]) => path);

        packageArchives.set(normalizedArchiveId, {
            archiveId: normalizedArchiveId,
            fileName: String(fileName || normalizedArchiveId),
            fileCount: paths.length,
            paths,
        });
    }

    function removePackageArchive(archiveId) {
        const normalizedArchiveId = String(archiveId || '').trim();
        if (!normalizedArchiveId) return;
        const archive = packageArchives.get(normalizedArchiveId);
        if (archive) {
            for (const path of archive.paths) {
                files.delete(path);
            }
        }
        packageArchives.delete(normalizedArchiveId);
    }

    function clearPackageArchives() {
        for (const archiveId of Array.from(packageArchives.keys())) {
            removePackageArchive(archiveId);
        }
    }

    function clearProject() {
        files.clear();
        cacheFiles.clear();
        packageArchives.clear();
        folders.clear();
        activeDocumentPath = 'Main.mo';
        editorState = null;
    }

    function setCacheFile(path, content) {
        const normalizedPath = normalizePath(path);
        if (!normalizedPath || !isCachePath(normalizedPath)) {
            throw new Error('cache file path is required');
        }
        cacheFiles.set(normalizedPath, fileRecordContentFromRaw(content));
        return normalizedPath;
    }

    function getCacheFile(path) {
        const normalizedPath = normalizePath(path);
        if (!normalizedPath || !isCachePath(normalizedPath)) {
            return null;
        }
        return cacheFiles.get(normalizedPath) ?? null;
    }

    function removeCacheFile(path) {
        const normalizedPath = normalizePath(path);
        if (!normalizedPath || !isCachePath(normalizedPath)) {
            return false;
        }
        return cacheFiles.delete(normalizedPath);
    }

    function getPackageArchiveFiles(archiveId) {
        const archive = packageArchives.get(String(archiveId || '').trim());
        if (!archive) {
            return {};
        }
        const result = {};
        for (const path of archive.paths) {
            const record = files.get(path);
            if (record) {
                result[path] = record.content;
            }
        }
        return result;
    }

    function setFile(path, content) {
        const normalizedPath = normalizePath(path);
        if (!normalizedPath) {
            throw new Error('file path is required');
        }
        if (isManagedProjectPath(normalizedPath)) {
            throw new Error(`cannot overwrite managed project file: ${normalizedPath}`);
        }
        ensureParentFolders(normalizedPath);
        files.set(
            normalizedPath,
            fileRecord(content, isSidecarPath(normalizedPath) ? 'sidecar' : 'workspace'),
        );
        if (normalizedPath === activeDocumentPath && isSidecarPath(normalizedPath)) {
            activeDocumentPath = 'Main.mo';
        }
        return normalizedPath;
    }

    function setFolder(path) {
        const normalizedPath = normalizePath(path);
        if (!normalizedPath) {
            throw new Error('folder path is required');
        }
        if (isManagedProjectPath(normalizedPath) || isSidecarPath(normalizedPath)) {
            throw new Error(`cannot create managed folder: ${normalizedPath}`);
        }
        const parts = normalizedPath.split('/');
        let prefix = '';
        for (const part of parts) {
            prefix = prefix ? `${prefix}/${part}` : part;
            folders.add(prefix);
        }
        return normalizedPath;
    }

    function chooseFallbackActiveDocument() {
        const entries = Array.from(files.entries())
            .filter(([, record]) => record.sourceKind === 'workspace');
        return (
            firstMatchingMo(entries)
            || entries[0]?.[0]
            || 'Main.mo'
        );
    }

    function removeFolder(path) {
        const normalizedPath = normalizePath(path);
        if (!normalizedPath || isManagedProjectPath(normalizedPath)) {
            return false;
        }
        const prefix = `${normalizedPath}/`;
        let removed = false;
        const removedPaths = [];
        for (const filePath of Array.from(files.keys())) {
            if (filePath === normalizedPath || filePath.startsWith(prefix)) {
                files.delete(filePath);
                removedPaths.push(filePath);
                removed = true;
            }
        }
        prunePackageArchivesForPaths(removedPaths);
        for (const folderPath of Array.from(folders)) {
            if (folderPath === normalizedPath || folderPath.startsWith(prefix)) {
                folders.delete(folderPath);
                removed = true;
            }
        }
        if (activeDocumentPath === normalizedPath || activeDocumentPath.startsWith(prefix)) {
            activeDocumentPath = chooseFallbackActiveDocument();
            if (!files.has(activeDocumentPath)) {
                files.set(activeDocumentPath, fileRecord('', 'workspace'));
            }
        }
        return removed;
    }

    function removeFile(path) {
        const normalizedPath = normalizePath(path);
        if (!normalizedPath || isManagedProjectPath(normalizedPath)) {
            return false;
        }
        const removed = files.delete(normalizedPath);
        if (removed) {
            prunePackageArchivesForPaths([normalizedPath]);
        }
        if (normalizedPath === activeDocumentPath) {
            activeDocumentPath = chooseFallbackActiveDocument();
            if (!files.has(activeDocumentPath)) {
                files.set(activeDocumentPath, fileRecord('', 'workspace'));
            }
        }
        return removed;
    }

    function getFileContent(path) {
        const normalizedPath = normalizePath(path);
        if (!normalizedPath) {
            return null;
        }
        return files.get(normalizedPath)?.content ?? null;
    }

    function listFiles() {
        return Array.from(files.entries())
            .sort(([lhs], [rhs]) => lhs.localeCompare(rhs))
            .map(([path, record]) => ({
                path,
                content: record.content,
                sourceKind: record.sourceKind,
                archiveId: record.archiveId,
            }));
    }

    function getFileEntry(path) {
        const normalizedPath = normalizePath(path);
        if (!normalizedPath) {
            return null;
        }
        const record = files.get(normalizedPath);
        if (!record) {
            return null;
        }
        return {
            path: normalizedPath,
            sourceKind: record.sourceKind,
            archiveId: record.archiveId,
            isText: typeof record.content === 'string',
        };
    }

    function listFileEntries() {
        return Array.from(files.entries())
            .sort(([lhs], [rhs]) => lhs.localeCompare(rhs))
            .map(([path, record]) => ({
                path,
                sourceKind: record.sourceKind,
                archiveId: record.archiveId,
                isText: typeof record.content === 'string',
            }));
    }

    function snapshotArchiveEntries({ includeCacheFiles = false } = {}) {
        const entries = listFiles().map(({ path, content }) => ({ path, content }));
        if (includeCacheFiles) {
            for (const [path, content] of Array.from(cacheFiles.entries()).sort(([lhs], [rhs]) => lhs.localeCompare(rhs))) {
                entries.push({ path, content });
            }
        }
        const serializedEditorState = serializeEditorState(editorState);
        const metadataContent = JSON.stringify({
            schemaVersion: PROJECT_SCHEMA_VERSION,
            activeDocument: activeDocumentPath,
            packageArchives: listPackageArchives().map((archive) => ({
                archiveId: archive.archiveId,
                fileName: archive.fileName,
                fileCount: archive.fileCount,
                paths: archive.paths,
            })),
            folders: listFolders(),
        }, null, 2);
        entries.push({
            path: PROJECT_METADATA_PATH,
            content: `${metadataContent}\n`,
        });
        if (serializedEditorState) {
            entries.push({
                path: PROJECT_EDITOR_STATE_PATH,
                content: serializedEditorState,
            });
        }
        return entries;
    }

    function loadFileEntries(entries) {
        clearProject();
        let metadata = null;
        let nextEditorState = null;
        for (const entry of entries || []) {
            const normalizedPath = normalizePath(entry?.path);
            if (!normalizedPath) continue;
            if (isCachePath(normalizedPath)) {
                cacheFiles.set(normalizedPath, fileRecordContentFromRaw(entry?.content));
                continue;
            }
            const content = normalizeArchiveFileContent(normalizedPath, entry?.content);
            if (isProjectMetadataPath(normalizedPath)) {
                metadata = parseProjectMetadata(content);
                continue;
            }
            if (isProjectEditorStatePath(normalizedPath)) {
                nextEditorState = parseEditorState(content);
                continue;
            }
            const sourceKind = isSidecarPath(normalizedPath) ? 'sidecar' : 'workspace';
            if (sourceKind === 'workspace') {
                ensureParentFolders(normalizedPath);
            }
            files.set(normalizedPath, fileRecord(content, sourceKind));
        }

        if (Array.isArray(metadata?.folders)) {
            for (const folderPath of metadata.folders) {
                const normalizedFolderPath = normalizePath(folderPath);
                if (normalizedFolderPath && !isManagedProjectPath(normalizedFolderPath)) {
                    folders.add(normalizedFolderPath);
                }
            }
        }

        if (Array.isArray(metadata?.packageArchives)) {
            for (const packageArchive of metadata.packageArchives) {
                const archiveId = String(packageArchive?.archiveId || '').trim();
                const paths = Array.isArray(packageArchive?.paths)
                    ? packageArchive.paths.map((path) => normalizePath(path)).filter(Boolean)
                    : [];
                if (!archiveId || paths.length === 0) continue;
                for (const path of paths) {
                    const existing = files.get(path);
                    if (!existing) continue;
                    files.set(path, fileRecord(existing.content, 'packageArchive', archiveId));
                }
                packageArchives.set(archiveId, {
                    archiveId,
                    fileName: String(packageArchive?.fileName || archiveId),
                    fileCount: paths.length,
                    paths,
                });
            }
        }

        const excludedPaths = new Set(
            listPackageArchives().flatMap((archive) => archive.paths),
        );
        const preferredActive = normalizePath(metadata?.activeDocument);
        if (preferredActive && files.has(preferredActive)) {
            activeDocumentPath = preferredActive;
        } else {
            const workspaceEntries = Array.from(files.entries())
                .filter(([, record]) => record.sourceKind === 'workspace');
            activeDocumentPath =
                firstMatchingMo(workspaceEntries, excludedPaths)
                || firstMatchingMo(Array.from(files.entries()))
                || 'Main.mo';
        }

        if (!files.has(activeDocumentPath)) {
            files.set(activeDocumentPath, fileRecord('', 'workspace'));
        }
        editorState = nextEditorState;

        return {
            activeDocumentPath,
            activeDocumentContent: normalizeTextContent(
                files.get(activeDocumentPath)?.content || '',
            ),
            editorState: cloneJson(editorState),
            packageArchives: listPackageArchives(),
            fileCount: files.size,
        };
    }

    function loadArchiveEntries(entries) {
        return loadFileEntries(entries);
    }

    function getActiveDocumentPath() {
        return activeDocumentPath;
    }

    function getActiveDocumentContent() {
        return normalizeTextContent(files.get(activeDocumentPath)?.content || '');
    }

    function getEditorState() {
        return cloneJson(editorState);
    }

    function setEditorState(nextState) {
        editorState = cloneJson(nextState);
    }

    return {
        clearProject,
        clearPackageArchives,
        ensureActiveDocumentFromSource,
        getActiveDocumentContent,
        getActiveDocumentPath,
        getCacheFile,
        getFileEntry,
        getFileContent,
        getPackageArchiveFiles,
        getEditorState,
        listFileEntries,
        listFiles,
        listFolders,
        listPackageArchives,
        loadArchiveEntries,
        loadFileEntries,
        removeCacheFile,
        removeFolder,
        removeFile,
        removePackageArchive,
        replacePackageArchive,
        activateDocument,
        setCacheFile,
        setFolder,
        setFile,
        setEditorState,
        setActiveDocument,
        snapshotArchiveEntries,
        updateActiveDocumentContent,
    };
}

export async function readProjectArchiveEntries(file) {
    if (!globalThis.JSZip) {
        throw new Error('JSZip is not available in this environment');
    }
    const bytes = await file.arrayBuffer();
    const zip = await globalThis.JSZip.loadAsync(bytes);
    const entries = [];
    const paths = Object.keys(zip.files).sort((lhs, rhs) => lhs.localeCompare(rhs));
    for (const path of paths) {
        const entry = zip.files[path];
        if (!entry || entry.dir) continue;
        entries.push({
            path,
            content: await entry.async('uint8array'),
        });
    }
    return entries;
}

export async function buildProjectArchiveBlob(projectFs) {
    if (!globalThis.JSZip) {
        throw new Error('JSZip is not available in this environment');
    }
    const zip = new globalThis.JSZip();
    for (const entry of projectFs.snapshotArchiveEntries({ includeCacheFiles: false })) {
        zip.file(entry.path, entry.content);
    }
    return await zip.generateAsync({
        type: 'blob',
        compression: 'DEFLATE',
        compressionOptions: { level: 6 },
    });
}
