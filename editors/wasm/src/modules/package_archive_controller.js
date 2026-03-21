function escapeHtmlSafe(value) {
    return String(value ?? '')
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#39;');
}

export function createPackageArchiveController({
    sendLanguageCommand,
    sendWorkspaceCommand,
    setTerminalOutput,
    isWorkerReady,
    projectFs,
    onPackageArchivesChanged,
}) {
    let stagedSourceRoots = {};
    const loadedPackageArchives = new Map();
    const packageArchiveRowIds = new Map();
    let packageArchiveRowCount = 1;
    let classTreeData = [];
    let classTreeFiltered = [];
    let classTreeExpanded = new Set();
    let selectedClassQualifiedName = null;
    const classInfoCache = new Map();
    let classSourceEditor = null;

    function activeSourceRootActivity(status) {
        return status?.current || status?.lastCompleted || null;
    }

    function sourceRootActivityLabel(activity) {
        switch (activity?.kind) {
            case 'ColdIndexBuild':
                return 'cold index build';
            case 'WarmCacheRestore':
                return 'warm cache restore';
            case 'SubtreeReindex':
                return 'subtree reindex';
            default:
                return 'source root activity';
        }
    }

    function sourceRootActivityPhase(activity) {
        switch (activity?.phase) {
            case 'Pending':
                return 'pending';
            case 'Running':
                return 'running';
            case 'Completed':
                return 'completed';
            default:
                return 'idle';
        }
    }

    function formatSourceRootStatus(status) {
        const activity = activeSourceRootActivity(status);
        if (!activity) {
            return `${status.sourceRootKey || 'source root'}: idle`;
        }
        const scope = Array.isArray(activity.dirtyClassPrefixes) && activity.dirtyClassPrefixes.length > 0
            ? ` for ${activity.dirtyClassPrefixes.join(', ')}`
            : '';
        return `${status.sourceRootKey || 'source root'}: ${sourceRootActivityLabel(activity)} ${sourceRootActivityPhase(activity)}${scope}`;
    }

    async function sourceRootStatusLines() {
        try {
            const resultJson = await sendWorkspaceCommand(
                'rumoca.workspace.getSourceRootStatuses',
                {},
                30000,
            );
            const statuses = JSON.parse(resultJson || '[]');
            if (!Array.isArray(statuses) || statuses.length === 0) {
                return [];
            }
            return statuses.map(formatSourceRootStatus);
        } catch (error) {
            console.warn('Failed to read source root statuses:', error);
            return [];
        }
    }

    async function setTerminalOutputWithSourceRootStatuses(baseOutput) {
        const lines = await sourceRootStatusLines();
        if (lines.length === 0) {
            setTerminalOutput(baseOutput);
            return;
        }
        setTerminalOutput(`${baseOutput}\n\nSource roots:\n${lines.map(line => `  - ${line}`).join('\n')}`);
    }

    function updateClassCountBadge(totalClasses) {
        const badge = document.getElementById('classCount');
        if (!badge) return;
        badge.textContent = `(${totalClasses || 0})`;
    }

    function ensurePackageArchiveProgress(row) {
        let progress = row.querySelector('.package-archive-progress');
        if (!progress) {
            progress = document.createElement('div');
            progress.className = 'package-archive-progress';
            progress.hidden = true;
            progress.innerHTML = `
                <div class="package-archive-progress-track"><div class="package-archive-progress-fill"></div></div>
                <span class="package-archive-progress-text">0%</span>
            `;
            row.appendChild(progress);
        }
        return {
            progress,
            fill: progress.querySelector('.package-archive-progress-fill'),
            text: progress.querySelector('.package-archive-progress-text'),
        };
    }

    function setPackageArchiveProgress(row, percent, label, indeterminate = false) {
        if (!row) return;
        const { progress, fill, text } = ensurePackageArchiveProgress(row);
        progress.hidden = false;
        progress.classList.toggle('indeterminate', !!indeterminate);
        if (fill) {
            const clamped = Math.max(0, Math.min(Number(percent) || 0, 100));
            fill.style.width = `${clamped}%`;
        }
        if (text) {
            text.textContent = label || `${Math.max(0, Math.min(Number(percent) || 0, 100)).toFixed(0)}%`;
        }
    }

    function hidePackageArchiveProgress(row) {
        if (!row) return;
        const { progress, fill, text } = ensurePackageArchiveProgress(row);
        progress.hidden = true;
        progress.classList.remove('indeterminate');
        if (fill) fill.style.width = '0%';
        if (text) text.textContent = '0%';
    }

    function archiveFingerprint(file) {
        return `${file.name}:${file.size}:${file.lastModified || 0}`;
    }

    function packageArchiveCachePath(archiveId) {
        return `.rumoca/cache/package-archives/${encodeURIComponent(String(archiveId || '').trim())}.bin`;
    }

    async function exportArchiveBinaryCache(archiveId) {
        const normalizedArchiveId = String(archiveId || '').trim();
        if (!normalizedArchiveId) {
            return 0;
        }
        const archive = loadedPackageArchives.get(normalizedArchiveId);
        const uris = archive ? Object.keys(archive.files || {}).sort((lhs, rhs) => lhs.localeCompare(rhs)) : [];
        if (uris.length === 0) {
            return 0;
        }
        const bytes = await sendWorkspaceCommand(
            'rumoca.workspace.exportParsedSourceRootsBinary',
            { urisJson: JSON.stringify(uris) },
            300000,
        );
        if (!(bytes instanceof Uint8Array) || bytes.length === 0) {
            return 0;
        }
        projectFs?.setCacheFile?.(packageArchiveCachePath(normalizedArchiveId), bytes);
        return bytes.length;
    }

    async function restoreArchiveBinaryCache(archiveId) {
        const normalizedArchiveId = String(archiveId || '').trim();
        if (!normalizedArchiveId) {
            return 0;
        }
        const bytes = projectFs?.getCacheFile?.(packageArchiveCachePath(normalizedArchiveId));
        if (!(bytes instanceof Uint8Array) || bytes.length === 0) {
            return 0;
        }
        const merged = await sendWorkspaceCommand(
            'rumoca.workspace.mergeParsedSourceRootsBinary',
            { bytes },
            300000,
        );
        return Number(merged) || 0;
    }

    function rebuildStagedSourceRootsFromArchives() {
        stagedSourceRoots = {};
        for (const archive of loadedPackageArchives.values()) {
            Object.assign(stagedSourceRoots, archive.files);
        }
    }

    function resetPackageArchiveRowsUi() {
        document.querySelectorAll('.package-archive-row input[type="file"]').forEach(input => {
            input.value = '';
        });
        document.querySelectorAll('.package-archive-row').forEach(row => {
            const status = row.querySelector('.package-archive-status');
            if (status) {
                status.textContent = 'No file selected';
                status.className = 'package-archive-status pending';
            }
            hidePackageArchiveProgress(row);
        });
    }

    async function reloadWorkerSourceRootsFromArchives(statusLabel, emptyLabel = 'No package archives loaded.') {
        await sendWorkspaceCommand('rumoca.workspace.clearSourceRootCache', {});
        const archiveEntries = Array.from(loadedPackageArchives.entries());
        const fileCount = archiveEntries.reduce(
            (count, [, archive]) => count + Object.keys(archive.files || {}).length,
            0,
        );
        if (fileCount === 0) {
            setTerminalOutput(emptyLabel);
            await refreshPackageViewer(true);
            return {
                parsedCount: 0,
                fileCount: 0,
            };
        }

        const uncachedSourceRoots = {};
        let parsedCount = 0;
        const uncachedArchiveIds = [];

        for (const [archiveId, archive] of archiveEntries) {
            const restoredCount = await restoreArchiveBinaryCache(archiveId);
            if (restoredCount > 0) {
                parsedCount += restoredCount;
                continue;
            }
            uncachedArchiveIds.push(archiveId);
            Object.assign(uncachedSourceRoots, archive.files || {});
        }

        if (Object.keys(uncachedSourceRoots).length > 0) {
            const resultJson = await sendWorkspaceCommand(
                'rumoca.workspace.loadSourceRoots',
                { sourceRoots: JSON.stringify(uncachedSourceRoots) },
                300000,
            );
            const result = JSON.parse(resultJson);
            parsedCount += Number(result?.parsed_count) || 0;
            for (const archiveId of uncachedArchiveIds) {
                try {
                    await exportArchiveBinaryCache(archiveId);
                } catch (error) {
                    console.warn('Failed to persist package-archive binary cache:', archiveId, error);
                }
            }
        }

        await setTerminalOutputWithSourceRootStatuses(statusLabel(parsedCount, fileCount));
        await refreshPackageViewer(true);
        return {
            parsedCount,
            fileCount,
        };
    }

    function updatePackageArchiveCount(count) {
        const finalCount = count === undefined ? loadedPackageArchives.size : count;
        const badge = document.getElementById('packageArchiveCount');
        if (badge) {
            badge.textContent = `(${finalCount})`;
        }
    }

    function renderLoadedPackageArchiveList() {
        const container = document.getElementById('loadedPackageArchiveList');
        if (!container) return;
        if (loadedPackageArchives.size === 0) {
            container.innerHTML = '';
            return;
        }

        const rows = [];
        for (const [archiveId, archive] of loadedPackageArchives.entries()) {
            const escapedName = escapeHtmlSafe(archive.file_name);
            const escapedMeta = escapeHtmlSafe(`${archive.file_count} files`);
            rows.push(`
                <div class="package-archive-loaded-item">
                    <div>
                        <div>${escapedName}</div>
                        <div class="package-archive-loaded-meta">${escapedMeta}</div>
                    </div>
                    <button class="small danger" data-archive-id="${encodeURIComponent(archiveId)}" title="Unload this package archive">Remove</button>
                </div>
            `);
        }

        container.innerHTML = rows.join('');
        container.querySelectorAll('button[data-archive-id]').forEach(button => {
            button.addEventListener('click', async () => {
                const archiveId = decodeURIComponent(button.dataset.archiveId || '');
                if (!archiveId) return;
                button.disabled = true;
                try {
                    await removeLoadedPackageArchive(archiveId);
                } finally {
                    button.disabled = false;
                }
            });
        });
    }

    function sanitizeDocumentationHtml(rawHtml) {
        const template = document.createElement('template');
        template.innerHTML = String(rawHtml || '');
        const blockedTags = new Set(['script', 'style', 'iframe', 'object', 'embed', 'link', 'meta']);
        const walker = document.createTreeWalker(template.content, NodeFilter.SHOW_ELEMENT);
        const toRemove = [];

        while (walker.nextNode()) {
            const el = walker.currentNode;
            const tag = el.tagName ? el.tagName.toLowerCase() : '';
            if (blockedTags.has(tag)) {
                toRemove.push(el);
                continue;
            }
            if (tag === 'a') {
                const href = (el.getAttribute('href') || '').trim();
                if (href) {
                    el.setAttribute('title', `Link disabled in WASM editor: ${href}`);
                }
                el.classList.add('doc-link-disabled');
                el.removeAttribute('href');
                el.removeAttribute('target');
                el.removeAttribute('rel');
            }
            for (const attr of Array.from(el.attributes)) {
                const name = attr.name.toLowerCase();
                const value = attr.value || '';
                if (name.startsWith('on')) {
                    el.removeAttribute(attr.name);
                } else if ((name === 'href' || name === 'src') && /^\s*javascript:/i.test(value)) {
                    el.removeAttribute(attr.name);
                }
            }
        }

        toRemove.forEach(node => node.remove());
        return template.innerHTML;
    }

    function renderDocBlock(rawText) {
        const text = String(rawText || '').trim();
        if (!text) return '';
        const looksLikeHtml = /<\s*[a-zA-Z][^>]*>/.test(text);
        if (looksLikeHtml) {
            return sanitizeDocumentationHtml(text);
        }
        return `<p>${escapeHtmlSafe(text).replace(/\n/g, '<br>')}</p>`;
    }

    function disposeClassSourceEditor() {
        if (!classSourceEditor) return;
        classSourceEditor.dispose();
        classSourceEditor = null;
    }

    function mountClassSourcePane(rawSource) {
        const host = document.getElementById('classSourcePane');
        if (!host) return;

        const source = String(rawSource || '');
        if (window.monaco && window.monaco.editor) {
            classSourceEditor = window.monaco.editor.create(host, {
                value: source,
                language: 'modelica',
                theme: 'rumoca-dark',
                readOnly: true,
                minimap: { enabled: false },
                fontSize: 12,
                lineNumbers: 'on',
                lineNumbersMinChars: 3,
                wordWrap: 'on',
                automaticLayout: true,
                scrollBeyondLastLine: false,
                folding: true,
            });
            return;
        }

        host.classList.add('class-source-pane-fallback');
        host.textContent = source.trim() || 'Source unavailable for this class.';
    }

    function countClassNodes(nodes) {
        let total = 0;
        for (const node of nodes) {
            total += 1 + countClassNodes(node.children || []);
        }
        return total;
    }

    function filterClassNodes(nodes, search) {
        const needle = String(search || '').trim().toLowerCase();
        if (!needle) return nodes;

        const filtered = [];
        for (const node of nodes) {
            const children = filterClassNodes(node.children || [], needle);
            const match = String(node.qualified_name || '').toLowerCase().includes(needle)
                || String(node.name || '').toLowerCase().includes(needle);
            if (match || children.length > 0) {
                filtered.push({
                    ...node,
                    children,
                });
                if (children.length > 0) {
                    classTreeExpanded.add(node.qualified_name);
                }
            }
        }
        return filtered;
    }

    function renderClassTreeRows(nodes, depth = 0) {
        let html = '';
        for (const node of nodes) {
            const qn = String(node.qualified_name || '');
            const encodedQn = encodeURIComponent(qn);
            const children = node.children || [];
            const hasChildren = children.length > 0;
            const expanded = hasChildren ? classTreeExpanded.has(qn) : false;
            const selected = selectedClassQualifiedName === qn ? ' selected' : '';
            const indent = depth * 14 + 4;
            const kind = node.partial ? `${node.class_type} partial` : node.class_type;

            html += `<div class="class-tree-row${selected}" style="padding-left:${indent}px">`;
            if (hasChildren) {
                html += `<button class="tree-toggle" onclick="toggleClassExpand(event, '${encodedQn}')" title="Expand/collapse">${expanded ? '▾' : '▸'}</button>`;
            } else {
                html += '<button class="tree-toggle" disabled title="Leaf class">•</button>';
            }
            html += `<button class="tree-item" onclick="selectClass('${encodedQn}')" title="${escapeHtmlSafe(qn)}">${escapeHtmlSafe(node.name)}</button>`;
            html += `<span class="tree-kind">${escapeHtmlSafe(kind)}</span>`;
            html += '</div>';

            if (hasChildren && expanded) {
                html += renderClassTreeRows(children, depth + 1);
            }
        }
        return html;
    }

    function renderClassTree() {
        const panel = document.getElementById('classTreePanel');
        const nodes = classTreeFiltered;
        if (!panel) return;
        if (!nodes || nodes.length === 0) {
            panel.innerHTML = '<div class="class-tree-empty">No matching classes.</div>';
            return;
        }
        panel.innerHTML = renderClassTreeRows(nodes);
    }

    function renderClassDocPlaceholder(message) {
        const panel = document.getElementById('classDocPanel');
        if (!panel) return;
        disposeClassSourceEditor();
        panel.innerHTML = `<div class="class-tree-empty">${escapeHtmlSafe(message)}</div>`;
    }

    function renderClassInfo(info) {
        const panel = document.getElementById('classDocPanel');
        if (!panel) return;

        const description = renderDocBlock(info.description || '');
        const docInfo = renderDocBlock(info.documentation_html || '');
        const revisions = renderDocBlock(info.documentation_revisions_html || '');
        const components = Array.isArray(info.components) ? info.components : [];
        const componentRows = components.length > 0
            ? components.map(c => `<tr>
                <td>${escapeHtmlSafe(c.name || '')}</td>
                <td>${escapeHtmlSafe(c.type_name || '')}</td>
                <td>${escapeHtmlSafe(c.variability || '')}</td>
                <td>${escapeHtmlSafe(c.causality || '')}</td>
                <td>${escapeHtmlSafe((c.description || '').trim() || '-')}</td>
            </tr>`).join('')
            : '<tr><td colspan="5">No components</td></tr>';

        disposeClassSourceEditor();
        panel.innerHTML = `
            <div class="class-doc-header">
                <div class="class-doc-title">${escapeHtmlSafe(info.qualified_name || '')}</div>
                <div class="class-doc-meta">${escapeHtmlSafe(info.class_type || '')}${info.partial ? ' (partial)' : ''}${info.encapsulated ? ', encapsulated' : ''}</div>
                <div class="class-doc-meta">
                    ${info.component_count || 0} components, ${info.equation_count || 0} equations, ${info.algorithm_count || 0} algorithm sections, ${info.nested_class_count || 0} nested classes
                </div>
            </div>
            <div class="class-doc-section">
                <h4>Description</h4>
                ${description || '<p><em>No description string.</em></p>'}
            </div>
            <div class="class-doc-section">
                <h4>Documentation</h4>
                ${docInfo || '<p><em>No Documentation(info=...) annotation.</em></p>'}
            </div>
            <div class="class-doc-section">
                <h4>Revisions</h4>
                ${revisions || '<p><em>No Documentation(revisions=...) annotation.</em></p>'}
            </div>
            <div class="class-doc-section">
                <h4>Components</h4>
                <table>
                    <thead>
                        <tr><th>Name</th><th>Type</th><th>Variability</th><th>Causality</th><th>Description</th></tr>
                    </thead>
                    <tbody>${componentRows}</tbody>
                </table>
            </div>
            <div class="class-doc-section">
                <h4>Source (read-only)</h4>
                <div id="classSourcePane" class="class-source-pane"></div>
            </div>
        `;
        mountClassSourcePane(info.source_modelica || '');
    }

    function classTreeContainsQualifiedName(nodes, qualifiedName) {
        for (const node of nodes) {
            if (node.qualified_name === qualifiedName) return true;
            if (classTreeContainsQualifiedName(node.children || [], qualifiedName)) return true;
        }
        return false;
    }

    async function removeLoadedPackageArchive(archiveId) {
        if (!loadedPackageArchives.has(archiveId)) return;

        loadedPackageArchives.delete(archiveId);
        projectFs?.removePackageArchive(archiveId);
        projectFs?.removeCacheFile?.(packageArchiveCachePath(archiveId));
        onPackageArchivesChanged?.();
        for (const [rowIndex, mappedArchiveId] of packageArchiveRowIds.entries()) {
            if (mappedArchiveId !== archiveId) continue;
            packageArchiveRowIds.delete(rowIndex);
            const row = document.querySelector(`.package-archive-row[data-index="${rowIndex}"]`);
            if (!row) continue;
            const status = row.querySelector('.package-archive-status');
            if (status) {
                status.textContent = 'Removed';
                status.className = 'package-archive-status pending';
            }
            hidePackageArchiveProgress(row);
        }

        rebuildStagedSourceRootsFromArchives();
        renderLoadedPackageArchiveList();
        updatePackageArchiveCount();

        setTerminalOutput('Reloading source-root cache after package-archive removal...');
        try {
            await reloadWorkerSourceRootsFromArchives(
                (parsedCount) => `Reloaded ${parsedCount} files from ${loadedPackageArchives.size} archive(s).`,
                'No package archives loaded.',
            );
        } catch (e) {
            setTerminalOutput(`Failed to reload package archives after removal: ${e.message || e}`);
        }
    }

    function toggleClassExpand(event, encodedQualifiedName) {
        event.stopPropagation();
        const qualifiedName = decodeURIComponent(encodedQualifiedName || '');
        if (!qualifiedName) return;
        if (classTreeExpanded.has(qualifiedName)) classTreeExpanded.delete(qualifiedName);
        else classTreeExpanded.add(qualifiedName);
        renderClassTree();
    }

    async function selectClass(encodedQualifiedName) {
        const qualifiedName = decodeURIComponent(encodedQualifiedName || '');
        if (!qualifiedName) return;

        selectedClassQualifiedName = qualifiedName;
        renderClassTree();

        if (classInfoCache.has(qualifiedName)) {
            renderClassInfo(classInfoCache.get(qualifiedName));
            return;
        }

        renderClassDocPlaceholder(`Loading ${qualifiedName}...`);
        try {
            const json = await sendLanguageCommand('rumoca.language.getClassInfo', { qualifiedName });
            const info = JSON.parse(json);
            classInfoCache.set(qualifiedName, info);
            renderClassInfo(info);
        } catch (e) {
            renderClassDocPlaceholder(`Failed to load class info: ${e.message || e}`);
        }
    }

    function filterClassTree() {
        const input = document.getElementById('classSearchInput');
        const search = input ? input.value || '' : '';
        classTreeFiltered = filterClassNodes(classTreeData, search);
        renderClassTree();
    }

    async function refreshPackageViewer() {
        const panel = document.getElementById('classTreePanel');
        if (!panel) return;

        if (!isWorkerReady()) {
            panel.innerHTML = '<div class="class-tree-empty">Worker is still initializing...</div>';
            return;
        }

        panel.innerHTML = '<div class="class-tree-empty">Loading class tree...</div>';
        try {
            const json = await sendLanguageCommand('rumoca.language.listClasses', {});
            const result = JSON.parse(json);

            classTreeData = Array.isArray(result.classes) ? result.classes : [];
            classTreeExpanded = new Set(classTreeData.map(node => node.qualified_name));
            updateClassCountBadge(Number(result.total_classes) || countClassNodes(classTreeData));

            const searchInput = document.getElementById('classSearchInput');
            classTreeFiltered = filterClassNodes(classTreeData, searchInput ? searchInput.value || '' : '');
            renderClassTree();

            if (selectedClassQualifiedName) {
                const exists = classTreeContainsQualifiedName(classTreeData, selectedClassQualifiedName);
                if (exists) {
                    await selectClass(encodeURIComponent(selectedClassQualifiedName));
                } else {
                    selectedClassQualifiedName = null;
                    renderClassDocPlaceholder('Select a class to view documentation.');
                }
            }
        } catch (e) {
            updateClassCountBadge(0);
            classTreeData = [];
            classTreeFiltered = [];
            panel.innerHTML = `<div class="class-tree-empty">Failed to load class tree: ${escapeHtmlSafe(e.message || String(e))}</div>`;
            renderClassDocPlaceholder('Select a class to view documentation.');
        }
    }

    function addPackageArchiveRow() {
        const list = document.getElementById('packageArchiveList');
        if (!list) return;

        const index = packageArchiveRowCount++;
        const row = document.createElement('div');
        row.className = 'package-archive-row';
        row.dataset.index = index;
        row.innerHTML = `
            <input type="file" accept=".zip" onchange="loadPackageArchiveRowFile(${index}, this)" title="Select a ZIP file containing .mo package files">
            <button class="small danger" onclick="removePackageArchiveRow(${index})" title="Remove this package archive">X</button>
            <span class="package-archive-status pending">No file selected</span>
            <div class="package-archive-progress" hidden>
                <div class="package-archive-progress-track"><div class="package-archive-progress-fill"></div></div>
                <span class="package-archive-progress-text">0%</span>
            </div>
        `;
        list.appendChild(row);
    }

    function removePackageArchiveRow(index) {
        const archiveId = packageArchiveRowIds.get(index);
        if (archiveId) {
            packageArchiveRowIds.delete(index);
            void removeLoadedPackageArchive(archiveId);
        }
        const row = document.querySelector(`.package-archive-row[data-index="${index}"]`);
        if (row) row.remove();
        updatePackageArchiveCount();
    }

    function ensurePackageArchiveRow() {
        let row = document.querySelector('.package-archive-row:last-child');
        if (row) {
            return row;
        }
        addPackageArchiveRow();
        row = document.querySelector('.package-archive-row:last-child');
        if (row) {
            return row;
        }
        const virtualRow = document.createElement('div');
        virtualRow.className = 'package-archive-row virtual';
        const status = document.createElement('span');
        status.className = 'package-archive-status pending';
        virtualRow.appendChild(status);
        return virtualRow;
    }

    async function importStagedSourceRoots(row, fileName, fileCount) {
        const status = row?.querySelector('.package-archive-status') || null;
        if (status) {
            status.textContent = `Parsing ${fileCount}...`;
            status.className = 'package-archive-status loading';
        }
        if (row) {
            setPackageArchiveProgress(row, 0, `Parsing ${fileCount} files`, true);
        }

        setTerminalOutput(`Loaded ${fileCount} .mo files from ${fileName}\n\nParsing source roots with rayon...`);
        const importStart = performance.now();
        const sourceRootsJson = JSON.stringify(stagedSourceRoots);
        const resultJson = await sendWorkspaceCommand(
            'rumoca.workspace.loadSourceRoots',
            { sourceRoots: sourceRootsJson },
            300000,
        );
        const result = JSON.parse(resultJson);
        const sourceRootImportMs = Math.round(performance.now() - importStart);
        const elapsed = (sourceRootImportMs / 1000).toFixed(1);

        if (status) {
            status.textContent = `${result.parsed_count} files parsed`;
            status.className = 'package-archive-status loaded';
        }
        if (row) {
            setPackageArchiveProgress(row, 100, 'Done', false);
        }

        let output = `Parsed ${result.parsed_count} files in ${elapsed}s.\nArchives loaded: ${loadedPackageArchives.size}`;
        if (result.skipped_files.length > 0) {
            output += `\n\nSkipped ${result.skipped_files.length} files (already loaded):`;
            result.skipped_files.slice(0, 5).forEach(f => {
                output += `\n  - ${f}`;
            });
            if (result.skipped_files.length > 5) {
                output += `\n  ... and ${result.skipped_files.length - 5} more`;
            }
        }
        if (result.conflicts.length > 0) {
            output += `\n\nWARNING: ${result.conflicts.length} source-root conflicts (replaced): ${result.conflicts.join(', ')}`;
        }
        output += '\n\nReady for compilation!';

        await setTerminalOutputWithSourceRootStatuses(output);
        renderLoadedPackageArchiveList();
        updatePackageArchiveCount();
        for (const archiveId of loadedPackageArchives.keys()) {
            try {
                await exportArchiveBinaryCache(archiveId);
            } catch (error) {
                console.warn('Failed to export package-archive binary cache:', archiveId, error);
            }
        }
        await refreshPackageViewer(true);

        return {
            sourceRootImportMs,
            fileCount,
            parsedCount: result.parsed_count,
        };
    }

    async function loadPackageArchive(index, row, file, options = {}) {
        if (!row) return;

        const { stageOnly = false } = options;
        const status = row.querySelector('.package-archive-status');
        let archivePrepMs = 0;

        status.textContent = 'Loading...';
        status.className = 'package-archive-status loading';
        setPackageArchiveProgress(row, 0, 'Loading', true);

        try {
            const prepStart = performance.now();
            setTerminalOutput(`Loading ${file.name}...`);

            const arrayBuffer = await file.arrayBuffer();
            const zip = await JSZip.loadAsync(arrayBuffer);

            status.textContent = 'Extracting...';
            setTerminalOutput(`Extracting ${file.name}...`);

            const data = {};
            let processedCount = 0;
            const moFiles = [];

            zip.forEach((relativePath, zipFile) => {
                if (relativePath.endsWith('.mo') && !zipFile.dir) {
                    if (!relativePath.includes('Test') && !relativePath.includes('Obsolete')) {
                        moFiles.push({ path: relativePath, file: zipFile });
                    }
                }
            });

            setTerminalOutput(`Found ${moFiles.length} .mo files, extracting...`);
            if (moFiles.length === 0) {
                throw new Error('No usable .mo files found in archive');
            }
            setPackageArchiveProgress(row, 0, `Extracting 0/${moFiles.length}`, false);

            for (const { path, file: zipFile } of moFiles) {
                const content = await zipFile.async('string');
                const parts = path.split('/');
                let normalizedPath;
                if (parts.length > 1 && /(?:Standard)?Library|^MSL/i.test(parts[0])) {
                    normalizedPath = parts.slice(1).join('/');
                } else if (parts.length > 0) {
                    parts[0] = parts[0].replace(/[\s-][\d.]+$/, '');
                    normalizedPath = parts.join('/');
                } else {
                    normalizedPath = path;
                }

                data[normalizedPath] = content;
                processedCount++;
                if (processedCount % 50 === 0 || processedCount === moFiles.length) {
                    const extractPercent = (processedCount / moFiles.length) * 100;
                    status.textContent = `Extracting ${processedCount}/${moFiles.length}`;
                    setPackageArchiveProgress(
                        row,
                        extractPercent,
                        `Extracting ${processedCount}/${moFiles.length}`,
                        false,
                    );
                }
            }

            const fileCount = Object.keys(data).length;
            archivePrepMs = Math.round(performance.now() - prepStart);
            status.textContent = `Parsing ${fileCount}...`;
            status.className = 'package-archive-status loading';
            setPackageArchiveProgress(row, 0, `Parsing ${fileCount} files`, true);

            setTerminalOutput(`Loaded ${fileCount} .mo files from ${file.name}\n\nParsing source roots with rayon...`);
            const archiveId = archiveFingerprint(file);
            const previousArchiveId = packageArchiveRowIds.get(index) || null;
            const previousArchiveEntry = previousArchiveId
                ? loadedPackageArchives.get(previousArchiveId) || null
                : null;

            if (previousArchiveId && previousArchiveId !== archiveId) {
                loadedPackageArchives.delete(previousArchiveId);
                projectFs?.removePackageArchive(previousArchiveId);
                projectFs?.removeCacheFile?.(packageArchiveCachePath(previousArchiveId));
            }
            loadedPackageArchives.set(archiveId, {
                file_name: file.name,
                file_count: fileCount,
                files: data,
            });
            projectFs?.replacePackageArchive(archiveId, file.name, data);
            onPackageArchivesChanged?.();
            packageArchiveRowIds.set(index, archiveId);
            rebuildStagedSourceRootsFromArchives();

            if (stageOnly) {
                renderLoadedPackageArchiveList();
                updatePackageArchiveCount();
                status.textContent = `${fileCount} files staged`;
                status.className = 'package-archive-status loaded';
                setPackageArchiveProgress(row, 100, 'Staged', false);
                setTerminalOutput(
                    `Loaded ${fileCount} .mo files from ${file.name}\n\nReady to import on first source-root request.`,
                );
                return {
                    archivePrepMs,
                    sourceRootImportMs: 0,
                    fileCount,
                    parsedCount: 0,
                };
            }

            try {
                const imported = await importStagedSourceRoots(row, file.name, fileCount);
                return {
                    archivePrepMs,
                    sourceRootImportMs: imported.sourceRootImportMs,
                    fileCount: imported.fileCount,
                    parsedCount: imported.parsedCount,
                };
            } catch (e) {
                loadedPackageArchives.delete(archiveId);
                projectFs?.removePackageArchive(archiveId);
                projectFs?.removeCacheFile?.(packageArchiveCachePath(archiveId));
                if (previousArchiveId && previousArchiveEntry) {
                    loadedPackageArchives.set(previousArchiveId, previousArchiveEntry);
                    projectFs?.replacePackageArchive(
                        previousArchiveId,
                        previousArchiveEntry.file_name,
                        previousArchiveEntry.files,
                    );
                    packageArchiveRowIds.set(index, previousArchiveId);
                } else {
                    packageArchiveRowIds.delete(index);
                }
                onPackageArchivesChanged?.();
                rebuildStagedSourceRootsFromArchives();
                renderLoadedPackageArchiveList();

                status.textContent = 'Parse error';
                status.className = 'package-archive-status error';
                setPackageArchiveProgress(row, 100, 'Parse error', false);
                setTerminalOutput(`Loaded ${fileCount} files but failed to parse: ${e.message}`);
                updatePackageArchiveCount();
            }
        } catch (e) {
            status.textContent = 'Error';
            status.className = 'package-archive-status error';
            setPackageArchiveProgress(row, 100, 'Error', false);
            setTerminalOutput(`Failed to load package archive: ${e.message}`);
        } finally {
            status.classList.remove('loading');
        }
    }

    async function loadPackageArchiveRowFile(index, input) {
        const row = document.querySelector(`.package-archive-row[data-index="${index}"]`);
        if (!row) return;

        const status = row.querySelector('.package-archive-status');
        const file = input.files[0];

        if (!file) {
            status.textContent = 'No file selected';
            status.className = 'package-archive-status pending';
            hidePackageArchiveProgress(row);
            return;
        }

        await loadPackageArchive(index, row, file);
    }

    async function loadPackageArchiveFile(file) {
        if (!file) {
            throw new Error('No package archive provided');
        }
        const row = ensurePackageArchiveRow();
        const index = Number(row.dataset.index || packageArchiveRowCount++);
        return await loadPackageArchive(index, row, file);
    }

    async function stagePackageArchiveFile(file) {
        if (!file) {
            throw new Error('No package archive provided');
        }
        const row = ensurePackageArchiveRow();
        const index = Number(row.dataset.index || packageArchiveRowCount++);
        return await loadPackageArchive(index, row, file, { stageOnly: true });
    }

    async function importLoadedPackageArchivesForSmoke() {
        const fileCount = Object.keys(stagedSourceRoots).length;
        if (fileCount === 0) {
            return {
                sourceRootImportMs: 0,
                fileCount: 0,
                parsedCount: 0,
            };
        }
        const row = document.querySelector('.package-archive-row:last-child') || null;
        return await importStagedSourceRoots(row, 'staged archive', fileCount);
    }

    async function clearPackageArchives() {
        for (const archiveId of loadedPackageArchives.keys()) {
            projectFs?.removeCacheFile?.(packageArchiveCachePath(archiveId));
        }
        stagedSourceRoots = {};
        loadedPackageArchives.clear();
        packageArchiveRowIds.clear();
        projectFs?.clearPackageArchives();
        onPackageArchivesChanged?.();
        resetPackageArchiveRowsUi();

        try {
            await sendWorkspaceCommand('rumoca.workspace.clearSourceRootCache', {});
        } catch (e) {
            console.warn('Failed to clear package-archive cache:', e);
        }

        renderLoadedPackageArchiveList();
        updatePackageArchiveCount(0);
        setTerminalOutput('Package archives cleared.');
        classInfoCache.clear();
        selectedClassQualifiedName = null;
        await refreshPackageViewer(true);
    }

    async function restoreProjectPackageArchives() {
        stagedSourceRoots = {};
        loadedPackageArchives.clear();
        packageArchiveRowIds.clear();
        resetPackageArchiveRowsUi();

        const archives = projectFs?.listPackageArchives?.() || [];
        for (const archive of archives) {
            loadedPackageArchives.set(archive.archiveId, {
                file_name: archive.fileName,
                file_count: archive.fileCount,
                files: projectFs.getPackageArchiveFiles(archive.archiveId),
            });
        }
        onPackageArchivesChanged?.();
        rebuildStagedSourceRootsFromArchives();
        renderLoadedPackageArchiveList();
        updatePackageArchiveCount();

        classInfoCache.clear();
        selectedClassQualifiedName = null;

        try {
            await reloadWorkerSourceRootsFromArchives(
                (parsedCount) => `Restored ${parsedCount} package-archive files from project.`,
                'Project has no package archives loaded.',
            );
        } catch (e) {
            setTerminalOutput(`Failed to restore project package archives: ${e.message || e}`);
        }
    }

    function handleWorkerProgress({ current, total, percent }) {
        const loadingRow = document.querySelector('.package-archive-row .package-archive-status.loading')?.closest('.package-archive-row');
        const packageArchiveStatus = loadingRow ? loadingRow.querySelector('.package-archive-status') : null;
        if (packageArchiveStatus) {
            packageArchiveStatus.textContent = `Parsing ${current}/${total} (${percent}%)`;
        }
        if (loadingRow) {
            setPackageArchiveProgress(loadingRow, Number(percent) || 0, `Parsing ${current}/${total}`, false);
        }
    }

    function bindWindowApi() {
        window.removeLoadedPackageArchive = removeLoadedPackageArchive;
        window.toggleClassExpand = toggleClassExpand;
        window.selectClass = selectClass;
        window.filterClassTree = filterClassTree;
        window.refreshPackageViewer = refreshPackageViewer;
        window.addPackageArchiveRow = addPackageArchiveRow;
        window.removePackageArchiveRow = removePackageArchiveRow;
        window.loadPackageArchiveRowFile = loadPackageArchiveRowFile;
        window.loadPackageArchiveFile = loadPackageArchiveFile;
        window.stagePackageArchiveFile = stagePackageArchiveFile;
        window.importLoadedPackageArchivesForSmoke = importLoadedPackageArchivesForSmoke;
        window.clearPackageArchives = clearPackageArchives;

        const classDocPanel = document.getElementById('classDocPanel');
        if (classDocPanel && !classDocPanel.dataset.linksGuardBound) {
            classDocPanel.addEventListener('click', event => {
                const anchor = event.target.closest('a');
                if (!anchor) return;
                event.preventDefault();
                event.stopPropagation();
            });
            classDocPanel.dataset.linksGuardBound = 'true';
        }

        renderLoadedPackageArchiveList();
        updatePackageArchiveCount(loadedPackageArchives.size);
    }

    return {
        bindWindowApi,
        clearPackageArchives,
        handleWorkerProgress,
        refreshPackageViewer,
        restoreProjectPackageArchives,
        updatePackageArchiveCount,
    };
}
