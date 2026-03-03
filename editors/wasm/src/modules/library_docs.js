function escapeHtmlSafe(value) {
    return String(value ?? '')
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#39;');
}

export function createLibraryDocsController({ sendRequest, setTerminalOutput, isWorkerReady }) {
    let loadedLibraries = {};
    const loadedLibraryArchives = new Map();
    const libraryRowArchiveIds = new Map();
    let activeLibraryLoadRow = null;
    let libraryRowCount = 1;
    let classTreeData = [];
    let classTreeFiltered = [];
    let classTreeExpanded = new Set();
    let selectedClassQualifiedName = null;
    const classInfoCache = new Map();
    let classSourceEditor = null;

    function updateClassCountBadge(totalClasses) {
        const badge = document.getElementById('classCount');
        if (!badge) return;
        badge.textContent = `(${totalClasses || 0})`;
    }

    function ensureLibraryProgress(row) {
        let progress = row.querySelector('.library-progress');
        if (!progress) {
            progress = document.createElement('div');
            progress.className = 'library-progress';
            progress.hidden = true;
            progress.innerHTML = `
                <div class="library-progress-track"><div class="library-progress-fill"></div></div>
                <span class="library-progress-text">0%</span>
            `;
            row.appendChild(progress);
        }
        return {
            progress,
            fill: progress.querySelector('.library-progress-fill'),
            text: progress.querySelector('.library-progress-text'),
        };
    }

    function setLibraryProgress(row, percent, label, indeterminate = false) {
        if (!row) return;
        const { progress, fill, text } = ensureLibraryProgress(row);
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

    function hideLibraryProgress(row) {
        if (!row) return;
        const { progress, fill, text } = ensureLibraryProgress(row);
        progress.hidden = true;
        progress.classList.remove('indeterminate');
        if (fill) fill.style.width = '0%';
        if (text) text.textContent = '0%';
    }

    function archiveFingerprint(file) {
        return `${file.name}:${file.size}:${file.lastModified || 0}`;
    }

    function rebuildLoadedLibrariesFromArchives() {
        loadedLibraries = {};
        for (const archive of loadedLibraryArchives.values()) {
            Object.assign(loadedLibraries, archive.files);
        }
    }

    function updateLibraryCount(count) {
        const finalCount = count === undefined ? loadedLibraryArchives.size : count;
        const badge = document.getElementById('libCount');
        if (badge) {
            badge.textContent = `(${finalCount})`;
        }
    }

    function renderLoadedLibraryList() {
        const container = document.getElementById('loadedLibraryList');
        if (!container) return;
        if (loadedLibraryArchives.size === 0) {
            container.innerHTML = '';
            return;
        }

        const rows = [];
        for (const [archiveId, archive] of loadedLibraryArchives.entries()) {
            const escapedName = escapeHtmlSafe(archive.file_name);
            const escapedMeta = escapeHtmlSafe(`${archive.file_count} files`);
            rows.push(`
                <div class="library-loaded-item">
                    <div>
                        <div>${escapedName}</div>
                        <div class="library-loaded-meta">${escapedMeta}</div>
                    </div>
                    <button class="small danger" data-archive-id="${encodeURIComponent(archiveId)}" title="Unload this library archive">Remove</button>
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
                    await removeLoadedArchive(archiveId);
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

    async function removeLoadedArchive(archiveId) {
        if (!loadedLibraryArchives.has(archiveId)) return;

        loadedLibraryArchives.delete(archiveId);
        for (const [rowIndex, mappedArchiveId] of libraryRowArchiveIds.entries()) {
            if (mappedArchiveId !== archiveId) continue;
            libraryRowArchiveIds.delete(rowIndex);
            const row = document.querySelector(`.library-row[data-index="${rowIndex}"]`);
            if (!row) continue;
            const status = row.querySelector('.library-status');
            if (status) {
                status.textContent = 'Removed';
                status.className = 'library-status pending';
            }
            hideLibraryProgress(row);
        }

        rebuildLoadedLibrariesFromArchives();
        renderLoadedLibraryList();
        updateLibraryCount();

        setTerminalOutput('Reloading library cache after archive removal...');
        try {
            await sendRequest('clearLibraryCache', {});
            const fileCount = Object.keys(loadedLibraries).length;
            if (fileCount > 0) {
                const resultJson = await sendRequest('loadLibraries', { libraries: JSON.stringify(loadedLibraries) }, 300000);
                const result = JSON.parse(resultJson);
                setTerminalOutput(`Reloaded ${result.parsed_count} files from ${loadedLibraryArchives.size} archive(s).`);
            } else {
                setTerminalOutput('No libraries loaded.');
            }
            await refreshPackageViewer(true);
        } catch (e) {
            setTerminalOutput(`Failed to reload libraries after removal: ${e.message || e}`);
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
            const json = await sendRequest('getClassInfo', { qualifiedName });
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
            const json = await sendRequest('listClasses', {});
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

    function addLibraryRow() {
        const list = document.getElementById('libraryList');
        if (!list) return;

        const index = libraryRowCount++;
        const row = document.createElement('div');
        row.className = 'library-row';
        row.dataset.index = index;
        row.innerHTML = `
            <input type="file" accept=".zip" onchange="loadLibraryFile(${index}, this)" title="Select a ZIP file containing .mo library files">
            <button class="small danger" onclick="removeLibraryRow(${index})" title="Remove this library">X</button>
            <span class="library-status pending">No file selected</span>
            <div class="library-progress" hidden>
                <div class="library-progress-track"><div class="library-progress-fill"></div></div>
                <span class="library-progress-text">0%</span>
            </div>
        `;
        list.appendChild(row);
    }

    function removeLibraryRow(index) {
        const archiveId = libraryRowArchiveIds.get(index);
        if (archiveId) {
            libraryRowArchiveIds.delete(index);
            void removeLoadedArchive(archiveId);
        }
        const row = document.querySelector(`.library-row[data-index="${index}"]`);
        if (row) row.remove();
        updateLibraryCount();
    }

    async function loadLibraryFile(index, input) {
        const row = document.querySelector(`.library-row[data-index="${index}"]`);
        if (!row) return;

        const status = row.querySelector('.library-status');
        const file = input.files[0];

        if (!file) {
            status.textContent = 'No file selected';
            status.className = 'library-status pending';
            hideLibraryProgress(row);
            return;
        }

        activeLibraryLoadRow = row;
        status.textContent = 'Loading...';
        status.className = 'library-status loading';
        setLibraryProgress(row, 0, 'Loading', true);

        try {
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
            setLibraryProgress(row, 0, `Extracting 0/${moFiles.length}`, false);

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
                    setLibraryProgress(
                        row,
                        extractPercent,
                        `Extracting ${processedCount}/${moFiles.length}`,
                        false,
                    );
                }
            }

            const fileCount = Object.keys(data).length;
            status.textContent = `Parsing ${fileCount}...`;
            status.className = 'library-status loading';
            setLibraryProgress(row, 0, `Parsing ${fileCount} files`, true);

            setTerminalOutput(`Loaded ${fileCount} .mo files from ${file.name}\n\nParsing libraries with rayon...`);
            const archiveId = archiveFingerprint(file);
            const previousArchiveId = libraryRowArchiveIds.get(index) || null;
            const previousArchiveEntry = previousArchiveId
                ? loadedLibraryArchives.get(previousArchiveId) || null
                : null;

            if (previousArchiveId && previousArchiveId !== archiveId) {
                loadedLibraryArchives.delete(previousArchiveId);
            }
            loadedLibraryArchives.set(archiveId, {
                file_name: file.name,
                file_count: fileCount,
                files: data,
            });
            libraryRowArchiveIds.set(index, archiveId);
            rebuildLoadedLibrariesFromArchives();

            try {
                const startTime = performance.now();
                const librariesJson = JSON.stringify(loadedLibraries);
                const resultJson = await sendRequest('loadLibraries', { libraries: librariesJson }, 300000);
                const result = JSON.parse(resultJson);
                const elapsed = ((performance.now() - startTime) / 1000).toFixed(1);

                status.textContent = `${result.parsed_count} files parsed`;
                status.className = 'library-status loaded';
                setLibraryProgress(row, 100, 'Done', false);

                let output = `Parsed ${result.parsed_count} files in ${elapsed}s.\nArchives loaded: ${loadedLibraryArchives.size}`;
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
                    output += `\n\nWARNING: ${result.conflicts.length} library conflicts (replaced): ${result.conflicts.join(', ')}`;
                }
                output += '\n\nReady for compilation!';

                setTerminalOutput(output);
                renderLoadedLibraryList();
                updateLibraryCount();
                await refreshPackageViewer(true);
            } catch (e) {
                loadedLibraryArchives.delete(archiveId);
                if (previousArchiveId && previousArchiveEntry) {
                    loadedLibraryArchives.set(previousArchiveId, previousArchiveEntry);
                    libraryRowArchiveIds.set(index, previousArchiveId);
                } else {
                    libraryRowArchiveIds.delete(index);
                }
                rebuildLoadedLibrariesFromArchives();
                renderLoadedLibraryList();

                status.textContent = 'Parse error';
                status.className = 'library-status error';
                setLibraryProgress(row, 100, 'Parse error', false);
                setTerminalOutput(`Loaded ${fileCount} files but failed to parse: ${e.message}`);
                updateLibraryCount();
            }
        } catch (e) {
            status.textContent = 'Error';
            status.className = 'library-status error';
            setLibraryProgress(row, 100, 'Error', false);
            setTerminalOutput(`Failed to load library: ${e.message}`);
        } finally {
            if (activeLibraryLoadRow === row) {
                activeLibraryLoadRow = null;
            }
        }
    }

    async function clearLibraries() {
        loadedLibraries = {};
        loadedLibraryArchives.clear();
        libraryRowArchiveIds.clear();
        activeLibraryLoadRow = null;

        document.querySelectorAll('.library-row input[type="file"]').forEach(input => {
            input.value = '';
        });
        document.querySelectorAll('.library-row').forEach(row => {
            const status = row.querySelector('.library-status');
            if (status) {
                status.textContent = 'No file selected';
                status.className = 'library-status pending';
            }
            hideLibraryProgress(row);
        });

        try {
            await sendRequest('clearLibraryCache', {});
        } catch (e) {
            console.warn('Failed to clear library cache:', e);
        }

        renderLoadedLibraryList();
        updateLibraryCount(0);
        setTerminalOutput('Libraries cleared.');
        classInfoCache.clear();
        selectedClassQualifiedName = null;
        await refreshPackageViewer(true);
    }

    function handleWorkerProgress({ current, total, percent }) {
        const loadingRow = activeLibraryLoadRow || document.querySelector('.library-row .library-status.loading')?.closest('.library-row');
        const libraryStatus = loadingRow ? loadingRow.querySelector('.library-status') : null;
        if (libraryStatus) {
            libraryStatus.textContent = `Parsing ${current}/${total} (${percent}%)`;
        }
        if (loadingRow) {
            setLibraryProgress(loadingRow, Number(percent) || 0, `Parsing ${current}/${total}`, false);
        }
    }

    function bindWindowApi() {
        window.removeLoadedArchive = removeLoadedArchive;
        window.toggleClassExpand = toggleClassExpand;
        window.selectClass = selectClass;
        window.filterClassTree = filterClassTree;
        window.refreshPackageViewer = refreshPackageViewer;
        window.addLibraryRow = addLibraryRow;
        window.removeLibraryRow = removeLibraryRow;
        window.loadLibraryFile = loadLibraryFile;
        window.clearLibraries = clearLibraries;

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

        renderLoadedLibraryList();
        updateLibraryCount(loadedLibraryArchives.size);
    }

    return {
        bindWindowApi,
        clearLibraries,
        handleWorkerProgress,
        refreshPackageViewer,
        updateLibraryCount,
    };
}
