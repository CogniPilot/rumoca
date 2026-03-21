function escapeHtmlSafe(value) {
    return String(value ?? '')
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#39;');
}

function normalizeSearchText(command) {
    return `${command.label || ''} ${command.description || ''} ${command.detail || ''} ${(
        command.tags || []
    ).join(' ')}`.toLowerCase();
}

function normalizeCommandItems(items) {
    if (!Array.isArray(items)) return [];
    return items
        .filter(item => item && typeof item === 'object' && typeof item.label === 'string')
        .map(item => ({
            label: item.label,
            description: item.description || '',
            detail: item.detail || '',
            shortcut: item.shortcut || '',
            tags: Array.isArray(item.tags) ? item.tags : [],
            run: typeof item.run === 'function' ? item.run : () => {},
        }));
}

function modePlaceholder(mode) {
    if (mode === 'quickOpen') return 'Type a model or class to open';
    if (mode === 'symbolSearch') return 'Type a symbol name to jump to';
    return 'Type a command (e.g., compile, simulate, package archive)';
}

function modeLoadingMessage(mode) {
    if (mode === 'quickOpen') return 'Loading classes and models...';
    if (mode === 'symbolSearch') return 'Loading symbols...';
    return 'Loading commands...';
}

function modeEmptyMessage(mode) {
    if (mode === 'quickOpen') return 'No matching model/class';
    if (mode === 'symbolSearch') return 'No matching symbol';
    return 'No matching commands';
}

function isEditableElement(target) {
    if (!target || !(target instanceof Element)) return false;
    if (target.isContentEditable) return true;
    const tag = target.tagName ? target.tagName.toLowerCase() : '';
    return tag === 'input' || tag === 'textarea' || tag === 'select';
}

function monacoEditorHasTextFocus() {
    return Boolean(
        window.editor
        && typeof window.editor.hasTextFocus === 'function'
        && window.editor.hasTextFocus(),
    );
}

export function setupCommandPalette(options = {}) {
    const {
        getQuickOpenItems = async () => [],
        getSymbolItems = async () => [],
    } = options;

    const overlay = document.getElementById('commandPaletteOverlay');
    const input = document.getElementById('commandPaletteInput');
    const listEl = document.getElementById('commandPaletteList');
    if (!overlay || !input || !listEl) return;

    const state = {
        mode: 'commands',
        commands: [],
        modeItems: [],
        filtered: [],
        activeIndex: 0,
        loadToken: 0,
    };

    function close() {
        if (overlay.hidden) return;
        overlay.hidden = true;
    }

    function currentModeItems() {
        return state.mode === 'commands' ? state.commands : state.modeItems;
    }

    function renderList() {
        if (!state.filtered.length) {
            listEl.innerHTML = `<div class="command-palette-empty">${modeEmptyMessage(state.mode)}</div>`;
            return;
        }

        const items = state.filtered.map((cmd, idx) => {
            const activeClass = idx === state.activeIndex ? ' active' : '';
            const shortcut = cmd.shortcut ? `<kbd>${escapeHtmlSafe(cmd.shortcut)}</kbd>` : '';
            const detail = cmd.detail
                ? `<div style="font-size:11px;color:#9aa0a6;margin-top:2px;">${escapeHtmlSafe(cmd.detail)}</div>`
                : '';
            return `<div class="command-palette-item${activeClass}" data-index="${idx}" title="${escapeHtmlSafe(cmd.description || cmd.label)}">
                <div>
                    <span>${escapeHtmlSafe(cmd.label)}</span>
                    ${detail}
                </div>
                ${shortcut}
            </div>`;
        });

        listEl.innerHTML = items.join('');
        listEl.querySelectorAll('.command-palette-item[data-index]').forEach(item => {
            item.addEventListener('mouseenter', () => {
                const idx = Number(item.dataset.index);
                if (Number.isFinite(idx)) {
                    state.activeIndex = idx;
                    renderList();
                }
            });

            item.addEventListener('click', () => {
                const idx = Number(item.dataset.index);
                if (!Number.isFinite(idx)) return;
                const cmd = state.filtered[idx];
                if (!cmd) return;
                close();
                Promise.resolve().then(() => cmd.run());
            });
        });
    }

    function filterList() {
        const query = String(input.value || '').trim().toLowerCase();
        const sourceItems = currentModeItems();
        if (!query) {
            state.filtered = [...sourceItems];
        } else {
            state.filtered = sourceItems.filter(cmd => normalizeSearchText(cmd).includes(query));
        }
        state.activeIndex = 0;
        renderList();
    }

    function openCommands() {
        state.mode = 'commands';
        state.modeItems = [];
        overlay.hidden = false;
        input.placeholder = modePlaceholder('commands');
        input.value = '';
        filterList();
        setTimeout(() => input.focus(), 0);
    }

    async function openAsyncMode(mode, loader) {
        state.mode = mode;
        state.modeItems = [];
        overlay.hidden = false;
        input.placeholder = modePlaceholder(mode);
        input.value = '';
        listEl.innerHTML = `<div class="command-palette-empty">${modeLoadingMessage(mode)}</div>`;
        setTimeout(() => input.focus(), 0);

        const loadToken = ++state.loadToken;
        try {
            const loadedItems = normalizeCommandItems(await loader());
            if (loadToken !== state.loadToken || state.mode !== mode) return;
            state.modeItems = loadedItems;
            filterList();
        } catch (error) {
            if (loadToken !== state.loadToken || state.mode !== mode) return;
            const message = error && error.message ? error.message : String(error || 'unknown error');
            state.modeItems = [];
            state.filtered = [];
            listEl.innerHTML = `<div class="command-palette-empty">Failed to load: ${escapeHtmlSafe(message)}</div>`;
        }
    }

    function openQuickOpen() {
        return openAsyncMode('quickOpen', getQuickOpenItems);
    }

    function openSymbolSearch() {
        return openAsyncMode('symbolSearch', getSymbolItems);
    }

    state.commands = [
        {
            label: 'Project: New',
            description: 'Create a new in-browser project workspace',
            tags: ['project', 'workspace', 'new'],
            run: () => {
                if (typeof window.newProject === 'function') {
                    void window.newProject();
                }
            },
        },
        {
            label: 'Project: Download',
            description: 'Download the current in-browser project as a zip archive',
            tags: ['project', 'workspace', 'download', 'zip'],
            run: () => {
                if (typeof window.downloadProject === 'function') {
                    void window.downloadProject();
                }
            },
        },
        {
            label: 'Project: Load Archive',
            description: 'Load a project zip archive into the in-browser workspace',
            tags: ['project', 'workspace', 'load', 'zip'],
            run: () => {
                if (typeof window.loadProject === 'function') {
                    window.loadProject();
                }
            },
        },
        {
            label: 'Project: Import Folder',
            description: 'Import a project folder into the in-browser workspace',
            tags: ['project', 'workspace', 'folder', 'import'],
            run: () => {
                if (typeof window.loadProjectFolder === 'function') {
                    void window.loadProjectFolder();
                }
            },
        },
        {
            label: 'Compile: Run Live Checks Now',
            description: 'Force immediate diagnostics and compile pass',
            shortcut: 'Ctrl+Enter',
            tags: ['compile', 'check', 'diagnostics'],
            run: () => {
                if (typeof window.triggerCompileNow === 'function') {
                    window.triggerCompileNow();
                }
            },
        },
        {
            label: 'Simulate: Run Selected Model',
            description: 'Run simulation for the selected model',
            shortcut: 'F5',
            tags: ['simulate', 'run'],
            run: () => {
                if (typeof window.switchRightTab === 'function') window.switchRightTab('simulate');
                if (typeof window.runSimulation === 'function') window.runSimulation();
            },
        },
        {
            label: 'Project: Import Package Archive',
            description: 'Open the file picker to import a Modelica package ZIP into the workspace',
            tags: ['package', 'archive', 'msl', 'zip'],
            run: () => {
                if (typeof window.openPackageArchivePicker === 'function') {
                    window.openPackageArchivePicker();
                }
            },
        },
        {
            label: 'Documentation: Refresh Class Tree',
            description: 'Reload package/class documentation tree from current cache',
            tags: ['packages', 'docs', 'refresh'],
            run: () => {
                if (typeof window.refreshPackageViewer === 'function') window.refreshPackageViewer(true);
            },
        },
        {
            label: 'Problems: Show Errors Panel',
            description: 'Open errors/problems panel',
            tags: ['problems', 'errors'],
            run: () => {
                if (typeof window.switchBottomTab === 'function') window.switchBottomTab('errors');
            },
        },
        {
            label: 'Quick Fix: At Cursor',
            description: 'Show language-server quick fixes at the active cursor',
            shortcut: 'Ctrl+.',
            tags: ['quickfix', 'code action', 'lsp'],
            run: () => {
                if (typeof window.triggerQuickFixAtCursor === 'function') {
                    void window.triggerQuickFixAtCursor();
                }
            },
        },
        {
            label: 'Problems: Next',
            description: 'Jump to next problem',
            shortcut: 'F8',
            tags: ['problems', 'next'],
            run: () => {
                if (typeof window.nextProblem === 'function') window.nextProblem();
            },
        },
        {
            label: 'Problems: Previous',
            description: 'Jump to previous problem',
            shortcut: 'Shift+F8',
            tags: ['problems', 'prev', 'previous'],
            run: () => {
                if (typeof window.previousProblem === 'function') window.previousProblem();
            },
        },
    ];
    state.commands = normalizeCommandItems(state.commands);

    input.addEventListener('input', () => filterList());
    input.addEventListener('keydown', event => {
        if (event.key === 'ArrowDown') {
            event.preventDefault();
            if (!state.filtered.length) return;
            state.activeIndex = (state.activeIndex + 1) % state.filtered.length;
            renderList();
            return;
        }
        if (event.key === 'ArrowUp') {
            event.preventDefault();
            if (!state.filtered.length) return;
            state.activeIndex = (state.activeIndex - 1 + state.filtered.length) % state.filtered.length;
            renderList();
            return;
        }
        if (event.key === 'Enter') {
            event.preventDefault();
            const cmd = state.filtered[state.activeIndex];
            if (!cmd) return;
            close();
            Promise.resolve().then(() => cmd.run());
            return;
        }
        if (event.key === 'Escape') {
            event.preventDefault();
            close();
        }
    });

    overlay.addEventListener('click', event => {
        if (event.target === overlay) close();
    });

    window.addEventListener('keydown', event => {
        if (event.key === 'Escape' && !overlay.hidden) {
            event.preventDefault();
            close();
            return;
        }

        if (
            overlay.hidden
            && isEditableElement(event.target)
            && !monacoEditorHasTextFocus()
        ) {
            return;
        }

        if (event.altKey) return;
        const primaryMod = event.ctrlKey || event.metaKey;
        if (!primaryMod) return;

        if (event.shiftKey && (event.key === 'P' || event.key === 'p')) {
            event.preventDefault();
            openCommands();
            return;
        }
        if (!event.shiftKey && (event.key === 'P' || event.key === 'p')) {
            event.preventDefault();
            void openQuickOpen();
            return;
        }
        if (event.shiftKey && (event.key === 'O' || event.key === 'o')) {
            event.preventDefault();
            void openSymbolSearch();
        }
    });
}
