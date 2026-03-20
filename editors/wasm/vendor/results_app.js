(function(root, factory) {
    if (typeof module === 'object' && module.exports) {
        module.exports = factory();
        return;
    }
    root.RumocaResultsApp = factory();
}(typeof globalThis !== 'undefined' ? globalThis : this, function() {
    const PALETTE = [
        '#4ec9b0', '#569cd6', '#ce9178', '#dcdcaa', '#c586c0',
        '#9cdcfe', '#d7ba7d', '#608b4e', '#d16969', '#b5cea8',
    ];

    function sharedVisualization() {
        const shared = globalThis.RumocaVisualizationShared;
        if (!shared) {
            throw new Error('RumocaVisualizationShared not loaded');
        }
        return shared;
    }

    function trimMaybeString(value) {
        return typeof value === 'string' ? value.trim() : '';
    }

    function formatNum(value) {
        const n = Number(value);
        if (!Number.isFinite(n)) {
            return String(value);
        }
        if (Math.abs(n) >= 1000 || (Math.abs(n) > 0 && Math.abs(n) < 0.001)) {
            return n.toExponential(3);
        }
        return n.toFixed(4).replace(/\.?0+$/, '');
    }

    function escapeHtml(text) {
        return String(text)
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;');
    }

    function sanitizeDownloadBaseName(name) {
        const text = String(name || 'rumoca_plot').trim();
        const cleaned = text.replace(/[^a-zA-Z0-9._-]+/g, '_').replace(/^_+|_+$/g, '');
        return cleaned.length > 0 ? cleaned : 'rumoca_plot';
    }

    const RESULTS_APP_BRIDGE_METHODS = Object.freeze([
        'loadViews',
        'saveViews',
        'resetViews',
        'savePng',
        'saveWebm',
        'notify',
    ]);

    function deepClone(value) {
        return JSON.parse(JSON.stringify(value));
    }

    function safeViewType(value) {
        const text = trimMaybeString(value).toLowerCase();
        if (text === 'scatter' || text === '3d') {
            return text;
        }
        return 'timeseries';
    }

    function defaultResultsView(type, index) {
        const viewType = safeViewType(type);
        const nextIndex = Number.isFinite(index) ? Math.max(0, Math.floor(index)) : 0;
        const suffix = nextIndex + 1;
        if (viewType === 'scatter') {
            return {
                id: `scatter_${suffix}`,
                title: 'Scatter',
                type: 'scatter',
                x: 'time',
                y: ['x'],
                scatterSeries: [{ name: 'x vs time', x: 'time', y: 'x' }],
            };
        }
        if (viewType === '3d') {
            return {
                id: `viewer_${suffix}`,
                title: '3D View',
                type: '3d',
                x: 'x',
                y: ['y', 'z'],
            };
        }
        return {
            id: nextIndex === 0 ? 'states_time' : `timeseries_${suffix}`,
            title: nextIndex === 0 ? 'States vs Time' : 'Time Series',
            type: 'timeseries',
            x: 'time',
            y: ['*states'],
        };
    }

    function parseSeriesList(text) {
        return String(text || '')
            .split(/[\n,]/)
            .map(trimMaybeString)
            .filter(Boolean);
    }

    function formatSeriesList(values) {
        return ensureArray(values).map(trimMaybeString).filter(Boolean).join(', ');
    }

    function parseScatterSeriesText(text) {
        const series = [];
        const lines = String(text || '').split(/\n/);
        for (const rawLine of lines) {
            const line = trimMaybeString(rawLine);
            if (!line) {
                continue;
            }
            const parts = line.split('|').map(trimMaybeString);
            if (parts.length < 3 || !parts[1] || !parts[2]) {
                continue;
            }
            series.push({
                name: parts[0] || `${parts[2]} vs ${parts[1]}`,
                x: parts[1],
                y: parts[2],
            });
        }
        return series;
    }

    function formatScatterSeriesText(values) {
        return ensureArray(values)
            .filter(function(entry) {
                return entry && typeof entry === 'object';
            })
            .map(function(entry) {
                const name = trimMaybeString(entry.name);
                const x = trimMaybeString(entry.x);
                const y = trimMaybeString(entry.y);
                if (!x || !y) {
                    return '';
                }
                return `${name || `${y} vs ${x}`} | ${x} | ${y}`;
            })
            .filter(Boolean)
            .join('\n');
    }

    function normalizeResultsViewDrafts(shared, drafts) {
        const normalizedDrafts = [];
        const safeDrafts = Array.isArray(drafts) ? drafts : [];
        for (let index = 0; index < safeDrafts.length; index += 1) {
            const draft = safeDrafts[index] && typeof safeDrafts[index] === 'object'
                ? safeDrafts[index]
                : defaultResultsView('timeseries', index);
            const type = safeViewType(draft.type);
            const title = trimMaybeString(draft.title) || defaultResultsView(type, index).title;
            const candidate = {
                id: trimMaybeString(draft.id) || defaultResultsView(type, index).id,
                title: title,
                type: type,
                x: trimMaybeString(draft.x) || undefined,
                y: parseSeriesList(draft.yText !== undefined ? draft.yText : formatSeriesList(draft.y)),
                scatterSeries: type === 'scatter'
                    ? parseScatterSeriesText(draft.scatterSeriesText)
                    : undefined,
                scriptPath: type === '3d'
                    ? (trimMaybeString(draft.scriptPath) || undefined)
                    : undefined,
            };
            if (type === '3d') {
                candidate.y = candidate.y.slice(0, 2);
            }
            normalizedDrafts.push(candidate);
        }
        return shared.normalizeVisualizationViews(normalizedDrafts);
    }

    function chooseActiveViewId(views, preferredId) {
        if (!Array.isArray(views) || views.length === 0) {
            return null;
        }
        const wanted = trimMaybeString(preferredId);
        if (wanted.length > 0 && views.some((view) => view && view.id === wanted)) {
            return wanted;
        }
        return views[0].id;
    }

    function buildResultsTimingSummary(metrics) {
        if (!metrics || typeof metrics !== 'object') {
            return [];
        }
        const summary = [];
        const compileSeconds = Number(metrics.compileSeconds);
        const simulateSeconds = Number(metrics.simulateSeconds);
        const points = Number(metrics.points);
        const variables = Number(metrics.variables);
        if (Number.isFinite(compileSeconds)) {
            summary.push(`compile ${formatNum(compileSeconds)}s`);
        }
        if (Number.isFinite(simulateSeconds)) {
            summary.push(`simulate ${formatNum(simulateSeconds)}s`);
        }
        if (Number.isFinite(points)) {
            summary.push(`points ${Math.max(0, Math.floor(points))}`);
        }
        if (Number.isFinite(variables)) {
            summary.push(`vars ${Math.max(0, Math.floor(variables))}`);
        }
        return summary;
    }

    function createNoopResultsHostBridge(bridge) {
        const overrides = bridge && typeof bridge === 'object' ? bridge : {};
        return {
            loadViews: typeof overrides.loadViews === 'function'
                ? overrides.loadViews
                : async function() { return undefined; },
            saveViews: typeof overrides.saveViews === 'function'
                ? overrides.saveViews
                : async function(_modelRef, views) { return views; },
            resetViews: typeof overrides.resetViews === 'function'
                ? overrides.resetViews
                : async function() { return undefined; },
            persistState: typeof overrides.persistState === 'function' ? overrides.persistState : function() {},
            notify: typeof overrides.notify === 'function' ? overrides.notify : function() {},
            savePng: typeof overrides.savePng === 'function' ? overrides.savePng : null,
            saveWebm: typeof overrides.saveWebm === 'function' ? overrides.saveWebm : null,
        };
    }

    function ensureArray(values) {
        return Array.isArray(values) ? values : [];
    }

    function renderLegend(legendEl, series) {
        legendEl.innerHTML = '';
        for (const item of ensureArray(series)) {
            const row = document.createElement('span');
            row.style.display = 'inline-flex';
            row.style.alignItems = 'center';
            row.style.gap = '6px';
            row.style.marginRight = '10px';
            const swatch = document.createElement('span');
            swatch.className = 'rumoca-results-swatch';
            swatch.style.background = item.color;
            const label = document.createElement('span');
            label.textContent = item.name;
            row.appendChild(swatch);
            row.appendChild(label);
            legendEl.appendChild(row);
        }
    }

    function buildDetailsText(metrics, payload, viewModel) {
        const lines = [];
        const simDetails = payload && payload.simDetails ? payload.simDetails : {};
        const actual = simDetails.actual || {};
        const requested = simDetails.requested || {};
        if (viewModel && viewModel.type === 'scatter') {
            lines.push(`Scatter series: ${viewModel.y.length}`);
        } else if (viewModel && viewModel.type === '3d') {
            lines.push(`3D points: ${viewModel.points.length}`);
        } else if (viewModel && viewModel.y) {
            lines.push(`Series: ${viewModel.y.length}`);
        }
        lines.push(`Points: ${Number(metrics && metrics.points) || 0}`);
        lines.push(`Variables: ${Number(metrics && metrics.variables) || 0}`);
        lines.push('');
        lines.push('Actual');
        if (actual.t_start !== undefined) lines.push(`  t_start: ${formatNum(actual.t_start)}`);
        if (actual.t_end !== undefined) lines.push(`  t_end: ${formatNum(actual.t_end)}`);
        lines.push('');
        lines.push('Requested');
        if (requested.solver !== undefined) lines.push(`  solver: ${requested.solver}`);
        if (requested.t_start !== undefined) lines.push(`  t_start: ${formatNum(requested.t_start)}`);
        if (requested.t_end !== undefined) lines.push(`  t_end: ${formatNum(requested.t_end)}`);
        if (requested.dt !== undefined) lines.push(`  dt: ${formatNum(requested.dt)}`);
        return lines.join('\n');
    }

    function fallbackDownload(dataUrl, defaultName) {
        if (typeof document === 'undefined') {
            return;
        }
        const link = document.createElement('a');
        link.href = dataUrl;
        link.download = defaultName;
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
    }

    function buildPngDataUrlFromCanvas(canvas) {
        if (!canvas || typeof canvas.toDataURL !== 'function') {
            return undefined;
        }
        return canvas.toDataURL('image/png');
    }

    function createDetailsModal(detailsTextBuilder) {
        const modal = document.createElement('div');
        modal.className = 'rumoca-results-details';
        const card = document.createElement('div');
        card.className = 'rumoca-results-details-card';
        const pre = document.createElement('pre');
        const close = document.createElement('button');
        close.textContent = 'Close';
        card.appendChild(pre);
        card.appendChild(close);
        modal.appendChild(card);
        close.addEventListener('click', function() {
            modal.classList.remove('open');
        });
        modal.addEventListener('click', function(event) {
            if (event.target === modal) {
                modal.classList.remove('open');
            }
        });
        return {
            element: modal,
            open: function() {
                pre.textContent = detailsTextBuilder();
                modal.classList.add('open');
            },
        };
    }

    function createStatusBanner() {
        const banner = document.createElement('div');
        banner.className = 'rumoca-results-status';
        return {
            element: banner,
            show: function(message, tone) {
                const text = trimMaybeString(message);
                banner.className = `rumoca-results-status${text ? ' open' : ''}${tone ? ` ${tone}` : ''}`;
                banner.textContent = text;
            },
            clear: function() {
                banner.className = 'rumoca-results-status';
                banner.textContent = '';
            },
        };
    }

    function extractBridgeViews(result) {
        if (Array.isArray(result)) {
            return result;
        }
        if (result && typeof result === 'object' && Array.isArray(result.views)) {
            return result.views;
        }
        return undefined;
    }

    function createSettingsModal(options) {
        const shared = options.shared;
        const bridge = options.bridge;
        const modelRef = options.modelRef;
        const modal = document.createElement('div');
        modal.className = 'rumoca-results-settings';

        const card = document.createElement('div');
        card.className = 'rumoca-results-settings-card';
        modal.appendChild(card);

        const header = document.createElement('div');
        header.className = 'rumoca-results-settings-header';
        const title = document.createElement('div');
        title.className = 'rumoca-results-settings-title';
        title.textContent = 'Visualization Settings';
        const status = document.createElement('div');
        status.className = 'rumoca-results-settings-status';
        header.appendChild(title);
        header.appendChild(status);
        card.appendChild(header);

        const body = document.createElement('div');
        body.className = 'rumoca-results-settings-body';
        card.appendChild(body);

        const sidebar = document.createElement('div');
        sidebar.className = 'rumoca-results-settings-sidebar';
        const viewList = document.createElement('div');
        viewList.className = 'rumoca-results-settings-list';
        const sidebarButtons = document.createElement('div');
        sidebarButtons.className = 'rumoca-results-settings-actions';
        const addTimeseriesBtn = document.createElement('button');
        addTimeseriesBtn.textContent = 'Add Time';
        const addScatterBtn = document.createElement('button');
        addScatterBtn.textContent = 'Add Scatter';
        const addThreeDBtn = document.createElement('button');
        addThreeDBtn.textContent = 'Add 3D';
        const removeBtn = document.createElement('button');
        removeBtn.textContent = 'Remove';
        sidebarButtons.appendChild(addTimeseriesBtn);
        sidebarButtons.appendChild(addScatterBtn);
        sidebarButtons.appendChild(addThreeDBtn);
        sidebarButtons.appendChild(removeBtn);
        sidebar.appendChild(viewList);
        sidebar.appendChild(sidebarButtons);
        body.appendChild(sidebar);

        const editor = document.createElement('div');
        editor.className = 'rumoca-results-settings-editor';
        body.appendChild(editor);

        function buildField(labelText, input) {
            const field = document.createElement('label');
            field.className = 'rumoca-results-field';
            const label = document.createElement('span');
            label.className = 'rumoca-results-field-label';
            label.textContent = labelText;
            field.appendChild(label);
            field.appendChild(input);
            return field;
        }

        const titleInput = document.createElement('input');
        const typeInput = document.createElement('select');
        for (const type of ['timeseries', 'scatter', '3d']) {
            const option = document.createElement('option');
            option.value = type;
            option.textContent = type;
            typeInput.appendChild(option);
        }
        const xInput = document.createElement('input');
        const yInput = document.createElement('textarea');
        yInput.rows = 3;
        const scatterInput = document.createElement('textarea');
        scatterInput.rows = 4;
        const scatterHint = document.createElement('div');
        scatterHint.className = 'rumoca-results-field-hint';
        scatterHint.textContent = 'One line per series: name | x | y';
        const scriptPathInput = document.createElement('input');

        const titleField = buildField('Title', titleInput);
        const typeField = buildField('Type', typeInput);
        const xField = buildField('X', xInput);
        const yField = buildField('Y', yInput);
        const scatterField = buildField('Scatter Series', scatterInput);
        scatterField.appendChild(scatterHint);
        const scriptPathField = buildField('3D Script Path', scriptPathInput);
        editor.appendChild(titleField);
        editor.appendChild(typeField);
        editor.appendChild(xField);
        editor.appendChild(yField);
        editor.appendChild(scatterField);
        editor.appendChild(scriptPathField);

        const footer = document.createElement('div');
        footer.className = 'rumoca-results-settings-actions';
        const resetBtn = document.createElement('button');
        resetBtn.textContent = 'Reset';
        const cancelBtn = document.createElement('button');
        cancelBtn.textContent = 'Cancel';
        const saveBtn = document.createElement('button');
        saveBtn.textContent = 'Save';
        saveBtn.className = 'primary';
        footer.appendChild(resetBtn);
        footer.appendChild(cancelBtn);
        footer.appendChild(saveBtn);
        card.appendChild(footer);

        let drafts = [];
        let selectedIndex = -1;

        function setStatus(message, tone) {
            const text = trimMaybeString(message);
            status.className = `rumoca-results-settings-status${text ? ' open' : ''}${tone ? ` ${tone}` : ''}`;
            status.textContent = text;
        }

        function syncDraftFromInputs() {
            if (selectedIndex < 0 || selectedIndex >= drafts.length) {
                return;
            }
            const draft = drafts[selectedIndex];
            draft.title = titleInput.value;
            draft.type = typeInput.value;
            draft.x = xInput.value;
            draft.yText = yInput.value;
            draft.scatterSeriesText = scatterInput.value;
            draft.scriptPath = scriptPathInput.value;
        }

        function applyTypeVisibility(type) {
            const viewType = safeViewType(type);
            xField.style.display = viewType === 'scatter' ? 'none' : 'grid';
            yField.style.display = viewType === 'scatter' ? 'none' : 'grid';
            scatterField.style.display = viewType === 'scatter' ? 'grid' : 'none';
            scriptPathField.style.display = viewType === '3d' ? 'grid' : 'none';
        }

        function renderList() {
            viewList.innerHTML = '';
            for (let index = 0; index < drafts.length; index += 1) {
                const draft = drafts[index];
                const row = document.createElement('button');
                row.className = `rumoca-results-settings-item${index === selectedIndex ? ' active' : ''}`;
                row.textContent = `${trimMaybeString(draft.title) || draft.id} [${safeViewType(draft.type)}]`;
                row.addEventListener('click', function() {
                    syncDraftFromInputs();
                    selectedIndex = index;
                    renderList();
                    renderEditor();
                });
                viewList.appendChild(row);
            }
        }

        function renderEditor() {
            if (selectedIndex < 0 || selectedIndex >= drafts.length) {
                titleInput.value = '';
                typeInput.value = 'timeseries';
                xInput.value = '';
                yInput.value = '';
                scatterInput.value = '';
                scriptPathInput.value = '';
                applyTypeVisibility('timeseries');
                return;
            }
            const draft = drafts[selectedIndex];
            titleInput.value = trimMaybeString(draft.title);
            typeInput.value = safeViewType(draft.type);
            xInput.value = trimMaybeString(draft.x);
            yInput.value = draft.yText !== undefined ? String(draft.yText) : formatSeriesList(draft.y);
            scatterInput.value = draft.scatterSeriesText !== undefined
                ? String(draft.scatterSeriesText)
                : formatScatterSeriesText(draft.scatterSeries);
            scriptPathInput.value = trimMaybeString(draft.scriptPath);
            applyTypeVisibility(draft.type);
        }

        function rebuildDrafts(rawViews) {
            const normalized = shared.normalizeVisualizationViews(rawViews);
            drafts = normalized.map(function(view, index) {
                const base = defaultResultsView(view.type, index);
                return {
                    id: view.id || base.id,
                    title: view.title || base.title,
                    type: view.type || base.type,
                    x: view.x || base.x || '',
                    y: ensureArray(view.y),
                    yText: formatSeriesList(view.y),
                    scatterSeries: ensureArray(view.scatterSeries),
                    scatterSeriesText: formatScatterSeriesText(view.scatterSeries),
                    scriptPath: trimMaybeString(view.scriptPath),
                };
            });
            if (drafts.length === 0) {
                rebuildDrafts(shared.defaultVisualizationViews());
                return;
            }
            if (selectedIndex < 0 || selectedIndex >= drafts.length) {
                selectedIndex = 0;
            }
            renderList();
            renderEditor();
        }

        function createDraft(type) {
            const next = defaultResultsView(type, drafts.length);
            return {
                id: next.id,
                title: next.title,
                type: next.type,
                x: next.x || '',
                y: ensureArray(next.y),
                yText: formatSeriesList(next.y),
                scatterSeries: ensureArray(next.scatterSeries),
                scatterSeriesText: formatScatterSeriesText(next.scatterSeries),
                scriptPath: trimMaybeString(next.scriptPath),
            };
        }

        function close() {
            modal.classList.remove('open');
            setStatus('', '');
        }

        async function open(currentViews) {
            let incomingViews = currentViews;
            if (bridge.loadViews) {
                try {
                    const loaded = await bridge.loadViews(modelRef);
                    const loadedViews = extractBridgeViews(loaded);
                    if (loadedViews) {
                        incomingViews = loadedViews;
                    }
                } catch (error) {
                    setStatus(String(error && error.message ? error.message : error), 'error');
                }
            }
            rebuildDrafts(incomingViews);
            modal.classList.add('open');
        }

        function normalizedDraftViews() {
            syncDraftFromInputs();
            return normalizeResultsViewDrafts(shared, drafts);
        }

        addTimeseriesBtn.addEventListener('click', function() {
            syncDraftFromInputs();
            drafts.push(createDraft('timeseries'));
            selectedIndex = drafts.length - 1;
            renderList();
            renderEditor();
        });
        addScatterBtn.addEventListener('click', function() {
            syncDraftFromInputs();
            drafts.push(createDraft('scatter'));
            selectedIndex = drafts.length - 1;
            renderList();
            renderEditor();
        });
        addThreeDBtn.addEventListener('click', function() {
            syncDraftFromInputs();
            drafts.push(createDraft('3d'));
            selectedIndex = drafts.length - 1;
            renderList();
            renderEditor();
        });
        removeBtn.addEventListener('click', function() {
            if (selectedIndex < 0 || selectedIndex >= drafts.length) {
                return;
            }
            drafts.splice(selectedIndex, 1);
            if (selectedIndex >= drafts.length) {
                selectedIndex = drafts.length - 1;
            }
            if (drafts.length === 0) {
                drafts.push(createDraft('timeseries'));
                selectedIndex = 0;
            }
            renderList();
            renderEditor();
        });
        typeInput.addEventListener('change', function() {
            applyTypeVisibility(typeInput.value);
        });
        cancelBtn.addEventListener('click', close);
        modal.addEventListener('click', function(event) {
            if (event.target === modal) {
                close();
            }
        });
        resetBtn.addEventListener('click', async function() {
            try {
                let nextViews = shared.defaultVisualizationViews();
                if (bridge.resetViews) {
                    const result = await bridge.resetViews(modelRef);
                    const resetViews = extractBridgeViews(result);
                    if (resetViews) {
                        nextViews = resetViews;
                    }
                }
                rebuildDrafts(nextViews);
                setStatus('Views reset.', 'ok');
                bridge.notify('Views reset.');
            } catch (error) {
                setStatus(String(error && error.message ? error.message : error), 'error');
            }
        });
        saveBtn.addEventListener('click', async function() {
            try {
                const nextViews = normalizedDraftViews();
                let savedViews = nextViews;
                if (bridge.saveViews) {
                    const result = await bridge.saveViews(modelRef, nextViews);
                    const bridgeViews = extractBridgeViews(result);
                    if (bridgeViews) {
                        savedViews = bridgeViews;
                    }
                }
                if (typeof options.onSave === 'function') {
                    options.onSave(savedViews);
                }
                bridge.notify('Views saved.');
                close();
            } catch (error) {
                setStatus(String(error && error.message ? error.message : error), 'error');
            }
        });

        return {
            element: modal,
            open: open,
            close: close,
        };
    }

    function createTimeseriesView(options) {
        const viewModel = options.viewModel;
        const bridge = options.bridge;
        const modelName = options.modelName;
        const metrics = options.metrics;
        const payload = options.payload;
        const root = document.createElement('section');
        root.className = 'rumoca-results-view';

        const wrap = document.createElement('div');
        wrap.className = 'rumoca-results-canvas-wrap';
        const plotHost = document.createElement('div');
        plotHost.style.position = 'absolute';
        plotHost.style.inset = '0';
        const overlay = document.createElement('div');
        overlay.className = 'rumoca-results-overlay';
        overlay.textContent = 'Hover plot for values';
        const legend = document.createElement('div');
        legend.className = 'rumoca-results-legend';
        const error = document.createElement('div');
        error.className = 'rumoca-results-error';
        error.style.display = 'none';
        wrap.appendChild(plotHost);
        wrap.appendChild(overlay);
        wrap.appendChild(legend);
        wrap.appendChild(error);
        root.appendChild(wrap);

        const controls = document.createElement('div');
        controls.className = 'rumoca-results-controls';
        const detailsBtn = document.createElement('button');
        detailsBtn.textContent = 'Run Details';
        const saveBtn = document.createElement('button');
        saveBtn.textContent = 'Save PNG';
        controls.appendChild(detailsBtn);
        controls.appendChild(saveBtn);
        root.appendChild(controls);

        const details = createDetailsModal(function() {
            return buildDetailsText(metrics, payload, viewModel);
        });
        root.appendChild(details.element);
        detailsBtn.addEventListener('click', function() {
            details.open();
        });

        let plot = null;

        function rebuild() {
            if (plot) {
                plot.destroy();
                plot = null;
            }
            if (!globalThis.uPlot || !viewModel.x || viewModel.y.length === 0) {
                error.style.display = 'flex';
                error.textContent = 'uPlot unavailable or no time-series data.';
                plotHost.innerHTML = '';
                legend.innerHTML = '';
                return;
            }
            error.style.display = 'none';
            const data = [viewModel.x.values];
            const series = [{}];
            const legendSeries = [];
            for (let index = 0; index < viewModel.y.length; index += 1) {
                const item = viewModel.y[index];
                const color = item.color || PALETTE[index % PALETTE.length];
                data.push(item.values);
                series.push({ label: item.name, stroke: color, width: 1.5 });
                legendSeries.push({ name: item.name, color: color });
            }
            renderLegend(legend, legendSeries);
            plot = new globalThis.uPlot({
                width: Math.max(320, plotHost.clientWidth || 640),
                height: Math.max(220, plotHost.clientHeight || 320),
                padding: [8, 8, 28, 8],
                scales: { x: { time: false } },
                axes: [
                    { stroke: '#888', grid: { stroke: '#333' }, label: viewModel.x.name, labelGap: 2, size: 36 },
                    { stroke: '#888', grid: { stroke: '#333' } },
                ],
                series: series,
                cursor: { drag: { x: true, y: true } },
                hooks: {
                    setCursor: [function(u) {
                        const idx = u.cursor.idx;
                        if (idx == null || idx < 0 || idx >= viewModel.x.values.length) {
                            overlay.textContent = 'Hover plot for values';
                            return;
                        }
                        const parts = [`${viewModel.x.name}=${formatNum(viewModel.x.values[idx])}`];
                        for (const item of viewModel.y) {
                            parts.push(`${item.name}=${formatNum(item.values[idx])}`);
                        }
                        overlay.textContent = parts.join(' | ');
                    }],
                },
                legend: { show: false },
            }, data, plotHost);
        }

        saveBtn.addEventListener('click', function() {
            if (!plot || !plot.root) {
                return;
            }
            const canvas = plot.root.querySelector('canvas');
            const dataUrl = buildPngDataUrlFromCanvas(canvas);
            if (!dataUrl) {
                return;
            }
            const fileName = `${sanitizeDownloadBaseName(modelName)}_plot.png`;
            if (bridge.savePng) {
                bridge.savePng({ dataUrl: dataUrl, defaultName: fileName, view: viewModel });
                return;
            }
            fallbackDownload(dataUrl, fileName);
        });

        root.__dispose = function() {
            if (plot) {
                plot.destroy();
                plot = null;
            }
        };
        root.__resize = rebuild;
        rebuild();
        return root;
    }

    function scatterBounds(values) {
        let min = Number.POSITIVE_INFINITY;
        let max = Number.NEGATIVE_INFINITY;
        for (const value of ensureArray(values)) {
            const n = Number(value);
            if (!Number.isFinite(n)) {
                continue;
            }
            min = Math.min(min, n);
            max = Math.max(max, n);
        }
        if (!Number.isFinite(min) || !Number.isFinite(max)) {
            return { min: -1, max: 1 };
        }
        if (min === max) {
            return { min: min - 1, max: max + 1 };
        }
        return { min: min, max: max };
    }

    function createScatterView(options) {
        const viewModel = options.viewModel;
        const bridge = options.bridge;
        const modelName = options.modelName;
        const metrics = options.metrics;
        const payload = options.payload;
        const root = document.createElement('section');
        root.className = 'rumoca-results-view';

        const wrap = document.createElement('div');
        wrap.className = 'rumoca-results-canvas-wrap';
        const canvas = document.createElement('canvas');
        canvas.className = 'rumoca-results-canvas';
        const overlay = document.createElement('div');
        overlay.className = 'rumoca-results-overlay';
        const legend = document.createElement('div');
        legend.className = 'rumoca-results-legend';
        const error = document.createElement('div');
        error.className = 'rumoca-results-error';
        error.style.display = 'none';
        wrap.appendChild(canvas);
        wrap.appendChild(overlay);
        wrap.appendChild(legend);
        wrap.appendChild(error);
        root.appendChild(wrap);

        const controls = document.createElement('div');
        controls.className = 'rumoca-results-controls';
        const detailsBtn = document.createElement('button');
        detailsBtn.textContent = 'Run Details';
        const saveBtn = document.createElement('button');
        saveBtn.textContent = 'Save PNG';
        controls.appendChild(detailsBtn);
        controls.appendChild(saveBtn);
        root.appendChild(controls);

        const details = createDetailsModal(function() {
            return buildDetailsText(metrics, payload, viewModel);
        });
        root.appendChild(details.element);
        detailsBtn.addEventListener('click', function() {
            details.open();
        });

        function rebuild() {
            const ctx = canvas.getContext('2d');
            if (!ctx || !viewModel.x || viewModel.y.length === 0) {
                error.style.display = 'flex';
                error.textContent = 'No scatter data configured.';
                legend.innerHTML = '';
                return;
            }
            error.style.display = 'none';
            const width = Math.max(320, wrap.clientWidth || 640);
            const height = Math.max(220, wrap.clientHeight || 320);
            canvas.width = width;
            canvas.height = height;
            const allX = viewModel.x.values;
            const allY = viewModel.y.flatMap(function(series) { return series.values; });
            const xb = scatterBounds(allX);
            const yb = scatterBounds(allY);
            const pad = 36;
            ctx.fillStyle = '#1e1e1e';
            ctx.fillRect(0, 0, width, height);
            ctx.strokeStyle = '#555';
            ctx.beginPath();
            ctx.moveTo(pad, height - pad);
            ctx.lineTo(width - 10, height - pad);
            ctx.moveTo(pad, height - pad);
            ctx.lineTo(pad, 10);
            ctx.stroke();
            const mapX = function(value) {
                return pad + ((value - xb.min) / (xb.max - xb.min)) * (width - pad - 16);
            };
            const mapY = function(value) {
                return height - pad - ((value - yb.min) / (yb.max - yb.min)) * (height - pad - 16);
            };
            const legendSeries = [];
            for (let index = 0; index < viewModel.y.length; index += 1) {
                const series = viewModel.y[index];
                const color = series.color || PALETTE[index % PALETTE.length];
                legendSeries.push({ name: series.name, color: color });
                ctx.fillStyle = color;
                const count = Math.min(viewModel.x.values.length, series.values.length);
                for (let pointIndex = 0; pointIndex < count; pointIndex += 1) {
                    const x = mapX(Number(viewModel.x.values[pointIndex]));
                    const y = mapY(Number(series.values[pointIndex]));
                    ctx.beginPath();
                    ctx.arc(x, y, 2.5, 0, Math.PI * 2);
                    ctx.fill();
                }
            }
            renderLegend(legend, legendSeries);
            overlay.textContent = `${viewModel.y.length} scatter series`;
        }

        saveBtn.addEventListener('click', function() {
            const dataUrl = buildPngDataUrlFromCanvas(canvas);
            if (!dataUrl) {
                return;
            }
            const fileName = `${sanitizeDownloadBaseName(modelName)}_scatter.png`;
            if (bridge.savePng) {
                bridge.savePng({ dataUrl: dataUrl, defaultName: fileName, view: viewModel });
                return;
            }
            fallbackDownload(dataUrl, fileName);
        });

        root.__dispose = function() {};
        root.__resize = rebuild;
        rebuild();
        return root;
    }

    function normalize3dPoints(points) {
        if (!Array.isArray(points) || points.length === 0) {
            return [];
        }
        const bounds = {
            minX: Math.min.apply(null, points.map(function(value) { return value.x; })),
            maxX: Math.max.apply(null, points.map(function(value) { return value.x; })),
            minY: Math.min.apply(null, points.map(function(value) { return value.y; })),
            maxY: Math.max.apply(null, points.map(function(value) { return value.y; })),
            minZ: Math.min.apply(null, points.map(function(value) { return value.z; })),
            maxZ: Math.max.apply(null, points.map(function(value) { return value.z; })),
        };
        const size = Math.max(
            bounds.maxX - bounds.minX,
            bounds.maxY - bounds.minY,
            bounds.maxZ - bounds.minZ,
            1,
        );
        const centerX = (bounds.minX + bounds.maxX) / 2;
        const centerY = (bounds.minY + bounds.maxY) / 2;
        const centerZ = (bounds.minZ + bounds.maxZ) / 2;
        return points.map(function(point) {
            return {
                x: (point.x - centerX) / size * 4,
                y: (point.y - centerY) / size * 4,
                z: (point.z - centerZ) / size * 4,
            };
        });
    }

    function createThreeDimensionalView(options) {
        const viewModel = options.viewModel;
        const bridge = options.bridge;
        const modelName = options.modelName;
        const metrics = options.metrics;
        const payload = options.payload;
        const root = document.createElement('section');
        root.className = 'rumoca-results-view';

        const wrap = document.createElement('div');
        wrap.className = 'rumoca-results-viewer-wrap';

        const host = document.createElement('div');
        host.className = 'rumoca-results-viewer-host';
        const canvas = document.createElement('canvas');
        canvas.className = 'rumoca-results-canvas';
        host.appendChild(canvas);

        const cameraInfoEl = document.createElement('div');
        cameraInfoEl.className = 'rumoca-results-viewer-camera-info';
        cameraInfoEl.textContent = 'camera: unavailable';
        host.appendChild(cameraInfoEl);

        const errorEl = document.createElement('div');
        errorEl.className = 'rumoca-results-viewer-error';
        errorEl.style.display = 'none';
        host.appendChild(errorEl);

        const inspectorEl = document.createElement('div');
        inspectorEl.className = 'rumoca-results-viewer-inspector hidden';
        const inspectorTitle = document.createElement('div');
        inspectorTitle.className = 'title';
        inspectorTitle.textContent = 'Scene Objects';
        const inspectorActions = document.createElement('div');
        inspectorActions.className = 'actions';
        const focusBtn = document.createElement('button');
        focusBtn.textContent = 'Focus';
        const followLabel = document.createElement('label');
        const followInput = document.createElement('input');
        followInput.type = 'checkbox';
        followInput.checked = false;
        const followText = document.createElement('span');
        followText.textContent = 'Follow';
        followLabel.appendChild(followInput);
        followLabel.appendChild(followText);
        inspectorActions.appendChild(focusBtn);
        inspectorActions.appendChild(followLabel);
        const objectListEl = document.createElement('div');
        objectListEl.className = 'objects';
        inspectorEl.appendChild(inspectorTitle);
        inspectorEl.appendChild(inspectorActions);
        inspectorEl.appendChild(objectListEl);
        host.appendChild(inspectorEl);

        const controls = document.createElement('div');
        controls.className = 'rumoca-results-viewer-controls';
        const detailsBtn = document.createElement('button');
        detailsBtn.textContent = 'Run Details';
        const objectsBtn = document.createElement('button');
        objectsBtn.textContent = 'Objects';
        const transport = document.createElement('div');
        transport.className = 'transport';
        const startBtn = document.createElement('button');
        startBtn.textContent = '|<';
        startBtn.title = 'Jump to start';
        const rewindBtn = document.createElement('button');
        rewindBtn.textContent = '<<';
        rewindBtn.title = 'Rewind';
        const playPauseBtn = document.createElement('button');
        playPauseBtn.innerHTML = '&#9654;';
        playPauseBtn.title = 'Play / Pause';
        const fastForwardBtn = document.createElement('button');
        fastForwardBtn.textContent = '>>';
        fastForwardBtn.title = 'Fast forward';
        const endBtn = document.createElement('button');
        endBtn.textContent = '>|';
        endBtn.title = 'Jump to end';
        transport.appendChild(startBtn);
        transport.appendChild(rewindBtn);
        transport.appendChild(playPauseBtn);
        transport.appendChild(fastForwardBtn);
        transport.appendChild(endBtn);
        const slider = document.createElement('input');
        slider.type = 'range';
        slider.min = '0';
        const points = payload && payload.allData && payload.allData[0] ? payload.allData[0].length : 1;
        slider.max = String(Math.max(points - 1, 0));
        slider.step = '1';
        slider.value = '0';
        const timeLabel = document.createElement('div');
        timeLabel.className = 'time';
        timeLabel.textContent = 't=0';
        const saveBtn = document.createElement('button');
        saveBtn.textContent = 'Save PNG';
        const movieBtn = document.createElement('button');
        movieBtn.textContent = 'Save WebM';
        movieBtn.title = 'Export viewer animation as WebM';
        controls.appendChild(detailsBtn);
        controls.appendChild(objectsBtn);
        controls.appendChild(transport);
        controls.appendChild(slider);
        controls.appendChild(timeLabel);
        controls.appendChild(saveBtn);
        controls.appendChild(movieBtn);

        wrap.appendChild(host);
        wrap.appendChild(controls);
        root.appendChild(wrap);

        const details = createDetailsModal(function() {
            return buildDetailsText(metrics, payload, viewModel);
        });
        root.appendChild(details.element);
        detailsBtn.addEventListener('click', function() {
            details.open();
        });

        let context2d = null;
        let viewerReady = false;
        let pendingSample = 0;
        let sceneObjects = [];
        let selectedObjectId = '';
        let followSelected = false;
        let inspectorVisible = false;
        let lastObjectRefreshMs = 0;
        const fallbackTarget = { x: 0, y: 0.5, z: 0 };

        function showError(message) {
            errorEl.textContent = message;
            errorEl.style.display = '';
        }

        function clearError() {
            errorEl.textContent = '';
            errorEl.style.display = 'none';
        }

        function resizeCanvas() {
            const rect = host.getBoundingClientRect();
            canvas.width = Math.max(1, Math.floor(rect.width));
            canvas.height = Math.max(1, Math.floor(rect.height));
        }

        function clearFallback() {
            if (!context2d) {
                context2d = canvas.getContext('2d');
            }
            if (!context2d) {
                return;
            }
            context2d.fillStyle = '#121212';
            context2d.fillRect(0, 0, canvas.width, canvas.height);
            context2d.strokeStyle = '#3a3a3a';
            context2d.beginPath();
            context2d.moveTo(0, canvas.height * 0.8);
            context2d.lineTo(canvas.width, canvas.height * 0.8);
            context2d.stroke();
        }

        const names = payload && Array.isArray(payload.names) ? payload.names : [];
        const allData = payload && Array.isArray(payload.allData) ? payload.allData : [];
        const timeData = allData.length > 0 ? allData[0] : [];

        function getSeries(name) {
            const idx = names.indexOf(name);
            if (idx < 0) {
                return undefined;
            }
            return allData[idx + 1];
        }

        const runtimeCtx = {};
        const THREE_NS = globalThis.THREE;
        let onInit = null;
        let onFrame = null;
        let onResize = null;
        let useDefaultViewerRuntime = false;

        const runtimeApi = {
            canvas: canvas,
            context2d: context2d,
            container: host,
            THREE: THREE_NS,
            names: names,
            times: timeData,
            sampleIndex: 0,
            state: {},
            getViewportSize: function() {
                const rect = host.getBoundingClientRect();
                return {
                    width: Math.max(1, Math.floor(rect.width)),
                    height: Math.max(1, Math.floor(rect.height)),
                };
            },
            getValue: function(name, sampleIndex) {
                const series = getSeries(name);
                if (!series) {
                    return undefined;
                }
                const idx = Number.isFinite(sampleIndex)
                    ? Math.max(0, Math.min(series.length - 1, Math.floor(sampleIndex)))
                    : runtimeApi.sampleIndex;
                return series[idx];
            },
            getTime: function(sampleIndex) {
                const idx = Number.isFinite(sampleIndex)
                    ? Math.max(0, Math.min(timeData.length - 1, Math.floor(sampleIndex)))
                    : runtimeApi.sampleIndex;
                return timeData[idx];
            },
            enableDefaultViewerRuntime: function(runtimeOptions) {
                useDefaultViewerRuntime = true;
                const nextOptions = runtimeOptions && typeof runtimeOptions === 'object'
                    ? runtimeOptions
                    : {};
                if (typeof nextOptions.selectedObjectName === 'string'
                    && nextOptions.selectedObjectName.trim().length > 0) {
                    selectedObjectId = nextOptions.selectedObjectName.trim();
                }
                if (typeof nextOptions.followSelected === 'boolean') {
                    followSelected = nextOptions.followSelected;
                    followInput.checked = followSelected;
                }
            },
            refreshDefaultViewerRuntime: function() {
                if (viewerReady) {
                    renderDefaultViewer(runtimeApi.sampleIndex || 0);
                } else {
                    refreshSceneObjects(false);
                    updateCameraInfo();
                }
            },
        };

        function setInspectorVisible(next) {
            inspectorVisible = !!next;
            inspectorEl.classList.toggle('hidden', !inspectorVisible);
        }

        function getControlTarget() {
            const controlsObj = runtimeApi.state && runtimeApi.state.controls;
            const target = controlsObj && controlsObj.target;
            if (target
                && Number.isFinite(Number(target.x))
                && Number.isFinite(Number(target.y))
                && Number.isFinite(Number(target.z))) {
                return target;
            }
            return fallbackTarget;
        }

        function setControlTarget(x, y, z) {
            const controlsObj = runtimeApi.state && runtimeApi.state.controls;
            if (controlsObj && controlsObj.target && typeof controlsObj.target.set === 'function') {
                controlsObj.target.set(Number(x) || 0, Number(y) || 0, Number(z) || 0);
                if (typeof controlsObj.update === 'function') {
                    controlsObj.update();
                }
                return;
            }
            fallbackTarget.x = Number(x) || 0;
            fallbackTarget.y = Number(y) || 0;
            fallbackTarget.z = Number(z) || 0;
            const camera = runtimeApi.state && runtimeApi.state.camera;
            if (camera && typeof camera.lookAt === 'function') {
                camera.lookAt(fallbackTarget.x, fallbackTarget.y, fallbackTarget.z);
            }
        }

        function objectIdFor(obj) {
            const name = obj && typeof obj.name === 'string' ? obj.name.trim() : '';
            if (name.length > 0) {
                return name;
            }
            return String((obj && obj.type) || 'Object') + '#' + String((obj && obj.id) || '?');
        }

        function objectLabelFor(obj) {
            const name = obj && typeof obj.name === 'string' ? obj.name.trim() : '';
            const type = String((obj && obj.type) || 'Object');
            if (name.length > 0) {
                return name + ' (' + type + ')';
            }
            return type + ' #' + String((obj && obj.id) || '?');
        }

        function renderObjectList() {
            objectListEl.innerHTML = '';
            if (sceneObjects.length === 0) {
                const empty = document.createElement('div');
                empty.className = 'object-row';
                empty.style.cursor = 'default';
                empty.style.opacity = '0.7';
                empty.textContent = 'No scene objects';
                objectListEl.appendChild(empty);
                return;
            }
            for (const entry of sceneObjects) {
                const row = document.createElement('div');
                row.className = 'object-row' + (entry.id === selectedObjectId ? ' active' : '');
                const swatch = document.createElement('span');
                swatch.className = 'swatch';
                const label = document.createElement('span');
                label.textContent = entry.label;
                row.appendChild(swatch);
                row.appendChild(label);
                row.addEventListener('click', function() {
                    selectedObjectId = entry.id;
                    renderObjectList();
                });
                objectListEl.appendChild(row);
            }
        }

        function refreshSceneObjects(force) {
            const scene = runtimeApi.state && runtimeApi.state.scene;
            if (!scene || typeof scene.traverse !== 'function') {
                sceneObjects = [];
                renderObjectList();
                return;
            }
            const now = Date.now();
            if (!force && now - lastObjectRefreshMs < 250) {
                return;
            }
            lastObjectRefreshMs = now;
            const nextObjects = [];
            scene.traverse(function(obj) {
                if (!obj || obj === scene || obj.visible === false) {
                    return;
                }
                nextObjects.push({
                    id: objectIdFor(obj),
                    label: objectLabelFor(obj),
                    obj: obj,
                });
            });
            nextObjects.sort(function(left, right) {
                return String(left.label).localeCompare(String(right.label));
            });
            sceneObjects = nextObjects;
            if (!selectedObjectId && sceneObjects.length > 0) {
                selectedObjectId = sceneObjects[0].id;
            }
            if (selectedObjectId) {
                const exists = sceneObjects.some(function(entry) {
                    return entry.id === selectedObjectId;
                });
                if (!exists) {
                    selectedObjectId = sceneObjects.length > 0 ? sceneObjects[0].id : '';
                }
            }
            renderObjectList();
        }

        function getSelectedObject() {
            if (!selectedObjectId) {
                return undefined;
            }
            const entry = sceneObjects.find(function(item) {
                return item.id === selectedObjectId;
            });
            return entry ? entry.obj : undefined;
        }

        function focusSelectedObject() {
            if (!THREE_NS) {
                return;
            }
            const selected = getSelectedObject();
            const camera = runtimeApi.state && runtimeApi.state.camera;
            if (!selected || !camera) {
                return;
            }
            try {
                const center = new THREE_NS.Vector3();
                const box = new THREE_NS.Box3().setFromObject(selected);
                if (!box.isEmpty()) {
                    box.getCenter(center);
                } else if (selected.position) {
                    center.set(
                        Number(selected.position.x) || 0,
                        Number(selected.position.y) || 0,
                        Number(selected.position.z) || 0,
                    );
                } else {
                    center.set(0, 0, 0);
                }
                setControlTarget(center.x, center.y, center.z);
                if (camera.position && Number.isFinite(Number(camera.position.z))) {
                    const direction = camera.position.clone().sub(center);
                    if (direction.lengthSq() < 1.0e-8) {
                        direction.set(2.5, 1.7, 2.8);
                    }
                    direction.normalize().multiplyScalar(3.0);
                    camera.position.copy(center.clone().add(direction));
                }
                if (typeof camera.lookAt === 'function') {
                    camera.lookAt(center);
                }
                if (typeof runtimeApi.state.renderNow === 'function') {
                    runtimeApi.state.renderNow();
                }
            } catch (error) {
                showError('Focus failed: ' + String(error));
            }
        }

        function followSelectedObjectIfEnabled() {
            if (!followSelected || !THREE_NS) {
                return;
            }
            const selected = getSelectedObject();
            if (!selected) {
                return;
            }
            try {
                const position = new THREE_NS.Vector3();
                if (typeof selected.getWorldPosition === 'function') {
                    selected.getWorldPosition(position);
                } else if (selected.position) {
                    position.set(
                        Number(selected.position.x) || 0,
                        Number(selected.position.y) || 0,
                        Number(selected.position.z) || 0,
                    );
                } else {
                    return;
                }
                setControlTarget(position.x, position.y, position.z);
            } catch (_) {
                // ignore follow errors
            }
        }

        function updateCameraInfo() {
            const camera = runtimeApi.state && runtimeApi.state.camera;
            if (!camera || !camera.position || !camera.rotation) {
                cameraInfoEl.textContent = 'camera: unavailable';
                return;
            }
            const toDeg = function(radians) {
                return Number(radians) * (180 / Math.PI);
            };
            const target = getControlTarget();
            cameraInfoEl.textContent = [
                'pos '
                    + formatNum(Number(camera.position.x))
                    + ', '
                    + formatNum(Number(camera.position.y))
                    + ', '
                    + formatNum(Number(camera.position.z)),
                'deg '
                    + formatNum(toDeg(camera.rotation.x))
                    + ', '
                    + formatNum(toDeg(camera.rotation.y))
                    + ', '
                    + formatNum(toDeg(camera.rotation.z)),
                'tgt '
                    + formatNum(target.x)
                    + ', '
                    + formatNum(target.y)
                    + ', '
                    + formatNum(target.z),
            ].join('\n');
        }

        function createSimpleControls(camera) {
            if (!THREE_NS || !camera || !camera.position) {
                return null;
            }
            const target = new THREE_NS.Vector3(0, 0.5, 0);
            let radius = camera.position.distanceTo(target);
            if (!Number.isFinite(radius) || radius < 0.2) {
                radius = 4.0;
            }
            let theta = Math.atan2(camera.position.x - target.x, camera.position.z - target.z);
            let phi = Math.acos(Math.max(-1, Math.min(1, (camera.position.y - target.y) / radius)));
            let dragging = false;
            let panning = false;
            let lastX = 0;
            let lastY = 0;

            function applyOrbit() {
                const sinPhi = Math.sin(phi);
                camera.position.x = target.x + radius * sinPhi * Math.sin(theta);
                camera.position.y = target.y + radius * Math.cos(phi);
                camera.position.z = target.z + radius * sinPhi * Math.cos(theta);
                camera.lookAt(target);
            }

            function onPointerDown(event) {
                if (!event.isPrimary) {
                    return;
                }
                dragging = true;
                panning = event.button === 1 || event.button === 2;
                lastX = event.clientX;
                lastY = event.clientY;
                try {
                    canvas.setPointerCapture(event.pointerId);
                } catch (_) {
                    // ignore pointer capture failures
                }
                event.preventDefault();
            }

            function onPointerMove(event) {
                if (!dragging) {
                    return;
                }
                const dx = event.clientX - lastX;
                const dy = event.clientY - lastY;
                lastX = event.clientX;
                lastY = event.clientY;
                if (panning) {
                    const panScale = Math.max(0.002, radius * 0.0014);
                    const direction = new THREE_NS.Vector3();
                    camera.getWorldDirection(direction);
                    const right = new THREE_NS.Vector3().crossVectors(direction, camera.up).normalize();
                    const up = camera.up.clone().normalize();
                    target.addScaledVector(right, -dx * panScale);
                    target.addScaledVector(up, dy * panScale);
                } else {
                    theta -= dx * 0.006;
                    phi -= dy * 0.006;
                    phi = Math.max(0.04, Math.min(Math.PI - 0.04, phi));
                }
                applyOrbit();
            }

            function onPointerUp(event) {
                dragging = false;
                try {
                    canvas.releasePointerCapture(event.pointerId);
                } catch (_) {
                    // ignore pointer capture release failures
                }
            }

            function onWheel(event) {
                const factor = event.deltaY > 0 ? 1.08 : 0.92;
                radius = Math.max(0.15, Math.min(300, radius * factor));
                applyOrbit();
                event.preventDefault();
            }

            canvas.addEventListener('pointerdown', onPointerDown);
            canvas.addEventListener('pointermove', onPointerMove);
            canvas.addEventListener('pointerup', onPointerUp);
            canvas.addEventListener('pointerleave', onPointerUp);
            canvas.addEventListener('wheel', onWheel, { passive: false });
            applyOrbit();

            return {
                target: target,
                update: applyOrbit,
                dispose: function() {
                    canvas.removeEventListener('pointerdown', onPointerDown);
                    canvas.removeEventListener('pointermove', onPointerMove);
                    canvas.removeEventListener('pointerup', onPointerUp);
                    canvas.removeEventListener('pointerleave', onPointerUp);
                    canvas.removeEventListener('wheel', onWheel);
                },
            };
        }

        function ensureDefaultSceneReady() {
            if (!THREE_NS) {
                return false;
            }
            let scene = runtimeApi.state.scene;
            if (!scene || scene.isScene !== true) {
                scene = new THREE_NS.Scene();
                scene.background = new THREE_NS.Color(0x101010);
                runtimeApi.state.scene = scene;
            }
            let camera = runtimeApi.state.camera;
            if (!camera || camera.isPerspectiveCamera !== true) {
                camera = new THREE_NS.PerspectiveCamera(52, 1, 0.01, 2000);
                camera.position.set(3.0, 2.1, 3.2);
                runtimeApi.state.camera = camera;
            }
            let renderer = runtimeApi.state.renderer;
            if (!renderer || typeof renderer.render !== 'function') {
                renderer = new THREE_NS.WebGLRenderer({ canvas: canvas, antialias: true, alpha: false });
                renderer.setPixelRatio(Math.max(1, Math.min(2, globalThis.devicePixelRatio || 1)));
                runtimeApi.state.renderer = renderer;
            }
            if (!runtimeApi.state.controls) {
                runtimeApi.state.controls = createSimpleControls(camera);
            }
            runtimeApi.state.renderNow = function() {
                renderer.render(scene, camera);
            };
            runtimeApi.state.resize = function() {
                const size = runtimeApi.getViewportSize();
                const width = Math.max(1, size.width || 1);
                const height = Math.max(1, size.height || 1);
                renderer.setSize(width, height, false);
                camera.aspect = width / height;
                camera.updateProjectionMatrix();
                if (runtimeApi.state.controls && typeof runtimeApi.state.controls.update === 'function') {
                    runtimeApi.state.controls.update();
                }
                renderer.render(scene, camera);
            };
            runtimeApi.state.resize();
            return true;
        }

        function resizeViewer() {
            resizeCanvas();
            if (viewerReady && onResize) {
                try {
                    onResize(runtimeApi);
                } catch (error) {
                    showError('3D resize error:\n' + String(error));
                    onResize = null;
                }
            } else if (runtimeApi.state && typeof runtimeApi.state.resize === 'function') {
                try {
                    runtimeApi.state.resize();
                } catch (_) {
                    // ignore default resize errors
                }
            }
            updateCameraInfo();
        }

        if (typeof viewModel.script === 'string' && viewModel.script.trim().length > 0) {
            try {
                const fn = new Function('ctx', 'api', viewModel.script + '\nreturn ctx;');
                fn(runtimeCtx, runtimeApi);
                onInit = typeof runtimeCtx.onInit === 'function' ? runtimeCtx.onInit : null;
                onFrame = typeof runtimeCtx.onFrame === 'function' ? runtimeCtx.onFrame : null;
                onResize = typeof runtimeCtx.onResize === 'function' ? runtimeCtx.onResize : null;
            } catch (error) {
                showError('3D script compile error:\n' + String(error));
            }
        }

        function drawFallback(sampleIndex) {
            clearFallback();
            if (!context2d) {
                return;
            }
            const xSeries = getSeries('x');
            const x = xSeries && sampleIndex < xSeries.length ? xSeries[sampleIndex] : 0;
            const yPix = canvas.height * 0.8 - (Number.isFinite(x) ? x : 0) * 16;
            context2d.fillStyle = '#3cb4ff';
            context2d.beginPath();
            context2d.arc(canvas.width * 0.5, yPix, 14, 0, Math.PI * 2);
            context2d.fill();
            runtimeApi.context2d = context2d;
        }

        function renderDefaultViewer(sampleIndex) {
            if (!ensureDefaultSceneReady()) {
                drawFallback(sampleIndex);
                return;
            }
            followSelectedObjectIfEnabled();
            if (runtimeApi.state.controls && typeof runtimeApi.state.controls.update === 'function') {
                runtimeApi.state.controls.update();
            }
            if (typeof runtimeApi.state.renderNow === 'function') {
                runtimeApi.state.renderNow();
            }
        }

        async function initializeViewer() {
            ensureDefaultSceneReady();
            clearError();
            try {
                if (onInit) {
                    await Promise.resolve(onInit(runtimeApi));
                }
            } catch (error) {
                showError('3D init error:\n' + String(error));
            }
            viewerReady = true;
            refreshSceneObjects(true);
            if (onResize) {
                try {
                    onResize(runtimeApi);
                } catch (error) {
                    showError('3D resize error:\n' + String(error));
                    onResize = null;
                }
            } else if (runtimeApi.state && typeof runtimeApi.state.resize === 'function') {
                runtimeApi.state.resize();
            }
            renderSample(pendingSample);
        }

        function renderSample(sampleIndex) {
            const clamped = Math.max(0, Math.min(Math.max(timeData.length - 1, 0), sampleIndex | 0));
            pendingSample = clamped;
            runtimeApi.sampleIndex = clamped;
            slider.value = String(clamped);
            const t = clamped < timeData.length ? timeData[clamped] : 0;
            timeLabel.textContent = 't=' + formatNum(t);
            if (!viewerReady) {
                return;
            }
            if (onFrame) {
                try {
                    onFrame(runtimeApi);
                } catch (error) {
                    showError('3D frame error:\n' + String(error));
                    onFrame = null;
                }
            }
            if (useDefaultViewerRuntime || !onFrame) {
                renderDefaultViewer(clamped);
            }
            followSelectedObjectIfEnabled();
            refreshSceneObjects(false);
            updateCameraInfo();
        }

        function refreshInteractiveView() {
            if (!viewerReady) {
                return;
            }
            renderSample(runtimeApi.sampleIndex || Number(slider.value) || 0);
        }

        objectsBtn.addEventListener('click', function() {
            setInspectorVisible(!inspectorVisible);
        });
        focusBtn.addEventListener('click', function() {
            focusSelectedObject();
        });
        followInput.addEventListener('change', function() {
            followSelected = followInput.checked;
        });
        canvas.addEventListener('pointerdown', refreshInteractiveView);
        canvas.addEventListener('pointermove', refreshInteractiveView);
        canvas.addEventListener('wheel', refreshInteractiveView, { passive: true });
        resizeCanvas();

        let playing = false;
        let playbackDirection = 1;
        let playbackRate = 1;
        let playbackRaf = 0;
        let playbackAnchorWallMs = 0;
        let playbackAnchorTime = 0;
        let lastRenderedIndex = 0;
        let exportingMovie = false;

        function updateTransportUi() {
            playPauseBtn.innerHTML = playing ? '&#10074;&#10074;' : '&#9654;';
            rewindBtn.classList.toggle('active', playing && playbackDirection < 0);
            fastForwardBtn.classList.toggle('active', playing && playbackDirection > 0 && playbackRate > 1);
            movieBtn.disabled = exportingMovie;
            movieBtn.textContent = exportingMovie ? 'Exporting…' : 'Save WebM';
        }

        function clampSampleIndex(index) {
            const max = Number(slider.max) || 0;
            return Math.max(0, Math.min(max, index | 0));
        }

        function timeAtIndex(index) {
            const value = Number(timeData[index]);
            return Number.isFinite(value) ? value : index;
        }

        function indexForTime(targetTime) {
            const max = Math.max(0, timeData.length - 1);
            if (max === 0) {
                return 0;
            }
            const first = timeAtIndex(0);
            const last = timeAtIndex(max);
            if (!(Number.isFinite(first) && Number.isFinite(last) && last > first)) {
                return clampSampleIndex(Math.round(targetTime));
            }
            if (targetTime <= first) {
                return 0;
            }
            if (targetTime >= last) {
                return max;
            }
            let lo = 0;
            let hi = max;
            while (lo < hi) {
                const mid = (lo + hi) >> 1;
                if (timeAtIndex(mid) < targetTime) {
                    lo = mid + 1;
                } else {
                    hi = mid;
                }
            }
            const upper = lo;
            const lower = Math.max(0, upper - 1);
            const lowerTime = timeAtIndex(lower);
            const upperTime = timeAtIndex(upper);
            return Math.abs(targetTime - lowerTime) <= Math.abs(upperTime - targetTime)
                ? lower
                : upper;
        }

        function stop() {
            playing = false;
            if (playbackRaf) {
                cancelAnimationFrame(playbackRaf);
                playbackRaf = 0;
            }
            updateTransportUi();
        }

        function start(direction, rate) {
            if (playing) {
                return;
            }
            playing = true;
            playbackDirection = direction < 0 ? -1 : 1;
            playbackRate = Math.max(1, Number(rate) || 1);
            playbackAnchorTime = timeAtIndex(clampSampleIndex(runtimeApi.sampleIndex || Number(slider.value) || 0));
            playbackAnchorWallMs = performance.now();
            lastRenderedIndex = clampSampleIndex(runtimeApi.sampleIndex || Number(slider.value) || 0);
            updateTransportUi();

            const tick = function(now) {
                if (!playing) {
                    return;
                }
                const elapsedSeconds = Math.max(0, (now - playbackAnchorWallMs) / 1000);
                const firstTime = timeAtIndex(0);
                const lastTime = timeAtIndex(Math.max(0, timeData.length - 1));
                const targetTime = playbackAnchorTime + (playbackDirection * playbackRate * elapsedSeconds);
                let nextIndex = indexForTime(targetTime);
                if (playbackDirection > 0 && targetTime >= lastTime) {
                    nextIndex = Number(slider.max) || 0;
                } else if (playbackDirection < 0 && targetTime <= firstTime) {
                    nextIndex = 0;
                }
                nextIndex = clampSampleIndex(nextIndex);
                if (nextIndex !== lastRenderedIndex) {
                    renderSample(nextIndex);
                    lastRenderedIndex = nextIndex;
                }
                const atStart = nextIndex <= 0;
                const atEnd = nextIndex >= (Number(slider.max) || 0);
                if ((playbackDirection < 0 && atStart) || (playbackDirection > 0 && atEnd)) {
                    stop();
                    return;
                }
                playbackRaf = requestAnimationFrame(tick);
            };

            playbackRaf = requestAnimationFrame(tick);
        }

        playPauseBtn.addEventListener('click', function() {
            if (playing) {
                stop();
                return;
            }
            start(1, 1);
        });
        rewindBtn.addEventListener('click', function() {
            if (playing && playbackDirection < 0) {
                stop();
                return;
            }
            if (playing) {
                stop();
            }
            start(-1, 4);
        });
        fastForwardBtn.addEventListener('click', function() {
            if (playing && playbackDirection > 0 && playbackRate > 1) {
                stop();
                return;
            }
            if (playing) {
                stop();
            }
            start(1, 4);
        });
        startBtn.addEventListener('click', function() {
            stop();
            renderSample(0);
        });
        endBtn.addEventListener('click', function() {
            stop();
            renderSample(Number(slider.max));
        });
        slider.addEventListener('input', function() {
            renderSample(Number(slider.value));
        });

        async function exportMovieWebm() {
            if (exportingMovie) {
                return;
            }
            if (!viewerReady) {
                showError('Movie export unavailable: viewer not ready.');
                return;
            }
            if (typeof MediaRecorder === 'undefined' || typeof canvas.captureStream !== 'function') {
                showError('Movie export unavailable: MediaRecorder/captureStream not supported.');
                return;
            }
            exportingMovie = true;
            updateTransportUi();
            const wasPlaying = playing;
            if (wasPlaying) {
                stop();
            }
            const originalSample = Number(slider.value) || 0;
            try {
                clearError();
                const fps = 30;
                const totalFrames = Math.max(1, Number(slider.max) + 1);
                renderSample(0);
                const stream = canvas.captureStream(fps);
                const mimeCandidates = [
                    'video/webm;codecs=vp9',
                    'video/webm;codecs=vp8',
                    'video/webm',
                ];
                let mimeType = '';
                for (const candidate of mimeCandidates) {
                    if (typeof MediaRecorder.isTypeSupported === 'function'
                        && MediaRecorder.isTypeSupported(candidate)) {
                        mimeType = candidate;
                        break;
                    }
                }
                const recorder = mimeType.length > 0
                    ? new MediaRecorder(stream, { mimeType: mimeType, videoBitsPerSecond: 8_000_000 })
                    : new MediaRecorder(stream, { videoBitsPerSecond: 8_000_000 });
                const chunks = [];
                recorder.ondataavailable = function(event) {
                    if (event.data && event.data.size > 0) {
                        chunks.push(event.data);
                    }
                };
                const stopPromise = new Promise(function(resolve, reject) {
                    recorder.onerror = function() {
                        reject(recorder.error || new Error('MediaRecorder failed'));
                    };
                    recorder.onstop = function() {
                        resolve();
                    };
                });
                recorder.start(Math.max(40, Math.floor(1000 / fps)));
                for (let frame = 0; frame < totalFrames; frame += 1) {
                    renderSample(frame);
                    await new Promise(function(resolve) {
                        setTimeout(resolve, Math.max(1, Math.floor(1000 / fps)));
                    });
                }
                recorder.stop();
                await stopPromise;
                const blob = new Blob(chunks, { type: recorder.mimeType || 'video/webm' });
                if (blob.size <= 0) {
                    throw new Error('No video frames captured.');
                }
                if (bridge.saveWebm) {
                    const dataUrl = await new Promise(function(resolve, reject) {
                        const reader = new FileReader();
                        reader.onerror = function() {
                            reject(new Error('Failed to encode video payload.'));
                        };
                        reader.onload = function() {
                            resolve(String(reader.result || ''));
                        };
                        reader.readAsDataURL(blob);
                    });
                    bridge.saveWebm({
                        dataUrl: dataUrl,
                        defaultName: sanitizeDownloadBaseName(modelName) + '_viewer.webm',
                        view: viewModel,
                    });
                } else {
                    const url = URL.createObjectURL(blob);
                    const link = document.createElement('a');
                    link.href = url;
                    link.download = sanitizeDownloadBaseName(modelName) + '_viewer.webm';
                    document.body.appendChild(link);
                    link.click();
                    document.body.removeChild(link);
                    URL.revokeObjectURL(url);
                }
            } catch (error) {
                showError('Movie export failed: ' + String(error));
                bridge.notify('Movie export failed: ' + String(error));
            } finally {
                exportingMovie = false;
                renderSample(originalSample);
                if (wasPlaying) {
                    start(1, 1);
                }
                updateTransportUi();
            }
        }

        saveBtn.addEventListener('click', function() {
            const dataUrl = buildPngDataUrlFromCanvas(canvas);
            if (!dataUrl) {
                return;
            }
            const fileName = sanitizeDownloadBaseName(modelName) + '_viewer.png';
            if (bridge.savePng) {
                bridge.savePng({ dataUrl: dataUrl, defaultName: fileName, view: viewModel });
                return;
            }
            fallbackDownload(dataUrl, fileName);
        });
        movieBtn.style.display = bridge.saveWebm ? '' : 'none';
        movieBtn.addEventListener('click', function() {
            void exportMovieWebm();
        });

        renderSample(0);
        updateTransportUi();
        void initializeViewer();

        root.__dispose = function() {
            stop();
            globalThis.removeEventListener('resize', resizeViewer);
            if (runtimeApi.state.controls && typeof runtimeApi.state.controls.dispose === 'function') {
                try {
                    runtimeApi.state.controls.dispose();
                } catch (_) {
                    // ignore control cleanup failures
                }
            }
            if (runtimeApi.state.renderer && typeof runtimeApi.state.renderer.dispose === 'function') {
                try {
                    runtimeApi.state.renderer.dispose();
                } catch (_) {
                    // ignore renderer cleanup failures
                }
            }
        };
        root.__resize = function() {
            resizeViewer();
            renderSample(runtimeApi.sampleIndex || Number(slider.value) || 0);
        };

        globalThis.addEventListener('resize', resizeViewer);
        return root;
    }

    function createViewElement(options) {
        if (options.viewModel.type === 'scatter') {
            return createScatterView(options);
        }
        if (options.viewModel.type === '3d') {
            return createThreeDimensionalView(options);
        }
        return createTimeseriesView(options);
    }

    function createResultsApp(config) {
        if (!config || typeof config !== 'object') {
            throw new Error('createResultsApp requires a configuration object.');
        }
        const root = config.root;
        if (!root) {
            throw new Error('createResultsApp requires a root element.');
        }
        const shared = sharedVisualization();
        const bridge = createNoopResultsHostBridge(config.bridge);
        const allowViewEditing = config.allowViewEditing !== false;
        const modelRef = config.modelRef !== undefined ? config.modelRef : trimMaybeString(config.model);
        let payload = config.payload || null;
        let metrics = config.metrics || null;
        let views = normalizeResultsViewDrafts(shared, config.views);
        let activeViewId = chooseActiveViewId(views, config.activeViewId);
        let mountedView = null;

        root.innerHTML = '';
        root.classList.add('rumoca-results-root');

        const header = document.createElement('div');
        header.className = 'rumoca-results-header';
        const title = document.createElement('div');
        title.className = 'rumoca-results-title';
        title.textContent = trimMaybeString(config.model) || 'Rumoca Results';
        const actions = document.createElement('div');
        actions.className = 'rumoca-results-header-actions';
        const settingsBtn = allowViewEditing ? document.createElement('button') : null;
        if (settingsBtn) {
            settingsBtn.className = 'rumoca-results-header-button';
            settingsBtn.textContent = 'Settings';
            actions.appendChild(settingsBtn);
        }
        header.appendChild(title);
        header.appendChild(actions);
        root.appendChild(header);

        const status = createStatusBanner();
        root.appendChild(status.element);

        const timing = document.createElement('div');
        timing.className = 'rumoca-results-timing';
        root.appendChild(timing);

        const tabs = document.createElement('div');
        tabs.className = 'rumoca-results-tabs';
        root.appendChild(tabs);

        const content = document.createElement('div');
        content.className = 'rumoca-results-content';
        root.appendChild(content);

        const settingsModal = allowViewEditing
            ? createSettingsModal({
                bridge: bridge,
                modelRef: modelRef,
                onSave: function(nextViews) {
                    views = normalizeResultsViewDrafts(shared, nextViews);
                    activeViewId = chooseActiveViewId(views, activeViewId);
                    render();
                    status.show('Views saved.', 'ok');
                },
                shared: shared,
            })
            : null;
        if (settingsModal) {
            root.appendChild(settingsModal.element);
        }

        function disposeMountedView() {
            if (mountedView && typeof mountedView.__dispose === 'function') {
                mountedView.__dispose();
            }
            mountedView = null;
            content.innerHTML = '';
        }

        function renderTiming() {
            const parts = buildResultsTimingSummary(metrics);
            timing.textContent = parts.join(' | ');
        }

        function renderTabs() {
            tabs.innerHTML = '';
            for (const view of views) {
                const button = document.createElement('button');
                button.className = `rumoca-results-tab${view.id === activeViewId ? ' active' : ''}`;
                button.textContent = trimMaybeString(view.title) || trimMaybeString(view.id);
                button.addEventListener('click', function() {
                    activeViewId = view.id;
                    bridge.persistState({ activeViewId: activeViewId });
                    status.clear();
                    render();
                });
                tabs.appendChild(button);
            }
        }

        function renderEmpty(message) {
            disposeMountedView();
            const empty = document.createElement('div');
            empty.className = 'rumoca-results-empty';
            empty.textContent = message;
            content.appendChild(empty);
        }

        function render() {
            renderTiming();
            renderTabs();
            if (!payload) {
                renderEmpty('Run a simulation to view results.');
                return;
            }
            if (!views.length) {
                views = shared.defaultVisualizationViews();
                activeViewId = chooseActiveViewId(views, activeViewId);
            }
            const activeView = views.find(function(view) { return view.id === activeViewId; }) || views[0];
            disposeMountedView();
            const viewModel = shared.buildVisualizationModel(payload, activeView);
            mountedView = createViewElement({
                bridge: bridge,
                metrics: metrics,
                modelName: title.textContent,
                payload: payload,
                viewModel: viewModel,
            });
            content.appendChild(mountedView);
            requestAnimationFrame(function() {
                if (mountedView && typeof mountedView.__resize === 'function') {
                    mountedView.__resize();
                }
            });
        }

        function update(next) {
            const options = next && typeof next === 'object' ? next : {};
            if (options.model !== undefined) {
                title.textContent = trimMaybeString(options.model) || title.textContent;
            }
            if (options.payload !== undefined) {
                payload = options.payload || null;
            }
            if (options.metrics !== undefined) {
                metrics = options.metrics || null;
            }
            if (options.views !== undefined) {
                views = normalizeResultsViewDrafts(shared, options.views);
                activeViewId = chooseActiveViewId(views, activeViewId);
            }
            if (options.activeViewId !== undefined) {
                activeViewId = chooseActiveViewId(views, options.activeViewId);
            }
            render();
        }

        function dispose() {
            globalThis.removeEventListener('resize', handleResize);
            if (settingsModal) {
                settingsModal.close();
            }
            disposeMountedView();
            root.innerHTML = '';
        }

        const handleResize = function() {
            if (mountedView && typeof mountedView.__resize === 'function') {
                mountedView.__resize();
            }
        };
        globalThis.addEventListener('resize', handleResize);

        if (settingsBtn && settingsModal) {
            settingsBtn.addEventListener('click', async function() {
                status.clear();
                try {
                    await settingsModal.open(views);
                } catch (error) {
                    status.show(String(error && error.message ? error.message : error), 'error');
                }
            });
        }

        render();
        return {
            dispose: dispose,
            update: update,
        };
    }

    return {
        RESULTS_APP_BRIDGE_METHODS: RESULTS_APP_BRIDGE_METHODS,
        buildResultsTimingSummary: buildResultsTimingSummary,
        chooseActiveViewId: chooseActiveViewId,
        createNoopResultsHostBridge: createNoopResultsHostBridge,
        createResultsApp: createResultsApp,
        defaultResultsView: defaultResultsView,
        formatScatterSeriesText: formatScatterSeriesText,
        formatSeriesList: formatSeriesList,
        normalizeResultsViewDrafts: normalizeResultsViewDrafts,
        parseScatterSeriesText: parseScatterSeriesText,
        parseSeriesList: parseSeriesList,
    };
}));
