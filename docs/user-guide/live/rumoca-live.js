// Rumoca live example runner for the mdBook guides.
//
// Upgrades fenced code blocks marked `modelica,interactive` into mini Monaco
// editor windows backed by the Rumoca WASM package: syntax highlighting,
// LSP completion/hover/diagnostics, one-click simulation with an inline plot,
// and a "Show DAE" view of the flattened system.
//
// Loading strategy:
//   - Monaco loads from the same CDN as the web playground when a page
//     contains at least one interactive block. If the CDN is unreachable the
//     widget falls back to a plain editable textarea.
//   - The Rumoca WASM package loads lazily on first interaction (editor
//     focus or a toolbar click), so reading a page stays cheap.
//
// Page layout assumptions (see dev-guide "Docs and Pages"):
//   GitHub Pages:  /user-guide/...  with the package at /pkg/<subdir>/ and
//                  the playground modules at /src/modules/
//   local serve:   serve the repository root; books live at docs/<book>/book/
// A page can override package discovery with `window.RUMOCA_LIVE_PKG_BASE`.
(function () {
    'use strict';

    const MONACO_CDN_BASE = 'https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.45.0/min';
    const PKG_SUBDIRS = ['release-full-web', 'release-full-web-rayon'];
    const LANGUAGE_MODULE_CANDIDATES = [
        // GitHub Pages: editors/wasm/src is deployed at the site root.
        '../src/modules/modelica_language.js',
        // Local: repository root served directly, book at docs/<book>/book/.
        '../../../editors/wasm/src/modules/modelica_language.js',
    ];
    const SERIES_COLORS = [
        '#2470c2', '#d94f30', '#2c9462', '#9356c8', '#c8842c', '#3aa0ab',
    ];
    const MAX_PLOT_SERIES = 6;
    const MAX_EDITOR_LINES = 28;
    const DIAGNOSTIC_DEBOUNCE_MS = 400;

    let monacoPromise = null;
    let wasmModulePromise = null;
    let wasmRequested = false;
    let languageServicesRegistered = false;
    const widgets = [];

    function bookRoot() {
        // mdBook defines `path_to_root` as a top-level const in every page.
        /* global path_to_root */
        return typeof path_to_root !== 'undefined' ? path_to_root : './';
    }

    function pageUrl(relative) {
        return new URL(relative, window.location.href).href;
    }

    // ---------------------------------------------------------------------
    // WASM package loading
    // ---------------------------------------------------------------------

    function pkgBaseCandidates() {
        if (window.RUMOCA_LIVE_PKG_BASE) {
            return [window.RUMOCA_LIVE_PKG_BASE];
        }
        const root = bookRoot();
        const layouts = [root + '../pkg/', root + '../../../pkg/'];
        const candidates = [];
        for (const layout of layouts) {
            for (const subdir of PKG_SUBDIRS) {
                candidates.push(layout + subdir + '/');
            }
        }
        return candidates;
    }

    async function locatePkgBase() {
        for (const base of pkgBaseCandidates()) {
            const probe = pageUrl(base + 'rumoca_bind_wasm.js');
            try {
                const response = await fetch(probe, { method: 'HEAD' });
                if (response.ok) {
                    return probe.replace(/rumoca_bind_wasm\.js$/, '');
                }
            } catch (_error) {
                // Try the next layout.
            }
        }
        throw new Error(
            'Rumoca WASM package not found next to this book. ' +
            'Live examples work on the published site ' +
            '(https://cognipilot.github.io/rumoca/user-guide/) or when the ' +
            'repository root is served locally after `cargo xtask wasm build`.'
        );
    }

    function loadWasm() {
        if (!wasmModulePromise) {
            wasmRequested = true;
            broadcastStatus('Downloading Rumoca WASM (first use on this page)…');
            wasmModulePromise = (async () => {
                const base = await locatePkgBase();
                const module = await import(base + 'rumoca_bind_wasm.js');
                await module.default();
                return module;
            })();
            wasmModulePromise.then(
                (wasm) => {
                    broadcastStatus('');
                    registerLanguageServices(wasm);
                    for (const widget of widgets) {
                        widget.onWasmReady(wasm);
                    }
                },
                () => {
                    // Allow a retry after a failed load.
                    wasmModulePromise = null;
                    wasmRequested = false;
                    broadcastStatus('Rumoca WASM failed to load — click Simulate to retry.');
                }
            );
        }
        return wasmModulePromise;
    }

    // -----------------------------------------------------------------
    // Simulation worker
    //
    // simulate_model and DAE rendering are synchronous WASM calls that can
    // take tens of seconds on large discretized models (the NACA airfoil
    // page). Running them in a module worker keeps the page responsive so
    // the progress bar can animate. Falls back to main-thread calls when
    // workers are unavailable.
    // -----------------------------------------------------------------

    let simWorkerPromise = null;

    function buildSimWorkerSource(pkgBase) {
        return `
import init, * as rumoca from '${pkgBase}rumoca_bind_wasm.js';
const ready = init();
self.onmessage = async (event) => {
    const { id, action, args } = event.data;
    try {
        await ready;
        let result;
        if (action === 'simulate') {
            result = rumoca.simulate_model(args.source, args.model, 0, 0, '');
        } else if (action === 'dae') {
            const envelope = JSON.parse(rumoca.compile(args.source, args.model));
            const daeJson = JSON.stringify(envelope.dae_native || envelope.dae);
            const rendered = rumoca.render_target(daeJson, args.model, 'dae-modelica', '', '{}');
            const files = rendered.files;
            result = Array.isArray(files)
                ? files.map((file) => file.content || '').join('\\n')
                : '';
        } else {
            throw new Error('unknown action ' + action);
        }
        self.postMessage({ id, ok: true, result });
    } catch (error) {
        self.postMessage({ id, ok: false, error: String((error && error.message) || error) });
    }
};
`;
    }

    function loadSimWorker() {
        if (!simWorkerPromise) {
            simWorkerPromise = (async () => {
                const base = await locatePkgBase();
                const blob = new Blob([buildSimWorkerSource(base)], { type: 'text/javascript' });
                const worker = new Worker(URL.createObjectURL(blob), { type: 'module' });
                const pending = new Map();
                let nextId = 1;
                worker.onmessage = (event) => {
                    const { id, ok, result, error } = event.data;
                    const entry = pending.get(id);
                    if (!entry) {
                        return;
                    }
                    pending.delete(id);
                    if (ok) {
                        entry.resolve(result);
                    } else {
                        entry.reject(new Error(error));
                    }
                };
                worker.onerror = (event) => {
                    const failure = new Error(event.message || 'simulation worker failed');
                    for (const entry of pending.values()) {
                        entry.reject(failure);
                    }
                    pending.clear();
                };
                return {
                    request(action, args) {
                        return new Promise((resolve, reject) => {
                            const id = nextId++;
                            pending.set(id, { resolve, reject });
                            worker.postMessage({ id, action, args });
                        });
                    },
                };
            })();
            simWorkerPromise.catch(() => {
                simWorkerPromise = null;
            });
        }
        return simWorkerPromise;
    }

    // Run a heavy action in the worker, falling back to the main thread.
    async function runHeavy(action, args, mainThreadFallback) {
        try {
            const worker = await loadSimWorker();
            return await worker.request(action, args);
        } catch (error) {
            console.warn(`rumoca-live: worker path failed for ${action}, using main thread:`, error);
            return mainThreadFallback();
        }
    }

    function broadcastStatus(text) {
        for (const widget of widgets) {
            if (!widget.busy) {
                widget.setStatus(text);
            }
        }
    }

    // ---------------------------------------------------------------------
    // Monaco loading
    // ---------------------------------------------------------------------

    function injectScript(src) {
        return new Promise((resolve, reject) => {
            const script = document.createElement('script');
            script.src = src;
            script.onload = resolve;
            script.onerror = () => reject(new Error(`Failed to load ${src}`));
            document.head.appendChild(script);
        });
    }

    async function loadModelicaLanguage(monaco) {
        const root = bookRoot();
        for (const candidate of LANGUAGE_MODULE_CANDIDATES) {
            try {
                const module = await import(pageUrl(root + candidate));
                module.registerModelicaLanguage(monaco);
                return;
            } catch (_error) {
                // Try the next layout.
            }
        }
        // Minimal fallback so editors still open with comment support.
        if (!monaco.languages.getLanguages().some((lang) => lang.id === 'modelica')) {
            monaco.languages.register({ id: 'modelica' });
            monaco.languages.setLanguageConfiguration('modelica', {
                comments: { lineComment: '//', blockComment: ['/*', '*/'] },
            });
        }
    }

    function loadMonaco() {
        if (!monacoPromise) {
            monacoPromise = (async () => {
                if (!window.require) {
                    await injectScript(`${MONACO_CDN_BASE}/vs/loader.min.js`);
                }
                window.require.config({ paths: { vs: `${MONACO_CDN_BASE}/vs` } });
                await new Promise((resolve, reject) => {
                    window.require(['vs/editor/editor.main'], resolve, reject);
                });
                const monaco = window.monaco;
                await loadModelicaLanguage(monaco);
                return monaco;
            })();
        }
        return monacoPromise;
    }

    function monacoTheme() {
        const html = document.documentElement;
        const dark = ['coal', 'navy', 'ayu'].some((theme) => html.classList.contains(theme));
        return dark ? 'vs-dark' : 'vs';
    }

    // ---------------------------------------------------------------------
    // Language services (completion / hover / diagnostics) over the WASM LSP
    // ---------------------------------------------------------------------

    function lspRangeToMonaco(range) {
        const start = (range && range.start) || {};
        const end = (range && range.end) || {};
        return {
            startLineNumber: Math.max(1, Number(start.line ?? 0) + 1),
            startColumn: Math.max(1, Number(start.character ?? 0) + 1),
            endLineNumber: Math.max(1, Number(end.line ?? start.line ?? 0) + 1),
            endColumn: Math.max(1, Number(end.character ?? start.character ?? 0) + 1),
        };
    }

    function registerLanguageServices(wasm) {
        if (languageServicesRegistered || !window.monaco) {
            return;
        }
        languageServicesRegistered = true;
        const monaco = window.monaco;

        monaco.languages.registerCompletionItemProvider('modelica', {
            triggerCharacters: ['.', '(', ','],
            provideCompletionItems(model, position) {
                try {
                    const json = wasm.lsp_completion(
                        model.getValue(),
                        position.lineNumber - 1,
                        position.column - 1
                    );
                    const completions = JSON.parse(json);
                    const items = (completions && completions.items) || completions || [];
                    const kinds = monaco.languages.CompletionItemKind;
                    const kindMap = {
                        1: kinds.Text, 2: kinds.Method, 3: kinds.Function,
                        4: kinds.Constructor, 5: kinds.Field, 6: kinds.Variable,
                        7: kinds.Class, 8: kinds.Interface, 9: kinds.Module,
                        10: kinds.Property, 14: kinds.Keyword, 21: kinds.Constant,
                    };
                    return {
                        suggestions: items.map((item) => {
                            const suggestion = {
                                label: item.label,
                                kind: kindMap[item.kind] || kinds.Text,
                                insertText: item.insertText || item.label,
                                detail: item.detail,
                                documentation: item.documentation,
                            };
                            if (item.insertTextFormat === 2) {
                                suggestion.insertTextRules = monaco.languages
                                    .CompletionItemInsertTextRule.InsertAsSnippet;
                            }
                            return suggestion;
                        }),
                    };
                } catch (error) {
                    console.warn('rumoca-live completion error:', error);
                    return { suggestions: [] };
                }
            },
        });

        monaco.languages.registerHoverProvider('modelica', {
            provideHover(model, position) {
                try {
                    const json = wasm.lsp_hover(
                        model.getValue(),
                        position.lineNumber - 1,
                        position.column - 1
                    );
                    const hover = JSON.parse(json);
                    if (hover && hover.contents) {
                        const content = typeof hover.contents === 'string'
                            ? hover.contents
                            : (hover.contents.value || JSON.stringify(hover.contents));
                        return { contents: [{ value: content }] };
                    }
                } catch (error) {
                    console.warn('rumoca-live hover error:', error);
                }
                return null;
            },
        });
    }

    function updateDiagnostics(wasm, monaco, model) {
        let diagnostics = [];
        try {
            diagnostics = JSON.parse(wasm.lsp_diagnostics(model.getValue())) || [];
        } catch (error) {
            console.warn('rumoca-live diagnostics error:', error);
            return;
        }
        const severities = [
            monaco.MarkerSeverity.Error,
            monaco.MarkerSeverity.Error,
            monaco.MarkerSeverity.Warning,
            monaco.MarkerSeverity.Info,
            monaco.MarkerSeverity.Hint,
        ];
        const markers = diagnostics.map((diagnostic) => ({
            ...lspRangeToMonaco(diagnostic.range),
            severity: severities[diagnostic.severity] || monaco.MarkerSeverity.Error,
            message: String(diagnostic.message || ''),
            source: 'rumoca',
        }));
        monaco.editor.setModelMarkers(model, 'rumoca', markers);
    }

    // ---------------------------------------------------------------------
    // Model name + plotting helpers
    // ---------------------------------------------------------------------

    function inferModelName(wasm, source) {
        try {
            const state = JSON.parse(wasm.get_simulation_models(source, ''));
            if (state && state.selected_model) {
                return state.selected_model;
            }
        } catch (_error) {
            // Fall through to the regex below on parse errors.
        }
        const match = /\b(?:model|block|class)\s+([A-Za-z_][A-Za-z0-9_]*)/.exec(source);
        return match ? match[1] : null;
    }

    function niceTicks(min, max, count) {
        if (!isFinite(min) || !isFinite(max) || min === max) {
            const value = isFinite(min) ? min : 0;
            return [value];
        }
        const span = max - min;
        const step = Math.pow(10, Math.floor(Math.log10(span / count)));
        const scaled = span / count / step;
        const niceStep = step * (scaled >= 5 ? 10 : scaled >= 2 ? 5 : scaled >= 1 ? 2 : 1);
        const start = Math.ceil(min / niceStep) * niceStep;
        const ticks = [];
        for (let v = start; v <= max + niceStep * 1e-9; v += niceStep) {
            ticks.push(v);
        }
        return ticks;
    }

    function formatTick(value) {
        if (value === 0) return '0';
        const abs = Math.abs(value);
        if (abs >= 1e4 || abs < 1e-3) return value.toExponential(1);
        return String(parseFloat(value.toPrecision(4)));
    }

    function svgEl(tag, attrs) {
        const el = document.createElementNS('http://www.w3.org/2000/svg', tag);
        for (const [key, value] of Object.entries(attrs)) {
            el.setAttribute(key, value);
        }
        return el;
    }

    // Render a time-series plot of the selected series as an inline SVG.
    function renderPlot(container, times, series) {
        const width = 640;
        const height = 320;
        const margin = { left: 56, right: 12, top: 12, bottom: 56 };
        const plotW = width - margin.left - margin.right;
        const plotH = height - margin.top - margin.bottom;

        let yMin = Infinity;
        let yMax = -Infinity;
        for (const s of series) {
            for (const v of s.values) {
                if (isFinite(v)) {
                    yMin = Math.min(yMin, v);
                    yMax = Math.max(yMax, v);
                }
            }
        }
        if (!isFinite(yMin)) { yMin = 0; yMax = 1; }
        if (yMin === yMax) { yMin -= 1; yMax += 1; }
        const pad = (yMax - yMin) * 0.05;
        yMin -= pad;
        yMax += pad;
        const xMin = times[0];
        const xMax = times[times.length - 1];

        const sx = (t) => margin.left + ((t - xMin) / (xMax - xMin || 1)) * plotW;
        const sy = (v) => margin.top + (1 - (v - yMin) / (yMax - yMin)) * plotH;

        const svg = svgEl('svg', {
            viewBox: `0 0 ${width} ${height}`,
            class: 'rumoca-live-plot-svg',
            role: 'img',
        });
        svg.appendChild(svgEl('rect', {
            x: margin.left, y: margin.top, width: plotW, height: plotH,
            class: 'rumoca-live-plot-frame',
        }));

        for (const t of niceTicks(xMin, xMax, 6)) {
            const x = sx(t);
            svg.appendChild(svgEl('line', {
                x1: x, y1: margin.top, x2: x, y2: margin.top + plotH,
                class: 'rumoca-live-plot-grid',
            }));
            const label = svgEl('text', {
                x, y: margin.top + plotH + 18, 'text-anchor': 'middle',
                class: 'rumoca-live-plot-tick',
            });
            label.textContent = formatTick(t);
            svg.appendChild(label);
        }
        for (const v of niceTicks(yMin, yMax, 5)) {
            const y = sy(v);
            svg.appendChild(svgEl('line', {
                x1: margin.left, y1: y, x2: margin.left + plotW, y2: y,
                class: 'rumoca-live-plot-grid',
            }));
            const label = svgEl('text', {
                x: margin.left - 6, y: y + 4, 'text-anchor': 'end',
                class: 'rumoca-live-plot-tick',
            });
            label.textContent = formatTick(v);
            svg.appendChild(label);
        }
        const xAxis = svgEl('text', {
            x: margin.left + plotW / 2, y: margin.top + plotH + 36,
            'text-anchor': 'middle', class: 'rumoca-live-plot-axis',
        });
        xAxis.textContent = 'time [s]';
        svg.appendChild(xAxis);

        series.forEach((s, index) => {
            const color = SERIES_COLORS[index % SERIES_COLORS.length];
            const points = times
                .map((t, i) => `${sx(t).toFixed(1)},${sy(s.values[i]).toFixed(1)}`)
                .join(' ');
            svg.appendChild(svgEl('polyline', {
                points, fill: 'none', stroke: color, 'stroke-width': 1.8,
            }));
        });

        const legend = document.createElement('div');
        legend.className = 'rumoca-live-legend';
        series.forEach((s, index) => {
            const item = document.createElement('span');
            item.className = 'rumoca-live-legend-item';
            const swatch = document.createElement('span');
            swatch.className = 'rumoca-live-legend-swatch';
            swatch.style.background = SERIES_COLORS[index % SERIES_COLORS.length];
            item.appendChild(swatch);
            item.appendChild(document.createTextNode(s.name));
            legend.appendChild(item);
        });

        container.replaceChildren(svg, legend);
    }

    function pickPlotSeries(payload) {
        const names = payload.names || [];
        const allData = payload.allData || [];
        const data = allData.slice(1);
        const nStates = payload.nStates || names.length;
        // Rank states by dynamic range so flat series (e.g. clamped boundary
        // cells of a discretized field) do not crowd out the real dynamics.
        const candidates = [];
        for (let i = 0; i < Math.min(nStates, names.length); i++) {
            if (!Array.isArray(data[i])) {
                continue;
            }
            let lo = Infinity;
            let hi = -Infinity;
            for (const v of data[i]) {
                if (isFinite(v)) {
                    lo = Math.min(lo, v);
                    hi = Math.max(hi, v);
                }
            }
            candidates.push({ index: i, range: isFinite(hi - lo) ? hi - lo : 0 });
        }
        candidates.sort((a, b) => (b.range - a.range) || (a.index - b.index));
        const picked = candidates.slice(0, MAX_PLOT_SERIES);
        if (picked.length === 0 && names.length > 0 && Array.isArray(data[0])) {
            picked.push({ index: 0 });
        }
        picked.sort((a, b) => a.index - b.index);
        return picked.map(({ index }) => ({ name: names[index], values: data[index] }));
    }

    // ---------------------------------------------------------------------
    // Radial field visualization (`viz-radial` blocks)
    //
    // Interprets an array state such as T[1..N] as concentric shells of a
    // sphere and animates a colored cross-section over the simulation time.
    // ---------------------------------------------------------------------

    function findRadialField(payload) {
        const names = payload.names || [];
        const data = (payload.allData || []).slice(1);
        const nStates = payload.nStates || names.length;
        const groups = new Map();
        for (let i = 0; i < Math.min(nStates, names.length); i++) {
            const match = /^(.+)\[(\d+)\]$/.exec(names[i]);
            if (!match || !Array.isArray(data[i])) {
                continue;
            }
            const base = match[1];
            if (!groups.has(base)) {
                groups.set(base, []);
            }
            groups.get(base).push({ index: Number(match[2]), values: data[i] });
        }
        let best = null;
        for (const [base, members] of groups) {
            if (!best || members.length > best.members.length) {
                best = { base, members };
            }
        }
        if (!best || best.members.length < 2) {
            return null;
        }
        best.members.sort((a, b) => a.index - b.index);
        return best;
    }

    // Group 2-D array states such as u[i,j] into a matrix field.
    function findMatrixField(payload) {
        const names = payload.names || [];
        const data = (payload.allData || []).slice(1);
        const nStates = payload.nStates || names.length;
        const groups = new Map();
        for (let i = 0; i < Math.min(nStates, names.length); i++) {
            const match = /^(.+)\[(\d+)\s*,\s*(\d+)\]$/.exec(names[i]);
            if (!match || !Array.isArray(data[i])) {
                continue;
            }
            const base = match[1];
            if (!groups.has(base)) {
                groups.set(base, []);
            }
            groups.get(base).push({
                row: Number(match[2]),
                col: Number(match[3]),
                values: data[i],
            });
        }
        let best = null;
        for (const [base, members] of groups) {
            if (!best || members.length > best.members.length) {
                best = { base, members };
            }
        }
        if (!best || best.members.length < 4) {
            return null;
        }
        let rows = 0;
        let cols = 0;
        const byCell = new Map();
        for (const member of best.members) {
            rows = Math.max(rows, member.row);
            cols = Math.max(cols, member.col);
            byCell.set(`${member.row},${member.col}`, member.values);
        }
        return {
            base: best.base,
            rows,
            cols,
            members: best.members,
            at: (row, col) => byCell.get(`${row},${col}`) || null,
        };
    }

    function heatColor(fraction) {
        const f = Math.max(0, Math.min(1, fraction));
        // Blue (cold) through yellow to red (hot).
        const hue = 240 * (1 - f);
        return `hsl(${hue.toFixed(0)}, 85%, ${(35 + 20 * f).toFixed(0)}%)`;
    }

    function formatClock(seconds) {
        if (seconds >= 3600) {
            const h = Math.floor(seconds / 3600);
            const m = Math.floor((seconds % 3600) / 60);
            return `${h} h ${String(m).padStart(2, '0')} min`;
        }
        if (seconds >= 60) {
            const m = Math.floor(seconds / 60);
            const s = Math.floor(seconds % 60);
            return `${m} min ${String(s).padStart(2, '0')} s`;
        }
        return `${formatTick(seconds)} s`;
    }

    function makeCanvas(container, width, height) {
        const canvas = document.createElement('canvas');
        canvas.width = width;
        canvas.height = height;
        canvas.className = 'rumoca-live-radial-canvas';
        container.appendChild(canvas);
        return { canvas, ctx2d: canvas.getContext('2d') };
    }

    // Play button + scrubber + time label driving `drawFrame(frameIndex)`.
    // The string returned by drawFrame becomes the label text.
    function addAnimation(container, times, drawFrame, playDurationMs = 10000) {
        const controls = document.createElement('div');
        controls.className = 'rumoca-live-radial-controls';
        const playBtn = document.createElement('button');
        playBtn.type = 'button';
        playBtn.className = 'rumoca-live-button';
        playBtn.textContent = '▶';
        playBtn.setAttribute('aria-label', 'Play animation');
        const slider = document.createElement('input');
        slider.type = 'range';
        slider.min = '0';
        slider.max = String(times.length - 1);
        slider.value = '0';
        slider.className = 'rumoca-live-radial-slider';
        const clock = document.createElement('span');
        clock.className = 'rumoca-live-status';
        controls.append(playBtn, slider, clock);
        container.appendChild(controls);

        let playing = false;
        let rafId = null;
        const stop = () => {
            playing = false;
            playBtn.textContent = '▶';
            if (rafId !== null) {
                cancelAnimationFrame(rafId);
                rafId = null;
            }
        };
        const clampFrame = (frame) =>
            Math.max(0, Math.min(times.length - 1, Math.round(frame)));
        // A drawFrame exception must stop playback visibly, not freeze the
        // loop while the button still reads "playing".
        const draw = (frame) => {
            try {
                clock.textContent = drawFrame(clampFrame(frame)) || '';
            } catch (error) {
                stop();
                clock.textContent = `draw error: ${error.message || error}`;
                console.error('rumoca-live animation draw error:', error);
            }
        };

        playBtn.addEventListener('click', () => {
            if (playing) {
                stop();
                return;
            }
            playing = true;
            playBtn.textContent = '⏸';
            // Delta-time accumulation: robust to rAF timestamp skew and to
            // pauses, and resumes from the current slider position.
            let framePos = Number(slider.value) >= times.length - 1
                ? 0 : Number(slider.value);
            let lastTick = null;
            const framesPerMs = (times.length - 1) / playDurationMs;
            const step = (now) => {
                if (!playing) {
                    return;
                }
                if (lastTick !== null) {
                    const dt = Math.max(0, now - lastTick);
                    framePos += dt * framesPerMs;
                }
                lastTick = now;
                const frame = clampFrame(framePos);
                slider.value = String(frame);
                draw(frame);
                if (frame >= times.length - 1) {
                    stop();
                    return;
                }
                rafId = requestAnimationFrame(step);
            };
            rafId = requestAnimationFrame(step);
        });
        slider.addEventListener('input', () => {
            stop();
            draw(Number(slider.value));
        });
        draw(0);
        return { redraw: draw };
    }

    function addColorbar(container, lo, hi, colorFn) {
        const colorbar = document.createElement('div');
        colorbar.className = 'rumoca-live-radial-colorbar';
        const gradient = document.createElement('span');
        gradient.className = 'rumoca-live-radial-gradient';
        const stops = [];
        for (let i = 0; i <= 10; i++) {
            stops.push(colorFn(i / 10));
        }
        gradient.style.background = `linear-gradient(to right, ${stops.join(', ')})`;
        const loLabel = document.createElement('span');
        loLabel.textContent = formatTick(lo);
        const hiLabel = document.createElement('span');
        hiLabel.textContent = formatTick(hi);
        colorbar.append(loLabel, gradient, hiLabel);
        container.appendChild(colorbar);
    }

    function valueRange(members) {
        let vMin = Infinity;
        let vMax = -Infinity;
        for (const member of members) {
            for (const v of member.values) {
                if (isFinite(v)) {
                    vMin = Math.min(vMin, v);
                    vMax = Math.max(vMax, v);
                }
            }
        }
        if (!isFinite(vMin)) { vMin = 0; vMax = 1; }
        if (vMin === vMax) { vMax = vMin + 1; }
        return { vMin, vMax };
    }

    // The API handed to editable `js,rumoca-viz` blocks (and used by the
    // built-in `viz-radial` mode). Documented in the user guide.
    function makeVizApi(payload, container) {
        const names = payload.names || [];
        const data = (payload.allData || []).slice(1);
        return {
            arrayField: () => findRadialField(payload),
            matrixField: () => findMatrixField(payload),
            series: (name) => {
                const index = names.indexOf(name);
                return index >= 0 ? data[index] : null;
            },
            valueRange,
            heatColor,
            formatTick,
            formatClock,
            makeCanvas: (width, height) => makeCanvas(container, width, height),
            addAnimation: (times, drawFrame, durationMs) =>
                addAnimation(container, times, drawFrame, durationMs),
            addColorbar: (lo, hi, colorFn) =>
                addColorbar(container, lo, hi, colorFn || heatColor),
        };
    }

    // Built-in cross-section animation for `viz-radial` blocks without an
    // attached editable script.
    function renderRadialViz(container, times, payload) {
        const field = findRadialField(payload);
        if (!field) {
            return false;
        }
        const api = makeVizApi(payload, container);
        const { vMin, vMax } = valueRange(field.members);
        const size = 280;
        const { ctx2d } = api.makeCanvas(size, size);
        const n = field.members.length;
        api.addAnimation(times, (frame) => {
            ctx2d.clearRect(0, 0, size, size);
            const maxR = size / 2 - 6;
            // Draw shells outermost-first so inner shells paint on top.
            for (let i = n - 1; i >= 0; i--) {
                const fraction = (field.members[i].values[frame] - vMin) / (vMax - vMin);
                ctx2d.beginPath();
                ctx2d.arc(size / 2, size / 2, maxR * ((i + 1) / n), 0, 2 * Math.PI);
                ctx2d.fillStyle = heatColor(fraction);
                ctx2d.fill();
            }
            ctx2d.beginPath();
            ctx2d.arc(size / 2, size / 2, maxR, 0, 2 * Math.PI);
            ctx2d.strokeStyle = '#555';
            ctx2d.lineWidth = 1.5;
            ctx2d.stroke();
            return `t = ${formatClock(times[frame])} · `
                + `${field.base}[1] = ${formatTick(field.members[0].values[frame])} · `
                + `${field.base}[${n}] = ${formatTick(field.members[n - 1].values[frame])}`;
        });
        api.addColorbar(vMin, vMax, heatColor);
        return true;
    }

    function runCustomViz(container, payload, times, code) {
        const api = makeVizApi(payload, container);
        const render = new Function(
            '{ payload, times, names, data, container, api }',
            code
        );
        render({
            payload,
            times,
            names: payload.names || [],
            data: (payload.allData || []).slice(1),
            container,
            api,
        });
    }

    // render_target returns { ok, files: [{ path, content }, ...] }.
    function targetFilesToText(files) {
        if (Array.isArray(files)) {
            return files.map((file) => file.content || '').join('\n');
        }
        if (files instanceof Map) {
            return [...files.values()].join('\n');
        }
        return Object.values(files || {}).join('\n');
    }

    function describeRun(result) {
        const payload = result.payload || {};
        const details = payload.simDetails || {};
        const actual = details.actual || {};
        const requested = details.requested || {};
        const metrics = result.metrics || {};
        const parts = [];
        if (requested.solver) parts.push(`solver ${requested.solver}`);
        if (actual.points) parts.push(`${actual.points} points`);
        if (typeof actual.t_end === 'number') {
            parts.push(`t = ${formatTick(actual.t_start || 0)}…${formatTick(actual.t_end)} s`);
        }
        if (typeof metrics.simulateSeconds === 'number') {
            parts.push(`${(metrics.simulateSeconds * 1000).toFixed(1)} ms`);
        }
        return parts.join(' · ');
    }

    // ---------------------------------------------------------------------
    // Editor backends: Monaco (preferred) and a plain textarea fallback
    // ---------------------------------------------------------------------

    function createMonacoEditor(monaco, host, source, language = 'modelica') {
        const lineHeight = 19;
        const verticalPadding = 12;
        const heightFor = (text) => {
            const lines = Math.min(text.split('\n').length + 1, MAX_EDITOR_LINES);
            return Math.max(lines, 4) * lineHeight + verticalPadding;
        };
        host.style.height = `${heightFor(source)}px`;
        const editor = monaco.editor.create(host, {
            value: source,
            language,
            theme: monacoTheme(),
            minimap: { enabled: false },
            fontSize: 13,
            lineNumbers: 'on',
            lineNumbersMinChars: 3,
            automaticLayout: true,
            quickSuggestions: true,
            suggestOnTriggerCharacters: true,
            glyphMargin: false,
            folding: false,
            scrollBeyondLastLine: false,
            overviewRulerLanes: 0,
            renderLineHighlight: 'none',
            scrollbar: { alwaysConsumeMouseWheel: false },
        });
        editor.onDidChangeModelContent(() => {
            host.style.height = `${heightFor(editor.getValue())}px`;
        });
        return {
            getValue: () => editor.getValue(),
            setValue: (text) => editor.setValue(text),
            onChange: (cb) => editor.onDidChangeModelContent(cb),
            onFocus: (cb) => editor.onDidFocusEditorText(cb),
            model: editor.getModel(),
        };
    }

    function createTextareaEditor(host, source) {
        const editor = document.createElement('textarea');
        editor.className = 'rumoca-live-editor';
        editor.spellcheck = false;
        editor.value = source;
        editor.setAttribute('aria-label', 'Editable Modelica example');
        const resize = () => {
            editor.rows = Math.max(editor.value.split('\n').length + 1, 4);
        };
        resize();
        editor.addEventListener('input', resize);
        editor.addEventListener('keydown', (event) => {
            if (event.key === 'Tab') {
                event.preventDefault();
                const { selectionStart, selectionEnd, value } = editor;
                editor.value = value.slice(0, selectionStart) + '  ' + value.slice(selectionEnd);
                editor.selectionStart = editor.selectionEnd = selectionStart + 2;
            }
        });
        host.appendChild(editor);
        return {
            getValue: () => editor.value,
            setValue: (text) => { editor.value = text; resize(); },
            onChange: (cb) => editor.addEventListener('input', cb),
            onFocus: (cb) => editor.addEventListener('focus', cb),
            model: null,
        };
    }

    // ---------------------------------------------------------------------
    // Widget
    // ---------------------------------------------------------------------

    // Turn a `js,rumoca-viz` block into an editable visualization script
    // attached to the preceding interactive widget.
    function buildVizEditor(codeEl, monaco) {
        const pre = codeEl.parentElement;
        const originalSource = codeEl.textContent.replace(/\n$/, '');

        const details = document.createElement('details');
        details.className = 'rumoca-live-viz';
        const summary = document.createElement('summary');
        summary.textContent = 'Visualization script (JavaScript — editable)';
        const host = document.createElement('div');
        host.className = 'rumoca-live-editor-host';
        details.append(summary, host);
        pre.replaceWith(details);

        const editor = monaco
            ? createMonacoEditor(monaco, host, originalSource, 'javascript')
            : createTextareaEditor(host, originalSource);
        return {
            getValue: () => editor.getValue(),
            reset: () => editor.setValue(originalSource),
        };
    }

    function buildWidget(codeEl, monaco) {
        const pre = codeEl.parentElement;
        const originalSource = codeEl.textContent.replace(/\n$/, '');
        const wantsRadialViz = /\bviz-radial\b/.test(codeEl.className || '');

        const container = document.createElement('div');
        container.className = 'rumoca-live';
        const editorHost = document.createElement('div');
        editorHost.className = 'rumoca-live-editor-host';

        const toolbar = document.createElement('div');
        toolbar.className = 'rumoca-live-toolbar';
        const runBtn = document.createElement('button');
        runBtn.type = 'button';
        runBtn.className = 'rumoca-live-button rumoca-live-run';
        runBtn.textContent = '▶ Simulate';
        const daeBtn = document.createElement('button');
        daeBtn.type = 'button';
        daeBtn.className = 'rumoca-live-button';
        daeBtn.textContent = 'Show DAE';
        const resetBtn = document.createElement('button');
        resetBtn.type = 'button';
        resetBtn.className = 'rumoca-live-button';
        resetBtn.textContent = 'Reset';
        const status = document.createElement('span');
        status.className = 'rumoca-live-status';
        toolbar.append(runBtn, daeBtn, resetBtn, status);

        const progress = document.createElement('div');
        progress.className = 'rumoca-live-progress';
        progress.hidden = true;
        const progressFill = document.createElement('div');
        progressFill.className = 'rumoca-live-progress-fill';
        progress.appendChild(progressFill);

        const output = document.createElement('div');
        output.className = 'rumoca-live-output';
        output.hidden = true;

        container.append(editorHost, toolbar, progress, output);
        pre.replaceWith(container);

        const editor = monaco
            ? createMonacoEditor(monaco, editorHost, originalSource)
            : createTextareaEditor(editorHost, originalSource);

        const lastRunMs = {};
        let progressTimer = null;
        const beginProgress = (key, label) => {
            const started = performance.now();
            const expected = lastRunMs[key];
            progress.hidden = false;
            progressFill.classList.toggle('rumoca-live-progress-indeterminate', !expected);
            progressFill.style.width = expected ? '0%' : '100%';
            const tick = () => {
                const elapsed = performance.now() - started;
                if (expected) {
                    const pct = Math.min(97, (elapsed / expected) * 100);
                    progressFill.style.width = `${pct.toFixed(1)}%`;
                }
                const suffix = expected
                    ? ` ${(elapsed / 1000).toFixed(0)} / ~${(expected / 1000).toFixed(0)} s`
                    : ` ${(elapsed / 1000).toFixed(0)} s`;
                status.textContent = label + suffix;
            };
            tick();
            progressTimer = setInterval(tick, 250);
            return started;
        };
        const endProgress = (key, started, succeeded) => {
            clearInterval(progressTimer);
            progressTimer = null;
            progress.hidden = true;
            if (succeeded) {
                lastRunMs[key] = performance.now() - started;
            }
        };

        const widget = {
            busy: false,
            vizEditor: null,
            setStatus(text) { status.textContent = text; },
            onWasmReady(wasm) {
                if (!monaco || !editor.model) {
                    return;
                }
                updateDiagnostics(wasm, monaco, editor.model);
                let timer = null;
                editor.onChange(() => {
                    clearTimeout(timer);
                    timer = setTimeout(
                        () => updateDiagnostics(wasm, monaco, editor.model),
                        DIAGNOSTIC_DEBOUNCE_MS
                    );
                });
            },
        };
        widgets.push(widget);

        // Editing intent: start the WASM download so diagnostics and
        // completion are ready by the time they are wanted.
        editor.onFocus(() => {
            if (!wasmRequested) {
                loadWasm().catch(() => { /* surfaced via status */ });
            }
        });

        const showError = (error) => {
            const message = error instanceof Error ? error.message : String(error);
            const errBox = document.createElement('pre');
            errBox.className = 'rumoca-live-error';
            errBox.textContent = message;
            output.replaceChildren(errBox);
            output.hidden = false;
            widget.setStatus('Failed');
        };

        const withWasm = async (key, busyLabel, action) => {
            runBtn.disabled = true;
            daeBtn.disabled = true;
            widget.busy = true;
            let started = null;
            let succeeded = false;
            try {
                const wasm = await loadWasm();
                started = beginProgress(key, busyLabel);
                const source = editor.getValue();
                const model = inferModelName(wasm, source);
                if (!model) {
                    throw new Error('No model/block/class found in this example.');
                }
                await action(wasm, source, model);
                succeeded = true;
            } catch (error) {
                showError(error);
            } finally {
                if (started !== null) {
                    endProgress(key, started, succeeded);
                }
                widget.busy = false;
                runBtn.disabled = false;
                daeBtn.disabled = false;
            }
        };

        runBtn.addEventListener('click', () => withWasm('simulate', 'Compiling & simulating…', async (wasm, source, model) => {
            // t_end = 0 / dt = 0 / solver = "" defer to the model's
            // experiment annotation, falling back to runtime defaults. The
            // call runs in a worker so the page (and progress bar) stay live.
            const raw = await runHeavy('simulate', { source, model },
                () => wasm.simulate_model(source, model, 0, 0, ''));
            const result = JSON.parse(raw);
            const payload = result.payload || {};
            const allData = payload.allData || [];
            if (allData.length < 2) {
                throw new Error('Simulation produced no plottable variables.');
            }
            const views = [];
            if (widget.vizEditor) {
                const custom = document.createElement('div');
                custom.className = 'rumoca-live-radial';
                try {
                    runCustomViz(custom, payload, allData[0], widget.vizEditor.getValue());
                    views.push(custom);
                } catch (error) {
                    const errBox = document.createElement('pre');
                    errBox.className = 'rumoca-live-error';
                    errBox.textContent = `Visualization script error: ${error.message || error}`;
                    views.push(errBox);
                }
            } else if (wantsRadialViz) {
                const radial = document.createElement('div');
                radial.className = 'rumoca-live-radial';
                if (renderRadialViz(radial, allData[0], payload)) {
                    views.push(radial);
                }
            }
            const plot = document.createElement('div');
            plot.className = 'rumoca-live-plot';
            renderPlot(plot, allData[0], pickPlotSeries(payload));
            views.push(plot);
            output.replaceChildren(...views);
            output.hidden = false;
            widget.setStatus(describeRun(result));
        }));

        daeBtn.addEventListener('click', () => withWasm('dae', 'Compiling to DAE…', async (wasm, source, model) => {
            const text = await runHeavy('dae', { source, model }, () => {
                // compile() returns an envelope; render_target wants the
                // native DAE document from its `dae_native` field.
                const envelope = JSON.parse(wasm.compile(source, model));
                const daeJson = JSON.stringify(envelope.dae_native || envelope.dae);
                const rendered = wasm.render_target(daeJson, model, 'dae-modelica', '', '{}');
                return targetFilesToText(rendered.files);
            });
            const daeBox = document.createElement('pre');
            daeBox.className = 'rumoca-live-dae';
            daeBox.textContent = text || '(empty render)';
            output.replaceChildren(daeBox);
            output.hidden = false;
            widget.setStatus('Flattened DAE (the form the solver integrates)');
        }));

        resetBtn.addEventListener('click', () => {
            editor.setValue(originalSource);
            if (widget.vizEditor) {
                widget.vizEditor.reset();
            }
            output.hidden = true;
            output.replaceChildren();
            widget.setStatus('');
        });

        return widget;
    }

    async function init() {
        const liveBlocks = [...document.querySelectorAll('pre > code')].filter((code) => {
            const cls = code.className || '';
            const isInteractive = cls.includes('language-modelica') && /\binteractive\b/.test(cls);
            const isViz = cls.includes('language-js') && /\brumoca-viz\b/.test(cls);
            return isInteractive || isViz;
        });
        if (liveBlocks.length === 0) {
            return;
        }
        let monaco = null;
        try {
            monaco = await loadMonaco();
        } catch (error) {
            console.warn('rumoca-live: Monaco unavailable, using plain editors:', error);
        }
        // Document order pairs each `js,rumoca-viz` script with the
        // interactive widget that precedes it.
        let lastWidget = null;
        for (const code of liveBlocks) {
            const cls = code.className || '';
            if (/\brumoca-viz\b/.test(cls)) {
                if (lastWidget) {
                    lastWidget.vizEditor = buildVizEditor(code, monaco);
                } else {
                    console.warn('rumoca-live: viz script with no preceding interactive block');
                }
            } else {
                lastWidget = buildWidget(code, monaco);
            }
        }
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', init);
    } else {
        init();
    }
})();
