import { setupCommandPalette } from './modules/command_palette.js';
import { createSourceBreadcrumbs } from './modules/breadcrumbs.js';
import { createLibraryDocsController } from './modules/library_docs.js';
import { createDiagnosticsController } from './modules/diagnostics_panel.js';
import { installFileActions } from './modules/file_actions.js';
import { setupMonacoWorkspace } from './modules/monaco_setup.js';

let editor;

// Panel references
const resizeHandleH = document.getElementById('resizeHandleH');
const leftArea = document.querySelector('.left-area');
let isResizingH = false;

function isNarrowLayout() {
    return window.innerWidth <= 980;
}

// Horizontal resize between left area and right panel
resizeHandleH.addEventListener('mousedown', (e) => {
    if (isNarrowLayout()) return;
    isResizingH = true;
    document.body.style.cursor = 'ew-resize';
    document.body.style.userSelect = 'none';
});

const layoutAllEditors = () => {
    if (window.editor) window.editor.layout();
    if (window.templateEditor) window.templateEditor.layout();
    if (window.outputEditor) window.outputEditor.layout();
    if (window.codegenOutputEditor) window.codegenOutputEditor.layout();
};

function syncResponsiveLayoutState() {
    if (!leftArea) return;
    if (isNarrowLayout()) {
        leftArea.style.width = '';
        leftArea.style.flex = '1 1 auto';
    } else if (leftArea.style.flex === '1 1 auto') {
        leftArea.style.flex = '1';
    }
}

window.addEventListener('resize', syncResponsiveLayoutState);
syncResponsiveLayoutState();

document.addEventListener('mousemove', (e) => {
    if (isResizingH) {
        if (isNarrowLayout()) {
            isResizingH = false;
            document.body.style.cursor = '';
            document.body.style.userSelect = '';
            return;
        }
        const containerRect = document.querySelector('.main-container').getBoundingClientRect();
        const newWidth = e.clientX - containerRect.left;
        leftArea.style.flex = 'none';
        leftArea.style.width = Math.max(200, newWidth) + 'px';
        layoutAllEditors();
    }
    if (isResizingV) {
        const leftAreaEl = document.querySelector('.left-area');
        const leftAreaRect = leftAreaEl.getBoundingClientRect();
        const newHeight = leftAreaRect.bottom - e.clientY;
        const clampedHeight = Math.max(100, Math.min(newHeight, window.innerHeight * 0.5));
        bottomPanel.style.height = clampedHeight + 'px';
        if (window.editor) window.editor.layout();
    }
});

document.addEventListener('mouseup', () => {
    if (isResizingH || isResizingV) {
        isResizingH = false;
        isResizingV = false;
        document.body.style.cursor = '';
        document.body.style.userSelect = '';
        layoutAllEditors();
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
window.activeRightTab = 'simulate';

// Switch between right panel tabs (Simulate / DAE / Codegen)
window.switchRightTab = function(tabName) {
    window.activeRightTab = tabName;
    // Update tab buttons
    document.querySelectorAll('.right-tab-bar .right-tab').forEach(tab => {
        tab.classList.toggle('active', tab.dataset.tab === tabName);
    });
    // Update tab content
    document.getElementById('simTab').classList.toggle('active', tabName === 'simulate');
    document.getElementById('daeTab').classList.toggle('active', tabName === 'dae');
    document.getElementById('codegenTab').classList.toggle('active', tabName === 'codegen');
    // Refresh editors in the newly visible tab
    setTimeout(() => {
        layoutAllEditors();
        // If switching to DAE or Codegen, refresh the output
        const modelName = document.getElementById('modelSelect').value;
        if (modelName && window.compiledModels && window.compiledModels[modelName]) {
            if (tabName === 'dae') {
                displayDaeOutput(modelName);
            } else if (tabName === 'codegen') {
                displayCodegenOutput(modelName);
            }
        }
        // Rebuild sim plot if switching to simulate
        if (tabName === 'simulate' && window.simData) {
            rebuildSimPlot();
        }
    }, 0);
};

// Toggle bottom panel collapse/expand (collapses down)
window.toggleBottomPanel = function() {
    bottomPanel.classList.toggle('collapsed');
    const arrow = document.getElementById('bottomArrow');
    if (bottomPanel.classList.contains('collapsed')) {
        arrow.innerHTML = '&#9650;'; // Up arrow when collapsed
    } else {
        arrow.innerHTML = '&#9660;'; // Down arrow when expanded
    }
    if (window.editor) window.editor.layout();
};

// Switch between bottom panel tabs
window.switchBottomTab = function(tabName) {
    // Update tab buttons (only within bottom panel)
    document.querySelectorAll('.panel-header-bottom .bottom-tab').forEach(tab => {
        tab.classList.toggle('active', tab.dataset.tab === tabName);
    });
    // Update sections
    document.getElementById('outputSection').classList.toggle('active', tabName === 'output');
    document.getElementById('errorsSection').classList.toggle('active', tabName === 'errors');
    document.getElementById('librariesSection').classList.toggle('active', tabName === 'libraries');
    document.getElementById('packagesSection').classList.toggle('active', tabName === 'packages');
    if (tabName === 'packages' && typeof window.refreshPackageViewer === 'function') {
        window.refreshPackageViewer();
    }
};

function parseBadgeNumber(text) {
    const match = String(text || '').match(/\d+/);
    return match ? Number(match[0]) : 0;
}

function updateGlobalStatusBar() {
    const modelSelect = document.getElementById('modelSelect');
    const diagnosticsBadge = document.getElementById('diagnosticsCount');
    const compileStatus = document.getElementById('autoCompileStatus');
    const libraryBadge = document.getElementById('libCount');

    const modelLabel = document.getElementById('statusBarModel');
    const problemsLabel = document.getElementById('statusBarProblems');
    const compileLabel = document.getElementById('statusBarCompile');
    const librariesLabel = document.getElementById('statusBarLibraries');
    if (!modelLabel || !problemsLabel || !compileLabel || !librariesLabel) return;

    const selectedModel = String(modelSelect?.value || '').trim();
    const problemText = String(diagnosticsBadge?.textContent || '').trim();
    const compileText = String(compileStatus?.textContent || '').trim();
    const libraryText = String(libraryBadge?.textContent || '').trim();

    modelLabel.textContent = `Model: ${selectedModel || '--'}`;
    problemsLabel.textContent = `Problems: ${problemText || parseBadgeNumber(problemText)}`;
    compileLabel.textContent = `Compile: ${compileText || 'idle'}`;
    librariesLabel.textContent = `Libraries: ${parseBadgeNumber(libraryText)}`;
}

function bindStatusBarObservers() {
    const idsToWatch = ['diagnosticsCount', 'autoCompileStatus', 'libCount'];
    const observer = new MutationObserver(() => updateGlobalStatusBar());
    for (const id of idsToWatch) {
        const element = document.getElementById(id);
        if (!element) continue;
        observer.observe(element, {
            childList: true,
            subtree: true,
            characterData: true,
            attributes: true,
        });
    }

    const modelSelect = document.getElementById('modelSelect');
    if (modelSelect) {
        modelSelect.addEventListener('change', () => updateGlobalStatusBar());
    }
}

bindStatusBarObservers();
window.updateGlobalStatusBar = updateGlobalStatusBar;
setTimeout(() => updateGlobalStatusBar(), 0);

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

// --- Simulation state ---
window.simPlot = null;
window.simData = null;

const simPalette = [
    '#4ec9b0','#569cd6','#ce9178','#dcdcaa','#c586c0',
    '#9cdcfe','#d7ba7d','#608b4e','#d16969','#b5cea8',
    '#6a9955','#c8c8c8','#e8c87a','#7fdbca','#f07178'
];

function formatNum(v) {
    const n = Number(v);
    if (!Number.isFinite(n)) return String(v);
    if (Math.abs(n) >= 1000 || (Math.abs(n) > 0 && Math.abs(n) < 0.001)) return n.toExponential(3);
    return n.toFixed(4).replace(/\.?0+$/, '');
}

window.runSimulation = async function() {
    const modelName = document.getElementById('modelSelect').value;
    if (!modelName) { document.getElementById('simStatus').textContent = 'No model selected'; return; }
    const tEnd = parseFloat(document.getElementById('simTEnd').value) || 1.0;
    const dt = parseFloat(document.getElementById('simDt').value) || 0;
    const source = window.editor ? window.editor.getValue() : '';
    const btn = document.getElementById('simRunBtn');
    const status = document.getElementById('simStatus');
    const hover = document.getElementById('simHover');

    btn.disabled = true;
    status.textContent = 'Simulating...';
    status.style.color = '#9a6700';
    if (hover) hover.textContent = '';

    try {
        const t0 = performance.now();
        const json = await sendRequest('simulate', { source, modelName, tEnd, dt }, 60000);
        const result = JSON.parse(json);
        const elapsed = (performance.now() - t0).toFixed(0);
        window.simData = result;
        status.textContent = `${result.times.length} pts, ${result.names.length} vars (${elapsed}ms)`;
        status.style.color = '#2d6a4f';
        if (hover) hover.textContent = 'Hover plot for values';
        buildSimCheckboxes(result);
        rebuildSimPlot();
    } catch (e) {
        status.textContent = e.message || 'Simulation failed';
        status.style.color = '#c9184a';
        if (hover) hover.textContent = '';
    } finally {
        btn.disabled = false;
    }
};

function buildSimCheckboxes(result) {
    const el = document.getElementById('simVarTree');
    // Group variables by dot-separated prefix into a tree
    const tree = {};
    for (let i = 0; i < result.names.length; i++) {
        const name = result.names[i];
        const parts = name.split('.');
        let node = tree;
        for (let p = 0; p < parts.length - 1; p++) {
            const key = parts[p];
            if (!node[key]) node[key] = { _children: {} };
            node = node[key]._children;
        }
        const leaf = parts[parts.length - 1];
        if (!node[leaf]) node[leaf] = {};
        node[leaf]._idx = i;
        node[leaf]._checked = i < result.n_states;
    }

    function renderNode(obj, depth) {
        let html = '';
        // Separate groups (have _children) from leaves (have _idx)
        const keys = Object.keys(obj).filter(k => k !== '_children' && k !== '_idx' && k !== '_checked');
        for (const key of keys) {
            const item = obj[key];
            const pad = (depth * 12) + 4;
            if (item._idx !== undefined) {
                // Leaf variable
                const c = simPalette[item._idx % simPalette.length];
                const chk = item._checked ? 'checked' : '';
                html += `<label class="sim-var-leaf" style="padding-left:${pad}px">` +
                    `<input type="checkbox" data-idx="${item._idx}" ${chk}>` +
                    `<span style="color:${c}">\u25CF</span> ${key}</label>`;
            }
            if (item._children && Object.keys(item._children).length > 0) {
                // Group node
                html += `<div class="sim-var-group" style="padding-left:${pad}px">` +
                    `<span class="sim-var-toggle" onclick="this.parentElement.classList.toggle('collapsed')">` +
                    `${key}</span></div>`;
                html += `<div class="sim-var-group-body">` +
                    renderNode(item._children, depth + 1) + `</div>`;
            }
        }
        return html;
    }

    el.innerHTML = renderNode(tree, 0);
    el.querySelectorAll('input[type=checkbox]').forEach(cb => cb.addEventListener('change', rebuildSimPlot));
}

function rebuildSimPlot() {
    const result = window.simData;
    if (!result) return;
    const el = document.getElementById('simPlot');
    const treeEl = document.getElementById('simVarTree');
    const hoverEl = document.getElementById('simHover');

    const active = [];
    treeEl.querySelectorAll('input:checked').forEach(cb => active.push(parseInt(cb.dataset.idx, 10)));

    if (window.simPlot) { window.simPlot.destroy(); window.simPlot = null; }
    if (active.length === 0) {
        if (hoverEl) hoverEl.textContent = '';
        el.innerHTML = '<p style="padding:20px;color:#888">Select variables to plot</p>';
        return;
    }

    const data = [result.times];
    const series = [{}];
    const activeSeriesIdx = [...active];
    activeSeriesIdx.forEach(idx => {
        data.push(result.data[idx]);
        series.push({ label: result.names[idx], stroke: simPalette[idx % simPalette.length], width: 1.5 });
    });

    window.simPlot = new uPlot({
        width: el.clientWidth,
        height: Math.max((el.clientHeight || 400) - 30, 200),
        padding: [8, 8, 28, 8],
        scales: { x: { time: false } },
        axes: [
            { stroke: '#888', grid: { stroke: '#333' }, label: 'time', labelGap: 2, size: 36, font: '11px monospace', labelFont: '12px monospace' },
            { stroke: '#888', grid: { stroke: '#333' }, font: '11px monospace' }
        ],
        series,
        cursor: { drag: { x: true, y: true } },
        hooks: {
            setCursor: [function(u) {
                if (!hoverEl) return;
                const idx = u.cursor.idx;
                if (idx == null || idx < 0 || idx >= result.times.length) {
                    hoverEl.textContent = 'Hover plot for values';
                    return;
                }
                const parts = [`t=${formatNum(Number(result.times[idx]))}`];
                for (let i = 0; i < activeSeriesIdx.length; i++) {
                    const sIdx = activeSeriesIdx[i];
                    const col = result.data[sIdx] || [];
                    if (idx < col.length) {
                        parts.push(`${result.names[sIdx]}=${formatNum(Number(col[idx]))}`);
                    }
                }
                hoverEl.textContent = parts.join(' | ');
            }]
        },
        legend: { show: false }
    }, data, el);
}

// Resize sim plot on window resize
window.addEventListener('resize', function() {
    if (window.simPlot) {
        const el = document.getElementById('simPlot');
        window.simPlot.setSize({ width: el.clientWidth, height: Math.max((el.clientHeight || 400) - 30, 200) });
    }
});

// Display output for the active tab
function displayModelOutput(modelName) {
    if (window.activeRightTab === 'dae') {
        displayDaeOutput(modelName);
    } else if (window.activeRightTab === 'codegen') {
        displayCodegenOutput(modelName);
    }
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

// Display codegen output (render template)
async function displayCodegenOutput(modelName) {
    const result = window.compiledModels[modelName];
    if (!result || result.error || !result.dae) {
        setCodegenOutput(result?.error ? '' : 'No DAE available');
        return;
    }
    const template = window.templateEditor ? window.templateEditor.getValue() : '';
    if (!template.trim()) {
        setCodegenOutput('Enter or select a template to render output.');
        clearTemplateErrors();
        return;
    }
    if (!result.dae_native) {
        setCodegenOutput('Native DAE not available for template rendering.');
        clearTemplateErrors();
        return;
    }
    try {
        const daeJson = JSON.stringify(result.dae_native);
        const rendered = await sendRequest('renderTemplate', { daeJson, template });
        setCodegenOutput(rendered);
        clearTemplateErrors();
    } catch (e) {
        setCodegenOutput('');
        showTemplateError(e.message);
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

// Set codegen output (Codegen tab) - uses Monaco editor
function setCodegenOutput(text, language) {
    if (window.codegenOutputEditor) {
        const lang = language || 'plaintext';
        const model = window.codegenOutputEditor.getModel();
        if (model) {
            monaco.editor.setModelLanguage(model, lang);
        }
        window.codegenOutputEditor.setValue(text || '');
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
    // Handle Simple equation variant
    if (eq.Simple) {
        return `${formatExpr(eq.Simple.lhs)} = ${formatExpr(eq.Simple.rhs)}`;
    }
    // Handle direct lhs/rhs format
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

const libraryDocsController = createLibraryDocsController({
    sendRequest,
    setTerminalOutput,
    isWorkerReady: () => workerReady,
});
libraryDocsController.bindWindowApi();

// Store compiled results for all models: { modelName: { dae, balance } }
window.compiledModels = {};
window.selectedModel = null;
installFileActions({
    getEditor: () => window.editor,
    getCompiledModels: () => window.compiledModels,
    getDaeFormat: () => window.daeFormat,
    getCodegenOutputEditor: () => window.codegenOutputEditor,
});

// Update display when model selection changes
window.updateSelectedModel = function() {
    const modelName = document.getElementById('modelSelect').value;
    window.selectedModel = modelName;
    if (modelName && window.compiledModels[modelName]) {
        const result = window.compiledModels[modelName];
        // Refresh the active tab's output
        if (window.activeRightTab === 'dae') displayDaeOutput(modelName);
        else if (window.activeRightTab === 'codegen') displayCodegenOutput(modelName);
        // Update DAE for template autocompletion
        if (result.dae_native) {
            window.currentDaeForCompletions = result.dae_native;
        }
        // Update balance status
        const statusSpan = document.getElementById('autoCompileStatus');
        const b = result.balance;
        if (b) {
            const balanceStatus = b.is_balanced ? 'BALANCED' :
                (typeof b.status === 'string' ? b.status.toUpperCase() :
                (b.status.CompileError ? 'ERROR' : 'UNBALANCED'));
            statusSpan.textContent = b.is_balanced ? `${modelName}: Balanced` : `${modelName}: ${balanceStatus}`;
            statusSpan.style.color = b.is_balanced ? '#2d6a4f' : '#c9184a';
        }
    } else if (!modelName) {
        setDaeOutput('No model selected');
    }
    updateSourceBreadcrumbs();
    updateGlobalStatusBar();
};

// Detect model/class definitions in source code
function detectModels(source) {
    // Heuristic extractor for compilable roots (model/block/class).
    // Restrict matching to declaration-like lines and ignore comments so text
    // like "block form" in comments does not create fake models.
    const lineRegex = /^\s*(?:final\s+)?(?:encapsulated\s+)?(?:partial\s+)?(?:model|block|class)\s+([A-Za-z_][A-Za-z0-9_]*)\b/;
    const blockCommentsStripped = source.replace(/\/\*[\s\S]*?\*\//g, '');
    const models = [];
    const seen = new Set();

    for (const rawLine of blockCommentsStripped.split(/\r?\n/)) {
        const line = rawLine.replace(/\/\/.*$/, '');
        const match = line.match(lineRegex);
        if (!match) continue;
        const name = match[1];
        if (seen.has(name)) continue;
        seen.add(name);
        models.push(name);
    }

    return models;
}

const sourceBreadcrumbs = createSourceBreadcrumbs();

function updateSourceBreadcrumbs() {
    sourceBreadcrumbs.update();
}

// Web Worker setup (cache-busted to avoid stale JS/WASM bundles during local iteration)
const workerCacheBust = String(Date.now());
const workerUrl = new URL('../../pkg/rumoca_worker.js', window.location.href);
workerUrl.searchParams.set('v', workerCacheBust);
const worker = new Worker(workerUrl, { type: 'module' });
let requestId = 0;
const pendingRequests = new Map();
let workerReady = false;

worker.onmessage = (e) => {
    const { id, ready, success, result, error, progress, current, total, percent } = e.data;

    // Handle progress updates for library loading
    if (progress) {
        libraryDocsController.handleWorkerProgress({ current, total, percent });
        return;
    }

    if (ready !== undefined) {
        const status = document.getElementById('status');
        if (success) {
            status.textContent = 'Ready';
            status.className = 'status ready';
            workerReady = true;
            if (window.refreshModelicaSemanticTokens) {
                window.refreshModelicaSemanticTokens();
            }
            // Fetch version and show welcome message
            sendRequest('getVersion', {}).then(version => {
                setTerminalOutput(`Rumoca v${version} - Modelica Compiler\nHover over tabs/buttons for help.`);
            }).catch(() => {
                setTerminalOutput('WASM initialized! Edit code to compile.');
            });
        } else {
            status.textContent = 'Error';
            status.className = 'status error';
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
    document.getElementById('status').textContent = 'Worker Error';
    document.getElementById('status').className = 'status error';
};

function sendRequest(action, params = {}, timeout = 30000) {
    const id = ++requestId;
    return new Promise((resolve, reject) => {
        const timeoutId = setTimeout(() => {
            if (pendingRequests.has(id)) {
                pendingRequests.delete(id);
                console.error(`[sendRequest] timeout for action '${action}' (id=${id})`);
                reject(new Error(`Request timeout for ${action}`));
            }
        }, timeout);

        pendingRequests.set(id, {
            resolve: (result) => {
                clearTimeout(timeoutId);
                resolve(result);
            },
            reject: (error) => {
                clearTimeout(timeoutId);
                reject(error);
            }
        });
        worker.postMessage({ id, action, ...params });
    });
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
                detail: 'Select model in right panel',
                tags: ['model', 'open'],
                run: () => {
                    modelSelect.value = modelName;
                    if (typeof window.updateSelectedModel === 'function') {
                        window.updateSelectedModel();
                    }
                    if (typeof window.switchRightTab === 'function') {
                        window.switchRightTab('dae');
                    }
                    if (editor) editor.focus();
                },
            });
        }
    }

    if (!workerReady) return items;

    try {
        const json = await sendRequest('listClasses', {});
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
                    if (typeof window.switchBottomTab === 'function') {
                        window.switchBottomTab('packages');
                    }
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

function flattenNestedDocumentSymbols(symbols, sink, parentPath = '') {
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
                tags: ['symbol', 'outline'],
                run: () => jumpToModelicaLocation(pos.lineNumber, pos.column),
            });
        }
        flattenNestedDocumentSymbols(symbol.children, sink, fullName);
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
            tags: ['symbol', 'outline'],
            run: () => jumpToModelicaLocation(pos.lineNumber, pos.column),
        });
    }
    return sink;
}

async function buildDocumentSymbolItems() {
    if (!editor || !workerReady) return [];
    try {
        const source = editor.getValue();
        const json = await sendRequest('documentSymbols', { source });
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
require.config({ paths: { 'vs': 'https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.45.0/min/vs' }});
require(['vs/editor/editor.main'], function() {
    const monacoState = setupMonacoWorkspace({ monaco, sendRequest, layoutAllEditors });
    editor = monacoState.editor;
    const templateEditor = monacoState.templateEditor;
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

    diagnosticsController = createDiagnosticsController({
        monaco,
        getModelEditor: () => editor,
        getTemplateEditor: () => window.templateEditor,
        switchToErrorsTab: () => window.switchBottomTab('errors'),
        triggerModelicaQuickFix: triggerModelicaQuickFixAt,
    });
    window.renderAllDiagnostics = () => diagnosticsController.renderAllDiagnostics();

    let liveCheckGeneration = 0;
    const runLiveChecks = debounce(async () => {
        if (!workerReady) return;
        const runGeneration = ++liveCheckGeneration;
        const isStaleRun = () => runGeneration !== liveCheckGeneration;

        const source = editor.getValue();
        const statusSpan = document.getElementById('autoCompileStatus');
        const modelSelect = document.getElementById('modelSelect');
        const startTime = performance.now();

        statusSpan.textContent = 'Compiling...';
        statusSpan.style.color = '#9a6700';
        updateCompileErrors([]);

        try {
        let diagnostics = [];
        // Run diagnostics
        try {
            const diagJson = await sendRequest('diagnostics', { source });
            if (isStaleRun()) return;
            diagnostics = normalizeDiagnosticsPayload(JSON.parse(diagJson), source);
            if (isStaleRun()) return;
            updateDiagnostics(diagnostics);
        } catch (e) {
            if (isStaleRun()) return;
            console.warn('Live diagnostics error:', e);
            // Clear old diagnostics on error to avoid showing stale errors
            updateDiagnostics([]);
            diagnostics = [];
        }

        // Detect all models in source
        const models = detectModels(source);
        if (isStaleRun()) return;

        // Update dropdown
        const previousSelection = modelSelect.value;
        modelSelect.innerHTML = models.length === 0
            ? '<option value="">-- No models --</option>'
            : models.map(m => `<option value="${m}">${m}</option>`).join('');

        // Restore selection if possible, otherwise select first
        if (models.includes(previousSelection)) {
            modelSelect.value = previousSelection;
        } else if (models.length > 0) {
            modelSelect.value = models[0];
        }
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
            statusSpan.textContent = hasParseErrors
                ? `Syntax Error (${elapsed}ms)`
                : `Error (${elapsed}ms)`;
            statusSpan.style.color = '#c9184a';
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
            if (document.getElementById('packagesSection').classList.contains('active')) {
                libraryDocsController.refreshPackageViewer(true);
            }
            return;
        }

        // Compile all models
        let cachedCount = 0;
        try { cachedCount = await sendRequest('getLibraryCount', {}); } catch (e) {}

        let successCount = 0;
        const compileErrors = [];

        for (const modelName of models) {
            if (isStaleRun()) return;
            try {
                let json;
                console.log('[compile] compiling model:', modelName);
                if (cachedCount > 0) {
                    json = await sendRequest('compileWithLibraries', { source, modelName, libraries: '{}' });
                } else {
                    json = await sendRequest('compile', { source, modelName });
                }
                if (isStaleRun()) return;
                const result = JSON.parse(json);
                console.log('[compile] got result for', modelName, '- pretty length:', result.pretty?.length, 'dae keys:', Object.keys(result.dae || {}));
                window.compiledModels[modelName] = {
                    dae: result.dae,
                    dae_native: result.dae_native,
                    balance: result.balance,
                    pretty: result.pretty
                };
                // Update DAE for template autocompletion (use dae_native for actual field names)
                if (result.dae_native && (modelName === modelSelect.value || modelSelect.value === '')) {
                    window.currentDaeForCompletions = result.dae_native;
                    console.log('[compile] updated currentDaeForCompletions for', modelName);
                }
                successCount++;
            } catch (e) {
                if (isStaleRun()) return;
                console.log('[compile] error for', modelName, ':', e.message);
                window.compiledModels[modelName] = {
                    dae: null,
                    balance: null,
                    error: e.message
                };
                compileErrors.push({ model: modelName, message: e.message });
            }
        }

        const elapsed = (performance.now() - startTime).toFixed(0);
        if (isStaleRun()) return;
        updateCompileErrors(compileErrors);

        // Update display with selected model
        let selectedModel = modelSelect.value;
        console.log('[runLiveChecks] selectedModel:', selectedModel, 'models:', models);

        // If no selection but we have models, select the first one
        if (!selectedModel && models.length > 0) {
            selectedModel = models[0];
            modelSelect.value = selectedModel;
            console.log('[runLiveChecks] auto-selected:', selectedModel);
        }
        window.selectedModel = selectedModel;

        // Refresh active tab output after compilation
        if (selectedModel && window.compiledModels[selectedModel]) {
            const result = window.compiledModels[selectedModel];
            // Refresh the active tab
            if (window.activeRightTab === 'dae') displayDaeOutput(selectedModel);
            else if (window.activeRightTab === 'codegen') displayCodegenOutput(selectedModel);

            if (result.error) {
                statusSpan.textContent = `Error (${elapsed}ms)`;
                statusSpan.style.color = '#c9184a';
                window.switchBottomTab('errors');
            } else {
                const b = result.balance;
                if (b) {
                    const balanceStatus = b.is_balanced ? 'BALANCED' :
                        (typeof b.status === 'string' ? b.status.toUpperCase() :
                        (b.status.CompileError ? 'ERROR' : 'UNBALANCED'));
                    const modelCountStr = models.length > 1 ? ` [${successCount}/${models.length}]` : '';
                    statusSpan.textContent = b.is_balanced
                        ? `Balanced (${elapsed}ms)${modelCountStr}`
                        : `${balanceStatus} (${elapsed}ms)${modelCountStr}`;
                    statusSpan.style.color = b.is_balanced ? '#2d6a4f' : '#c9184a';
                }
            }
        } else if (selectedModel && models.includes(selectedModel)) {
            setDaeOutput(`Waiting for compilation of ${selectedModel}...`);
            statusSpan.textContent = `${elapsed}ms`;
            statusSpan.style.color = '#888';
        } else {
            setDaeOutput(models.length === 0 ? 'No models found in source' : 'Select a model');
            statusSpan.textContent = models.length === 0 ? 'No models' : `${elapsed}ms`;
            statusSpan.style.color = '#888';
        }

        // Refresh CodeLens to show balance for all models
        if (window.refreshCodeLens) window.refreshCodeLens();
        if (document.getElementById('packagesSection').classList.contains('active')) {
            libraryDocsController.refreshPackageViewer(true);
        }
        } catch (unexpectedError) {
            if (isStaleRun()) return;
            // Catch any unexpected errors to ensure status is always updated
            console.error('[runLiveChecks] Unexpected error:', unexpectedError);
            statusSpan.textContent = 'Error';
            statusSpan.style.color = '#c9184a';
            updateCompileErrors([{ model: 'live-check', message: String(unexpectedError?.message || unexpectedError) }]);
        } finally {
            updateGlobalStatusBar();
        }
    }, 500);
    window.triggerCompileNow = () => {
        runLiveChecks();
    };

    editor.onDidChangeModelContent(() => {
        if (window.refreshModelicaSemanticTokens) window.refreshModelicaSemanticTokens();
        updateSourceBreadcrumbs();
        runLiveChecks();
    });
    editor.onDidChangeCursorPosition(() => {
        updateSourceBreadcrumbs();
    });

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

    setTimeout(() => {
        updateSourceBreadcrumbs();
        runLiveChecks();
    }, 1000);

    // Template editor change listener - re-render when template changes (if codegen tab is active)
    let templateDebounceTimer = null;
    templateEditor.onDidChangeModelContent(() => {
        if (window.activeRightTab !== 'codegen') return;
        if (templateDebounceTimer) clearTimeout(templateDebounceTimer);
        templateDebounceTimer = setTimeout(() => {
            const modelName = document.getElementById('modelSelect').value;
            if (modelName && window.compiledModels[modelName]) {
                displayCodegenOutput(modelName);
            }
        }, 300);
    });
});
