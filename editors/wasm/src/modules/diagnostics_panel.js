function escapeHtml(value) {
    return String(value ?? '')
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#39;');
}

function compactDiagnosticMessage(message, maxLen = 220) {
    const compact = String(message ?? '').replace(/\s+/g, ' ').trim();
    if (compact.length <= maxLen) return compact;
    return `${compact.slice(0, maxLen - 3)}...`;
}

function hasPreciseDiagnosticRange(diagnostic) {
    return diagnostic?.data?.precise_range !== false;
}

function escapeRegex(value) {
    return String(value ?? '').replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function offsetToEditorPosition(text, offset) {
    const clamped = Math.max(0, Math.min(offset, text.length));
    let line = 1;
    let col = 1;
    for (let i = 0; i < clamped; i++) {
        if (text[i] === '\n') {
            line++;
            col = 1;
        } else {
            col++;
        }
    }
    return { line, col };
}

function extractQuotedIdentifier(message) {
    const single = String(message ?? '').match(/'([^']+)'/);
    if (single && single[1]) return single[1];
    const backtick = String(message ?? '').match(/`([^`]+)`/);
    if (backtick && backtick[1]) return backtick[1];
    return null;
}

function extractLineColCandidates(message) {
    const text = String(message ?? '');
    const regex = /:(\d+):(\d+)(?:-\d+)?(?:\[\d+\])?/g;
    let match;
    const candidates = [];
    while ((match = regex.exec(text)) !== null) {
        const line = Number(match[1]);
        const col = Number(match[2]);
        if (Number.isFinite(line) && Number.isFinite(col) && line > 0 && col > 0) {
            candidates.push({ line, col, index: match.index });
        }
    }
    return candidates;
}

function lineTextAt(sourceText, lineNumber) {
    if (!sourceText || lineNumber <= 0) return '';
    const lines = String(sourceText).split('\n');
    if (lineNumber > lines.length) return '';
    return lines[lineNumber - 1] || '';
}

function extractBestLineColFromMessage(message, sourceText) {
    const candidates = extractLineColCandidates(message);
    if (candidates.length === 0) return null;

    const token = extractQuotedIdentifier(message);
    if (token && sourceText) {
        for (const candidate of candidates) {
            const lineText = lineTextAt(sourceText, candidate.line);
            if (lineText.includes(token)) {
                const clampedCol = Math.min(candidate.col, lineText.length + 1);
                return { line: candidate.line, col: clampedCol };
            }
        }
    }

    for (const candidate of candidates) {
        if (candidate.line > 1 || candidate.col > 1) {
            return { line: candidate.line, col: candidate.col };
        }
    }

    const first = candidates[0];
    return { line: first.line, col: first.col };
}

function stripLegacyDiagnosticContext(message) {
    const compact = String(message ?? '').replace(/\s+/g, ' ').trim();
    const pipeIdx = compact.indexOf(' | ');
    return pipeIdx >= 0 ? compact.slice(0, pipeIdx).trim() : compact;
}

function hasQuotedKeyword(lowerMessage, keyword) {
    return lowerMessage.includes(`\`${keyword}\``) || lowerMessage.includes(`'${keyword}'`);
}

function normalizeLegacyDiagnosticMessage(message) {
    const compact = stripLegacyDiagnosticContext(message);
    const lowered = compact.toLowerCase();
    if (lowered.includes('is a reserved keyword in modelica')) {
        if (hasQuotedKeyword(lowered, 'equation')) {
            return 'unexpected `equation` (possible missing `;` before equation section)';
        }
        if (hasQuotedKeyword(lowered, 'algorithm')) {
            return 'unexpected `algorithm` (possible missing `;` before algorithm section)';
        }
        if (hasQuotedKeyword(lowered, 'end')) {
            return 'unexpected `end` (possible missing `;` before end statement)';
        }
        if (hasQuotedKeyword(lowered, 'der')) {
            return 'unexpected `der` (possible missing `;` between equations)';
        }
    }
    return compact;
}

function extractRumocaCode(message) {
    const m = String(message ?? '').match(/\b(E[A-Z]\d{3})\b/);
    return m ? m[1] : '';
}

function isMietteFrameLine(line) {
    const trimmed = String(line || '').trim();
    if (!trimmed) return true;
    return /^help:/i.test(trimmed)
        || /^caused by:/i.test(trimmed)
        || /^stack backtrace:/i.test(trimmed)
        || /^\[.*:\d+:\d+\]$/.test(trimmed)
        || /^[\d\s]*[│|].*$/.test(trimmed)
        || /^[╭╰│┆┬└┌─├┤┼]+.*$/.test(trimmed)
        || /^at\s+/.test(trimmed)
        || /^unexpected end of input/i.test(trimmed);
}

function summarizeCompileError(message) {
    const raw = String(message || '');
    const lines = raw
        .split('\n')
        .map((line) => line.trim())
        .filter(Boolean);
    const code = extractRumocaCode(raw);
    for (const line of lines) {
        const withoutMarker = line.replace(/^[×x]\s+/, '').trim();
        if (!withoutMarker || withoutMarker === code || isMietteFrameLine(withoutMarker)) {
            continue;
        }
        if (/\bhelp: expected one of:/i.test(withoutMarker)) {
            continue;
        }
        return {
            code,
            message: normalizeLegacyDiagnosticMessage(withoutMarker),
        };
    }
    return {
        code,
        message: normalizeLegacyDiagnosticMessage(raw),
    };
}

export function createDiagnosticsController({
    monaco,
    getModelEditor,
    getModelPath,
    getTemplateEditor,
    switchToErrorsTab,
    triggerModelicaQuickFix,
}) {
    let currentTemplateError = null;
    let currentCompileErrors = [];
    let currentModelicaDiagnostics = [];
    let problemNavigationItems = [];
    let problemNavigationIndex = -1;

    function unresolvedIdentifierRange(diagnostic, model) {
        const message = String(diagnostic?.message ?? '');
        const isUnresolved = message.toLowerCase().includes('unresolved component reference')
            || message.toLowerCase().includes('unresolved function');
        if (!isUnresolved) return null;

        const ident = extractQuotedIdentifier(message);
        if (!ident) return null;

        const text = model.getValue();
        const pattern = new RegExp(`\\b${escapeRegex(ident)}\\b`, 'g');
        const matches = [];
        let match;
        while ((match = pattern.exec(text)) !== null) {
            matches.push(match.index);
        }
        if (matches.length === 0) return null;

        const equationIdx = text.indexOf('\nequation');
        const chosenOffset = (equationIdx >= 0
            ? matches.find(offset => offset >= equationIdx)
            : null) ?? matches[0];
        const start = offsetToEditorPosition(text, chosenOffset);
        const end = offsetToEditorPosition(text, chosenOffset + ident.length);
        return {
            startLineNumber: start.line,
            startColumn: start.col,
            endLineNumber: end.line,
            endColumn: Math.max(end.col, start.col + 1),
        };
    }

    function effectiveMarkerRange(diagnostic, model) {
        const startLine = diagnostic.range.start.line + 1;
        const startCol = diagnostic.range.start.character + 1;
        const endLine = diagnostic.range.end.line + 1;
        const endCol = diagnostic.range.end.character + 1;
        const unresolvedRange = unresolvedIdentifierRange(diagnostic, model);
        const suspiciousTopLeft = startLine === 1 && startCol === 1;

        if (unresolvedRange && (!hasPreciseDiagnosticRange(diagnostic) || suspiciousTopLeft)) {
            return unresolvedRange;
        }
        if (hasPreciseDiagnosticRange(diagnostic)) {
            return {
                startLineNumber: startLine,
                startColumn: startCol,
                endLineNumber: endLine,
                endColumn: Math.max(endCol, startCol + 1),
            };
        }
        return unresolvedRange || {
            startLineNumber: startLine,
            startColumn: startCol,
            endLineNumber: endLine,
            endColumn: Math.max(endCol, startCol + 1),
        };
    }

    function diagnosticsToMarkers(diagnostics, model) {
        return diagnostics.map(diagnostic => {
            const markerRange = effectiveMarkerRange(diagnostic, model);
            let endCol = markerRange.endColumn;
            if (markerRange.startLineNumber === markerRange.endLineNumber
                && markerRange.startColumn >= markerRange.endColumn) {
                const lineContent = model.getLineContent(markerRange.startLineNumber);
                let wordEnd = markerRange.startColumn;
                while (wordEnd <= lineContent.length && /\w/.test(lineContent[wordEnd - 1])) wordEnd++;
                if (wordEnd === markerRange.startColumn) {
                    endCol = Math.min(markerRange.startColumn + 10, lineContent.length + 1);
                } else {
                    endCol = wordEnd;
                }
            }
            return {
                startLineNumber: markerRange.startLineNumber,
                startColumn: markerRange.startColumn,
                endLineNumber: markerRange.endLineNumber,
                endColumn: Math.max(endCol, markerRange.startColumn + 1),
                message: diagnostic.message,
                severity: [
                    0,
                    monaco.MarkerSeverity.Error,
                    monaco.MarkerSeverity.Warning,
                    monaco.MarkerSeverity.Info,
                    monaco.MarkerSeverity.Hint,
                ][diagnostic.severity] || monaco.MarkerSeverity.Error,
                source: 'rumoca',
            };
        });
    }

    function diagnosticCodeString(diagnostic) {
        const code = diagnostic?.code;
        if (typeof code === 'string') return code;
        if (typeof code === 'number') return String(code);
        if (code && typeof code === 'object') {
            if (typeof code.String === 'string') return code.String;
            if (typeof code.Number === 'number') return String(code.Number);
        }
        return '';
    }

    function normalizeLegacyLspDiagnostic(diagnostic, sourceText) {
        const normalized = { ...diagnostic };
        const originalCode = diagnosticCodeString(diagnostic);
        const extractedCode = extractRumocaCode(diagnostic.message);
        if (originalCode === 'syntax-error' && extractedCode) {
            normalized.code = extractedCode;
        }

        normalized.message = normalizeLegacyDiagnosticMessage(diagnostic.message);

        const startLine = Number(diagnostic?.range?.start?.line ?? 0) + 1;
        const startCol = Number(diagnostic?.range?.start?.character ?? 0) + 1;
        const lineCol = extractBestLineColFromMessage(diagnostic.message, sourceText);
        const suspiciousTopLeft = startLine === 1 && startCol === 1;
        if ((originalCode === 'syntax-error' || suspiciousTopLeft) && lineCol) {
            normalized.range = {
                start: { line: lineCol.line - 1, character: lineCol.col - 1 },
                end: { line: lineCol.line - 1, character: lineCol.col },
            };
            normalized.data = {
                ...(diagnostic?.data && typeof diagnostic.data === 'object' ? diagnostic.data : {}),
                precise_range: true,
            };
        }

        return normalized;
    }

    function normalizeDiagnosticEntry(diagnostic, sourceText) {
        if (!diagnostic || typeof diagnostic !== 'object') return null;

        if (diagnostic.range && diagnostic.range.start && diagnostic.range.end && diagnostic.message !== undefined) {
            return normalizeLegacyLspDiagnostic(diagnostic, sourceText);
        }

        const hasLegacyShape =
            diagnostic.rule !== undefined
            && diagnostic.level !== undefined
            && diagnostic.message !== undefined;
        if (!hasLegacyShape) return null;

        const lineCol = extractBestLineColFromMessage(diagnostic.message, sourceText);
        const baseLine = Number.isFinite(Number(diagnostic.line)) ? Number(diagnostic.line) : 1;
        const baseCol = Number.isFinite(Number(diagnostic.column)) ? Number(diagnostic.column) : 1;
        const line = lineCol?.line || Math.max(1, baseLine);
        const col = lineCol?.col || Math.max(1, baseCol);
        const codeFromMessage = extractRumocaCode(diagnostic.message);
        const normalizedMessage = normalizeLegacyDiagnosticMessage(diagnostic.message);
        const level = String(diagnostic.level || '').toLowerCase();
        const severity = level === 'error'
            ? 1
            : (level === 'warning' ? 2 : (level === 'note' ? 3 : 4));

        return {
            range: {
                start: { line: line - 1, character: col - 1 },
                end: { line: line - 1, character: col },
            },
            severity,
            code: codeFromMessage || String(diagnostic.rule || ''),
            source: 'rumoca',
            message: normalizedMessage,
            data: { precise_range: !!lineCol || line > 1 || col > 1 },
            _legacy_payload: true,
            _source_len: sourceText ? sourceText.length : 0,
        };
    }

    function normalizeDiagnosticsPayload(payload, sourceText) {
        if (!Array.isArray(payload)) return [];
        return payload
            .map(diagnostic => normalizeDiagnosticEntry(diagnostic, sourceText))
            .filter(Boolean);
    }

    function jumpToEditorLocation(lineNumber, column, targetEditor = 'modelica') {
        const modelEditor = getModelEditor();
        const templateEditor = getTemplateEditor();
        const editorRef = targetEditor === 'template' ? templateEditor : modelEditor;
        if (!editorRef || !editorRef.getModel) return;

        const model = editorRef.getModel();
        if (!model) return;

        const maxLine = model.getLineCount();
        const safeLine = Math.max(1, Math.min(Number(lineNumber) || 1, maxLine));
        const maxCol = model.getLineMaxColumn(safeLine);
        const safeCol = Math.max(1, Math.min(Number(column) || 1, maxCol));
        const position = { lineNumber: safeLine, column: safeCol };

        editorRef.focus();
        editorRef.setPosition(position);
        editorRef.revealPositionInCenter(position);
    }

    function bindDiagnosticLinks(container) {
        container.querySelectorAll('.diagnostic-link[data-line][data-col][data-target]').forEach(link => {
            link.addEventListener('click', event => {
                event.preventDefault();
                const line = Number(link.dataset.line);
                const col = Number(link.dataset.col);
                const target = String(link.dataset.target || 'modelica');
                if (!Number.isFinite(line) || !Number.isFinite(col)) return;
                jumpToEditorLocation(line, col, target);
            });
        });
    }

    function bindQuickFixButtons(container) {
        container.querySelectorAll('.diagnostic-quick-fix[data-line][data-col][data-target]').forEach(button => {
            button.addEventListener('click', async event => {
                event.preventDefault();
                const line = Number(button.dataset.line);
                const col = Number(button.dataset.col);
                const target = String(button.dataset.target || 'modelica');
                if (!Number.isFinite(line) || !Number.isFinite(col)) return;
                jumpToEditorLocation(line, col, target);
                if (target !== 'modelica' || typeof triggerModelicaQuickFix !== 'function') return;

                button.disabled = true;
                button.classList.add('running');
                try {
                    await triggerModelicaQuickFix(line, col);
                } finally {
                    button.classList.remove('running');
                    button.disabled = false;
                }
            });
        });
    }

    function renderDiagnosticGroup(title, itemsHtml) {
        if (!Array.isArray(itemsHtml) || itemsHtml.length === 0) return '';
        return `<div class="diagnostic-group">
            <div class="diagnostic-group-header">${escapeHtml(title)}</div>
            ${itemsHtml.join('')}
        </div>`;
    }

    function baseNameFromPath(path, fallback) {
        const text = String(path || '').trim();
        if (!text) {
            return fallback;
        }
        const parts = text.split('/').filter(Boolean);
        return parts.at(-1) || fallback;
    }

    function diagnosticLocationButton(lineNumber, column, target = 'modelica', fileLabel = '') {
        const targetLabel = target === 'template' ? 'template' : 'source';
        const prefix = String(fileLabel || '').trim();
        const visibleLabel = prefix ? `${prefix}:${lineNumber}:${column}` : `${lineNumber}:${column}`;
        return `<button class="diagnostic-link" data-line="${lineNumber}" data-col="${column}" data-target="${target}" title="Jump to ${targetLabel} ${visibleLabel}">${escapeHtml(visibleLabel)}</button>`;
    }

    function diagnosticQuickFixButton(lineNumber, column, target = 'modelica') {
        return '';
    }

    function renderDiagnosticItem({ severity, locationLabel, code, message, contextLabel, quickFixButton }) {
        const normalizedSeverity = severity || 'info';
        const shortMessageRaw = compactDiagnosticMessage(message || '', 140);
        const shortMessage = escapeHtml(shortMessageRaw);
        const fullMessageRaw = String(message || '');
        const codeLabel = code ? `<span class="diagnostic-code">[${escapeHtml(code)}]</span>` : '';
        const contextPrefix = contextLabel
            ? `<span class="diagnostic-context">${escapeHtml(contextLabel)}</span>`
            : '';
        const actionsHtml = `
            <span class="diagnostic-actions">
                ${locationLabel || '<span class="diagnostic-context">Model</span>'}
                ${quickFixButton || ''}
            </span>
        `;
        const title = escapeHtml(fullMessageRaw || shortMessageRaw);
        return `<div class="diagnostic-item diagnostic-${normalizedSeverity}" title="${title}">
            ${actionsHtml}
            <span class="diagnostic-item-main">
                ${contextPrefix}
                ${codeLabel}
                <span class="diagnostic-message">${shortMessage}</span>
            </span>
        </div>`;
    }

    function updateProblemStatus() {
        const status = document.getElementById('problemStatus');
        if (!status) return;

        const total = problemNavigationItems.length;
        if (total === 0) {
            status.textContent = 'Problems: 0';
            status.style.color = '#888';
            return;
        }

        const active = Math.max(0, Math.min(problemNavigationIndex, total - 1));
        status.textContent = `Problems: ${total} (${active + 1}/${total})`;
        status.style.color = '#d7ba7d';
    }

    function setProblemNavigationItems(items) {
        problemNavigationItems = Array.isArray(items) ? items : [];
        if (problemNavigationItems.length === 0) {
            problemNavigationIndex = -1;
        } else if (problemNavigationIndex < 0 || problemNavigationIndex >= problemNavigationItems.length) {
            problemNavigationIndex = 0;
        }
        updateProblemStatus();
    }

    function navigateProblems(step) {
        if (!Array.isArray(problemNavigationItems) || problemNavigationItems.length === 0) {
            updateProblemStatus();
            return;
        }
        const total = problemNavigationItems.length;
        if (problemNavigationIndex < 0 || problemNavigationIndex >= total) {
            problemNavigationIndex = 0;
        } else {
            problemNavigationIndex = (problemNavigationIndex + step + total) % total;
        }
        const item = problemNavigationItems[problemNavigationIndex];
        if (item) {
            jumpToEditorLocation(item.line, item.col, item.target || 'modelica');
        }
        updateProblemStatus();
    }

    function renderAllDiagnostics() {
        const diagnosticsDiv = document.getElementById('diagnosticsPanel');
        const diagnosticsCount = document.getElementById('diagnosticsCount');
        if (!diagnosticsDiv || !diagnosticsCount) return;

        const modelEditor = getModelEditor();
        const model = modelEditor && modelEditor.getModel ? modelEditor.getModel() : null;
        const sourceText = model ? model.getValue() : '';
        const modelFileLabel = baseNameFromPath(getModelPath?.(), 'Model.mo');

        const templateEditor = getTemplateEditor();
        const templateText = templateEditor && templateEditor.getValue
            ? templateEditor.getValue()
            : '';

        const modelicaErrors = currentModelicaDiagnostics.filter(diagnostic => diagnostic.severity === 1).length;
        const modelicaWarnings = currentModelicaDiagnostics.filter(diagnostic => diagnostic.severity === 2).length;
        const templateErrors = currentTemplateError ? 1 : 0;
        const compileErrors = currentCompileErrors.length;

        const totalErrors = modelicaErrors + templateErrors + compileErrors;
        const totalWarnings = modelicaWarnings;

        if (totalErrors > 0) {
            diagnosticsCount.textContent = `(${totalErrors} error${totalErrors > 1 ? 's' : ''})`;
            diagnosticsCount.style.color = '#f44336';
        } else if (totalWarnings > 0) {
            diagnosticsCount.textContent = `(${totalWarnings} warning${totalWarnings > 1 ? 's' : ''})`;
            diagnosticsCount.style.color = '#ffc107';
        } else {
            diagnosticsCount.textContent = '';
        }

        const groups = [];
        const navItems = [];

        if (currentTemplateError) {
            const templateLineCol = extractBestLineColFromMessage(currentTemplateError, templateText);
            const templateLocation = templateLineCol
                ? diagnosticLocationButton(templateLineCol.line, templateLineCol.col, 'template', 'Template')
                : 'Template';

            if (templateLineCol) {
                navItems.push({
                    target: 'template',
                    line: templateLineCol.line,
                    col: templateLineCol.col,
                    severity: 1,
                    message: currentTemplateError,
                });
            }

            groups.push(renderDiagnosticGroup('Template (1)', [
                renderDiagnosticItem({
                    severity: 'error',
                    locationLabel: templateLocation,
                    code: 'template',
                    message: currentTemplateError,
                    contextLabel: null,
                    quickFixButton: '',
                }),
            ]));
        }

        if (currentCompileErrors.length > 0) {
            const compileItems = currentCompileErrors.map(err => {
                const summary = summarizeCompileError(err?.message || '');
                const lineCol = extractBestLineColFromMessage(err?.message || '', sourceText);
                const locationLabel = lineCol
                    ? diagnosticLocationButton(lineCol.line, lineCol.col, 'modelica', modelFileLabel)
                    : 'Model';
                if (lineCol) {
                    navItems.push({
                        target: 'modelica',
                        line: lineCol.line,
                        col: lineCol.col,
                        severity: 1,
                        message: err?.message || 'compile error',
                    });
                }
                return renderDiagnosticItem({
                    severity: 'error',
                    locationLabel,
                    code: summary.code,
                    message: summary.message || 'compile error',
                    contextLabel: null,
                    quickFixButton: '',
                });
            });
            groups.push(renderDiagnosticGroup(`Compile (${currentCompileErrors.length})`, compileItems));
        }

        if (currentModelicaDiagnostics.length > 0 && model) {
            const modelicaItems = currentModelicaDiagnostics.map(diagnostic => {
                const severity = ['', 'error', 'warning', 'info', 'hint'][diagnostic.severity] || 'info';
                const markerRange = effectiveMarkerRange(diagnostic, model);
                const hasPreciseRange = hasPreciseDiagnosticRange(diagnostic) || !!unresolvedIdentifierRange(diagnostic, model);
                const locationLabel = hasPreciseRange
                    ? diagnosticLocationButton(markerRange.startLineNumber, markerRange.startColumn, 'modelica', modelFileLabel)
                    : 'Model';
                const quickFixButton = hasPreciseRange
                    ? diagnosticQuickFixButton(markerRange.startLineNumber, markerRange.startColumn, 'modelica')
                    : '';
                if (hasPreciseRange) {
                    navItems.push({
                        target: 'modelica',
                        line: markerRange.startLineNumber,
                        col: markerRange.startColumn,
                        severity: diagnostic.severity,
                        message: diagnostic.message || '',
                    });
                }
                return renderDiagnosticItem({
                    severity,
                    locationLabel,
                    code: diagnosticCodeString(diagnostic),
                    message: diagnostic.message || '',
                    contextLabel: null,
                    quickFixButton,
                });
            });
            groups.push(renderDiagnosticGroup(`Modelica (${currentModelicaDiagnostics.length})`, modelicaItems));
        }

        diagnosticsDiv.innerHTML = groups.length > 0
            ? groups.join('')
            : '<div class="diagnostic-item diagnostic-info">No issues found!</div>';

        bindDiagnosticLinks(diagnosticsDiv);
        bindQuickFixButtons(diagnosticsDiv);
        setProblemNavigationItems(navItems);

        if (model) {
            if (currentModelicaDiagnostics.length === 0) {
                monaco.editor.setModelMarkers(model, 'rumoca', []);
            } else {
                const markers = diagnosticsToMarkers(currentModelicaDiagnostics, model);
                monaco.editor.setModelMarkers(model, 'rumoca', markers);
            }
        }
    }

    function updateCompileErrors(errors) {
        currentCompileErrors = Array.isArray(errors) ? errors : [];
        renderAllDiagnostics();
    }

    function updateModelicaDiagnostics(diagnostics) {
        currentModelicaDiagnostics = Array.isArray(diagnostics) ? diagnostics : [];
        renderAllDiagnostics();
    }

    function showTemplateError(message) {
        currentTemplateError = String(message || '');
        renderAllDiagnostics();
        if (typeof switchToErrorsTab === 'function') {
            switchToErrorsTab();
        }
    }

    function clearTemplateErrors() {
        if (!currentTemplateError) return;
        currentTemplateError = null;
        renderAllDiagnostics();
    }

    return {
        clearTemplateErrors,
        diagnosticCodeString,
        navigateProblems,
        normalizeDiagnosticsPayload,
        renderAllDiagnostics,
        showTemplateError,
        updateCompileErrors,
        updateModelicaDiagnostics,
    };
}
