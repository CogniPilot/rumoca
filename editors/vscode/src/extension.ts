import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';
import { createRequire } from 'module';
import * as vscode from 'vscode';
import { execSync, spawn, spawnSync } from 'child_process';
import {
    LanguageClient,
    LanguageClientOptions,
    TransportKind
} from 'vscode-languageclient/node';
import {
    SourceRootPathSources,
    resolveSourceRootPaths as resolveSourceRootPathsForEntries,
} from './modelica_paths';
import {
    resolvePreferredViewerScriptPath,
} from './results_paths';
import {
    StartedLanguageClient,
    createLanguageClientRuntime,
} from './language_client_runtime';
import { createNotebookControllerRuntime } from './notebook_controller_runtime';
import { buildNotebookPythonSnippet } from './notebook_python_snippets';

let client: LanguageClient | undefined;
let outputChannel: vscode.OutputChannel;
const nodeRequire = createRequire(__filename);
const pendingSimulationJobs = new Map<string, {
    resolve: (response: {
        ok?: boolean;
        payload?: unknown;
        error?: string;
        metrics?: unknown;
    }) => void;
}>();
const pendingPrepareSimulationJobs = new Map<string, {
    resolve: (response: {
        ok?: boolean;
        preparedModels?: string[];
        failures?: Array<{ model?: string; error?: string }>;
        error?: string;
    }) => void;
}>();

// ============================================================================
// Virtual Document Provider for %%modelica blocks in Python cells
// This enables LSP features (hover, completion, diagnostics) in magic blocks
// ============================================================================

interface ModelicaBlock {
    startLine: number;      // Line in the Python cell where block starts
    endLine: number;        // Last line of the Modelica code
    content: string;        // The Modelica code
    cellUri: string;        // URI of the notebook cell
    type: 'magic' | 'compile_source';  // Type of block for position mapping
}

// Track Modelica blocks in Python cells: cellUri -> ModelicaBlock[]
const modelicaBlocks = new Map<string, ModelicaBlock[]>();

// Track which virtual documents are already open in the LSP
const openVirtualDocuments = new Map<string, { version: number; content: string }>();

// Debounce timers for document updates
const updateDebounceTimers = new Map<string, NodeJS.Timeout>();
const DEBOUNCE_DELAY_MS = 150;

// Virtual document scheme for embedded Modelica
const EMBEDDED_MODELICA_SCHEME = 'embedded-modelica';

/**
 * Parse a Python cell to find %%modelica blocks and compile_source() calls
 */
function findModelicaBlocks(document: vscode.TextDocument): ModelicaBlock[] {
    const blocks: ModelicaBlock[] = [];
    const text = document.getText();
    const lines = text.split('\n');

    // Pattern 1: %%modelica_rumoca cell magic
    let inBlock = false;
    let blockStartLine = 0;
    let blockLines: string[] = [];

    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        const trimmed = line.trim();

        if (trimmed.startsWith('%%modelica_rumoca')) {
            // Start of a new block
            inBlock = true;
            blockStartLine = i;
            blockLines = [];
        } else if (inBlock) {
            // Check if this line ends the block (empty line or new magic/code)
            // In Jupyter, cell magics capture the entire rest of the cell
            blockLines.push(line);
        }
    }

    // If we were in a block, save it
    if (inBlock && blockLines.length > 0) {
        blocks.push({
            startLine: blockStartLine,
            endLine: blockStartLine + blockLines.length,
            content: blockLines.join('\n'),
            cellUri: document.uri.toString(),
            type: 'magic'
        });
    }

    // Pattern 2: compile_modelica() with triple quotes
    // Match both triple single and double quotes
    const compileSourcePatterns = [
        /compile_modelica\s*\(\s*'''/g,
        /compile_modelica\s*\(\s*"""/g
    ];

    for (const pattern of compileSourcePatterns) {
        const quoteType = pattern.source.includes("'''") ? "'''" : '"""';
        let match;
        while ((match = pattern.exec(text)) !== null) {
            const startOffset = match.index + match[0].length;
            const endOffset = text.indexOf(quoteType, startOffset);
            if (endOffset === -1) continue;

            // Find line numbers
            const beforeStart = text.substring(0, startOffset);
            const startLine = beforeStart.split('\n').length - 1;
            const content = text.substring(startOffset, endOffset);
            const endLine = startLine + content.split('\n').length - 1;

            blocks.push({
                startLine: startLine,  // Line where content starts (after opening quotes)
                endLine: endLine,
                content: content,
                cellUri: document.uri.toString(),
                type: 'compile_source'
            });
        }
    }

    return blocks;
}

/**
 * Convert a position in the Python cell to a position in the virtual Modelica document
 */
function cellToVirtualPosition(cellPos: vscode.Position, block: ModelicaBlock): vscode.Position | null {
    // For 'magic' blocks, skip the %%modelica line (hence -1)
    // For 'compile_source' blocks, content starts directly (no skip)
    const lineOffset = block.type === 'magic' ? 1 : 0;
    const virtualLine = cellPos.line - block.startLine - lineOffset;
    if (virtualLine < 0 || cellPos.line > block.endLine) {
        return null;
    }
    return new vscode.Position(virtualLine, cellPos.character);
}

/**
 * Get the virtual document URI for a Modelica block
 */
function getVirtualDocumentUri(cellUri: string, blockIndex: number): vscode.Uri {
    return vscode.Uri.parse(`${EMBEDDED_MODELICA_SCHEME}://${encodeURIComponent(cellUri)}/block${blockIndex}.mo`);
}

/**
 * Virtual document content provider for embedded Modelica
 */
class EmbeddedModelicaProvider implements vscode.TextDocumentContentProvider {
    private _onDidChange = new vscode.EventEmitter<vscode.Uri>();
    readonly onDidChange = this._onDidChange.event;

    provideTextDocumentContent(uri: vscode.Uri): string {
        // Parse the URI to get cell URI and block index
        const cellUri = decodeURIComponent(uri.authority);
        const blockMatch = uri.path.match(/block(\d+)\.mo/);
        if (!blockMatch) return '';

        const blockIndex = parseInt(blockMatch[1], 10);
        const blocks = modelicaBlocks.get(cellUri);
        if (!blocks || blockIndex >= blocks.length) return '';

        return blocks[blockIndex].content;
    }

    update(uri: vscode.Uri) {
        this._onDidChange.fire(uri);
    }
}

let embeddedModelicaProvider: EmbeddedModelicaProvider | undefined;

/**
 * Update Modelica blocks for a document and notify the LSP (internal, called after debounce)
 */
async function updateModelicaBlocksImmediate(document: vscode.TextDocument) {
    const blocks = findModelicaBlocks(document);
    const cellUri = document.uri.toString();

    if (blocks.length > 0) {
        modelicaBlocks.set(cellUri, blocks);

        // Update virtual documents and notify LSP
        if (embeddedModelicaProvider && client) {
            for (let index = 0; index < blocks.length; index++) {
                const block = blocks[index];
                const virtualUri = getVirtualDocumentUri(cellUri, index);
                const virtualUriStr = virtualUri.toString();
                embeddedModelicaProvider.update(virtualUri);

                const existing = openVirtualDocuments.get(virtualUriStr);

                if (!existing) {
                    // First time - send didOpen
                    try {
                        await client.sendNotification('textDocument/didOpen', {
                            textDocument: {
                                uri: virtualUriStr,
                                languageId: 'modelica',
                                version: 1,
                                text: block.content
                            }
                        });
                        openVirtualDocuments.set(virtualUriStr, { version: 1, content: block.content });
                    } catch {
                        // Ignore errors
                    }
                } else if (existing.content !== block.content) {
                    // Content changed - send didChange with incremented version
                    const newVersion = existing.version + 1;
                    try {
                        await client.sendNotification('textDocument/didChange', {
                            textDocument: {
                                uri: virtualUriStr,
                                version: newVersion
                            },
                            contentChanges: [{ text: block.content }]
                        });
                        openVirtualDocuments.set(virtualUriStr, { version: newVersion, content: block.content });
                    } catch {
                        // Ignore errors
                    }
                }
                // If content is the same, skip notification entirely
            }
        }
    } else {
        modelicaBlocks.delete(cellUri);
    }
}

/**
 * Update Modelica blocks for a document and notify the LSP (debounced)
 */
function updateModelicaBlocks(document: vscode.TextDocument) {
    // Only process Python files in notebooks
    if (document.languageId !== 'python') return;
    if (!document.uri.scheme.includes('notebook')) return;

    const cellUri = document.uri.toString();

    // Clear existing timer for this document
    const existingTimer = updateDebounceTimers.get(cellUri);
    if (existingTimer) {
        clearTimeout(existingTimer);
    }

    // Set new debounced timer
    const timer = setTimeout(() => {
        updateDebounceTimers.delete(cellUri);
        updateModelicaBlocksImmediate(document);
    }, DEBOUNCE_DELAY_MS);

    updateDebounceTimers.set(cellUri, timer);
}

/**
 * Find the Modelica block containing a position in a Python cell
 */
function findBlockAtPosition(cellUri: string, position: vscode.Position): { block: ModelicaBlock; index: number } | null {
    const blocks = modelicaBlocks.get(cellUri);
    if (!blocks) return null;

    for (let i = 0; i < blocks.length; i++) {
        const block = blocks[i];
        // For 'magic' blocks, content starts after the %%modelica line (line > startLine)
        // For 'compile_source' blocks, content starts at startLine (line >= startLine)
        const minLine = block.type === 'magic' ? block.startLine + 1 : block.startLine;
        if (position.line >= minLine && position.line <= block.endLine) {
            return { block, index: i };
        }
    }
    return null;
}

// Annotation collapsing feature
interface AnnotationInfo {
    startLine: number;
    endLine: number;
    contentRange: vscode.Range;  // The content inside annotation(...)
    isMultiLine: boolean;
}

// Track which single-line annotations are expanded (by document URI -> set of range keys)
const expandedSingleLineAnnotations = new Map<string, Set<string>>();

// Decoration types for single-line annotation collapsing
let hiddenContentDecorationType: vscode.TextEditorDecorationType | undefined;
let ellipsisDecorationType: vscode.TextEditorDecorationType | undefined;

function getRangeKey(range: vscode.Range): string {
    return `${range.start.line}:${range.start.character}-${range.end.line}:${range.end.character}`;
}

function findAllAnnotations(document: vscode.TextDocument): AnnotationInfo[] {
    const annotations: AnnotationInfo[] = [];
    const text = document.getText();

    // Match annotation(...) with balanced parentheses
    const annotationRegex = /\bannotation\s*\(/g;
    let match;

    while ((match = annotationRegex.exec(text)) !== null) {
        const startOffset = match.index;
        const openParenOffset = startOffset + match[0].length - 1;

        // Find matching closing parenthesis
        let depth = 1;
        let i = openParenOffset + 1;
        while (i < text.length && depth > 0) {
            if (text[i] === '(') depth++;
            else if (text[i] === ')') depth--;
            i++;
        }

        if (depth === 0) {
            const startLine = document.positionAt(startOffset).line;
            const endLine = document.positionAt(i).line;
            const contentStart = document.positionAt(openParenOffset + 1);
            const contentEnd = document.positionAt(i - 1);

            annotations.push({
                startLine,
                endLine,
                contentRange: new vscode.Range(contentStart, contentEnd),
                isMultiLine: endLine > startLine
            });
        }
    }

    return annotations;
}

function updateSingleLineDecorations(editor: vscode.TextEditor, enabled: boolean) {
    if (!enabled || !hiddenContentDecorationType || !ellipsisDecorationType) {
        if (hiddenContentDecorationType) {
            editor.setDecorations(hiddenContentDecorationType, []);
        }
        if (ellipsisDecorationType) {
            editor.setDecorations(ellipsisDecorationType, []);
        }
        return;
    }

    const document = editor.document;
    if (document.languageId !== 'modelica') return;

    const docKey = document.uri.toString();
    const expanded = expandedSingleLineAnnotations.get(docKey) || new Set<string>();
    const annotations = findAllAnnotations(document);

    const hiddenDecorations: vscode.DecorationOptions[] = [];
    const ellipsisDecorations: vscode.DecorationOptions[] = [];

    for (const annotation of annotations) {
        // Only apply decorations to single-line annotations
        if (!annotation.isMultiLine) {
            const rangeKey = getRangeKey(annotation.contentRange);
            if (!expanded.has(rangeKey)) {
                // Hide the content
                hiddenDecorations.push({ range: annotation.contentRange });
                // Show "..." at the start of the hidden content
                ellipsisDecorations.push({
                    range: new vscode.Range(annotation.contentRange.start, annotation.contentRange.start)
                });
            }
        }
    }

    editor.setDecorations(hiddenContentDecorationType, hiddenDecorations);
    editor.setDecorations(ellipsisDecorationType, ellipsisDecorations);
}

async function foldAllAnnotations(editor: vscode.TextEditor, collapseEnabled: boolean) {
    const annotations = findAllAnnotations(editor.document);
    const multiLineAnnotations = annotations.filter(a => a.isMultiLine);

    if (multiLineAnnotations.length > 0) {
        const originalSelections = editor.selections;
        const foldSelections = multiLineAnnotations.map(a =>
            new vscode.Selection(a.startLine, 0, a.startLine, 0)
        );
        editor.selections = foldSelections;
        await vscode.commands.executeCommand('editor.fold');
        editor.selections = originalSelections;
    }

    // Collapse all single-line annotations (clear expanded set)
    const docKey = editor.document.uri.toString();
    expandedSingleLineAnnotations.set(docKey, new Set<string>());
    updateSingleLineDecorations(editor, collapseEnabled);
}

async function unfoldAllAnnotations(editor: vscode.TextEditor, collapseEnabled: boolean) {
    const annotations = findAllAnnotations(editor.document);
    const multiLineAnnotations = annotations.filter(a => a.isMultiLine);

    if (multiLineAnnotations.length > 0) {
        const originalSelections = editor.selections;
        const unfoldSelections = multiLineAnnotations.map(a =>
            new vscode.Selection(a.startLine, 0, a.startLine, 0)
        );
        editor.selections = unfoldSelections;
        await vscode.commands.executeCommand('editor.unfold');
        editor.selections = originalSelections;
    }

    // Expand all single-line annotations
    const docKey = editor.document.uri.toString();
    const singleLineAnnotations = annotations.filter(a => !a.isMultiLine);
    const expanded = new Set<string>();
    for (const ann of singleLineAnnotations) {
        expanded.add(getRangeKey(ann.contentRange));
    }
    expandedSingleLineAnnotations.set(docKey, expanded);
    updateSingleLineDecorations(editor, collapseEnabled);
}

async function toggleAnnotationAtCursor(editor: vscode.TextEditor, collapseEnabled: boolean) {
    const position = editor.selection.active;
    const annotations = findAllAnnotations(editor.document);
    const docKey = editor.document.uri.toString();

    for (const annotation of annotations) {
        if (position.line >= annotation.startLine && position.line <= annotation.endLine) {
            if (annotation.isMultiLine) {
                // Use VSCode's native fold toggle for multi-line
                await vscode.commands.executeCommand('editor.toggleFold');
            } else {
                // Toggle single-line annotation expansion via decorations
                if (!expandedSingleLineAnnotations.has(docKey)) {
                    expandedSingleLineAnnotations.set(docKey, new Set<string>());
                }
                const expanded = expandedSingleLineAnnotations.get(docKey)!;
                const rangeKey = getRangeKey(annotation.contentRange);
                if (expanded.has(rangeKey)) {
                    expanded.delete(rangeKey);
                } else {
                    expanded.add(rangeKey);
                }
                updateSingleLineDecorations(editor, collapseEnabled);
            }
            return;
        }
    }
}

function findInPath(command: string): string | undefined {
    try {
        const result = execSync(process.platform === 'win32' ? `where ${command}` : `which ${command}`, {
            encoding: 'utf-8',
            timeout: 5000
        }).trim();
        // 'which' returns the path, 'where' may return multiple lines
        const firstLine = result.split('\n')[0].trim();
        if (firstLine && fs.existsSync(firstLine)) {
            return firstLine;
        }
    } catch {
        // Command not found in PATH
    }
    return undefined;
}

interface ServerProbeResult {
    ok: boolean;
    detail?: string;
}

function probeServerExecutable(serverPath: string): ServerProbeResult {
    const result = spawnSync(serverPath, ['--version'], {
        encoding: 'utf-8',
        timeout: 5000,
        windowsHide: true
    });
    if (result.error) {
        return { ok: false, detail: result.error.message };
    }

    if (result.status !== 0) {
        const detail = [result.stderr, result.stdout]
            .map(value => value?.trim())
            .find(value => Boolean(value));
        return {
            ok: false,
            detail: detail || `process exited with status ${result.status ?? 'unknown'}`
        };
    }

    return { ok: true };
}

interface SimulationExecutionSettings {
    tEnd: number;
    dt?: number;
    solver: 'auto' | 'bdf' | 'rk-like';
    outputDir: string;
    sourceRootPaths: string[];
}

interface SimulationSettings extends SimulationExecutionSettings {
    model: string;
}

interface ModelSimulationPreset {
    tEnd: number;
    dt?: number;
    solver: 'auto' | 'bdf' | 'rk-like';
    outputDir: string;
    sourceRootOverrides: string[];
}

interface CompilePhaseSeconds {
    instantiate: number;
    typecheck: number;
    flatten: number;
    todae: number;
}

interface SimulationRunMetrics {
    compileSeconds: number;
    simulateSeconds: number;
    points: number;
    variables: number;
    compilePhaseSeconds?: CompilePhaseSeconds;
}

interface SimulationRunResult {
    exitCode: number;
    stderr: string;
    payload?: ParsedSimulationPayload;
    metrics?: SimulationRunMetrics;
}

interface PersistedSimulationRun {
    runId: string;
    model: string;
    savedAtUnixMs?: number;
    payload?: ParsedSimulationPayload;
    metrics?: SimulationRunMetrics;
    views: VisualizationView[];
}

interface ResultsPanelState {
    version: 1;
    runId: string;
    model: string;
    workspaceRoot?: string;
    title?: string;
    activeViewId?: string;
}

interface ResultsPanelModelRef {
    model: string;
    workspaceRoot?: string;
    runId?: string;
    title?: string;
}

interface ResultsWebviewAssets {
    uplotCss: string;
    uplotJs: string;
    threeJs: string;
    visualizationSharedJs: string;
    resultsAppJs: string;
    resultsAppCss: string;
}

interface VisualizationSharedModule {
    buildHostedSimulationSettingsDocument(args: {
        activeModel: string;
        availableModels: string[];
        current: {
            solver: string;
            tEnd: number;
            dt?: number | null;
            outputDir?: string;
            sourceRootOverrides: string[];
        };
        views: unknown;
    }): string;
    buildHostedSimulationSettingsHandlers(args: unknown): Record<string, (args: {
        method: string;
        payload: unknown;
    }) => Promise<unknown> | unknown>;
    buildHostedSimulationSettingsState(args: unknown): {
        activeModel: string;
        availableModels: string[];
        current: {
            solver: string;
            tEnd: number;
            dt: number | null;
            outputDir: string;
            sourceRootOverrides: string[];
        };
        views: VisualizationView[];
    };
    buildVisualizationViewStorageHandlers(args: {
        resolveViewerScriptPath?: (model: string, viewId: string) => Promise<string> | string;
        readTextFile?: (scriptPath: string) => Promise<string> | string;
        writeTextFile?: (scriptPath: string, content: string) => Promise<void> | void;
        removeTextFile?: (scriptPath: string) => Promise<void> | void;
        defaultViewerScript?: () => string;
    }): {
        hydrateViews(args: {
            model: string;
            views: unknown;
        }): Promise<VisualizationView[]>;
        persistViews(args: {
            model: string;
            views: unknown;
        }): Promise<VisualizationView[]>;
        removeViews(args: {
            views: unknown;
        }): Promise<void>;
        removeStaleViews(args: {
            previousViews: unknown;
            nextViews: unknown;
        }): Promise<void>;
    };
    buildHostedResultsDocument(args: {
        model: string;
        payload?: unknown;
        views?: unknown;
        metrics?: unknown;
        panelState?: unknown;
        assets?: ResultsWebviewAssets;
    }): string;
    buildHostedResultsPanelState(args: unknown): ResultsPanelState | undefined;
    buildHostedResultsPanelTitle(args: unknown): string;
    handleHostedResultsRequest(args: {
        message: unknown;
        fallbackWorkspaceRoot?: string | (() => string | undefined);
        postMessage?: (message: unknown) => Promise<void> | void;
        onError?: (args: { method: string; error: unknown }) => void;
        handlers: Record<string, (args: {
            method: string;
            payload: unknown;
            modelRef?: ResultsPanelModelRef;
        }) => Promise<unknown> | unknown>;
    }): Promise<boolean>;
    handleHostedSimulationSettingsRequest(args: {
        message: unknown;
        postMessage?: (message: unknown) => Promise<void> | void;
        onError?: (args: { method: string; error: unknown }) => void;
        handlers: Record<string, (args: {
            method: string;
            payload: unknown;
        }) => Promise<unknown> | unknown>;
    }): Promise<boolean>;
    defaultThreeDimensionalViewerScript(): string;
    defaultVisualizationViews(): VisualizationView[];
    normalizeVisualizationViews(raw: unknown): VisualizationView[];
    normalizeHostedSimulationSettingsState(args: unknown): {
        activeModel: string;
        availableModels: string[];
        current: {
            solver: string;
            tEnd: number;
            dt: number | null;
            outputDir: string;
            sourceRootOverrides: string[];
        };
        views: VisualizationView[];
    };
    normalizeSimulationRunMetrics(raw: unknown): SimulationRunMetrics | undefined;
    normalizeSimulationPayload(raw: unknown): ParsedSimulationPayload | undefined;
    normalizeHostedResultsPanelState(raw: unknown, fallbackWorkspaceRoot?: string): ResultsPanelState | undefined;
    resetHostedProjectSimulationSettings(args: {
        model: string;
        loadViews: (args: { model: string }) => Promise<unknown> | unknown;
        resetPreset: (args: {
            model: string;
            previousViews: VisualizationView[];
        }) => Promise<unknown> | unknown;
        writeViews: (args: {
            model: string;
            views: VisualizationView[];
            previousViews: VisualizationView[];
        }) => Promise<unknown> | unknown;
        readCurrent: (args: { model: string }) => Promise<unknown> | unknown;
        readViews: (args: { model: string }) => Promise<unknown> | unknown;
        removeViews?: (args: {
            model: string;
            views: VisualizationView[];
        }) => Promise<void> | void;
        afterReset?: (args: {
            model: string;
            previousViews: VisualizationView[];
            current: {
                solver: string;
                tEnd: number;
                dt?: number | null;
                outputDir?: string;
                sourceRootPaths: string[];
            };
            views: VisualizationView[];
        }) => Promise<void> | void;
        defaultViews?: VisualizationView[];
        resetPresetError?: string;
        writeViewsError?: string;
    }): Promise<{
        current: {
            solver: string;
            tEnd: number;
            dt?: number | null;
            outputDir?: string;
            sourceRootPaths: string[];
        };
        views: VisualizationView[];
    }>;
    saveHostedProjectSimulationSettings(args: {
        model: string;
        preset: ModelSimulationPreset;
        views: VisualizationView[];
        loadViews: (args: { model: string }) => Promise<unknown> | unknown;
        persistViews: (args: {
            model: string;
            views: VisualizationView[];
            previousViews: VisualizationView[];
        }) => Promise<unknown> | unknown;
        writeViews: (args: {
            model: string;
            views: VisualizationView[];
            previousViews: VisualizationView[];
        }) => Promise<unknown> | unknown;
        writePreset: (args: {
            model: string;
            preset: ModelSimulationPreset;
            views: VisualizationView[];
            previousViews: VisualizationView[];
        }) => Promise<unknown> | unknown;
        removeStaleViews?: (args: {
            model: string;
            previousViews: VisualizationView[];
            nextViews: VisualizationView[];
        }) => Promise<void> | void;
        afterSave?: (args: {
            model: string;
            preset: ModelSimulationPreset;
            previousViews: VisualizationView[];
            views: VisualizationView[];
        }) => Promise<void> | void;
        writePresetError?: string;
        writeViewsError?: string;
    }): Promise<{
        ok: boolean;
        views: VisualizationView[];
    }>;
    buildSimulationRunDocument(args: {
        runId: string;
        model: string;
        savedAtUnixMs: number;
        payload: unknown;
        metrics?: unknown;
        views?: unknown;
    }): {
        version: 1;
        runId: string;
        model: string;
        savedAtUnixMs?: number;
        payload: ParsedSimulationPayload;
        metrics: SimulationRunMetrics | null;
        views: VisualizationView[];
    } | undefined;
    hydrateVisualizationViewsForModel(args: {
        views: unknown;
        model: string;
        resolveViewerScriptPath?: (model: string, viewId: string) => Promise<string> | string;
        readTextFile?: (scriptPath: string) => Promise<string> | string;
        writeMissingTextFile?: (scriptPath: string, content: string) => Promise<void> | void;
        defaultViewerScript?: () => string;
    }): Promise<VisualizationView[]>;
    nextSimulationRunLocation(
        model: string,
        pathExists?: (runPath: string) => boolean,
    ): {
        runId: string;
        runPath: string;
        savedAtUnixMs: number;
    };
    normalizeHostedResultsModelRef(
        raw: unknown,
        fallbackWorkspaceRoot?: string,
    ): ResultsPanelModelRef | undefined;
    normalizeHostedPngExportRequest(payload: unknown): {
        base64: string;
        defaultName: string;
    };
    normalizeHostedResultsNotifyPayload(payload: unknown): {
        message: string;
    };
    normalizeHostedWebmExportRequest(payload: unknown): {
        base64: string;
        defaultName: string;
    };
    normalizePersistedSimulationRun(raw: unknown): PersistedSimulationRun | undefined;
    loadHostedProjectResultsViews(args: {
        model: string;
        workspaceRoot?: string;
        loadConfiguredViews: (args: { model: string; workspaceRoot?: string }) => Promise<unknown> | unknown;
        hydrateViews: (args: {
            model: string;
            workspaceRoot?: string;
            views: VisualizationView[];
        }) => Promise<unknown> | unknown;
        defaultViews?: VisualizationView[];
    }): Promise<{ views: VisualizationView[] }>;
    resetHostedProjectResultsViews(args: {
        model: string;
        workspaceRoot?: string;
        loadConfiguredViews: (args: { model: string; workspaceRoot?: string }) => Promise<unknown> | unknown;
        writeConfiguredViews: (args: {
            model: string;
            workspaceRoot?: string;
            views: VisualizationView[];
            previousViews: VisualizationView[];
        }) => Promise<unknown> | unknown;
        hydrateViews: (args: {
            model: string;
            workspaceRoot?: string;
            views: VisualizationView[];
        }) => Promise<unknown> | unknown;
        removeViews?: (args: {
            model: string;
            workspaceRoot?: string;
            views: VisualizationView[];
        }) => Promise<void> | void;
        defaultViews?: VisualizationView[];
        writeViewsError?: string;
    }): Promise<{ views: VisualizationView[] }>;
    simulationRunDocumentPath(runId: string): string | undefined;
    persistVisualizationViewsForModel(args: {
        views: unknown;
        model: string;
        resolveViewerScriptPath?: (model: string, viewId: string) => Promise<string> | string;
        readTextFile?: (scriptPath: string) => Promise<string> | string;
        writeTextFile?: (scriptPath: string, content: string) => Promise<void> | void;
        defaultViewerScript?: () => string;
    }): Promise<VisualizationView[]>;
    readPersistedSimulationRunDocument(args: {
        runId: string;
        readTextFile: (runPath: string) => Promise<string> | string;
    }): Promise<PersistedSimulationRun | undefined>;
    loadHostedSimulationRun(args: {
        model?: string;
        runId?: string;
        readTextFile: (runPath: string) => Promise<string> | string;
    }): Promise<PersistedSimulationRun | {
        payload: ParsedSimulationPayload;
        metrics?: SimulationRunMetrics;
    } | null | undefined>;
    loadHostedSimulationRunWithViews(args: {
        model: string;
        runId?: string;
        workspaceRoot?: string;
        readTextFile: (runPath: string) => Promise<string> | string;
        loadConfiguredViews: (args: { model: string; workspaceRoot?: string }) => Promise<unknown> | unknown;
        hydrateViews?: (args: {
            model: string;
            workspaceRoot?: string;
            views: VisualizationView[];
        }) => Promise<unknown> | unknown;
        defaultViews?: VisualizationView[];
    }): Promise<{
        run: PersistedSimulationRun | {
            payload: ParsedSimulationPayload;
            metrics?: SimulationRunMetrics;
        } | null | undefined;
        views: VisualizationView[];
    } | undefined>;
    persistHostedSimulationRun(args: {
        model: string;
        payload?: unknown;
        metrics?: unknown;
        views?: unknown;
        hydrateViews?: (args: {
            model: string;
            views: VisualizationView[];
        }) => Promise<unknown> | unknown;
        pathExists?: (runPath: string) => boolean;
        writeTextFile?: (runPath: string, content: string) => Promise<void> | void;
        writeLastResultTextFile?: (resultPath: string, content: string) => Promise<void> | void;
    }): Promise<{
        runId: string;
        runPath: string;
        savedAtUnixMs: number;
        views: VisualizationView[];
    } | undefined>;
    persistHostedSimulationRunWithViews(args: {
        model: string;
        workspaceRoot?: string;
        payload?: unknown;
        metrics?: unknown;
        loadConfiguredViews: (args: { model: string; workspaceRoot?: string }) => Promise<unknown> | unknown;
        hydrateViews?: (args: {
            model: string;
            workspaceRoot?: string;
            views: VisualizationView[];
        }) => Promise<unknown> | unknown;
        defaultViews?: VisualizationView[];
        pathExists?: (runPath: string) => boolean;
        writeTextFile?: (runPath: string, content: string) => Promise<void> | void;
        writeLastResultTextFile?: (resultPath: string, content: string) => Promise<void> | void;
    }): Promise<{
        runId: string;
        runPath: string;
        savedAtUnixMs: number;
        views: VisualizationView[];
    } | undefined>;
    saveHostedProjectResultsViews(args: {
        model: string;
        workspaceRoot?: string;
        views: VisualizationView[];
        loadConfiguredViews: (args: { model: string; workspaceRoot?: string }) => Promise<unknown> | unknown;
        persistViews: (args: {
            model: string;
            workspaceRoot?: string;
            views: VisualizationView[];
            previousViews: VisualizationView[];
        }) => Promise<unknown> | unknown;
        writeConfiguredViews: (args: {
            model: string;
            workspaceRoot?: string;
            views: VisualizationView[];
            previousViews: VisualizationView[];
        }) => Promise<unknown> | unknown;
        hydrateViews: (args: {
            model: string;
            workspaceRoot?: string;
            views: VisualizationView[];
        }) => Promise<unknown> | unknown;
        writeViewsError?: string;
    }): Promise<{ views: VisualizationView[] }>;
    writePersistedSimulationRunDocument(args: {
        model: string;
        payload: unknown;
        metrics?: unknown;
        views?: unknown;
        pathExists?: (runPath: string) => boolean;
        writeTextFile: (runPath: string, content: string) => Promise<void> | void;
    }): Promise<{
        runId: string;
        runPath: string;
        savedAtUnixMs: number;
        runDoc: PersistedSimulationRun;
    } | undefined>;
}

let visualizationSharedCache: VisualizationSharedModule | undefined;

function loadVisualizationShared(): VisualizationSharedModule {
    if (visualizationSharedCache) {
        return visualizationSharedCache;
    }
    const candidates = [
        path.resolve(__dirname, '..', 'media', 'vendor', 'visualization_shared.js'),
        path.resolve(__dirname, '..', '..', '..', 'crates', 'rumoca-sim', 'web', 'visualization_shared.js'),
    ];
    for (const candidate of candidates) {
        if (!fs.existsSync(candidate)) {
            continue;
        }
        const loaded = nodeRequire(candidate) as Partial<VisualizationSharedModule>;
        if (typeof loaded.buildHostedResultsDocument !== 'function'
            || typeof loaded.buildHostedSimulationSettingsDocument !== 'function'
            || typeof loaded.buildHostedSimulationSettingsState !== 'function'
            || typeof loaded.handleHostedResultsRequest !== 'function'
            || typeof loaded.handleHostedSimulationSettingsRequest !== 'function'
            || typeof loaded.defaultThreeDimensionalViewerScript !== 'function'
            || typeof loaded.defaultVisualizationViews !== 'function'
            || typeof loaded.normalizeVisualizationViews !== 'function'
            || typeof loaded.normalizeHostedPngExportRequest !== 'function'
            || typeof loaded.normalizeHostedResultsNotifyPayload !== 'function'
            || typeof loaded.normalizeHostedWebmExportRequest !== 'function'
            || typeof loaded.normalizeSimulationRunMetrics !== 'function'
            || typeof loaded.normalizeSimulationPayload !== 'function'
            || typeof loaded.resetHostedProjectSimulationSettings !== 'function'
            || typeof loaded.resetHostedProjectResultsViews !== 'function'
            || typeof loaded.saveHostedProjectSimulationSettings !== 'function'
            || typeof loaded.saveHostedProjectResultsViews !== 'function'
            || typeof loaded.buildSimulationRunDocument !== 'function'
            || typeof loaded.hydrateVisualizationViewsForModel !== 'function'
            || typeof loaded.loadHostedProjectResultsViews !== 'function'
            || typeof loaded.nextSimulationRunLocation !== 'function'
            || typeof loaded.normalizeHostedResultsModelRef !== 'function'
            || typeof loaded.normalizePersistedSimulationRun !== 'function'
            || typeof loaded.persistVisualizationViewsForModel !== 'function'
            || typeof loaded.readPersistedSimulationRunDocument !== 'function'
            || typeof loaded.simulationRunDocumentPath !== 'function'
            || typeof loaded.writePersistedSimulationRunDocument !== 'function') {
            continue;
        }
        visualizationSharedCache = loaded as VisualizationSharedModule;
        return visualizationSharedCache;
    }
    throw new Error('Failed to load shared visualization contract from rumoca-sim.');
}

interface SimulationModelStateResponse {
    ok?: boolean;
    models?: unknown;
    selectedModel?: unknown;
    error?: string;
}

function normalizeSimulationModelState(
    response: SimulationModelStateResponse | undefined,
): {
    ok: boolean;
    models: string[];
    selectedModel?: string;
    error?: string;
} {
    const models = Array.isArray(response?.models)
        ? response.models
            .filter((entry): entry is string => typeof entry === 'string')
            .map(entry => entry.trim())
            .filter(Boolean)
        : [];
    const selectedModel = typeof response?.selectedModel === 'string'
        ? response.selectedModel.trim()
        : '';
    return {
        ok: response?.ok === true,
        models,
        selectedModel: selectedModel.length > 0 ? selectedModel : undefined,
        error: typeof response?.error === 'string' ? response.error : undefined,
    };
}

async function getSimulationModelState(
    documentUri: string,
    defaultModel?: string,
): Promise<{
    ok: boolean;
    models: string[];
    selectedModel?: string;
    error?: string;
}> {
    const response = await sendProjectCommand<SimulationModelStateResponse>(
        'rumoca.project.getSimulationModels',
        {
            uri: documentUri,
            defaultModel: defaultModel?.trim() || null,
        },
    );
    return normalizeSimulationModelState(response);
}

async function setSelectedSimulationModel(
    documentUri: string,
    model: string,
    defaultModel?: string,
): Promise<{
    ok: boolean;
    models: string[];
    selectedModel?: string;
    error?: string;
}> {
    const response = await sendProjectCommand<SimulationModelStateResponse>(
        'rumoca.project.setSelectedSimulationModel',
        {
            uri: documentUri,
            model,
            defaultModel: defaultModel?.trim() || null,
        },
    );
    return normalizeSimulationModelState(response);
}

function resolveSourceRootPaths(config: vscode.WorkspaceConfiguration): SourceRootPathSources {
    return resolveSourceRootPathsForEntries(
        config.get<string[]>('sourceRootPaths') ?? [],
        process.env,
    );
}

function getSimulationSettings(config: vscode.WorkspaceConfiguration): SimulationSettings {
    const dtRaw = config.get<number>('simulation.dt');
    const dt = Number.isFinite(dtRaw) && (dtRaw ?? 0) > 0 ? dtRaw : undefined;
    const solverRaw = (config.get<string>('simulation.solver') ?? 'auto').toLowerCase();
    const solver = solverRaw === 'bdf' || solverRaw === 'rk-like' ? solverRaw : 'auto';
    const sourceRootPaths = resolveSourceRootPaths(config);
    return {
        model: (config.get<string>('simulation.model') ?? '').trim(),
        tEnd: config.get<number>('simulation.tEnd') ?? 10.0,
        dt,
        solver,
        outputDir: (config.get<string>('simulation.outputDir') ?? '').trim(),
        sourceRootPaths: sourceRootPaths.mergedPaths,
    };
}

function resolveWorkspaceRootForDocument(document: vscode.TextDocument): string | undefined {
    const folder = vscode.workspace.getWorkspaceFolder(document.uri);
    if (folder) {
        return folder.uri.fsPath;
    }
    return vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
}

function resolveWorkspaceRootFallback(): string | undefined {
    return vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
}

interface ProjectSimulationConfigResponse {
    preset?: ModelSimulationPreset;
    defaults?: SimulationExecutionSettings;
    effective?: SimulationExecutionSettings;
    diagnostics?: string[];
}

interface ScatterSeriesConfig {
    name: string;
    x: string;
    y: string;
}

interface VisualizationView {
    id: string;
    title: string;
    type: 'timeseries' | 'scatter' | '3d';
    x?: string;
    y: string[];
    scatterSeries?: ScatterSeriesConfig[];
    script?: string;
    scriptPath?: string;
}

interface ProjectResyncSidecarsReport {
    dry_run: boolean;
    prune_orphans: boolean;
    parsed_model_files: number;
    parse_failures: number;
    discovered_models: number;
    remapped_models: number;
    removed_orphans: number;
}

interface ParsedSimulationPayload {
    version?: number;
    names: string[];
    allData: number[][];
    nStates: number;
    variableMeta: unknown[];
    simDetails: unknown;
}

interface BackgroundRequestAccepted {
    ok?: boolean;
    requestId?: string;
    error?: string;
}

interface SimulationCompleteNotification {
    requestId?: string;
    ok?: boolean;
    payload?: unknown;
    error?: string;
    metrics?: unknown;
}

interface PrepareSimulationModelsCompleteNotification {
    requestId?: string;
    ok?: boolean;
    preparedModels?: string[];
    failures?: Array<{ model?: string; error?: string }>;
    error?: string;
}

interface BuiltinCodegenTemplate {
    id: string;
    label: string;
    language: string;
    source: string;
}

interface CodegenSettings {
    mode: 'builtin' | 'custom';
    builtinTemplateId: string;
    customTemplatePath: string;
}

interface ResolvedCodegenTemplate {
    source: string;
    label: string;
    language: string;
}

interface CodegenRenderResponse {
    ok?: boolean;
    output?: string;
    error?: string;
}

interface CodegenOutputState {
    model: string;
    templateLabel: string;
    language: string;
    output: string;
    workspaceRoot?: string;
    suggestedFileName: string;
}

const DEFAULT_CODEGEN_TEMPLATE_ID = 'sympy.py.jinja';
const CODEGEN_SETTINGS_STATE_KEY = 'rumoca.codegenSettings';
let builtInCodegenTemplatesCache: BuiltinCodegenTemplate[] | undefined;

function wireSimulationJobNotifications(activeClient: LanguageClient) {
    activeClient.onNotification('rumoca/simulationComplete', (payload: SimulationCompleteNotification) => {
        const requestId = typeof payload?.requestId === 'string' ? payload.requestId : '';
        if (!requestId) {
            return;
        }
        const pending = pendingSimulationJobs.get(requestId);
        if (!pending) {
            return;
        }
        pendingSimulationJobs.delete(requestId);
        pending.resolve(payload);
    });
    activeClient.onNotification(
        'rumoca/prepareSimulationModelsComplete',
        (payload: PrepareSimulationModelsCompleteNotification) => {
            const requestId = typeof payload?.requestId === 'string' ? payload.requestId : '';
            if (!requestId) {
                return;
            }
            const pending = pendingPrepareSimulationJobs.get(requestId);
            if (!pending) {
                return;
            }
            pendingPrepareSimulationJobs.delete(requestId);
            pending.resolve(payload);
        },
    );
}

function trimMaybeString(value: unknown): string {
    return typeof value === 'string' ? value.trim() : '';
}

function defaultCodegenSettings(): CodegenSettings {
    return {
        mode: 'builtin',
        builtinTemplateId: DEFAULT_CODEGEN_TEMPLATE_ID,
        customTemplatePath: '',
    };
}

function normalizeCodegenSettings(raw: unknown): CodegenSettings {
    const next = raw && typeof raw === 'object' && !Array.isArray(raw)
        ? raw as Record<string, unknown>
        : {};
    return {
        mode: next.mode === 'custom' ? 'custom' : 'builtin',
        builtinTemplateId: trimMaybeString(next.builtinTemplateId) || DEFAULT_CODEGEN_TEMPLATE_ID,
        customTemplatePath: trimMaybeString(next.customTemplatePath),
    };
}

function normalizeBuiltinCodegenTemplates(raw: unknown): BuiltinCodegenTemplate[] {
    if (!Array.isArray(raw)) {
        return [];
    }
    return raw
        .map((entry) => {
            const next = entry && typeof entry === 'object' && !Array.isArray(entry)
                ? entry as Record<string, unknown>
                : {};
            return {
                id: trimMaybeString(next.id),
                label: trimMaybeString(next.label),
                language: trimMaybeString(next.language) || 'plaintext',
                source: typeof next.source === 'string' ? next.source : '',
            };
        })
        .filter((entry) => entry.id.length > 0 && entry.label.length > 0 && entry.source.length > 0);
}

function findBuiltinCodegenTemplate(
    templates: BuiltinCodegenTemplate[],
    templateId: string,
): BuiltinCodegenTemplate | undefined {
    const preferredId = trimMaybeString(templateId);
    if (!preferredId) {
        return templates[0];
    }
    return templates.find((template) => template.id === preferredId) ?? templates[0];
}

function inferCodegenLanguage(templateId: string): string {
    const normalized = trimMaybeString(templateId).toLowerCase();
    if (normalized.endsWith('.py.jinja')) return 'python';
    if (normalized.endsWith('.jl.jinja')) return 'julia';
    if (normalized.endsWith('.c.jinja') || normalized.endsWith('.h.jinja')) return 'c';
    if (normalized.endsWith('.xml.jinja')) return 'xml';
    if (normalized.endsWith('.mo.jinja')) return 'modelica';
    if (normalized.endsWith('.json.jinja')) return 'json';
    if (normalized.endsWith('.html.jinja')) return 'html';
    return 'plaintext';
}

function normalizeCodegenLanguageId(language: string): string {
    const normalized = trimMaybeString(language).toLowerCase();
    if (normalized === 'python' || normalized === 'julia' || normalized === 'c'
        || normalized === 'xml' || normalized === 'html' || normalized === 'json') {
        return normalized;
    }
    return normalized === 'modelica' ? 'modelica' : 'plaintext';
}

function codegenOutputExtension(templateLabel: string, language: string): string {
    const normalizedLabel = trimMaybeString(templateLabel);
    const jinjaIndex = normalizedLabel.toLowerCase().lastIndexOf('.jinja');
    if (jinjaIndex > 0) {
        const withoutJinja = normalizedLabel.slice(0, jinjaIndex);
        const ext = path.extname(withoutJinja).replace(/^\./, '').trim();
        if (ext.length > 0) {
            return ext;
        }
    }
    const normalizedLanguage = normalizeCodegenLanguageId(language);
    if (normalizedLanguage === 'python') return 'py';
    if (normalizedLanguage === 'julia') return 'jl';
    if (normalizedLanguage === 'modelica') return 'mo';
    if (normalizedLanguage === 'xml') return 'xml';
    if (normalizedLanguage === 'html') return 'html';
    if (normalizedLanguage === 'json') return 'json';
    if (normalizedLanguage === 'c') return 'c';
    return 'txt';
}

function codegenSaveFilters(templateLabel: string, language: string): Record<string, string[]> {
    const extension = codegenOutputExtension(templateLabel, language);
    const normalizedLanguage = normalizeCodegenLanguageId(language);
    if (normalizedLanguage === 'python') return { 'Python': [extension] };
    if (normalizedLanguage === 'julia') return { 'Julia': [extension] };
    if (normalizedLanguage === 'modelica') return { 'Modelica': [extension] };
    if (normalizedLanguage === 'xml') return { 'XML': [extension] };
    if (normalizedLanguage === 'html') return { 'HTML': [extension] };
    if (normalizedLanguage === 'json') return { 'JSON': [extension] };
    if (normalizedLanguage === 'c') return { 'C Source': [extension] };
    return { 'Text Files': [extension] };
}

function sanitizeCodegenFileStem(model: string): string {
    const normalized = trimMaybeString(model).replace(/[^A-Za-z0-9._-]+/g, '_');
    return normalized.length > 0 ? normalized : 'model';
}

function suggestedCodegenFileName(model: string, templateLabel: string, language: string): string {
    const stem = sanitizeCodegenFileStem(model);
    const extension = codegenOutputExtension(templateLabel, language);
    return `${stem}.${extension}`;
}

function normalizeComparableFsPath(fsPath: string): string {
    const normalized = path.normalize(fsPath);
    return process.platform === 'win32' ? normalized.toLowerCase() : normalized;
}

function escapeHtml(value: string): string {
    return value
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;');
}

function buildCodegenOutputWebviewHtml(state: CodegenOutputState): string {
    return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Rumoca Template Output</title>
  <style>
    body {
      font-family: var(--vscode-font-family);
      color: var(--vscode-editor-foreground);
      background: var(--vscode-editor-background);
      margin: 0;
      padding: 16px;
      display: flex;
      flex-direction: column;
      gap: 12px;
      height: 100vh;
      box-sizing: border-box;
    }
    .toolbar {
      display: flex;
      align-items: center;
      gap: 12px;
      flex-wrap: wrap;
    }
    .summary {
      color: var(--vscode-descriptionForeground);
    }
    button {
      border: 1px solid var(--vscode-button-border, transparent);
      border-radius: 6px;
      padding: 7px 12px;
      cursor: pointer;
      color: var(--vscode-button-foreground);
      background: var(--vscode-button-background);
    }
    pre {
      margin: 0;
      padding: 16px;
      flex: 1;
      overflow: auto;
      white-space: pre-wrap;
      font-family: var(--vscode-editor-font-family, monospace);
      font-size: var(--vscode-editor-font-size, 13px);
      line-height: 1.5;
      background: var(--vscode-textCodeBlock-background, var(--vscode-editorWidget-background));
      border: 1px solid var(--vscode-panel-border);
      border-radius: 8px;
    }
  </style>
</head>
<body>
  <div class="toolbar">
    <button id="saveBtn" type="button">Save</button>
    <span class="summary">Rendered with: ${escapeHtml(state.templateLabel)}</span>
  </div>
  <pre>${escapeHtml(state.output)}</pre>
  <script>
    const vscode = acquireVsCodeApi();
    document.getElementById('saveBtn').addEventListener('click', () => {
      vscode.postMessage({ command: 'save' });
    });
  </script>
</body>
</html>`;
}

async function sendExecuteCommand<T>(
    command: string,
    payload: Record<string, unknown>
): Promise<T | undefined> {
    if (!client) {
        return undefined;
    }
    const response = await client.sendRequest('workspace/executeCommand', {
        command,
        arguments: [payload],
    });
    return response as T | undefined;
}

async function sendProjectCommand<T>(
    command: string,
    payload: Record<string, unknown>
): Promise<T | undefined> {
    return await sendExecuteCommand<T>(command, payload);
}

async function sendWorkspaceCommand<T>(
    command: string,
    payload: Record<string, unknown>
): Promise<T | undefined> {
    return await sendExecuteCommand<T>(command, payload);
}

function loadStoredCodegenSettings(context: vscode.ExtensionContext): CodegenSettings {
    return normalizeCodegenSettings(context.workspaceState.get(CODEGEN_SETTINGS_STATE_KEY));
}

async function storeCodegenSettings(
    context: vscode.ExtensionContext,
    settings: CodegenSettings,
): Promise<void> {
    await context.workspaceState.update(
        CODEGEN_SETTINGS_STATE_KEY,
        normalizeCodegenSettings(settings),
    );
}

async function loadBuiltInCodegenTemplates(
    forceReload = false,
): Promise<BuiltinCodegenTemplate[]> {
    if (!forceReload && builtInCodegenTemplatesCache) {
        return builtInCodegenTemplatesCache;
    }
    const response = await sendWorkspaceCommand<unknown>('rumoca.workspace.getBuiltinTemplates', {});
    const templates = normalizeBuiltinCodegenTemplates(response);
    if (templates.length === 0) {
        throw new Error('No built-in codegen templates are available from rumoca-lsp.');
    }
    builtInCodegenTemplatesCache = templates;
    return templates;
}

async function loadCurrentCodegenSettingsState(
    context: vscode.ExtensionContext,
): Promise<{ settings: CodegenSettings; templates: BuiltinCodegenTemplate[] }> {
    const templates = await loadBuiltInCodegenTemplates();
    let settings = loadStoredCodegenSettings(context);
    const builtin = findBuiltinCodegenTemplate(templates, settings.builtinTemplateId);
    if (settings.mode === 'builtin' && builtin && builtin.id !== settings.builtinTemplateId) {
        settings = {
            ...settings,
            builtinTemplateId: builtin.id,
        };
        await storeCodegenSettings(context, settings);
    }
    return { settings, templates };
}

function workspaceRelativeTemplatePath(
    workspaceRoot: string | undefined,
    absolutePath: string,
): string {
    if (!workspaceRoot) {
        return absolutePath;
    }
    const relativePath = path.relative(workspaceRoot, absolutePath);
    if (relativePath.length === 0) {
        return path.basename(absolutePath);
    }
    if (relativePath.startsWith('..') || path.isAbsolute(relativePath)) {
        return absolutePath;
    }
    return relativePath.split(path.sep).join('/');
}

function resolveCodegenTemplateAbsolutePath(
    templatePath: string,
    workspaceRoot: string | undefined,
    sourceDocument: vscode.TextDocument,
): string {
    if (path.isAbsolute(templatePath)) {
        return templatePath;
    }
    if (workspaceRoot) {
        return path.resolve(workspaceRoot, templatePath);
    }
    return path.resolve(path.dirname(sourceDocument.uri.fsPath), templatePath);
}

async function readCodegenTemplateSource(
    templatePath: string,
    workspaceRoot: string | undefined,
    sourceDocument: vscode.TextDocument,
): Promise<ResolvedCodegenTemplate> {
    const absolutePath = resolveCodegenTemplateAbsolutePath(templatePath, workspaceRoot, sourceDocument);
    const comparableTargetPath = normalizeComparableFsPath(absolutePath);
    const openTemplateDocument = vscode.workspace.textDocuments.find((document) =>
        document.uri.scheme === 'file'
        && normalizeComparableFsPath(document.uri.fsPath) === comparableTargetPath,
    );
    if (openTemplateDocument) {
        return {
            source: openTemplateDocument.getText(),
            label: workspaceRelativeTemplatePath(workspaceRoot, absolutePath),
            language: inferCodegenLanguage(templatePath),
        };
    }
    const source = await fs.promises.readFile(absolutePath, 'utf-8');
    return {
        source,
        label: workspaceRelativeTemplatePath(workspaceRoot, absolutePath),
        language: inferCodegenLanguage(templatePath),
    };
}

async function resolveCodegenTemplateSelection(
    context: vscode.ExtensionContext,
    sourceDocument: vscode.TextDocument,
    workspaceRoot: string | undefined,
): Promise<ResolvedCodegenTemplate> {
    const settings = loadStoredCodegenSettings(context);
    const templates = await loadBuiltInCodegenTemplates();
    if (settings.mode === 'custom') {
        const templatePath = trimMaybeString(settings.customTemplatePath);
        if (!templatePath) {
            throw new Error('Choose a custom template file in Rumoca Settings.');
        }
        return await readCodegenTemplateSource(templatePath, workspaceRoot, sourceDocument);
    }
    const builtin = findBuiltinCodegenTemplate(templates, settings.builtinTemplateId);
    if (!builtin) {
        throw new Error('No built-in codegen templates are available from rumoca-lsp.');
    }
    return {
        source: builtin.source,
        label: builtin.label,
        language: builtin.language || inferCodegenLanguage(builtin.id),
    };
}

async function getProjectSimulationConfig(
    model: string,
    workspaceRoot: string | undefined,
    fallback: SimulationSettings
): Promise<ProjectSimulationConfigResponse | undefined> {
    return await sendProjectCommand<ProjectSimulationConfigResponse>(
        'rumoca.project.getSimulationConfig',
        {
            workspaceRoot,
            model,
            fallback,
        }
    );
}

async function reopenEmbeddedModelicaDocuments() {
    if (!client) {
        return;
    }
    openVirtualDocuments.clear();
    for (const [cellUri, blocks] of modelicaBlocks.entries()) {
        for (let index = 0; index < blocks.length; index++) {
            const block = blocks[index];
            const virtualUri = getVirtualDocumentUri(cellUri, index);
            const virtualUriStr = virtualUri.toString();
            try {
                await client.sendNotification('textDocument/didOpen', {
                    textDocument: {
                        uri: virtualUriStr,
                        languageId: 'modelica',
                        version: 1,
                        text: block.content
                    }
                });
                openVirtualDocuments.set(virtualUriStr, { version: 1, content: block.content });
            } catch {
                // Ignore restart replay errors; the next edit will retry.
            }
        }
    }
}

async function setProjectSimulationPreset(
    model: string,
    workspaceRoot: string | undefined,
    preset: ModelSimulationPreset
): Promise<boolean> {
    if (!workspaceRoot) {
        return false;
    }
    const response = await sendProjectCommand<{ ok?: boolean }>(
        'rumoca.project.setSimulationPreset',
        {
            workspaceRoot,
            model,
            preset,
        }
    );
    return response?.ok === true;
}

async function resetProjectSimulationPreset(
    model: string,
    workspaceRoot: string | undefined
): Promise<boolean> {
    if (!workspaceRoot) {
        return false;
    }
    const response = await sendProjectCommand<{ ok?: boolean }>(
        'rumoca.project.resetSimulationPreset',
        {
            workspaceRoot,
            model,
        }
    );
    return response?.ok === true;
}

function defaultThreeDimensionalViewerScript(): string {
    return loadVisualizationShared().defaultThreeDimensionalViewerScript();
}

function defaultVisualizationViews(): VisualizationView[] {
    return loadVisualizationShared().defaultVisualizationViews();
}

function normalizeVisualizationViews(raw: unknown): VisualizationView[] {
    return loadVisualizationShared().normalizeVisualizationViews(raw);
}

async function getProjectVisualizationConfig(
    model: string,
    workspaceRoot: string | undefined
): Promise<VisualizationView[]> {
    const response = await sendProjectCommand<{ views?: unknown }>(
        'rumoca.project.getVisualizationConfig',
        {
            workspaceRoot,
            model,
        }
    );
    return normalizeVisualizationViews(response?.views);
}

async function setProjectVisualizationConfig(
    model: string,
    workspaceRoot: string | undefined,
    views: VisualizationView[]
): Promise<boolean> {
    if (!workspaceRoot) {
        return false;
    }
    const response = await sendProjectCommand<{ ok?: boolean }>(
        'rumoca.project.setVisualizationConfig',
        {
            workspaceRoot,
            model,
            views,
        }
    );
    return response?.ok === true;
}

async function resyncProjectSidecars(
    workspaceRoot: string | undefined,
    options?: { dryRun?: boolean; pruneOrphans?: boolean; reason?: string },
): Promise<ProjectResyncSidecarsReport | undefined> {
    if (!workspaceRoot) {
        return undefined;
    }
    const response = await sendProjectCommand<{ ok?: boolean; report?: unknown }>(
        'rumoca.project.resyncSidecars',
        {
            workspaceRoot,
            dryRun: options?.dryRun ?? false,
            pruneOrphans: options?.pruneOrphans ?? false,
            reason: options?.reason ?? 'manual',
        },
    );
    if (!response?.ok || !response.report || typeof response.report !== 'object') {
        return undefined;
    }
    const raw = response.report as Record<string, unknown>;
    const report: ProjectResyncSidecarsReport = {
        dry_run: Boolean(raw.dry_run),
        prune_orphans: Boolean(raw.prune_orphans),
        parsed_model_files: Number(raw.parsed_model_files ?? 0),
        parse_failures: Number(raw.parse_failures ?? 0),
        discovered_models: Number(raw.discovered_models ?? 0),
        remapped_models: Number(raw.remapped_models ?? 0),
        removed_orphans: Number(raw.removed_orphans ?? 0),
    };
    return report;
}

async function notifyProjectFilesMoved(
    workspaceRoot: string | undefined,
    files: Array<{ oldPath: string; newPath: string }>,
): Promise<ProjectResyncSidecarsReport | undefined> {
    if (!workspaceRoot || files.length === 0) {
        return undefined;
    }
    const response = await sendProjectCommand<{ ok?: boolean; report?: unknown }>(
        'rumoca.project.filesMoved',
        {
            workspaceRoot,
            files,
        },
    );
    if (!response?.ok || !response.report || typeof response.report !== 'object') {
        return undefined;
    }
    const raw = response.report as Record<string, unknown>;
    return {
        dry_run: Boolean(raw.dry_run),
        prune_orphans: Boolean(raw.prune_orphans),
        parsed_model_files: Number(raw.parsed_model_files ?? 0),
        parse_failures: Number(raw.parse_failures ?? 0),
        discovered_models: Number(raw.discovered_models ?? 0),
        remapped_models: Number(raw.remapped_models ?? 0),
        removed_orphans: Number(raw.removed_orphans ?? 0),
    };
}

function resolveWorkspaceVisualizationScriptPath(
    workspaceRoot: string | undefined,
    scriptPath: string,
): string | undefined {
    if (!workspaceRoot) {
        return undefined;
    }
    return path.isAbsolute(scriptPath)
        ? scriptPath
        : path.join(workspaceRoot, scriptPath);
}

function buildWorkspaceVisualizationViewStorage(workspaceRoot: string | undefined) {
    return loadVisualizationShared().buildVisualizationViewStorageHandlers({
        resolveViewerScriptPath: async (nextModel, viewId) => await resolvePreferredViewerScriptPath(
            workspaceRoot,
            nextModel,
            viewId,
        ),
        readTextFile: async (scriptPath) => {
            const absPath = resolveWorkspaceVisualizationScriptPath(workspaceRoot, scriptPath);
            if (!absPath) {
                return '';
            }
            return await fs.promises.readFile(absPath, 'utf-8');
        },
        writeTextFile: async (scriptPath, content) => {
            const absPath = resolveWorkspaceVisualizationScriptPath(workspaceRoot, scriptPath);
            if (!absPath) {
                return;
            }
            await fs.promises.mkdir(path.dirname(absPath), { recursive: true });
            await fs.promises.writeFile(absPath, content, 'utf-8');
        },
        defaultViewerScript: () => defaultThreeDimensionalViewerScript(),
    });
}

async function loadHydratedVisualizationViews(
    model: string,
    workspaceRoot: string | undefined,
): Promise<VisualizationView[]> {
    const configuredViews = await getProjectVisualizationConfig(model, workspaceRoot);
    const baseViews = configuredViews.length > 0 ? configuredViews : defaultVisualizationViews();
    return await buildWorkspaceVisualizationViewStorage(workspaceRoot).hydrateViews({
        views: baseViews,
        model,
    });
}

function normalizeSimulationRunMetrics(raw: unknown): SimulationRunMetrics | undefined {
    return loadVisualizationShared().normalizeSimulationRunMetrics(raw);
}

function normalizeSimulationPayload(raw: unknown): ParsedSimulationPayload | undefined {
    return loadVisualizationShared().normalizeSimulationPayload(raw);
}

async function runRumocaSimulation(
    modelUri: string,
    model: string,
    settings: SimulationExecutionSettings,
    onProgress?: (message: string) => void,
): Promise<SimulationRunResult> {
    if (onProgress) {
        onProgress('Queued rumoca-lsp simulation...');
    }
    const accepted = await sendProjectCommand<BackgroundRequestAccepted>('rumoca.project.startSimulation', {
        uri: modelUri,
        model,
        settings: {
            solver: settings.solver,
            tEnd: settings.tEnd,
            dt: settings.dt ?? null,
            sourceRootPaths: settings.sourceRootPaths ?? [],
        },
    });

    if (!accepted) {
        return {
            exitCode: 1,
            stderr: 'No response from rumoca-lsp simulation start command.',
            payload: undefined,
            metrics: undefined,
        };
    }

    if (!accepted.ok || !accepted.requestId) {
        return {
            exitCode: 1,
            stderr: accepted.error ?? 'rumoca-lsp rejected the simulation request.',
            payload: undefined,
            metrics: undefined,
        };
    }

    const response = await new Promise<SimulationCompleteNotification>((resolve) => {
        pendingSimulationJobs.set(accepted.requestId!, { resolve });
    });
    const metrics = normalizeSimulationRunMetrics(response?.metrics);
    const payload = normalizeSimulationPayload(response?.payload);
    if (metrics && onProgress) {
        onProgress(
            `compile=${metrics.compileSeconds.toFixed(2)}s · simulate=${metrics.simulateSeconds.toFixed(2)}s · points=${metrics.points} · vars=${metrics.variables}`
        );
    }
    if (response.ok && payload) {
        return {
            exitCode: 0,
            stderr: '',
            payload,
            metrics,
        };
    }

    return {
        exitCode: 1,
        stderr: response.error ?? 'Simulation failed in rumoca-lsp.',
        payload,
        metrics,
    };
}

async function prepareRumocaSimulationModels(
    modelUri: string,
    models: string[],
    settings: SimulationExecutionSettings,
): Promise<{
    ok: boolean;
    preparedModels: string[];
    failures: Array<{ model?: string; error?: string }>;
    error?: string;
}> {
    const accepted = await sendProjectCommand<BackgroundRequestAccepted>(
        'rumoca.project.prepareSimulationModels',
        {
            uri: modelUri,
            models,
            settings: {
                solver: settings.solver,
                tEnd: settings.tEnd,
                dt: settings.dt ?? null,
                sourceRootPaths: settings.sourceRootPaths ?? [],
            },
        },
    );
    if (!accepted) {
        return {
            ok: false,
            preparedModels: [],
            failures: [],
            error: 'No response from rumoca-lsp prepare request.',
        };
    }
    if (!accepted.ok || !accepted.requestId) {
        return {
            ok: false,
            preparedModels: [],
            failures: [],
            error: accepted.error ?? 'rumoca-lsp rejected the prepare request.',
        };
    }
    const response = await new Promise<PrepareSimulationModelsCompleteNotification>((resolve) => {
        pendingPrepareSimulationJobs.set(accepted.requestId!, { resolve });
    });
    return {
        ok: response.ok === true,
        preparedModels: response.preparedModels ?? [],
        failures: response.failures ?? [],
        error: response.error,
    };
}

function buildResultsWebviewHtml(
    model: string,
    payload: ParsedSimulationPayload | undefined,
    views: VisualizationView[],
    metrics?: SimulationRunMetrics,
    panelState?: ResultsPanelState,
    assets?: ResultsWebviewAssets,
): string {
    return loadVisualizationShared().buildHostedResultsDocument({
        model,
        payload,
        views: views.length > 0 ? views : defaultVisualizationViews(),
        metrics,
        panelState,
        assets,
    });
}

async function showSimulationSettingsPanel(
    context: vscode.ExtensionContext,
    workspaceRoot: string | undefined,
    sourceDocumentUri: string | undefined,
    preferredModel?: string,
): Promise<void> {
    await resyncProjectSidecars(workspaceRoot, {
        reason: 'open-settings',
        pruneOrphans: false,
    });
    const fallbackDefaults = getSimulationSettings(vscode.workspace.getConfiguration('rumoca'));
    if (!sourceDocumentUri) {
        throw new Error('Cannot open simulation settings without a source document URI.');
    }
    const modelState = await getSimulationModelState(
        sourceDocumentUri,
        preferredModel ?? fallbackDefaults.model,
    );
    if (!modelState.ok) {
        throw new Error(modelState.error ?? 'Failed to enumerate simulation models for the active file.');
    }
    const activeModel =
        (preferredModel && modelState.models.includes(preferredModel) ? preferredModel : undefined)
        ?? modelState.selectedModel;
    if (!activeModel) {
        throw new Error('Could not infer a model name from the active file.');
    }

    const projectConfig = await getProjectSimulationConfig(activeModel, workspaceRoot, fallbackDefaults);
    const defaults = projectConfig?.defaults ?? fallbackDefaults;
    const configuredViews = await getProjectVisualizationConfig(activeModel, workspaceRoot);
    const {
        settings: codegenSettings,
        templates: codegenTemplates,
    } = await loadCurrentCodegenSettingsState(context);
    const settingsState = loadVisualizationShared().buildHostedSimulationSettingsState({
        activeModel,
        availableModels: modelState.models.length > 0 ? modelState.models : [activeModel],
        current: projectConfig?.preset,
        fallbackCurrent: {
            solver: defaults.solver,
            tEnd: defaults.tEnd,
            dt: defaults.dt,
            outputDir: defaults.outputDir,
            sourceRootPaths: [...defaults.sourceRootPaths],
        },
        codegen: codegenSettings,
        fallbackCodegen: defaultCodegenSettings(),
        codegenTemplates,
        views: configuredViews,
        defaultViews: defaultVisualizationViews(),
    });

    const panel = vscode.window.createWebviewPanel(
        'rumocaSimulationSettings',
        `Rumoca Settings: ${activeModel}`,
        vscode.ViewColumn.Beside,
        { enableScripts: true, retainContextWhenHidden: true },
    );

    panel.webview.html = loadVisualizationShared().buildHostedSimulationSettingsDocument(settingsState);

    const settingsHandlers = loadVisualizationShared().buildHostedSimulationSettingsHandlers({
        getActiveModel: () => activeModel,
        save: async ({ model, preset, codegenSettings: nextCodegenSettings, views }: {
            model: string;
            preset: ModelSimulationPreset;
            codegenSettings: CodegenSettings;
            views: VisualizationView[];
        }) => {
            const saved = await loadVisualizationShared().saveHostedProjectSimulationSettings({
                model,
                preset,
                views,
                loadViews: async ({ model: nextModel }) =>
                    await getProjectVisualizationConfig(nextModel, workspaceRoot),
                persistViews: async ({ model: nextModel, views: nextViews }) =>
                    await buildWorkspaceVisualizationViewStorage(workspaceRoot).persistViews({
                        views: nextViews,
                        model: nextModel,
                    }),
                writeViews: async ({ model: nextModel, views: nextViews }) =>
                    await setProjectVisualizationConfig(nextModel, workspaceRoot, nextViews),
                writePreset: async ({ model: nextModel, preset: nextPreset }) =>
                    await setProjectSimulationPreset(nextModel, workspaceRoot, nextPreset),
                afterSave: async () => {
                    await resyncProjectSidecars(workspaceRoot, {
                        reason: 'save-settings',
                        pruneOrphans: false,
                    });
                },
                writePresetError: 'Failed to save preset via LSP project config endpoint.',
                writeViewsError:
                    'Failed to save results-panel configuration via LSP project config endpoint.',
            });
            await storeCodegenSettings(context, nextCodegenSettings);
            return saved;
        },
        reset: async ({ model }: { model: string }) => {
            const resetCodegen = defaultCodegenSettings();
            const resetState = await loadVisualizationShared().resetHostedProjectSimulationSettings({
                model,
                loadViews: async ({ model: nextModel }) =>
                    await getProjectVisualizationConfig(nextModel, workspaceRoot),
                resetPreset: async ({ model: nextModel }) =>
                    await resetProjectSimulationPreset(nextModel, workspaceRoot),
                writeViews: async ({ model: nextModel, views }) =>
                    await setProjectVisualizationConfig(nextModel, workspaceRoot, views),
                readCurrent: async ({ model: nextModel }) =>
                    (
                        await getProjectSimulationConfig(nextModel, workspaceRoot, fallbackDefaults)
                    )?.defaults ?? defaults,
                readViews: async ({ model: nextModel }) =>
                    await getProjectVisualizationConfig(nextModel, workspaceRoot),
                defaultViews: defaultVisualizationViews(),
                resetPresetError: 'Failed to reset preset via LSP project config endpoint.',
                writeViewsError:
                    'Failed to reset results-panel configuration via LSP project config endpoint.',
            });
            await storeCodegenSettings(context, resetCodegen);
            return {
                ...resetState,
                codegen: resetCodegen,
            };
        },
        pickSourceRootPath: async () => {
            const picked = await vscode.window.showOpenDialog({
                canSelectFiles: false,
                canSelectFolders: true,
                canSelectMany: false,
                openLabel: 'Select Source Root Folder',
            });
            return picked && picked.length > 0 ? picked[0].fsPath : undefined;
        },
        resyncSidecars: async () => {
            const report = await resyncProjectSidecars(workspaceRoot, {
                reason: 'settings-button',
                pruneOrphans: false,
            });
            if (!report) {
                throw new Error('Failed to resync sidecars via LSP endpoint.');
            }
            return report;
        },
        prepareModels: async () => {
            const sourceDocument = vscode.workspace.textDocuments.find(
                (document) => document.uri.toString() === sourceDocumentUri,
            );
            if (sourceDocument && (sourceDocument.isUntitled || sourceDocument.isDirty)) {
                const saved = await sourceDocument.save();
                if (!saved) {
                    throw new Error('Save the Modelica file before preparing simulation models.');
                }
            }
            const latestModelState = await getSimulationModelState(
                sourceDocumentUri,
                preferredModel ?? fallbackDefaults.model,
            );
            if (!latestModelState.ok || latestModelState.models.length === 0) {
                throw new Error(
                    latestModelState.error
                        ?? 'No model/block/class declarations were found in the active file.',
                );
            }
            const prepareDefaults = getSimulationSettings(vscode.workspace.getConfiguration('rumoca'));
            const result = await vscode.window.withProgress(
                {
                    location: vscode.ProgressLocation.Notification,
                    title: `Preparing ${latestModelState.models.length} model${latestModelState.models.length === 1 ? '' : 's'} for simulation...`,
                    cancellable: false,
                },
                async () =>
                    prepareRumocaSimulationModels(
                        sourceDocumentUri,
                        latestModelState.models,
                        prepareDefaults,
                    ),
            );
            if (!result.ok) {
                throw new Error(result.error ?? 'Failed to prepare simulation models.');
            }
            return {
                ...result,
                totalModels: latestModelState.models.length,
            };
        },
        selectModel: async ({ model }: { model: string }) => {
            const selection = await setSelectedSimulationModel(
                sourceDocumentUri,
                model,
                fallbackDefaults.model,
            );
            if (!selection.ok || !selection.selectedModel) {
                throw new Error(selection.error ?? 'Selected model is not available in this file.');
            }
            return { model: selection.selectedModel };
        },
        afterOpenModel: ({ model }: { model: string }) => {
            setTimeout(() => {
                void (async () => {
                    panel.dispose();
                    await showSimulationSettingsPanel(context, workspaceRoot, sourceDocumentUri, model);
                })();
            }, 0);
        },
        openWorkspaceSettings: async () => {
            await vscode.commands.executeCommand('workbench.action.openWorkspaceSettings', 'rumoca');
            return { ok: true };
        },
        openUserSettings: async () => {
            await vscode.commands.executeCommand('workbench.action.openSettings', 'rumoca');
            return { ok: true };
        },
        openViewScript: async ({ model, viewId }: { model: string; viewId: string }) => {
            if (!workspaceRoot) {
                throw new Error('Cannot open 3D script file without a workspace root.');
            }
            const resolvedViewId = viewId.trim() || 'viewer';
            const scriptPath = await resolvePreferredViewerScriptPath(
                workspaceRoot,
                model,
                resolvedViewId,
            );
            const absolutePath = path.isAbsolute(scriptPath)
                ? scriptPath
                : path.join(workspaceRoot, scriptPath);
            await fs.promises.mkdir(path.dirname(absolutePath), { recursive: true });
            try {
                await fs.promises.access(absolutePath, fs.constants.F_OK);
            } catch {
                await fs.promises.writeFile(
                    absolutePath,
                    defaultThreeDimensionalViewerScript(),
                    'utf-8',
                );
            }
            const document = await vscode.workspace.openTextDocument(absolutePath);
            await vscode.window.showTextDocument(document, { preview: false });
            return {
                path: path.isAbsolute(scriptPath)
                    ? scriptPath
                    : scriptPath.split(path.sep).join('/'),
            };
        },
    });

    panel.webview.onDidReceiveMessage(async (message) => {
        const handled = await loadVisualizationShared().handleHostedSimulationSettingsRequest({
            message,
            postMessage: async (response) => {
                await panel.webview.postMessage(response);
            },
            onError: ({ error }) => {
                void vscode.window.showErrorMessage(String(error));
            },
            handlers: settingsHandlers,
        });
        if (!handled) {
            return;
        }
    }, undefined, context.subscriptions);
}

/**
 * Parse magic directives from the first line(s) of a Modelica cell.
 *
 * Supported formats:
 *   // @rumoca model=ModelName output=/path/to/output.json lib=/path/to/lib1 lib=/path/to/lib2
 *   // @rumoca --model ModelName --output /path/to/output.json -l /path/to/lib1 -l /path/to/lib2
 *
 * Or multiline:
 *   // @rumoca model=Test
 *   // @rumoca lib=/path/to/MSL
 *   // @rumoca output=model.json
 */
interface CellMagic {
    model?: string;
    output?: string;
    libs: string[];
    code: string;  // The code without magic lines
}

function parseCellMagic(code: string): CellMagic {
    const lines = code.split('\n');
    const magic: CellMagic = { libs: [], code: '' };
    const codeLines: string[] = [];

    for (const line of lines) {
        const trimmed = line.trim();

        // Check for magic comment: // @rumoca ...
        if (trimmed.startsWith('// @rumoca') || trimmed.startsWith('//@rumoca')) {
            const directive = trimmed.replace(/^\/\/\s*@rumoca\s*/, '');

            // Parse key=value or --key value pairs
            const modelMatch = directive.match(/(?:--)?model[=\s]+(\S+)/);
            if (modelMatch) magic.model = modelMatch[1];

            const outputMatch = directive.match(/(?:--)?output[=\s]+(\S+)/);
            if (outputMatch) magic.output = outputMatch[1];

            // Multiple libs can be specified
            const libMatches = directive.matchAll(/(?:--)?lib[=\s]+(\S+)/g);
            for (const match of libMatches) {
                magic.libs.push(match[1]);
            }
        } else {
            codeLines.push(line);
        }
    }

    magic.code = codeLines.join('\n');

    // Auto-detect model name if not specified
    if (!magic.model) {
        const modelMatch = magic.code.match(/\b(?:model|block|connector|record|type|package|function|class)\s+(\w+)/);
        if (modelMatch) {
            magic.model = modelMatch[1];
        }
    }

    return magic;
}

/**
 * Execute Modelica code using rumoca and return the result.
 * The output format can be used in subsequent Python cells.
 */
async function executeModelicaCell(
    code: string,
    rumocaPath: string,
    globalLibPaths: string[]
): Promise<{ success: boolean; output: string; error?: string; outputFile?: string; model?: string }> {
    return new Promise((resolve) => {
        const magic = parseCellMagic(code);

        if (!magic.model) {
            resolve({
                success: false,
                output: '',
                error: 'No model name specified. Add a magic comment like:\n// @rumoca model=MyModel\n\nOr define a model/block/class in the cell.'
            });
            return;
        }

        // Create a temporary file for the Modelica code
        const tmpDir = os.tmpdir();
        const tmpFile = path.join(tmpDir, `rumoca_cell_${Date.now()}.mo`);

        try {
            fs.writeFileSync(tmpFile, magic.code);

            // Build rumoca arguments
            const args = ['--json', '--model', magic.model];

            // Add source-root paths (cell-specific + global config)
            const allLibs = [...magic.libs, ...globalLibPaths];
            for (const lib of allLibs) {
                args.push('-L', lib);
            }

            args.push(tmpFile);

            const proc = spawn(rumocaPath, args);

            let stdout = '';
            let stderr = '';

            proc.stdout.on('data', (data: Buffer) => {
                stdout += data.toString();
            });

            proc.stderr.on('data', (data: Buffer) => {
                stderr += data.toString();
            });

            proc.on('close', (exitCode: number) => {
                // Clean up temp file
                try { fs.unlinkSync(tmpFile); } catch { /* ignore */ }

                if (exitCode === 0) {
                    // Try to parse as JSON and format nicely
                    try {
                        const parsed = JSON.parse(stdout);
                        const jsonOutput = JSON.stringify(parsed, null, 2);

                        // Write to output file if specified
                        if (magic.output) {
                            try {
                                fs.writeFileSync(magic.output, jsonOutput);
                            } catch (writeErr) {
                                resolve({
                                    success: false,
                                    output: '',
                                    error: `Failed to write output file ${magic.output}: ${writeErr}`
                                });
                                return;
                            }
                        }

                        resolve({
                            success: true,
                            output: jsonOutput,
                            outputFile: magic.output,
                            model: magic.model
                        });
                    } catch {
                        // Not valid JSON, return raw output
                        resolve({
                            success: true,
                            output: stdout || 'Model compiled successfully.',
                            model: magic.model
                        });
                    }
                } else {
                    resolve({
                        success: false,
                        output: '',
                        error: stderr || stdout || `rumoca exited with code ${exitCode}`
                    });
                }
            });

            proc.on('error', (err: Error) => {
                // Clean up temp file
                try { fs.unlinkSync(tmpFile); } catch { /* ignore */ }
                resolve({
                    success: false,
                    output: '',
                    error: `Failed to execute rumoca: ${err.message}`
                });
            });
        } catch (err) {
            // Clean up temp file if it exists
            try { fs.unlinkSync(tmpFile); } catch { /* ignore */ }
            resolve({
                success: false,
                output: '',
                error: `Failed to create temp file: ${err}`
            });
        }
    });
}

/**
 * Create the notebook controller for Modelica cells in Jupyter notebooks.
 */
function createNotebookController(
    context: vscode.ExtensionContext,
    getRumocaPath: () => string | undefined,
    getGlobalLibPaths: () => string[],
    log: (msg: string) => void
): vscode.NotebookController {
    const controller = vscode.notebooks.createNotebookController(
        'rumoca-modelica-controller',
        'jupyter-notebook',
        'Rumoca Modelica'
    );

    controller.supportedLanguages = ['modelica'];
    controller.supportsExecutionOrder = true;
    controller.description = 'Execute Modelica code using Rumoca compiler';

    let executionOrder = 0;

    controller.executeHandler = async (
        cells: vscode.NotebookCell[],
        _notebook: vscode.NotebookDocument,
        _controller: vscode.NotebookController
    ) => {
        for (const cell of cells) {
            const execution = controller.createNotebookCellExecution(cell);
            execution.executionOrder = ++executionOrder;
            execution.start(Date.now());

            const code = cell.document.getText();
            log(`Executing Modelica cell: ${code.substring(0, 100)}...`);

            try {
                const rumocaPath = getRumocaPath();
                if (!rumocaPath) {
                    throw new Error('Rumoca executable is not available for notebook execution.');
                }
                const result = await executeModelicaCell(code, rumocaPath, getGlobalLibPaths());

                if (result.success) {
                    const modelName = result.model || 'model';
                    const outputFile = result.outputFile;

                    const snippet = buildNotebookPythonSnippet(modelName, code, outputFile);

                    execution.replaceOutput([
                        new vscode.NotebookCellOutput([
                            vscode.NotebookCellOutputItem.text(snippet.summaryText, 'text/plain')
                        ]),
                        new vscode.NotebookCellOutput([
                            vscode.NotebookCellOutputItem.text(snippet.pythonCode, 'text/x-python')
                        ])
                    ]);
                    execution.end(true, Date.now());
                } else {
                    execution.replaceOutput([
                        new vscode.NotebookCellOutput([
                            vscode.NotebookCellOutputItem.error(new Error(result.error || 'Unknown error'))
                        ])
                    ]);
                    execution.end(false, Date.now());
                }
            } catch (err) {
                execution.replaceOutput([
                    new vscode.NotebookCellOutput([
                        vscode.NotebookCellOutputItem.error(err instanceof Error ? err : new Error(String(err)))
                    ])
                ]);
                execution.end(false, Date.now());
            }
        }
    };

    context.subscriptions.push(controller);
    return controller;
}

export async function activate(context: vscode.ExtensionContext) {
    const startTime = Date.now();
    outputChannel = vscode.window.createOutputChannel('Rumoca Extension');

    let config = vscode.workspace.getConfiguration('rumoca');
    let debug = config.get<boolean>('debug') ?? false;

    const log = (msg: string) => {
        outputChannel.appendLine(msg);
        if (debug) console.log('[Rumoca]', msg);
    };
    const debugLog = (msg: string) => {
        if (debug) {
            outputChannel.appendLine(msg);
            console.log('[Rumoca]', msg);
        }
    };

    const refreshConfig = (): vscode.WorkspaceConfiguration => {
        config = vscode.workspace.getConfiguration('rumoca');
        debug = config.get<boolean>('debug') ?? false;
        if (debug) {
            outputChannel.show(true);
        }
        return config;
    };

    if (debug) {
        outputChannel.show(true); // Show output channel immediately when debugging
    }

    log('Activating Rumoca Modelica extension...');
    console.log('[Rumoca] Debug mode:', debug);
    debugLog(`[DEBUG] Workspace folders: ${vscode.workspace.workspaceFolders?.map(f => f.uri.fsPath).join(', ') || 'none'}`);

    const elapsed = () => `${Date.now() - startTime}ms`;

    const findSystemServer = (): string | undefined => {
        const pathResult = findInPath('rumoca-lsp');
        if (pathResult) {
            debugLog(`[${elapsed()}] Found rumoca-lsp in PATH: ${pathResult}`);
            return pathResult;
        }
        const cargoPath = path.join(process.env.HOME || '', '.cargo', 'bin', 'rumoca-lsp');
        if (fs.existsSync(cargoPath)) {
            debugLog(`[${elapsed()}] Found rumoca-lsp at cargo location: ${cargoPath}`);
            return cargoPath;
        }
        return undefined;
    };

    const openRumocaSetting = (setting: string) => {
        void vscode.commands.executeCommand('workbench.action.openSettings', setting);
    };

    const promptForMissingLanguageServer = async (): Promise<void> => {
        const installAction = 'Install with cargo';
        const msg = 'rumoca-lsp not found. Install it with: cargo install rumoca';
        log(`ERROR: ${msg}`);

        const selection = await vscode.window.showErrorMessage(msg, installAction, 'Configure Path');
        if (selection === installAction) {
            const terminal = vscode.window.createTerminal('Rumoca Install');
            terminal.show();
            terminal.sendText('cargo install rumoca');
        } else if (selection === 'Configure Path') {
            openRumocaSetting('rumoca.serverPath');
        }
    };

    const resolveLanguageServerExecutable = async (
        currentConfig: vscode.WorkspaceConfiguration
    ): Promise<{ serverPath: string; usingBundledServer: boolean; usingSystemFallback: boolean } | undefined> => {
        const configuredServerPath = currentConfig.get<string>('serverPath');
        if (configuredServerPath) {
            debugLog(`[${elapsed()}] Using configured serverPath: ${configuredServerPath}`);
            return {
                serverPath: configuredServerPath,
                usingBundledServer: false,
                usingSystemFallback: false,
            };
        }

        if (currentConfig.get<boolean>('useSystemServer') ?? false) {
            debugLog(`[${elapsed()}] useSystemServer is enabled, searching for system rumoca-lsp...`);
            const systemServerPath = findSystemServer();
            if (!systemServerPath) {
                await promptForMissingLanguageServer();
                return undefined;
            }
            log(`Using system-installed rumoca-lsp: ${systemServerPath}`);
            return {
                serverPath: systemServerPath,
                usingBundledServer: false,
                usingSystemFallback: false,
            };
        }

        debugLog(`[${elapsed()}] Searching for rumoca-lsp...`);
        const binaryName = process.platform === 'win32' ? 'rumoca-lsp.exe' : 'rumoca-lsp';
        const bundledPath = path.join(context.extensionPath, 'bin', binaryName);
        debugLog(`[${elapsed()}] Checking for bundled binary: ${bundledPath}`);
        if (fs.existsSync(bundledPath)) {
            log('Using bundled rumoca-lsp');
            debugLog(`[${elapsed()}] Found bundled rumoca-lsp: ${bundledPath}`);
            return {
                serverPath: bundledPath,
                usingBundledServer: true,
                usingSystemFallback: false,
            };
        }

        debugLog(`[${elapsed()}] No bundled binary, searching for system rumoca-lsp...`);
        const fallbackServerPath = findSystemServer();
        if (!fallbackServerPath) {
            await promptForMissingLanguageServer();
            return undefined;
        }
        return {
            serverPath: fallbackServerPath,
            usingBundledServer: false,
            usingSystemFallback: true,
        };
    };

    const showSystemFallbackWarning = (resolvedServerPath: string) => {
        log(`Warning: Using system-installed rumoca-lsp: ${resolvedServerPath}`);
        log('The bundled binary was not found. This may indicate a platform mismatch.');
        vscode.window.showWarningMessage(
            'Using system-installed rumoca-lsp. Set "rumoca.useSystemServer": true to suppress this warning.',
            'Open Settings'
        ).then(selection => {
            if (selection === 'Open Settings') {
                openRumocaSetting('rumoca.useSystemServer');
            }
        });
    };

    const validateLanguageServerExecutable = async (
        resolvedServerPath: string,
        usingBundledServer: boolean
    ): Promise<string | undefined> => {
        debugLog(`[${elapsed()}] Verifying server binary exists...`);
        if (!fs.existsSync(resolvedServerPath)) {
            const msg = `rumoca-lsp not found at: ${resolvedServerPath}`;
            log(`ERROR: ${msg}`);
            vscode.window.showErrorMessage(msg);
            return undefined;
        }

        let usableServerPath = resolvedServerPath;
        let probeResult = probeServerExecutable(usableServerPath);
        if (!probeResult.ok && usingBundledServer) {
            const fallbackServerPath = findSystemServer();
            if (fallbackServerPath && fallbackServerPath !== usableServerPath) {
                const fallbackProbeResult = probeServerExecutable(fallbackServerPath);
                if (fallbackProbeResult.ok) {
                    usableServerPath = fallbackServerPath;
                    probeResult = fallbackProbeResult;
                    log(`Warning: Bundled rumoca-lsp could not execute; falling back to system server: ${usableServerPath}`);
                    vscode.window.showWarningMessage(
                        'Bundled rumoca-lsp could not execute on this machine. Falling back to the system-installed server.',
                        'Open Settings'
                    ).then(selection => {
                        if (selection === 'Open Settings') {
                            openRumocaSetting('rumoca.useSystemServer');
                        }
                    });
                } else {
                    const fallbackDetail = fallbackProbeResult.detail ?? 'unknown error';
                    log(`ERROR: System rumoca-lsp fallback also failed at ${fallbackServerPath}: ${fallbackDetail}`);
                }
            }
        }

        if (probeResult.ok) {
            return usableServerPath;
        }

        const probeDetail = probeResult.detail ?? 'unknown error';
        const msg = `Failed to execute rumoca-lsp: ${probeDetail}`;
        log(`ERROR: ${msg}`);
        outputChannel.show();
        const selection = await vscode.window.showErrorMessage(msg, 'Open Settings', 'Configure Path');
        if (selection === 'Open Settings') {
            openRumocaSetting('rumoca.useSystemServer');
        } else if (selection === 'Configure Path') {
            openRumocaSetting('rumoca.serverPath');
        }
        return undefined;
    };

    const startLanguageClient = async (): Promise<StartedLanguageClient> => {
        const currentConfig = refreshConfig();
        const resolvedServer = await resolveLanguageServerExecutable(currentConfig);
        if (!resolvedServer) {
            return { clientStarted: false };
        }
        if (resolvedServer.usingSystemFallback) {
            showSystemFallbackWarning(resolvedServer.serverPath);
        }

        const usableServerPath = await validateLanguageServerExecutable(
            resolvedServer.serverPath,
            resolvedServer.usingBundledServer,
        );
        if (!usableServerPath) {
            return { clientStarted: false };
        }

        debugLog(`[${elapsed()}] Starting language server: ${usableServerPath}`);
        const sourceRootPaths = resolveSourceRootPaths(currentConfig);
        if (sourceRootPaths.configuredPaths.length > 0) {
            debugLog(`[${elapsed()}] Configured sourceRootPaths: ${sourceRootPaths.configuredPaths.join(', ')}`);
        }
        if (sourceRootPaths.environmentPaths.length > 0) {
            debugLog(`[${elapsed()}] Environment MODELICAPATH: ${sourceRootPaths.environmentPaths.join(', ')}`);
        }

        const nextClient = new LanguageClient(
            'rumoca',
            'Rumoca LSP',
            {
                run: { command: usableServerPath, transport: TransportKind.stdio },
                debug: { command: usableServerPath, transport: TransportKind.stdio }
            },
            {
                documentSelector: [
                    { scheme: 'file', language: 'modelica' },
                    { scheme: 'vscode-notebook-cell', language: 'modelica' },
                    { scheme: EMBEDDED_MODELICA_SCHEME, language: 'modelica' }
                ],
                outputChannelName: 'Rumoca LSP',
                initializationOptions: {
                    debug: debug,
                    sourceRootPaths: sourceRootPaths.mergedPaths
                }
            } satisfies LanguageClientOptions
        );

        client = nextClient;
        wireSimulationJobNotifications(nextClient);
        try {
            debugLog(`[${elapsed()}] Calling client.start() - this launches the server and waits for initialization...`);
            debugLog(`[${elapsed()}] If stuck here, the language server may be scanning workspace files...`);
            await nextClient.start();
            await reopenEmbeddedModelicaDocuments();
            debugLog(`[${elapsed()}] Language server started successfully`);
            return {
                clientStarted: true,
                serverPath: usableServerPath,
            };
        } catch (error) {
            if (client === nextClient) {
                client = undefined;
            }
            const msg = `Failed to start language server: ${error}`;
            log(`ERROR: ${msg}`);
            outputChannel.show();
            vscode.window.showErrorMessage(msg);
            return { clientStarted: false };
        }
    };

    const languageClientRuntime = createLanguageClientRuntime<LanguageClient>({
        getClient: () => client,
        setClient: (nextClient) => {
            client = nextClient;
            builtInCodegenTemplatesCache = undefined;
        },
        startLanguageClient,
        stopLanguageClient: async (existingClient) => {
            await existingClient.stop();
        },
        log,
        reportError: (msg) => {
            log(`ERROR: ${msg}`);
            outputChannel.show();
            vscode.window.showErrorMessage(msg);
        },
    });

    const initialLanguageClient = await startLanguageClient();
    if (initialLanguageClient.clientStarted) {
        languageClientRuntime.setServerPath(initialLanguageClient.serverPath);
    } else {
        log('Continuing activation without a running language server so commands remain available.');
    }

    const wireResultsPanelMessageHandling = (panel: vscode.WebviewPanel) => {
        const messageDisposable = panel.webview.onDidReceiveMessage(async (message) => {
            await loadVisualizationShared().handleHostedResultsRequest({
                message,
                fallbackWorkspaceRoot: () => resolveWorkspaceRootFallback(),
                postMessage: async (response) => {
                    await panel.webview.postMessage(response);
                },
                onError: ({ method, error }) => {
                    const detail = String(error instanceof Error ? error.message : error);
                    if (method === 'savePng') {
                        vscode.window.showErrorMessage(`Save PNG failed: ${detail}`);
                    } else if (method === 'saveWebm') {
                        vscode.window.showErrorMessage(`Export movie failed: ${detail}`);
                    }
                },
                handlers: {
                    loadViews: async ({ modelRef }) => {
                        return await loadVisualizationShared().loadHostedProjectResultsViews({
                            model: modelRef?.model ?? '',
                            workspaceRoot: modelRef?.workspaceRoot,
                            loadConfiguredViews: async ({ model, workspaceRoot }) =>
                                await getProjectVisualizationConfig(model, workspaceRoot),
                            hydrateViews: async ({ model, workspaceRoot, views }) =>
                                await buildWorkspaceVisualizationViewStorage(workspaceRoot)
                                    .hydrateViews({
                                        views,
                                        model,
                                    }),
                            defaultViews: defaultVisualizationViews(),
                        });
                    },
                    saveViews: async ({ modelRef, payload }) => {
                        const rawPayload = payload && typeof payload === 'object'
                            ? payload as Record<string, unknown>
                            : {};
                        return await loadVisualizationShared().saveHostedProjectResultsViews({
                            model: modelRef?.model ?? '',
                            workspaceRoot: modelRef?.workspaceRoot,
                            views: normalizeVisualizationViews(rawPayload.views),
                            loadConfiguredViews: async ({ model, workspaceRoot }) =>
                                await getProjectVisualizationConfig(model, workspaceRoot),
                            persistViews: async ({ model, workspaceRoot, views }) =>
                                await buildWorkspaceVisualizationViewStorage(workspaceRoot)
                                    .persistViews({
                                        views,
                                        model,
                                    }),
                            writeConfiguredViews: async ({ model, workspaceRoot, views }) =>
                                await setProjectVisualizationConfig(model, workspaceRoot, views),
                            hydrateViews: async ({ model, workspaceRoot, views }) =>
                                await buildWorkspaceVisualizationViewStorage(workspaceRoot)
                                    .hydrateViews({
                                        views,
                                        model,
                                    }),
                            writeViewsError: 'Failed to save visualization settings.',
                        });
                    },
                    resetViews: async ({ modelRef }) => {
                        return await loadVisualizationShared().resetHostedProjectResultsViews({
                            model: modelRef?.model ?? '',
                            workspaceRoot: modelRef?.workspaceRoot,
                            loadConfiguredViews: async ({ model, workspaceRoot }) =>
                                await getProjectVisualizationConfig(model, workspaceRoot),
                            writeConfiguredViews: async ({ model, workspaceRoot, views }) =>
                                await setProjectVisualizationConfig(model, workspaceRoot, views),
                            hydrateViews: async ({ model, workspaceRoot, views }) =>
                                await buildWorkspaceVisualizationViewStorage(workspaceRoot)
                                    .hydrateViews({
                                        views,
                                        model,
                                    }),
                            defaultViews: defaultVisualizationViews(),
                            writeViewsError: 'Failed to reset visualization settings.',
                        });
                    },
                    savePng: async ({ modelRef, payload }) => {
                        const workspaceDir = modelRef?.workspaceRoot ?? resolveWorkspaceRootFallback();
                        const exportRequest = loadVisualizationShared().normalizeHostedPngExportRequest(payload);
                        const defaultUri = workspaceDir
                            ? vscode.Uri.file(path.join(workspaceDir, exportRequest.defaultName))
                            : undefined;
                        const targetUri = await vscode.window.showSaveDialog({
                            saveLabel: 'Save Plot PNG',
                            defaultUri,
                            filters: {
                                'PNG Image': ['png'],
                            },
                        });
                        if (!targetUri) {
                            return { cancelled: true };
                        }
                        const bytes = Buffer.from(exportRequest.base64, 'base64');
                        await vscode.workspace.fs.writeFile(targetUri, new Uint8Array(bytes));
                        log(`Saved plot PNG: ${targetUri.fsPath}`);
                        return { saved: true };
                    },
                    saveWebm: async ({ modelRef, payload }) => {
                        const workspaceDir = modelRef?.workspaceRoot ?? resolveWorkspaceRootFallback();
                        const exportRequest = loadVisualizationShared().normalizeHostedWebmExportRequest(payload);
                        const defaultUri = workspaceDir
                            ? vscode.Uri.file(path.join(workspaceDir, exportRequest.defaultName))
                            : undefined;
                        const targetUri = await vscode.window.showSaveDialog({
                            saveLabel: 'Export Movie',
                            defaultUri,
                            filters: {
                                'WebM Video': ['webm'],
                            },
                        });
                        if (!targetUri) {
                            return { cancelled: true };
                        }
                        const bytes = Buffer.from(exportRequest.base64, 'base64');
                        await vscode.workspace.fs.writeFile(targetUri, new Uint8Array(bytes));
                        log(`Saved viewer movie: ${targetUri.fsPath}`);
                        return { saved: true };
                    },
                    notify: ({ payload }) => {
                        const detail = loadVisualizationShared().normalizeHostedResultsNotifyPayload(payload).message;
                        if (detail.length > 0) {
                            log(`results: ${detail}`);
                        }
                        return { logged: true };
                    },
                },
            });
        });
        panel.onDidDispose(() => {
            messageDisposable.dispose();
        });
    };

    const resultsWebviewLocalRoots = [
        vscode.Uri.joinPath(context.extensionUri, 'media', 'vendor'),
    ];

    const resultsWebviewOptions = (): vscode.WebviewOptions & vscode.WebviewPanelOptions => ({
        enableScripts: true,
        retainContextWhenHidden: true,
        localResourceRoots: resultsWebviewLocalRoots,
    });

    const getResultsWebviewAssets = (webview: vscode.Webview): ResultsWebviewAssets => {
        const vendorRoot = vscode.Uri.joinPath(context.extensionUri, 'media', 'vendor');
        return {
            uplotCss: webview.asWebviewUri(vscode.Uri.joinPath(vendorRoot, 'uPlot.min.css')).toString(),
            uplotJs: webview.asWebviewUri(vscode.Uri.joinPath(vendorRoot, 'uPlot.iife.min.js')).toString(),
            threeJs: webview.asWebviewUri(vscode.Uri.joinPath(vendorRoot, 'three.min.js')).toString(),
            visualizationSharedJs: webview.asWebviewUri(vscode.Uri.joinPath(vendorRoot, 'visualization_shared.js')).toString(),
            resultsAppJs: webview.asWebviewUri(vscode.Uri.joinPath(vendorRoot, 'results_app.js')).toString(),
            resultsAppCss: webview.asWebviewUri(vscode.Uri.joinPath(vendorRoot, 'results_app.css')).toString(),
        };
    };

    const openResultsPanelForRun = async (
        model: string,
        payload: ParsedSimulationPayload | undefined,
        views: VisualizationView[],
        metrics: SimulationRunMetrics | undefined,
        workspaceRoot: string | undefined,
        runId: string | undefined,
        title: string,
        column: vscode.ViewColumn = vscode.ViewColumn.Beside,
    ): Promise<vscode.WebviewPanel> => {
        const panelTitle = loadVisualizationShared().buildHostedResultsPanelTitle({ model, title });
        const panel = vscode.window.createWebviewPanel(
            'rumocaResults',
            panelTitle,
            column,
            resultsWebviewOptions()
        );
        wireResultsPanelMessageHandling(panel);
        const state = loadVisualizationShared().buildHostedResultsPanelState({
            runId,
            model,
            workspaceRoot,
            title: panelTitle,
        });
        panel.webview.html = buildResultsWebviewHtml(
            model,
            payload,
            views,
            metrics,
            state,
            getResultsWebviewAssets(panel.webview),
        );
        return panel;
    };

    context.subscriptions.push(
        vscode.window.registerWebviewPanelSerializer('rumocaResults', {
            async deserializeWebviewPanel(webviewPanel: vscode.WebviewPanel, state: unknown) {
                webviewPanel.webview.options = resultsWebviewOptions();
                wireResultsPanelMessageHandling(webviewPanel);
                const restoredState = loadVisualizationShared().normalizeHostedResultsPanelState(
                    state,
                    resolveWorkspaceRootFallback(),
                );
                if (!restoredState) {
                    webviewPanel.title = loadVisualizationShared().buildHostedResultsPanelTitle({
                        unavailable: true,
                    });
                    webviewPanel.webview.html = buildResultsWebviewHtml(
                        'Unavailable',
                        undefined,
                        defaultVisualizationViews(),
                        undefined,
                        undefined,
                        getResultsWebviewAssets(webviewPanel.webview),
                    );
                    return;
                }
                const { runId, model, workspaceRoot } = restoredState;
                const restored = workspaceRoot
                    ? await loadVisualizationShared().loadHostedSimulationRunWithViews({
                        model,
                        runId,
                        workspaceRoot,
                        readTextFile: async (relativeRunPath) => {
                            const absPath = path.join(workspaceRoot, ...relativeRunPath.split('/'));
                            return await fs.promises.readFile(absPath, 'utf-8');
                        },
                        loadConfiguredViews: async ({ model, workspaceRoot }) =>
                            await getProjectVisualizationConfig(model, workspaceRoot),
                        hydrateViews: async ({ model, workspaceRoot, views }) =>
                            await buildWorkspaceVisualizationViewStorage(workspaceRoot).hydrateViews({
                                views,
                                model,
                            }),
                        defaultViews: defaultVisualizationViews(),
                    })
                    : undefined;
                const persisted = restored?.run as PersistedSimulationRun | undefined;
                if (!persisted || !persisted.payload) {
                    webviewPanel.title = loadVisualizationShared().buildHostedResultsPanelTitle({
                        model,
                        missingRun: true,
                    });
                    webviewPanel.webview.html = buildResultsWebviewHtml(
                        model,
                        undefined,
                        defaultVisualizationViews(),
                        undefined,
                        undefined,
                        getResultsWebviewAssets(webviewPanel.webview),
                    );
                    return;
                }
                const restoredTitle = loadVisualizationShared().buildHostedResultsPanelTitle({
                    model,
                    title: restoredState.title,
                });
                webviewPanel.title = restoredTitle;
                const nextState = loadVisualizationShared().buildHostedResultsPanelState({
                    runId,
                    model,
                    workspaceRoot,
                    title: restoredTitle,
                    activeViewId: restoredState.activeViewId,
                });
                webviewPanel.webview.html = buildResultsWebviewHtml(
                    model,
                    persisted.payload,
                    restored?.views ?? defaultVisualizationViews(),
                    persisted.metrics,
                    nextState,
                    getResultsWebviewAssets(webviewPanel.webview),
                );
            },
        }),
    );

    let codegenOutputPanel: vscode.WebviewPanel | undefined;
    let codegenOutputState: CodegenOutputState | undefined;

    const currentModelicaEditor = (): vscode.TextEditor | undefined => {
        const editor = vscode.window.activeTextEditor;
        if (!editor || editor.document.languageId !== 'modelica') {
            return undefined;
        }
        return editor;
    };

    const showCodegenOutputPanel = async (
        state: CodegenOutputState,
        column: vscode.ViewColumn = vscode.ViewColumn.Beside,
    ): Promise<void> => {
        codegenOutputState = state;
        if (!codegenOutputPanel) {
            const panel = vscode.window.createWebviewPanel(
                'rumocaTemplateOutput',
                `Rumoca Template: ${state.model}`,
                column,
                { enableScripts: true, retainContextWhenHidden: true },
            );
            panel.webview.onDidReceiveMessage(async (message) => {
                if (message?.command !== 'save' || !codegenOutputState) {
                    return;
                }
                try {
                    const defaultUri = codegenOutputState.workspaceRoot
                        ? vscode.Uri.file(
                            path.join(
                                codegenOutputState.workspaceRoot,
                                codegenOutputState.suggestedFileName,
                            ),
                        )
                        : undefined;
                    const targetUri = await vscode.window.showSaveDialog({
                        saveLabel: 'Save Rendered Output',
                        defaultUri,
                        filters: codegenSaveFilters(
                            codegenOutputState.templateLabel,
                            codegenOutputState.language,
                        ),
                    });
                    if (!targetUri) {
                        return;
                    }
                    const bytes = Buffer.from(codegenOutputState.output, 'utf8');
                    await vscode.workspace.fs.writeFile(targetUri, new Uint8Array(bytes));
                    const savedDocument = await vscode.workspace.openTextDocument(targetUri);
                    await vscode.languages.setTextDocumentLanguage(
                        savedDocument,
                        normalizeCodegenLanguageId(codegenOutputState.language),
                    );
                    await vscode.window.showTextDocument(savedDocument, {
                        preview: false,
                        viewColumn: vscode.ViewColumn.Beside,
                    });
                    log(`Saved rendered template output: ${targetUri.fsPath}`);
                } catch (error) {
                    const detail = error instanceof Error ? error.message : String(error);
                    vscode.window.showErrorMessage(`Failed to save rendered output: ${detail}`);
                }
            });
            panel.onDidDispose(() => {
                if (codegenOutputPanel === panel) {
                    codegenOutputPanel = undefined;
                    codegenOutputState = undefined;
                }
            });
            codegenOutputPanel = panel;
        } else {
            codegenOutputPanel.reveal(column, true);
        }

        codegenOutputPanel.title = `Rumoca Template: ${state.model}`;
        codegenOutputPanel.webview.html = buildCodegenOutputWebviewHtml(state);
    };

    const openUnifiedSettingsForEditor = async (editor: vscode.TextEditor): Promise<void> => {
        const defaults = getSimulationSettings(vscode.workspace.getConfiguration('rumoca'));
        const workspaceRoot = resolveWorkspaceRootForDocument(editor.document) ?? resolveWorkspaceRootFallback();
        await showSimulationSettingsPanel(
            context,
            workspaceRoot,
            editor.document.uri.toString(),
            defaults.model,
        );
    };

    const renderTemplateCommand = vscode.commands.registerCommand('rumoca.renderTemplate', async () => {
        const editor = currentModelicaEditor();
        if (!editor) {
            vscode.window.showErrorMessage('No active Modelica editor.');
            return;
        }

        const document = editor.document;
        if (document.isUntitled || document.isDirty) {
            const saved = await document.save();
            if (!saved) {
                vscode.window.showWarningMessage('Save the Modelica file before rendering a template.');
                return;
            }
        }

        const defaults = getSimulationSettings(vscode.workspace.getConfiguration('rumoca'));
        const modelState = await getSimulationModelState(document.uri.toString(), defaults.model);
        if (!modelState.ok) {
            vscode.window.showErrorMessage(
                modelState.error ?? 'Failed to enumerate template render targets for the active file.',
            );
            return;
        }
        const model = modelState.selectedModel;
        if (!model) {
            vscode.window.showErrorMessage(
                'Could not infer a model name from the active file. Select a model in simulation settings first.',
            );
            return;
        }

        const workspaceRoot = resolveWorkspaceRootForDocument(document) ?? resolveWorkspaceRootFallback();
        let templateSelection: ResolvedCodegenTemplate;
        try {
            templateSelection = await resolveCodegenTemplateSelection(context, document, workspaceRoot);
        } catch (error) {
            const detail = error instanceof Error ? error.message : String(error);
            vscode.window.showErrorMessage(detail);
            return;
        }

        try {
            const response = await vscode.window.withProgress(
                {
                    location: vscode.ProgressLocation.Notification,
                    title: `Rendering ${model} with ${templateSelection.label}...`,
                    cancellable: false,
                },
                async () =>
                    await sendWorkspaceCommand<CodegenRenderResponse>(
                        'rumoca.workspace.renderTemplate',
                        {
                            uri: document.uri.toString(),
                            model,
                            template: templateSelection.source,
                        },
                    ),
            );
            if (!response?.ok || typeof response.output !== 'string') {
                throw new Error(response?.error ?? 'rumoca-lsp did not return rendered template output.');
            }
            await showCodegenOutputPanel({
                model,
                templateLabel: templateSelection.label,
                language: templateSelection.language,
                output: response.output,
                workspaceRoot,
                suggestedFileName: suggestedCodegenFileName(
                    model,
                    templateSelection.label,
                    templateSelection.language,
                ),
            });
            log(`Rendered template for ${model}: ${templateSelection.label}`);
        } catch (error) {
            const detail = error instanceof Error ? error.message : String(error);
            log(`Template render failed for ${model}: ${detail}`);
            vscode.window.showErrorMessage(`Template render failed: ${detail}`);
        }
    });
    context.subscriptions.push(renderTemplateCommand);

    const simulateCommand = vscode.commands.registerCommand('rumoca.simulateModel', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor || editor.document.languageId !== 'modelica') {
            vscode.window.showErrorMessage('No active Modelica editor.');
            return;
        }

        const document = editor.document;
        if (document.isUntitled || document.isDirty) {
            const saved = await document.save();
            if (!saved) {
                vscode.window.showWarningMessage('Save the Modelica file before simulation.');
                return;
            }
        }

        const runConfig = vscode.workspace.getConfiguration('rumoca');
        const defaults = getSimulationSettings(runConfig);
        const modelState = await getSimulationModelState(document.uri.toString(), defaults.model);
        if (!modelState.ok) {
            vscode.window.showErrorMessage(
                modelState.error ?? 'Failed to enumerate simulation models for the active file.',
            );
            return;
        }
        const model = modelState.selectedModel;
        if (!model) {
            vscode.window.showErrorMessage(
                'Could not infer a model name from the active file. Select a model in simulation settings or set "Model Override" in simulation settings.',
            );
            return;
        }
        const workspaceRoot = resolveWorkspaceRootForDocument(document) ?? resolveWorkspaceRootFallback();
        // Sidecar maintenance already has explicit and watcher-driven paths.
        // Blocking every simulation on a full resync adds wall time that is
        // outside the reported rumoca-lsp compile/sim metrics.
        const projectConfig = await getProjectSimulationConfig(model, workspaceRoot, defaults);
        const settings = projectConfig?.effective ?? defaults;
        for (const diagnostic of projectConfig?.diagnostics ?? []) {
            log(`project config warning: ${diagnostic}`);
        }

        try {
            const result = await vscode.window.withProgress(
                {
                    location: vscode.ProgressLocation.Notification,
                    title: `Simulating ${model}...`,
                    cancellable: false,
                },
                async (progress) =>
                    runRumocaSimulation(document.uri.toString(), model, settings, (message) =>
                        progress.report({ message })
                    ),
            );

            if (result.exitCode !== 0) {
                const details = (result.stderr || `rumoca exited with code ${result.exitCode}`).trim();
                log(`Simulation failed for ${model}: ${details}`);
                vscode.window.showErrorMessage(`Simulation failed: ${details}`);
                return;
            }

            const runId = workspaceRoot
                ? (await loadVisualizationShared().persistHostedSimulationRunWithViews({
                    model,
                    workspaceRoot,
                    payload: result.payload,
                    metrics: result.metrics,
                    loadConfiguredViews: async ({ model, workspaceRoot }) =>
                        await getProjectVisualizationConfig(model, workspaceRoot),
                    hydrateViews: async ({ model, workspaceRoot, views }) =>
                        await buildWorkspaceVisualizationViewStorage(workspaceRoot).hydrateViews({
                            views,
                            model,
                        }),
                    defaultViews: defaultVisualizationViews(),
                    pathExists: (relativeRunPath) =>
                        fs.existsSync(path.join(workspaceRoot, ...relativeRunPath.split('/'))),
                    writeTextFile: async (relativeRunPath, content) => {
                        const absPath = path.join(workspaceRoot, ...relativeRunPath.split('/'));
                        await fs.promises.mkdir(path.dirname(absPath), { recursive: true });
                        await fs.promises.writeFile(absPath, content, 'utf-8');
                    },
                    writeLastResultTextFile: async (relativeResultPath, content) => {
                        const absPath = path.join(workspaceRoot, ...relativeResultPath.split('/'));
                        await fs.promises.mkdir(path.dirname(absPath), { recursive: true });
                        await fs.promises.writeFile(absPath, content, 'utf-8');
                    },
                }))?.runId
                : undefined;
            const views = await loadHydratedVisualizationViews(model, workspaceRoot);

            const timestamp = new Date().toLocaleTimeString([], { hour12: false });
            await openResultsPanelForRun(
                model,
                result.payload,
                views,
                result.metrics,
                workspaceRoot,
                runId,
                loadVisualizationShared().buildHostedResultsPanelTitle({ model, timestamp }),
            );
            log(`Simulation completed for ${model}`);
        } catch (error) {
            const message = error instanceof Error ? error.message : String(error);
            log(`Simulation error: ${message}`);
            vscode.window.showErrorMessage(`Simulation failed: ${message}`);
        }
    });
    context.subscriptions.push(simulateCommand);

    const resyncSidecarsCommand = vscode.commands.registerCommand('rumoca.resyncSidecars', async () => {
        const workspaceRoot = resolveWorkspaceRootFallback();
        if (!workspaceRoot) {
            vscode.window.showErrorMessage('No workspace root is available for sidecar resync.');
            return;
        }
        const report = await vscode.window.withProgress(
            {
                location: vscode.ProgressLocation.Notification,
                title: 'Resyncing Rumoca sidecars...',
                cancellable: false,
            },
            async () =>
                resyncProjectSidecars(workspaceRoot, {
                    reason: 'manual-command',
                    pruneOrphans: false,
                }),
        );
        if (!report) {
            vscode.window.showErrorMessage('Failed to resync sidecars.');
            return;
        }
        vscode.window.showInformationMessage(
            `Resync complete: remapped=${report.remapped_models}, parseFailures=${report.parse_failures}, models=${report.discovered_models}`,
        );
    });
    context.subscriptions.push(resyncSidecarsCommand);

    const simulationSettingsCommand = vscode.commands.registerCommand(
        'rumoca.openSimulationSettings',
        async () => {
            const editor = currentModelicaEditor();
            if (!editor) {
                vscode.window.showErrorMessage('Open a Modelica file to configure Rumoca settings.');
                return;
            }
            await openUnifiedSettingsForEditor(editor);
        },
    );
    context.subscriptions.push(simulationSettingsCommand);

    const templateSettingsCommand = vscode.commands.registerCommand(
        'rumoca.openTemplateSettings',
        async () => {
            const editor = currentModelicaEditor();
            if (!editor) {
                vscode.window.showErrorMessage('Open a Modelica file to configure Rumoca settings.');
                return;
            }
            try {
                await openUnifiedSettingsForEditor(editor);
            } catch (error) {
                const detail = error instanceof Error ? error.message : String(error);
                vscode.window.showErrorMessage(`Failed to open Rumoca settings: ${detail}`);
            }
        },
    );
    context.subscriptions.push(templateSettingsCommand);

    const settingsMenuCommand = vscode.commands.registerCommand(
        'rumoca.openSettingsMenu',
        async () => {
            const editor = currentModelicaEditor();
            if (!editor) {
                vscode.window.showErrorMessage('Open a Modelica file to configure Rumoca settings.');
                return;
            }
            await openUnifiedSettingsForEditor(editor);
        },
    );
    context.subscriptions.push(settingsMenuCommand);

    let sidecarResyncTimer: NodeJS.Timeout | undefined;
    const runSidecarResyncNow = async (reason: string) => {
        const workspaceRoot = resolveWorkspaceRootFallback();
        if (!workspaceRoot) {
            return;
        }
        const report = await resyncProjectSidecars(workspaceRoot, {
            reason,
            pruneOrphans: false,
        });
        if (report) {
            log(
                `[resync] ${reason}: remapped=${report.remapped_models}, parseFailures=${report.parse_failures}, models=${report.discovered_models}`,
            );
        }
    };
    const scheduleSidecarResync = (reason: string, delayMs = 1000) => {
        if (sidecarResyncTimer) {
            clearTimeout(sidecarResyncTimer);
        }
        sidecarResyncTimer = setTimeout(async () => {
            await runSidecarResyncNow(reason);
        }, delayMs);
    };
    const isModelicaPath = (uri: vscode.Uri): boolean => uri.fsPath.toLowerCase().endsWith('.mo');
    context.subscriptions.push(
        vscode.workspace.onDidRenameFiles((event) => {
            if (event.files.some((item) => isModelicaPath(item.oldUri) || isModelicaPath(item.newUri))) {
                const renameFiles = event.files.map((item) => ({
                    oldPath: item.oldUri.fsPath,
                    newPath: item.newUri.fsPath,
                }));
                const workspaceRoot = resolveWorkspaceRootFallback();
                if (workspaceRoot) {
                    void notifyProjectFilesMoved(workspaceRoot, renameFiles).then((report) => {
                        if (!report) {
                            return;
                        }
                        log(
                            `[filesMoved] remapped=${report.remapped_models}, parseFailures=${report.parse_failures}, models=${report.discovered_models}`,
                        );
                    });
                }
                // Follow-up pass after filesystem settles.
                scheduleSidecarResync('rename-files-followup', 300);
            }
        }),
    );
    context.subscriptions.push(
        vscode.workspace.onDidCreateFiles((event) => {
            if (event.files.some((uri) => isModelicaPath(uri))) {
                scheduleSidecarResync('create-files');
            }
        }),
    );
    context.subscriptions.push(
        vscode.workspace.onDidDeleteFiles((event) => {
            if (event.files.some((uri) => isModelicaPath(uri))) {
                scheduleSidecarResync('delete-files');
            }
        }),
    );
    const modelicaFsWatcher = vscode.workspace.createFileSystemWatcher('**/*.mo');
    context.subscriptions.push(modelicaFsWatcher);
    context.subscriptions.push(
        modelicaFsWatcher.onDidCreate(() => {
            scheduleSidecarResync('fswatch-create');
        }),
    );
    context.subscriptions.push(
        modelicaFsWatcher.onDidDelete(() => {
            scheduleSidecarResync('fswatch-delete');
        }),
    );

    const getNotebookRumocaExecutable = () => languageClientRuntime.getNotebookExecutable();
    const notebookControllerRuntime = createNotebookControllerRuntime({
        refreshConfig,
        languageClientRuntime,
        fileExists: (targetPath) => fs.existsSync(targetPath),
        createNotebookController: () => createNotebookController(
            context,
            getNotebookRumocaExecutable,
            () => resolveSourceRootPaths(vscode.workspace.getConfiguration('rumoca')).mergedPaths,
            log
        ),
        debugLog,
    });
    context.subscriptions.push({
        dispose: () => notebookControllerRuntime.disposeNotebookController(),
    });
    notebookControllerRuntime.reconcileNotebookController();

    context.subscriptions.push(vscode.workspace.onDidChangeConfiguration((event) => {
        void notebookControllerRuntime.handleConfigurationChange((section) => event.affectsConfiguration(section));
    }));

    // ========================================================================
    // Register embedded Modelica support for %%modelica blocks in Python cells
    // ========================================================================

    // Register the virtual document provider
    embeddedModelicaProvider = new EmbeddedModelicaProvider();
    context.subscriptions.push(
        vscode.workspace.registerTextDocumentContentProvider(EMBEDDED_MODELICA_SCHEME, embeddedModelicaProvider)
    );
    debugLog(`[${elapsed()}] Registered embedded Modelica document provider`);

    // Listen for document changes to update Modelica blocks
    context.subscriptions.push(
        vscode.workspace.onDidChangeTextDocument(event => {
            updateModelicaBlocks(event.document);
        })
    );

    // Listen for document opens to initialize Modelica blocks
    context.subscriptions.push(
        vscode.workspace.onDidOpenTextDocument(document => {
            updateModelicaBlocks(document);
        })
    );

    // Initialize blocks for already open documents
    vscode.workspace.textDocuments.forEach(doc => {
        updateModelicaBlocks(doc);
    });

    // Register hover provider for Python cells that forwards to Modelica LSP
    context.subscriptions.push(
        vscode.languages.registerHoverProvider(
            { language: 'python', scheme: 'vscode-notebook-cell' },
            {
                async provideHover(document, position, _token) {
                    const cellUri = document.uri.toString();
                    log(`[Hover] Checking position ${position.line}:${position.character} in ${cellUri}`);

                    const blockInfo = findBlockAtPosition(cellUri, position);
                    if (!blockInfo) {
                        log(`[Hover] No modelica block found at position`);
                        return null;
                    }

                    const { block, index } = blockInfo;
                    log(`[Hover] Found block ${index}: lines ${block.startLine}-${block.endLine}`);

                    const virtualPos = cellToVirtualPosition(position, block);
                    if (!virtualPos) {
                        log(`[Hover] Position not in block content`);
                        return null;
                    }

                    // Get the virtual document URI
                    const virtualUri = getVirtualDocumentUri(cellUri, index);
                    log(`[Hover] Virtual pos: ${virtualPos.line}:${virtualPos.character}, URI: ${virtualUri.toString()}`);

                    // Request hover from the language client
                    if (!client) {
                        log(`[Hover] No language client`);
                        return null;
                    }

                    try {
                        log(`[Hover] Sending hover request to LSP...`);
                        const result = await client.sendRequest('textDocument/hover', {
                            textDocument: { uri: virtualUri.toString() },
                            position: { line: virtualPos.line, character: virtualPos.character }
                        });
                        log(`[Hover] LSP result: ${JSON.stringify(result)}`);

                        if (result && typeof result === 'object' && 'contents' in result) {
                            const hoverResult = result as { contents: { kind: string; value: string } | string };
                            let contents: vscode.MarkdownString | string;
                            if (typeof hoverResult.contents === 'object' && 'value' in hoverResult.contents) {
                                // LSP returns { kind: "markdown", value: "..." }
                                contents = new vscode.MarkdownString(hoverResult.contents.value);
                            } else {
                                contents = hoverResult.contents as string;
                            }
                            return new vscode.Hover(contents);
                        }
                    } catch (err) {
                        log(`[Hover] Error: ${err}`);
                    }

                    return null;
                }
            }
        )
    );
    debugLog(`[${elapsed()}] Registered hover provider for %%modelica blocks`);

    // Register completion provider for Python cells that forwards to Modelica LSP
    context.subscriptions.push(
        vscode.languages.registerCompletionItemProvider(
            { language: 'python', scheme: 'vscode-notebook-cell' },
            {
                async provideCompletionItems(document, position, _token, _context) {
                    const cellUri = document.uri.toString();
                    const blockInfo = findBlockAtPosition(cellUri, position);
                    if (!blockInfo) return null;

                    const { block, index } = blockInfo;
                    const virtualPos = cellToVirtualPosition(position, block);
                    if (!virtualPos) return null;

                    const virtualUri = getVirtualDocumentUri(cellUri, index);

                    if (!client) return null;

                    try {
                        const result = await client.sendRequest('textDocument/completion', {
                            textDocument: { uri: virtualUri.toString() },
                            position: { line: virtualPos.line, character: virtualPos.character }
                        });

                        if (result && Array.isArray(result)) {
                            return result.map((item: { label: string; kind?: number; detail?: string; documentation?: string }) => {
                                const completionItem = new vscode.CompletionItem(item.label);
                                if (item.kind) completionItem.kind = item.kind;
                                if (item.detail) completionItem.detail = item.detail;
                                if (item.documentation) completionItem.documentation = item.documentation;
                                return completionItem;
                            });
                        }
                    } catch (err) {
                        debugLog(`Completion error: ${err}`);
                    }

                    return null;
                }
            },
            '.', '(' // Trigger characters
        )
    );
    debugLog(`[${elapsed()}] Registered completion provider for %%modelica blocks`);

    // Initialize annotation collapsing feature (disabled by default - use Ctrl+K Ctrl+0 to fold all)
    const collapseAnnotations = config.get<boolean>('collapseAnnotations') ?? false;

    // Create decoration types for single-line annotation collapsing
    hiddenContentDecorationType = vscode.window.createTextEditorDecorationType({
        textDecoration: 'none',
        letterSpacing: '-1000em',  // Effectively hides the text
        opacity: '0',
    });

    ellipsisDecorationType = vscode.window.createTextEditorDecorationType({
        before: {
            contentText: '...',
            color: new vscode.ThemeColor('editorCodeLens.foreground'),
            fontStyle: 'italic',
        },
    });

    // Register command to toggle annotation expansion
    const toggleCommand = vscode.commands.registerCommand('rumoca.toggleAnnotation', async () => {
        const editor = vscode.window.activeTextEditor;
        if (editor && editor.document.languageId === 'modelica') {
            await toggleAnnotationAtCursor(editor, collapseAnnotations);
        }
    });
    context.subscriptions.push(toggleCommand);

    // Register command to expand all annotations
    const expandAllCommand = vscode.commands.registerCommand('rumoca.expandAllAnnotations', async () => {
        const editor = vscode.window.activeTextEditor;
        if (editor && editor.document.languageId === 'modelica') {
            await unfoldAllAnnotations(editor, collapseAnnotations);
        }
    });
    context.subscriptions.push(expandAllCommand);

    // Register command to collapse all annotations
    const collapseAllCommand = vscode.commands.registerCommand('rumoca.collapseAllAnnotations', async () => {
        const editor = vscode.window.activeTextEditor;
        if (editor && editor.document.languageId === 'modelica') {
            await foldAllAnnotations(editor, collapseAnnotations);
        }
    });
    context.subscriptions.push(collapseAllCommand);

    // Apply decorations to current editor and auto-fold if enabled
    const initializeEditor = async (editor: vscode.TextEditor | undefined) => {
        if (editor && editor.document.languageId === 'modelica') {
            updateSingleLineDecorations(editor, collapseAnnotations);
            // Auto-fold multi-line annotations on file open
            if (collapseAnnotations) {
                // Delay to let the editor and folding ranges fully load
                setTimeout(async () => {
                    // Ensure this editor is still active
                    if (vscode.window.activeTextEditor !== editor) return;

                    const annotations = findAllAnnotations(editor.document);
                    const multiLineAnnotations = annotations.filter(a => a.isMultiLine);
                    if (multiLineAnnotations.length > 0) {
                        const originalSelections = editor.selections;
                        const foldSelections = multiLineAnnotations.map(a =>
                            new vscode.Selection(a.startLine, 0, a.startLine, 0)
                        );
                        editor.selections = foldSelections;
                        await vscode.commands.executeCommand('editor.fold');
                        editor.selections = originalSelections;
                    }
                }, 300);
            }
        }
    };

    // Apply to current editor
    if (vscode.window.activeTextEditor) {
        initializeEditor(vscode.window.activeTextEditor);
    }

    // Listen for editor changes - auto-fold annotations when switching to a new file
    context.subscriptions.push(
        vscode.window.onDidChangeActiveTextEditor(editor => {
            if (editor && editor.document.languageId === 'modelica') {
                initializeEditor(editor);
            }
        })
    );

    // Note: We intentionally don't update decorations on document change
    // Annotations only collapse on file open or explicit double-click on "annotation" keyword
    // This prevents the annoying auto-collapse while typing

    // Listen for double-click on "annotation" keyword to toggle collapse/expand
    context.subscriptions.push(
        vscode.window.onDidChangeTextEditorSelection(async event => {
            const editor = event.textEditor;
            if (editor.document.languageId !== 'modelica') return;

            // Check if this is a mouse-triggered selection (double-click creates a word selection)
            if (event.kind === vscode.TextEditorSelectionChangeKind.Mouse) {
                const selection = event.selections[0];
                // Double-click selects a word, so selection won't be empty
                if (selection && !selection.isEmpty) {
                    const annotations = findAllAnnotations(editor.document);
                    const docKey = editor.document.uri.toString();

                    for (const annotation of annotations) {
                        const lineText = editor.document.lineAt(annotation.startLine).text;
                        const annotationMatch = lineText.match(/\bannotation\s*\(/);
                        if (annotationMatch) {
                            const keywordStart = lineText.indexOf(annotationMatch[0]);
                            const keywordEnd = keywordStart + 'annotation'.length;

                            // Check if double-click is on "annotation" keyword
                            const keywordRange = new vscode.Range(
                                new vscode.Position(annotation.startLine, keywordStart),
                                new vscode.Position(annotation.startLine, keywordEnd)
                            );

                            // Double-click on "annotation" keyword → toggle
                            if (keywordRange.contains(selection.start) || keywordRange.contains(selection.end)) {
                                if (annotation.isMultiLine) {
                                    // Toggle fold for multi-line annotation
                                    const originalSelections = editor.selections;
                                    editor.selections = [new vscode.Selection(annotation.startLine, 0, annotation.startLine, 0)];
                                    await vscode.commands.executeCommand('editor.toggleFold');
                                    editor.selections = originalSelections;
                                } else if (collapseAnnotations) {
                                    // Toggle single-line annotation expansion via decorations
                                    if (!expandedSingleLineAnnotations.has(docKey)) {
                                        expandedSingleLineAnnotations.set(docKey, new Set<string>());
                                    }
                                    const expanded = expandedSingleLineAnnotations.get(docKey)!;
                                    const rangeKey = getRangeKey(annotation.contentRange);
                                    if (expanded.has(rangeKey)) {
                                        expanded.delete(rangeKey);
                                    } else {
                                        expanded.add(rangeKey);
                                    }
                                    updateSingleLineDecorations(editor, collapseAnnotations);
                                }
                                return;
                            }
                        }
                    }
                }
            }
        })
    );

    // Clean up decoration types
    context.subscriptions.push(hiddenContentDecorationType);
    context.subscriptions.push(ellipsisDecorationType);

    log('Rumoca Modelica extension activated');
}

export async function deactivate(): Promise<void> {
    if (client) {
        await client.stop();
    }
}
