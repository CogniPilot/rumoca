import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';
import { createRequire } from 'module';
import * as vscode from 'vscode';
import { execSync, spawn, spawnSync, type ChildProcessWithoutNullStreams } from 'child_process';
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
const DEFAULT_WEB_RESULTS_OUTPUT_DIR = 'results';
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

    // Pattern 1: %%modelica cell magic (the name registered by the rumoca
    // Python package).
    let inBlock = false;
    let blockStartLine = 0;
    let blockLines: string[] = [];

    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        const trimmed = line.trim();

        if (trimmed.startsWith('%%modelica')) {
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

function shellQuote(value: string): string {
    if (process.platform === 'win32') {
        return `"${value.replace(/"/g, '\\"')}"`;
    }
    return `'${value.replace(/'/g, "'\\''")}'`;
}

function rumocaExecutableFromServerPath(serverPath: string | undefined): string | undefined {
    if (serverPath) {
        const executable = process.platform === 'win32' ? 'rumoca.exe' : 'rumoca';
        const sibling = path.join(path.dirname(serverPath), executable);
        if (fs.existsSync(sibling)) {
            return sibling;
        }
    }
    return findInPath('rumoca');
}

async function openLiveViewerInPanel(url: vscode.Uri): Promise<void> {
    await vscode.commands.executeCommand('simpleBrowser.show', url.toString());
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
    parameterOverrides?: Record<string, number>;
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
    diagnostic?: SimulationDiagnosticLocation;
}

interface SimulationDiagnosticLocation {
    uri?: string;
    range?: {
        start?: { line?: number; character?: number };
        end?: { line?: number; character?: number };
    };
    source?: string;
    code?: string;
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
    runId?: string;
    runPath?: string;
    model: string;
    workspaceRoot?: string;
    title?: string;
    activeViewId?: string;
}

interface ResultsPanelModelRef {
    model: string;
    workspaceRoot?: string;
    runId?: string;
    runPath?: string;
    title?: string;
}

interface ResultsWebviewAssets {
    uplotCss: string;
    resultsAppJs: string;
    resultsAppCss: string;
}

interface VisualizationSharedModule {
    buildScenarioConfigDocument(args: unknown): string;
    applyScenarioConfigEdits(tree: unknown, edits: unknown): Record<string, unknown>;
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
    defaultThreeDimensionalViewerScript(): string;
    defaultVisualizationViews(): VisualizationView[];
    normalizeVisualizationViews(raw: unknown): VisualizationView[];
    normalizeSimulationRunMetrics(raw: unknown): SimulationRunMetrics | undefined;
    normalizeSimulationPayload(raw: unknown): ParsedSimulationPayload | undefined;
    normalizeHostedResultsPanelState(raw: unknown, fallbackWorkspaceRoot?: string): ResultsPanelState | undefined;
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
        resultDirectory?: string,
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
    persistHostedSimulationRunWithViews(args: {
        model: string;
        workspaceRoot?: string;
        payload: unknown;
        metrics?: unknown;
        loadConfiguredViews: (args: { model: string; workspaceRoot?: string }) => Promise<unknown> | unknown;
        hydrateViews: (args: {
            model: string;
            workspaceRoot?: string;
            views: VisualizationView[];
        }) => Promise<unknown> | unknown;
        defaultViews?: VisualizationView[];
        pathExists?: (runPath: string) => boolean;
        readTextFile?: (runPath: string) => Promise<string> | string;
        writeTextFile?: (runPath: string, content: string) => Promise<void> | void;
        resultDirectory?: string;
    }): Promise<{
        runId: string;
        runPath: string;
        savedAtUnixMs: number;
        views: VisualizationView[];
    } | undefined>;
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
        resultDirectory?: string;
        readTextFile: (runPath: string) => Promise<string> | string;
    }): Promise<PersistedSimulationRun | undefined>;
    simulationRunDocumentPath(runId: string, resultDirectory?: string): string | undefined;
    writePersistedSimulationRunDocument(args: {
        model: string;
        payload: unknown;
        metrics?: unknown;
        views?: unknown;
        resultDirectory?: string;
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
    const candidate = path.resolve(__dirname, '..', 'media', 'vendor', 'visualization_shared.cjs');
    if (!fs.existsSync(candidate)) {
        throw new Error(
            `Missing shared Rumoca visualization helpers at ${candidate}. `
            + 'Run `npm run build` in packages/vscode.',
        );
    }
    const loaded = nodeRequire(candidate) as Partial<VisualizationSharedModule>;
    if (typeof loaded.buildScenarioConfigDocument !== 'function'
        || typeof loaded.applyScenarioConfigEdits !== 'function'
        || typeof loaded.buildHostedResultsDocument !== 'function'
        || typeof loaded.handleHostedResultsRequest !== 'function'
        || typeof loaded.defaultThreeDimensionalViewerScript !== 'function'
        || typeof loaded.defaultVisualizationViews !== 'function'
        || typeof loaded.normalizeVisualizationViews !== 'function'
        || typeof loaded.normalizeHostedPngExportRequest !== 'function'
        || typeof loaded.normalizeHostedResultsNotifyPayload !== 'function'
        || typeof loaded.normalizeHostedWebmExportRequest !== 'function'
        || typeof loaded.normalizeSimulationRunMetrics !== 'function'
        || typeof loaded.normalizeSimulationPayload !== 'function'
        || typeof loaded.buildSimulationRunDocument !== 'function'
        || typeof loaded.hydrateVisualizationViewsForModel !== 'function'
        || typeof loaded.nextSimulationRunLocation !== 'function'
        || typeof loaded.normalizeHostedResultsModelRef !== 'function'
        || typeof loaded.normalizePersistedSimulationRun !== 'function'
        || typeof loaded.persistVisualizationViewsForModel !== 'function'
        || typeof loaded.readPersistedSimulationRunDocument !== 'function'
        || typeof loaded.simulationRunDocumentPath !== 'function'
        || typeof loaded.writePersistedSimulationRunDocument !== 'function') {
        throw new Error(`Invalid shared Rumoca visualization helpers at ${candidate}.`);
    }
    visualizationSharedCache = loaded as VisualizationSharedModule;
    return visualizationSharedCache;
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
    const response = await sendScenarioCommand<SimulationModelStateResponse>(
        'rumoca.scenario.getSimulationModels',
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
    const response = await sendScenarioCommand<SimulationModelStateResponse>(
        'rumoca.scenario.setSelectedSimulationModel',
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
        os.homedir(),
        resolveWorkspaceRootFallback(),
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
        outputDir: (config.get<string>('simulation.outputDir') ?? DEFAULT_WEB_RESULTS_OUTPUT_DIR).trim()
            || DEFAULT_WEB_RESULTS_OUTPUT_DIR,
        sourceRootPaths: sourceRootPaths.mergedPaths,
    };
}

function getConfiguredWorkspaceSourceRootPaths(): string[] {
    return vscode.workspace
        .getConfiguration('rumoca')
        .get<string[]>('sourceRootPaths') ?? [];
}

async function setConfiguredWorkspaceSourceRootPaths(paths: string[]): Promise<void> {
    const normalized = Array.from(
        new Set(paths.map((entry) => entry.trim()).filter(Boolean)),
    );
    await vscode.workspace
        .getConfiguration('rumoca')
        .update('sourceRootPaths', normalized, vscode.ConfigurationTarget.Workspace);
}

function isSimulationRunnableDocument(document: vscode.TextDocument): boolean {
    // Filename is the discovery hook; the `[rumoca]` marker is the authoritative
    // declaration. Both must hold to enable Rumoca-specific features.
    return isScenarioConfigPath(document.uri.fsPath) && hasRumocaMarker(document.getText());
}

/// Matches the Rumoca task-file naming convention: `rumoca-scenario.toml`
/// (default) or `rumoca-scenario.<profile>.toml`. This is the discovery hook
/// only; the `[rumoca]` marker (see `hasRumocaMarker`) is authoritative.
function isScenarioConfigPath(filePath: string): boolean {
    const name = path.basename(filePath).toLowerCase();
    return name === 'rumoca-scenario.toml'
        || (name.startsWith('rumoca-scenario.') && name.endsWith('.toml'));
}

/// Whether a TOML document declares itself a Rumoca task file via a top-level
/// `[rumoca]` marker section.
function hasRumocaMarker(text: string): boolean {
    return /^[ \t]*\[rumoca\][ \t]*$/m.test(text);
}

async function scenarioDocumentFromCommandResource(
    resource: vscode.Uri | undefined,
): Promise<vscode.TextDocument | undefined> {
    if (resource?.scheme === 'file') {
        const document = await vscode.workspace.openTextDocument(resource);
        if (isSimulationRunnableDocument(document)) {
            return document;
        }
    }

    const editor = vscode.window.activeTextEditor;
    return editor && isSimulationRunnableDocument(editor.document)
        ? editor.document
        : undefined;
}

function isModelicaSourceDocument(document: vscode.TextDocument): boolean {
    return document.uri.fsPath.toLowerCase().endsWith('.mo');
}

function modelLeafName(model: string): string {
    const parts = model.split('.').map(part => part.trim()).filter(Boolean);
    return parts.length > 0 ? parts[parts.length - 1] : 'model';
}

function sanitizeScenarioStem(value: string): string {
    return value
        .trim()
        .replace(/\.(rum|toml)$/i, '')
        .replace(/([a-z0-9])([A-Z])/g, '$1_$2')
        .toLowerCase()
        .replace(/[^a-z0-9]+/g, '_')
        .replace(/^_+|_+$/g, '')
        || 'scenario';
}

function relativeModelFileForScenario(document: vscode.TextDocument): string {
    return path.basename(document.uri.fsPath).split(path.sep).join('/');
}

function defaultSourceRootsForScenario(model: string): string[] {
    return model.includes('.') ? ['..'] : [];
}

function buildScenarioConfig(args: {
    task: 'simulate' | 'codegen';
    model: string;
    modelFile: string;
    sourceRoots: string[];
    target?: string;
    outputDir?: string;
}): Record<string, unknown> {
    const config: Record<string, unknown> = {};
    if (args.sourceRoots.length > 0) {
        config.source_roots = args.sourceRoots;
    }
    config.rumoca = {
        version: '1',
        task: args.task,
    };
    config.model = {
        file: args.modelFile,
        name: args.model,
    };
    if (args.task === 'simulate') {
        config.viewer = {
            mode: 'results_panel',
        };
        const sim: Record<string, unknown> = {
            solver: 'auto',
            t_end: 10.0,
        };
        if (args.outputDir) {
            sim.output_dir = args.outputDir;
        }
        config.sim = sim;
        config.plot = {
            views: [{
                id: 'states_time',
                title: 'States vs Time',
                type: 'timeseries',
                x: 'time',
                y: ['*states'],
            }],
        };
    } else {
        const codegen: Record<string, unknown> = {
            target: args.target || DEFAULT_CODEGEN_TARGET_ID,
        };
        if (args.outputDir) {
            codegen.output_dir = args.outputDir;
        }
        config.codegen = codegen;
    }
    return config;
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

interface ScenarioSimulationConfigResponse {
    preset?: ModelSimulationPreset;
    defaults?: SimulationExecutionSettings;
    effective?: SimulationExecutionSettings;
    scenarioPath?: string;
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
    diagnostic?: unknown;
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
    manifest: string;
}

interface CodegenSettings {
    mode: 'target' | 'custom-target';
    builtinTargetId: string;
    customTargetPath: string;
}

interface CodegenTargetFile {
    path: string;
    content: string;
}

interface CodegenTargetRenderResponse {
    ok?: boolean;
    files?: CodegenTargetFile[];
    error?: string;
}

interface ScenarioConfigResponse {
    ok?: boolean;
    task?: 'simulate' | 'codegen';
    model?: string;
    target?: string;
    outputDir?: string;
    viewerMode?: 'results_panel' | 'external_web';
    viewerPreferExternal?: boolean;
    simMode?: 'as_fast_as_possible' | 'realtime' | 'lockstep';
    httpPort?: number;
    websocketPort?: number;
    httpScene?: string;
    inputEnabled?: boolean;
    error?: string;
}

interface ScenarioConfigFullResponse {
    ok?: boolean;
    uri?: string;
    config?: Record<string, unknown>;
    descriptor?: ScenarioConfigResponse;
    effectiveSourceRootPaths?: string[];
    parameterMetadata?: unknown;
    error?: string;
}

function scenarioNeedsInputRunner(scenario: ScenarioConfigResponse): boolean {
    return scenario.viewerMode === 'external_web';
}

const DEFAULT_CODEGEN_TARGET_ID = 'sympy';
const SELECTED_SIMULATION_MODELS_STATE_KEY = 'rumoca.selectedSimulationModelsByDocument';
const LIVE_VIEWER_READY_PREFIX = 'rumoca-viewer-ready ';
const MAX_INTERACTIVE_FAILURE_LINES = 30;

function stripAnsiControlCodes(text: string): string {
    let stripped = '';
    for (let i = 0; i < text.length; i += 1) {
        if (text.charCodeAt(i) !== 27 || text[i + 1] !== '[') {
            stripped += text[i];
            continue;
        }
        i += 2;
        while (i < text.length) {
            const code = text.charCodeAt(i);
            if ((code >= 65 && code <= 90) || (code >= 97 && code <= 122)) {
                break;
            }
            i += 1;
        }
    }
    return stripped;
}

function rememberInteractiveOutput(recentLines: string[], text: string): void {
    const normalized = stripAnsiControlCodes(text).replace(/\r/g, '\n');
    for (const rawLine of normalized.split('\n')) {
        const line = rawLine.trim();
        if (!line || line.startsWith('[sim] t=')) {
            continue;
        }
        recentLines.push(line);
    }
    while (recentLines.length > MAX_INTERACTIVE_FAILURE_LINES) {
        recentLines.shift();
    }
}

function collectPortNumbers(lines: string[]): string[] {
    const ports = new Set<string>();
    for (const line of lines) {
        for (const match of line.matchAll(/\b(?:port\s+|localhost:|0\.0\.0\.0:)(\d{2,5})\b/g)) {
            ports.add(match[1]);
        }
    }
    return [...ports];
}

function summarizeInteractiveFailure(recentLines: string[]): string | undefined {
    const combined = recentLines.join('\n');
    const portConflictLines = recentLines.filter((line) =>
        /Address already in use|Failed to bind .*port|HTTP server error/i.test(line)
    );
    if (portConflictLines.length > 0) {
        const ports = collectPortNumbers(portConflictLines);
        const portText = ports.length > 0 ? ` ${ports.join('/')}` : '';
        return `viewer port${ports.length === 1 ? '' : 's'}${portText} already in use; stop the running Rumoca simulation or choose different ports in the scenario config.`;
    }

    const missingSourceRoot = combined.match(/source-root path does not exist:\s*([^\n]+)/);
    if (missingSourceRoot) {
        return `missing Modelica source root: ${missingSourceRoot[1].trim()}. Run cargo xtask repo modelica-deps ensure for the example dependencies.`;
    }

    const unexpectedArg = combined.match(/unexpected argument ['"]([^'"]+)['"]/);
    if (unexpectedArg) {
        return `unsupported rumoca CLI argument ${unexpectedArg[1]}; update the extension launch command or the rumoca binary.`;
    }

    return [...recentLines].reverse().find((line) =>
        !line.startsWith('Finished ')
        && !line.startsWith('Running ')
        && line !== 'rumoca sim'
    );
}

// Benchmark/smoke instrumentation (no environment variables). The LSP benchmark
// harness records timing artifacts by setting workspace settings such as
// `rumoca.benchmark.completionTimingFile`; the `rumoca-lsp` server writes those
// artifacts only when given the matching CLI flag, so we translate the settings
// into server argv here. In a normal editor session none of these are set and
// no extra flags are passed.
function benchmarkServerArgs(config: vscode.WorkspaceConfiguration): string[] {
    const flagBySetting: Array<[string, string]> = [
        ['benchmark.completionTimingFile', '--completion-timing-file'],
        ['benchmark.completionProgressFile', '--completion-progress-file'],
        ['benchmark.diagnosticsTimingFile', '--diagnostics-timing-file'],
        ['benchmark.navigationTimingFile', '--navigation-timing-file'],
        ['benchmark.startupTimingFile', '--startup-timing-file'],
    ];
    const args: string[] = [];
    for (const [settingKey, flag] of flagBySetting) {
        const value = config.get<string>(settingKey);
        if (typeof value === 'string' && value.length > 0) {
            args.push(flag, value);
        }
    }
    return args;
}
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
        mode: 'target',
        builtinTargetId: DEFAULT_CODEGEN_TARGET_ID,
        customTargetPath: '',
    };
}

function normalizeCodegenSettings(raw: unknown): CodegenSettings {
    const next = raw && typeof raw === 'object' && !Array.isArray(raw)
        ? raw as Record<string, unknown>
        : {};
    const mode = next.mode === 'custom-target'
            ? 'custom-target'
            : 'target';
    return {
        mode,
        builtinTargetId: trimMaybeString(next.builtinTargetId) || DEFAULT_CODEGEN_TARGET_ID,
        customTargetPath: trimMaybeString(next.customTargetPath),
    };
}

function codegenSettingsFromScenarioConfig(
    raw: unknown,
    templates: BuiltinCodegenTemplate[],
): CodegenSettings {
    const config = raw && typeof raw === 'object' && !Array.isArray(raw)
        ? raw as Record<string, unknown>
        : {};
    const target = trimMaybeString(config.target) || DEFAULT_CODEGEN_TARGET_ID;
    const builtin = templates.find((template) => template.id === target);
    if (builtin) {
        return {
            mode: 'target',
            builtinTargetId: builtin.id,
            customTargetPath: '',
        };
    }
    return {
        mode: 'custom-target',
        builtinTargetId: findBuiltinCodegenTemplate(templates, DEFAULT_CODEGEN_TARGET_ID)?.id
            ?? templates[0]?.id
            ?? DEFAULT_CODEGEN_TARGET_ID,
        customTargetPath: target,
    };
}

function codegenConfigFromSettings(raw: unknown): { target: string } {
    const settings = normalizeCodegenSettings(raw);
    const target = settings.mode === 'custom-target'
        ? settings.customTargetPath
        : settings.builtinTargetId;
    return {
        target: trimMaybeString(target) || DEFAULT_CODEGEN_TARGET_ID,
    };
}

function normalizeSelectedSimulationModels(raw: unknown): Record<string, string> {
    if (!raw || typeof raw !== 'object' || Array.isArray(raw)) {
        return {};
    }
    const out: Record<string, string> = {};
    for (const [documentUri, model] of Object.entries(raw as Record<string, unknown>)) {
        const key = trimMaybeString(documentUri);
        const value = trimMaybeString(model);
        if (key && value) {
            out[key] = value;
        }
    }
    return out;
}

function loadStoredSelectedSimulationModel(
    context: vscode.ExtensionContext,
    documentUri: string | undefined,
): string | undefined {
    const uri = trimMaybeString(documentUri);
    if (!uri) {
        return undefined;
    }
    const models = normalizeSelectedSimulationModels(
        context.workspaceState.get(SELECTED_SIMULATION_MODELS_STATE_KEY),
    );
    return models[uri];
}

async function storeSelectedSimulationModel(
    context: vscode.ExtensionContext,
    documentUri: string | undefined,
    model: string | undefined,
): Promise<void> {
    const uri = trimMaybeString(documentUri);
    if (!uri) {
        return;
    }
    const models = normalizeSelectedSimulationModels(
        context.workspaceState.get(SELECTED_SIMULATION_MODELS_STATE_KEY),
    );
    const selected = trimMaybeString(model);
    if (selected) {
        models[uri] = selected;
    } else {
        delete models[uri];
    }
    await context.workspaceState.update(
        SELECTED_SIMULATION_MODELS_STATE_KEY,
        Object.keys(models).length > 0 ? models : undefined,
    );
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
                manifest: typeof next.manifest === 'string' ? next.manifest : '',
            };
        })
        .filter((entry) => entry.id.length > 0 && entry.label.length > 0);
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

function scenarioDefaultOutputDir(document: vscode.TextDocument): string {
    const stem = sanitizeScenarioStem(path.basename(document.uri.fsPath));
    return `${stem}_out`;
}

function resolveScenarioOutputDir(
    document: vscode.TextDocument,
    configuredOutputDir: string | undefined,
): string {
    const raw = trimMaybeString(configuredOutputDir) || scenarioDefaultOutputDir(document);
    if (path.isAbsolute(raw)) {
        return raw;
    }
    return path.resolve(path.dirname(document.uri.fsPath), raw);
}

function scenarioResultDirectory(
    document: vscode.TextDocument,
    configuredOutputDir: string | undefined,
    workspaceRoot: string | undefined,
    scenarioPath?: string,
): string {
    const raw = trimMaybeString(configuredOutputDir) || DEFAULT_WEB_RESULTS_OUTPUT_DIR;
    const scenarioFsPath = resolveResultDocumentPath(workspaceRoot, scenarioPath) ?? document.uri.fsPath;
    const absoluteDir = path.isAbsolute(raw) ? raw : path.resolve(path.dirname(scenarioFsPath), raw);
    return workspaceRelativeTemplatePath(workspaceRoot, absoluteDir);
}

function resolveResultDocumentPath(
    workspaceRoot: string | undefined,
    resultPath: string | undefined,
): string | undefined {
    const raw = trimMaybeString(resultPath);
    if (!raw) {
        return undefined;
    }
    if (path.isAbsolute(raw)) {
        return raw;
    }
    return workspaceRoot
        ? path.join(workspaceRoot, ...raw.split('/'))
        : path.resolve(raw);
}

function resolveTargetOutputPath(outputRoot: string, targetPath: string): string {
    const normalizedRoot = path.resolve(outputRoot);
    const resolved = path.resolve(normalizedRoot, targetPath);
    const relative = path.relative(normalizedRoot, resolved);
    if (relative.startsWith('..') || path.isAbsolute(relative)) {
        throw new Error(`Target attempted to write outside output directory: ${targetPath}`);
    }
    return resolved;
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

async function sendScenarioCommand<T>(
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

async function loadBuiltInCodegenTemplates(
    forceReload = false,
): Promise<BuiltinCodegenTemplate[]> {
    if (!forceReload && builtInCodegenTemplatesCache) {
        return builtInCodegenTemplatesCache;
    }
    const response = await sendWorkspaceCommand<unknown>('rumoca.workspace.getBuiltinTargets', {});
    const templates = normalizeBuiltinCodegenTemplates(response);
    if (templates.length === 0) {
        throw new Error('No built-in codegen targets are available from rumoca-lsp.');
    }
    builtInCodegenTemplatesCache = templates;
    return templates;
}

async function getScenarioCodegenConfig(
    model: string,
    workspaceRoot: string | undefined,
): Promise<unknown> {
    if (!workspaceRoot) {
        return undefined;
    }
    return await sendScenarioCommand<unknown>(
        'rumoca.scenario.getCodegenConfig',
        {
            workspaceRoot,
            model,
        },
    );
}

async function setScenarioCodegenConfig(
    model: string,
    workspaceRoot: string | undefined,
    settings: CodegenSettings,
): Promise<boolean> {
    if (!workspaceRoot) {
        return false;
    }
    const response = await sendScenarioCommand<{ ok?: boolean }>(
        'rumoca.scenario.setCodegenConfig',
        {
            workspaceRoot,
            model,
            config: codegenConfigFromSettings(settings),
        },
    );
    return response?.ok === true;
}

async function setScenarioSourceRoots(
    model: string,
    workspaceRoot: string | undefined,
    sourceRootPaths: string[],
): Promise<boolean> {
    if (!workspaceRoot) {
        return false;
    }
    const response = await sendScenarioCommand<{ ok?: boolean }>(
        'rumoca.scenario.setSourceRoots',
        {
            workspaceRoot,
            model,
            task: 'codegen',
            config: { sourceRootPaths },
        },
    );
    return response?.ok === true;
}

async function loadCurrentCodegenSettingsState(
    model: string,
    workspaceRoot: string | undefined,
): Promise<{ settings: CodegenSettings; templates: BuiltinCodegenTemplate[] }> {
    const templates = await loadBuiltInCodegenTemplates();
    const settings = codegenSettingsFromScenarioConfig(
        await getScenarioCodegenConfig(model, workspaceRoot),
        templates,
    );
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

function workspaceRelativeSourceRootPath(
    workspaceRoot: string | undefined,
    absolutePath: string,
): string {
    return workspaceRelativeTemplatePath(workspaceRoot, absolutePath);
}

async function getScenarioSimulationConfig(
    model: string,
    workspaceRoot: string | undefined,
    fallback: SimulationSettings
): Promise<ScenarioSimulationConfigResponse | undefined> {
    return await sendScenarioCommand<ScenarioSimulationConfigResponse>(
        'rumoca.scenario.getSimulationConfig',
        {
            workspaceRoot,
            model,
            fallback,
        }
    );
}

async function getScenarioConfig(documentUri: string): Promise<ScenarioConfigResponse | undefined> {
    return await sendScenarioCommand<ScenarioConfigResponse>(
        'rumoca.scenario.getScenarioConfig',
        { uri: documentUri },
    );
}

async function getScenarioConfigFull(
    documentUri: string,
    source?: string,
): Promise<ScenarioConfigFullResponse | undefined> {
    return await sendScenarioCommand<ScenarioConfigFullResponse>(
        'rumoca.scenario.getScenarioConfigFull',
        {
            uri: documentUri,
            ...(source !== undefined ? { source } : {}),
        },
    );
}

async function getModelParameterMetadata(
    payload: Record<string, unknown>,
): Promise<unknown[]> {
    const response = await sendScenarioCommand<{ ok?: boolean; parameters?: unknown; error?: string }>(
        'rumoca.model.parameterMetadata',
        payload,
    );
    if (response?.ok === true && Array.isArray(response.parameters)) {
        return response.parameters;
    }
    return [];
}

function scenarioParameterOverrides(config?: Record<string, unknown>): Record<string, number> {
    const parameters = config?.parameters;
    if (!parameters || typeof parameters !== 'object' || Array.isArray(parameters)) {
        return {};
    }
    const result: Record<string, number> = {};
    for (const [name, value] of Object.entries(parameters as Record<string, unknown>)) {
        if (typeof name !== 'string' || !name.trim()) {
            continue;
        }
        const numeric = typeof value === 'number' ? value : Number(value);
        if (Number.isFinite(numeric)) {
            result[name] = numeric;
        }
    }
    return result;
}

async function renderScenarioConfig(config: Record<string, unknown>): Promise<string> {
    const response = await sendScenarioCommand<{ ok?: boolean; content?: unknown; error?: string }>(
        'rumoca.scenario.renderScenarioConfig',
        { config },
    );
    if (response?.ok === true && typeof response.content === 'string') {
        return response.content;
    }
    throw new Error(response?.error || 'failed to render scenario config');
}

function fullDocumentRange(document: vscode.TextDocument): vscode.Range {
    const lastLine = document.lineAt(document.lineCount - 1);
    return new vscode.Range(0, 0, lastLine.range.end.line, lastLine.range.end.character);
}

function escapeHtml(value: string): string {
    return value
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;');
}

async function setScenarioConfig(
    documentUri: string,
    config: Record<string, unknown>,
    options?: { replaceDirtyOpenDocument?: boolean },
): Promise<void> {
    const uri = vscode.Uri.parse(documentUri);
    const openDocument = vscode.workspace.textDocuments.find(
        (document) => document.uri.toString() === uri.toString(),
    );
    if (openDocument) {
        if (openDocument.isDirty && options?.replaceDirtyOpenDocument !== true) {
            throw new Error('save or revert raw TOML edits before saving the scenario GUI');
        }
        const content = await renderScenarioConfig(config);
        const edit = new vscode.WorkspaceEdit();
        edit.replace(uri, fullDocumentRange(openDocument), content);
        const applied = await vscode.workspace.applyEdit(edit);
        if (!applied) {
            throw new Error('failed to update scenario config editor');
        }
        const saved = await openDocument.save();
        if (!saved) {
            throw new Error('failed to save scenario config');
        }
        return;
    }

    const response = await sendScenarioCommand<{ ok?: boolean; error?: string }>(
        'rumoca.scenario.setScenarioConfig',
        { uri: documentUri, config },
    );
    if (response?.ok !== true) {
        throw new Error(response?.error || 'failed to save scenario config');
    }
}

function scenarioConfigErrorDocument(document: vscode.TextDocument, error: string): string {
    return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Rumoca Scenario</title>
  <style>
    body { margin: 0; padding: 16px; font-family: var(--vscode-font-family, system-ui, sans-serif); color: var(--vscode-foreground, #d4d4d4); background: var(--vscode-editor-background, #1e1e1e); }
    .panel { border: 1px solid var(--vscode-inputValidation-errorBorder, #f48771); border-radius: 8px; padding: 12px; background: var(--vscode-sideBar-background, #252526); }
    h1 { margin: 0 0 8px 0; font-size: 16px; }
    p { margin: 0 0 12px 0; color: var(--vscode-descriptionForeground, #9da5b4); }
    pre { white-space: pre-wrap; color: var(--vscode-errorForeground, #f48771); }
    button { font: inherit; padding: 6px 12px; border-radius: 6px; border: 1px solid var(--vscode-button-border, transparent); background: var(--vscode-button-background, #0e639c); color: var(--vscode-button-foreground, #fff); cursor: pointer; }
  </style>
</head>
<body>
  <div class="panel">
    <h1>Could not load ${escapeHtml(path.basename(document.uri.fsPath))}</h1>
    <p>Open the raw TOML view to fix the scenario file.</p>
    <pre>${escapeHtml(error)}</pre>
    <button id="rawBtn">Raw TOML</button>
  </div>
  <script>
    const vscodeApi = typeof acquireVsCodeApi === 'function' ? acquireVsCodeApi() : null;
    document.getElementById('rawBtn').addEventListener('click', () => {
      if (vscodeApi) {
        vscodeApi.postMessage({ command: 'scenarioConfig.request', requestId: 'raw', method: 'toggleRaw', payload: {} });
      }
    });
  </script>
</body>
</html>`;
}

function resultsJsonErrorDocument(document: vscode.TextDocument, error: string): string {
    return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Rumoca Results</title>
  <style>
    body { margin: 0; padding: 16px; font-family: var(--vscode-font-family, system-ui, sans-serif); color: var(--vscode-foreground, #d4d4d4); background: var(--vscode-editor-background, #1e1e1e); }
    .panel { border: 1px solid var(--vscode-inputValidation-errorBorder, #f48771); border-radius: 8px; padding: 12px; background: var(--vscode-sideBar-background, #252526); }
    h1 { margin: 0 0 8px 0; font-size: 16px; }
    p { margin: 0 0 12px 0; color: var(--vscode-descriptionForeground, #9da5b4); }
    pre { white-space: pre-wrap; color: var(--vscode-errorForeground, #f48771); }
    button { font: inherit; padding: 6px 12px; border-radius: 6px; border: 1px solid var(--vscode-button-border, transparent); background: var(--vscode-button-background, #0e639c); color: var(--vscode-button-foreground, #fff); cursor: pointer; }
  </style>
</head>
<body>
  <div class="panel">
    <h1>Could not render ${escapeHtml(path.basename(document.uri.fsPath))}</h1>
    <p>This editor only renders persisted Rumoca result JSON documents.</p>
    <pre>${escapeHtml(error)}</pre>
    <button id="rawBtn">Raw JSON</button>
  </div>
  <script>
    const vscodeApi = typeof acquireVsCodeApi === 'function' ? acquireVsCodeApi() : null;
    document.getElementById('rawBtn').addEventListener('click', () => {
      if (vscodeApi) {
        vscodeApi.postMessage({ command: 'resultsJson.openRaw' });
      }
    });
  </script>
</body>
</html>`;
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

async function setScenarioSimulationPreset(
    model: string,
    workspaceRoot: string | undefined,
    preset: ModelSimulationPreset
): Promise<boolean> {
    if (!workspaceRoot) {
        return false;
    }
    const response = await sendScenarioCommand<{ ok?: boolean }>(
        'rumoca.scenario.setSimulationPreset',
        {
            workspaceRoot,
            model,
            preset,
        }
    );
    return response?.ok === true;
}

async function resetScenarioSimulationPreset(
    model: string,
    workspaceRoot: string | undefined
): Promise<boolean> {
    if (!workspaceRoot) {
        return false;
    }
    const response = await sendScenarioCommand<{ ok?: boolean }>(
        'rumoca.scenario.resetSimulationPreset',
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

async function getScenarioVisualizationConfig(
    model: string,
    workspaceRoot: string | undefined
): Promise<VisualizationView[]> {
    const response = await sendScenarioCommand<{ views?: unknown }>(
        'rumoca.scenario.getVisualizationConfig',
        {
            workspaceRoot,
            model,
        }
    );
    return normalizeVisualizationViews(response?.views);
}

async function setScenarioVisualizationConfig(
    model: string,
    workspaceRoot: string | undefined,
    views: VisualizationView[]
): Promise<boolean> {
    if (!workspaceRoot) {
        return false;
    }
    const response = await sendScenarioCommand<{ ok?: boolean }>(
        'rumoca.scenario.setVisualizationConfig',
        {
            workspaceRoot,
            model,
            views,
        }
    );
    return response?.ok === true;
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
    const configuredViews = await getScenarioVisualizationConfig(model, workspaceRoot);
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

function normalizeSimulationDiagnostic(raw: unknown): SimulationDiagnosticLocation | undefined {
    if (!raw || typeof raw !== 'object' || Array.isArray(raw)) {
        return undefined;
    }
    const diagnostic = raw as SimulationDiagnosticLocation;
    const uri = typeof diagnostic.uri === 'string' ? diagnostic.uri : undefined;
    const start = diagnostic.range?.start;
    const end = diagnostic.range?.end;
    if (
        !uri
        || typeof start?.line !== 'number'
        || typeof start.character !== 'number'
        || typeof end?.line !== 'number'
        || typeof end.character !== 'number'
    ) {
        return undefined;
    }
    return diagnostic;
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
    const accepted = await sendScenarioCommand<BackgroundRequestAccepted>('rumoca.scenario.startSimulation', {
        uri: modelUri,
        model,
        settings: {
            solver: settings.solver,
            tEnd: settings.tEnd,
            dt: settings.dt ?? null,
            sourceRootPaths: settings.sourceRootPaths ?? [],
            parameterOverrides: settings.parameterOverrides ?? {},
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
    const diagnostic = normalizeSimulationDiagnostic(response?.diagnostic);
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
        diagnostic,
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
    const accepted = await sendScenarioCommand<BackgroundRequestAccepted>(
        'rumoca.scenario.prepareSimulationModels',
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

        void vscode.window.showErrorMessage(msg, installAction, 'Configure Path').then(selection => {
            if (selection === installAction) {
                const terminal = vscode.window.createTerminal('Rumoca Install');
                terminal.show();
                terminal.sendText('cargo install rumoca');
            } else if (selection === 'Configure Path') {
                openRumocaSetting('rumoca.serverPath');
            }
        });
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
        void vscode.window.showErrorMessage(msg, 'Open Settings', 'Configure Path').then(selection => {
            if (selection === 'Open Settings') {
                openRumocaSetting('rumoca.useSystemServer');
            } else if (selection === 'Configure Path') {
                openRumocaSetting('rumoca.serverPath');
            }
        });
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

        const serverArgs = benchmarkServerArgs(vscode.workspace.getConfiguration('rumoca'));
        const nextClient = new LanguageClient(
            'rumoca',
            'Rumoca LSP',
            {
                run: { command: usableServerPath, args: serverArgs, transport: TransportKind.stdio },
                debug: { command: usableServerPath, args: serverArgs, transport: TransportKind.stdio }
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

    const startInteractiveScenario = async (
        document: vscode.TextDocument,
        scenario: ScenarioConfigResponse,
    ): Promise<void> => {
        const rumocaPath = rumocaExecutableFromServerPath(languageClientRuntime.getServerPath());
        if (!rumocaPath) {
            throw new Error('rumoca executable not found. Configure rumoca.serverPath or add rumoca to PATH.');
        }
        const expectsInputViewer = scenarioNeedsInputRunner(scenario);
        const args = [
            'sim',
            '--config',
            document.uri.fsPath,
        ];
        const writeEmitter = new vscode.EventEmitter<string>();
        const closeEmitter = new vscode.EventEmitter<number>();
        let child: ChildProcessWithoutNullStreams | undefined;
        let lineBuffer = '';
        let viewerOpened = false;
        const recentOutputLines: string[] = [];

        const openViewerFromReadyLine = (line: string) => {
            if (!line.startsWith(LIVE_VIEWER_READY_PREFIX) || viewerOpened) {
                return;
            }
            const urlText = line.slice(LIVE_VIEWER_READY_PREFIX.length).trim();
            if (!urlText) {
                return;
            }
            viewerOpened = true;
            const url = vscode.Uri.parse(urlText);
            if (scenario.viewerPreferExternal === true) {
                void vscode.env.openExternal(url);
            } else {
                void openLiveViewerInPanel(url).catch((error) => {
                    const detail = error instanceof Error ? error.message : String(error);
                    vscode.window.showErrorMessage(`Could not open Rumoca viewer in VS Code: ${detail}`);
                });
            }
        };
        const handleOutput = (chunk: Buffer) => {
            const text = chunk.toString();
            rememberInteractiveOutput(recentOutputLines, text);
            lineBuffer += text.replace(/\r/g, '');
            const lines = lineBuffer.split('\n');
            lineBuffer = lines.pop() ?? '';
            const visibleLines = [];
            for (const line of lines) {
                openViewerFromReadyLine(line);
                if (!line.startsWith(LIVE_VIEWER_READY_PREFIX)) {
                    visibleLines.push(line);
                }
            }
            if (text.includes(LIVE_VIEWER_READY_PREFIX)) {
                if (visibleLines.length > 0) {
                    writeEmitter.fire(`${visibleLines.join('\r\n')}\r\n`);
                }
                return;
            }
            writeEmitter.fire(text.replace(/\r?\n/g, '\r\n'));
        };
        const terminal = vscode.window.createTerminal({
            name: `Rumoca: ${path.basename(document.uri.fsPath)}`,
            pty: {
                onDidWrite: writeEmitter.event,
                onDidClose: closeEmitter.event,
                open: () => {
                    writeEmitter.fire(`${[rumocaPath, ...args].map(shellQuote).join(' ')}\r\n`);
                    // The runner always emits the `rumoca-viewer-ready <url>`
                    // marker on stderr once the viewer server is up; we grep
                    // stderr for it below (no env var needed).
                    child = spawn(rumocaPath, args, {
                        cwd: path.dirname(document.uri.fsPath),
                        env: { ...process.env },
                    });
                    child.stdout.on('data', handleOutput);
                    child.stderr.on('data', handleOutput);
                    child.on('error', (error) => {
                        writeEmitter.fire(`Failed to start rumoca: ${String(error)}\r\n`);
                        closeEmitter.fire(1);
                    });
                    child.on('exit', (code) => {
                        const exitCode = code ?? 0;
                        if (exitCode !== 0) {
                            writeEmitter.fire(`\r\nRumoca exited with code ${exitCode}.\r\n`);
                        }
                        if (!viewerOpened && expectsInputViewer) {
                            const summary = summarizeInteractiveFailure(recentOutputLines);
                            const detail = summary ? ` ${summary}` : ' See the Rumoca terminal for details.';
                            const message = `Rumoca exited before the input viewer was ready (exit code ${exitCode}).${detail}`;
                            if (exitCode !== 0) {
                                const model = trimMaybeString(scenario.model);
                                if (model) {
                                    setSimulationDiagnostic(document, model, message);
                                }
                                vscode.window.showErrorMessage(message);
                            } else {
                                vscode.window.showWarningMessage(message);
                            }
                        }
                        closeEmitter.fire(0);
                    });
                },
                close: () => {
                    child?.kill('SIGINT');
                },
                handleInput: (data: string) => {
                    if (data === '\x03') {
                        child?.kill('SIGINT');
                        return;
                    }
                    child?.stdin.write(data);
                },
            },
        });
        terminal.show();
        if (expectsInputViewer) {
            vscode.window.showInformationMessage('Started Rumoca input-enabled simulation; viewer opens after compile.');
        } else {
            vscode.window.showInformationMessage('Started Rumoca simulation in the terminal.');
        }
    };

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
                        const runPath = modelRef?.runPath
                            ?? loadVisualizationShared().simulationRunDocumentPath(modelRef?.runId ?? '');
                        const absRunPath = resolveResultDocumentPath(modelRef?.workspaceRoot, runPath);
                        if (absRunPath) {
                            try {
                                const text = await fs.promises.readFile(
                                    absRunPath,
                                    'utf-8',
                                );
                                const persisted = loadVisualizationShared()
                                    .normalizePersistedSimulationRun(JSON.parse(text));
                                if (persisted) {
                                    return {
                                        views: persisted.views.length > 0
                                            ? persisted.views
                                            : defaultVisualizationViews(),
                                    };
                                }
                            } catch {
                                // Fall through to default views for missing or invalid result JSON.
                            }
                        }
                        return { views: defaultVisualizationViews() };
                    },
                    saveViews: async ({ modelRef, payload }) => {
                        const rawPayload = payload && typeof payload === 'object'
                            ? payload as Record<string, unknown>
                            : {};
                        return { views: normalizeVisualizationViews(rawPayload.views) };
                    },
                    resetViews: async ({ modelRef }) => {
                        const runPath = modelRef?.runPath
                            ?? loadVisualizationShared().simulationRunDocumentPath(modelRef?.runId ?? '');
                        const absRunPath = resolveResultDocumentPath(modelRef?.workspaceRoot, runPath);
                        if (absRunPath) {
                            try {
                                const text = await fs.promises.readFile(
                                    absRunPath,
                                    'utf-8',
                                );
                                const persisted = loadVisualizationShared()
                                    .normalizePersistedSimulationRun(JSON.parse(text));
                                if (persisted) {
                                    return {
                                        views: persisted.views.length > 0
                                            ? persisted.views
                                            : defaultVisualizationViews(),
                                    };
                                }
                            } catch {
                                // Fall through to default views for missing or invalid result JSON.
                            }
                        }
                        return { views: defaultVisualizationViews() };
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
            uplotCss: webview.asWebviewUri(vscode.Uri.joinPath(vendorRoot, 'uplot.min.css')).toString(),
            resultsAppJs: webview.asWebviewUri(vscode.Uri.joinPath(vendorRoot, 'results_app.js')).toString(),
            resultsAppCss: webview.asWebviewUri(vscode.Uri.joinPath(vendorRoot, 'results_app.css')).toString(),
        };
    };

    const getExternalResultsAssets = (): ResultsWebviewAssets => {
        const vendorRoot = vscode.Uri.joinPath(context.extensionUri, 'media', 'vendor');
        return {
            uplotCss: vscode.Uri.joinPath(vendorRoot, 'uplot.min.css').toString(),
            resultsAppJs: vscode.Uri.joinPath(vendorRoot, 'results_app.js').toString(),
            resultsAppCss: vscode.Uri.joinPath(vendorRoot, 'results_app.css').toString(),
        };
    };

    const openResultsPanelForRun = async (
        model: string,
        payload: ParsedSimulationPayload | undefined,
        views: VisualizationView[],
        metrics: SimulationRunMetrics | undefined,
        workspaceRoot: string | undefined,
        runId: string | undefined,
        runPath: string | undefined,
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
            runPath,
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

    const openResultsExternalForRun = async (
        model: string,
        payload: ParsedSimulationPayload | undefined,
        views: VisualizationView[],
        metrics: SimulationRunMetrics | undefined,
        title: string,
    ): Promise<void> => {
        const html = buildResultsWebviewHtml(
            model,
            payload,
            views,
            metrics,
            undefined,
            getExternalResultsAssets(),
        );
        const stem = sanitizeScenarioStem(`${modelLeafName(model)}_${Date.now().toString()}`);
        const outputPath = path.join(os.tmpdir(), `${stem}_results.html`);
        await fs.promises.writeFile(outputPath, html, 'utf-8');
        await vscode.env.openExternal(vscode.Uri.file(outputPath));
        log(`Opened external results report for ${title}: ${outputPath}`);
    };

    const buildResultsJsonEditorHtml = (
        document: vscode.TextDocument,
        webview: vscode.Webview,
        activeViewId?: string,
    ): string => {
        let raw: unknown;
        try {
            raw = JSON.parse(document.getText());
        } catch (error) {
            return resultsJsonErrorDocument(
                document,
                `Invalid JSON: ${String(error instanceof Error ? error.message : error)}`,
            );
        }
        const persisted = loadVisualizationShared().normalizePersistedSimulationRun(raw);
        if (!persisted || !persisted.payload) {
            return resultsJsonErrorDocument(
                document,
                'Missing Rumoca result fields: runId, model, payload.names, payload.allData, and payload.nStates.',
            );
        }
        const workspaceRoot = vscode.workspace.getWorkspaceFolder(document.uri)?.uri.fsPath;
        const title = loadVisualizationShared().buildHostedResultsPanelTitle({
            model: persisted.model,
            title: path.basename(document.uri.fsPath),
        });
        const panelState = loadVisualizationShared().buildHostedResultsPanelState({
            runId: persisted.runId,
            runPath: workspaceRelativeTemplatePath(workspaceRoot, document.uri.fsPath),
            model: persisted.model,
            workspaceRoot,
            title,
            activeViewId,
        });
        const views = persisted.views.length > 0 ? persisted.views : defaultVisualizationViews();
        return buildResultsWebviewHtml(
            persisted.model,
            persisted.payload,
            views,
            persisted.metrics,
            panelState,
            getResultsWebviewAssets(webview),
        );
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
                const { runId, runPath: persistedRunPath, model, workspaceRoot } = restoredState;
                const runPath = persistedRunPath
                    ?? loadVisualizationShared().simulationRunDocumentPath(runId ?? '');
                let persisted: PersistedSimulationRun | undefined;
                const absRunPath = resolveResultDocumentPath(workspaceRoot, runPath);
                if (absRunPath) {
                    try {
                        const runJson = await fs.promises.readFile(
                            absRunPath,
                            'utf-8',
                        );
                        persisted = loadVisualizationShared().normalizePersistedSimulationRun(JSON.parse(runJson));
                    } catch {
                        persisted = undefined;
                    }
                }
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
                    runPath,
                    model,
                    workspaceRoot,
                    title: restoredTitle,
                    activeViewId: restoredState.activeViewId,
                });
                webviewPanel.webview.html = buildResultsWebviewHtml(
                    model,
                    persisted.payload,
                    persisted.views.length > 0 ? persisted.views : defaultVisualizationViews(),
                    persisted.metrics,
                    nextState,
                    getResultsWebviewAssets(webviewPanel.webview),
                );
            },
        }),
    );

    const openResultsRawJson = async (
        document: vscode.TextDocument,
        viewColumn?: vscode.ViewColumn,
    ): Promise<void> => {
        try {
            await vscode.commands.executeCommand(
                'vscode.openWith',
                document.uri,
                'default',
                {
                    preview: false,
                    viewColumn: viewColumn ?? vscode.ViewColumn.Active,
                },
            );
        } catch {
            await vscode.window.showTextDocument(document, {
                preview: false,
                viewColumn: viewColumn ?? vscode.ViewColumn.Active,
            });
        }
    };

    const resultsJsonProvider: vscode.CustomTextEditorProvider = {
        async resolveCustomTextEditor(document, webviewPanel) {
            webviewPanel.webview.options = resultsWebviewOptions();

            const refresh = () => {
                webviewPanel.webview.html = buildResultsJsonEditorHtml(
                    document,
                    webviewPanel.webview,
                );
            };

            wireResultsPanelMessageHandling(webviewPanel);
            const rawMessageDisposable = webviewPanel.webview.onDidReceiveMessage(async (message) => {
                if (message?.command === 'resultsJson.openRaw') {
                    await openResultsRawJson(document, webviewPanel.viewColumn);
                }
            });

            const documentChangeDisposable = vscode.workspace.onDidChangeTextDocument((event) => {
                if (event.document.uri.toString() === document.uri.toString()) {
                    refresh();
                }
            });

            webviewPanel.onDidDispose(() => {
                rawMessageDisposable.dispose();
                documentChangeDisposable.dispose();
            });

            refresh();
        },
    };

    context.subscriptions.push(
        vscode.window.registerCustomEditorProvider(
            'rumoca.results',
            resultsJsonProvider,
            {
                webviewOptions: {
                    retainContextWhenHidden: true,
                },
            },
        ),
    );

    const openScenarioConfigEditor = async (
        document: vscode.TextDocument,
        viewColumn?: vscode.ViewColumn,
    ): Promise<void> => {
        try {
            await vscode.commands.executeCommand(
                'vscode.openWith',
                document.uri,
                'rumoca.scenarioConfig',
                {
                    preview: false,
                    viewColumn: viewColumn ?? vscode.ViewColumn.Active,
                },
            );
        } catch {
            await vscode.window.showTextDocument(document, {
                preview: false,
                viewColumn: viewColumn ?? vscode.ViewColumn.Active,
            });
        }
    };

    const openScenarioRawToml = async (
        document: vscode.TextDocument,
        viewColumn?: vscode.ViewColumn,
    ): Promise<void> => {
        try {
            await vscode.commands.executeCommand(
                'vscode.openWith',
                document.uri,
                'default',
                {
                    preview: false,
                    viewColumn: viewColumn ?? vscode.ViewColumn.Active,
                },
            );
        } catch {
            await vscode.window.showTextDocument(document, {
                preview: false,
                viewColumn: viewColumn ?? vscode.ViewColumn.Active,
            });
        }
    };

    const scenarioModelPath = (
        document: vscode.TextDocument,
        modelFile: string,
    ): string | undefined => {
        const raw = trimMaybeString(modelFile);
        if (!raw) {
            return undefined;
        }
        return path.isAbsolute(raw)
            ? raw
            : path.resolve(path.dirname(document.uri.fsPath), raw);
    };

    const loadScenarioParameterMetadata = async (
        document: vscode.TextDocument,
        response: ScenarioConfigFullResponse,
        payload?: unknown,
    ): Promise<unknown[]> => {
        const config = response.config ?? {};
        const modelConfig = config.model && typeof config.model === 'object'
            ? config.model as Record<string, unknown>
            : {};
        const payloadConfig = payload && typeof payload === 'object'
            ? payload as Record<string, unknown>
            : {};
        const modelName = trimMaybeString(payloadConfig.modelName)
            || (typeof modelConfig.name === 'string' ? modelConfig.name.trim() : '');
        const modelFile = trimMaybeString(payloadConfig.modelFile)
            || (typeof modelConfig.file === 'string' ? modelConfig.file.trim() : '');
        const modelPath = scenarioModelPath(document, modelFile);
        if (!modelName || !modelPath) {
            return [];
        }
        try {
            return await getModelParameterMetadata({
                uri: vscode.Uri.file(modelPath).toString(),
                modelName,
                fallback: {
                    sourceRootPaths: Array.isArray(response.effectiveSourceRootPaths)
                        ? response.effectiveSourceRootPaths
                        : [],
                },
            });
        } catch {
            return [];
        }
    };

    const buildScenarioConfigEditorHtml = async (document: vscode.TextDocument): Promise<string> => {
        const response = await getScenarioConfigFull(document.uri.toString(), document.getText());
        if (response?.ok !== true || !response.config) {
            return scenarioConfigErrorDocument(
                document,
                response?.error ?? 'rumoca-lsp did not return a scenario config.',
            );
        }
        return loadVisualizationShared().buildScenarioConfigDocument({
            ...response,
            path: path.basename(document.uri.fsPath),
        });
    };

    const scenarioEditorProvider: vscode.CustomTextEditorProvider = {
        async resolveCustomTextEditor(document, webviewPanel) {
            webviewPanel.webview.options = {
                enableScripts: true,
            };

            const refresh = async () => {
                try {
                    webviewPanel.webview.html = await buildScenarioConfigEditorHtml(document);
                } catch (error) {
                    webviewPanel.webview.html = scenarioConfigErrorDocument(
                        document,
                        String(error instanceof Error ? error.message : error),
                    );
                }
            };

            const respond = async (
                requestId: string,
                ok: boolean,
                value?: unknown,
                error?: string,
            ) => {
                await webviewPanel.webview.postMessage({
                    command: 'scenarioConfig.response',
                    requestId,
                    ok,
                    value,
                    error,
                });
            };

            const handleSave = async (payload: unknown): Promise<unknown> => {
                const latest = await getScenarioConfigFull(document.uri.toString(), document.getText());
                if (latest?.ok !== true || !latest.config) {
                    throw new Error(latest?.error ?? 'failed to load scenario config before saving');
                }
                const edits = payload && typeof payload === 'object'
                    ? (payload as { edits?: unknown }).edits
                    : undefined;
                const config = loadVisualizationShared().applyScenarioConfigEdits(latest.config, edits);
                await setScenarioConfig(document.uri.toString(), config, {
                    replaceDirtyOpenDocument: true,
                });
                await refresh();
                return await getScenarioConfigFull(document.uri.toString(), document.getText());
            };

            const handleRun = async (payload: unknown): Promise<unknown> => {
                await handleSave(payload);
                await vscode.commands.executeCommand('rumoca.simulateModel', document.uri);
                return { ok: true, message: 'Scenario run requested' };
            };

            const handleParameterMetadata = async (payload: unknown): Promise<unknown[]> => {
                const latest = await getScenarioConfigFull(document.uri.toString(), document.getText());
                if (latest?.ok !== true || !latest.config) {
                    return [];
                }
                return await loadScenarioParameterMetadata(document, latest, payload);
            };

            const browseSourceRoot = async (): Promise<{ path: string } | null> => {
                const selected = await vscode.window.showOpenDialog({
                    title: 'Select Modelica Source Root',
                    canSelectFiles: false,
                    canSelectFolders: true,
                    canSelectMany: false,
                    defaultUri: vscode.Uri.file(path.dirname(document.uri.fsPath)),
                });
                const selectedUri = selected?.[0];
                if (!selectedUri) {
                    return null;
                }
                const relativePath = path
                    .relative(path.dirname(document.uri.fsPath), selectedUri.fsPath)
                    .replace(/\\/g, '/');
                return { path: relativePath || '.' };
            };

            const messageDisposable = webviewPanel.webview.onDidReceiveMessage(async (message) => {
                if (!message || message.command !== 'scenarioConfig.request') {
                    return;
                }
                const requestId = typeof message.requestId === 'string' ? message.requestId : '';
                const method = typeof message.method === 'string' ? message.method : '';
                try {
                    if (method === 'save') {
                        const value = await handleSave(message.payload);
                        await respond(requestId, true, value);
                        return;
                    }
                    if (method === 'run') {
                        await respond(requestId, true, await handleRun(message.payload));
                        return;
                    }
                    if (method === 'toggleRaw') {
                        await openScenarioRawToml(document, webviewPanel.viewColumn);
                        await respond(requestId, true, { ok: true });
                        return;
                    }
                    if (method === 'browseSourceRoot') {
                        await respond(requestId, true, await browseSourceRoot());
                        return;
                    }
                    if (method === 'parameterMetadata') {
                        await respond(requestId, true, await handleParameterMetadata(message.payload));
                        return;
                    }
                    await respond(requestId, false, undefined, `Unknown scenario config request: ${method}`);
                } catch (error) {
                    const detail = String(error instanceof Error ? error.message : error);
                    await respond(requestId, false, undefined, detail);
                    void vscode.window.showErrorMessage(`Scenario config request failed: ${detail}`);
                }
            });

            const saveDisposable = vscode.workspace.onDidSaveTextDocument((savedDocument) => {
                if (savedDocument.uri.toString() === document.uri.toString()) {
                    void refresh();
                }
            });
            const viewStateDisposable = webviewPanel.onDidChangeViewState((event) => {
                if (event.webviewPanel.active) {
                    void refresh();
                }
            });

            webviewPanel.onDidDispose(() => {
                messageDisposable.dispose();
                saveDisposable.dispose();
                viewStateDisposable.dispose();
            });

            await refresh();
        },
    };
    context.subscriptions.push(
        vscode.window.registerCustomEditorProvider(
            'rumoca.scenarioConfig',
            scenarioEditorProvider,
            {
                webviewOptions: {
                    retainContextWhenHidden: true,
                },
                supportsMultipleEditorsPerDocument: false,
            },
        ),
    );

    const currentModelicaEditor = (): vscode.TextEditor | undefined => {
        const editor = vscode.window.activeTextEditor;
        if (!editor || editor.document.languageId !== 'modelica') {
            return undefined;
        }
        return editor;
    };

    const simulationDiagnostics = vscode.languages.createDiagnosticCollection('rumoca simulation');
    context.subscriptions.push(simulationDiagnostics);
    let lastSimulationDiagnosticUri: vscode.Uri | undefined;

    const firstContentRange = (document: vscode.TextDocument): vscode.Range => {
        for (let line = 0; line < document.lineCount; line++) {
            const textLine = document.lineAt(line);
            const firstContent = textLine.firstNonWhitespaceCharacterIndex;
            if (!textLine.isEmptyOrWhitespace) {
                const end = Math.max(firstContent + 1, textLine.text.length);
                return new vscode.Range(line, firstContent, line, end);
            }
        }
        return new vscode.Range(0, 0, 0, 0);
    };

    const modelDeclarationRange = (
        document: vscode.TextDocument,
        model: string,
    ): vscode.Range => {
        const shortName = model.split('.').filter(Boolean).pop();
        if (!shortName) {
            return firstContentRange(document);
        }
        const escapedName = shortName.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
        const declarationPattern = new RegExp(
            `\\b(model|block|class|connector|function|package|record)\\s+${escapedName}\\b`,
        );
        for (let line = 0; line < document.lineCount; line++) {
            const text = document.lineAt(line).text;
            const match = declarationPattern.exec(text);
            if (match?.index !== undefined) {
                return new vscode.Range(line, match.index, line, match.index + match[0].length);
            }
        }
        return firstContentRange(document);
    };

    const setSimulationDiagnostic = (
        document: vscode.TextDocument,
        model: string,
        message: string,
        diagnosticLocation?: SimulationDiagnosticLocation,
    ) => {
        const diagnosticUri = diagnosticLocation?.uri
            ? vscode.Uri.parse(diagnosticLocation.uri)
            : document.uri;
        const range = diagnosticLocation?.range
            ? new vscode.Range(
                diagnosticLocation.range.start?.line ?? 0,
                diagnosticLocation.range.start?.character ?? 0,
                diagnosticLocation.range.end?.line ?? 0,
                diagnosticLocation.range.end?.character ?? 1,
            )
            : modelDeclarationRange(document, model);
        const diagnostic = new vscode.Diagnostic(
            range,
            `Simulation failed for ${model}: ${message}`,
            vscode.DiagnosticSeverity.Error,
        );
        diagnostic.source = diagnosticLocation?.source ?? 'Rumoca Simulation';
        diagnostic.code = diagnosticLocation?.code ?? 'simulation';
        simulationDiagnostics.set(diagnosticUri, [diagnostic]);
        lastSimulationDiagnosticUri = diagnosticUri;
    };

    const setScenarioDiagnostic = (document: vscode.TextDocument, message: string) => {
        const diagnostic = new vscode.Diagnostic(
            firstContentRange(document),
            `Scenario config error: ${message}`,
            vscode.DiagnosticSeverity.Error,
        );
        diagnostic.source = 'Rumoca Scenario';
        diagnostic.code = 'scenario';
        simulationDiagnostics.set(document.uri, [diagnostic]);
        lastSimulationDiagnosticUri = document.uri;
    };

    const clearSimulationDiagnostic = (document: vscode.TextDocument) => {
        simulationDiagnostics.delete(document.uri);
        if (lastSimulationDiagnosticUri) {
            simulationDiagnostics.delete(lastSimulationDiagnosticUri);
            lastSimulationDiagnosticUri = undefined;
        }
    };

    const createScenarioConfigCommand = vscode.commands.registerCommand(
        'rumoca.createScenarioConfig',
        async () => {
            const editor = vscode.window.activeTextEditor;
            if (!editor || !isModelicaSourceDocument(editor.document)) {
                vscode.window.showErrorMessage('Open a Modelica source file to create a Rumoca scenario.');
                return;
            }
            const document = editor.document;
            if (document.isUntitled) {
                vscode.window.showErrorMessage('Save the Modelica file before creating a Rumoca scenario.');
                return;
            }
            if (document.isDirty) {
                const saved = await document.save();
                if (!saved) {
                    vscode.window.showWarningMessage('Save the Modelica file before creating a Rumoca scenario.');
                    return;
                }
            }

            const defaults = getSimulationSettings(vscode.workspace.getConfiguration('rumoca'));
            const modelState = await getSimulationModelState(document.uri.toString(), defaults.model);
            if (!modelState.ok || modelState.models.length === 0) {
                vscode.window.showErrorMessage(
                    modelState.error ?? 'No model/block/class declarations were found in this file.',
                );
                return;
            }
            const selectedModel = modelState.selectedModel && modelState.models.includes(modelState.selectedModel)
                ? modelState.selectedModel
                : modelState.models[0];
            if (!selectedModel) {
                return;
            }
            const defaultStem = sanitizeScenarioStem(
                `${modelLeafName(selectedModel)}_sim`,
            );
            const defaultScenarioPath = path.join(
                path.dirname(document.uri.fsPath),
                `rumoca-scenario.${defaultStem}.toml`,
            );
            const targetUri = await vscode.window.showSaveDialog({
                title: 'Create Rumoca Scenario',
                defaultUri: vscode.Uri.file(defaultScenarioPath),
                filters: {
                    'Rumoca scenario': ['toml'],
                },
            });
            if (!targetUri) {
                return;
            }
            const scenarioPath = targetUri.fsPath;
            const text = await renderScenarioConfig(buildScenarioConfig({
                task: 'simulate',
                model: selectedModel,
                modelFile: relativeModelFileForScenario(document),
                sourceRoots: defaultSourceRootsForScenario(selectedModel),
            }));
            await fs.promises.writeFile(scenarioPath, text, 'utf-8');
            const scenarioDocument = await vscode.workspace.openTextDocument(scenarioPath);
            await openScenarioConfigEditor(scenarioDocument);
            vscode.window.showInformationMessage(`Created ${path.basename(scenarioPath)}.`);
        },
    );
    context.subscriptions.push(createScenarioConfigCommand);

    const renderConfiguredCodegenScenario = async (
        document: vscode.TextDocument,
        scenario: ScenarioConfigResponse,
    ): Promise<void> => {
        const model = trimMaybeString(scenario.model);
        const target = trimMaybeString(scenario.target);
        if (!model || !target) {
            throw new Error('Codegen scenario requires [model].name and [codegen].target.');
        }
        const workspaceRoot = resolveWorkspaceRootForDocument(document) ?? resolveWorkspaceRootFallback();
        const renderedFiles = await vscode.window.withProgress(
            {
                location: vscode.ProgressLocation.Notification,
                title: `Rendering ${model} with ${target}...`,
                cancellable: false,
            },
            async () => {
                const response = await sendWorkspaceCommand<CodegenTargetRenderResponse>(
                    'rumoca.workspace.renderTarget',
                    { uri: document.uri.toString(), model, target },
                );
                if (!response?.ok || !Array.isArray(response.files)) {
                    throw new Error(response?.error ?? 'rumoca-lsp did not return rendered target output.');
                }
                return response.files;
            },
        );
        if (renderedFiles.length === 0) {
            throw new Error('Target rendered no files.');
        }
        const outputRoot = resolveScenarioOutputDir(document, scenario.outputDir);
        await fs.promises.mkdir(outputRoot, { recursive: true });
        const writtenPaths = [];
        for (const file of renderedFiles) {
            const outputPath = resolveTargetOutputPath(outputRoot, file.path);
            await fs.promises.mkdir(path.dirname(outputPath), { recursive: true });
            await fs.promises.writeFile(outputPath, file.content, 'utf-8');
            writtenPaths.push(outputPath);
        }
        const firstDocument = await vscode.workspace.openTextDocument(writtenPaths[0]);
        await vscode.window.showTextDocument(firstDocument, {
            preview: false,
            viewColumn: vscode.ViewColumn.Beside,
        });
        const relativeOutput = workspaceRoot
            ? path.relative(workspaceRoot, outputRoot).split(path.sep).join('/')
            : outputRoot;
        vscode.window.showInformationMessage(
            `Rendered ${renderedFiles.length} file${renderedFiles.length === 1 ? '' : 's'} to ${relativeOutput}.`,
        );
        log(`Rendered target for ${model}: ${target} -> ${outputRoot}`);
    };

    const simulateCommand = vscode.commands.registerCommand('rumoca.simulateModel', async (resource?: vscode.Uri) => {
        const document = await scenarioDocumentFromCommandResource(resource);
        if (!document) {
            vscode.window.showErrorMessage('No active Rumoca scenario editor.');
            return;
        }

        if (document.isUntitled || document.isDirty) {
            const saved = await document.save();
            if (!saved) {
                vscode.window.showWarningMessage('Save the active simulation file before simulation.');
                return;
            }
        }

        const documentUri = document.uri.toString();
        const runConfig = vscode.workspace.getConfiguration('rumoca');
        const defaults = getSimulationSettings(runConfig);
        clearSimulationDiagnostic(document);
        const scenario = await getScenarioConfig(documentUri);
        if (!scenario?.ok) {
            const detail = scenario?.error ?? 'Failed to load Rumoca scenario.';
            setScenarioDiagnostic(document, detail);
            vscode.window.showErrorMessage(detail);
            return;
        }
        const model = trimMaybeString(scenario.model);
        if (!model) {
            const detail = 'Could not infer a model name from the scenario. Set [model].name.';
            setScenarioDiagnostic(document, detail);
            vscode.window.showErrorMessage(detail);
            return;
        }
        await storeSelectedSimulationModel(context, documentUri, model);
        if (scenario.task === 'codegen') {
            try {
                await renderConfiguredCodegenScenario(document, scenario);
            } catch (error) {
                const detail = error instanceof Error ? error.message : String(error);
                log(`Codegen scenario failed for ${model}: ${detail}`);
                vscode.window.showErrorMessage(`Codegen failed: ${detail}`);
            }
            return;
        }
        if (scenarioNeedsInputRunner(scenario)) {
            try {
                await startInteractiveScenario(document, scenario);
                clearSimulationDiagnostic(document);
            } catch (error) {
                const detail = error instanceof Error ? error.message : String(error);
                log(`Interactive scenario failed for ${model}: ${detail}`);
                setSimulationDiagnostic(document, model, detail);
                vscode.window.showErrorMessage(`Interactive simulation failed: ${detail}`);
            }
            return;
        }

        const workspaceRoot = resolveWorkspaceRootForDocument(document) ?? resolveWorkspaceRootFallback();
        const scenarioConfig = await getScenarioSimulationConfig(model, workspaceRoot, defaults);
        const scenarioConfigFull = await getScenarioConfigFull(document.uri.toString(), document.getText());
        const parameterOverrides = scenarioParameterOverrides(scenarioConfigFull?.config);
        const settings = {
            ...(scenarioConfig?.effective ?? defaults),
            parameterOverrides,
        };
        for (const diagnostic of scenarioConfig?.diagnostics ?? []) {
            log(`scenario config warning: ${diagnostic}`);
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
                setSimulationDiagnostic(document, model, details, result.diagnostic);
                vscode.window.showErrorMessage(`Simulation failed: ${details}`);
                return;
            }
            clearSimulationDiagnostic(document);

            const resultDirectory = scenarioResultDirectory(
                document,
                settings.outputDir,
                workspaceRoot,
                scenarioConfig?.scenarioPath,
            );
            const persistedRun = await loadVisualizationShared().persistHostedSimulationRunWithViews({
                model,
                workspaceRoot,
                payload: result.payload,
                metrics: result.metrics,
                loadConfiguredViews: async ({ model, workspaceRoot }) =>
                    await getScenarioVisualizationConfig(model, workspaceRoot),
                hydrateViews: async ({ model, workspaceRoot, views }) =>
                    await buildWorkspaceVisualizationViewStorage(workspaceRoot).hydrateViews({
                        views,
                        model,
                    }),
                defaultViews: defaultVisualizationViews(),
                resultDirectory,
                pathExists: (relativeRunPath) => {
                    const absPath = resolveResultDocumentPath(workspaceRoot, relativeRunPath);
                    return Boolean(absPath && fs.existsSync(absPath));
                },
                writeTextFile: async (relativeRunPath, content) => {
                    const absPath = resolveResultDocumentPath(workspaceRoot, relativeRunPath);
                    if (!absPath) {
                        throw new Error('Unable to resolve simulation result output path.');
                    }
                    await fs.promises.mkdir(path.dirname(absPath), { recursive: true });
                    await fs.promises.writeFile(absPath, content, 'utf-8');
                },
            });
            const views = await loadHydratedVisualizationViews(model, workspaceRoot);

            const timestamp = new Date().toLocaleTimeString([], { hour12: false });
            const title = loadVisualizationShared().buildHostedResultsPanelTitle({ model, timestamp });
            if (scenario.viewerPreferExternal === true) {
                await openResultsExternalForRun(model, result.payload, views, result.metrics, title);
            } else {
                await openResultsPanelForRun(
                    model,
                    result.payload,
                    views,
                    result.metrics,
                    workspaceRoot,
                    persistedRun?.runId,
                    persistedRun?.runPath,
                    title,
                );
            }
            log(`Simulation completed for ${model}`);
        } catch (error) {
            const message = error instanceof Error ? error.message : String(error);
            log(`Simulation error: ${message}`);
            setSimulationDiagnostic(document, model, message);
            vscode.window.showErrorMessage(`Simulation failed: ${message}`);
        }
    });
    context.subscriptions.push(simulateCommand);

    const simulationSettingsCommand = vscode.commands.registerCommand(
        'rumoca.openSimulationSettings',
        async (resource?: vscode.Uri) => {
            const document = await scenarioDocumentFromCommandResource(resource);
            if (!document) {
                vscode.window.showErrorMessage('Open a Rumoca scenario file to configure it.');
                return;
            }
            await openScenarioConfigEditor(document);
        },
    );
    context.subscriptions.push(simulationSettingsCommand);

    const settingsMenuCommand = vscode.commands.registerCommand(
        'rumoca.openSettingsMenu',
        async () => {
            const document = await scenarioDocumentFromCommandResource(undefined);
            if (document) {
                await openScenarioConfigEditor(document);
                return;
            }
            await vscode.commands.executeCommand('rumoca.createScenarioConfig');
        },
    );
    context.subscriptions.push(settingsMenuCommand);

    const openGuideUrl = (url: string) => vscode.env.openExternal(vscode.Uri.parse(url));
    context.subscriptions.push(
        vscode.commands.registerCommand('rumoca.openUserGuide', () =>
            openGuideUrl('https://cognipilot.github.io/rumoca/user-guide/')),
        vscode.commands.registerCommand('rumoca.openDevGuide', () =>
            openGuideUrl('https://cognipilot.github.io/rumoca/dev-guide/')),
        vscode.commands.registerCommand('rumoca.openPlayground', () =>
            openGuideUrl('https://cognipilot.github.io/rumoca/')),
    );

    const isModelicaPath = (uri: vscode.Uri): boolean => uri.fsPath.toLowerCase().endsWith('.mo');
    context.subscriptions.push(
        vscode.workspace.onDidRenameFiles((event) => {
            if (event.files.some((item) => isModelicaPath(item.oldUri) || isModelicaPath(item.newUri))) {
                log('[config] Modelica files renamed; colocated Rumoca scenarios now follow normal file moves.');
            }
        }),
    );
    const modelicaFsWatcher = vscode.workspace.createFileSystemWatcher('**/*.mo');
    context.subscriptions.push(modelicaFsWatcher);

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
