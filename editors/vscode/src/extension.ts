import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';
import * as vscode from 'vscode';
import { execSync, spawn, spawnSync } from 'child_process';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let outputChannel: vscode.OutputChannel;

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
    modelicaPath: string[];
}

interface SimulationSettings extends SimulationExecutionSettings {
    model: string;
}

interface ModelSimulationPreset {
    tEnd: number;
    dt?: number;
    solver: 'auto' | 'bdf' | 'rk-like';
    outputDir: string;
    libraryOverrides: string[];
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
}

interface ResultsWebviewAssets {
    uplotCss: string;
    uplotJs: string;
    threeJs: string;
}

function escapeHtml(text: string): string {
    return text
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#39;');
}

function inferModelNameFromSource(source: string): string | undefined {
    const match = source.match(/\b(?:model|block|class)\s+([A-Za-z_][A-Za-z0-9_]*)/);
    return match ? match[1] : undefined;
}

function mergeLibraryPathLists(primary: readonly string[], secondary: readonly string[]): string[] {
    const merged: string[] = [];
    const seen = new Set<string>();
    for (const entry of [...primary, ...secondary]) {
        const trimmed = String(entry).trim();
        if (!trimmed) {
            continue;
        }
        const key = process.platform === 'win32' ? trimmed.toLowerCase() : trimmed;
        if (seen.has(key)) {
            continue;
        }
        seen.add(key);
        merged.push(trimmed);
    }
    return merged;
}

function parsePathListEnvVar(name: string): string[] {
    const raw = process.env[name];
    if (!raw) {
        return [];
    }
    return raw
        .split(path.delimiter)
        .map((entry) => entry.trim())
        .filter(Boolean);
}

function resolveModelicaPathSources(config: vscode.WorkspaceConfiguration): {
    configuredPaths: string[];
    environmentPaths: string[];
    mergedPaths: string[];
    usedLegacyAlias: boolean;
} {
    const configuredPaths = (config.get<string[]>('modelicaPath') ?? [])
        .map((entry) => String(entry).trim())
        .filter(Boolean);
    const envModelicaPath = parsePathListEnvVar('MODELICAPATH');
    const envLegacyPath = parsePathListEnvVar('MODELICPATH');
    const environmentPaths = mergeLibraryPathLists(envModelicaPath, envLegacyPath);
    return {
        configuredPaths,
        environmentPaths,
        mergedPaths: mergeLibraryPathLists(configuredPaths, environmentPaths),
        usedLegacyAlias: envLegacyPath.length > 0,
    };
}

function getSimulationSettings(config: vscode.WorkspaceConfiguration): SimulationSettings {
    const dtRaw = config.get<number>('simulation.dt');
    const dt = Number.isFinite(dtRaw) && (dtRaw ?? 0) > 0 ? dtRaw : undefined;
    const solverRaw = (config.get<string>('simulation.solver') ?? 'auto').toLowerCase();
    const solver = solverRaw === 'bdf' || solverRaw === 'rk-like' ? solverRaw : 'auto';
    const modelicaPathSources = resolveModelicaPathSources(config);
    return {
        model: (config.get<string>('simulation.model') ?? '').trim(),
        tEnd: config.get<number>('simulation.tEnd') ?? 10.0,
        dt,
        solver,
        outputDir: (config.get<string>('simulation.outputDir') ?? '').trim(),
        modelicaPath: modelicaPathSources.mergedPaths,
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

function normalizeSimulationPreset(raw: unknown): ModelSimulationPreset | undefined {
    if (!raw || typeof raw !== 'object') {
        return undefined;
    }
    const candidate = raw as Record<string, unknown>;
    const tEnd = Number(candidate.tEnd);
    if (!Number.isFinite(tEnd) || tEnd <= 0) {
        return undefined;
    }

    const solverRaw = String(candidate.solver ?? 'auto').toLowerCase();
    const solver: 'auto' | 'bdf' | 'rk-like' =
        solverRaw === 'bdf' || solverRaw === 'rk-like' ? solverRaw : 'auto';

    let dt: number | undefined;
    if (candidate.dt !== undefined && candidate.dt !== null && String(candidate.dt).trim() !== '') {
        const parsedDt = Number(candidate.dt);
        if (!Number.isFinite(parsedDt) || parsedDt <= 0) {
            dt = undefined;
        } else {
            dt = parsedDt;
        }
    }

    const outputDir = typeof candidate.outputDir === 'string' ? candidate.outputDir : '';
    const libraryOverrides = Array.isArray(candidate.libraryOverrides)
        ? candidate.libraryOverrides
            .filter((entry): entry is string => typeof entry === 'string')
            .map(entry => entry.trim())
            .filter(Boolean)
        : [];

    return {
        tEnd,
        dt,
        solver,
        outputDir,
        libraryOverrides,
    };
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

async function sendProjectCommand<T>(
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

function defaultBouncingBallViewerScript(): string {
    return `// Default Rumoca 3D preset.
// Geometry is defined here so each model can fully customize visuals.
ctx.onInit = (api) => {
  if (typeof api.enableDefaultViewerRuntime === "function") {
    api.enableDefaultViewerRuntime({ selectedObjectName: "ball", followSelected: false });
  }
  const { THREE, state } = api;
  if (!THREE || !state || !state.scene) return;

  state.scene.background = new THREE.Color(0x101010);

  const keyLight = new THREE.DirectionalLight(0xffffff, 1.0);
  keyLight.position.set(2, 4, 3);
  state.scene.add(keyLight);
  state.scene.add(new THREE.AmbientLight(0x404040, 0.9));
  state.scene.add(new THREE.GridHelper(12, 24, 0x2f4f63, 0x2a2a2a));

  const floor = new THREE.Mesh(
    new THREE.BoxGeometry(8, 0.1, 8),
    new THREE.MeshStandardMaterial({ color: 0x444444 })
  );
  floor.position.set(0, -0.05, 0);
  floor.name = "floor";
  state.scene.add(floor);

  const ball = new THREE.Mesh(
    new THREE.SphereGeometry(0.2, 32, 24),
    new THREE.MeshStandardMaterial({ color: 0x3cb4ff })
  );
  ball.name = "ball";
  state.scene.add(ball);
  state.ball = ball;
};

ctx.onFrame = (api) => {
  const ball = api.state ? api.state.ball : null;
  if (ball) {
    const x = Number(api.getValue("x", api.sampleIndex));
    const y = Number(api.getValue("y", api.sampleIndex));
    const z = Number(api.getValue("z", api.sampleIndex));
    ball.position.set(
      Number.isFinite(y) ? y : 0,
      Number.isFinite(x) ? x : 0,
      Number.isFinite(z) ? z : 0
    );
  }
};`;
}

function defaultVisualizationViews(): VisualizationView[] {
    return [
        {
            id: 'states_time',
            title: 'States vs Time',
            type: 'timeseries',
            x: 'time',
            y: ['*states'],
        },
    ];
}

function normalizeVisualizationViews(raw: unknown): VisualizationView[] {
    if (!Array.isArray(raw)) {
        return [];
    }
    const out: VisualizationView[] = [];
    for (const entry of raw) {
        if (!entry || typeof entry !== 'object') {
            continue;
        }
        const obj = entry as Record<string, unknown>;
        const typeRaw = String(obj.type ?? 'timeseries').toLowerCase();
        const type: 'timeseries' | 'scatter' | '3d' =
            typeRaw === 'scatter' || typeRaw === '3d' ? (typeRaw as 'scatter' | '3d') : 'timeseries';
        const id = String(obj.id ?? '').trim();
        const title = String(obj.title ?? '').trim();
        const x = obj.x !== undefined && obj.x !== null ? String(obj.x).trim() : undefined;
        const y = Array.isArray(obj.y)
            ? obj.y.map(v => String(v).trim()).filter(Boolean)
            : [];
        const scriptRaw = obj.script !== undefined && obj.script !== null ? String(obj.script) : '';
        const script = scriptRaw.trim().length > 0 ? scriptRaw : undefined;
        const scriptPathRaw =
            obj.scriptPath !== undefined && obj.scriptPath !== null
                ? String(obj.scriptPath)
                : obj.script_path !== undefined && obj.script_path !== null
                    ? String(obj.script_path)
                    : '';
        const scriptPath = scriptPathRaw.trim().length > 0 ? scriptPathRaw.trim() : undefined;
        const scatterSeriesRaw = Array.isArray(obj.scatterSeries) ? obj.scatterSeries : [];
        const scatterSeries: ScatterSeriesConfig[] = [];
        for (const entry of scatterSeriesRaw) {
            if (!entry || typeof entry !== 'object') {
                continue;
            }
            const seriesObj = entry as Record<string, unknown>;
            const name = String(seriesObj.name ?? '').trim();
            const sx = String(seriesObj.x ?? '').trim();
            const sy = String(seriesObj.y ?? '').trim();
            if (sx.length === 0 || sy.length === 0) {
                continue;
            }
            scatterSeries.push({
                name: name.length > 0 ? name : `${sy} vs ${sx}`,
                x: sx,
                y: sy,
            });
        }
        const normalizedX = type === '3d' ? undefined : x;
        const normalizedY = type === '3d' ? [] : y;
        let normalizedScatterSeries = type === 'scatter' ? [...scatterSeries] : undefined;
        if (type === 'scatter' && (normalizedScatterSeries?.length ?? 0) === 0) {
            const fallbackX = normalizedX && normalizedX.length > 0 ? normalizedX : 'time';
            const fallbackY = normalizedY.length > 0 ? normalizedY[0] : '';
            if (fallbackY.length > 0) {
                normalizedScatterSeries = [
                    {
                        name: `${fallbackY} vs ${fallbackX}`,
                        x: fallbackX,
                        y: fallbackY,
                    },
                ];
            }
        }
        out.push({
            id: id || `view_${out.length + 1}`,
            title: title || `View ${out.length + 1}`,
            type,
            x: normalizedX,
            y: normalizedY,
            scatterSeries: normalizedScatterSeries,
            script,
            scriptPath,
        });
    }
    return out;
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

function sanitizePathSegment(input: string): string {
    const cleaned = input
        .trim()
        .replace(/[^a-zA-Z0-9._-]+/g, '_')
        .replace(/^_+|_+$/g, '');
    return cleaned.length > 0 ? cleaned : 'view';
}

function defaultViewerScriptPath(model: string, viewId: string): string {
    return path.join(
        '.rumoca',
        'viewer3d',
        sanitizePathSegment(model),
        `${sanitizePathSegment(viewId)}.js`,
    );
}

async function hydrateVisualizationViewsFromDisk(
    views: VisualizationView[],
    workspaceRoot: string | undefined,
    model: string,
): Promise<VisualizationView[]> {
    const hydrated: VisualizationView[] = [];
    for (const [idx, view] of views.entries()) {
        const next: VisualizationView = {
            ...view,
            y: [...(view.y ?? [])],
            scatterSeries: Array.isArray(view.scatterSeries)
                ? view.scatterSeries.map(series => ({ ...series }))
                : undefined,
        };
        if (next.type === '3d') {
            let scriptContent = typeof next.script === 'string' ? next.script : '';
            const rawPath = typeof next.scriptPath === 'string' ? next.scriptPath.trim() : '';
            const fallbackScript = defaultBouncingBallViewerScript();
            if (!scriptContent && rawPath && workspaceRoot) {
                const absPath = path.isAbsolute(rawPath) ? rawPath : path.join(workspaceRoot, rawPath);
                try {
                    scriptContent = await fs.promises.readFile(absPath, 'utf-8');
                } catch {
                    scriptContent = fallbackScript;
                    // Only seed default script when the file truly does not exist.
                    // Never overwrite an existing user script from a read failure path.
                    let exists = true;
                    try {
                        await fs.promises.access(absPath, fs.constants.F_OK);
                    } catch {
                        exists = false;
                    }
                    if (!exists) {
                        try {
                            await fs.promises.mkdir(path.dirname(absPath), { recursive: true });
                            await fs.promises.writeFile(absPath, scriptContent, 'utf-8');
                        } catch {
                            // best effort only; keep in-memory fallback
                        }
                    }
                }
            }
            if (!scriptContent || scriptContent.trim().length === 0) {
                scriptContent = fallbackScript;
            }
            next.script = scriptContent;
            if (!next.scriptPath || next.scriptPath.trim().length === 0) {
                const id = next.id || `view_${idx + 1}`;
                next.scriptPath = defaultViewerScriptPath(model, id);
            }
        }
        hydrated.push(next);
    }
    return hydrated;
}

async function persistVisualizationScripts(
    views: VisualizationView[],
    workspaceRoot: string | undefined,
    model: string,
): Promise<VisualizationView[]> {
    const persisted: VisualizationView[] = [];
    for (const [idx, view] of views.entries()) {
        const next: VisualizationView = {
            ...view,
            y: [...(view.y ?? [])],
            scatterSeries: Array.isArray(view.scatterSeries)
                ? view.scatterSeries.map(series => ({ ...series }))
                : undefined,
        };
        if (next.type === '3d') {
            const id = next.id || `view_${idx + 1}`;
            const scriptPathRaw =
                typeof next.scriptPath === 'string' && next.scriptPath.trim().length > 0
                    ? next.scriptPath.trim()
                    : defaultViewerScriptPath(model, id);

            if (workspaceRoot) {
                const absPath = path.isAbsolute(scriptPathRaw)
                    ? scriptPathRaw
                    : path.join(workspaceRoot, scriptPathRaw);
                await fs.promises.mkdir(path.dirname(absPath), { recursive: true });
                const hasExplicitScript =
                    typeof next.script === 'string' && next.script.trim().length > 0;
                if (hasExplicitScript) {
                    await fs.promises.writeFile(absPath, next.script as string, 'utf-8');
                } else {
                    // Do not clobber existing user script content.
                    // Only seed a default script when no script file exists yet.
                    let exists = true;
                    try {
                        await fs.promises.access(absPath, fs.constants.F_OK);
                    } catch {
                        exists = false;
                    }
                    if (!exists) {
                        await fs.promises.writeFile(absPath, defaultBouncingBallViewerScript(), 'utf-8');
                    }
                }
                next.scriptPath = path.isAbsolute(scriptPathRaw)
                    ? scriptPathRaw
                    : scriptPathRaw.split(path.sep).join('/');
            } else {
                next.scriptPath = scriptPathRaw;
            }
            next.script = undefined;
        } else {
            next.script = undefined;
            next.scriptPath = undefined;
        }
        persisted.push(next);
    }
    return persisted;
}

function normalizeSimulationRunMetrics(raw: unknown): SimulationRunMetrics | undefined {
    if (!raw || typeof raw !== 'object') {
        return undefined;
    }
    const obj = raw as Record<string, unknown>;
    const compileSeconds = Number(obj.compileSeconds);
    const simulateSeconds = Number(obj.simulateSeconds);
    const points = Number(obj.points);
    const variables = Number(obj.variables);
    if (
        !Number.isFinite(compileSeconds) ||
        !Number.isFinite(simulateSeconds) ||
        !Number.isFinite(points) ||
        !Number.isFinite(variables)
    ) {
        return undefined;
    }

    let compilePhaseSeconds: CompilePhaseSeconds | undefined;
    if (obj.compilePhaseSeconds && typeof obj.compilePhaseSeconds === 'object') {
        const phase = obj.compilePhaseSeconds as Record<string, unknown>;
        const instantiate = Number(phase.instantiate);
        const typecheck = Number(phase.typecheck);
        const flatten = Number(phase.flatten);
        const todae = Number(phase.todae);
        if (
            Number.isFinite(instantiate) &&
            Number.isFinite(typecheck) &&
            Number.isFinite(flatten) &&
            Number.isFinite(todae)
        ) {
            compilePhaseSeconds = {
                instantiate,
                typecheck,
                flatten,
                todae,
            };
        }
    }

    return {
        compileSeconds,
        simulateSeconds,
        points,
        variables,
        compilePhaseSeconds,
    };
}

function normalizeSimulationPayload(raw: unknown): ParsedSimulationPayload | undefined {
    if (!raw || typeof raw !== 'object') {
        return undefined;
    }
    const obj = raw as Record<string, unknown>;
    const names = Array.isArray(obj.names)
        ? obj.names.filter((entry): entry is string => typeof entry === 'string')
        : [];
    const allData = Array.isArray(obj.allData)
        ? obj.allData.map((column) =>
            Array.isArray(column)
                ? column.map((value) => Number(value))
                : []
        )
        : [];
    const nStates = Number(obj.nStates);
    if (!Number.isFinite(nStates) || names.length === 0 || allData.length === 0) {
        return undefined;
    }
    return {
        version: Number.isFinite(Number(obj.version)) ? Number(obj.version) : undefined,
        names,
        allData,
        nStates,
        variableMeta: Array.isArray(obj.variableMeta) ? obj.variableMeta : [],
        simDetails: obj.simDetails ?? {},
    };
}

async function saveSimulationRunToDisk(
    model: string,
    workspaceRoot: string | undefined,
    payload: ParsedSimulationPayload | undefined,
    metrics: SimulationRunMetrics | undefined,
    views: VisualizationView[],
): Promise<string | undefined> {
    if (!workspaceRoot || !payload) {
        return undefined;
    }
    const modelSlug = model
        .trim()
        .replace(/[^a-zA-Z0-9._-]+/g, '_')
        .replace(/^_+|_+$/g, '') || 'model';
    const runsDir = path.join(workspaceRoot, '.rumoca', 'results', 'runs');
    await fs.promises.mkdir(runsDir, { recursive: true });

    const now = Date.now();
    let runId = `${now}_${modelSlug}`;
    let runPath = path.join(runsDir, `${runId}.json`);
    for (let suffix = 1; ; suffix++) {
        try {
            await fs.promises.access(runPath, fs.constants.F_OK);
            runId = `${now}_${modelSlug}_${suffix}`;
            runPath = path.join(runsDir, `${runId}.json`);
        } catch {
            break;
        }
    }

    const runDoc = {
        version: 1,
        runId,
        model,
        savedAtUnixMs: now,
        payload,
        metrics: metrics ?? null,
        views,
    };
    await fs.promises.writeFile(runPath, JSON.stringify(runDoc, null, 2), 'utf-8');
    return runId;
}

function isSafeRunId(runId: string): boolean {
    const id = runId.trim();
    return id.length > 0 && /^[A-Za-z0-9._-]+$/.test(id);
}

async function loadPersistedSimulationRunFromDisk(
    workspaceRoot: string | undefined,
    runId: string,
): Promise<PersistedSimulationRun | undefined> {
    if (!workspaceRoot || !isSafeRunId(runId)) {
        return undefined;
    }
    const runPath = path.join(workspaceRoot, '.rumoca', 'results', 'runs', `${runId}.json`);
    let text: string;
    try {
        text = await fs.promises.readFile(runPath, 'utf-8');
    } catch {
        return undefined;
    }
    let parsed: unknown;
    try {
        parsed = JSON.parse(text);
    } catch {
        return undefined;
    }
    if (!parsed || typeof parsed !== 'object') {
        return undefined;
    }
    const obj = parsed as Record<string, unknown>;
    const model = typeof obj.model === 'string' ? obj.model.trim() : '';
    const payload = normalizeSimulationPayload(obj.payload);
    if (!model || !payload) {
        return undefined;
    }
    const metrics = normalizeSimulationRunMetrics(obj.metrics);
    const views = normalizeVisualizationViews(obj.views);
    return {
        runId,
        model,
        payload,
        metrics,
        views,
    };
}

async function runRumocaSimulation(
    modelUri: string,
    model: string,
    settings: SimulationExecutionSettings,
    onProgress?: (message: string) => void,
): Promise<SimulationRunResult> {
    if (onProgress) {
        onProgress('Compiling and simulating via rumoca-lsp session...');
    }
    const response = await sendProjectCommand<{
        ok?: boolean;
        payload?: unknown;
        error?: string;
        metrics?: unknown;
    }>('rumoca.project.simulate', {
        uri: modelUri,
        model,
        settings: {
            solver: settings.solver,
            tEnd: settings.tEnd,
            dt: settings.dt ?? null,
            modelicaPath: settings.modelicaPath ?? [],
        },
    });
    const metrics = normalizeSimulationRunMetrics(response?.metrics);
    const payload = normalizeSimulationPayload(response?.payload);

    if (!response) {
        return {
            exitCode: 1,
            stderr: 'No response from rumoca-lsp simulation command.',
            payload,
            metrics,
        };
    }

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
        stderr: response.error ?? `Simulation failed in rumoca-lsp (invalid response shape: keys=${Object.keys(response).join(', ')})`,
        payload,
        metrics,
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
    const escapeInlineScriptJson = (raw: string): string =>
        raw.replace(/<\/script/gi, '<\\/script').replace(/<!--/g, '<\\!--');
    const safeViews = views.length > 0 ? views : defaultVisualizationViews();
    const viewsJson = escapeInlineScriptJson(JSON.stringify(safeViews));
    const payloadJson = escapeInlineScriptJson(JSON.stringify(payload ?? null));
    const metricsJson = escapeInlineScriptJson(JSON.stringify(metrics ?? null));
    const modelJson = escapeInlineScriptJson(JSON.stringify(model));
    const panelStateJson = escapeInlineScriptJson(JSON.stringify(panelState ?? null));

    return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Rumoca Results: ${escapeHtml(model)}</title>
  <link rel="stylesheet" href="${escapeHtml(assets?.uplotCss ?? '')}">
  <style>
    :root {
      --bg: #1e1e1e;
      --fg: #d4d4d4;
      --muted: #8b8b8b;
      --border: #353535;
      --accent: #569cd6;
      --panel: #252526;
    }
    html, body {
      margin: 0;
      padding: 0;
      width: 100%;
      height: 100%;
      background: var(--bg);
      color: var(--fg);
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
      overflow: hidden;
    }
    .root {
      position: fixed;
      inset: 0;
      display: flex;
      flex-direction: column;
      min-height: 0;
    }
    .header {
      display: flex;
      align-items: center;
      border-bottom: 1px solid var(--border);
      padding: 8px 12px;
    }
    .title {
      color: var(--accent);
      font-weight: 700;
      font-size: 14px;
      min-width: 0;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
    }
    .timing-summary {
      color: var(--muted);
      font-size: 12px;
      border-bottom: 1px solid var(--border);
      padding: 6px 12px;
      white-space: normal;
      line-height: 1.3;
    }
    .tabs {
      display: flex;
      gap: 6px;
      border-bottom: 1px solid var(--border);
      padding: 8px 12px 6px 12px;
      overflow-x: auto;
    }
    .tab {
      border: 1px solid var(--border);
      border-radius: 6px;
      background: transparent;
      color: var(--fg);
      font-size: 12px;
      padding: 4px 10px;
      cursor: pointer;
      white-space: nowrap;
    }
    .tab.active {
      border-color: #4b89bf;
      background: rgba(86, 156, 214, 0.18);
      color: #9bd3ff;
    }
    #content {
      flex: 1 1 auto;
      min-height: 0;
      min-width: 0;
      position: relative;
      overflow: hidden;
      display: block;
      height: 100%;
    }
    .view {
      display: none;
      position: absolute;
      top: 0;
      right: 0;
      bottom: 0;
      left: 0;
      min-width: 0;
      min-height: 0;
    }
    .view.active {
      display: flex;
      flex-direction: column;
    }
    .view-caption {
      font-size: 12px;
      color: var(--muted);
      padding: 6px 10px;
      border-bottom: 1px solid var(--border);
      background: #1b1b1b;
    }
    .report-frame {
      position: absolute;
      inset: 0;
      border: 0;
      display: block;
      width: 100%;
      height: 100%;
      min-height: 0;
      background: transparent;
    }
    .viewer-wrap {
      display: grid;
      grid-template-rows: 1fr auto;
      width: 100%;
      height: 100%;
      min-height: 0;
    }
    .viewer-host {
      position: relative;
      min-height: 0;
      background: #151515;
      overflow: hidden;
    }
    #viewerCanvas {
      width: 100%;
      height: 100%;
      display: block;
    }
    .viewer-error {
      position: absolute;
      left: 8px;
      right: 8px;
      bottom: 8px;
      border: 1px solid #834;
      background: rgba(80, 24, 24, 0.9);
      color: #ffb3b3;
      border-radius: 6px;
      padding: 8px;
      font-size: 12px;
      white-space: pre-wrap;
    }
    .viewer-camera-info {
      display: none;
      position: absolute;
      right: 8px;
      bottom: 8px;
      z-index: 2;
      border: 1px solid var(--border);
      background: rgba(18, 18, 18, 0.9);
      color: var(--muted);
      border-radius: 6px;
      padding: 6px 8px;
      font-size: 11px;
      line-height: 1.25;
      white-space: pre;
      pointer-events: none;
    }
    .viewer-inspector {
      position: absolute;
      left: 8px;
      top: 8px;
      width: 250px;
      max-width: min(36vw, 300px);
      max-height: calc(100% - 16px);
      border: 1px solid var(--border);
      border-radius: 6px;
      background: rgba(22, 22, 22, 0.92);
      display: grid;
      grid-template-rows: auto auto minmax(0, 1fr);
      gap: 6px;
      padding: 8px;
      overflow: hidden;
      z-index: 3;
    }
    .viewer-inspector.hidden {
      display: none;
    }
    .viewer-inspector .title {
      font-size: 11px;
      text-transform: uppercase;
      letter-spacing: 0.04em;
      color: var(--muted);
      font-weight: 700;
    }
    .viewer-inspector .actions {
      display: flex;
      gap: 6px;
      align-items: center;
      border: 0;
      border-radius: 0;
      padding: 0;
      background: transparent;
    }
    .viewer-inspector .actions button {
      border: 1px solid var(--border);
      border-radius: 6px;
      background: var(--panel);
      color: var(--fg);
      padding: 3px 8px;
      font-size: 11px;
      cursor: pointer;
    }
    .viewer-inspector .actions label {
      display: inline-flex;
      align-items: center;
      gap: 6px;
      color: var(--muted);
      font-size: 11px;
      margin-left: auto;
    }
    .viewer-inspector .objects {
      overflow: auto;
      border: 1px solid var(--border);
      border-radius: 6px;
      background: #141414;
      padding: 4px;
      display: grid;
      gap: 3px;
      align-content: start;
    }
    .viewer-inspector .object-row {
      display: flex;
      align-items: center;
      gap: 6px;
      min-height: 22px;
      border: 1px solid transparent;
      border-radius: 5px;
      padding: 2px 6px;
      font-size: 11px;
      color: var(--fg);
      cursor: pointer;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }
    .viewer-inspector .object-row:hover {
      background: #1f1f1f;
      border-color: #2a2a2a;
    }
    .viewer-inspector .object-row.active {
      background: rgba(86, 156, 214, 0.18);
      border-color: #4b89bf;
      color: #9bd3ff;
    }
    .viewer-inspector .swatch {
      width: 8px;
      height: 8px;
      border-radius: 3px;
      background: #7f7f7f;
      flex: 0 0 auto;
    }
    .viewer-controls {
      border-top: 1px solid var(--border);
      background: #1b1b1b;
      display: flex;
      align-items: center;
      gap: 8px;
      padding: 8px 10px;
      font-size: 12px;
    }
    .viewer-controls button {
      border: 1px solid var(--border);
      border-radius: 6px;
      background: var(--panel);
      color: var(--fg);
      padding: 4px 10px;
      cursor: pointer;
    }
    .viewer-controls button.active {
      border-color: #4b89bf;
      background: rgba(86, 156, 214, 0.18);
      color: #9bd3ff;
    }
    .viewer-controls .transport {
      display: inline-flex;
      align-items: center;
      gap: 4px;
      margin-left: 2px;
      margin-right: 2px;
    }
    .viewer-controls .transport button {
      min-width: 30px;
      padding: 4px 6px;
      font-family: var(--vscode-editor-font-family, monospace);
      font-weight: 600;
      line-height: 1;
      text-align: center;
    }
    .viewer-controls input[type="range"] {
      flex: 1;
      min-width: 120px;
    }
    .viewer-controls .time {
      color: var(--muted);
      width: 120px;
      text-align: right;
    }
    .scatter-wrap {
      display: flex;
      flex-direction: column;
      height: 100%;
      min-height: 0;
      padding: 8px 10px 12px 10px;
      gap: 8px;
    }
    .scatter-info {
      font-size: 12px;
      color: var(--muted);
      min-height: 18px;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }
    .scatter-canvas-wrap {
      position: relative;
      flex: 1;
      min-height: 0;
      border: 1px solid var(--border);
      background: #151515;
      border-radius: 8px;
      overflow: hidden;
    }
    .scatter-canvas {
      width: 100%;
      height: 100%;
      display: block;
      cursor: crosshair;
    }
    .scatter-error {
      position: absolute;
      inset: 0;
      display: flex;
      align-items: center;
      justify-content: center;
      color: #e57373;
      font-size: 13px;
      padding: 12px;
      text-align: center;
      background: rgba(0, 0, 0, 0.2);
      pointer-events: none;
    }
    .uplot, .u-wrap, .u-over, .u-under, .u-cursor-x, .u-cursor-y {
      transition: none !important;
      animation: none !important;
    }
  </style>
</head>
<body>
  <div class="root">
    <div class="header">
      <div class="title">${escapeHtml(model)}</div>
    </div>
    <div class="tabs" id="tabs"></div>
    <div class="timing-summary" id="timingSummary"></div>
    <div id="content"></div>
  </div>
  <script src="${escapeHtml(assets?.uplotJs ?? '')}"></script>
  <script src="${escapeHtml(assets?.threeJs ?? '')}"></script>
  <script>
    const configuredViews = ${viewsJson};
    const payload = ${payloadJson};
    const runMetrics = ${metricsJson};
    const modelName = ${modelJson};
    const panelState = ${panelStateJson};
    const vscodeApi = (typeof acquireVsCodeApi === 'function') ? acquireVsCodeApi() : null;
    if (vscodeApi && panelState) {
      try {
        vscodeApi.setState(panelState);
      } catch (_) {
        // ignore panel-state persistence errors
      }
    }
    const tabsEl = document.getElementById('tabs');
    const contentEl = document.getElementById('content');
    const timingSummaryEl = document.getElementById('timingSummary');
    let globalErrorOverlay = null;

    function showGlobalWebviewError(message) {
      if (!globalErrorOverlay) {
        globalErrorOverlay = document.createElement('div');
        globalErrorOverlay.style.position = 'fixed';
        globalErrorOverlay.style.left = '10px';
        globalErrorOverlay.style.right = '10px';
        globalErrorOverlay.style.bottom = '10px';
        globalErrorOverlay.style.zIndex = '9999';
        globalErrorOverlay.style.border = '1px solid #834';
        globalErrorOverlay.style.background = 'rgba(80, 24, 24, 0.95)';
        globalErrorOverlay.style.color = '#ffb3b3';
        globalErrorOverlay.style.borderRadius = '6px';
        globalErrorOverlay.style.padding = '8px';
        globalErrorOverlay.style.fontSize = '12px';
        globalErrorOverlay.style.whiteSpace = 'pre-wrap';
        globalErrorOverlay.style.pointerEvents = 'none';
        document.body.appendChild(globalErrorOverlay);
      }
      globalErrorOverlay.textContent = String(message || 'Unknown webview error');
    }

    window.addEventListener('error', (event) => {
      const msg = event && event.error
        ? String(event.error.stack || event.error.message || event.message || event.error)
        : String(event && event.message ? event.message : 'Unknown window error');
      console.error('[rumoca] webview runtime error', event && event.error ? event.error : event);
      showGlobalWebviewError('Webview runtime error:\\n' + msg);
    });
    window.addEventListener('unhandledrejection', (event) => {
      const reason = event ? event.reason : undefined;
      const msg = reason && reason.stack ? String(reason.stack) : String(reason || 'Unhandled promise rejection');
      console.error('[rumoca] webview unhandled rejection', reason);
      showGlobalWebviewError('Webview unhandled rejection:\\n' + msg);
    });

    function formatNum(v) {
      const n = Number(v);
      if (!Number.isFinite(n)) return String(v);
      if (Math.abs(n) >= 1000 || (Math.abs(n) > 0 && Math.abs(n) < 0.001)) return n.toExponential(3);
      return n.toFixed(4).replace(/\\.?0+$/, "");
    }

    function viewLabel(view) {
      const kind = view.type === '3d' ? '3D' : (view.type === 'scatter' ? 'Scatter' : 'Time Series');
      return view.title + " · " + kind;
    }

    function sanitizeFileBaseName(name) {
      const text = String(name || 'rumoca_plot').trim();
      const cleaned = text.replace(/[^a-zA-Z0-9._-]+/g, '_').replace(/^_+|_+$/g, '');
      return cleaned.length > 0 ? cleaned : 'rumoca_plot';
    }

    function setTimingSummary() {
      if (!timingSummaryEl) return;
      timingSummaryEl.textContent = '';
      timingSummaryEl.style.display = 'none';
    }

    function createPlotView(view) {
      const root = document.createElement('section');
      root.className = 'view';
      root.classList.add('plot-view');
      root.style.position = 'relative';
      root.style.minHeight = '0';
      const names = payload && Array.isArray(payload.names) ? payload.names : [];
      const allData = payload && Array.isArray(payload.allData) ? payload.allData : [];
      const nStates = Number(payload && payload.nStates);
      const variableMeta = payload && Array.isArray(payload.variableMeta) ? payload.variableMeta : [];
      const simDetails = payload ? (payload.simDetails ?? {}) : {};
      const timeData = allData.length > 0 ? allData[0] : [];
      const hasPayloadData = names.length > 0 && allData.length > 0 && timeData.length > 0;
      const hasUPlot = typeof window.uPlot === 'function';

      const palette = [
        '#4ec9b0', '#569cd6', '#ce9178', '#dcdcaa', '#c586c0',
        '#9cdcfe', '#d7ba7d', '#608b4e', '#d16969', '#b5cea8',
        '#6a9955', '#c8c8c8', '#e8c87a', '#7fdbca', '#f07178'
      ];

      function resolveDefaultSelection() {
        if (view && Array.isArray(view.y) && view.y.length > 0) {
          if (view.y.includes('*states')) {
            return new Set(Array.from({ length: Math.max(0, Math.min(names.length, Number.isFinite(nStates) ? nStates : 0)) }, (_, i) => i));
          }
          const set = new Set();
          for (const name of view.y) {
            const idx = names.indexOf(String(name));
            if (idx >= 0) set.add(idx);
          }
          if (set.size > 0) return set;
        }
        return new Set(Array.from({ length: Math.max(0, Math.min(names.length, Number.isFinite(nStates) ? nStates : 0)) }, (_, i) => i));
      }
      const selected = resolveDefaultSelection();

      const wrap = document.createElement('div');
      wrap.style.display = 'grid';
      wrap.style.gridTemplateRows = 'minmax(0, 1fr) auto';
      wrap.style.flex = '1 1 auto';
      wrap.style.width = '100%';
      wrap.style.height = '100%';
      wrap.style.minHeight = '0';

      const plotLayout = document.createElement('div');
      plotLayout.style.position = 'relative';
      plotLayout.style.height = 'auto';
      plotLayout.style.width = '100%';
      plotLayout.style.minHeight = '0';
      plotLayout.style.border = '1px solid var(--border)';
      plotLayout.style.borderRadius = '8px';
      plotLayout.style.overflow = 'hidden';
      wrap.appendChild(plotLayout);

      const sidebar = document.createElement('div');
      sidebar.style.position = 'absolute';
      sidebar.style.left = '0';
      sidebar.style.top = '0';
      sidebar.style.bottom = '0';
      sidebar.style.width = '280px';
      sidebar.style.borderRight = '1px solid var(--border)';
      sidebar.style.background = '#1b1b1b';
      sidebar.style.overflow = 'auto';
      sidebar.style.minWidth = '0';
      sidebar.style.minHeight = '0';
      sidebar.style.zIndex = '3';
      sidebar.style.display = 'none';
      plotLayout.appendChild(sidebar);

      const plotPane = document.createElement('div');
      plotPane.style.position = 'absolute';
      plotPane.style.left = '0';
      plotPane.style.right = '0';
      plotPane.style.top = '0';
      plotPane.style.bottom = '0';
      plotPane.style.display = 'block';
      plotPane.style.minWidth = '0';
      plotPane.style.minHeight = '0';
      plotPane.style.background = '#151515';
      plotPane.style.visibility = 'visible';
      plotLayout.appendChild(plotPane);

      const plotHost = document.createElement('div');
      plotHost.style.position = 'absolute';
      plotHost.style.inset = '0';
      plotPane.appendChild(plotHost);

      const legend = document.createElement('div');
      legend.style.position = 'absolute';
      legend.style.right = '8px';
      legend.style.bottom = '8px';
      legend.style.maxWidth = '48%';
      legend.style.maxHeight = '35%';
      legend.style.overflow = 'auto';
      legend.style.background = 'rgba(37,37,38,0.92)';
      legend.style.border = '1px solid #3a3a3a';
      legend.style.borderRadius = '6px';
      legend.style.padding = '6px 8px';
      legend.style.fontSize = '12px';
      plotPane.appendChild(legend);

      const hoverInfo = document.createElement('div');
      hoverInfo.style.position = 'absolute';
      hoverInfo.style.right = '8px';
      hoverInfo.style.top = '8px';
      hoverInfo.style.maxWidth = '58%';
      hoverInfo.style.whiteSpace = 'nowrap';
      hoverInfo.style.overflow = 'hidden';
      hoverInfo.style.textOverflow = 'ellipsis';
      hoverInfo.style.fontSize = '12px';
      hoverInfo.style.color = 'var(--muted)';
      hoverInfo.style.background = 'rgba(0,0,0,0.25)';
      hoverInfo.style.padding = '4px 6px';
      hoverInfo.style.borderRadius = '6px';
      hoverInfo.textContent = 'Hover plot for values';
      plotPane.appendChild(hoverInfo);

      const controls = document.createElement('div');
      controls.className = 'viewer-controls';
      const showVarsBtn = document.createElement('button');
      showVarsBtn.textContent = 'Show Variables';
      const detailsBtn = document.createElement('button');
      detailsBtn.textContent = 'Run Details';
      const saveBtn = document.createElement('button');
      saveBtn.textContent = 'Save PNG';
      controls.appendChild(showVarsBtn);
      controls.appendChild(detailsBtn);
      controls.appendChild(saveBtn);
      wrap.appendChild(controls);
      root.appendChild(wrap);

      const plotError = document.createElement('div');
      plotError.className = 'scatter-error';
      plotError.style.display = 'none';
      plotError.style.pointerEvents = 'none';
      plotPane.appendChild(plotError);

      const detailsModal = document.createElement('div');
      detailsModal.style.position = 'absolute';
      detailsModal.style.inset = '0';
      detailsModal.style.display = 'none';
      detailsModal.style.alignItems = 'center';
      detailsModal.style.justifyContent = 'center';
      detailsModal.style.background = 'rgba(0,0,0,0.45)';
      detailsModal.style.zIndex = '10';
      const detailsCard = document.createElement('div');
      detailsCard.style.width = 'min(560px, 92vw)';
      detailsCard.style.maxHeight = '82vh';
      detailsCard.style.overflow = 'auto';
      detailsCard.style.border = '1px solid #3a3a3a';
      detailsCard.style.borderRadius = '8px';
      detailsCard.style.background = '#1f1f1f';
      detailsCard.style.padding = '12px';
      const detailsPre = document.createElement('pre');
      detailsPre.style.margin = '0';
      detailsPre.style.whiteSpace = 'pre-wrap';
      detailsPre.style.fontSize = '12px';
      detailsCard.appendChild(detailsPre);
      const closeBtn = document.createElement('button');
      closeBtn.textContent = 'Close';
      closeBtn.style.marginTop = '8px';
      detailsCard.appendChild(closeBtn);
      detailsModal.appendChild(detailsCard);
      root.appendChild(detailsModal);

      function buildDetailsText() {
        const lines = [];
        const actual = simDetails && simDetails.actual ? simDetails.actual : {};
        const requested = simDetails && simDetails.requested ? simDetails.requested : {};
        const timing = simDetails && simDetails.timing ? simDetails.timing : {};
        const actualPoints = actual.points ?? (runMetrics ? runMetrics.points : timeData.length);
        const actualVars = actual.variables ?? (runMetrics ? runMetrics.variables : names.length);
        const compileSeconds = Number.isFinite(Number(timing.compile_seconds))
          ? Number(timing.compile_seconds)
          : (runMetrics ? Number(runMetrics.compileSeconds) : NaN);
        const simulateSeconds = Number.isFinite(Number(timing.simulate_seconds))
          ? Number(timing.simulate_seconds)
          : (runMetrics ? Number(runMetrics.simulateSeconds) : NaN);
        lines.push('Actual');
        lines.push('  t_start: ' + formatNum(Number(actual.t_start)));
        lines.push('  t_end:   ' + formatNum(Number(actual.t_end)));
        lines.push('  points:  ' + String(actualPoints));
        lines.push('  vars:    ' + String(actualVars));
        lines.push('');
        lines.push('Requested');
        lines.push('  solver:  ' + String(requested.solver ?? 'auto'));
        lines.push('  t_start: ' + formatNum(Number(requested.t_start)));
        lines.push('  t_end:   ' + formatNum(Number(requested.t_end)));
        lines.push('  dt:      ' + (requested.dt == null ? 'auto' : formatNum(Number(requested.dt))));
        if (Number.isFinite(compileSeconds) || Number.isFinite(simulateSeconds)) {
          lines.push('');
          lines.push('Run Timing');
          if (Number.isFinite(compileSeconds)) {
            lines.push('  compile: ' + formatNum(compileSeconds) + 's');
          }
          if (Number.isFinite(simulateSeconds)) {
            lines.push('  simulate:' + ' ' + formatNum(simulateSeconds) + 's');
          }
        }
        return lines.join('\\n');
      }

      function renderSidebar() {
        const rows = [];
        rows.push('<div style="display:flex;gap:6px;margin:8px;">'
          + '<button id="rumocaSelectAll" type="button" style="flex:1;">Select All</button>'
          + '<button id="rumocaSelectNone" type="button" style="flex:1;">Deselect All</button>'
          + '</div>');
        rows.push('<div style="display:grid;gap:2px;padding:0 8px 8px 8px;">');
        for (let i = 0; i < names.length; i++) {
          const meta = variableMeta[i] && typeof variableMeta[i] === 'object' ? variableMeta[i] : {};
          const isState = i < (Number.isFinite(nStates) ? nStates : 0);
          const checked = selected.has(i);
          const color = palette[i % palette.length];
          const role = typeof meta.role === 'string' ? meta.role : (isState ? 'state' : 'algebraic');
          const title = String(names[i]) + ' [' + role + ']';
          rows.push(
            '<label data-idx="' + String(i) + '" title="' + title.replace(/"/g, '&quot;') + '" style="display:flex;align-items:center;gap:10px;padding:2px 0;cursor:pointer;">'
            + '<span class="rumoca-var-box" data-idx="' + String(i) + '" style="display:inline-block;width:12px;height:12px;border-radius:5px;border:1px solid #666;'
            + (checked ? ('background:' + color + ';border-color:' + color + ';') : '') + '"></span>'
            + '<span style="' + (isState ? 'font-weight:700;' : '') + 'overflow:hidden;text-overflow:ellipsis;white-space:nowrap;">'
            + String(names[i]).replace(/</g, '&lt;').replace(/>/g, '&gt;')
            + '</span></label>'
          );
        }
        rows.push('</div>');
        sidebar.innerHTML = rows.join('');
        const selectAll = sidebar.querySelector('#rumocaSelectAll');
        const selectNone = sidebar.querySelector('#rumocaSelectNone');
        selectAll?.addEventListener('click', () => {
          selected.clear();
          for (let i = 0; i < names.length; i++) selected.add(i);
          renderSidebar();
          rebuild();
        });
        selectNone?.addEventListener('click', () => {
          selected.clear();
          renderSidebar();
          rebuild();
        });
        sidebar.querySelectorAll('.rumoca-var-box').forEach((el) => {
          el.addEventListener('click', (event) => {
            event.preventDefault();
            event.stopPropagation();
            const idx = Number((el).getAttribute('data-idx'));
            if (!Number.isFinite(idx)) return;
            if (selected.has(idx)) selected.delete(idx);
            else selected.add(idx);
            renderSidebar();
            rebuild();
          });
        });
      }

      let sidebarVisible = false;
      let resizeRaf = 0;

      function readRawPlotSize() {
        const rect = plotPane.getBoundingClientRect();
        return {
          width: Math.floor(Number(rect.width) || 0),
          height: Math.floor(Number(rect.height) || 0),
        };
      }

      function getStablePlotSize() {
        const raw = readRawPlotSize();
        return {
          width: Math.max(320, Math.floor(Number(raw.width) || 0)),
          height: Math.max(220, Math.floor(Number(raw.height) || 0)),
        };
      }

      function applyPlotResize() {
        if (!plot) return;
        const size = getStablePlotSize();
        plot.setSize(size);
      }

      function schedulePlotResize() {
        if (resizeRaf) {
          cancelAnimationFrame(resizeRaf);
          resizeRaf = 0;
        }
        resizeRaf = requestAnimationFrame(() => {
          resizeRaf = 0;
          applyPlotResize();
        });
      }

      function setSidebarVisible(visible) {
        sidebarVisible = visible;
        sidebar.style.display = sidebarVisible ? 'block' : 'none';
        plotPane.style.left = sidebarVisible ? '280px' : '0';
        showVarsBtn.textContent = sidebarVisible ? 'Hide Variables' : 'Show Variables';
        schedulePlotResize();
      }

      function buildExportPngDataUrl() {
        if (!plot || !plot.root) return undefined;
        const srcCanvas = plot.root.querySelector('canvas');
        if (!srcCanvas || typeof srcCanvas.toDataURL !== 'function') return undefined;
        const exportCanvas = document.createElement('canvas');
        exportCanvas.width = srcCanvas.width;
        exportCanvas.height = srcCanvas.height;
        const ctx = exportCanvas.getContext('2d');
        if (!ctx) return undefined;
        ctx.fillStyle = '#1e1e1e';
        ctx.fillRect(0, 0, exportCanvas.width, exportCanvas.height);
        ctx.drawImage(srcCanvas, 0, 0);

        const items = Array.from(selected)
          .filter((idx) => idx >= 0 && idx < names.length)
          .map((idx) => ({ label: names[idx], color: palette[idx % palette.length] }));
        if (items.length > 0) {
          ctx.save();
          ctx.font = '12px monospace';
          ctx.textBaseline = 'middle';
          const sw = 10;
          const gap = 6;
          const itemGap = 12;
          const rowH = 16;
          const pad = 8;
          const maxBoxW = Math.max(160, Math.floor(exportCanvas.width * 0.58));
          const itemWidths = items.map((item) =>
            Math.ceil(sw + gap + ctx.measureText(item.label).width + itemGap)
          );
          const rows = [];
          let currentRow = [];
          let currentRowW = 0;
          for (let i = 0; i < items.length; i++) {
            const w = itemWidths[i];
            const nextW = currentRow.length === 0 ? w : currentRowW + w;
            if (currentRow.length > 0 && nextW > maxBoxW) {
              rows.push({ indices: currentRow, width: currentRowW });
              currentRow = [i];
              currentRowW = w;
            } else {
              currentRow.push(i);
              currentRowW = nextW;
            }
          }
          if (currentRow.length > 0) {
            rows.push({ indices: currentRow, width: currentRowW });
          }
          let maxRowW = 0;
          for (const row of rows) {
            maxRowW = Math.max(maxRowW, row.width);
          }
          const boxW = Math.ceil(pad * 2 + maxRowW);
          const boxH = Math.ceil(pad * 2 + rowH * rows.length);
          const boxX = Math.max(6, exportCanvas.width - boxW - 10);
          const boxY = Math.max(6, exportCanvas.height - boxH - 10);
          ctx.fillStyle = 'rgba(37,37,38,0.92)';
          ctx.strokeStyle = 'rgba(90,90,90,0.9)';
          ctx.fillRect(boxX, boxY, boxW, boxH);
          ctx.strokeRect(boxX + 0.5, boxY + 0.5, boxW - 1, boxH - 1);
          for (let rowIdx = 0; rowIdx < rows.length; rowIdx++) {
            const y = boxY + pad + rowIdx * rowH + rowH / 2;
            let cursorX = boxX + pad;
            for (const itemIdx of rows[rowIdx].indices) {
              const item = items[itemIdx];
              ctx.fillStyle = item.color;
              ctx.fillRect(cursorX, y - sw / 2, sw, sw);
              ctx.fillStyle = '#d4d4d4';
              ctx.fillText(item.label, cursorX + sw + gap, y);
              cursorX += itemWidths[itemIdx];
            }
          }
          ctx.restore();
        }
        return exportCanvas.toDataURL('image/png');
      }

      let plot = null;
      let plotDblClickCleanup = null;
      let activeSeriesIdx = [];
      function clearPlotError() {
        plotError.style.display = 'none';
        plotError.textContent = '';
      }
      function showPlotError(message) {
        plotError.textContent = message;
        plotError.style.display = 'flex';
      }
      function rebuild() {
        if (!hasPayloadData) {
          if (plot) {
            plot.destroy();
            plot = null;
          }
          legend.innerHTML = '';
          hoverInfo.textContent = 'No simulation payload returned by rumoca-lsp.';
          showPlotError('No simulation payload returned by rumoca-lsp.');
          return;
        }
        if (!hasUPlot) {
          if (plot) {
            plot.destroy();
            plot = null;
          }
          legend.innerHTML = '';
          hoverInfo.textContent = 'uPlot failed to load.';
          showPlotError('uPlot failed to load.');
          return;
        }
        activeSeriesIdx = Array.from(selected).filter((idx) => idx >= 0 && idx < names.length);
        if (activeSeriesIdx.length === 0) {
          activeSeriesIdx = [0];
        }
        const data = [timeData];
        const series = [{}];
        const legendItems = [];
        for (const idx of activeSeriesIdx) {
          data.push(allData[idx + 1] || []);
          series.push({
            label: names[idx],
            stroke: palette[idx % palette.length],
            width: 1.5,
          });
          legendItems.push({ label: names[idx], color: palette[idx % palette.length] });
        }
        legend.innerHTML = '';
        for (const item of legendItems) {
          const row = document.createElement('span');
          row.style.display = 'inline-flex';
          row.style.alignItems = 'center';
          row.style.gap = '6px';
          row.style.marginRight = '10px';
          const sw = document.createElement('span');
          sw.style.display = 'inline-block';
          sw.style.width = '10px';
          sw.style.height = '10px';
          sw.style.borderRadius = '3px';
          sw.style.background = item.color;
          const label = document.createElement('span');
          label.textContent = item.label;
          row.appendChild(sw);
          row.appendChild(label);
          legend.appendChild(row);
        }

        if (plot) {
          if (plotDblClickCleanup) {
            plotDblClickCleanup();
            plotDblClickCleanup = null;
          }
          plot.destroy();
          plot = null;
        }
        clearPlotError();
        const size = getStablePlotSize();
        plot = new window.uPlot({
          width: size.width,
          height: size.height,
          padding: [8, 8, 28, 8],
          scales: { x: { time: false } },
          axes: [
            {
              stroke: '#888',
              grid: { stroke: '#333' },
              label: 'time',
              labelGap: 2,
              size: 36,
              font: '11px monospace',
              labelFont: '12px monospace'
            },
            { stroke: '#888', grid: { stroke: '#333' }, font: '11px monospace' }
          ],
          series,
          cursor: { drag: { x: true, y: true } },
          hooks: {
            setCursor: [function(u) {
              const idx = u.cursor.idx;
              if (idx == null || idx < 0 || idx >= timeData.length) {
                hoverInfo.textContent = 'Hover plot for values';
                return;
              }
              const parts = ['t=' + formatNum(Number(timeData[idx]))];
              for (let i = 0; i < activeSeriesIdx.length; i++) {
                const sIdx = activeSeriesIdx[i];
                const col = allData[sIdx + 1] || [];
                if (idx < col.length) {
                  parts.push(names[sIdx] + '=' + formatNum(Number(col[idx])));
                }
              }
              hoverInfo.textContent = parts.join(' | ');
            }]
          },
          legend: { show: false },
        }, data, plotHost);
      }

      let initialized = false;
      let initializing = false;
      function ensureInitialized() {
        if (initialized || initializing) {
          return;
        }
        const raw = readRawPlotSize();
        if (raw.width < 32 || raw.height < 32) {
          return;
        }
        initializing = true;
        try {
          rebuild();
          initialized = true;
          clearPlotError();
          schedulePlotResize();
        } catch (error) {
          initialized = false;
          const message = (error && typeof error === 'object' && 'message' in error)
            ? String(error.message ?? error)
            : String(error);
          showPlotError('Failed to initialize time-series plot: ' + message);
          console.error('[rumoca] time-series initialization failed', error);
        } finally {
          initializing = false;
        }
      }

      function scheduleInitializeWhenReady() {
        ensureInitialized();
        schedulePlotResize();
      }

      renderSidebar();
      setSidebarVisible(false);
      if (!hasPayloadData || !hasUPlot) {
        showVarsBtn.disabled = true;
      }

      showVarsBtn.addEventListener('click', () => setSidebarVisible(!sidebarVisible));
      detailsBtn.addEventListener('click', () => {
        ensureInitialized();
        detailsPre.textContent = buildDetailsText();
        detailsModal.style.display = 'flex';
      });
      closeBtn.addEventListener('click', () => { detailsModal.style.display = 'none'; });
      detailsModal.addEventListener('click', (event) => {
        if (event.target === detailsModal) {
          detailsModal.style.display = 'none';
        }
      });
      saveBtn.addEventListener('click', () => {
        ensureInitialized();
        const dataUrl = buildExportPngDataUrl();
        if (!dataUrl) {
          return;
        }
        if (vscodeApi) {
          vscodeApi.postMessage({
            command: 'savePlotPng',
            dataUrl,
            defaultName: sanitizeFileBaseName(modelName) + '_plot.png',
          });
          return;
        }
        const link = document.createElement('a');
        link.href = dataUrl;
        link.download = sanitizeFileBaseName(modelName) + '_plot.png';
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
      });

      const handleWindowResize = () => {
        schedulePlotResize();
      };
      window.addEventListener('resize', handleWindowResize);

      let resizeObserver = null;
      if (typeof ResizeObserver !== 'undefined') {
        resizeObserver = new ResizeObserver(() => {
          if (!initialized) {
            scheduleInitializeWhenReady();
            return;
          }
          schedulePlotResize();
        });
        resizeObserver.observe(root);
      }

      root.__onActivate = () => {
        scheduleInitializeWhenReady();
      };
      root.__onDispose = () => {
        window.removeEventListener('resize', handleWindowResize);
        if (resizeObserver) {
          resizeObserver.disconnect();
          resizeObserver = null;
        }
        if (resizeRaf) {
          cancelAnimationFrame(resizeRaf);
          resizeRaf = 0;
        }
        if (plot) {
          plot.destroy();
          plot = null;
        }
      };
      return root;
    }

    function createScatterView(view) {
      const root = document.createElement('section');
      root.className = 'view';
      root.classList.add('plot-view');
      root.style.position = 'relative';
      root.style.minHeight = '0';

      const names = payload && Array.isArray(payload.names) ? payload.names : [];
      const allData = payload && Array.isArray(payload.allData) ? payload.allData : [];
      const simDetails = payload ? (payload.simDetails ?? {}) : {};
      const timeData = allData.length > 0 ? allData[0] : [];
      const hasPayloadData = allData.length > 0 && timeData.length > 0;
      const hasUPlot = typeof window.uPlot === 'function';
      const palette = [
        '#4ec9b0', '#569cd6', '#ce9178', '#dcdcaa', '#c586c0',
        '#9cdcfe', '#d7ba7d', '#608b4e', '#d16969', '#b5cea8',
        '#6a9955', '#c8c8c8', '#e8c87a', '#7fdbca', '#f07178'
      ];

      function resolveSeries(exprRaw) {
        const expr = String(exprRaw || '').trim();
        if (!expr) return null;
        if (expr === 'time' || expr === 't') {
          return { label: 'time', data: timeData };
        }
        const idx = names.indexOf(expr);
        if (idx < 0) return null;
        return { label: expr, data: allData[idx + 1] };
      }

      function resolveScatterSeriesDefs() {
        const defs = [];
        if (Array.isArray(view.scatterSeries)) {
          for (const entry of view.scatterSeries) {
            if (!entry || typeof entry !== 'object') continue;
            const sx = String(entry.x || '').trim();
            const sy = String(entry.y || '').trim();
            const name = String(entry.name || '').trim();
            if (sx.length === 0 || sy.length === 0) continue;
            defs.push({
              name: name.length > 0 ? name : (sy + ' vs ' + sx),
              x: sx,
              y: sy,
            });
          }
        }
        if (defs.length === 0) {
          const fallbackX = String(view.x || 'time').trim() || 'time';
          const fallbackY = Array.isArray(view.y) && view.y.length > 0 ? String(view.y[0] || '').trim() : '';
          if (fallbackY.length > 0) {
            defs.push({
              name: fallbackY + ' vs ' + fallbackX,
              x: fallbackX,
              y: fallbackY,
            });
          }
        }
        return defs;
      }

      const wrap = document.createElement('div');
      wrap.style.display = 'grid';
      wrap.style.gridTemplateRows = 'minmax(0, 1fr) auto';
      wrap.style.flex = '1 1 auto';
      wrap.style.width = '100%';
      wrap.style.height = '100%';
      wrap.style.minHeight = '0';

      const plotLayout = document.createElement('div');
      plotLayout.style.position = 'relative';
      plotLayout.style.height = 'auto';
      plotLayout.style.width = '100%';
      plotLayout.style.minHeight = '0';
      plotLayout.style.border = '1px solid var(--border)';
      plotLayout.style.borderRadius = '8px';
      plotLayout.style.overflow = 'hidden';
      wrap.appendChild(plotLayout);

      const plotPane = document.createElement('div');
      plotPane.style.position = 'absolute';
      plotPane.style.left = '0';
      plotPane.style.right = '0';
      plotPane.style.top = '0';
      plotPane.style.bottom = '0';
      plotPane.style.display = 'block';
      plotPane.style.minWidth = '0';
      plotPane.style.minHeight = '0';
      plotPane.style.background = '#151515';
      plotPane.style.visibility = 'visible';
      plotLayout.appendChild(plotPane);

      const plotHost = document.createElement('div');
      plotHost.style.position = 'absolute';
      plotHost.style.inset = '0';
      plotPane.appendChild(plotHost);

      const legend = document.createElement('div');
      legend.style.position = 'absolute';
      legend.style.right = '8px';
      legend.style.bottom = '8px';
      legend.style.maxWidth = '48%';
      legend.style.maxHeight = '35%';
      legend.style.overflow = 'auto';
      legend.style.background = 'rgba(37,37,38,0.92)';
      legend.style.border = '1px solid #3a3a3a';
      legend.style.borderRadius = '6px';
      legend.style.padding = '6px 8px';
      legend.style.fontSize = '12px';
      plotPane.appendChild(legend);

      const hoverInfo = document.createElement('div');
      hoverInfo.style.position = 'absolute';
      hoverInfo.style.right = '8px';
      hoverInfo.style.top = '8px';
      hoverInfo.style.maxWidth = '58%';
      hoverInfo.style.whiteSpace = 'nowrap';
      hoverInfo.style.overflow = 'hidden';
      hoverInfo.style.textOverflow = 'ellipsis';
      hoverInfo.style.fontSize = '12px';
      hoverInfo.style.color = 'var(--muted)';
      hoverInfo.style.background = 'rgba(0,0,0,0.25)';
      hoverInfo.style.padding = '4px 6px';
      hoverInfo.style.borderRadius = '6px';
      hoverInfo.textContent = 'Hover plot for values';
      plotPane.appendChild(hoverInfo);

      const controls = document.createElement('div');
      controls.className = 'viewer-controls';
      const fitBtn = document.createElement('button');
      fitBtn.textContent = 'Auto Fit';
      fitBtn.title = 'Fit X/Y scales to data bounds';
      const detailsBtn = document.createElement('button');
      detailsBtn.textContent = 'Run Details';
      const saveBtn = document.createElement('button');
      saveBtn.textContent = 'Save PNG';
      controls.appendChild(fitBtn);
      controls.appendChild(detailsBtn);
      controls.appendChild(saveBtn);
      wrap.appendChild(controls);
      root.appendChild(wrap);

      const plotError = document.createElement('div');
      plotError.className = 'scatter-error';
      plotError.style.display = 'none';
      plotError.style.pointerEvents = 'none';
      plotPane.appendChild(plotError);

      const detailsModal = document.createElement('div');
      detailsModal.style.position = 'absolute';
      detailsModal.style.inset = '0';
      detailsModal.style.display = 'none';
      detailsModal.style.alignItems = 'center';
      detailsModal.style.justifyContent = 'center';
      detailsModal.style.background = 'rgba(0,0,0,0.45)';
      detailsModal.style.zIndex = '10';
      const detailsCard = document.createElement('div');
      detailsCard.style.width = 'min(560px, 92vw)';
      detailsCard.style.maxHeight = '82vh';
      detailsCard.style.overflow = 'auto';
      detailsCard.style.border = '1px solid #3a3a3a';
      detailsCard.style.borderRadius = '8px';
      detailsCard.style.background = '#1f1f1f';
      detailsCard.style.padding = '12px';
      const detailsPre = document.createElement('pre');
      detailsPre.style.margin = '0';
      detailsPre.style.whiteSpace = 'pre-wrap';
      detailsPre.style.fontSize = '12px';
      detailsCard.appendChild(detailsPre);
      const closeBtn = document.createElement('button');
      closeBtn.textContent = 'Close';
      closeBtn.style.marginTop = '8px';
      detailsCard.appendChild(closeBtn);
      detailsModal.appendChild(detailsCard);
      root.appendChild(detailsModal);

      function buildDetailsText() {
        const lines = [];
        const actual = simDetails && simDetails.actual ? simDetails.actual : {};
        const requested = simDetails && simDetails.requested ? simDetails.requested : {};
        const timing = simDetails && simDetails.timing ? simDetails.timing : {};
        const compileSeconds = Number.isFinite(Number(timing.compile_seconds))
          ? Number(timing.compile_seconds)
          : (runMetrics ? Number(runMetrics.compileSeconds) : NaN);
        const simulateSeconds = Number.isFinite(Number(timing.simulate_seconds))
          ? Number(timing.simulate_seconds)
          : (runMetrics ? Number(runMetrics.simulateSeconds) : NaN);
        lines.push('Scatter');
        lines.push('  series: ' + String(activeScatterSeries.length));
        lines.push('  points: ' + String(flatXVals.length));
        for (let i = 0; i < Math.min(5, activeScatterSeries.length); i += 1) {
          const series = activeScatterSeries[i];
          lines.push(
            '  - ' + String(series.name) + ' (' + String(series.points.length) + '): '
            + String(series.yLabel) + ' vs ' + String(series.xLabel)
          );
        }
        if (activeScatterSeries.length > 5) {
          lines.push('  - ...');
        }
        lines.push('');
        lines.push('Actual');
        lines.push('  t_start: ' + formatNum(Number(actual.t_start)));
        lines.push('  t_end:   ' + formatNum(Number(actual.t_end)));
        lines.push('');
        lines.push('Requested');
        lines.push('  solver:  ' + String(requested.solver ?? 'auto'));
        lines.push('  t_start: ' + formatNum(Number(requested.t_start)));
        lines.push('  t_end:   ' + formatNum(Number(requested.t_end)));
        lines.push('  dt:      ' + (requested.dt == null ? 'auto' : formatNum(Number(requested.dt))));
        if (Number.isFinite(compileSeconds) || Number.isFinite(simulateSeconds)) {
          lines.push('');
          lines.push('Run Timing');
          if (Number.isFinite(compileSeconds)) {
            lines.push('  compile: ' + formatNum(compileSeconds) + 's');
          }
          if (Number.isFinite(simulateSeconds)) {
            lines.push('  simulate:' + ' ' + formatNum(simulateSeconds) + 's');
          }
        }
        return lines.join('\\n');
      }

      function setError(message) {
        plotError.textContent = message;
        plotError.style.display = 'flex';
      }
      function clearError() {
        plotError.textContent = '';
        plotError.style.display = 'none';
      }

      function readRawPlotSize() {
        const rect = plotPane.getBoundingClientRect();
        return {
          width: Math.floor(Number(rect.width) || 0),
          height: Math.floor(Number(rect.height) || 0),
        };
      }
      function getStablePlotSize() {
        const raw = readRawPlotSize();
        return {
          width: Math.max(320, Math.floor(Number(raw.width) || 0)),
          height: Math.max(220, Math.floor(Number(raw.height) || 0)),
        };
      }

      let plot = null;
      let plotDblClickCleanup = null;
      let resizeRaf = 0;
      let activeScatterSeries = [];
      let flatXVals = [];
      let flatYVals = [];

      function scatterBounds(values) {
        let min = Infinity;
        let max = -Infinity;
        for (const value of values) {
          const n = Number(value);
          if (!Number.isFinite(n)) continue;
          if (n < min) min = n;
          if (n > max) max = n;
        }
        if (!Number.isFinite(min) || !Number.isFinite(max)) {
          return { min: -1, max: 1 };
        }
        if (min === max) {
          const pad = Math.max(1, Math.abs(min) * 0.1);
          min -= pad;
          max += pad;
        } else {
          const range = max - min;
          const pad = Math.max(range * 0.02, 1e-9);
          min -= pad;
          max += pad;
        }
        return { min, max };
      }

      function autoFitScales() {
        if (!plot || flatXVals.length === 0 || flatYVals.length === 0) return;
        const xb = scatterBounds(flatXVals);
        const yb = scatterBounds(flatYVals);
        try {
          plot.setScale('x', { min: xb.min, max: xb.max });
          plot.setScale('y', { min: yb.min, max: yb.max });
        } catch (error) {
          console.error('[rumoca] scatter auto-fit failed', error);
        }
      }

      function buildExportPngDataUrl() {
        if (!plot || !plot.root) return undefined;
        const srcCanvas = plot.root.querySelector('canvas');
        if (!srcCanvas || typeof srcCanvas.toDataURL !== 'function') return undefined;
        const exportCanvas = document.createElement('canvas');
        exportCanvas.width = srcCanvas.width;
        exportCanvas.height = srcCanvas.height;
        const ctx = exportCanvas.getContext('2d');
        if (!ctx) return undefined;
        ctx.fillStyle = '#1e1e1e';
        ctx.fillRect(0, 0, exportCanvas.width, exportCanvas.height);
        ctx.drawImage(srcCanvas, 0, 0);
        if (activeScatterSeries.length > 0) {
          ctx.save();
          ctx.font = '12px monospace';
          ctx.textBaseline = 'middle';
          const sw = 10;
          const gap = 6;
          const itemGap = 12;
          const rowH = 16;
          const pad = 8;
          const items = activeScatterSeries.map((series) => ({
            label: String(series.name),
            color: String(series.color),
          }));
          const maxBoxW = Math.max(180, Math.floor(exportCanvas.width * 0.62));
          const itemWidths = items.map((item) =>
            Math.ceil(sw + gap + ctx.measureText(item.label).width + itemGap)
          );
          const rows = [];
          let currentRow = [];
          let currentRowW = 0;
          for (let i = 0; i < items.length; i++) {
            const w = itemWidths[i];
            const nextW = currentRow.length === 0 ? w : currentRowW + w;
            if (currentRow.length > 0 && nextW > maxBoxW) {
              rows.push({ indices: currentRow, width: currentRowW });
              currentRow = [i];
              currentRowW = w;
            } else {
              currentRow.push(i);
              currentRowW = nextW;
            }
          }
          if (currentRow.length > 0) {
            rows.push({ indices: currentRow, width: currentRowW });
          }
          let maxRowW = 0;
          for (const row of rows) {
            maxRowW = Math.max(maxRowW, row.width);
          }
          const boxW = Math.ceil(pad * 2 + maxRowW);
          const boxH = Math.ceil(pad * 2 + rowH * rows.length);
          const boxX = Math.max(6, exportCanvas.width - boxW - 10);
          const boxY = Math.max(6, exportCanvas.height - boxH - 10);
          ctx.fillStyle = 'rgba(37,37,38,0.92)';
          ctx.strokeStyle = 'rgba(90,90,90,0.9)';
          ctx.fillRect(boxX, boxY, boxW, boxH);
          ctx.strokeRect(boxX + 0.5, boxY + 0.5, boxW - 1, boxH - 1);
          for (let rowIdx = 0; rowIdx < rows.length; rowIdx++) {
            const cy = boxY + pad + rowIdx * rowH + rowH / 2;
            let cursorX = boxX + pad;
            for (const itemIdx of rows[rowIdx].indices) {
              const item = items[itemIdx];
              ctx.fillStyle = item.color;
              ctx.fillRect(cursorX, cy - sw / 2, sw, sw);
              ctx.fillStyle = '#d4d4d4';
              ctx.fillText(item.label, cursorX + sw + gap, cy);
              cursorX += itemWidths[itemIdx];
            }
          }
          ctx.restore();
        }
        return exportCanvas.toDataURL('image/png');
      }

      function rebuild() {
        if (!hasPayloadData) {
          if (plot) {
            plot.destroy();
            plot = null;
          }
          legend.innerHTML = '';
          hoverInfo.textContent = 'No simulation payload returned by rumoca-lsp.';
          setError('No simulation payload returned by rumoca-lsp.');
          return;
        }
        if (!hasUPlot) {
          if (plot) {
            plot.destroy();
            plot = null;
          }
          legend.innerHTML = '';
          hoverInfo.textContent = 'uPlot failed to load.';
          setError('uPlot failed to load.');
          return;
        }

        const defs = resolveScatterSeriesDefs();
        const nextSeries = [];
        const failedDefs = [];
        for (let i = 0; i < defs.length; i++) {
          const def = defs[i];
          const xSeries = resolveSeries(def.x);
          const ySeries = resolveSeries(def.y);
          if (!xSeries || !ySeries) {
            failedDefs.push(def);
            continue;
          }
          const points = [];
          const n = Math.min(xSeries.data.length, ySeries.data.length, timeData.length);
          for (let j = 0; j < n; j++) {
            const xv = Number(xSeries.data[j]);
            const yv = Number(ySeries.data[j]);
            if (!Number.isFinite(xv) || !Number.isFinite(yv)) continue;
            points.push({
              x: xv,
              y: yv,
              t: Number(timeData[j]),
            });
          }
          if (points.length === 0) {
            failedDefs.push(def);
            continue;
          }
          nextSeries.push({
            name: def.name,
            xLabel: xSeries.label,
            yLabel: ySeries.label,
            color: palette[i % palette.length],
            points,
          });
        }

        if (nextSeries.length === 0) {
          if (plot) {
            if (plotDblClickCleanup) {
              plotDblClickCleanup();
              plotDblClickCleanup = null;
            }
            plot.destroy();
            plot = null;
          }
          legend.innerHTML = '';
          hoverInfo.textContent = 'Configure valid scatter series in settings';
          if (failedDefs.length > 0) {
            setError('Scatter series could not be resolved: ' + failedDefs.map((d) => d.name).join(', '));
          } else {
            setError('No scatter series configured.');
          }
          return;
        }

        activeScatterSeries = nextSeries;
        flatXVals = [];
        flatYVals = [];
        for (const series of activeScatterSeries) {
          for (const point of series.points) {
            flatXVals.push(point.x);
            flatYVals.push(point.y);
          }
        }
        legend.innerHTML = '';
        for (const series of activeScatterSeries) {
          const row = document.createElement('span');
          row.style.display = 'inline-flex';
          row.style.alignItems = 'center';
          row.style.gap = '6px';
          row.style.marginRight = '10px';
          const sw = document.createElement('span');
          sw.style.display = 'inline-block';
          sw.style.width = '10px';
          sw.style.height = '10px';
          sw.style.borderRadius = '3px';
          sw.style.background = series.color;
          const label = document.createElement('span');
          label.textContent = series.name;
          row.appendChild(sw);
          row.appendChild(label);
          legend.appendChild(row);
        }

        if (plot) {
          if (plotDblClickCleanup) {
            plotDblClickCleanup();
            plotDblClickCleanup = null;
          }
          plot.destroy();
          plot = null;
        }
        clearError();
        const xb = scatterBounds(flatXVals);
        const yb = scatterBounds(flatYVals);
        const size = getStablePlotSize();
        plot = new window.uPlot({
          width: size.width,
          height: size.height,
          padding: [8, 8, 28, 8],
          scales: {
            x: { time: false, auto: false, min: xb.min, max: xb.max },
            y: { auto: false, min: yb.min, max: yb.max },
          },
          axes: [
            {
              stroke: '#888',
              grid: { stroke: '#333' },
              label: 'x',
              labelGap: 2,
              size: 36,
              font: '11px monospace',
              labelFont: '12px monospace',
            },
            {
              stroke: '#888',
              grid: { stroke: '#333' },
              label: 'y',
              font: '11px monospace',
              labelFont: '12px monospace',
            },
          ],
          series: [
            {},
            {
              label: 'scatter',
              stroke: '#00000000',
              width: 0,
              points: { show: false },
            },
          ],
          cursor: { drag: { x: true, y: true } },
          hooks: {
            draw: [function(u) {
              const ctx = u.ctx;
              const left = u.bbox.left;
              const top = u.bbox.top;
              const width = u.bbox.width;
              const height = u.bbox.height;
              ctx.save();
              ctx.beginPath();
              ctx.rect(left, top, width, height);
              ctx.clip();
              for (const series of activeScatterSeries) {
                ctx.fillStyle = series.color;
                for (const point of series.points) {
                  const px = u.valToPos(point.x, 'x');
                  const py = u.valToPos(point.y, 'y');
                  if (!Number.isFinite(px) || !Number.isFinite(py)) {
                    continue;
                  }
                  ctx.beginPath();
                  ctx.arc(px, py, 2.2, 0, Math.PI * 2);
                  ctx.fill();
                }
              }
              ctx.restore();
            }],
            setCursor: [function(u) {
              const mx = Number(u.cursor.left);
              const my = Number(u.cursor.top);
              if (!Number.isFinite(mx) || !Number.isFinite(my)) {
                hoverInfo.textContent = 'Hover plot for values';
                return;
              }
              let best = null;
              let bestD2 = Infinity;
              for (const series of activeScatterSeries) {
                for (const point of series.points) {
                  const px = u.valToPos(point.x, 'x');
                  const py = u.valToPos(point.y, 'y');
                  if (!Number.isFinite(px) || !Number.isFinite(py)) {
                    continue;
                  }
                  const dx = px - mx;
                  const dy = py - my;
                  const d2 = dx * dx + dy * dy;
                  if (d2 < bestD2) {
                    bestD2 = d2;
                    best = { series, point };
                  }
                }
              }
              if (!best || bestD2 > 196) {
                hoverInfo.textContent = 'Hover plot for values';
                return;
              }
              const parts = [
                best.series.name,
                best.series.xLabel + '=' + formatNum(best.point.x),
                best.series.yLabel + '=' + formatNum(best.point.y),
              ];
              if (Number.isFinite(best.point.t)) {
                parts.push('t=' + formatNum(best.point.t));
              }
              hoverInfo.textContent = parts.join(' | ');
            }],
          },
          legend: { show: false },
        }, [[xb.min, xb.max], [null, null]], plotHost);
        if (plot && plot.root) {
          const onDblClick = (event) => {
            if (event) {
              event.preventDefault();
            }
            autoFitScales();
          };
          plot.root.addEventListener('dblclick', onDblClick);
          plotDblClickCleanup = () => {
            plot.root.removeEventListener('dblclick', onDblClick);
          };
        }
        hoverInfo.textContent = String(activeScatterSeries.length) + ' scatter series (' + String(flatXVals.length) + ' points)';
      }

      function applyPlotResize() {
        if (!plot) return;
        const size = getStablePlotSize();
        plot.setSize(size);
      }
      function schedulePlotResize() {
        if (resizeRaf) {
          cancelAnimationFrame(resizeRaf);
          resizeRaf = 0;
        }
        resizeRaf = requestAnimationFrame(() => {
          resizeRaf = 0;
          applyPlotResize();
        });
      }

      let initialized = false;
      let initializing = false;
      function ensureInitialized() {
        if (initialized || initializing) {
          return;
        }
        const raw = readRawPlotSize();
        if (raw.width < 32 || raw.height < 32) {
          return;
        }
        initializing = true;
        try {
          rebuild();
          initialized = true;
          clearError();
          schedulePlotResize();
        } catch (error) {
          initialized = false;
          const message = (error && typeof error === 'object' && 'message' in error)
            ? String(error.message ?? error)
            : String(error);
          setError('Failed to initialize scatter plot: ' + message);
          console.error('[rumoca] scatter initialization failed', error);
        } finally {
          initializing = false;
        }
      }
      function scheduleInitializeWhenReady() {
        ensureInitialized();
        schedulePlotResize();
      }

      fitBtn.addEventListener('click', () => {
        ensureInitialized();
        autoFitScales();
      });
      detailsBtn.addEventListener('click', () => {
        ensureInitialized();
        detailsPre.textContent = buildDetailsText();
        detailsModal.style.display = 'flex';
      });
      closeBtn.addEventListener('click', () => { detailsModal.style.display = 'none'; });
      detailsModal.addEventListener('click', (event) => {
        if (event.target === detailsModal) {
          detailsModal.style.display = 'none';
        }
      });
      saveBtn.addEventListener('click', () => {
        ensureInitialized();
        const dataUrl = buildExportPngDataUrl();
        if (!dataUrl) return;
        if (vscodeApi) {
          vscodeApi.postMessage({
            command: 'savePlotPng',
            dataUrl,
            defaultName: sanitizeFileBaseName(modelName) + '_scatter.png',
          });
          return;
        }
        const link = document.createElement('a');
        link.href = dataUrl;
        link.download = sanitizeFileBaseName(modelName) + '_scatter.png';
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
      });

      const handleWindowResize = () => {
        schedulePlotResize();
      };
      window.addEventListener('resize', handleWindowResize);
      let resizeObserver = null;
      if (typeof ResizeObserver !== 'undefined') {
        resizeObserver = new ResizeObserver(() => {
          if (!initialized) {
            scheduleInitializeWhenReady();
            return;
          }
          schedulePlotResize();
        });
        resizeObserver.observe(root);
      }

      root.__onActivate = () => {
        scheduleInitializeWhenReady();
      };
      root.__onDispose = () => {
        window.removeEventListener('resize', handleWindowResize);
        if (resizeObserver) {
          resizeObserver.disconnect();
          resizeObserver = null;
        }
        if (resizeRaf) {
          cancelAnimationFrame(resizeRaf);
          resizeRaf = 0;
        }
        if (plot) {
          if (plotDblClickCleanup) {
            plotDblClickCleanup();
            plotDblClickCleanup = null;
          }
          plot.destroy();
          plot = null;
        }
      };
      return root;
    }

    function createViewer3dView(view) {
      const root = document.createElement('section');
      root.className = 'view';

      const wrap = document.createElement('div');
      wrap.className = 'viewer-wrap';

      const host = document.createElement('div');
      host.className = 'viewer-host';
      const canvas = document.createElement('canvas');
      canvas.id = 'viewerCanvas';
      host.appendChild(canvas);

      const cameraInfoEl = document.createElement('div');
      cameraInfoEl.className = 'viewer-camera-info';
      cameraInfoEl.textContent = 'camera: unavailable';
      host.appendChild(cameraInfoEl);

      const errorEl = document.createElement('div');
      errorEl.className = 'viewer-error';
      errorEl.style.display = 'none';
      host.appendChild(errorEl);

      const inspectorEl = document.createElement('div');
      inspectorEl.className = 'viewer-inspector hidden';
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
      controls.className = 'viewer-controls';
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
      const exportMovieBtn = document.createElement('button');
      exportMovieBtn.textContent = 'Export Movie';
      exportMovieBtn.title = 'Export viewer animation as WebM';
      controls.appendChild(objectsBtn);
      controls.appendChild(transport);
      controls.appendChild(slider);
      controls.appendChild(timeLabel);
      controls.appendChild(exportMovieBtn);

      wrap.appendChild(host);
      wrap.appendChild(controls);
      root.appendChild(wrap);

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
        if (!context2d) return;
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
        if (idx < 0) return undefined;
        return allData[idx + 1];
      }

      const runtimeCtx = {};
      const THREE_NS = window.THREE;
      let onInit = null;
      let onFrame = null;
      let onResize = null;
      let useDefaultViewerRuntime = false;

      const runtimeApi = {
        canvas,
        context2d,
        container: host,
        THREE: THREE_NS,
        names,
        times: timeData,
        sampleIndex: 0,
        state: {},
        getViewportSize() {
          const rect = host.getBoundingClientRect();
          return {
            width: Math.max(1, Math.floor(rect.width)),
            height: Math.max(1, Math.floor(rect.height)),
          };
        },
        getValue(name, sampleIndex) {
          const series = getSeries(name);
          if (!series) return undefined;
          const idx = Number.isFinite(sampleIndex)
            ? Math.max(0, Math.min(series.length - 1, Math.floor(sampleIndex)))
            : runtimeApi.sampleIndex;
          return series[idx];
        },
        getTime(sampleIndex) {
          const idx = Number.isFinite(sampleIndex)
            ? Math.max(0, Math.min(timeData.length - 1, Math.floor(sampleIndex)))
            : runtimeApi.sampleIndex;
          return timeData[idx];
        },
        enableDefaultViewerRuntime(options) {
          useDefaultViewerRuntime = true;
          const opts = options && typeof options === 'object' ? options : {};
          if (typeof opts.selectedObjectName === 'string' && opts.selectedObjectName.trim().length > 0) {
            selectedObjectId = opts.selectedObjectName.trim();
          }
          if (typeof opts.followSelected === 'boolean') {
            followSelected = opts.followSelected;
            followInput.checked = followSelected;
          }
        },
        refreshDefaultViewerRuntime() {
          if (viewerReady) {
            renderDefaultViewer(runtimeApi.sampleIndex || 0);
          } else {
            refreshSceneObjects(false);
            updateCameraInfo();
          }
        }
      };

      function setInspectorVisible(next) {
        inspectorVisible = !!next;
        inspectorEl.classList.toggle('hidden', !inspectorVisible);
      }

      function getControlTarget() {
        const controlsObj = runtimeApi.state?.controls;
        const target = controlsObj && controlsObj.target;
        if (target && Number.isFinite(Number(target.x)) && Number.isFinite(Number(target.y)) && Number.isFinite(Number(target.z))) {
          return target;
        }
        return fallbackTarget;
      }

      function setControlTarget(x, y, z) {
        const controlsObj = runtimeApi.state?.controls;
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
        const camera = runtimeApi.state?.camera;
        if (camera && typeof camera.lookAt === 'function') {
          camera.lookAt(fallbackTarget.x, fallbackTarget.y, fallbackTarget.z);
        }
      }

      function objectIdFor(obj) {
        const name = (obj && typeof obj.name === 'string' ? obj.name.trim() : '');
        if (name.length > 0) {
          return name;
        }
        return String((obj && obj.type) || 'Object') + '#' + String((obj && obj.id) || '?');
      }

      function objectLabelFor(obj) {
        const name = (obj && typeof obj.name === 'string' ? obj.name.trim() : '');
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
          const sw = document.createElement('span');
          sw.className = 'swatch';
          const label = document.createElement('span');
          label.textContent = entry.label;
          row.appendChild(sw);
          row.appendChild(label);
          row.addEventListener('click', () => {
            selectedObjectId = entry.id;
            renderObjectList();
          });
          objectListEl.appendChild(row);
        }
      }

      function refreshSceneObjects(force) {
        const scene = runtimeApi.state?.scene;
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
        scene.traverse((obj) => {
          if (!obj || obj === scene) return;
          if (obj.visible === false) return;
          const id = objectIdFor(obj);
          nextObjects.push({
            id,
            label: objectLabelFor(obj),
            obj,
          });
        });
        nextObjects.sort((a, b) => String(a.label).localeCompare(String(b.label)));
        sceneObjects = nextObjects;
        if (!selectedObjectId && sceneObjects.length > 0) {
          selectedObjectId = sceneObjects[0].id;
        }
        if (selectedObjectId) {
          const exists = sceneObjects.some((entry) => entry.id === selectedObjectId);
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
        const entry = sceneObjects.find((item) => item.id === selectedObjectId);
        return entry ? entry.obj : undefined;
      }

      function focusSelectedObject() {
        if (!THREE_NS) return;
        const selected = getSelectedObject();
        const camera = runtimeApi.state?.camera;
        if (!selected || !camera) return;
        try {
          const center = new THREE_NS.Vector3();
          const box = new THREE_NS.Box3().setFromObject(selected);
          if (!box.isEmpty()) {
            box.getCenter(center);
          } else if (selected.position) {
            center.set(Number(selected.position.x) || 0, Number(selected.position.y) || 0, Number(selected.position.z) || 0);
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
          if (typeof runtimeApi.state?.renderNow === 'function') {
            runtimeApi.state.renderNow();
          }
        } catch (error) {
          showError('Focus failed: ' + String(error));
        }
      }

      function followSelectedObjectIfEnabled() {
        if (!followSelected || !THREE_NS) return;
        const selected = getSelectedObject();
        if (!selected) return;
        try {
          const pos = new THREE_NS.Vector3();
          if (typeof selected.getWorldPosition === 'function') {
            selected.getWorldPosition(pos);
          } else if (selected.position) {
            pos.set(Number(selected.position.x) || 0, Number(selected.position.y) || 0, Number(selected.position.z) || 0);
          } else {
            return;
          }
          setControlTarget(pos.x, pos.y, pos.z);
        } catch (_) {
          // ignore follow errors
        }
      }

      function updateCameraInfo() {
        const camera = runtimeApi.state?.camera;
        if (!camera || !camera.position || !camera.rotation) {
          cameraInfoEl.textContent = 'camera: unavailable';
          return;
        }
        const toDeg = (rad) => Number(rad) * (180 / Math.PI);
        const px = Number(camera.position.x);
        const py = Number(camera.position.y);
        const pz = Number(camera.position.z);
        const rx = toDeg(camera.rotation.x);
        const ry = toDeg(camera.rotation.y);
        const rz = toDeg(camera.rotation.z);
        const target = getControlTarget();
        const posLine = 'pos ' + formatNum(px) + ', ' + formatNum(py) + ', ' + formatNum(pz);
        const angLine = 'deg ' + formatNum(rx) + ', ' + formatNum(ry) + ', ' + formatNum(rz);
        const tgtLine = 'tgt ' + formatNum(target.x) + ', ' + formatNum(target.y) + ', ' + formatNum(target.z);
        cameraInfoEl.textContent = posLine + '\\n' + angLine + '\\n' + tgtLine;
      }

      function createSimpleControls(camera) {
        if (!THREE_NS || !camera || !camera.position) {
          return null;
        }
        const target = new THREE_NS.Vector3(0, 0.5, 0);
        let radius = camera.position.distanceTo(target);
        if (!Number.isFinite(radius) || radius < 0.2) radius = 4.0;
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

        function onPointerDown(ev) {
          if (!ev.isPrimary) return;
          dragging = true;
          panning = ev.button === 1 || ev.button === 2;
          lastX = ev.clientX;
          lastY = ev.clientY;
          try { canvas.setPointerCapture(ev.pointerId); } catch (_) {}
          ev.preventDefault();
        }
        function onPointerMove(ev) {
          if (!dragging) return;
          const dx = ev.clientX - lastX;
          const dy = ev.clientY - lastY;
          lastX = ev.clientX;
          lastY = ev.clientY;
          if (panning) {
            const panScale = Math.max(0.002, radius * 0.0014);
            const dir = new THREE_NS.Vector3();
            camera.getWorldDirection(dir);
            const right = new THREE_NS.Vector3().crossVectors(dir, camera.up).normalize();
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
        function onPointerUp(ev) {
          dragging = false;
          try { canvas.releasePointerCapture(ev.pointerId); } catch (_) {}
        }
        function onWheel(ev) {
          const factor = ev.deltaY > 0 ? 1.08 : 0.92;
          radius = Math.max(0.15, Math.min(300, radius * factor));
          applyOrbit();
          ev.preventDefault();
        }
        canvas.addEventListener('pointerdown', onPointerDown);
        canvas.addEventListener('pointermove', onPointerMove);
        canvas.addEventListener('pointerup', onPointerUp);
        canvas.addEventListener('pointerleave', onPointerUp);
        canvas.addEventListener('wheel', onWheel, { passive: false });
        applyOrbit();

        return {
          target,
          update: applyOrbit,
          dispose() {
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
          renderer = new THREE_NS.WebGLRenderer({ canvas, antialias: true, alpha: false });
          renderer.setPixelRatio(Math.max(1, Math.min(2, window.devicePixelRatio || 1)));
          runtimeApi.state.renderer = renderer;
        }
        if (!runtimeApi.state.controls) {
          runtimeApi.state.controls = createSimpleControls(camera);
        }

        runtimeApi.state.renderNow = () => {
          renderer.render(scene, camera);
        };
        runtimeApi.state.resize = () => {
          const size = runtimeApi.getViewportSize();
          const w = Math.max(1, size.width || 1);
          const h = Math.max(1, size.height || 1);
          renderer.setSize(w, h, false);
          camera.aspect = w / h;
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
          } catch (err) {
            showError('3D resize error:\\n' + String(err));
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

      if (typeof view.script === 'string' && view.script.trim().length > 0) {
        try {
          const fn = new Function('ctx', 'api', view.script + '\\nreturn ctx;');
          fn(runtimeCtx, runtimeApi);
          onInit = typeof runtimeCtx.onInit === 'function' ? runtimeCtx.onInit : null;
          onFrame = typeof runtimeCtx.onFrame === 'function' ? runtimeCtx.onFrame : null;
          onResize = typeof runtimeCtx.onResize === 'function' ? runtimeCtx.onResize : null;
        } catch (err) {
          showError('3D script compile error:\\n' + String(err));
        }
      }

      function drawFallback(sampleIndex) {
        clearFallback();
        if (!context2d) return;
        const xSeries = getSeries('x');
        const x = xSeries && sampleIndex < xSeries.length ? xSeries[sampleIndex] : 0;
        const yPix = canvas.height * 0.8 - (Number.isFinite(x) ? x : 0) * 16;
        context2d.fillStyle = '#3cb4ff';
        context2d.beginPath();
        context2d.arc(canvas.width * 0.5, yPix, 14, 0, Math.PI * 2);
        context2d.fill();
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
        } catch (err) {
          showError('3D init error:\\n' + String(err));
        }
        viewerReady = true;
        refreshSceneObjects(true);
        if (onResize) {
          try {
            onResize(runtimeApi);
          } catch (err) {
            showError('3D resize error:\\n' + String(err));
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
          } catch (err) {
            showError('3D frame error:\\n' + String(err));
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

      objectsBtn.addEventListener('click', () => {
        setInspectorVisible(!inspectorVisible);
      });
      focusBtn.addEventListener('click', () => {
        focusSelectedObject();
      });
      followInput.addEventListener('change', () => {
        followSelected = followInput.checked;
      });

      canvas.addEventListener('pointerdown', refreshInteractiveView);
      canvas.addEventListener('pointermove', refreshInteractiveView);
      canvas.addEventListener('wheel', refreshInteractiveView, { passive: true });
      resizeCanvas();
      window.addEventListener('resize', resizeViewer);

      let playing = false;
      let playbackDirection = 1;
      let playbackRate = 1;
      let playbackRaf = 0;
      let playbackAnchorWallMs = 0;
      let playbackAnchorTime = 0;
      let playbackAnchorIndex = 0;
      let lastRenderedIndex = 0;
      let exportingMovie = false;

      function updateTransportUi() {
        playPauseBtn.innerHTML = playing ? '&#10074;&#10074;' : '&#9654;';
        rewindBtn.classList.toggle('active', playing && playbackDirection < 0);
        fastForwardBtn.classList.toggle('active', playing && playbackDirection > 0 && playbackRate > 1);
        exportMovieBtn.disabled = exportingMovie;
        exportMovieBtn.textContent = exportingMovie ? 'Exporting…' : 'Export Movie';
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
        if (playing) return;
        playing = true;
        playbackDirection = direction < 0 ? -1 : 1;
        playbackRate = Math.max(1, Number(rate) || 1);
        playbackAnchorIndex = clampSampleIndex(runtimeApi.sampleIndex || Number(slider.value) || 0);
        playbackAnchorTime = timeAtIndex(playbackAnchorIndex);
        playbackAnchorWallMs = performance.now();
        lastRenderedIndex = playbackAnchorIndex;
        updateTransportUi();

        const tick = (now) => {
          if (!playing) {
            return;
          }
          const elapsedSeconds = Math.max(0, (now - playbackAnchorWallMs) / 1000);
          const firstTime = timeAtIndex(0);
          const lastTime = timeAtIndex(Math.max(0, timeData.length - 1));
          const targetTime =
            playbackAnchorTime + (playbackDirection * playbackRate * elapsedSeconds);

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
      playPauseBtn.addEventListener('click', () => {
        if (playing) {
          stop();
          return;
        }
        start(1, 1);
      });
      rewindBtn.addEventListener('click', () => {
        if (playing && playbackDirection < 0) {
          stop();
          return;
        }
        if (playing) {
          stop();
        }
        start(-1, 4);
      });
      fastForwardBtn.addEventListener('click', () => {
        if (playing && playbackDirection > 0 && playbackRate > 1) {
          stop();
          return;
        }
        if (playing) {
          stop();
        }
        start(1, 4);
      });
      startBtn.addEventListener('click', () => {
        stop();
        renderSample(0);
      });
      endBtn.addEventListener('click', () => {
        stop();
        renderSample(Number(slider.max));
      });
      slider.addEventListener('input', () => renderSample(Number(slider.value)));

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
            if (typeof MediaRecorder.isTypeSupported === 'function' && MediaRecorder.isTypeSupported(candidate)) {
              mimeType = candidate;
              break;
            }
          }
          const recorder = mimeType.length > 0
            ? new MediaRecorder(stream, { mimeType, videoBitsPerSecond: 8_000_000 })
            : new MediaRecorder(stream, { videoBitsPerSecond: 8_000_000 });
          const chunks = [];
          recorder.ondataavailable = (event) => {
            if (event.data && event.data.size > 0) {
              chunks.push(event.data);
            }
          };
          const stopPromise = new Promise((resolve, reject) => {
            recorder.onerror = () => reject(recorder.error || new Error('MediaRecorder failed'));
            recorder.onstop = () => resolve();
          });
          recorder.start(Math.max(40, Math.floor(1000 / fps)));
          for (let frame = 0; frame < totalFrames; frame++) {
            renderSample(frame);
            await new Promise((resolve) => setTimeout(resolve, Math.max(1, Math.floor(1000 / fps))));
          }
          recorder.stop();
          await stopPromise;
          const blob = new Blob(chunks, { type: recorder.mimeType || 'video/webm' });
          if (blob.size <= 0) {
            throw new Error('No video frames captured.');
          }
          if (vscodeApi) {
            const reader = new FileReader();
            await new Promise((resolve, reject) => {
              reader.onerror = () => reject(new Error('Failed to encode video payload.'));
              reader.onload = () => resolve(undefined);
              reader.readAsDataURL(blob);
            });
            vscodeApi.postMessage({
              command: 'saveMovieWebm',
              dataUrl: String(reader.result || ''),
              defaultName: sanitizeFileBaseName(modelName) + '_viewer.webm',
            });
          } else {
            const url = URL.createObjectURL(blob);
            const link = document.createElement('a');
            link.href = url;
            link.download = sanitizeFileBaseName(modelName) + '_viewer.webm';
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
            URL.revokeObjectURL(url);
          }
        } catch (error) {
          showError('Movie export failed: ' + String(error));
          if (vscodeApi) {
            vscodeApi.postMessage({
              command: 'saveMovieWebmError',
              message: String(error),
            });
          }
        } finally {
          exportingMovie = false;
          renderSample(originalSample);
          if (wasPlaying) {
            start(1, 1);
          }
          updateTransportUi();
        }
      }
      exportMovieBtn.addEventListener('click', () => {
        void exportMovieWebm();
      });

      renderSample(0);
      updateTransportUi();
      void initializeViewer();
      root.__onActivate = () => {
        resizeViewer();
        renderSample(runtimeApi.sampleIndex || Number(slider.value) || 0);
      };
      root.__onDispose = () => {
        window.removeEventListener('resize', resizeViewer);
        if (timer) {
          clearInterval(timer);
          timer = null;
        }
        if (runtimeApi.state.controls && typeof runtimeApi.state.controls.dispose === 'function') {
          try { runtimeApi.state.controls.dispose(); } catch (_) {}
        }
      };
      return root;
    }

    function createViewErrorPanel(title, error) {
      const root = document.createElement('section');
      root.className = 'view';
      const empty = document.createElement('div');
      empty.className = 'scatter-error';
      empty.style.position = 'relative';
      empty.style.inset = 'auto';
      empty.style.display = 'flex';
      empty.style.flex = '1 1 auto';
      empty.style.padding = '12px';
      empty.style.whiteSpace = 'pre-wrap';
      empty.style.textAlign = 'left';
      empty.textContent = 'Failed to render view "' + String(title || 'View') + '".\\n' + String(error || 'Unknown error');
      root.appendChild(empty);
      return root;
    }

    function buildViews() {
      const views = Array.isArray(configuredViews) && configuredViews.length > 0
        ? configuredViews
        : [{ id: 'states_time', title: 'States vs Time', type: 'timeseries', x: 'time', y: ['*states'] }];
      const rendered = [];
      for (const view of views) {
        const safeView = view && typeof view === 'object' ? view : { title: 'View', type: 'timeseries', y: ['*states'] };
        const tab = document.createElement('button');
        tab.className = 'tab';
        tab.textContent = safeView.title || safeView.id || 'View';
        tabsEl.appendChild(tab);
        rendered.push({ tab, view: safeView });
      }
      if (rendered.length === 0) {
        const tab = document.createElement('button');
        tab.className = 'tab';
        tab.textContent = 'States vs Time';
        tabsEl.appendChild(tab);
        rendered.push({
          tab,
          view: { id: 'states_time', title: 'States vs Time', type: 'timeseries', x: 'time', y: ['*states'] },
        });
      }

      function createPanelForView(safeView) {
        let panel;
        try {
          panel = safeView.type === '3d'
            ? createViewer3dView(safeView)
            : safeView.type === 'scatter'
              ? createScatterView(safeView)
              : createPlotView(safeView);
        } catch (error) {
          console.error('[rumoca] results view construction failed', safeView, error);
          panel = createViewErrorPanel(safeView.title || safeView.id || 'View', error);
        }
        panel.classList.add('active');
        panel.style.display = 'flex';
        panel.style.position = 'absolute';
        panel.style.top = '0';
        panel.style.right = '0';
        panel.style.bottom = '0';
        panel.style.left = '0';
        panel.style.width = '100%';
        panel.style.height = '100%';
        panel.style.minHeight = '0';
        return panel;
      }

      let mountedPanel = null;
      function activate(index) {
        if (index < 0 || index >= rendered.length) {
          return;
        }
        for (let i = 0; i < rendered.length; i++) {
          rendered[i].tab.classList.toggle('active', i === index);
        }
        if (mountedPanel) {
          if (typeof mountedPanel.__onDispose === 'function') {
            try {
              mountedPanel.__onDispose();
            } catch (error) {
              console.error('[rumoca] results view dispose failed', error);
            }
          }
          if (mountedPanel.parentElement === contentEl) {
            contentEl.removeChild(mountedPanel);
          }
          mountedPanel = null;
        }
        const next = rendered[index];
        const panel = createPanelForView(next.view);
        contentEl.appendChild(panel);
        mountedPanel = panel;
        requestAnimationFrame(() => {
          if (mountedPanel !== panel) return;
          if (typeof panel.__onActivate === 'function') {
            try {
              panel.__onActivate();
            } catch (error) {
              console.error('[rumoca] results view activate failed', error);
            }
          }
        });
      }
      for (let i = 0; i < rendered.length; i++) {
        rendered[i].tab.addEventListener('click', () => activate(i));
      }
      activate(0);
    }

    setTimingSummary();
    buildViews();
  </script>
</body>
</html>`;
}

async function showSimulationSettingsPanel(
    context: vscode.ExtensionContext,
    activeModel: string,
    workspaceRoot: string | undefined
): Promise<void> {
    await resyncProjectSidecars(workspaceRoot, {
        reason: 'open-settings',
        pruneOrphans: false,
    });
    const fallbackDefaults = getSimulationSettings(vscode.workspace.getConfiguration('rumoca'));
    const projectConfig = await getProjectSimulationConfig(activeModel, workspaceRoot, fallbackDefaults);
    const defaults = projectConfig?.defaults ?? fallbackDefaults;
    const preset = normalizeSimulationPreset(projectConfig?.preset);
    const current: ModelSimulationPreset = preset ?? {
        tEnd: defaults.tEnd,
        dt: defaults.dt,
        solver: defaults.solver,
        outputDir: defaults.outputDir,
        libraryOverrides: [...defaults.modelicaPath],
    };
    const currentOutputDirJson = JSON.stringify(current.outputDir ?? '');
    const configuredViews = await getProjectVisualizationConfig(activeModel, workspaceRoot);
    const currentViews = configuredViews.length > 0 ? configuredViews : defaultVisualizationViews();
    const currentViewsJson = JSON.stringify(currentViews).replace(/<\/script/gi, '<\\/script');

    const panel = vscode.window.createWebviewPanel(
        'rumocaSimulationSettings',
        `Rumoca Settings: ${activeModel}`,
        vscode.ViewColumn.Beside,
        { enableScripts: true, retainContextWhenHidden: true }
    );

    panel.webview.html = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <style>
    :root {
      --pad: 16px;
      --radius: 8px;
      --card-border: var(--vscode-panel-border, var(--vscode-input-border));
      --muted: var(--vscode-descriptionForeground);
      --ok: var(--vscode-testing-iconPassed);
      --error: var(--vscode-errorForeground);
    }
    html, body { height: 100%; }
    body {
      margin: 0;
      font-family: var(--vscode-font-family);
      color: var(--vscode-foreground);
      background: var(--vscode-editor-background);
    }
    .page {
      padding: var(--pad);
      display: grid;
      gap: 12px;
      min-height: 100%;
      grid-template-rows: auto auto auto auto auto 1fr auto;
    }
    .header {
      display: flex;
      align-items: flex-start;
      justify-content: space-between;
      gap: 12px;
    }
    .title {
      font-size: 18px;
      font-weight: 700;
      margin: 0;
      letter-spacing: 0.01em;
    }
    .subtitle {
      margin-top: 4px;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.4;
    }
    .model-chip {
      border: 1px solid var(--card-border);
      border-radius: 999px;
      padding: 4px 10px;
      font-size: 12px;
      white-space: nowrap;
      color: var(--vscode-editorInfo-foreground, var(--vscode-textLink-foreground));
      background: var(--vscode-editor-inactiveSelectionBackground, transparent);
    }
    .card {
      border: 1px solid var(--card-border);
      border-radius: var(--radius);
      padding: 12px;
      background: var(--vscode-sideBar-background, transparent);
    }
    .card h3 {
      margin: 0 0 10px 0;
      font-size: 12px;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.04em;
      color: var(--muted);
    }
    .grid {
      display: grid;
      grid-template-columns: repeat(3, minmax(0, 1fr));
      gap: 10px;
    }
    .field { display: grid; gap: 4px; }
    label { font-size: 12px; font-weight: 600; }
    input, textarea, select {
      width: 100%;
      box-sizing: border-box;
      background: var(--vscode-input-background);
      color: var(--vscode-input-foreground);
      border: 1px solid var(--vscode-input-border);
      border-radius: 6px;
      padding: 7px 8px;
      font-size: 12px;
      min-height: 30px;
    }
    textarea { min-height: 140px; resize: vertical; font-family: var(--vscode-editor-font-family, monospace); }
    .hint { color: var(--muted); font-size: 11px; line-height: 1.35; }
    .row { display: flex; gap: 8px; align-items: center; }
    .row .grow { flex: 1; }
    .toolbar { display: flex; gap: 8px; margin-bottom: 8px; flex-wrap: wrap; }
    .results-layout {
      display: grid;
      grid-template-columns: minmax(220px, 0.9fr) minmax(320px, 1.6fr);
      gap: 10px;
      min-height: 280px;
    }
    .results-list-pane, .results-editor-pane {
      border: 1px solid var(--card-border);
      border-radius: 6px;
      padding: 10px;
      background: var(--vscode-editor-background);
    }
    #viewList {
      min-height: 180px;
      height: 100%;
    }
    .stack { display: grid; gap: 8px; }
    .mono { font-family: var(--vscode-editor-font-family, monospace); }
    #viewScriptRow { display: none; }
    @media (max-width: 980px) {
      .results-layout { grid-template-columns: 1fr; }
      #viewList { min-height: 120px; }
    }
    button {
      border: 0;
      border-radius: 6px;
      padding: 6px 10px;
      font-size: 12px;
      cursor: pointer;
      background: var(--vscode-button-background);
      color: var(--vscode-button-foreground);
    }
    button.secondary {
      background: var(--vscode-button-secondaryBackground);
      color: var(--vscode-button-secondaryForeground);
    }
    button.ghost {
      background: transparent;
      border: 1px solid var(--card-border);
      color: var(--vscode-foreground);
    }
    .footer {
      position: sticky;
      bottom: 0;
      padding-top: 8px;
      background: linear-gradient(to bottom, transparent, var(--vscode-editor-background) 22%);
    }
    .actions {
      display: flex;
      gap: 8px;
      flex-wrap: wrap;
      align-items: center;
      border: 1px solid var(--card-border);
      border-radius: var(--radius);
      padding: 10px;
      background: var(--vscode-editorWidget-background, var(--vscode-editor-background));
    }
    .actions-hint {
      width: 100%;
      margin-top: 2px;
      font-size: 12px;
      color: var(--muted);
    }
    .status {
      margin-left: auto;
      font-size: 12px;
      color: var(--muted);
      min-height: 16px;
    }
    .status.ok { color: var(--ok); }
    .status.error { color: var(--error); }
    @media (max-width: 900px) {
      .grid { grid-template-columns: 1fr; }
      .status { width: 100%; margin-left: 0; }
    }
  </style>
</head>
<body>
  <div class="page">
    <div class="header">
      <div>
        <h1 class="title">Simulation Preset</h1>
        <div class="subtitle">One-click Play uses these values for this model.</div>
      </div>
      <div class="model-chip">Model: ${escapeHtml(activeModel)}</div>
    </div>

    <div class="card">
      <h3>Solver</h3>
      <div class="grid">
        <div class="field">
          <label for="solver">Method</label>
          <select id="solver">
            <option value="auto" ${current.solver === 'auto' ? 'selected' : ''}>auto</option>
            <option value="bdf" ${current.solver === 'bdf' ? 'selected' : ''}>bdf</option>
            <option value="rk-like" ${current.solver === 'rk-like' ? 'selected' : ''}>rk-like</option>
          </select>
        </div>
        <div class="field">
          <label for="tEnd">End Time (t_end)</label>
          <input id="tEnd" type="number" step="0.01" value="${current.tEnd}">
        </div>
        <div class="field">
          <label for="dt">Fixed Step (dt, optional)</label>
          <input id="dt" type="number" step="0.0001" value="${current.dt ?? ''}" placeholder="auto">
        </div>
      </div>
    </div>

    <div class="card">
      <h3>Libraries</h3>
      <div class="toolbar">
        <button id="addLib" class="ghost">Add Library Directory...</button>
        <button id="clearLibs" class="ghost">Clear</button>
      </div>
      <div class="field">
        <label for="modelicaPath">Additional Library Paths For This Model</label>
        <textarea id="modelicaPath" placeholder="/path/to/ModelicaStandardLibrary">${escapeHtml(current.libraryOverrides.join('\n'))}</textarea>
        <div class="hint">If set, these are appended to the default library paths for this model.</div>
      </div>
    </div>

    <div class="card">
      <h3>Results Panels</h3>
      <div class="results-layout">
        <div class="results-list-pane">
          <div class="field">
            <label for="viewList">Configured Panels</label>
            <select id="viewList" size="9" class="mono"></select>
          </div>
          <div class="toolbar" style="margin-top: 8px;">
            <button id="addView" class="ghost">+ Add Panel</button>
            <button id="removeView" class="ghost">Remove</button>
          </div>
        </div>
        <div class="results-editor-pane stack">
          <div class="field">
            <label for="viewTitle">Panel Title</label>
            <input id="viewTitle" placeholder="States vs Time">
          </div>
          <div class="field">
            <label for="viewType">Type</label>
            <select id="viewType">
              <option value="timeseries">timeseries</option>
              <option value="scatter">scatter</option>
              <option value="3d">3d</option>
            </select>
          </div>
          <div class="field" id="viewXRow">
            <label id="viewXLabel" for="viewX">X Expression</label>
            <input id="viewX" placeholder="time">
          </div>
          <div class="field" id="viewYSeriesRow">
            <label for="viewY">Series / Variables</label>
            <textarea id="viewY" class="mono" style="min-height: 110px;" placeholder="*states"></textarea>
            <div id="viewYHint" class="hint">Use one variable per line. Special token: <code>*states</code>.</div>
          </div>
          <div class="field" id="viewYScatterRow" style="display:none;">
            <label for="viewScatterSeriesList">Scatter Series</label>
            <div class="results-layout" style="grid-template-columns: minmax(200px, 0.95fr) minmax(220px, 1.05fr); min-height: 220px;">
              <div class="results-list-pane" style="padding:8px;">
                <select id="viewScatterSeriesList" size="7" class="mono" style="min-height: 150px;"></select>
                <div class="toolbar" style="margin-top: 8px;">
                  <button id="addScatterSeries" class="ghost">+ Add Series</button>
                  <button id="removeScatterSeries" class="ghost">Remove Series</button>
                </div>
              </div>
              <div class="results-editor-pane stack" style="padding:8px;">
                <div class="field">
                  <label for="viewScatterSeriesName">Series Name</label>
                  <input id="viewScatterSeriesName" placeholder="Actual trajectory">
                </div>
                <div class="field">
                  <label for="viewScatterSeriesX">X Expression</label>
                  <input id="viewScatterSeriesX" placeholder="x">
                </div>
                <div class="field">
                  <label for="viewScatterSeriesY">Y Expression</label>
                  <input id="viewScatterSeriesY" placeholder="y">
                </div>
              </div>
            </div>
          </div>
          <div id="viewScriptRow" class="field">
            <label for="viewScriptPath">3D Script File</label>
            <div class="row">
              <input id="viewScriptPath" class="grow mono" placeholder=".rumoca/viewer3d/Ball/3d_viewer.js">
              <button id="openViewScript" class="ghost">Open Script File</button>
            </div>
            <div class="hint">Script file is stored under <code>.rumoca/viewer3d/</code> and edited in the regular text editor.</div>
          </div>
        </div>
      </div>
      <div class="hint" style="margin-top: 8px;">Play opens one <code>Rumoca Results</code> window with these configured subpanels.</div>
    </div>

    <div class="footer">
      <div class="actions">
        <button id="reset" class="secondary">Reset Preset</button>
        <button id="resyncSidecars" class="secondary">Resync Sidecars</button>
        <button id="workspaceSettings" class="secondary">Workspace Settings</button>
        <button id="userSettings" class="secondary">User Settings</button>
        <div id="status" class="status"></div>
        <div class="actions-hint">Autosaves per-model preset for <code>${escapeHtml(activeModel)}</code> in <code>.rumoca/models/by-id/&lt;uuid&gt;/simulation.toml</code> and panel layout in <code>.rumoca/models/by-id/&lt;uuid&gt;/views.toml</code>. Play uses these values.</div>
      </div>
    </div>
  </div>

  <script>
    const vscode = acquireVsCodeApi();
    const solverInput = document.getElementById('solver');
    const tEndInput = document.getElementById('tEnd');
    const dtInput = document.getElementById('dt');
    const preservedOutputDir = ${currentOutputDirJson};
    const modelicaPathInput = document.getElementById('modelicaPath');
    const clearLibsBtn = document.getElementById('clearLibs');
    const statusEl = document.getElementById('status');
    const viewListInput = document.getElementById('viewList');
    const viewTitleInput = document.getElementById('viewTitle');
    const viewTypeInput = document.getElementById('viewType');
    const viewXRow = document.getElementById('viewXRow');
    const viewXLabelEl = document.getElementById('viewXLabel');
    const viewXInput = document.getElementById('viewX');
    const viewYInput = document.getElementById('viewY');
    const viewYSeriesRow = document.getElementById('viewYSeriesRow');
    const viewYScatterRow = document.getElementById('viewYScatterRow');
    const viewScatterSeriesListInput = document.getElementById('viewScatterSeriesList');
    const addScatterSeriesBtn = document.getElementById('addScatterSeries');
    const removeScatterSeriesBtn = document.getElementById('removeScatterSeries');
    const viewScatterSeriesNameInput = document.getElementById('viewScatterSeriesName');
    const viewScatterSeriesXInput = document.getElementById('viewScatterSeriesX');
    const viewScatterSeriesYInput = document.getElementById('viewScatterSeriesY');
    const viewYHintEl = document.getElementById('viewYHint');
    const viewScriptPathInput = document.getElementById('viewScriptPath');
    const viewScriptRow = document.getElementById('viewScriptRow');
    const openViewScriptBtn = document.getElementById('openViewScript');
    const pageEl = document.querySelector('.page');
    const initialViews = ${currentViewsJson};
    const activeModelName = ${JSON.stringify(activeModel)};
    let views = Array.isArray(initialViews) && initialViews.length > 0
      ? initialViews
      : [{ id: 'states_time', title: 'States vs Time', type: 'timeseries', x: 'time', y: ['*states'] }];
    let selectedViewIndex = views.length > 0 ? 0 : -1;
    let selectedScatterSeriesIndex = -1;
    let autoSaveTimer = null;
    let isResettingFromHost = false;

    function setStatus(text, level) {
      statusEl.textContent = text || '';
      statusEl.classList.remove('ok', 'error');
      if (level) statusEl.classList.add(level);
    }

    function parseSeriesList(text) {
      return String(text || '')
        .split(/\\r?\\n|,/)
        .map(v => v.trim())
        .filter(Boolean);
    }

    function stringifySeries(list) {
      return (Array.isArray(list) ? list : []).map(v => String(v).trim()).filter(Boolean).join('\\n');
    }

    function makeUniqueViewId(prefix) {
      const base = (prefix || 'view').replace(/[^a-zA-Z0-9_]+/g, '_').toLowerCase();
      const existing = new Set(views.map(v => String(v.id || '')));
      let candidate = base + '_' + String(views.length + 1);
      let counter = 1;
      while (existing.has(candidate)) {
        counter += 1;
        candidate = base + '_' + String(views.length + counter);
      }
      return candidate;
    }

    function sanitizePathSegment(input) {
      const cleaned = String(input || '')
        .trim()
        .replace(/[^a-zA-Z0-9._-]+/g, '_')
        .replace(/^_+|_+$/g, '');
      return cleaned.length > 0 ? cleaned : 'view';
    }

    function defaultScriptPath(viewId) {
      return '.rumoca/viewer3d/' + sanitizePathSegment(activeModelName) + '/' + sanitizePathSegment(viewId) + '.js';
    }

    function getActiveView() {
      if (selectedViewIndex < 0 || selectedViewIndex >= views.length) {
        return null;
      }
      return views[selectedViewIndex];
    }

    function ensureScatterSeries(view) {
      if (!view || view.type !== 'scatter') {
        return [];
      }
      if (!Array.isArray(view.scatterSeries)) {
        view.scatterSeries = [];
      }
      if (view.scatterSeries.length === 0) {
        const fallbackX = String(view.x || 'time').trim() || 'time';
        const fallbackY = Array.isArray(view.y) && view.y.length > 0 ? String(view.y[0] || '').trim() : '';
        if (fallbackY.length > 0) {
          view.scatterSeries.push({
            name: fallbackY + ' vs ' + fallbackX,
            x: fallbackX,
            y: fallbackY,
          });
        }
      }
      view.scatterSeries = view.scatterSeries
        .map((entry) => {
          const name = String(entry && entry.name !== undefined ? entry.name : '').trim();
          const x = String(entry && entry.x !== undefined ? entry.x : '').trim();
          const y = String(entry && entry.y !== undefined ? entry.y : '').trim();
          if (x.length === 0 || y.length === 0) {
            return null;
          }
          return {
            name: name.length > 0 ? name : (y + ' vs ' + x),
            x,
            y,
          };
        })
        .filter(Boolean);
      return view.scatterSeries;
    }

    function setScatterSeriesEditorEnabled(enabled) {
      viewScatterSeriesListInput.disabled = !enabled;
      addScatterSeriesBtn.disabled = !enabled;
      removeScatterSeriesBtn.disabled = !enabled;
      viewScatterSeriesNameInput.disabled = !enabled;
      viewScatterSeriesXInput.disabled = !enabled;
      viewScatterSeriesYInput.disabled = !enabled;
    }

    function renderScatterSeriesEditor() {
      const view = getActiveView();
      if (!view || view.type !== 'scatter') {
        viewScatterSeriesListInput.innerHTML = '';
        viewScatterSeriesNameInput.value = '';
        viewScatterSeriesXInput.value = '';
        viewScatterSeriesYInput.value = '';
        selectedScatterSeriesIndex = -1;
        setScatterSeriesEditorEnabled(false);
        return;
      }
      const scatterSeries = ensureScatterSeries(view);
      viewScatterSeriesListInput.innerHTML = '';
      for (let i = 0; i < scatterSeries.length; i += 1) {
        const series = scatterSeries[i];
        const option = document.createElement('option');
        option.value = String(i);
        option.textContent = String(series.name || ('Series ' + String(i + 1)));
        viewScatterSeriesListInput.appendChild(option);
      }
      if (scatterSeries.length === 0) {
        selectedScatterSeriesIndex = -1;
      } else if (selectedScatterSeriesIndex < 0 || selectedScatterSeriesIndex >= scatterSeries.length) {
        selectedScatterSeriesIndex = 0;
      }
      setScatterSeriesEditorEnabled(true);
      if (selectedScatterSeriesIndex >= 0) {
        viewScatterSeriesListInput.value = String(selectedScatterSeriesIndex);
        const selectedSeries = scatterSeries[selectedScatterSeriesIndex];
        viewScatterSeriesNameInput.value = String(selectedSeries.name || '');
        viewScatterSeriesXInput.value = String(selectedSeries.x || '');
        viewScatterSeriesYInput.value = String(selectedSeries.y || '');
      } else {
        viewScatterSeriesNameInput.value = '';
        viewScatterSeriesXInput.value = '';
        viewScatterSeriesYInput.value = '';
      }
    }

    function commitScatterSeriesEditor() {
      const view = getActiveView();
      if (!view || view.type !== 'scatter') {
        return;
      }
      const scatterSeries = ensureScatterSeries(view);
      if (selectedScatterSeriesIndex < 0 || selectedScatterSeriesIndex >= scatterSeries.length) {
        return;
      }
      const current = scatterSeries[selectedScatterSeriesIndex];
      const nextX = String(viewScatterSeriesXInput.value || '').trim();
      const nextY = String(viewScatterSeriesYInput.value || '').trim();
      current.name = String(viewScatterSeriesNameInput.value || '').trim();
      current.x = nextX;
      current.y = nextY;
      if (!current.name) {
        current.name = nextY.length > 0 && nextX.length > 0 ? (nextY + ' vs ' + nextX) : ('Series ' + String(selectedScatterSeriesIndex + 1));
      }
      ensureScatterSeries(view);
    }

    function setViewEditorEnabled(enabled) {
      viewTitleInput.disabled = !enabled;
      viewTypeInput.disabled = !enabled;
      viewXInput.disabled = !enabled;
      viewYInput.disabled = !enabled;
      viewScriptPathInput.disabled = !enabled;
      openViewScriptBtn.disabled = !enabled;
      document.getElementById('removeView').disabled = !enabled;
      if (!enabled) {
        setScatterSeriesEditorEnabled(false);
      }
    }

    function applyTypeHint(typeValue) {
      const type = String(typeValue || 'timeseries');
      if (type === '3d') {
        viewXRow.style.display = 'none';
        viewYSeriesRow.style.display = 'none';
        viewYScatterRow.style.display = 'none';
        viewScriptRow.style.display = 'grid';
        viewYHintEl.textContent = '';
      } else if (type === 'scatter') {
        viewXRow.style.display = 'none';
        viewXLabelEl.textContent = 'X Expression';
        viewYSeriesRow.style.display = 'none';
        viewYScatterRow.style.display = 'block';
        viewScriptRow.style.display = 'none';
        viewYHintEl.textContent = '';
      } else {
        viewXRow.style.display = 'block';
        viewXLabelEl.textContent = 'X Expression';
        viewYSeriesRow.style.display = 'block';
        viewYScatterRow.style.display = 'none';
        viewScriptRow.style.display = 'none';
        viewYHintEl.textContent = 'Use one variable per line. Special token: *states.';
      }
    }

    function renderViewEditor() {
      const valid = selectedViewIndex >= 0 && selectedViewIndex < views.length;
      setViewEditorEnabled(valid);
      if (!valid) {
        viewTitleInput.value = '';
        viewTypeInput.value = 'timeseries';
        viewXInput.value = '';
        viewYInput.value = '';
        viewScriptPathInput.value = '';
        selectedScatterSeriesIndex = -1;
        renderScatterSeriesEditor();
        applyTypeHint('timeseries');
        return;
      }
      const view = views[selectedViewIndex];
      viewTitleInput.value = String(view.title || '');
      viewTypeInput.value = String(view.type || 'timeseries');
      viewXInput.value = String(view.x || '');
      viewYInput.value = stringifySeries(view.y);
      const resolvedPath = String(view.scriptPath || '').trim();
      viewScriptPathInput.value =
        resolvedPath.length > 0 ? resolvedPath : defaultScriptPath(String(view.id || ('view_' + String(selectedViewIndex + 1))));
      if (view.type === 'scatter') {
        ensureScatterSeries(view);
      } else {
        selectedScatterSeriesIndex = -1;
      }
      applyTypeHint(view.type);
      renderScatterSeriesEditor();
    }

    function renderViewList() {
      viewListInput.innerHTML = '';
      for (let i = 0; i < views.length; i += 1) {
        const view = views[i];
        const option = document.createElement('option');
        const title = String(view.title || view.id || ('View ' + String(i + 1)));
        option.value = String(i);
        option.textContent = title + ' [' + String(view.type || 'timeseries') + ']';
        viewListInput.appendChild(option);
      }
      if (views.length === 0) {
        selectedViewIndex = -1;
      } else if (selectedViewIndex < 0 || selectedViewIndex >= views.length) {
        selectedViewIndex = 0;
      }
      if (selectedViewIndex >= 0) {
        viewListInput.value = String(selectedViewIndex);
      }
      renderViewEditor();
    }

    function commitViewEditor() {
      if (selectedViewIndex < 0 || selectedViewIndex >= views.length) return;
      const view = views[selectedViewIndex];
      const type = String(viewTypeInput.value || 'timeseries');
      view.type = (type === '3d' || type === 'scatter') ? type : 'timeseries';
      view.title = String(viewTitleInput.value || '').trim();
      if (view.type === '3d') {
        view.x = undefined;
        view.y = [];
        view.scatterSeries = undefined;
      } else if (view.type === 'scatter') {
        view.x = String(viewXInput.value || '').trim();
        if (view.x.length === 0) {
          view.x = undefined;
        }
        commitScatterSeriesEditor();
        const scatterSeries = ensureScatterSeries(view);
        view.scatterSeries = scatterSeries;
        if (scatterSeries.length > 0) {
          view.x = scatterSeries[0].x;
          view.y = [scatterSeries[0].y];
        } else {
          view.y = [];
        }
      } else {
        view.x = String(viewXInput.value || '').trim();
        if (view.x.length === 0) {
          view.x = undefined;
        }
        view.y = parseSeriesList(viewYInput.value);
        view.scatterSeries = undefined;
      }
      if (view.type === '3d') {
        const scriptPath = String(viewScriptPathInput.value || '').trim();
        view.scriptPath = scriptPath.length > 0 ? scriptPath : defaultScriptPath(String(view.id || ('view_' + String(selectedViewIndex + 1))));
      } else {
        view.script = undefined;
        view.scriptPath = undefined;
      }
      if (!view.id || String(view.id).trim().length === 0) {
        view.id = makeUniqueViewId(view.type);
      }
      if (!view.title || String(view.title).trim().length === 0) {
        view.title = 'View ' + String(selectedViewIndex + 1);
      }
    }

    function addView(type) {
      commitViewEditor();
      const nextType = type === '3d' || type === 'scatter' ? type : 'timeseries';
      const next = {
        id: makeUniqueViewId(nextType),
        title: 'New Panel',
        type: nextType,
        x: nextType === 'timeseries' ? 'time' : undefined,
        y: nextType === 'timeseries' ? ['*states'] : [],
        scatterSeries: nextType === 'scatter' ? [{ name: 'Series 1', x: 'x', y: 'y' }] : undefined,
        scriptPath: nextType === '3d' ? defaultScriptPath(makeUniqueViewId('3d_viewer')) : undefined,
      };
      views.push(next);
      selectedViewIndex = views.length - 1;
      renderViewList();
    }

    function collectSavePayload() {
      commitViewEditor();
      const libs = modelicaPathInput.value
        .split(/\\r?\\n/)
        .map(v => v.trim())
        .filter(Boolean);
      const normalizedViews = views.map((view, idx) => {
        const type = String(view.type || 'timeseries');
        const normalizedType = type === '3d' || type === 'scatter' ? type : 'timeseries';
        const yValues = Array.isArray(view.y) ? view.y.map(v => String(v).trim()).filter(Boolean) : [];
        const xValue = view.x === undefined || view.x === null ? undefined : String(view.x).trim();
        const scatterSeriesValues = Array.isArray(view.scatterSeries)
          ? view.scatterSeries
              .map((series) => ({
                name: String(series && series.name !== undefined ? series.name : '').trim(),
                x: String(series && series.x !== undefined ? series.x : '').trim(),
                y: String(series && series.y !== undefined ? series.y : '').trim(),
              }))
              .filter((series) => series.x.length > 0 && series.y.length > 0)
          : [];
        const scriptPathValue = view.scriptPath === undefined || view.scriptPath === null
          ? undefined
          : String(view.scriptPath).trim();
        const normalizedX = normalizedType === '3d'
          ? undefined
          : xValue && xValue.length > 0
            ? xValue
            : undefined;
        const normalizedY = normalizedType === '3d' ? [] : yValues;
        return {
          id: String(view.id || ('view_' + String(idx + 1))).trim(),
          title: String(view.title || ('View ' + String(idx + 1))).trim(),
          type: normalizedType,
          x: normalizedX,
          y: normalizedY,
          scatterSeries: normalizedType === 'scatter' ? scatterSeriesValues : undefined,
          scriptPath: normalizedType === '3d' && scriptPathValue && scriptPathValue.length > 0 ? scriptPathValue : undefined,
        };
      });
      return {
        command: 'save',
        solver: solverInput.value,
        tEnd: Number(tEndInput.value),
        dt: dtInput.value.trim(),
        outputDir: preservedOutputDir,
        modelicaPath: libs,
        views: normalizedViews,
      };
    }

    function saveNow() {
      if (isResettingFromHost) return;
      setStatus('Saving…');
      vscode.postMessage(collectSavePayload());
    }

    function scheduleAutoSave() {
      if (isResettingFromHost) return;
      if (autoSaveTimer) clearTimeout(autoSaveTimer);
      setStatus('Saving…');
      autoSaveTimer = setTimeout(() => {
        autoSaveTimer = null;
        saveNow();
      }, 500);
    }

    function shouldDelegateAutoSave(target) {
      if (!(target instanceof HTMLElement)) return false;
      if (target.closest('#reset, #workspaceSettings, #userSettings, #openViewScript, #addLib')) {
        return false;
      }
      return !!target.closest('input, textarea, select');
    }

    if (pageEl) {
      pageEl.addEventListener('input', (event) => {
        if (shouldDelegateAutoSave(event.target)) {
          scheduleAutoSave();
        }
      }, true);
      pageEl.addEventListener('change', (event) => {
        if (shouldDelegateAutoSave(event.target)) {
          scheduleAutoSave();
        }
      }, true);
    }

    document.getElementById('addView').addEventListener('click', () => {
      const requestedType = String(viewTypeInput.value || 'timeseries');
      addView(requestedType);
      scheduleAutoSave();
    });
    document.getElementById('removeView').addEventListener('click', () => {
      if (selectedViewIndex < 0 || selectedViewIndex >= views.length) return;
      views.splice(selectedViewIndex, 1);
      if (selectedViewIndex >= views.length) {
        selectedViewIndex = views.length - 1;
      }
      renderViewList();
      scheduleAutoSave();
    });

    viewListInput.addEventListener('change', () => {
      commitViewEditor();
      const parsed = Number(viewListInput.value);
      selectedViewIndex = Number.isFinite(parsed) ? parsed : -1;
      renderViewEditor();
    });

    viewTypeInput.addEventListener('change', () => {
      const nextType = String(viewTypeInput.value || 'timeseries');
      applyTypeHint(nextType);
      if (nextType === '3d' && String(viewScriptPathInput.value || '').trim().length === 0) {
        const currentView = selectedViewIndex >= 0 && selectedViewIndex < views.length ? views[selectedViewIndex] : null;
        const viewId = currentView ? String(currentView.id || makeUniqueViewId('3d_viewer')) : makeUniqueViewId('3d_viewer');
        viewScriptPathInput.value = defaultScriptPath(viewId);
      }
      commitViewEditor();
      renderViewList();
    });

    openViewScriptBtn.addEventListener('click', () => {
      commitViewEditor();
      if (selectedViewIndex < 0 || selectedViewIndex >= views.length) {
        return;
      }
      const view = views[selectedViewIndex];
      if (view.type !== '3d') {
        setStatus('Selected panel is not a 3D viewer.', 'error');
        return;
      }
      if (!view.scriptPath || String(view.scriptPath).trim().length === 0) {
        view.scriptPath = defaultScriptPath(String(view.id || makeUniqueViewId('3d_viewer')));
        viewScriptPathInput.value = view.scriptPath;
      }
      vscode.postMessage({ command: 'openViewScript', scriptPath: view.scriptPath, viewId: view.id });
    });

    viewTitleInput.addEventListener('blur', () => {
      commitViewEditor();
      renderViewList();
    });

    function addScatterSeriesFromPrompt() {
      const view = getActiveView();
      if (!view || view.type !== 'scatter') {
        return;
      }
      const nameRaw = window.prompt('Scatter series name:', 'Series ' + String((view.scatterSeries?.length ?? 0) + 1));
      if (nameRaw === null) return;
      const xRaw = window.prompt('Scatter series X expression:', 'x');
      if (xRaw === null) return;
      const yRaw = window.prompt('Scatter series Y expression:', 'y');
      if (yRaw === null) return;
      const x = String(xRaw || '').trim();
      const y = String(yRaw || '').trim();
      if (x.length === 0 || y.length === 0) {
        setStatus('Scatter series requires both X and Y expressions.', 'error');
        return;
      }
      const scatterSeries = ensureScatterSeries(view);
      const nextName = String(nameRaw || '').trim();
      scatterSeries.push({
        name: nextName.length > 0 ? nextName : (y + ' vs ' + x),
        x,
        y,
      });
      selectedScatterSeriesIndex = scatterSeries.length - 1;
      renderScatterSeriesEditor();
      commitViewEditor();
      renderViewList();
      scheduleAutoSave();
    }

    addScatterSeriesBtn.addEventListener('click', () => {
      addScatterSeriesFromPrompt();
    });
    removeScatterSeriesBtn.addEventListener('click', () => {
      const view = getActiveView();
      if (!view || view.type !== 'scatter') return;
      const scatterSeries = ensureScatterSeries(view);
      if (selectedScatterSeriesIndex < 0 || selectedScatterSeriesIndex >= scatterSeries.length) return;
      scatterSeries.splice(selectedScatterSeriesIndex, 1);
      if (selectedScatterSeriesIndex >= scatterSeries.length) {
        selectedScatterSeriesIndex = scatterSeries.length - 1;
      }
      renderScatterSeriesEditor();
      commitViewEditor();
      renderViewList();
      scheduleAutoSave();
    });
    viewScatterSeriesListInput.addEventListener('change', () => {
      commitScatterSeriesEditor();
      const parsed = Number(viewScatterSeriesListInput.value);
      selectedScatterSeriesIndex = Number.isFinite(parsed) ? parsed : -1;
      renderScatterSeriesEditor();
      commitViewEditor();
      renderViewList();
      scheduleAutoSave();
    });
    viewScatterSeriesNameInput.addEventListener('blur', () => {
      commitScatterSeriesEditor();
      renderScatterSeriesEditor();
      commitViewEditor();
      renderViewList();
      scheduleAutoSave();
    });
    viewScatterSeriesXInput.addEventListener('blur', () => {
      commitScatterSeriesEditor();
      renderScatterSeriesEditor();
      commitViewEditor();
      renderViewList();
      scheduleAutoSave();
    });
    viewScatterSeriesYInput.addEventListener('blur', () => {
      commitScatterSeriesEditor();
      renderScatterSeriesEditor();
      commitViewEditor();
      renderViewList();
      scheduleAutoSave();
    });

    document.getElementById('reset').addEventListener('click', () => {
      vscode.postMessage({ command: 'reset' });
    });
    document.getElementById('resyncSidecars').addEventListener('click', () => {
      vscode.postMessage({ command: 'resyncSidecars' });
    });

    document.getElementById('addLib').addEventListener('click', () => {
      vscode.postMessage({ command: 'pickLibraryPath' });
    });

    clearLibsBtn.addEventListener('click', () => {
      modelicaPathInput.value = '';
      scheduleAutoSave();
    });

    document.getElementById('workspaceSettings').addEventListener('click', () => {
      vscode.postMessage({ command: 'openWorkspaceSettings' });
    });

    document.getElementById('userSettings').addEventListener('click', () => {
      vscode.postMessage({ command: 'openUserSettings' });
    });

    window.addEventListener('message', event => {
      const msg = event.data;
      if (msg.command === 'pickedLibraryPath' && msg.path) {
        const lines = modelicaPathInput.value
          .split(/\\r?\\n/)
          .map(v => v.trim())
          .filter(Boolean);
        if (!lines.includes(msg.path)) {
          lines.push(msg.path);
          modelicaPathInput.value = lines.join('\\n');
          scheduleAutoSave();
        }
      } else if (msg.command === 'saved') {
        setStatus('Saved.', 'ok');
      } else if (msg.command === 'resetDone') {
        isResettingFromHost = true;
        if (autoSaveTimer) {
          clearTimeout(autoSaveTimer);
          autoSaveTimer = null;
        }
        solverInput.value = msg.solver;
        tEndInput.value = String(msg.tEnd);
        dtInput.value = msg.dt;
        modelicaPathInput.value = msg.modelicaPath.join('\\n');
        views = Array.isArray(msg.views) && msg.views.length > 0
          ? msg.views
          : [{ id: 'states_time', title: 'States vs Time', type: 'timeseries', x: 'time', y: ['*states'] }];
        selectedViewIndex = views.length > 0 ? 0 : -1;
        renderViewList();
        setStatus('Reset to global defaults.', 'ok');
        isResettingFromHost = false;
      } else if (msg.command === 'openedScriptPath' && msg.path) {
        setStatus('Opened script: ' + msg.path, 'ok');
      } else if (msg.command === 'resyncDone') {
        setStatus(msg.message || 'Resync completed.', 'ok');
      } else if (msg.command === 'error') {
        setStatus(msg.message || 'Failed to save settings.', 'error');
      }
    });
    renderViewList();
  </script>
</body>
</html>`;

    panel.webview.onDidReceiveMessage(async message => {
        try {
            switch (message.command) {
                case 'save': {
                    const tEnd = Number(message.tEnd);
                    if (!Number.isFinite(tEnd) || tEnd <= 0) {
                        panel.webview.postMessage({ command: 'error', message: 't_end must be a positive number.' });
                        return;
                    }
                    const solver = String(message.solver || 'auto').toLowerCase();
                    if (solver !== 'auto' && solver !== 'bdf' && solver !== 'rk-like') {
                        panel.webview.postMessage({ command: 'error', message: 'Invalid solver mode.' });
                        return;
                    }
                    const dtRaw = String(message.dt ?? '').trim();
                    const dt = dtRaw.length === 0 ? undefined : Number(dtRaw);
                    if (dt !== undefined && (!Number.isFinite(dt) || dt <= 0)) {
                        panel.webview.postMessage({ command: 'error', message: 'dt must be empty or a positive number.' });
                        return;
                    }
                    const nextPreset: ModelSimulationPreset = {
                        solver: solver as 'auto' | 'bdf' | 'rk-like',
                        tEnd,
                        dt,
                        outputDir: String(message.outputDir || ''),
                        libraryOverrides: Array.isArray(message.modelicaPath) ? message.modelicaPath : [],
                    };
                    const saved = await setProjectSimulationPreset(activeModel, workspaceRoot, nextPreset);
                    if (!saved) {
                        panel.webview.postMessage({
                            command: 'error',
                            message: 'Failed to save preset via LSP project config endpoint.',
                        });
                        return;
                    }
                    const nextViews = normalizeVisualizationViews(message.views);
                    const persistedViews = await persistVisualizationScripts(
                        nextViews,
                        workspaceRoot,
                        activeModel,
                    );
                    const viewSaved = await setProjectVisualizationConfig(
                        activeModel,
                        workspaceRoot,
                        persistedViews,
                    );
                    if (!viewSaved) {
                        panel.webview.postMessage({
                            command: 'error',
                            message: 'Failed to save results-panel configuration via LSP project config endpoint.',
                        });
                        return;
                    }
                    await resyncProjectSidecars(workspaceRoot, {
                        reason: 'save-settings',
                        pruneOrphans: false,
                    });
                    panel.webview.postMessage({ command: 'saved' });
                    return;
                }
                case 'reset': {
                    const reset = await resetProjectSimulationPreset(activeModel, workspaceRoot);
                    if (!reset) {
                        panel.webview.postMessage({
                            command: 'error',
                            message: 'Failed to reset preset via LSP project config endpoint.',
                        });
                        return;
                    }
                    const resetViews = await setProjectVisualizationConfig(activeModel, workspaceRoot, []);
                    if (!resetViews) {
                        panel.webview.postMessage({
                            command: 'error',
                            message: 'Failed to reset results-panel configuration via LSP project config endpoint.',
                        });
                        return;
                    }
                    const refreshed =
                        (await getProjectSimulationConfig(activeModel, workspaceRoot, fallbackDefaults)) ??
                        undefined;
                    const resetDefaults = refreshed?.defaults ?? defaults;
                    const refreshedViews = await getProjectVisualizationConfig(activeModel, workspaceRoot);
                    panel.webview.postMessage({
                        command: 'resetDone',
                        solver: resetDefaults.solver,
                        tEnd: resetDefaults.tEnd,
                        dt: resetDefaults.dt ? String(resetDefaults.dt) : '',
                        modelicaPath: resetDefaults.modelicaPath,
                        views: refreshedViews.length > 0 ? refreshedViews : defaultVisualizationViews(),
                    });
                    return;
                }
                case 'pickLibraryPath': {
                    const picked = await vscode.window.showOpenDialog({
                        canSelectFiles: false,
                        canSelectFolders: true,
                        canSelectMany: false,
                        openLabel: 'Select Library Folder',
                    });
                    if (picked && picked.length > 0) {
                        panel.webview.postMessage({ command: 'pickedLibraryPath', path: picked[0].fsPath });
                    }
                    return;
                }
                case 'resyncSidecars': {
                    const report = await resyncProjectSidecars(workspaceRoot, {
                        reason: 'settings-button',
                        pruneOrphans: false,
                    });
                    if (!report) {
                        panel.webview.postMessage({
                            command: 'error',
                            message: 'Failed to resync sidecars via LSP endpoint.',
                        });
                        return;
                    }
                    panel.webview.postMessage({
                        command: 'resyncDone',
                        message: `Resync complete: remapped=${report.remapped_models}, parseFailures=${report.parse_failures}`,
                    });
                    return;
                }
                case 'openWorkspaceSettings':
                    await vscode.commands.executeCommand('workbench.action.openWorkspaceSettings', 'rumoca');
                    return;
                case 'openUserSettings':
                    await vscode.commands.executeCommand('workbench.action.openSettings', 'rumoca');
                    return;
                case 'openViewScript': {
                    if (!workspaceRoot) {
                        panel.webview.postMessage({
                            command: 'error',
                            message: 'Cannot open 3D script file without a workspace root.',
                        });
                        return;
                    }
                    const requestedPath = String(message.scriptPath || '').trim();
                    const viewId = String(message.viewId || 'viewer').trim();
                    const scriptPath =
                        requestedPath.length > 0
                            ? requestedPath
                            : defaultViewerScriptPath(activeModel, viewId);
                    const absolutePath = path.isAbsolute(scriptPath)
                        ? scriptPath
                        : path.join(workspaceRoot, scriptPath);
                    await fs.promises.mkdir(path.dirname(absolutePath), { recursive: true });
                    try {
                        await fs.promises.access(absolutePath, fs.constants.F_OK);
                    } catch {
                        await fs.promises.writeFile(
                            absolutePath,
                            defaultBouncingBallViewerScript(),
                            'utf-8',
                        );
                    }
                    const document = await vscode.workspace.openTextDocument(absolutePath);
                    await vscode.window.showTextDocument(document, { preview: false });
                    panel.webview.postMessage({
                        command: 'openedScriptPath',
                        path: path.isAbsolute(scriptPath)
                            ? scriptPath
                            : scriptPath.split(path.sep).join('/'),
                    });
                    return;
                }
                default:
                    return;
            }
        } catch (error) {
            panel.webview.postMessage({ command: 'error', message: `${error}` });
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

            // Add library paths (cell-specific + global config)
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
    rumocaPath: string,
    globalLibPaths: string[],
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
                const result = await executeModelicaCell(code, rumocaPath, globalLibPaths);

                if (result.success) {
                    const modelName = result.model || 'model';
                    const outputFile = result.outputFile;

                    // Generate Python code that uses rumoca to compile the model
                    let pythonCode: string;
                    if (outputFile) {
                        // If output file specified, compile from file
                        pythonCode = `import rumoca

# Compile from saved .mo file
${modelName} = rumoca.compile("${outputFile}")

# Access the model data
print(${modelName})  # Display model summary

# Get as Python dict for further processing
# model_dict = ${modelName}.to_base_modelica_dict()`;
                    } else {
                        // Inline the Modelica code in the Python call (requires native bindings)
                        const escapedModelica = code.replace(/\\/g, '\\\\').replace(/"""/g, '\\"""');
                        pythonCode = `import rumoca

# Compile from inline Modelica source (requires native bindings)
${modelName} = rumoca.compile_source("""
${escapedModelica}
""", "${modelName}")

# Access the model data
print(${modelName})  # Display model summary

# Get as Python dict for further processing
# model_dict = ${modelName}.to_base_modelica_dict()`;
                    }

                    // Output the Python code that can be copied to a Python cell
                    // Also show a summary of the compilation
                    const summaryText = outputFile
                        ? `✓ Model "${modelName}" compiled and saved to: ${outputFile}\n\nCopy the Python code below to a Python cell to use the model:`
                        : `✓ Model "${modelName}" compiled successfully.\n\nCopy the Python code below to a Python cell to use the model:`;

                    execution.replaceOutput([
                        new vscode.NotebookCellOutput([
                            vscode.NotebookCellOutputItem.text(summaryText, 'text/plain')
                        ]),
                        new vscode.NotebookCellOutput([
                            vscode.NotebookCellOutputItem.text(pythonCode, 'text/x-python')
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

    const config = vscode.workspace.getConfiguration('rumoca');
    const debug = config.get<boolean>('debug') ?? false;
    const useSystemServer = config.get<boolean>('useSystemServer') ?? false;

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

    if (debug) {
        outputChannel.show(true); // Show output channel immediately when debugging
    }

    log('Activating Rumoca Modelica extension...');
    console.log('[Rumoca] Debug mode:', debug);
    debugLog(`[DEBUG] Workspace folders: ${vscode.workspace.workspaceFolders?.map(f => f.uri.fsPath).join(', ') || 'none'}`);

    // Find the server executable
    let serverPath = config.get<string>('serverPath');
    let usingSystemFallback = false;
    let usingBundledServer = false;

    const elapsed = () => `${Date.now() - startTime}ms`;

    // Helper to find system-installed rumoca-lsp
    const findSystemServer = (): string | undefined => {
        // Try PATH first
        const pathResult = findInPath('rumoca-lsp');
        if (pathResult) {
            debugLog(`[${elapsed()}] Found rumoca-lsp in PATH: ${pathResult}`);
            return pathResult;
        }
        // Try cargo installation location
        const cargoPath = path.join(process.env.HOME || '', '.cargo', 'bin', 'rumoca-lsp');
        if (fs.existsSync(cargoPath)) {
            debugLog(`[${elapsed()}] Found rumoca-lsp at cargo location: ${cargoPath}`);
            return cargoPath;
        }
        return undefined;
    };

    if (serverPath) {
        // Explicit path configured - use it directly
        debugLog(`[${elapsed()}] Using configured serverPath: ${serverPath}`);
    } else if (useSystemServer) {
        // User explicitly wants system server
        debugLog(`[${elapsed()}] useSystemServer is enabled, searching for system rumoca-lsp...`);
        serverPath = findSystemServer();
        if (serverPath) {
            log(`Using system-installed rumoca-lsp: ${serverPath}`);
        }
    } else {
        debugLog(`[${elapsed()}] Searching for rumoca-lsp...`);

        // 1. Check for bundled binary (platform-specific extension)
        const binaryName = process.platform === 'win32' ? 'rumoca-lsp.exe' : 'rumoca-lsp';
        const bundledPath = path.join(context.extensionPath, 'bin', binaryName);
        debugLog(`[${elapsed()}] Checking for bundled binary: ${bundledPath}`);
        if (fs.existsSync(bundledPath)) {
            serverPath = bundledPath;
            usingBundledServer = true;
            log(`Using bundled rumoca-lsp`);
            debugLog(`[${elapsed()}] Found bundled rumoca-lsp: ${serverPath}`);
        } else {
            // 2. Fall back to system-installed version
            debugLog(`[${elapsed()}] No bundled binary, searching for system rumoca-lsp...`);
            serverPath = findSystemServer();
            if (serverPath) {
                usingSystemFallback = true;
            }
        }
    }

    if (!serverPath) {
        const installAction = 'Install with cargo';
        const msg = 'rumoca-lsp not found. Install it with: cargo install rumoca';
        log(`ERROR: ${msg}`);

        const selection = await vscode.window.showErrorMessage(msg, installAction, 'Configure Path');
        if (selection === installAction) {
            // Open terminal with install command
            const terminal = vscode.window.createTerminal('Rumoca Install');
            terminal.show();
            terminal.sendText('cargo install rumoca');
        } else if (selection === 'Configure Path') {
            vscode.commands.executeCommand('workbench.action.openSettings', 'rumoca.serverPath');
        }
        return;
    }

    // Show warning if using system fallback (bundled binary not found)
    if (usingSystemFallback) {
        log(`Warning: Using system-installed rumoca-lsp: ${serverPath}`);
        log('The bundled binary was not found. This may indicate a platform mismatch.');
        vscode.window.showWarningMessage(
            `Using system-installed rumoca-lsp. Set "rumoca.useSystemServer": true to suppress this warning.`,
            'Open Settings'
        ).then(selection => {
            if (selection === 'Open Settings') {
                vscode.commands.executeCommand('workbench.action.openSettings', 'rumoca.useSystemServer');
            }
        });
    }

    // Verify the binary exists and is executable
    debugLog(`[${elapsed()}] Verifying server binary exists...`);
    if (!fs.existsSync(serverPath)) {
        const msg = `rumoca-lsp not found at: ${serverPath}`;
        log(`ERROR: ${msg}`);
        vscode.window.showErrorMessage(msg);
        return;
    }

    let probeResult = probeServerExecutable(serverPath);
    if (!probeResult.ok) {
        const detail = probeResult.detail ?? 'unknown error';
        log(`ERROR: Failed to execute rumoca-lsp at ${serverPath}: ${detail}`);

        if (usingBundledServer) {
            const fallbackServerPath = findSystemServer();
            if (fallbackServerPath && fallbackServerPath !== serverPath) {
                const fallbackProbeResult = probeServerExecutable(fallbackServerPath);
                if (fallbackProbeResult.ok) {
                    serverPath = fallbackServerPath;
                    probeResult = fallbackProbeResult;
                    log(`Warning: Bundled rumoca-lsp could not execute; falling back to system server: ${serverPath}`);
                    vscode.window.showWarningMessage(
                        'Bundled rumoca-lsp could not execute on this machine. Falling back to the system-installed server.',
                        'Open Settings'
                    ).then(selection => {
                        if (selection === 'Open Settings') {
                            vscode.commands.executeCommand('workbench.action.openSettings', 'rumoca.useSystemServer');
                        }
                    });
                } else {
                    const fallbackDetail = fallbackProbeResult.detail ?? 'unknown error';
                    log(`ERROR: System rumoca-lsp fallback also failed at ${fallbackServerPath}: ${fallbackDetail}`);
                }
            }
        }
    }

    if (!serverPath || !probeResult.ok) {
        const probeDetail = probeResult.detail ?? 'unknown error';
        const msg = `Failed to execute rumoca-lsp: ${probeDetail}`;
        log(`ERROR: ${msg}`);
        outputChannel.show();
        const selection = await vscode.window.showErrorMessage(msg, 'Open Settings', 'Configure Path');
        if (selection === 'Open Settings') {
            vscode.commands.executeCommand('workbench.action.openSettings', 'rumoca.useSystemServer');
        } else if (selection === 'Configure Path') {
            vscode.commands.executeCommand('workbench.action.openSettings', 'rumoca.serverPath');
        }
        return;
    }

    debugLog(`[${elapsed()}] Starting language server: ${serverPath}`);

    const serverOptions: ServerOptions = {
        run: {
            command: serverPath,
            transport: TransportKind.stdio
        },
        debug: {
            command: serverPath,
            transport: TransportKind.stdio
        }
    };

    // Get library paths from configuration + environment.
    const modelicaPathSources = resolveModelicaPathSources(config);
    const modelicaPath = modelicaPathSources.mergedPaths;
    if (modelicaPathSources.configuredPaths.length > 0) {
        debugLog(`[${elapsed()}] Configured modelicaPath: ${modelicaPathSources.configuredPaths.join(', ')}`);
    }
    if (modelicaPathSources.environmentPaths.length > 0) {
        debugLog(`[${elapsed()}] Environment MODELICAPATH: ${modelicaPathSources.environmentPaths.join(', ')}`);
    }
    if (modelicaPathSources.usedLegacyAlias) {
        log('Warning: MODELICPATH is deprecated; use MODELICAPATH instead.');
    }

    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'modelica' },
            // Support Modelica cells in Jupyter notebooks
            { scheme: 'vscode-notebook-cell', language: 'modelica' },
            // Support embedded Modelica in %%modelica blocks
            { scheme: EMBEDDED_MODELICA_SCHEME, language: 'modelica' }
        ],
        outputChannelName: 'Rumoca LSP',
        initializationOptions: {
            debug: debug,
            modelicaPath: modelicaPath
        }
    };

    debugLog(`[${elapsed()}] Creating LanguageClient...`);
    client = new LanguageClient(
        'rumoca',
        'Rumoca LSP',
        serverOptions,
        clientOptions
    );
    debugLog(`[${elapsed()}] LanguageClient created`);

    // Start the client. This will also launch the server
    try {
        debugLog(`[${elapsed()}] Calling client.start() - this launches the server and waits for initialization...`);
        debugLog(`[${elapsed()}] If stuck here, the language server may be scanning workspace files...`);
        await client.start();
        debugLog(`[${elapsed()}] Language server started successfully`);
    } catch (error) {
        const msg = `Failed to start language server: ${error}`;
        log(`ERROR: ${msg}`);
        outputChannel.show();
        vscode.window.showErrorMessage(msg);
        return;
    }

    const wireResultsPanelMessageHandling = (panel: vscode.WebviewPanel) => {
        const messageDisposable = panel.webview.onDidReceiveMessage(async (message) => {
            if (!message || typeof message !== 'object') {
                return;
            }
            const command = String((message as { command?: unknown }).command ?? '');
            if (command === 'savePlotPngError') {
                const detail = String((message as { message?: unknown }).message ?? 'Unknown PNG export error.');
                vscode.window.showErrorMessage(`Save PNG failed: ${detail}`);
                return;
            }
            if (command === 'saveMovieWebmError') {
                const detail = String((message as { message?: unknown }).message ?? 'Unknown movie export error.');
                vscode.window.showErrorMessage(`Export movie failed: ${detail}`);
                return;
            }
            if (command !== 'savePlotPng' && command !== 'saveMovieWebm') {
                return;
            }
            const dataUrl = String((message as { dataUrl?: unknown }).dataUrl ?? '');
            const workspaceDir = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
            if (command === 'savePlotPng') {
                const defaultNameRaw = String((message as { defaultName?: unknown }).defaultName ?? 'rumoca_plot.png');
                const defaultName = defaultNameRaw
                    .replace(/[^a-zA-Z0-9._-]+/g, '_')
                    .replace(/^_+|_+$/g, '') || 'rumoca_plot.png';
                const match = dataUrl.match(/^data:image\/png;base64,(.+)$/);
                if (!match) {
                    vscode.window.showErrorMessage('Save PNG failed: invalid PNG payload from webview.');
                    return;
                }
                const defaultUri = workspaceDir
                    ? vscode.Uri.file(path.join(workspaceDir, defaultName.endsWith('.png') ? defaultName : `${defaultName}.png`))
                    : undefined;
                const targetUri = await vscode.window.showSaveDialog({
                    saveLabel: 'Save Plot PNG',
                    defaultUri,
                    filters: {
                        'PNG Image': ['png'],
                    },
                });
                if (!targetUri) {
                    return;
                }
                const bytes = Buffer.from(match[1], 'base64');
                await vscode.workspace.fs.writeFile(targetUri, new Uint8Array(bytes));
                log(`Saved plot PNG: ${targetUri.fsPath}`);
                return;
            }
            const defaultNameRaw = String((message as { defaultName?: unknown }).defaultName ?? 'rumoca_viewer.webm');
            const defaultName = defaultNameRaw
                .replace(/[^a-zA-Z0-9._-]+/g, '_')
                .replace(/^_+|_+$/g, '') || 'rumoca_viewer.webm';
            const match = dataUrl.match(/^data:video\/webm[^,]*;base64,(.+)$/);
            if (!match) {
                vscode.window.showErrorMessage('Export movie failed: invalid WebM payload from webview.');
                return;
            }
            const defaultUri = workspaceDir
                ? vscode.Uri.file(path.join(workspaceDir, defaultName.endsWith('.webm') ? defaultName : `${defaultName}.webm`))
                : undefined;
            const targetUri = await vscode.window.showSaveDialog({
                saveLabel: 'Export Movie',
                defaultUri,
                filters: {
                    'WebM Video': ['webm'],
                },
            });
            if (!targetUri) {
                return;
            }
            const bytes = Buffer.from(match[1], 'base64');
            await vscode.workspace.fs.writeFile(targetUri, new Uint8Array(bytes));
            log(`Saved viewer movie: ${targetUri.fsPath}`);
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
        const panel = vscode.window.createWebviewPanel(
            'rumocaResults',
            title,
            column,
            resultsWebviewOptions()
        );
        wireResultsPanelMessageHandling(panel);
        const state: ResultsPanelState | undefined =
            runId && model.trim().length > 0
                ? {
                    version: 1,
                    runId,
                    model,
                    workspaceRoot,
                    title,
                }
                : undefined;
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
                const stateObj = (state && typeof state === 'object') ? (state as Partial<ResultsPanelState>) : {};
                const runId = typeof stateObj.runId === 'string' ? stateObj.runId.trim() : '';
                const model = typeof stateObj.model === 'string' ? stateObj.model.trim() : '';
                const workspaceRoot =
                    typeof stateObj.workspaceRoot === 'string' && stateObj.workspaceRoot.trim().length > 0
                        ? stateObj.workspaceRoot.trim()
                        : resolveWorkspaceRootFallback();
                if (!runId || !model) {
                    webviewPanel.title = 'Rumoca Results (Unavailable)';
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
                const persisted = await loadPersistedSimulationRunFromDisk(workspaceRoot, runId);
                if (!persisted || !persisted.payload) {
                    webviewPanel.title = `Rumoca Results: ${model} (Missing Run)`;
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
                const persistedViews = persisted.views.length > 0 ? persisted.views : defaultVisualizationViews();
                const hydratedViews = await hydrateVisualizationViewsFromDisk(persistedViews, workspaceRoot, model);
                const restoredTitle =
                    typeof stateObj.title === 'string' && stateObj.title.trim().length > 0
                        ? stateObj.title.trim()
                        : `Rumoca Results: ${model}`;
                webviewPanel.title = restoredTitle;
                const nextState: ResultsPanelState = {
                    version: 1,
                    runId,
                    model,
                    workspaceRoot,
                    title: restoredTitle,
                };
                webviewPanel.webview.html = buildResultsWebviewHtml(
                    model,
                    persisted.payload,
                    hydratedViews,
                    persisted.metrics,
                    nextState,
                    getResultsWebviewAssets(webviewPanel.webview),
                );
            },
        }),
    );

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
        const source = document.getText();
        const model = defaults.model || inferModelNameFromSource(source);
        if (!model) {
            vscode.window.showErrorMessage(
                'Could not infer a model name from the active file. Set "Model Override" in simulation settings (gear button).'
            );
            return;
        }
        const workspaceRoot = resolveWorkspaceRootForDocument(document) ?? resolveWorkspaceRootFallback();
        await resyncProjectSidecars(workspaceRoot, {
            reason: 'simulate',
            pruneOrphans: false,
        });
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

            const configuredViews = await getProjectVisualizationConfig(model, workspaceRoot);
            const baseViews = configuredViews.length > 0 ? configuredViews : defaultVisualizationViews();
            const views = await hydrateVisualizationViewsFromDisk(baseViews, workspaceRoot, model);
            const runId = await saveSimulationRunToDisk(
                model,
                workspaceRoot,
                result.payload,
                result.metrics,
                views,
            );

            const timestamp = new Date().toLocaleTimeString([], { hour12: false });
            await openResultsPanelForRun(
                model,
                result.payload,
                views,
                result.metrics,
                workspaceRoot,
                runId,
                `Rumoca Results: ${model} (${timestamp})`,
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
            const editor = vscode.window.activeTextEditor;
            if (!editor || editor.document.languageId !== 'modelica') {
                vscode.window.showErrorMessage('Open a Modelica file to configure per-model simulation settings.');
                return;
            }
            const defaults = getSimulationSettings(vscode.workspace.getConfiguration('rumoca'));
            const model = defaults.model || inferModelNameFromSource(editor.document.getText());
            if (!model) {
                vscode.window.showErrorMessage('Could not infer a model name from the active file.');
                return;
            }
            const workspaceRoot = resolveWorkspaceRootForDocument(editor.document) ?? resolveWorkspaceRootFallback();
            await showSimulationSettingsPanel(context, model, workspaceRoot);
        },
    );
    context.subscriptions.push(simulationSettingsCommand);

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

    // Create notebook controller for Modelica cells in Jupyter notebooks
    // This allows executing Modelica code and getting JSON output for Python interop
    const rumocaExecutable = serverPath.replace('-lsp', '');
    if (fs.existsSync(rumocaExecutable)) {
        createNotebookController(context, rumocaExecutable, modelicaPath, log);
        debugLog(`[${elapsed()}] Notebook controller created using: ${rumocaExecutable}`);
    } else {
        debugLog(`[${elapsed()}] Skipping notebook controller - rumoca executable not found at: ${rumocaExecutable}`);
    }

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
