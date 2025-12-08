import * as path from 'path';
import * as fs from 'fs';
import * as vscode from 'vscode';
import { execSync, spawn } from 'child_process';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let outputChannel: vscode.OutputChannel;
let notebookController: vscode.NotebookController | undefined;

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

/**
 * Parse magic directives from the first line(s) of a Modelica cell.
 *
 * Supported formats:
 *   // @rumoca model=ModelName output=/path/to/output.json lib=/path/to/lib1 lib=/path/to/lib2
 *   // @rumoca --model ModelName --output /path/to/output.json --lib /path/to/lib1 --lib /path/to/lib2
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
        const tmpDir = require('os').tmpdir();
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

    // Get library paths configuration
    const modelicaPath = config.get<string[]>('modelicaPath') ?? [];
    if (modelicaPath.length > 0) {
        debugLog(`[${elapsed()}] Configured modelicaPath: ${modelicaPath.join(', ')}`);
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

    // Create notebook controller for Modelica cells in Jupyter notebooks
    // This allows executing Modelica code and getting JSON output for Python interop
    const rumocaExecutable = serverPath.replace('-lsp', '');
    if (fs.existsSync(rumocaExecutable)) {
        notebookController = createNotebookController(context, rumocaExecutable, modelicaPath, log);
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
