import * as path from 'path';
import * as fs from 'fs';
import * as vscode from 'vscode';
import { execSync } from 'child_process';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;
let outputChannel: vscode.OutputChannel;

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

export async function activate(context: vscode.ExtensionContext) {
    const startTime = Date.now();
    outputChannel = vscode.window.createOutputChannel('Rumoca Modelica');

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
            { scheme: 'vscode-notebook-cell', language: 'modelica' }
        ],
        outputChannelName: 'Rumoca Modelica',
        initializationOptions: {
            debug: debug,
            modelicaPath: modelicaPath
        }
    };

    debugLog(`[${elapsed()}] Creating LanguageClient...`);
    client = new LanguageClient(
        'rumoca',
        'Rumoca Modelica',
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

    log('Rumoca Modelica extension activated');
}

export async function deactivate(): Promise<void> {
    if (client) {
        await client.stop();
    }
}
