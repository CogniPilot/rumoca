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

function getBundledServerPath(extensionPath: string): string | undefined {
    const platform = process.platform;
    const arch = process.arch;

    let binaryName: string;
    if (platform === 'win32') {
        binaryName = 'rumoca-lsp-win32-x64.exe';
    } else if (platform === 'darwin') {
        binaryName = arch === 'arm64' ? 'rumoca-lsp-darwin-arm64.bin' : 'rumoca-lsp-darwin-x64.bin';
    } else if (platform === 'linux') {
        binaryName = arch === 'arm64' ? 'rumoca-lsp-linux-arm64.bin' : 'rumoca-lsp-linux-x64.bin';
    } else {
        return undefined;
    }

    const bundledPath = path.join(extensionPath, 'bin', binaryName);
    if (fs.existsSync(bundledPath)) {
        return bundledPath;
    }
    return undefined;
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

export async function activate(context: vscode.ExtensionContext) {
    outputChannel = vscode.window.createOutputChannel('Rumoca Modelica');
    outputChannel.appendLine('Activating Rumoca Modelica extension...');

    const config = vscode.workspace.getConfiguration('rumoca');

    // Find the server executable
    let serverPath = config.get<string>('serverPath');

    if (serverPath) {
        outputChannel.appendLine(`Using configured serverPath: ${serverPath}`);
    } else {
        outputChannel.appendLine('No serverPath configured, searching for rumoca-lsp...');

        // First, try bundled binary
        const bundledPath = getBundledServerPath(context.extensionPath);
        if (bundledPath) {
            serverPath = bundledPath;
            outputChannel.appendLine(`Found bundled binary: ${serverPath}`);
        } else {
            // Try to find rumoca-lsp in PATH
            const pathResult = findInPath('rumoca-lsp');
            if (pathResult) {
                serverPath = pathResult;
                outputChannel.appendLine(`Found rumoca-lsp in PATH: ${serverPath}`);
            } else {
                // Fall back to common locations
                const possiblePaths = [
                    path.join(context.extensionPath, '..', '..', '..', 'target', 'release', 'rumoca-lsp'),
                    path.join(context.extensionPath, '..', '..', '..', 'target', 'debug', 'rumoca-lsp'),
                    path.join(process.env.HOME || '', '.cargo', 'bin', 'rumoca-lsp'),
                ];

                for (const p of possiblePaths) {
                    if (fs.existsSync(p)) {
                        serverPath = p;
                        outputChannel.appendLine(`Found rumoca-lsp at: ${serverPath}`);
                        break;
                    }
                }
            }
        }
    }

    if (!serverPath) {
        const msg = 'Could not find rumoca-lsp executable. Please install it with "cargo install rumoca" or set rumoca.serverPath in settings.';
        outputChannel.appendLine(`ERROR: ${msg}`);
        vscode.window.showErrorMessage(msg);
        return;
    }

    // Verify the binary exists and is executable
    if (!fs.existsSync(serverPath)) {
        const msg = `rumoca-lsp not found at: ${serverPath}`;
        outputChannel.appendLine(`ERROR: ${msg}`);
        vscode.window.showErrorMessage(msg);
        return;
    }

    outputChannel.appendLine(`Starting language server: ${serverPath}`);

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

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'modelica' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.mo')
        },
        outputChannelName: 'Rumoca Modelica'
    };

    client = new LanguageClient(
        'rumoca',
        'Rumoca Modelica',
        serverOptions,
        clientOptions
    );

    // Start the client. This will also launch the server
    try {
        await client.start();
        outputChannel.appendLine('Language server started successfully');
    } catch (error) {
        const msg = `Failed to start language server: ${error}`;
        outputChannel.appendLine(`ERROR: ${msg}`);
        outputChannel.show();
        vscode.window.showErrorMessage(msg);
        return;
    }

    outputChannel.appendLine('Rumoca Modelica extension activated');
}

export async function deactivate(): Promise<void> {
    if (client) {
        await client.stop();
    }
}
