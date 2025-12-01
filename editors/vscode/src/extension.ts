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

export async function activate(_context: vscode.ExtensionContext) {
    outputChannel = vscode.window.createOutputChannel('Rumoca Modelica');
    outputChannel.appendLine('Activating Rumoca Modelica extension...');

    const config = vscode.workspace.getConfiguration('rumoca');

    // Find the server executable
    let serverPath = config.get<string>('serverPath');

    if (serverPath) {
        outputChannel.appendLine(`Using configured serverPath: ${serverPath}`);
    } else {
        outputChannel.appendLine('No serverPath configured, searching for rumoca-lsp...');

        // Try to find rumoca-lsp in PATH
        const pathResult = findInPath('rumoca-lsp');
        if (pathResult) {
            serverPath = pathResult;
            outputChannel.appendLine(`Found rumoca-lsp in PATH: ${serverPath}`);
        } else {
            // Try common cargo installation location
            const cargoPath = path.join(process.env.HOME || '', '.cargo', 'bin', 'rumoca-lsp');
            if (fs.existsSync(cargoPath)) {
                serverPath = cargoPath;
                outputChannel.appendLine(`Found rumoca-lsp at: ${serverPath}`);
            }
        }
    }

    if (!serverPath) {
        const installAction = 'Install with cargo';
        const msg = 'rumoca-lsp not found. Install it with: cargo install rumoca --features lsp';
        outputChannel.appendLine(`ERROR: ${msg}`);

        const selection = await vscode.window.showErrorMessage(msg, installAction, 'Configure Path');
        if (selection === installAction) {
            // Open terminal with install command
            const terminal = vscode.window.createTerminal('Rumoca Install');
            terminal.show();
            terminal.sendText('cargo install rumoca --features lsp');
        } else if (selection === 'Configure Path') {
            vscode.commands.executeCommand('workbench.action.openSettings', 'rumoca.serverPath');
        }
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
