import * as path from 'path';
import * as fs from 'fs';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

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

export async function activate(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('rumoca');

    // Find the server executable
    let serverPath = config.get<string>('serverPath');

    if (!serverPath) {
        // First, try bundled binary
        const bundledPath = getBundledServerPath(context.extensionPath);
        if (bundledPath) {
            serverPath = bundledPath;
        } else {
            // Fall back to searching in PATH and common locations
            const possiblePaths = [
                'rumoca-lsp',  // In PATH
                path.join(context.extensionPath, '..', '..', '..', 'target', 'release', 'rumoca-lsp'),
                path.join(context.extensionPath, '..', '..', '..', 'target', 'debug', 'rumoca-lsp'),
            ];

            for (const p of possiblePaths) {
                serverPath = p;
                break;
            }
        }
    }

    if (!serverPath) {
        vscode.window.showErrorMessage(
            'Could not find rumoca-lsp executable. Please set rumoca.serverPath in settings.'
        );
        return;
    }

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
    await client.start();

    console.log('Rumoca Modelica extension activated');
}

export async function deactivate(): Promise<void> {
    if (client) {
        await client.stop();
    }
}
