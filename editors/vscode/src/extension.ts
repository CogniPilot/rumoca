import * as path from 'path';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export async function activate(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('rumoca');

    // Find the server executable
    let serverPath = config.get<string>('serverPath');

    if (!serverPath) {
        // Try to find rumoca-lsp in common locations
        const possiblePaths = [
            'rumoca-lsp',  // In PATH
            path.join(context.extensionPath, '..', '..', '..', 'target', 'release', 'rumoca-lsp'),
            path.join(context.extensionPath, '..', '..', '..', 'target', 'debug', 'rumoca-lsp'),
        ];

        for (const p of possiblePaths) {
            try {
                // Just use the first one that might exist
                serverPath = p;
                break;
            } catch {
                continue;
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
