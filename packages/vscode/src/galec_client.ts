// GALEC (.alg) language client. Mirrors the Modelica language-server wiring in
// extension.ts (bundled binary -> PATH/cargo fallback, `--version` probe, stdio
// LanguageClient) but stays self-contained and much simpler: the GALEC server
// needs no source-root paths, notebooks, or simulation plumbing.
import * as path from 'path';
import * as fs from 'fs';
import { execSync, spawnSync } from 'child_process';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind,
} from 'vscode-languageclient/node';

const SERVER_BINARY = 'rumoca-galec-lsp';

function serverExeName(): string {
    return process.platform === 'win32' ? `${SERVER_BINARY}.exe` : SERVER_BINARY;
}

function findInPath(command: string): string | undefined {
    try {
        const result = execSync(
            process.platform === 'win32' ? `where ${command}` : `which ${command}`,
            { encoding: 'utf-8' }
        );
        const firstLine = result.split('\n')[0]?.trim();
        if (firstLine && fs.existsSync(firstLine)) {
            return firstLine;
        }
    } catch {
        // Not on PATH.
    }
    return undefined;
}

function findSystemServer(): string | undefined {
    const inPath = findInPath(SERVER_BINARY);
    if (inPath) {
        return inPath;
    }
    const cargoPath = path.join(process.env.HOME || '', '.cargo', 'bin', serverExeName());
    if (fs.existsSync(cargoPath)) {
        return cargoPath;
    }
    return undefined;
}

// Resolution order mirrors the Modelica server: an explicit setting, then the
// system server when requested, then the bundled binary, then a system fallback.
function resolveServerPath(
    context: vscode.ExtensionContext,
    config: vscode.WorkspaceConfiguration
): string | undefined {
    const configured = config.get<string>('galecServerPath');
    if (configured) {
        return configured;
    }
    if (config.get<boolean>('useSystemServer') ?? false) {
        return findSystemServer();
    }
    const bundled = path.join(context.extensionPath, 'bin', serverExeName());
    if (fs.existsSync(bundled)) {
        return bundled;
    }
    return findSystemServer();
}

// Verify the binary can execute (`--version` exits 0) before launching it, so a
// platform-mismatched bundle degrades gracefully instead of hanging the server.
function serverExecutes(serverPath: string): boolean {
    const result = spawnSync(serverPath, ['--version'], {
        encoding: 'utf-8',
        timeout: 5000,
        windowsHide: true,
    });
    return !result.error && result.status === 0;
}

/**
 * Build the GALEC `.alg` language client, or return `undefined` (having logged
 * why) when no usable `rumoca-galec-lsp` is available — GALEC features are then
 * simply absent, without disturbing the Modelica server.
 */
export function createGalecLanguageClient(
    context: vscode.ExtensionContext,
    log: (msg: string) => void
): LanguageClient | undefined {
    const config = vscode.workspace.getConfiguration('rumoca');
    const serverPath = resolveServerPath(context, config);
    if (!serverPath || !fs.existsSync(serverPath)) {
        log(
            'rumoca-galec-lsp not found; GALEC (.alg) language features are disabled. ' +
                'Install it with `cargo install rumoca` or set "rumoca.galecServerPath".'
        );
        return undefined;
    }
    if (!serverExecutes(serverPath)) {
        log(
            `rumoca-galec-lsp at ${serverPath} could not execute (--version probe failed); ` +
                'GALEC (.alg) language features are disabled.'
        );
        return undefined;
    }

    const serverOptions: ServerOptions = {
        run: { command: serverPath, args: [], transport: TransportKind.stdio },
        debug: { command: serverPath, args: [], transport: TransportKind.stdio },
    };
    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'galec' }],
        outputChannelName: 'Rumoca GALEC LSP',
    };
    log(`Starting GALEC language server: ${serverPath}`);
    return new LanguageClient('rumoca-galec', 'Rumoca GALEC LSP', serverOptions, clientOptions);
}
