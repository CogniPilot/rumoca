import * as os from 'os';
import * as path from 'path';

export interface ModelicaPathSources {
    configuredPaths: string[];
    environmentPaths: string[];
    mergedPaths: string[];
    usedLegacyAlias: boolean;
}

export function mergeLibraryPathLists(primary: readonly string[], secondary: readonly string[]): string[] {
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

export function expandHomeDirectory(entry: string, homeDir: string = os.homedir()): string {
    const trimmed = String(entry).trim();
    if (!trimmed) {
        return '';
    }
    if (trimmed === '~') {
        return homeDir;
    }
    if (trimmed.startsWith('~/') || trimmed.startsWith('~\\')) {
        return path.join(homeDir, trimmed.slice(2));
    }
    return trimmed;
}

export function normalizeLibraryPathEntry(
    entry: string,
    homeDir: string = os.homedir()
): string {
    const expanded = expandHomeDirectory(entry, homeDir);
    return expanded ? path.normalize(expanded) : '';
}

export function parsePathListEnvVar(
    raw: string | undefined,
    homeDir: string = os.homedir()
): string[] {
    if (!raw) {
        return [];
    }
    return raw
        .split(path.delimiter)
        .map((entry) => normalizeLibraryPathEntry(entry, homeDir))
        .filter(Boolean);
}

export function resolveModelicaPathSources(
    configuredEntries: readonly string[],
    env: NodeJS.ProcessEnv = process.env,
    homeDir: string = os.homedir()
): ModelicaPathSources {
    const configuredPaths = mergeLibraryPathLists(
        configuredEntries
            .map((entry) => normalizeLibraryPathEntry(entry, homeDir))
            .filter(Boolean),
        [],
    );
    const envModelicaPath = parsePathListEnvVar(env.MODELICAPATH, homeDir);
    const envLegacyPath = parsePathListEnvVar(env.MODELICPATH, homeDir);
    const environmentPaths = mergeLibraryPathLists(envModelicaPath, envLegacyPath);
    return {
        configuredPaths,
        environmentPaths,
        mergedPaths: mergeLibraryPathLists(configuredPaths, environmentPaths),
        usedLegacyAlias: envLegacyPath.length > 0,
    };
}

export function changedRumocaRestartKeys(
    affectsConfiguration: (section: string) => boolean
): string[] {
    const keys = ['modelicaPath', 'serverPath', 'useSystemServer', 'debug'];
    return keys.filter((key) => affectsConfiguration(`rumoca.${key}`));
}

export function notebookExecutableFromServerPath(serverPath: string | undefined): string | undefined {
    if (!serverPath) {
        return undefined;
    }
    return serverPath.replace('-lsp', '');
}
