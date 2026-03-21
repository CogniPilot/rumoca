import * as fs from 'node:fs';
import { createRequire } from 'node:module';
import * as path from 'node:path';

const nodeRequire = createRequire(__filename);

type VisualizationSharedModule = {
    modelScopedViewerScriptRelativePath(uuid: string, viewId: string): string;
    preferredViewerScriptPathForModel(model: string, viewId: string): string;
    sanitizeResultsPathSegment(input: string): string;
};

let visualizationSharedCache: VisualizationSharedModule | undefined;

function loadVisualizationShared(): VisualizationSharedModule {
    if (visualizationSharedCache) {
        return visualizationSharedCache;
    }
    const candidates = [
        path.resolve(__dirname, '..', 'media', 'vendor', 'visualization_shared.js'),
        path.resolve(__dirname, '..', '..', '..', 'crates', 'rumoca-sim', 'web', 'visualization_shared.js'),
    ];
    for (const candidate of candidates) {
        if (!fs.existsSync(candidate)) {
            continue;
        }
        const loaded = nodeRequire(candidate) as Partial<VisualizationSharedModule>;
        if (typeof loaded.modelScopedViewerScriptRelativePath !== 'function'
            || typeof loaded.preferredViewerScriptPathForModel !== 'function'
            || typeof loaded.sanitizeResultsPathSegment !== 'function') {
            continue;
        }
        visualizationSharedCache = loaded as VisualizationSharedModule;
        return visualizationSharedCache;
    }
    throw new Error('Failed to load shared Rumoca visualization helpers.');
}

function trimMaybeString(value: unknown): string {
    return typeof value === 'string' ? value.trim() : '';
}

function classNameFromQualifiedName(model: string): string {
    return model.split('.').filter(Boolean).pop() || 'Model';
}

function unescapeTomlString(value: string): string {
    try {
        return JSON.parse(`"${value}"`);
    } catch {
        return value;
    }
}

function matchTomlStringField(text: string, key: string): string | undefined {
    const match = text.match(new RegExp(`^${key}\\s*=\\s*"((?:[^"\\\\]|\\\\.)*)"\\s*$`, 'm'));
    return match ? unescapeTomlString(match[1]) : undefined;
}

function matchTomlStringArrayField(text: string, key: string): string[] {
    const match = text.match(new RegExp(`^${key}\\s*=\\s*\\[(.*)\\]\\s*$`, 'm'));
    if (!match) {
        return [];
    }
    const values = [];
    const itemPattern = /"((?:[^"\\]|\\.)*)"/g;
    let itemMatch;
    while ((itemMatch = itemPattern.exec(match[1])) !== null) {
        values.push(unescapeTomlString(itemMatch[1]));
    }
    return values;
}

function parseIdentityRecord(text: string): { qualifiedName?: string; className?: string; aliases: string[] } {
    return {
        qualifiedName: matchTomlStringField(text, 'qualified_name'),
        className: matchTomlStringField(text, 'class_name'),
        aliases: matchTomlStringArrayField(text, 'aliases'),
    };
}

export function sanitizeResultsPathSegment(input: string): string {
    return loadVisualizationShared().sanitizeResultsPathSegment(input);
}

export function modelScopedViewerScriptRelativePath(uuid: string, viewId: string): string {
    return loadVisualizationShared().modelScopedViewerScriptRelativePath(uuid, viewId);
}

export function preferredViewerScriptPathForModel(model: string, viewId: string): string {
    return loadVisualizationShared().preferredViewerScriptPathForModel(model, viewId);
}

export async function resolveModelIdentityUuid(
    workspaceRoot: string,
    model: string,
): Promise<string | undefined> {
    const byIdRoot = path.join(workspaceRoot, '.rumoca', 'models', 'by-id');
    let entries;
    try {
        entries = await fs.promises.readdir(byIdRoot, { withFileTypes: true });
    } catch {
        return undefined;
    }

    let aliasMatch: string | undefined;
    const classMatches: string[] = [];
    const className = classNameFromQualifiedName(model);

    for (const entry of entries) {
        if (!entry.isDirectory()) {
            continue;
        }
        const uuid = entry.name;
        const identityPath = path.join(byIdRoot, uuid, 'identity.toml');
        let text: string;
        try {
            text = await fs.promises.readFile(identityPath, 'utf-8');
        } catch {
            continue;
        }
        const identity = parseIdentityRecord(text);
        if (trimMaybeString(identity.qualifiedName) === model) {
            return uuid;
        }
        if (!aliasMatch && identity.aliases.some((alias) => trimMaybeString(alias) === model)) {
            aliasMatch = uuid;
        }
        if (trimMaybeString(identity.className) === className) {
            classMatches.push(uuid);
        }
    }

    if (aliasMatch) {
        return aliasMatch;
    }
    if (classMatches.length === 1) {
        return classMatches[0];
    }
    return undefined;
}

export async function resolvePreferredViewerScriptPath(
    workspaceRoot: string | undefined,
    model: string,
    viewId: string,
): Promise<string> {
    if (!workspaceRoot) {
        throw new Error(`Cannot resolve a model-scoped 3D viewer script path for '${model}' without a workspace root.`);
    }
    const uuid = await resolveModelIdentityUuid(workspaceRoot, model);
    if (uuid) {
        return modelScopedViewerScriptRelativePath(uuid, viewId);
    }
    return preferredViewerScriptPathForModel(model, viewId);
}
