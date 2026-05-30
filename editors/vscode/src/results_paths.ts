import { createRequire } from 'node:module';
import * as fs from 'node:fs';
import * as path from 'node:path';

const nodeRequire = createRequire(__filename);

type VisualizationSharedModule = {
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
        path.resolve(__dirname, '..', '..', '..', 'crates', 'rumoca-viz-web', 'web', 'visualization_shared.js'),
    ];
    for (const candidate of candidates) {
        if (!fs.existsSync(candidate)) {
            continue;
        }
        const loaded = nodeRequire(candidate) as Partial<VisualizationSharedModule>;
        if (typeof loaded.preferredViewerScriptPathForModel !== 'function'
            || typeof loaded.sanitizeResultsPathSegment !== 'function') {
            continue;
        }
        visualizationSharedCache = loaded as VisualizationSharedModule;
        return visualizationSharedCache;
    }
    throw new Error('Failed to load shared Rumoca visualization helpers.');
}

export function sanitizeResultsPathSegment(input: string): string {
    return loadVisualizationShared().sanitizeResultsPathSegment(input);
}

export function preferredViewerScriptPathForModel(model: string, viewId: string): string {
    return loadVisualizationShared().preferredViewerScriptPathForModel(model, viewId);
}

export async function resolvePreferredViewerScriptPath(
    workspaceRoot: string | undefined,
    model: string,
    viewId: string,
): Promise<string> {
    if (!workspaceRoot) {
        throw new Error(`Cannot resolve a colocated 3D viewer script path for '${model}' without a workspace root.`);
    }
    return preferredViewerScriptPathForModel(model, viewId);
}
