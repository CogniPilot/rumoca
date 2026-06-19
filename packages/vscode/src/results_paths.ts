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
    const candidate = path.resolve(__dirname, '..', 'media', 'vendor', 'visualization_shared.cjs');
    if (!fs.existsSync(candidate)) {
        throw new Error(
            `Missing shared Rumoca visualization helpers at ${candidate}. `
            + 'Run `npm run build` in packages/vscode.',
        );
    }
    const loaded = nodeRequire(candidate) as Partial<VisualizationSharedModule>;
    if (typeof loaded.preferredViewerScriptPathForModel !== 'function'
        || typeof loaded.sanitizeResultsPathSegment !== 'function') {
        throw new Error(`Invalid shared Rumoca visualization helpers at ${candidate}.`);
    }
    visualizationSharedCache = loaded as VisualizationSharedModule;
    return visualizationSharedCache;
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
