import { preferredViewerScriptPathForModel } from './scenario_interface.js';
import * as shared from '../../vendor/visualization_shared.js';

export function buildScenarioVisualizationViewStorage({
    workspaceFs,
    defaultViewerScript,
}) {
    return shared.buildVisualizationViewStorageHandlers({
        resolveViewerScriptPath: (nextModel, viewId) => preferredViewerScriptPathForModel(nextModel, viewId),
        readTextFile: (scriptPath) => workspaceFs.getFileContent(scriptPath),
        writeTextFile: (scriptPath, content) => {
            workspaceFs.setFile(scriptPath, content);
        },
        removeTextFile: (scriptPath) => {
            workspaceFs.removeFile(scriptPath);
        },
        defaultViewerScript: typeof defaultViewerScript === 'function'
            ? defaultViewerScript
            : () => shared.defaultThreeDimensionalViewerScript(),
    });
}
