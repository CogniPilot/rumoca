import type { LanguageClientRuntime } from './language_client_runtime';

export interface DisposableNotebookController {
    dispose(): void;
}

export interface NotebookControllerRuntimeCallbacks {
    refreshConfig: () => void;
    languageClientRuntime: Pick<LanguageClientRuntime, 'getNotebookExecutable' | 'handleConfigurationChange'>;
    fileExists: (path: string) => boolean;
    createNotebookController: () => DisposableNotebookController;
    debugLog: (msg: string) => void;
}

export interface NotebookControllerRuntime {
    reconcileNotebookController: () => boolean;
    handleConfigurationChange: (
        affectsConfiguration: (section: string) => boolean
    ) => Promise<string[]>;
    disposeNotebookController: () => void;
}

export function createNotebookControllerRuntime(
    callbacks: NotebookControllerRuntimeCallbacks
): NotebookControllerRuntime {
    let notebookController: DisposableNotebookController | undefined;

    const disposeNotebookController = () => {
        if (!notebookController) {
            return;
        }
        notebookController.dispose();
        notebookController = undefined;
    };

    const reconcileNotebookController = (): boolean => {
        const rumocaExecutable = callbacks.languageClientRuntime.getNotebookExecutable();
        if (rumocaExecutable && callbacks.fileExists(rumocaExecutable)) {
            if (!notebookController) {
                notebookController = callbacks.createNotebookController();
                callbacks.debugLog(`Notebook controller created using: ${rumocaExecutable}`);
            }
            return true;
        }

        if (notebookController) {
            callbacks.debugLog(`Disposing notebook controller; rumoca executable is unavailable: ${rumocaExecutable}`);
            disposeNotebookController();
        } else {
            callbacks.debugLog(`Skipping notebook controller - rumoca executable not found at: ${rumocaExecutable}`);
        }
        return false;
    };

    const handleConfigurationChange = async (
        affectsConfiguration: (section: string) => boolean
    ): Promise<string[]> => {
        callbacks.refreshConfig();
        const changedKeys = await callbacks.languageClientRuntime.handleConfigurationChange(
            affectsConfiguration
        );
        if (changedKeys.length > 0) {
            reconcileNotebookController();
        }
        return changedKeys;
    };

    return {
        reconcileNotebookController,
        handleConfigurationChange,
        disposeNotebookController,
    };
}
