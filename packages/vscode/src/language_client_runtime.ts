import {
    changedRumocaRestartKeys,
    notebookExecutableFromServerPath,
} from './modelica_paths';

export interface StartedLanguageClient {
    clientStarted: boolean;
    serverPath?: string;
}

export interface LanguageClientRuntimeCallbacks<TClient> {
    getClient: () => TClient | undefined;
    setClient: (client: TClient | undefined) => void;
    startLanguageClient: () => Promise<StartedLanguageClient>;
    stopLanguageClient: (client: TClient) => Promise<void>;
    log: (msg: string) => void;
    reportError: (msg: string) => void;
}

export interface LanguageClientRuntime {
    setServerPath: (serverPath: string | undefined) => void;
    getServerPath: () => string | undefined;
    getNotebookExecutable: () => string | undefined;
    restartLanguageClient: (reason: string) => Promise<boolean>;
    handleConfigurationChange: (
        affectsConfiguration: (section: string) => boolean
    ) => Promise<string[]>;
}

export function createLanguageClientRuntime<TClient>(
    callbacks: LanguageClientRuntimeCallbacks<TClient>
): LanguageClientRuntime {
    let restartingLanguageClient = false;
    let serverPath: string | undefined;

    const restartLanguageClient = async (reason: string): Promise<boolean> => {
        if (restartingLanguageClient) {
            return false;
        }
        restartingLanguageClient = true;
        try {
            callbacks.log(`Restarting language server: ${reason}`);
            const previousClient = callbacks.getClient();
            callbacks.setClient(undefined);
            serverPath = undefined;
            if (previousClient) {
                await callbacks.stopLanguageClient(previousClient);
            }
            const started = await callbacks.startLanguageClient();
            serverPath = started.serverPath;
            return started.clientStarted;
        } catch (error) {
            callbacks.reportError(`Failed to restart language server: ${error}`);
            return false;
        } finally {
            restartingLanguageClient = false;
        }
    };

    const handleConfigurationChange = async (
        affectsConfiguration: (section: string) => boolean
    ): Promise<string[]> => {
        const changedKeys = changedRumocaRestartKeys(affectsConfiguration);
        if (changedKeys.length === 0) {
            return [];
        }

        await restartLanguageClient(`configuration changed (${changedKeys.join(', ')})`);
        return changedKeys;
    };

    return {
        setServerPath(nextServerPath: string | undefined) {
            serverPath = nextServerPath;
        },
        getServerPath() {
            return serverPath;
        },
        getNotebookExecutable() {
            return notebookExecutableFromServerPath(serverPath);
        },
        restartLanguageClient,
        handleConfigurationChange,
    };
}
