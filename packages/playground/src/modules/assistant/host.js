// The adapter between the assistant tools and the playground's own owners.
// main.js supplies the playground functions; this file only translates their
// results into the shapes the tools return, so no tool reaches into globals.

const SEVERITY = { 1: 'error', 2: 'warning', 3: 'info', 4: 'hint' };
const WORKSPACE_CONFIG = 'rumoca-workspace.toml';

const baseName = (path) => path.split('/').pop();

export function createAssistantHost({
    workspaceFs,
    isScenarioPath,
    flushEditors,
    sendLanguageCommand,
    sendScenarioCommand,
    normalizeDiagnostics,
    diagnosticCode,
    runScenario,
    runModel,
    normalizeRun,
    openDocument,
    didChangeFiles,
}) {
    const isConfigPath = (path) => baseName(path) === WORKSPACE_CONFIG || isScenarioPath(path);

    return {
        activePath: () => workspaceFs.getActiveDocumentPath(),
        listFiles() {
            return workspaceFs
                .listFileEntries()
                .filter((entry) => entry.sourceKind === 'workspace' && entry.isText)
                .map((entry) => entry.path);
        },
        listConfigFiles() {
            return this.listFiles().filter(isConfigPath);
        },
        readFile(path) {
            flushEditors();
            return workspaceFs.getFileContent(path);
        },
        async applyFile(path, content) {
            workspaceFs.setFile(path, content);
            await openDocument(path);
            didChangeFiles();
        },
        async diagnose(path) {
            const source = this.readFile(path);
            if (source === null) throw new Error(`no such file: ${path}`);
            const raw = await sendLanguageCommand('rumoca.language.diagnostics', { source, focusPath: path });
            return normalizeDiagnostics(JSON.parse(raw), source).map((diagnostic) => ({
                severity: SEVERITY[diagnostic.severity] ?? 'error',
                code: diagnosticCode(diagnostic),
                message: diagnostic.message,
                line: diagnostic.range.start.line + 1,
                column: diagnostic.range.start.character + 1,
                endLine: diagnostic.range.end.line + 1,
                endColumn: diagnostic.range.end.character + 1,
            }));
        },
        async lint(path) {
            const source = this.readFile(path);
            if (source === null) throw new Error(`no such file: ${path}`);
            return await sendLanguageCommand('rumoca.language.lint', { source });
        },
        async simulate({ scenario, model }) {
            flushEditors();
            return scenario ? await runScenario(scenario) : await runModel(model);
        },
        readRun(path) {
            const text = workspaceFs.getFileContent(path);
            if (text === null) throw new Error(`no such results file: ${path}`);
            const run = normalizeRun(JSON.parse(text));
            if (!run) throw new Error(`${path} is not a simulation results file`);
            return run;
        },
        // Validate with the same parsers the playground uses for these files.
        async validateConfig(path, content) {
            const sources = JSON.stringify({ [path]: content });
            if (isScenarioPath(path)) {
                const full = await sendScenarioCommand('rumoca.scenario.getScenarioConfigFull', { workspaceSources: sources, path });
                return full.ok === false ? { ok: false, error: full.error } : { ok: true };
            }
            if (baseName(path) === WORKSPACE_CONFIG) {
                const snapshot = await sendScenarioCommand('rumoca.scenario.getSimulationConfig', {
                    workspaceSources: sources, model: '', fallback: null,
                });
                const problems = snapshot.diagnostics ?? [];
                return problems.length ? { ok: false, error: problems.map((entry) => entry.message ?? String(entry)).join('; ') } : { ok: true };
            }
            return { ok: false, error: `${path} is not a Rumoca configuration file` };
        },
    };
}
