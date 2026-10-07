const MAX_DIAGNOSTICS = 60;
const MAX_VARIABLES = 20;

function summarizeRun(run, requested) {
    const { names, allData } = run.payload;
    // allData[0] is time; allData[i + 1] is names[i].
    const time = allData[0];
    const wanted = requested?.length ? requested : names.slice(0, MAX_VARIABLES);
    const variables = wanted.slice(0, MAX_VARIABLES).map((name) => {
        const column = allData[names.indexOf(name) + 1];
        if (!column) return { name, error: 'no such variable' };
        return {
            name,
            first: column[0],
            last: column[column.length - 1],
            min: Math.min(...column),
            max: Math.max(...column),
        };
    });
    return {
        model: run.model,
        points: time.length,
        startTime: time[0],
        endTime: time[time.length - 1],
        variableCount: names.length,
        variableNames: names.slice(0, 200),
        variables,
    };
}

export function createBuildTools({ host, docs }) {
    return [
        {
            name: 'compile',
            description:
                'Check a Modelica file with the Rumoca compiler. Returns structured diagnostics '
                + '(code, severity, file, 1-based span, message, help). Defaults to the active file.',
            parameters: {
                type: 'object',
                properties: { path: { type: 'string' } },
                additionalProperties: false,
            },
            async run({ path }) {
                const target = path ?? host.activePath();
                const found = await host.diagnose(target);
                const diagnostics = found.slice(0, MAX_DIAGNOSTICS).map((diagnostic) => ({
                    ...diagnostic,
                    file: target,
                    help: diagnostic.help ?? docs.diagnostic(diagnostic.code)?.help ?? null,
                }));
                return { file: target, errorCount: found.filter((d) => d.severity === 'error').length, diagnostics };
            },
        },
        {
            name: 'lint',
            description: 'Run the Rumoca linter on a Modelica file. Returns rule, level, message, line, column, suggestion.',
            parameters: {
                type: 'object',
                properties: { path: { type: 'string' } },
                additionalProperties: false,
            },
            async run({ path }) {
                const target = path ?? host.activePath();
                return { file: target, messages: await host.lint(target) };
            },
        },
        {
            name: 'simulate',
            description:
                'Simulate a scenario (a rumoca-scenario*.toml path) or a model by name, using the settings the '
                + 'playground would use. Returns the saved results file and a summary.',
            parameters: {
                type: 'object',
                properties: {
                    scenario: { type: 'string', description: 'Path of a rumoca-scenario*.toml file.' },
                    model: { type: 'string', description: 'Qualified model name when no scenario is given.' },
                },
                additionalProperties: false,
            },
            async run({ scenario, model }) {
                if (!scenario && !model) throw new Error('give a scenario path or a model name');
                const { runPath } = await host.simulate({ scenario, model });
                return { runPath, summary: summarizeRun(host.readRun(runPath)) };
            },
        },
        {
            name: 'read_results',
            description: 'Summarize a saved simulation results file: time span and first, last, min, max of variables.',
            parameters: {
                type: 'object',
                properties: {
                    path: { type: 'string' },
                    variables: { type: 'array', items: { type: 'string' } },
                },
                required: ['path'],
                additionalProperties: false,
            },
            async run({ path, variables }) {
                return summarizeRun(host.readRun(path), variables);
            },
        },
    ];
}
