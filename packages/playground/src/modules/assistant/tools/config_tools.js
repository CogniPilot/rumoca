export function createConfigTools({ host, proposals }) {
    return [
        {
            name: 'read_config',
            description:
                'Read rumoca-workspace.toml or a rumoca-scenario*.toml file. Without a path, list the '
                + 'configuration files present in the workspace.',
            parameters: {
                type: 'object',
                properties: { path: { type: 'string' } },
                additionalProperties: false,
            },
            async run({ path }) {
                if (!path) return { files: host.listConfigFiles() };
                const content = host.readFile(path);
                if (content === null) throw new Error(`no such config file: ${path}`);
                return { path, content };
            },
        },
        {
            name: 'propose_config_edit',
            description:
                'Propose new full text for rumoca-workspace.toml or a rumoca-scenario*.toml file. The text is '
                + 'validated by the same parser the playground uses; invalid TOML is rejected here. The user '
                + 'reviews a diff and accepts or rejects.',
            parameters: {
                type: 'object',
                properties: {
                    path: { type: 'string' },
                    summary: { type: 'string' },
                    content: { type: 'string', description: 'Complete new file text.' },
                },
                required: ['path', 'summary', 'content'],
                additionalProperties: false,
            },
            async run({ path, summary, content }) {
                const validation = await host.validateConfig(path, content);
                if (!validation.ok) throw new Error(`invalid configuration: ${validation.error}`);
                const id = proposals.add({ path, original: host.readFile(path), proposed: content, summary, kind: 'config' });
                return { proposalId: id, status: 'pending user review' };
            },
        },
    ];
}
