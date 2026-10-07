import { applyEdits } from './proposals.js';

const MAX_READ_CHARS = 40000;
const MAX_LISTED = 300;

export function createWorkspaceTools({ host, proposals }) {
    return [
        {
            name: 'list_files',
            description: 'List workspace files (paths only). Package archives and generated results are excluded.',
            parameters: {
                type: 'object',
                properties: { prefix: { type: 'string', description: 'Only paths starting with this prefix.' } },
                additionalProperties: false,
            },
            async run({ prefix = '' }) {
                const paths = host.listFiles().filter((path) => path.startsWith(prefix));
                return { paths: paths.slice(0, MAX_LISTED), truncated: paths.length > MAX_LISTED };
            },
        },
        {
            name: 'read_file',
            description: 'Read a text file from the workspace. Lines are 1-based.',
            parameters: {
                type: 'object',
                properties: {
                    path: { type: 'string' },
                    start_line: { type: 'integer', minimum: 1 },
                    end_line: { type: 'integer', minimum: 1 },
                },
                required: ['path'],
                additionalProperties: false,
            },
            async run({ path, start_line: start = 1, end_line: end }) {
                const content = host.readFile(path);
                if (content === null) throw new Error(`no such file: ${path}`);
                const lines = content.split('\n');
                const slice = lines.slice(start - 1, end ?? lines.length).join('\n');
                return {
                    path,
                    totalLines: lines.length,
                    content: slice.slice(0, MAX_READ_CHARS),
                    truncated: slice.length > MAX_READ_CHARS,
                };
            },
        },
        {
            name: 'propose_edit',
            description:
                'Propose a change to a Modelica or text file. The user reviews a diff and accepts or rejects; '
                + 'nothing is written until they accept. For an existing file give exact `edits` (each `old` text '
                + 'must occur exactly once). For a new file give `content`.',
            parameters: {
                type: 'object',
                properties: {
                    path: { type: 'string' },
                    summary: { type: 'string', description: 'One sentence describing the change.' },
                    content: { type: 'string', description: 'Full text of a new file.' },
                    edits: {
                        type: 'array',
                        items: {
                            type: 'object',
                            properties: { old: { type: 'string' }, new: { type: 'string' } },
                            required: ['old', 'new'],
                            additionalProperties: false,
                        },
                    },
                },
                required: ['path', 'summary'],
                additionalProperties: false,
            },
            async run({ path, summary, content, edits }) {
                const original = host.readFile(path);
                let proposed;
                if (original === null) {
                    if (typeof content !== 'string') throw new Error('a new file needs `content`');
                    proposed = content;
                } else {
                    if (!Array.isArray(edits) || edits.length === 0) throw new Error('an existing file needs `edits`');
                    proposed = applyEdits(original, edits);
                }
                const id = proposals.add({ path, original, proposed, summary, kind: 'file' });
                return { proposalId: id, status: 'pending user review' };
            },
        },
    ];
}
