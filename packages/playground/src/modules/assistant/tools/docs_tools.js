export function createDocsTools({ docs }) {
    return [
        {
            name: 'explain_diagnostic',
            description: 'Look up a Rumoca diagnostic code (for example ER002) in the diagnostic catalogue.',
            parameters: {
                type: 'object',
                properties: { code: { type: 'string' } },
                required: ['code'],
                additionalProperties: false,
            },
            async run({ code }) {
                const entry = docs.diagnostic(code);
                if (!entry) throw new Error(`no catalogue entry for ${code}`);
                const { description, message, help, phase } = entry;
                return { code: entry.code, phase, message, description, help };
            },
        },
        {
            name: 'search_docs',
            description: 'Search the Rumoca user guide. Returns the best matching sections.',
            parameters: {
                type: 'object',
                properties: { query: { type: 'string' } },
                required: ['query'],
                additionalProperties: false,
            },
            async run({ query }) {
                return { results: docs.search(query, 4) };
            },
        },
    ];
}
