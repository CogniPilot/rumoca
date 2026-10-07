// Retrieval over the static index built at site build time
// (vendor/assistant_index.json: diagnostic catalogue + user guide chunks).

let indexPromise = null;

export function loadAssistantIndex() {
    indexPromise ??= fetch(new URL('../../../../vendor/assistant_index.json', import.meta.url)).then(
        async (response) => {
            if (!response.ok) throw new Error(`assistant index unavailable (${response.status})`);
            return await response.json();
        },
    );
    return indexPromise;
}

const tokenize = (text) => String(text).toLowerCase().match(/[a-z0-9_]{2,}/gu) ?? [];

export function searchChunks(chunks, query, limit = 5) {
    const terms = [...new Set(tokenize(query))];
    if (terms.length === 0) return [];
    const frequency = new Map(
        terms.map((term) => [term, chunks.filter((chunk) => chunk.search.includes(term)).length]),
    );
    return chunks
        .map((chunk) => {
            let score = 0;
            for (const term of terms) {
                if (!chunk.search.includes(term)) continue;
                const idf = Math.log(1 + chunks.length / (1 + frequency.get(term)));
                const inHeading = chunk.heading.toLowerCase().includes(term) ? 2 : 0;
                score += idf * (1 + inHeading);
            }
            return { chunk, score };
        })
        .filter((entry) => entry.score > 0)
        .sort((a, b) => b.score - a.score)
        .slice(0, limit)
        .map((entry) => entry.chunk);
}

export async function createDocsRetrieval() {
    const index = await loadAssistantIndex();
    const chunks = index.docs.map((doc) => ({ ...doc, search: `${doc.title} ${doc.heading} ${doc.text}`.toLowerCase() }));
    const byCode = new Map(index.diagnostics.map((entry) => [entry.code, entry]));
    return {
        diagnostic: (code) => byCode.get(String(code).trim().toUpperCase()) ?? null,
        search: (query, limit) =>
            searchChunks(chunks, query, limit).map(({ path, title, heading, text }) => ({ path, title, heading, text })),
    };
}
