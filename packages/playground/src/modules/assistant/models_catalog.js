import { kv } from './store.js';

// Model metadata (tool-calling support, context size) from the open
// models.dev catalogue. The vendored snapshot is the offline default; the live
// catalogue is fetched lazily, cached on this device, and replaces it.

const CATALOG_URL = 'https://models.dev/api.json';
const CACHE_KEY = 'models:catalog';
const MAX_AGE_MS = 24 * 3600 * 1000;
const PROVIDERS = ['openai', 'anthropic'];

async function loadSnapshot() {
    const response = await fetch(new URL('./models_snapshot.json', import.meta.url));
    if (!response.ok) throw new Error(`model snapshot missing (${response.status})`);
    return await response.json();
}

function reduce(catalog) {
    const reduced = {};
    for (const provider of PROVIDERS) {
        reduced[provider] = {};
        for (const model of Object.values(catalog[provider].models)) {
            reduced[provider][model.id] = {
                name: model.name,
                toolCall: model.tool_call === true,
                context: model.limit?.context ?? 0,
            };
        }
    }
    return reduced;
}

let loaded = null;

// Resolves to { provider: { modelId: { name, toolCall, context } } }.
export function loadModelCatalog({ fetchImpl = (...args) => globalThis.fetch(...args), now = Date.now } = {}) {
    loaded ??= (async () => {
        const cached = await kv.get(CACHE_KEY);
        if (cached && now() - cached.fetchedAt < MAX_AGE_MS) return cached.catalog;
        try {
            const response = await fetchImpl(CATALOG_URL);
            if (!response.ok) throw new Error(`models.dev answered ${response.status}`);
            const catalog = reduce(await response.json());
            await kv.set(CACHE_KEY, { fetchedAt: now(), catalog });
            return catalog;
        } catch {
            // Offline or blocked: the snapshot is the declared default.
            return cached ? cached.catalog : await loadSnapshot();
        }
    })();
    return loaded;
}

// Annotates listed model ids; a model the catalogue marks as unable to call
// tools is dropped because every assistant turn depends on tools. Models the
// catalogue does not know (for example user-pulled local models) are kept.
export async function describeModels(providerKey, modelIds) {
    const known = (await loadModelCatalog())[providerKey] ?? {};
    return modelIds
        .map((id) => ({ id, ...(known[id] ?? { name: id, toolCall: null, context: 0 }) }))
        .filter((model) => model.toolCall !== false);
}
