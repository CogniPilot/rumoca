import { kv } from '../store.js';

// Strategy: the base URL of a reachable local model server.
export function createLocalUrlStrategy(storageKey, defaultUrl) {
    return {
        kind: 'local_url',
        defaultUrl,
        async load() {
            return await kv.get(storageKey);
        },
        async save(url) {
            await kv.set(storageKey, normalizeServerUrl(url));
        },
        async forget() {
            await kv.delete(storageKey);
        },
    };
}

export function normalizeServerUrl(value) {
    let parsed;
    try {
        parsed = new URL(String(value || '').trim());
    } catch {
        throw new Error('Enter a full URL such as http://localhost:11434');
    }
    if (parsed.protocol !== 'http:' && parsed.protocol !== 'https:') {
        throw new Error('The server URL must start with http:// or https://');
    }
    return parsed.origin;
}
