import { kv } from '../store.js';

// Strategy: a user-supplied API key remembered on this device.
export function createApiKeyStrategy(storageKey) {
    return {
        kind: 'api_key',
        async load() {
            return await kv.get(storageKey);
        },
        async save(apiKey) {
            const trimmed = String(apiKey || '').trim();
            if (!trimmed) throw new Error('An API key is required.');
            await kv.set(storageKey, trimmed);
        },
        async forget() {
            await kv.delete(storageKey);
        },
    };
}
