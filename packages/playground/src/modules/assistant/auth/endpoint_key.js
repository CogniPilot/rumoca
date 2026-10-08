import { kv } from '../store.js';
import { normalizeServerUrl } from './local_url.js';

// Strategy: an OpenAI-compatible endpoint (base URL) plus its API key.
export const ENDPOINT_PRESETS = Object.freeze([
    { label: 'OpenRouter', baseUrl: 'https://openrouter.ai/api/v1' },
]);

export function createEndpointKeyStrategy(storageKey) {
    return {
        kind: 'endpoint_key',
        presets: ENDPOINT_PRESETS,
        load: () => kv.get(storageKey),
        async save({ baseUrl, apiKey }) {
            const key = String(apiKey || '').trim();
            if (!key) throw new Error('An API key is required.');
            const url = new URL(String(baseUrl || '').trim().replace(/\/+$/u, ''));
            normalizeServerUrl(url.origin);
            await kv.set(storageKey, { baseUrl: url.toString().replace(/\/+$/u, ''), apiKey: key });
        },
        forget: () => kv.delete(storageKey),
    };
}
