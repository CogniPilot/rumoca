import { loadSdk } from '../sdk.js';
import { AssistantError } from './errors.js';

// Ollama through its OpenAI-compatible /v1 endpoint, so the Local card shares
// the AI SDK's OpenAI-compatible provider (streaming and tool calls included)
// instead of a second client.

export function ollamaOriginsInstruction(origin) {
    return `OLLAMA_ORIGINS=${origin}`;
}

// A blocked cross-origin request and an unreachable server look the same to
// the page: both reject with a TypeError, and the site is cross-origin
// isolated, so a no-cors probe cannot tell them apart either. A failure
// therefore names both causes and always carries the exact CORS instruction.
export async function probeLocalServer(url, { fetchImpl = (...args) => globalThis.fetch(...args), origin = globalThis.location.origin } = {}) {
    try {
        const response = await fetchImpl(`${url}/v1/models`);
        if (!response.ok) {
            return { ok: false, kind: 'http', message: `The server answered ${response.status}.` };
        }
        const { data } = await response.json();
        return { ok: true, models: data.map((model) => model.id) };
    } catch (error) {
        if (!(error instanceof TypeError)) throw error;
        return {
            ok: false,
            kind: 'unreachable_or_cors',
            instruction: ollamaOriginsInstruction(origin),
            message: `Could not reach ${url}. If Ollama is running there, the browser is blocking this site (CORS): restart Ollama with ${ollamaOriginsInstruction(origin)}. Otherwise start Ollama or check the URL.`,
        };
    }
}

export function createLocalProvider({ url }) {
    return {
        id: 'ollama',
        providerOptions: {},
        async listModels() {
            const probe = await probeLocalServer(url);
            if (!probe.ok) throw new AssistantError('network', probe.message);
            return probe.models;
        },
        async model(modelId) {
            const { createOpenAICompatible } = await loadSdk();
            return createOpenAICompatible({ name: 'ollama', baseURL: `${url}/v1` }).chatModel(modelId);
        },
    };
}
