import { loadSdk } from '../sdk.js';
import { AssistantError } from './errors.js';

// Any OpenAI-compatible chat endpoint (for example OpenRouter) with a key.
export function createCompatibleProvider({ baseUrl, apiKey }) {
    return {
        id: 'compatible',
        providerOptions: {},
        async listModels() {
            const response = await fetch(`${baseUrl}/models`, { headers: { authorization: `Bearer ${apiKey}` } });
            if (!response.ok) {
                throw new AssistantError(response.status === 401 ? 'auth' : 'provider', `Listing models failed (${response.status}).`);
            }
            const { data } = await response.json();
            // Endpoints that publish supported parameters (OpenRouter) say which models call tools.
            return data
                .filter((model) => !model.supported_parameters || model.supported_parameters.includes('tools'))
                .map((model) => model.id)
                .sort();
        },
        async model(modelId) {
            const { createOpenAICompatible } = await loadSdk();
            return createOpenAICompatible({ name: 'compatible', baseURL: baseUrl, apiKey }).chatModel(modelId);
        },
    };
}
