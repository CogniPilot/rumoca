import { loadSdk } from '../sdk.js';
import { AssistantError } from './errors.js';

// Anthropic Messages API called directly from the browser with the
// user's own API key.

const BROWSER_ACCESS_HEADER = { 'anthropic-dangerous-direct-browser-access': 'true' };

export function createAnthropicProvider({ config, apiKey }) {
    return {
        id: 'anthropic',
        providerOptions: {},
        async listModels() {
            const response = await fetch(`${config.anthropicApiBase}/models`, {
                headers: {
                    'x-api-key': apiKey,
                    'anthropic-version': '2023-06-01',
                    ...BROWSER_ACCESS_HEADER,
                },
            });
            if (!response.ok) {
                throw new AssistantError(
                    response.status === 401 ? 'auth' : 'provider',
                    response.status === 401
                        ? 'Anthropic rejected the API key.'
                        : `Listing models failed (${response.status}).`,
                );
            }
            return (await response.json()).data.map((model) => model.id);
        },
        async model(modelId) {
            const { createAnthropic } = await loadSdk();
            return createAnthropic({
                apiKey,
                baseURL: config.anthropicApiBase,
                headers: BROWSER_ACCESS_HEADER,
            })(modelId);
        },
    };
}
