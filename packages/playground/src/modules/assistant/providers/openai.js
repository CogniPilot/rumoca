import { loadSdk } from '../sdk.js';
import { AssistantError } from './errors.js';

// OpenAI Responses API, used for both the ChatGPT plan (OAuth bearer) and an
// OpenAI API key. `getBearer` supplies the current bearer token; the plan path
// refreshes it before every request.

// Options a Responses request must never carry on the ChatGPT plan path. The
// SDK sends none of them by default; nothing in the assistant sets them.
export const PROHIBITED_RESPONSE_FIELDS = Object.freeze([
    'background', 'conversation', 'max_output_tokens', 'max_tool_calls', 'metadata',
    'moderation', 'multi_agent', 'prompt', 'prompt_cache_retention', 'safety_identifier',
    'temperature', 'top_logprobs', 'top_p', 'truncation', 'user',
]);

export function createOpenAiProvider({ config, getBearer, planModels }) {
    async function authorizedFetch(input, init = {}) {
        const headers = new Headers(init.headers);
        headers.set('authorization', `Bearer ${await getBearer()}`);
        return await globalThis.fetch(input, { ...init, headers });
    }

    return {
        id: 'openai',
        providerOptions: { openai: { store: false } },
        async listModels() {
            const response = await authorizedFetch(`${config.openaiApiBase}/models`);
            if (!response.ok) {
                throw new AssistantError('provider', `Listing models failed (${response.status}).`);
            }
            const { data } = await response.json();
            const usable = planModels
                ? data.filter((model) => model.visibility === 'list')
                : data.filter((model) => /^(gpt-|o\d)/u.test(model.id));
            return usable.map((model) => model.id).sort();
        },
        async model(modelId) {
            const { createOpenAI } = await loadSdk();
            const openai = createOpenAI({
                baseURL: config.openaiApiBase,
                apiKey: 'replaced-by-authorizedFetch',
                fetch: authorizedFetch,
            });
            return openai.responses(modelId);
        },
    };
}
