import { getAssistantConfig, resolveChatgptSignIn } from './config.js';
import { kv } from './store.js';
import { createApiKeyStrategy } from './auth/api_key.js';
import { createLocalUrlStrategy } from './auth/local_url.js';
import { createOAuthPkceStrategy } from './auth/oauth_pkce.js';
import { createEndpointKeyStrategy } from './auth/endpoint_key.js';
import { createOpenAiProvider } from './providers/openai.js';
import { createAnthropicProvider } from './providers/anthropic.js';
import { createLocalProvider } from './providers/local.js';
import { createCompatibleProvider } from './providers/compatible.js';
import { withAiSdkStream } from './providers/ai_sdk_stream.js';

// The session owns which connection is active, what is remembered on this
// device, and how a remembered credential becomes a provider. Each card lists
// the auth strategies it supports; adding a strategy to a card (for example
// OAuth for Claude) changes the table below and nothing in the panel. A
// provider is anything with `listModels()` and `stream()` (see
// providers/ai_sdk_stream.js), so a later local-agent card slots in here too.

const CARD_ORDER = ['chatgpt', 'anthropic', 'local', 'compatible'];
const ACTIVE_KEY = 'session:active';
const modelKey = (card) => `session:model:${card}`;

export function createAssistantSession({ location = globalThis.location } = {}) {
    const config = getAssistantConfig();
    const chatgptSignIn = resolveChatgptSignIn(location, config);
    const chatgpt = createOAuthPkceStrategy({ config, signIn: chatgptSignIn });
    const openaiKey = createApiKeyStrategy('apikey:openai');
    const anthropicKey = createApiKeyStrategy('apikey:anthropic');
    const compatibleEndpoint = createEndpointKeyStrategy('endpoint:compatible');
    const localUrl = createLocalUrlStrategy('local:url', config.ollamaDefaultUrl);

    const cards = {
        local: {
            title: 'Local',
            catalogKey: 'ollama',
            strategies: { local_url: localUrl },
            async build() {
                const provider = createLocalProvider({ url: await localUrl.load() });
                return { usingPlan: false, provider: withAiSdkStream(provider) };
            },
        },
        chatgpt: {
            title: 'ChatGPT',
            catalogKey: 'openai',
            strategies: { oauth_pkce: chatgpt, api_key: openaiKey },
            async build(kind) {
                if (kind === 'oauth_pkce') {
                    const provider = createOpenAiProvider({
                        config, planModels: true, getBearer: () => chatgpt.getAccessToken(),
                    });
                    return { usingPlan: true, provider: withAiSdkStream(provider) };
                }
                const apiKey = await openaiKey.load();
                const provider = createOpenAiProvider({ config, planModels: false, getBearer: async () => apiKey });
                return { usingPlan: false, provider: withAiSdkStream(provider) };
            },
        },
        anthropic: {
            title: 'Claude',
            catalogKey: 'anthropic',
            strategies: { api_key: anthropicKey },
            async build() {
                const provider = createAnthropicProvider({ config, apiKey: await anthropicKey.load() });
                return { usingPlan: false, provider: withAiSdkStream(provider) };
            },
        },
        compatible: {
            title: 'OpenAI-compatible endpoint + key',
            catalogKey: null,
            advanced: true,
            strategies: { endpoint_key: compatibleEndpoint },
            async build() {
                const provider = createCompatibleProvider(await compatibleEndpoint.load());
                return { usingPlan: false, provider: withAiSdkStream(provider) };
            },
        },
    };

    async function strategyStatus(kind, strategy) {
        const credential = await strategy.load();
        const status = { kind, connected: credential !== null };
        if (kind === 'oauth_pkce') {
            status.available = chatgptSignIn.mode !== 'unavailable';
        } else if (kind === 'local_url') {
            status.url = credential || strategy.defaultUrl;
        }
        return status;
    }

    function strategyFor(card, kind) {
        const strategy = cards[card]?.strategies[kind];
        if (!strategy) throw new Error(`Unknown connection method ${card}/${kind}.`);
        return strategy;
    }

    const session = {
        config,
        async status() {
            const active = await kv.get(ACTIVE_KEY);
            const result = {};
            for (const card of CARD_ORDER) {
                const definition = cards[card];
                const methods = [];
                for (const [kind, strategy] of Object.entries(definition.strategies)) {
                    methods.push(await strategyStatus(kind, strategy));
                }
                result[card] = {
                    title: definition.title,
                    advanced: definition.advanced === true,
                    methods,
                    connected: methods.some((method) => method.connected),
                    active: active?.card === card,
                };
            }
            return result;
        },
        presets: (card, kind) => strategyFor(card, kind).presets,
        // Persist a credential (API key or server URL) and make it active.
        async connect(card, kind, value) {
            await strategyFor(card, kind).save(value);
            await kv.set(ACTIVE_KEY, { card, kind });
        },
        // Redirect-based strategies: returns the URL to navigate to.
        beginRedirect: (card, kind) => strategyFor(card, kind).begin(),
        // Called once on page load; finishes a redirect sign-in if one is pending.
        async completeRedirect(urlString) {
            const result = await chatgpt.completeFromUrl(urlString);
            if (!result) return null;
            await kv.set(ACTIVE_KEY, { card: 'chatgpt', kind: 'oauth_pkce' });
            return result;
        },
        markNoticeShown: () => chatgpt.markNoticeShown(),
        async signOut(card, kind) {
            const strategy = strategyFor(card, kind);
            await (strategy.signOut ? strategy.signOut() : strategy.forget());
            await session.clearActiveIf(card);
        },
        async forget(card) {
            for (const strategy of Object.values(cards[card].strategies)) {
                await strategy.forget();
            }
            await kv.delete(modelKey(card));
            await session.clearActiveIf(card);
        },
        async clearActiveIf(card) {
            if ((await kv.get(ACTIVE_KEY))?.card === card) await kv.delete(ACTIVE_KEY);
        },
        setModel: (card, modelId) => kv.set(modelKey(card), modelId),
        // The active connection as a provider, or null when none is remembered.
        async active() {
            const active = await kv.get(ACTIVE_KEY);
            if (!active) return null;
            const built = await cards[active.card].build(active.kind);
            return {
                card: active.card,
                title: cards[active.card].title,
                catalogKey: cards[active.card].catalogKey,
                usingPlan: built.usingPlan,
                provider: built.provider,
                modelId: await kv.get(modelKey(active.card)),
            };
        },
    };
    return session;
}
