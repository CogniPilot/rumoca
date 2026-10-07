// Deployment configuration for the assistant. `window.rumocaAssistantConfig`
// is declared in index.html (the deploy step fills `openaiSiwcClientId`) and
// may override any endpoint, which is how self-hosted copies and the
// contract tests point the assistant at other servers.

export const DEFAULT_CONFIG = Object.freeze({
    openaiSiwcClientId: '',
    agentName: 'Rumoca Playground',
    openaiIssuer: 'https://auth.openai.com',
    openaiAuthorizeUrl: 'https://auth.openai.com/api/accounts/authorize',
    openaiTokenUrl: 'https://auth.openai.com/api/accounts/oauth/token',
    openaiApiBase: 'https://api.openai.com/v1',
    anthropicApiBase: 'https://api.anthropic.com/v1',
    ollamaDefaultUrl: 'http://localhost:11434',
});

export function getAssistantConfig() {
    return { ...DEFAULT_CONFIG, ...(globalThis.rumocaAssistantConfig || {}) };
}

// Sign in with ChatGPT is open to loopback redirects only
// (http://127.0.0.1:{port}/callback, dynamic client registration) until a
// hosted client id is issued for the site.
export function resolveChatgptSignIn(locationLike, config) {
    if (locationLike.hostname === '127.0.0.1') {
        return {
            mode: 'dynamic',
            redirectUri: `${locationLike.origin}/callback`,
        };
    }
    if (config.openaiSiwcClientId) {
        return {
            mode: 'hosted',
            clientId: config.openaiSiwcClientId,
            redirectUri: `${locationLike.origin}${locationLike.pathname}`,
        };
    }
    return { mode: 'unavailable' };
}
