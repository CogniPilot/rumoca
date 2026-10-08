import { el } from './dom.js';
import { probeLocalServer } from './providers/local.js';

// The click-to-connect page: one card per provider, one generic form per
// auth strategy kind. A new strategy kind only needs a renderer here.

const CARD_BLURB = {
    chatgpt: 'Use your ChatGPT plan, or an OpenAI API key.',
    anthropic: 'Use an Anthropic API key. Anthropic offers no third-party sign-in.',
    compatible: 'Any OpenAI-compatible chat endpoint and its key, for example OpenRouter.',
    local: 'Use a model served by Ollama on your machine or network.',
};

const MOBILE_QUERY = '(max-width: 760px)';

export function renderConnectPage({ session, status, onChange, onError }) {
    const root = el('div', { class: 'assistant-connect', 'data-testid': 'assistant-connect' });
    root.append(
        el('p', { class: 'assistant-note' },
            'Credentials are remembered only in this browser, on this device. Nothing is stored on the site.'),
    );
    const advanced = el('details', { class: 'assistant-advanced', 'data-testid': 'assistant-advanced' },
        el('summary', {}, 'Advanced: OpenAI-compatible endpoint + key'));
    for (const [card, info] of Object.entries(status)) {
        (info.advanced ? advanced : root).append(renderCard({ session, card, info, onChange, onError }));
    }
    root.append(advanced);
    return root;
}

function renderCard({ session, card, info, onChange, onError }) {
    const body = el('div', { class: 'assistant-card-body' });
    for (const method of info.methods) {
        body.append(renderMethod({ session, card, method, onChange, onError }));
    }
    const state = info.connected ? (info.active ? 'Connected (in use)' : 'Connected') : 'Not connected';
    return el('section', { class: 'assistant-card', 'data-card': card, 'data-connected': info.connected },
        el('header', {},
            el('h3', {}, info.title),
            el('span', { class: 'assistant-card-state', 'data-testid': `state-${card}` }, state)),
        el('p', { class: 'assistant-note' }, CARD_BLURB[card]),
        body,
        info.connected && el('button', {
            type: 'button', class: 'assistant-link', 'data-action': `forget-${card}`,
            onclick: () => guard(onError, async () => { await session.forget(card); await onChange(); }),
        }, 'Forget on this device'));
}

async function guard(onError, action) {
    try {
        await action();
    } catch (error) {
        onError(error);
    }
}

function renderMethod({ session, card, method, onChange, onError }) {
    if (method.kind === 'oauth_pkce') return renderOAuth({ session, card, method, onChange, onError });
    if (method.kind === 'api_key') return renderApiKey({ session, card, method, onChange, onError });
    if (method.kind === 'local_url') return renderLocalUrl({ session, card, method, onChange, onError });
    if (method.kind === 'endpoint_key') return renderEndpointKey({ session, card, method, onChange, onError });
    throw new Error(`no connect form for strategy ${method.kind}`);
}

function renderOAuth({ session, card, method, onChange, onError }) {
    if (!method.available) {
        return el('div', { class: 'assistant-method', 'data-method': 'oauth_pkce' },
            el('button', { type: 'button', class: 'assistant-signin', disabled: true, 'data-testid': 'chatgpt-coming-soon' },
                'Sign-in coming soon'),
            el('p', { class: 'assistant-note' },
                'Sign in with ChatGPT needs a registered client for this site. Use an API key for now.'));
    }
    if (method.connected) {
        return el('div', { class: 'assistant-method', 'data-method': 'oauth_pkce' },
            el('p', { class: 'assistant-note' }, 'Signed in with your ChatGPT plan.'),
            el('button', {
                type: 'button', class: 'assistant-secondary', 'data-action': 'signout-chatgpt',
                onclick: () => guard(onError, async () => { await session.signOut(card, 'oauth_pkce'); await onChange(); }),
            }, 'Sign out'));
    }
    return el('div', { class: 'assistant-method', 'data-method': 'oauth_pkce' },
        el('button', {
            type: 'button', class: 'assistant-signin', 'data-testid': 'chatgpt-continue',
            onclick: () => guard(onError, async () => {
                globalThis.location.assign(await session.beginRedirect(card, 'oauth_pkce'));
            }),
        },
        el('img', { src: new URL('./assets/chatgpt-logo-white.svg', import.meta.url).href, alt: '', width: 18, height: 18 }),
        'Continue with ChatGPT'));
}

function renderApiKey({ session, card, method, onChange, onError }) {
    if (method.connected) {
        return el('p', { class: 'assistant-note', 'data-method': 'api_key' }, 'API key saved on this device.');
    }
    const input = el('input', {
        type: 'password', autocomplete: 'off', spellcheck: 'false',
        placeholder: 'API key', 'data-testid': `key-${card}`,
    });
    return el('form', {
        class: 'assistant-method', 'data-method': 'api_key',
        onsubmit: (event) => {
            event.preventDefault();
            guard(onError, async () => { await session.connect(card, 'api_key', input.value); await onChange(); });
        },
    }, input, el('button', { type: 'submit', class: 'assistant-secondary', 'data-action': `connect-${card}` }, 'Connect with API key'));
}

function renderLocalUrl({ session, card, method, onChange, onError }) {
    const input = el('input', { type: 'url', value: method.url, spellcheck: 'false', 'data-testid': 'local-url' });
    const result = el('div', { class: 'assistant-test-result', 'data-testid': 'local-test-result', role: 'status' });
    const test = async () => {
        result.textContent = 'Testing...';
        const probe = await probeLocalServer(input.value.replace(/\/+$/u, ''));
        result.dataset.ok = String(probe.ok);
        result.dataset.kind = probe.ok ? 'ok' : probe.kind;
        result.replaceChildren(
            probe.ok
                ? `Connected. ${probe.models.length} model(s) available.`
                : el('span', {}, probe.message),
        );
        return probe;
    };
    const mobileNote = globalThis.matchMedia?.(MOBILE_QUERY).matches
        && el('p', { class: 'assistant-note', 'data-testid': 'local-mobile-note' },
            'On a phone or tablet this needs an Ollama server reachable from this device (a computer on your network). localhost is the device itself.');
    return el('div', { class: 'assistant-method', 'data-method': 'local_url' },
        mobileNote,
        input,
        el('div', { class: 'assistant-row' },
            el('button', { type: 'button', class: 'assistant-secondary', 'data-action': 'test-local', onclick: () => guard(onError, test) },
                'Test connection'),
            el('button', {
                type: 'button', class: 'assistant-secondary', 'data-action': 'connect-local',
                onclick: () => guard(onError, async () => {
                    const probe = await test();
                    if (!probe.ok) return;
                    await session.connect(card, 'local_url', input.value);
                    await onChange();
                }),
            }, method.connected ? 'Save URL' : 'Connect')),
        result);
}

function renderEndpointKey({ session, card, method, onChange, onError }) {
    if (method.connected) {
        return el('p', { class: 'assistant-note', 'data-method': 'endpoint_key' }, 'Endpoint and key saved on this device.');
    }
    const presets = session.presets(card, 'endpoint_key');
    const base = el('input', { type: 'url', placeholder: 'https://host/v1', spellcheck: 'false', 'data-testid': 'compatible-base' });
    const key = el('input', { type: 'password', autocomplete: 'off', placeholder: 'API key', 'data-testid': 'compatible-key' });
    return el('form', {
        class: 'assistant-method', 'data-method': 'endpoint_key',
        onsubmit: (event) => {
            event.preventDefault();
            guard(onError, async () => {
                await session.connect(card, 'endpoint_key', { baseUrl: base.value, apiKey: key.value });
                await onChange();
            });
        },
    },
    el('div', { class: 'assistant-row' }, presets.map((preset) => el('button', {
        type: 'button', class: 'assistant-secondary', onclick: () => { base.value = preset.baseUrl; },
    }, preset.label))),
    base, key,
    el('button', { type: 'submit', class: 'assistant-secondary', 'data-action': 'connect-compatible' }, 'Connect'));
}
