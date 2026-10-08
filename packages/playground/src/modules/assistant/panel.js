import { el } from './dom.js';
import { renderConnectPage } from './connect_page.js';
import { renderProposal } from './proposal_view.js';
import { describeModels } from './models_catalog.js';

const MANAGE_USAGE_URL = 'https://chatgpt.com/#settings';
const FIRST_SIGN_IN_NOTICE = 'Eligible usage in this app uses your ChatGPT plan. Manage usage in your ChatGPT settings.';

export function createAssistantPanel({ session, agent, toolbox, getMonaco }) {
    const root = el('aside', { id: 'assistantPanel', class: 'assistant-panel', hidden: true, 'aria-label': 'Assistant', 'data-testid': 'assistant-panel' });
    document.body.append(root);

    let view = 'connect';
    let banner = null;
    let running = null;
    let usage = null;
    let pendingNotes = [];
    let draft = '';
    let renderToken = 0;
    const modelCache = new Map();
    const transcript = [];
    const dispose = () => root.querySelectorAll('.assistant-diff').forEach((node) => node.dispatchEvent(new Event('assistant-dispose')));

    function showError(error) {
        banner = error;
        void render();
    }

    // Renders are asynchronous and can overlap; only the newest one is applied.
    async function render() {
        const token = ++renderToken;
        const status = await session.status();
        const active = await session.active();
        if (view === 'chat' && !active) view = 'connect';
        const body = view === 'connect'
            ? el('div', { class: 'assistant-body' },
                renderConnectPage({ session, status, onChange: afterConnectionChange, onError: showError }),
                active && el('button', { type: 'button', class: 'assistant-primary', 'data-action': 'back-to-chat', onclick: () => { view = 'chat'; void render(); } }, 'Back to chat'))
            : await renderChat(active);
        if (token !== renderToken) return;
        dispose();
        root.replaceChildren(...[renderHeader(active), banner ? renderBanner(banner) : null, body].filter(Boolean));
    }

    async function afterConnectionChange() {
        banner = null;
        modelCache.clear();
        const active = await session.active();
        view = active ? 'chat' : 'connect';
        await render();
    }

    function renderHeader(active) {
        return el('header', { class: 'assistant-header' },
            el('strong', {}, 'Assistant'),
            active?.usingPlan && el('span', { class: 'assistant-chip', 'data-testid': 'plan-indicator' }, 'Using ChatGPT plan'),
            el('span', { class: 'assistant-spacer' }),
            el('button', { type: 'button', class: 'assistant-icon', 'data-action': 'open-connect', title: 'Connections', onclick: () => { view = 'connect'; void render(); } }, 'Connect'),
            el('button', { type: 'button', class: 'assistant-icon', 'data-action': 'close-assistant', title: 'Close', 'aria-label': 'Close assistant', onclick: () => api.close() }, 'Close'));
    }

    function renderBanner(error) {
        const usageLimit = error.kind === 'usage_limit';
        return el('div', { class: 'assistant-error', role: 'alert', 'data-testid': 'assistant-error', 'data-kind': error.kind || 'other' },
            el('span', {}, error.message || String(error)),
            usageLimit && el('a', { class: 'assistant-primary', href: MANAGE_USAGE_URL, target: '_blank', rel: 'noopener', 'data-testid': 'manage-usage' }, 'Manage usage'));
    }

    async function renderChat(active) {
        const models = await loadModels(active);
        const select = el('select', {
            'data-testid': 'model-select', 'aria-label': 'Model',
            onchange: () => session.setModel(active.card, select.value).then(render),
        }, models.map((model) => el('option', { value: model.id, selected: model.id === active.modelId }, model.id)));
        const log = el('div', { class: 'assistant-log', 'data-testid': 'assistant-log', role: 'log' },
            transcript.length === 0 && el('p', { class: 'assistant-note' }, 'Ask about Modelica, configuration, or a diagnostic.'),
            transcript.map(renderEntry));
        const proposalList = el('div', { class: 'assistant-proposals', 'data-testid': 'assistant-proposals' },
            toolbox.proposals.list().map((proposal) => renderProposal({ proposal, store: toolbox.proposals, getMonaco })));
        const input = el('textarea', { rows: 2, placeholder: 'Ask the assistant', 'data-testid': 'assistant-input', 'aria-label': 'Message' });
        input.value = draft;
        input.addEventListener('input', () => { draft = input.value; });
        const send = () => {
            const text = input.value.trim();
            if (text) {
                draft = '';
                void submit(text);
            }
        };
        input.addEventListener('keydown', (event) => {
            if (event.key === 'Enter' && !event.shiftKey) {
                event.preventDefault();
                send();
            }
        });
        queueMicrotask(() => { log.scrollTop = log.scrollHeight; });
        return el('div', { class: 'assistant-body assistant-chat' },
            el('div', { class: 'assistant-row' }, el('span', { class: 'assistant-note' }, `${active.title}:`), select,
                active.usingPlan && el('a', { href: MANAGE_USAGE_URL, target: '_blank', rel: 'noopener', class: 'assistant-link' }, 'Manage usage')),
            log,
            proposalList,
            usage && el('div', { class: 'assistant-note', 'data-testid': 'assistant-usage' }, `Tokens: ${usage.inputTokens ?? '?'} in, ${usage.outputTokens ?? '?'} out`),
            el('div', { class: 'assistant-composer' }, input,
                running
                    ? el('button', { type: 'button', class: 'assistant-secondary', 'data-action': 'stop', onclick: () => running.abort() }, 'Stop')
                    : el('button', { type: 'button', class: 'assistant-primary', 'data-action': 'send', onclick: send }, 'Send')));
    }

    function renderEntry(entry) {
        if (entry.role === 'tool') {
            return el('details', { class: 'assistant-tool', 'data-tool': entry.name },
                el('summary', {}, `Tool: ${entry.name}`), el('pre', {}, entry.detail));
        }
        return el('div', { class: `assistant-msg assistant-${entry.role}`, 'data-role': entry.role }, entry.text);
    }

    async function loadModels(active) {
        try {
            if (!modelCache.has(active.card)) modelCache.set(active.card, await active.provider.listModels());
            const ids = modelCache.get(active.card);
            const listed = active.catalogKey
                ? await describeModels(active.catalogKey, ids)
                : ids.map((id) => ({ id }));
            if (!active.modelId && listed[0]) {
                await session.setModel(active.card, listed[0].id);
                active.modelId = listed[0].id;
            }
            return listed;
        } catch (error) {
            banner = error;
            return [];
        }
    }

    async function submit(text) {
        banner = null;
        const prompt = [...pendingNotes, text].join('\n');
        pendingNotes = [];
        transcript.push({ role: 'user', text });
        const reply = { role: 'assistant', text: '' };
        transcript.push(reply);
        running = new AbortController();
        await render();
        await agent.run({
            prompt,
            signal: running.signal,
            onEvent(event) {
                if (event.type === 'text') reply.text += event.text;
                else if (event.type === 'tool_call') transcript.splice(-1, 0, { role: 'tool', name: event.name, detail: JSON.stringify(event.input, null, 1) });
                else if (event.type === 'tool_error') transcript.splice(-1, 0, { role: 'tool', name: event.name, detail: `error: ${event.message}` });
                else if (event.type === 'done') usage = event.usage;
                else if (event.type === 'error' && event.error.kind !== 'cancelled') banner = event.error;
                void render();
            },
        });
        running = null;
        await render();
    }

    toolbox.proposals.subscribe(() => {
        for (const proposal of toolbox.proposals.list()) {
            if (proposal.status === 'accepted' && !proposal.noted) {
                proposal.noted = true;
                pendingNotes.push(`(The user accepted your edit to ${proposal.path}.)`);
            }
            if (proposal.status === 'rejected' && !proposal.noted) {
                proposal.noted = true;
                pendingNotes.push(`(The user rejected your edit to ${proposal.path}.)`);
            }
        }
        void render();
    });

    const api = {
        async open() {
            root.hidden = false;
            const active = await session.active();
            if (!active) view = 'connect';
            else if (view === 'connect' && !banner) view = 'chat';
            await render();
        },
        close() {
            root.hidden = true;
        },
        toggle() {
            return root.hidden ? api.open() : api.close();
        },
        // Finishes a redirect sign-in when the page was opened by one.
        async start() {
            try {
                const result = await session.completeRedirect(globalThis.location.href);
                if (!result) return;
                history.replaceState(null, '', globalThis.location.pathname);
                view = 'chat';
                await api.open();
                if (result.firstSignIn) showFirstSignInNotice();
            } catch (error) {
                history.replaceState(null, '', globalThis.location.pathname);
                banner = error;
                view = 'connect';
                await api.open();
            }
        },
    };

    function showFirstSignInNotice() {
        const dialog = el('dialog', { class: 'assistant-dialog', 'data-testid': 'first-sign-in-notice' },
            el('p', {}, FIRST_SIGN_IN_NOTICE),
            el('button', {
                type: 'button', class: 'assistant-primary', 'data-action': 'dismiss-notice',
                onclick: async () => { await session.markNoticeShown(); dialog.close(); dialog.remove(); },
            }, 'OK'));
        document.body.append(dialog);
        dialog.showModal();
    }

    return api;
}
