import { el } from './dom.js';

// Review UI for one proposed edit: a Monaco diff plus accept / reject.
export function renderProposal({ proposal, store, getMonaco }) {
    const diffHost = el('div', { class: 'assistant-diff', 'data-testid': 'proposal-diff' });
    const actions = el('div', { class: 'assistant-row' });
    if (proposal.status === 'pending') {
        actions.append(
            el('button', { type: 'button', class: 'assistant-primary', 'data-action': 'accept-proposal', onclick: () => store.accept(proposal.id) }, 'Accept'),
            el('button', { type: 'button', class: 'assistant-secondary', 'data-action': 'reject-proposal', onclick: () => store.reject(proposal.id) }, 'Reject'),
        );
    } else {
        actions.append(el('span', { class: 'assistant-note', 'data-testid': 'proposal-status' }, proposal.status));
    }
    const node = el('details', {
        class: 'assistant-proposal', 'data-proposal': proposal.id, 'data-status': proposal.status, open: proposal.status === 'pending',
    },
    el('summary', {}, `${proposal.kind === 'config' ? 'Config' : 'Edit'}: ${proposal.path}`, el('span', { class: 'assistant-note' }, ` ${proposal.summary}`)),
    proposal.error && el('div', { class: 'assistant-error', role: 'alert' }, proposal.error),
    diffHost,
    actions);
    // Mount only once attached: a superseded render never reaches the document.
    if (proposal.status === 'pending') {
        requestAnimationFrame(() => {
            if (diffHost.isConnected) mountDiff(diffHost, proposal, getMonaco());
        });
    }
    return node;
}

function languageFor(path) {
    return path.endsWith('.toml') ? 'ini' : 'modelica';
}

function mountDiff(host, proposal, monaco) {
    const language = languageFor(proposal.path);
    const diff = monaco.editor.createDiffEditor(host, {
        readOnly: true,
        renderSideBySide: false,
        automaticLayout: true,
        minimap: { enabled: false },
        scrollBeyondLastLine: false,
    });
    const original = monaco.editor.createModel(proposal.original ?? '', language);
    const modified = monaco.editor.createModel(proposal.proposed, language);
    diff.setModel({ original, modified });
    // Disposed with the host node when the list re-renders.
    host.addEventListener('assistant-dispose', () => {
        diff.dispose();
        original.dispose();
        modified.dispose();
    });
}
