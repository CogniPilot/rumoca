// Proposed edits. The model never writes: every change is a proposal the user
// accepts or rejects in the panel, and acceptance goes through the host.

export function applyEdits(original, edits) {
    let text = original;
    for (const [index, edit] of edits.entries()) {
        const first = text.indexOf(edit.old);
        if (edit.old === '' || first < 0) {
            throw new Error(`edit ${index + 1}: the text to replace was not found; read the file again and copy it exactly`);
        }
        if (text.indexOf(edit.old, first + 1) >= 0) {
            throw new Error(`edit ${index + 1}: the text to replace occurs more than once; include more surrounding lines`);
        }
        text = text.slice(0, first) + edit.new + text.slice(first + edit.old.length);
    }
    return text;
}

export function createProposalStore({ host }) {
    const proposals = new Map();
    const listeners = new Set();
    let nextId = 1;
    const notify = () => listeners.forEach((listener) => listener());

    return {
        subscribe(listener) {
            listeners.add(listener);
            return () => listeners.delete(listener);
        },
        list: () => [...proposals.values()],
        add({ path, original, proposed, summary, kind }) {
            const id = `p${nextId++}`;
            proposals.set(id, { id, path, original, proposed, summary, kind, status: 'pending', error: null });
            notify();
            return id;
        },
        async accept(id) {
            const proposal = proposals.get(id);
            if (!proposal || proposal.status !== 'pending') return;
            if (host.readFile(proposal.path) !== proposal.original) {
                proposal.error = 'The file changed after this proposal was made. Ask for a new one.';
                notify();
                return;
            }
            await host.applyFile(proposal.path, proposal.proposed);
            proposal.status = 'accepted';
            proposal.error = null;
            notify();
        },
        reject(id) {
            const proposal = proposals.get(id);
            if (proposal?.status === 'pending') {
                proposal.status = 'rejected';
                notify();
            }
        },
    };
}
