import { createAssistantSession } from './session.js';
import { createToolbox } from './tools/index.js';
import { createAgent } from './agent.js';
import { createAssistantPanel } from './panel.js';

// Entry point. `host` is the adapter main.js builds over the playground's own
// modules (see tools/*.js for the methods the tools call). Nothing is loaded
// until the assistant is opened or an OAuth redirect has to be completed.
export function createAssistant({ host, getMonaco }) {
    const session = createAssistantSession();
    let panelPromise = null;

    function ensurePanel() {
        panelPromise ??= (async () => {
            const toolbox = await createToolbox({ host });
            const agent = createAgent({ session, toolbox });
            return createAssistantPanel({ session, agent, toolbox, getMonaco });
        })();
        return panelPromise;
    }

    return {
        async toggle() {
            return (await ensurePanel()).toggle();
        },
        async open() {
            return (await ensurePanel()).open();
        },
        // Call once at page load: completes an authorization redirect if the
        // page was opened by one.
        async start() {
            if (new URL(globalThis.location.href).searchParams.has('state')) {
                await (await ensurePanel()).start();
            }
        },
    };
}
