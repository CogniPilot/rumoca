import { classifyProviderError } from './providers/errors.js';

export const MAX_STEPS = 12;

export function buildSystemPrompt(toolNames) {
    return [
        'You are the assistant inside the Rumoca playground, a browser Modelica workspace with an in-browser compiler, linter and simulator.',
        'You help the user write Modelica, set configuration (rumoca-workspace.toml and rumoca-scenario*.toml) and understand compiler diagnostics.',
        `Tools available: ${toolNames.join(', ')}.`,
        'Rules:',
        '- Propose edits, never apply them. Use propose_edit or propose_config_edit; the user reviews a diff and accepts or rejects. Never claim a change was made before the user accepts it.',
        '- Read a file before proposing an edit to it, and copy the text to replace exactly.',
        '- When the user says an edit was accepted, run compile to confirm it before reporting success.',
        '- For a diagnostic code, call explain_diagnostic; for language or tool questions, call search_docs before answering from memory.',
        '- Keep answers short. Quote diagnostic codes and file paths exactly.',
    ].join('\n');
}

export function createAgent({ session, toolbox }) {
    let history = [];
    const toolNames = toolbox.tools.map((tool) => tool.name);

    return {
        reset() {
            history = [];
        },
        // Streams one user turn. `onEvent` receives the provider events plus
        // { type: 'error', error } with a classified error; resolves when the
        // turn ends (finished, failed or cancelled).
        async run({ prompt, signal, onEvent }) {
            const active = await session.active();
            if (!active) throw new Error('Connect a provider first.');
            if (!active.modelId) throw new Error('Choose a model first.');
            const messages = [...history, { role: 'user', content: prompt }];
            try {
                for await (const event of active.provider.stream({
                    modelId: active.modelId,
                    system: buildSystemPrompt(toolNames),
                    messages,
                    tools: toolbox.tools,
                    signal,
                    maxSteps: MAX_STEPS,
                })) {
                    if (event.type === 'done') history = [...messages, ...event.messages];
                    onEvent(event);
                }
            } catch (error) {
                onEvent({ type: 'error', error: classifyProviderError(error) });
            }
        },
    };
}
