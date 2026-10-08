import { loadSdk } from '../sdk.js';

// The provider contract the agent depends on:
//
//   provider.stream({ modelId, system, messages, tools, signal, maxSteps })
//     -> async iterable of events
//        { type: 'text', text }
//        { type: 'tool_call', id, name, input }
//        { type: 'tool_result', id, name, output }
//        { type: 'tool_error', id, name, message }
//        { type: 'step', usage }
//        { type: 'done', usage, messages }   // messages: opaque history to append
//
// where `tools` are `{ name, description, parameters, run(input) }`. A
// provider may run the multi-step tool loop itself (the AI SDK providers do)
// or be an agent runtime that calls `run` for its own tool requests. Nothing
// above this interface knows which.

function tokenUsage(usage) {
    if (!usage) return null;
    return {
        inputTokens: usage.inputTokens ?? null,
        outputTokens: usage.outputTokens ?? null,
    };
}

export function withAiSdkStream(provider) {
    return {
        ...provider,
        async *stream({ modelId, system, messages, tools, signal, maxSteps }) {
            const sdk = await loadSdk();
            const toolSet = Object.fromEntries(
                tools.map((definition) => [
                    definition.name,
                    sdk.tool({
                        description: definition.description,
                        inputSchema: sdk.jsonSchema(definition.parameters),
                        execute: (input) => definition.run(input),
                    }),
                ]),
            );
            const result = sdk.streamText({
                model: await provider.model(modelId),
                instructions: system,
                messages,
                tools: toolSet,
                stopWhen: sdk.stepCountIs(maxSteps),
                abortSignal: signal,
                providerOptions: provider.providerOptions,
                maxRetries: 0,
            });
            for await (const part of result.fullStream) {
                switch (part.type) {
                    case 'text-delta':
                        yield { type: 'text', text: part.text };
                        break;
                    case 'tool-call':
                        yield { type: 'tool_call', id: part.toolCallId, name: part.toolName, input: part.input };
                        break;
                    case 'tool-result':
                        yield { type: 'tool_result', id: part.toolCallId, name: part.toolName, output: part.output };
                        break;
                    case 'tool-error':
                        yield { type: 'tool_error', id: part.toolCallId, name: part.toolName, message: String(part.error?.message ?? part.error) };
                        break;
                    case 'finish-step':
                        yield { type: 'step', usage: tokenUsage(part.usage) };
                        break;
                    case 'error':
                        throw part.error;
                    default:
                        break;
                }
            }
            const { messages: appended } = await result.response;
            yield { type: 'done', usage: tokenUsage(await result.totalUsage), messages: appended };
        },
    };
}
