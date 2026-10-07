#!/usr/bin/env node
// Regenerates the offline model catalogue snapshot from https://models.dev.
// Usage: node packages/playground/scripts/refresh_models_snapshot.mjs
import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const PROVIDERS = ['openai', 'anthropic'];
const output = path.join(
    path.dirname(fileURLToPath(import.meta.url)),
    '..', 'src', 'modules', 'assistant', 'models_snapshot.json',
);

const response = await fetch('https://models.dev/api.json');
if (!response.ok) throw new Error(`models.dev answered ${response.status}`);
const catalog = await response.json();

const snapshot = {};
for (const provider of PROVIDERS) {
    snapshot[provider] = {};
    for (const model of Object.values(catalog[provider].models)) {
        snapshot[provider][model.id] = {
            name: model.name,
            toolCall: model.tool_call === true,
            context: model.limit?.context ?? 0,
        };
    }
}
await fs.writeFile(output, `${JSON.stringify(snapshot, null, 1)}\n`);
console.log(`wrote ${output}`);
