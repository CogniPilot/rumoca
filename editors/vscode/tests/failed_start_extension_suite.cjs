const assert = require("node:assert/strict");
const fs = require("node:fs");
const { performance } = require("node:perf_hooks");

function envPath(name) {
  const value = process.env[name];
  if (!value) {
    throw new Error(`missing required env var: ${name}`);
  }
  return value;
}

function envMs(name, fallback) {
  const value = process.env[name];
  if (!value) {
    return fallback;
  }
  const parsed = Number.parseInt(value, 10);
  if (!Number.isFinite(parsed) || parsed <= 0) {
    throw new Error(`invalid timeout env var ${name}=${value}`);
  }
  return parsed;
}

async function withTimeout(label, promiseFactory, timeoutMs) {
  return await Promise.race([
    Promise.resolve().then(promiseFactory),
    new Promise((_, reject) => {
      setTimeout(() => {
        reject(new Error(`${label} timed out after ${timeoutMs}ms`));
      }, timeoutMs);
    }),
  ]);
}

function writeResult(resultPath, payload) {
  fs.writeFileSync(resultPath, `${JSON.stringify(payload, null, 2)}\n`);
}

exports.run = async function run() {
  const vscode = require("vscode");

  const documentPath = envPath("RUMOCA_VSCODE_FAILED_START_DOCUMENT");
  const resultPath = envPath("RUMOCA_VSCODE_FAILED_START_RESULT");
  const activateMaxMs = envMs("RUMOCA_VSCODE_FAILED_START_ACTIVATE_MAX_MS", 15000);
  const commandMaxMs = envMs("RUMOCA_VSCODE_FAILED_START_COMMAND_MAX_MS", 10000);
  const requiredCommands = [
    "rumoca.simulateModel",
    "rumoca.renderTemplate",
    "rumoca.openSettingsMenu",
  ];

  const extension = vscode.extensions.getExtension("JamesGoppert.rumoca-modelica");
  assert(extension, "Rumoca extension should be available in the extension host");

  const result = {};

  const activateStart = performance.now();
  await withTimeout("extension activation", () => extension.activate(), activateMaxMs);
  result.activateMs = Math.round(performance.now() - activateStart);
  result.extensionActive = extension.isActive;
  assert.equal(extension.isActive, true, "extension should stay active even when rumoca-lsp startup fails");

  const uri = vscode.Uri.file(documentPath);
  const document = await withTimeout(
    "open text document",
    () => vscode.workspace.openTextDocument(uri),
    10000,
  );
  await withTimeout(
    "show text document",
    () => vscode.window.showTextDocument(document, { preview: false }),
    10000,
  );

  const commands = await withTimeout(
    "get command inventory",
    () => vscode.commands.getCommands(true),
    5000,
  );
  result.registeredCommands = requiredCommands.filter((command) => commands.includes(command));
  assert.deepEqual(
    result.registeredCommands,
    requiredCommands,
    "Rumoca editor commands should remain registered after failed initial language-server startup",
  );

  const simulateStart = performance.now();
  await withTimeout(
    "execute rumoca.simulateModel",
    () => vscode.commands.executeCommand("rumoca.simulateModel"),
    commandMaxMs,
  );
  result.simulateExecuteMs = Math.round(performance.now() - simulateStart);

  const renderStart = performance.now();
  await withTimeout(
    "execute rumoca.renderTemplate",
    () => vscode.commands.executeCommand("rumoca.renderTemplate"),
    commandMaxMs,
  );
  result.renderExecuteMs = Math.round(performance.now() - renderStart);

  writeResult(resultPath, result);
};
