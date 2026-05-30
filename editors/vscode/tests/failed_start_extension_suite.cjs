const assert = require("node:assert/strict");
const fs = require("node:fs");
const { performance } = require("node:perf_hooks");

// Baked timeout budgets (previously RUMOCA_VSCODE_FAILED_START_*_MAX_MS env
// overrides that were never set).
const ACTIVATE_MAX_MS = 15000;
const COMMAND_MAX_MS = 10000;

// Read a required string workspace setting under `rumoca.benchmark.*` (the smoke
// runner writes these into the launched `.code-workspace`; no env vars).
function requiredBenchmarkSetting(vscode, key) {
  const value = vscode.workspace.getConfiguration("rumoca").get(key);
  if (typeof value !== "string" || value.length === 0) {
    throw new Error(`missing required workspace setting: rumoca.${key}`);
  }
  return value;
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

  const documentPath = requiredBenchmarkSetting(vscode, "benchmark.failedStart.document");
  const resultPath = requiredBenchmarkSetting(vscode, "benchmark.failedStart.result");
  const activateMaxMs = ACTIVATE_MAX_MS;
  const commandMaxMs = COMMAND_MAX_MS;
  const requiredCommands = [
    "rumoca.createScenarioConfig",
    "rumoca.simulateModel",
    "rumoca.openSimulationSettings",
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

  const settingsStart = performance.now();
  await withTimeout(
    "execute rumoca.openSimulationSettings",
    () => vscode.commands.executeCommand("rumoca.openSimulationSettings"),
    commandMaxMs,
  );
  result.settingsExecuteMs = Math.round(performance.now() - settingsStart);

  writeResult(resultPath, result);
};
