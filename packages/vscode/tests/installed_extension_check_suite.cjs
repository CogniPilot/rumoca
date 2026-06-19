const assert = require("node:assert/strict");
const fs = require("node:fs");
const { performance } = require("node:perf_hooks");

// Baked timeout budgets (previously RUMOCA_VSCODE_INSTALL_CHECK_*_MAX_MS env
// overrides that were never set).
const ACTIVATE_MAX_MS = 15000;
const COMMAND_MAX_MS = 10000;

// Read a required string workspace setting under `rumoca.benchmark.*` (the
// install-check runner writes these into the launched workspace; no env vars).
function requiredBenchmarkSetting(vscode, key) {
  const value = vscode.workspace.getConfiguration("rumoca").get(key);
  if (typeof value !== "string" || value.length === 0) {
    throw new Error(`missing required workspace setting: rumoca.${key}`);
  }
  return value;
}

function sleep(ms) {
  return new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
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

function commandNotFoundMessage(commandId, error) {
  const message = String(error?.message ?? error ?? "");
  if (message.includes(`command '${commandId}' not found`)) {
    return message;
  }
  return null;
}

exports.run = async function run() {
  const vscode = require("vscode");

  const documentPath = requiredBenchmarkSetting(vscode, "benchmark.installCheck.document");
  const resultPath = requiredBenchmarkSetting(vscode, "benchmark.installCheck.result");
  const activateMaxMs = ACTIVATE_MAX_MS;
  const commandMaxMs = COMMAND_MAX_MS;
  const requiredCommands = [
    "rumoca.createScenarioConfig",
    "rumoca.simulateModel",
    "rumoca.openSimulationSettings",
    "rumoca.openSettingsMenu",
  ];

  function rumocaExtension() {
    return vscode.extensions.getExtension("JamesGoppert.rumoca-modelica");
  }

  assert(rumocaExtension(), "Installed Rumoca extension should be available in the extension host");

  const result = {};
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

  const activateStart = performance.now();
  await withTimeout(
    "installed extension activation on .mo open",
    async () => {
      while (!rumocaExtension()?.isActive) {
        await sleep(100);
      }
    },
    activateMaxMs,
  );
  result.activateMs = Math.round(performance.now() - activateStart);
  result.extensionActive = rumocaExtension()?.isActive ?? false;
  assert.equal(
    result.extensionActive,
    true,
    "Installed Rumoca extension should activate when a .mo file is opened",
  );

  const commands = await withTimeout(
    "get installed rumoca command inventory",
    async () => {
      while (true) {
        const inventory = await vscode.commands.getCommands(true);
        if (requiredCommands.every((command) => inventory.includes(command))) {
          return inventory;
        }
        await sleep(100);
      }
    },
    commandMaxMs,
  );
  result.registeredCommands = requiredCommands.filter((command) => commands.includes(command));
  assert.deepEqual(
    result.registeredCommands,
    requiredCommands,
    "Installed Rumoca commands should remain registered after failed initial language-server startup",
  );

  async function executeCommand(commandId) {
    const start = performance.now();
    let errorMessage = null;
    try {
      await withTimeout(
        `execute ${commandId}`,
        () => vscode.commands.executeCommand(commandId),
        commandMaxMs,
      );
    } catch (error) {
      const missing = commandNotFoundMessage(commandId, error);
      assert.equal(
        missing,
        null,
        `Installed command ${commandId} should not fail with command-not-found`,
      );
      errorMessage = String(error?.message ?? error ?? "");
    }
    return {
      durationMs: Math.round(performance.now() - start),
      errorMessage,
    };
  }

  result.simulate = await executeCommand("rumoca.simulateModel");
  result.settings = await executeCommand("rumoca.openSimulationSettings");

  writeResult(resultPath, result);
};
