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

  const documentPath = envPath("RUMOCA_VSCODE_INSTALL_CHECK_DOCUMENT");
  const resultPath = envPath("RUMOCA_VSCODE_INSTALL_CHECK_RESULT");
  const activateMaxMs = envMs("RUMOCA_VSCODE_INSTALL_CHECK_ACTIVATE_MAX_MS", 15000);
  const commandMaxMs = envMs("RUMOCA_VSCODE_INSTALL_CHECK_COMMAND_MAX_MS", 10000);
  const requiredCommands = [
    "rumoca.simulateModel",
    "rumoca.renderTemplate",
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
  result.render = await executeCommand("rumoca.renderTemplate");

  writeResult(resultPath, result);
};
