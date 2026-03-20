import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const testDir = path.dirname(fileURLToPath(import.meta.url));
const extensionSourcePath = path.join(testDir, "..", "src", "extension.ts");

function readExtensionSource() {
  return fs.readFileSync(extensionSourcePath, "utf8");
}

function sliceFrom(source, startPattern, endPattern) {
  const startIndex = source.indexOf(startPattern);
  assert.notEqual(startIndex, -1, `missing start pattern: ${startPattern}`);
  const endIndex = source.indexOf(endPattern, startIndex);
  assert.notEqual(endIndex, -1, `missing end pattern after ${startPattern}: ${endPattern}`);
  return source.slice(startIndex, endIndex);
}

test("simulate command does not block on sidecar resync before starting the run", () => {
  const simulateCommandBlock = sliceFrom(
    readExtensionSource(),
    "const simulateCommand = vscode.commands.registerCommand('rumoca.simulateModel', async () => {",
    "context.subscriptions.push(simulateCommand);",
  );

  assert.equal(
    simulateCommandBlock.includes("await resyncProjectSidecars("),
    false,
    "simulate command should not synchronously await sidecar resync before invoking rumoca-lsp",
  );
  assert.equal(
    simulateCommandBlock.includes("getProjectSimulationConfig("),
    true,
    "simulate command should still load project simulation config before starting rumoca-lsp",
  );
});
