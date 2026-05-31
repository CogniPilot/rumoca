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

test("simulate command has no sidecar resync dependency before starting the run", () => {
  const simulateCommandBlock = sliceFrom(
    readExtensionSource(),
    "const simulateCommand = vscode.commands.registerCommand('rumoca.simulateModel', async () => {",
    "context.subscriptions.push(simulateCommand);",
  );

  assert.equal(
    simulateCommandBlock.includes("resyncProjectSidecars"),
    false,
    "simulate command should not depend on removed sidecar resync plumbing",
  );
  assert.equal(
    simulateCommandBlock.includes("getProjectSimulationConfig("),
    true,
    "simulate command should still load project simulation config before starting rumoca-lsp",
  );
});

test("simulate command publishes failures to Problems diagnostics", () => {
  const source = readExtensionSource();
  const simulateCommandBlock = sliceFrom(
    source,
    "const simulateCommand = vscode.commands.registerCommand('rumoca.simulateModel', async () => {",
    "context.subscriptions.push(simulateCommand);",
  );

  assert.equal(
    source.includes("vscode.languages.createDiagnosticCollection('rumoca simulation')"),
    true,
    "extension should own a simulation diagnostic collection for Problems panel entries",
  );
  assert.equal(
    source.includes("const setSimulationDiagnostic = ("),
    true,
    "extension should expose a helper that records simulation failures as diagnostics",
  );
  assert.equal(
    simulateCommandBlock.includes("setSimulationDiagnostic(document, model, details, result.diagnostic);"),
    true,
    "nonzero rumoca-lsp simulation results should be published as diagnostics",
  );
  assert.equal(
    simulateCommandBlock.includes("setSimulationDiagnostic(document, model, message);"),
    true,
    "thrown simulation command errors should be published as diagnostics",
  );
  assert.equal(
    simulateCommandBlock.includes("clearSimulationDiagnostic(document);"),
    true,
    "simulation diagnostics should be cleared when a new run starts or succeeds",
  );
});

test("interactive viewer launch waits for runner ready output", () => {
  const source = readExtensionSource();
  const startInteractiveScenarioBlock = sliceFrom(
    source,
    "const startInteractiveScenario = async (",
    "const wireResultsPanelMessageHandling = (panel: vscode.WebviewPanel) => {",
  );

  assert.equal(
    source.includes("rumoca-viewer-ready"),
    true,
    "interactive scenario process should wait for the runner's machine-readable ready line",
  );
  assert.equal(
    startInteractiveScenarioBlock.includes("LIVE_VIEWER_READY_PREFIX"),
    true,
    "viewer launch should wait for the runner ready line before opening the page",
  );
  assert.equal(
    startInteractiveScenarioBlock.includes("vscode.window.createTerminal({"),
    true,
    "interactive runs should still be surfaced as a VS Code terminal",
  );
  assert.equal(
    startInteractiveScenarioBlock.includes("setTimeout("),
    false,
    "viewer launch should not use a fixed delay before opening the page",
  );
});

test("viewer prefer_external controls embedded versus browser presentation", () => {
  const source = readExtensionSource();
  const startInteractiveScenarioBlock = sliceFrom(
    source,
    "const startInteractiveScenario = async (",
    "const wireResultsPanelMessageHandling = (panel: vscode.WebviewPanel) => {",
  );
  const simulateCommandBlock = sliceFrom(
    source,
    "const simulateCommand = vscode.commands.registerCommand('rumoca.simulateModel', async () => {",
    "context.subscriptions.push(simulateCommand);",
  );

  assert.equal(
    source.includes("viewerPreferExternal?: boolean;"),
    true,
    "scenario config response should expose viewerPreferExternal",
  );
  assert.equal(
    startInteractiveScenarioBlock.includes("scenario.viewerPreferExternal === true"),
    true,
    "interactive web viewer should respect prefer_external",
  );
  assert.equal(
    source.includes("simpleBrowser.show"),
    true,
    "embedded live viewers should use VS Code Simple Browser instead of a custom iframe by default",
  );
  assert.equal(
    simulateCommandBlock.includes("openResultsExternalForRun("),
    true,
    "batch plotting should open externally when prefer_external is true",
  );
  assert.equal(
    simulateCommandBlock.includes("openResultsPanelForRun("),
    true,
    "batch plotting should still default to the embedded VS Code results panel",
  );
});
