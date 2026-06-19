import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const testDir = path.dirname(fileURLToPath(import.meta.url));
const extensionSourcePath = path.join(testDir, "..", "src", "extension.ts");
const simulateCommandStartPattern =
  "const simulateCommand = vscode.commands.registerCommand('rumoca.simulateModel', async (resource?: vscode.Uri) => {";

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

test("simulate command has no generated config resync dependency before starting the run", () => {
  const simulateCommandBlock = sliceFrom(
    readExtensionSource(),
    simulateCommandStartPattern,
    "context.subscriptions.push(simulateCommand);",
  );

  assert.equal(
    simulateCommandBlock.includes("resyncGeneratedConfig"),
    false,
    "simulate command should not depend on removed generated config resync plumbing",
  );
  assert.equal(
    simulateCommandBlock.includes("getScenarioSimulationConfig("),
    true,
    "simulate command should still load scenario simulation config before starting rumoca-lsp",
  );
});

test("simulate command publishes failures to Problems diagnostics", () => {
  const source = readExtensionSource();
  const simulateCommandBlock = sliceFrom(
    source,
    simulateCommandStartPattern,
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

test("input-enabled results-panel scenarios stay on batch results path", () => {
  const source = readExtensionSource();
  const predicateBlock = sliceFrom(
    source,
    "function scenarioNeedsInputRunner(",
    "const DEFAULT_CODEGEN_TARGET_ID",
  );

  assert.equal(
    predicateBlock.includes("scenario.viewerMode === 'external_web'"),
    true,
    "external viewer routing should be selected by viewerMode",
  );
  assert.equal(
    predicateBlock.includes("inputEnabled"),
    false,
    "input routing alone should not bypass the results-panel simulation path",
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

test("interactive viewer launch does not pass unsupported port flags", () => {
  const source = readExtensionSource();
  const startInteractiveScenarioBlock = sliceFrom(
    source,
    "const startInteractiveScenario = async (",
    "const wireResultsPanelMessageHandling = (panel: vscode.WebviewPanel) => {",
  );

  assert.equal(
    startInteractiveScenarioBlock.includes("--http-port"),
    false,
    "rumoca sim --config reads HTTP transport from the scenario file",
  );
  assert.equal(
    startInteractiveScenarioBlock.includes("--ws-port"),
    false,
    "rumoca sim --config reads WebSocket transport from the scenario file",
  );
  assert.equal(
    startInteractiveScenarioBlock.includes("document.uri.fsPath"),
    true,
    "interactive runs should still launch the selected scenario config",
  );
});

test("interactive viewer launch reports actionable process failures", () => {
  const source = readExtensionSource();
  const startInteractiveScenarioBlock = sliceFrom(
    source,
    "const startInteractiveScenario = async (",
    "const wireResultsPanelMessageHandling = (panel: vscode.WebviewPanel) => {",
  );

  assert.equal(
    source.includes("function summarizeInteractiveFailure("),
    true,
    "interactive process output should be summarized before showing failure messages",
  );
  assert.equal(
    source.includes("Address already in use"),
    true,
    "port conflicts should be recognized as a first-class interactive failure",
  );
  assert.equal(
    source.includes("source-root path does not exist"),
    true,
    "missing Modelica dependency roots should be recognized as a first-class interactive failure",
  );
  assert.equal(
    startInteractiveScenarioBlock.includes("rememberInteractiveOutput(recentOutputLines, text);"),
    true,
    "interactive runs should keep recent output for failure messages",
  );
  assert.equal(
    startInteractiveScenarioBlock.includes("setSimulationDiagnostic(document, model, message);"),
    true,
    "interactive run failures should be published to Problems diagnostics",
  );
  assert.equal(
    startInteractiveScenarioBlock.includes("closeEmitter.fire(0);"),
    true,
    "normal rumoca process exits should avoid VS Code's misleading terminal-launch failure toast",
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
    simulateCommandStartPattern,
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

test("scenario toolbar commands honor the menu resource URI", () => {
  const source = readExtensionSource();
  const simulateCommandBlock = sliceFrom(
    source,
    simulateCommandStartPattern,
    "context.subscriptions.push(simulateCommand);",
  );
  const simulationSettingsCommandBlock = sliceFrom(
    source,
    "const simulationSettingsCommand = vscode.commands.registerCommand(",
    "context.subscriptions.push(simulationSettingsCommand);",
  );

  assert.equal(
    source.includes("async function scenarioDocumentFromCommandResource("),
    true,
    "extension should resolve a scenario document from the editor title resource URI",
  );
  assert.equal(
    simulateCommandBlock.includes("scenarioDocumentFromCommandResource(resource)"),
    true,
    "run command should use the menu resource URI before falling back to the active editor",
  );
  assert.equal(
    simulationSettingsCommandBlock.includes("scenarioDocumentFromCommandResource(resource)"),
    true,
    "settings command should use the menu resource URI before falling back to the active editor",
  );
});
