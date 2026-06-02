const assert = require("node:assert/strict");
const fs = require("node:fs");
const { performance } = require("node:perf_hooks");

// Per-phase timeout budgets for the smoke (baked constants; previously
// overridable via RUMOCA_VSCODE_SMOKE_*_MAX_MS env vars that were never set).
const ACTIVATE_MAX_MS = 15000;
const CODELENS_MAX_MS = 5000;
const COMPLETION_MAX_MS = 20000;
const HOVER_MAX_MS = 10000;
const DEFINITION_MAX_MS = 10000;

// Read a required string workspace setting under `rumoca.benchmark.*`. The
// smoke runner writes these into the `.code-workspace` it launches (no env var).
function requiredBenchmarkSetting(vscode, key) {
  const value = vscode.workspace.getConfiguration("rumoca").get(key);
  if (typeof value !== "string" || value.length === 0) {
    throw new Error(`missing required workspace setting: rumoca.${key}`);
  }
  return value;
}

function optionalPositiveNumberBenchmarkSetting(vscode, key, fallback) {
  const value = vscode.workspace.getConfiguration("rumoca").get(key);
  if (value === undefined || value === null) {
    return fallback;
  }
  if (typeof value === "number" && Number.isFinite(value) && value > 0) {
    return value;
  }
  throw new Error(`expected positive numeric workspace setting: rumoca.${key}`);
}

function labelText(label) {
  if (typeof label === "string") {
    return label;
  }
  if (label && typeof label === "object" && typeof label.label === "string") {
    return label.label;
  }
  return String(label ?? "");
}

const EXPECTED_MSL_COMPLETION_LABEL = "Electrical";

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
  const {
    collectDefinitionUris,
    collectHoverText,
    findMslCompletionProbe,
    findMslNavigationProbe,
  } = await import("./msl_extension_smoke_support.mjs");

  const documentPath = requiredBenchmarkSetting(vscode, "benchmark.smoke.document");
  const resultPath = requiredBenchmarkSetting(vscode, "benchmark.smoke.result");
  const activateMaxMs = optionalPositiveNumberBenchmarkSetting(
    vscode,
    "benchmark.smoke.activateMaxMs",
    ACTIVATE_MAX_MS,
  );
  const codeLensMaxMs = CODELENS_MAX_MS;
  const completionMaxMs = COMPLETION_MAX_MS;
  const hoverMaxMs = HOVER_MAX_MS;
  const definitionMaxMs = DEFINITION_MAX_MS;

  const extension = vscode.extensions.getExtension("JamesGoppert.rumoca-modelica");
  assert(extension, "Rumoca extension should be available in the extension host");

  const result = {};

  const activateStart = performance.now();
  await withTimeout("extension activation", () => extension.activate(), activateMaxMs);
  result.activateMs = Math.round(performance.now() - activateStart);

  const openStart = performance.now();
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
  result.openMs = Math.round(performance.now() - openStart);

  const sourceText = document.getText();
  const { probeText, probeOffset } = findMslCompletionProbe(sourceText);
  const probePosition = document.positionAt(probeOffset + probeText.length);
  const navigationProbe = findMslNavigationProbe(sourceText);
  const navigationPosition = document.positionAt(navigationProbe.probeOffset);

  const codeLensStart = performance.now();
  const codeLenses = await withTimeout(
    "executeCodeLensProvider",
    () => vscode.commands.executeCommand("vscode.executeCodeLensProvider", uri, 8),
    codeLensMaxMs,
  );
  result.codeLensMs = Math.round(performance.now() - codeLensStart);
  result.codeLensCount = Array.isArray(codeLenses) ? codeLenses.length : 0;

  const sourceRootLoadStart = performance.now();
  const sourceRootLoad = await withTimeout(
    "source-root load executeCompletionItemProvider",
    () => vscode.commands.executeCommand(
      "vscode.executeCompletionItemProvider",
      uri,
      probePosition,
    ),
    completionMaxMs,
  );
  result.sourceRootLoadMs = Math.round(performance.now() - sourceRootLoadStart);
  const sourceRootItems = Array.isArray(sourceRootLoad?.items) ? sourceRootLoad.items : [];
  result.sourceRootLoadCompletionCount = sourceRootItems.length;
  result.sourceRootExpectedCompletionPresent = sourceRootItems.some(
    (item) => labelText(item.label) === EXPECTED_MSL_COMPLETION_LABEL,
  );

  const completionStart = performance.now();
  const completion = await withTimeout(
    "cold executeCompletionItemProvider",
    () => vscode.commands.executeCommand(
      "vscode.executeCompletionItemProvider",
      uri,
      probePosition,
    ),
    completionMaxMs,
  );
  result.completionMs = Math.round(performance.now() - completionStart);
  const items = Array.isArray(completion?.items) ? completion.items : [];
  result.completionCount = items.length;
  result.expectedCompletionPresent = items.some(
    (item) => labelText(item.label) === EXPECTED_MSL_COMPLETION_LABEL,
  );

  const warmCompletionStart = performance.now();
  const warmCompletion = await withTimeout(
    "warm executeCompletionItemProvider",
    () => vscode.commands.executeCommand(
      "vscode.executeCompletionItemProvider",
      uri,
      probePosition,
    ),
    completionMaxMs,
  );
  result.warmCompletionMs = Math.round(performance.now() - warmCompletionStart);
  const warmItems = Array.isArray(warmCompletion?.items) ? warmCompletion.items : [];
  result.warmCompletionCount = warmItems.length;
  result.warmExpectedCompletionPresent = warmItems.some(
    (item) => labelText(item.label) === EXPECTED_MSL_COMPLETION_LABEL,
  );

  const hoverStart = performance.now();
  const hoverResults = await withTimeout(
    "executeHoverProvider",
    () => vscode.commands.executeCommand("vscode.executeHoverProvider", uri, navigationPosition),
    hoverMaxMs,
  );
  result.hoverMs = Math.round(performance.now() - hoverStart);
  result.hoverCount = Array.isArray(hoverResults) ? hoverResults.length : 0;
  const hoverText = collectHoverText(hoverResults);
  result.expectedHoverPresent = hoverText.includes(navigationProbe.expectedLabel);

  const definitionStart = performance.now();
  const definitionResults = await withTimeout(
    "executeDefinitionProvider",
    () =>
      vscode.commands.executeCommand("vscode.executeDefinitionProvider", uri, navigationPosition),
    definitionMaxMs,
  );
  result.definitionMs = Math.round(performance.now() - definitionStart);
  const definitionUris = collectDefinitionUris(definitionResults);
  result.definitionCount = definitionUris.length;
  result.expectedDefinitionPresent = definitionUris.length > 0;
  result.crossFileDefinitionPresent = definitionUris.some((target) => target !== uri.toString());

  writeResult(resultPath, result);

  assert(
    result.sourceRootExpectedCompletionPresent,
    `expected initial MSL completion items to include ${EXPECTED_MSL_COMPLETION_LABEL}, got ${sourceRootItems
      .slice(0, 20)
      .map((item) => labelText(item.label))
      .join(", ")}`,
  );
  assert(
    result.expectedCompletionPresent,
    `expected MSL completion items to include ${EXPECTED_MSL_COMPLETION_LABEL}, got ${items
      .slice(0, 20)
      .map((item) => labelText(item.label))
      .join(", ")}`,
  );
  assert(
    result.warmExpectedCompletionPresent,
    `expected warm MSL completion items to include ${EXPECTED_MSL_COMPLETION_LABEL}, got ${warmItems
      .slice(0, 20)
      .map((item) => labelText(item.label))
      .join(", ")}`,
  );
  assert(
    result.expectedHoverPresent,
    `expected hover text to include ${navigationProbe.expectedLabel}, got ${hoverText}`,
  );
  assert(
    result.expectedDefinitionPresent,
    `expected goto-definition to return at least one target for ${navigationProbe.expectedLabel}`,
  );
  assert(
    result.crossFileDefinitionPresent,
    `expected goto-definition to leave the active document for ${navigationProbe.expectedLabel}`,
  );
};
