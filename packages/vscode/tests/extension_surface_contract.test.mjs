import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const testDir = path.dirname(fileURLToPath(import.meta.url));
const extensionRoot = path.resolve(testDir, "..");
const packageJsonPath = path.join(extensionRoot, "package.json");
const extensionSourcePath = path.join(extensionRoot, "src", "extension.ts");
const sharedVisualizationSourcePath = path.join(extensionRoot, "media", "vendor", "visualization_shared.js");

const COMMAND_CONTRACT_REF =
  "runtime:tests/extension_surface_contract.test.mjs#surface contract: contributed commands are registered and covered";
const ACTIVATION_CONTRACT_REF =
  "runtime:tests/extension_surface_contract.test.mjs#surface contract: activation commands stay aligned with contributed commands";
const REGISTRATION_CONTRACT_REF =
  "runtime:tests/extension_surface_contract.test.mjs#surface contract: extension registrations and hooks are inventory-mapped";
const LANGUAGE_CLIENT_RUNTIME_REF =
  "runtime:tests/language_client_runtime.test.mjs#configuration restart stops the old client and updates the notebook executable";
const NOTEBOOK_CONTROLLER_RUNTIME_REF =
  "runtime:tests/notebook_controller_runtime.test.mjs#configuration changes can create the notebook controller after an initial missing executable";

const SURFACE_COVERAGE = {
  lifecycleHooks: {
    activate: [ACTIVATION_CONTRACT_REF],
    deactivate: [REGISTRATION_CONTRACT_REF],
  },
  packageCommands: {
    "rumoca.collapseAllAnnotations": [COMMAND_CONTRACT_REF],
    "rumoca.createScenarioConfig": [COMMAND_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.expandAllAnnotations": [COMMAND_CONTRACT_REF],
    "rumoca.openDevGuide": [COMMAND_CONTRACT_REF],
    "rumoca.openPlayground": [COMMAND_CONTRACT_REF],
    "rumoca.openSettingsMenu": [COMMAND_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.openSimulationSettings": [COMMAND_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.openUserGuide": [COMMAND_CONTRACT_REF],
    "rumoca.simulateModel": [COMMAND_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.toggleAnnotation": [COMMAND_CONTRACT_REF],
  },
  activationCommands: {
    "rumoca.createScenarioConfig": [ACTIVATION_CONTRACT_REF],
    "rumoca.openSettingsMenu": [ACTIVATION_CONTRACT_REF],
    "rumoca.openSimulationSettings": [ACTIVATION_CONTRACT_REF],
    "rumoca.simulateModel": [ACTIVATION_CONTRACT_REF],
  },
  customEditors: {
    "rumoca.results": [REGISTRATION_CONTRACT_REF],
    "rumoca.scenarioConfig": [REGISTRATION_CONTRACT_REF],
  },
  customEditorActivations: {
    "rumoca.results": [REGISTRATION_CONTRACT_REF],
    "rumoca.scenarioConfig": [REGISTRATION_CONTRACT_REF],
  },
  customEditorProviders: {
    "rumoca.results": [REGISTRATION_CONTRACT_REF],
    "rumoca.scenarioConfig": [REGISTRATION_CONTRACT_REF],
  },
  extensionCommands: {
    "rumoca.collapseAllAnnotations": [REGISTRATION_CONTRACT_REF],
    "rumoca.createScenarioConfig": [REGISTRATION_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.expandAllAnnotations": [REGISTRATION_CONTRACT_REF],
    "rumoca.openDevGuide": [REGISTRATION_CONTRACT_REF],
    "rumoca.openPlayground": [REGISTRATION_CONTRACT_REF],
    "rumoca.openSettingsMenu": [REGISTRATION_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.openSimulationSettings": [REGISTRATION_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.openUserGuide": [REGISTRATION_CONTRACT_REF],
    "rumoca.simulateModel": [REGISTRATION_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.toggleAnnotation": [REGISTRATION_CONTRACT_REF],
  },
  menuEntries: {
    "editor/title:rumoca.createScenarioConfig": [COMMAND_CONTRACT_REF],
    "editor/title:rumoca.openSimulationSettings": [COMMAND_CONTRACT_REF],
    "editor/title/run:rumoca.simulateModel": [COMMAND_CONTRACT_REF],
  },
  serializers: {
    rumocaResults: [REGISTRATION_CONTRACT_REF],
  },
  workspaceHooks: {
    "workspace.onDidChangeConfiguration": [
      REGISTRATION_CONTRACT_REF,
      LANGUAGE_CLIENT_RUNTIME_REF,
      NOTEBOOK_CONTROLLER_RUNTIME_REF,
    ],
    "workspace.onDidChangeTextDocument": [REGISTRATION_CONTRACT_REF],
    "workspace.onDidOpenTextDocument": [REGISTRATION_CONTRACT_REF],
    "workspace.onDidRenameFiles": [REGISTRATION_CONTRACT_REF],
    "workspace.onDidSaveTextDocument": [REGISTRATION_CONTRACT_REF],
  },
  contentProviders: {
    "workspace.registerTextDocumentContentProvider:EMBEDDED_MODELICA_SCHEME": [
      REGISTRATION_CONTRACT_REF,
    ],
  },
  languageProviders: {
    "languages.registerCompletionItemProvider:python@vscode-notebook-cell": [
      REGISTRATION_CONTRACT_REF,
    ],
    "languages.registerHoverProvider:python@vscode-notebook-cell": [
      REGISTRATION_CONTRACT_REF,
    ],
  },
  windowHooks: {
    "window.onDidChangeActiveTextEditor": [REGISTRATION_CONTRACT_REF],
    "window.onDidChangeTextEditorSelection": [REGISTRATION_CONTRACT_REF],
  },
  resultsWebviewMessages: {
    "results.response": [REGISTRATION_CONTRACT_REF],
  },
  scenarioConfigWebviewMessages: {
    "scenarioConfig.request": [REGISTRATION_CONTRACT_REF],
    "scenarioConfig.response": [REGISTRATION_CONTRACT_REF],
  },
  fileSystemWatchers: {
    "workspace.createFileSystemWatcher:**/*.mo": [REGISTRATION_CONTRACT_REF],
  },
  fileSystemWatcherHooks: {},
};

function readPackageJson() {
  return JSON.parse(fs.readFileSync(packageJsonPath, "utf8"));
}

function readExtensionSource() {
  return fs.readFileSync(extensionSourcePath, "utf8");
}

function readSharedVisualizationSource() {
  return fs.readFileSync(sharedVisualizationSourcePath, "utf8");
}

function sortUnique(values) {
  return [...new Set(values)].sort();
}

function escapeRegex(value) {
  return value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

function collectMatches(source, regex, projector) {
  const values = [];
  for (const match of source.matchAll(regex)) {
    values.push(projector(match));
  }
  return sortUnique(values);
}

function sliceFrom(source, startPattern, endPattern) {
  const startIndex = source.indexOf(startPattern);
  assert.notEqual(startIndex, -1, `missing start pattern: ${startPattern}`);
  const endIndex = source.indexOf(endPattern, startIndex);
  assert.notEqual(endIndex, -1, `missing end pattern after ${startPattern}: ${endPattern}`);
  return source.slice(startIndex, endIndex);
}

function inventoryPackageSurface(packageJson) {
  const menuEntries = [];
  for (const [menuId, entries] of Object.entries(packageJson.contributes?.menus ?? {})) {
    for (const entry of entries ?? []) {
      if (typeof entry?.command === "string" && entry.command.length > 0) {
        menuEntries.push(`${menuId}:${entry.command}`);
      }
    }
  }
  return {
    packageCommands: sortUnique(
      (packageJson.contributes?.commands ?? [])
        .map((entry) => entry.command)
        .filter((value) => typeof value === "string" && value.length > 0),
    ),
    activationCommands: sortUnique(
      (packageJson.activationEvents ?? [])
        .filter((value) => typeof value === "string" && value.startsWith("onCommand:"))
        .map((value) => value.slice("onCommand:".length)),
    ),
    customEditorActivations: sortUnique(
      (packageJson.activationEvents ?? [])
        .filter((value) => typeof value === "string" && value.startsWith("onCustomEditor:"))
        .map((value) => value.slice("onCustomEditor:".length)),
    ),
    customEditors: sortUnique(
      (packageJson.contributes?.customEditors ?? [])
        .map((entry) => entry.viewType)
        .filter((value) => typeof value === "string" && value.length > 0),
    ),
    menuEntries: sortUnique(menuEntries),
  };
}

function inventoryExtensionSurface(source) {
  const sharedVisualizationSource = readSharedVisualizationSource();
  const watcherBindings = [...source.matchAll(
    /const\s+([A-Za-z_$][A-Za-z0-9_$]*)\s*=\s*vscode\.workspace\.createFileSystemWatcher\(\s*'([^']+)'\s*\);/g,
  )].map((match) => ({
    name: match[1],
    glob: match[2],
  }));
  const resultsWebviewSource = sliceFrom(
    sharedVisualizationSource,
    "function buildHostedResultsDocument(args) {",
    "function buildSimulationRunDocument(args) {",
  );
  const resultsWebviewHandler = sliceFrom(
    source,
    "const wireResultsPanelMessageHandling = (panel: vscode.WebviewPanel) => {",
    "const resultsWebviewLocalRoots = [",
  );
  const scenarioConfigProvider = sliceFrom(
    source,
    "const scenarioEditorProvider: vscode.CustomTextEditorProvider = {",
    "const simulationDiagnostics = vscode.languages.createDiagnosticCollection('rumoca simulation');",
  );

  return {
    lifecycleHooks: sortUnique(
      collectMatches(
        source,
        /export async function (activate|deactivate)\(/g,
        (match) => match[1],
      ),
    ),
    extensionCommands: collectMatches(
      source,
      /vscode\.commands\.registerCommand\(\s*'([^']+)'/g,
      (match) => match[1],
    ),
    serializers: collectMatches(
      source,
      /vscode\.window\.registerWebviewPanelSerializer\(\s*'([^']+)'/g,
      (match) => match[1],
    ),
    customEditorProviders: collectMatches(
      source,
      /vscode\.window\.registerCustomEditorProvider\(\s*'([^']+)'/g,
      (match) => match[1],
    ),
    workspaceHooks: collectMatches(
      source,
      /vscode\.workspace\.(onDid(?:ChangeConfiguration|ChangeTextDocument|OpenTextDocument|RenameFiles|CreateFiles|DeleteFiles|SaveTextDocument))\(/g,
      (match) => `workspace.${match[1]}`,
    ),
    contentProviders: collectMatches(
      source,
      /vscode\.workspace\.registerTextDocumentContentProvider\(\s*([^,\n)]+)/g,
      (match) => `workspace.registerTextDocumentContentProvider:${match[1].trim()}`,
    ),
    languageProviders: collectMatches(
      source,
      /vscode\.languages\.(register(?:Hover|CompletionItem)Provider)\(\s*\{\s*language:\s*'([^']+)'\s*,\s*scheme:\s*'([^']+)'\s*\}/gs,
      (match) => `languages.${match[1]}:${match[2]}@${match[3]}`,
    ),
    windowHooks: collectMatches(
      source,
      /vscode\.window\.(onDid(?:ChangeActiveTextEditor|ChangeTextEditorSelection))\(/g,
      (match) => `window.${match[1]}`,
    ),
    resultsWebviewMessages: collectMatches(
      `${resultsWebviewHandler}\n${resultsWebviewSource}`,
      /command (?:===|!==) '([^']+)'/g,
      (match) => match[1].startsWith("results.") ? match[1] : `results.${match[1]}`,
    ),
    scenarioConfigWebviewMessages: collectMatches(
      scenarioConfigProvider,
      /(?:command:\s*|message\.command !== )'([^']+)'/g,
      (match) => match[1],
    ),
    fileSystemWatchers: sortUnique(
      watcherBindings.map((binding) => `workspace.createFileSystemWatcher:${binding.glob}`),
    ),
    fileSystemWatcherHooks: sortUnique(
      watcherBindings.flatMap((binding) =>
        collectMatches(
          source,
          new RegExp(`\\b${escapeRegex(binding.name)}\\.(onDid(?:Create|Delete|Change))\\(`, "g"),
          (match) => `${binding.name}.${match[1]}`,
        ),
      ),
    ),
  };
}

function assertCoverageReferences(category, coverageMap) {
  for (const [surfaceKey, refs] of Object.entries(coverageMap)) {
    assert.ok(Array.isArray(refs) && refs.length > 0, `${category}:${surfaceKey} must list coverage refs`);
    for (const ref of refs) {
      const match = /^(runtime|smoke):([^#]+)(?:#.+)?$/.exec(ref);
      assert.ok(match, `${category}:${surfaceKey} has invalid coverage ref: ${ref}`);
      const refPath = path.join(extensionRoot, match[2]);
      assert.ok(fs.existsSync(refPath), `${category}:${surfaceKey} points to missing coverage file: ${ref}`);
    }
  }
}

function assertInventory(category, actualValues, expectedCoverageMap) {
  const expectedValues = Object.keys(expectedCoverageMap).sort();
  assert.deepEqual(
    actualValues,
    expectedValues,
    `${category} inventory drifted; update tests/extension_surface_contract.test.mjs`,
  );
  assertCoverageReferences(category, expectedCoverageMap);
}

test("surface contract: contributed commands are registered and covered", () => {
  const packageSurface = inventoryPackageSurface(readPackageJson());
  const extensionSurface = inventoryExtensionSurface(readExtensionSource());

  assertInventory("packageCommands", packageSurface.packageCommands, SURFACE_COVERAGE.packageCommands);
  assert.deepEqual(
    packageSurface.packageCommands,
    packageSurface.packageCommands.filter((command) => extensionSurface.extensionCommands.includes(command)),
    "every contributed command should also be registered in src/extension.ts",
  );
});

test("surface contract: activation commands stay aligned with contributed commands", () => {
  const packageSurface = inventoryPackageSurface(readPackageJson());

  assertInventory(
    "activationCommands",
    packageSurface.activationCommands,
    SURFACE_COVERAGE.activationCommands,
  );
  assert.deepEqual(
    packageSurface.activationCommands,
    packageSurface.activationCommands.filter((command) =>
      packageSurface.packageCommands.includes(command),
    ),
    "every onCommand activation event should refer to a contributed command",
  );
});

test("surface contract: scenario files open the shared custom editor by default", () => {
  const packageJson = readPackageJson();
  const packageSurface = inventoryPackageSurface(packageJson);
  const extensionSurface = inventoryExtensionSurface(readExtensionSource());

  assertInventory("customEditors", packageSurface.customEditors, SURFACE_COVERAGE.customEditors);
  assertInventory(
    "customEditorActivations",
    packageSurface.customEditorActivations,
    SURFACE_COVERAGE.customEditorActivations,
  );
  assertInventory(
    "customEditorProviders",
    extensionSurface.customEditorProviders,
    SURFACE_COVERAGE.customEditorProviders,
  );
  const editor = packageJson.contributes?.customEditors?.find(
    (entry) => entry.viewType === "rumoca.scenarioConfig",
  );
  assert.equal(editor?.priority, "default", "scenario files should open the shared GUI by default");
  assert.deepEqual(
    (editor?.selector ?? []).map((entry) => entry.filenamePattern).sort(),
    ["rumoca-scenario.*.toml", "rumoca-scenario.toml"],
    "scenario custom editor should target only rumoca-scenario.toml / rumoca-scenario.<profile>.toml files",
  );
});

test("surface contract: modelica files activate the extension", () => {
  const packageJson = readPackageJson();

  assert.ok(
    Array.isArray(packageJson.activationEvents)
      && packageJson.activationEvents.includes("onLanguage:modelica"),
    "opening a .mo file should activate the extension so LSP features like code lens can appear",
  );
});

test("surface contract: galec (.alg) files activate the extension", () => {
  const packageJson = readPackageJson();

  assert.ok(
    Array.isArray(packageJson.activationEvents)
      && packageJson.activationEvents.includes("onLanguage:galec"),
    "opening a .alg file should activate the extension so GALEC LSP diagnostics appear",
  );
  const galec = (packageJson.contributes?.languages ?? []).find((entry) => entry?.id === "galec");
  assert.ok(galec, "expected a galec language contribution");
  assert.ok(
    (galec.extensions ?? []).includes(".alg"),
    "galec language should own the .alg extension",
  );
  const galecGrammar = (packageJson.contributes?.grammars ?? []).find(
    (entry) => entry?.language === "galec",
  );
  assert.ok(galecGrammar, "expected a galec TextMate grammar for .alg highlighting");
});

test("surface contract: rumoca-scenario.toml scenario files activate as toml", () => {
  const packageJson = readPackageJson();
  const activationEvents = packageJson.activationEvents ?? [];

  // Scenarios are `rumoca-scenario.toml` / `rumoca-scenario.<task>.toml`,
  // ordinary `.toml` files — not a bespoke `.rum` extension. They activate
  // through the built-in TOML language so TOML extensions stay active.
  assert.ok(
    Array.isArray(activationEvents) && activationEvents.includes("onLanguage:toml"),
    "opening a rumoca-scenario.toml scenario should activate the extension through the TOML language",
  );
  assert.ok(
    activationEvents.includes("workspaceContains:**/rumoca-scenario.toml")
      && activationEvents.includes("workspaceContains:**/rumoca-scenario.*.toml"),
    "a workspace containing rumoca-scenario.toml / rumoca-scenario.<task>.toml scenarios should activate the extension",
  );
  assert.equal(
    (packageJson.contributes?.languages ?? []).some((entry) => entry.id === "toml"),
    false,
    "the extension must not redefine the TOML language; rumoca-scenario.toml uses the standard .toml extension",
  );
});

test("surface contract: packaged VSIX bundles vscode-languageclient runtime", () => {
  const packageJson = readPackageJson();
  const esbuildBase = packageJson.scripts?.["esbuild-base"];

  assert.equal(typeof esbuildBase, "string", "package.json scripts.esbuild-base must exist");
  assert.equal(
    esbuildBase.includes("--external:vscode-languageclient"),
    false,
    "the packaged extension must bundle vscode-languageclient because the VSIX ships without node_modules",
  );
});

test("surface contract: editor title command placement stays aligned with the toolbar layout", () => {
  const packageSurface = inventoryPackageSurface(readPackageJson());

  assertInventory("menuEntries", packageSurface.menuEntries, SURFACE_COVERAGE.menuEntries);
});

test("surface contract: settings commands route to the scenario or model settings surface", () => {
  const source = readExtensionSource();
  const settingsMenuCommandBlock = sliceFrom(
    source,
    "const settingsMenuCommand = vscode.commands.registerCommand(",
    "context.subscriptions.push(settingsMenuCommand);",
  );
  const simulationSettingsCommandBlock = sliceFrom(
    source,
    "const simulationSettingsCommand = vscode.commands.registerCommand(",
    "context.subscriptions.push(simulationSettingsCommand);",
  );
  const createScenarioCommandBlock = sliceFrom(
    source,
    "const createScenarioConfigCommand = vscode.commands.registerCommand(",
    "context.subscriptions.push(createScenarioConfigCommand);",
  );
  assert.ok(
    !settingsMenuCommandBlock.includes("vscode.window.showQuickPick("),
    "shared settings command should no longer present a picker",
  );
  assert.ok(
    settingsMenuCommandBlock.includes("await openScenarioConfigEditor(document);")
      && settingsMenuCommandBlock.includes("await vscode.commands.executeCommand('rumoca.createScenarioConfig');"),
    "shared settings command should open or create a scenario file in the shared scenario editor",
  );
  assert.ok(
    simulationSettingsCommandBlock.includes("await openScenarioConfigEditor(document);")
      && !simulationSettingsCommandBlock.includes("openUnifiedSettingsForEditor("),
    "scenario settings command should open the shared scenario custom editor",
  );
  assert.ok(
    source.includes("await openScenarioConfigEditor(scenarioDocument);")
      && !createScenarioCommandBlock.includes("showQuickPick(")
      && !createScenarioCommandBlock.includes("showInputBox("),
    "scenario creation should open the shared editor without prompt-heavy task/name pickers",
  );
});

test("surface contract: scenario creation renders TOML through LSP", () => {
  const source = readExtensionSource();
  const createScenarioCommandBlock = sliceFrom(
    source,
    "const createScenarioConfigCommand = vscode.commands.registerCommand(",
    "context.subscriptions.push(createScenarioConfigCommand);",
  );

  assert.equal(
    source.includes("function buildScenarioToml("),
    false,
    "VS Code scenario creation should not hand-build TOML strings",
  );
  assert.equal(
    source.includes("function tomlQuotedString(") || source.includes("function tomlStringArray("),
    false,
    "VS Code scenario creation should use shared Rust TOML rendering",
  );
  assert.ok(
    source.includes("rumoca.scenario.renderScenarioConfig"),
    "extension should call the LSP scenario config renderer",
  );
  assert.ok(
    createScenarioCommandBlock.includes("await renderScenarioConfig(buildScenarioConfig({"),
    "create scenario command should render the JSON config through LSP before writing",
  );
  assert.ok(
    createScenarioCommandBlock.includes("vscode.window.showSaveDialog("),
    "create scenario command should let the user choose the scenario file path through the editor save UI",
  );
});

test("surface contract: VS Code has shared scenario GUI command primitives", () => {
  const source = readExtensionSource();
  const sharedVisualizationSource = readSharedVisualizationSource();

  assert.ok(
    sharedVisualizationSource.includes("function buildScenarioConfigDocument(scenario)"),
    "shared web helpers should expose the scenario config GUI document",
  );
  assert.ok(
    sharedVisualizationSource.includes("buildScenarioConfigDocument,"),
    "shared scenario config GUI should be exported to hosts",
  );
  assert.ok(
    source.includes("rumoca.scenario.getScenarioConfigFull"),
    "VS Code should be able to load full scenario config trees through LSP",
  );
  assert.ok(
    source.includes("rumoca.scenario.setScenarioConfig"),
    "VS Code should be able to save full scenario config trees through LSP",
  );
  assert.ok(
    source.includes("async function getScenarioConfigFull(")
      && source.includes("async function setScenarioConfig("),
    "VS Code should expose typed helper functions for scenario GUI load/save",
  );
  const scenarioProviderBlock = sliceFrom(
    source,
    "const scenarioEditorProvider: vscode.CustomTextEditorProvider = {",
    "const simulationDiagnostics = vscode.languages.createDiagnosticCollection('rumoca simulation');",
  );
  assert.ok(
    scenarioProviderBlock.includes("getScenarioConfigFull(document.uri.toString(), document.getText())"),
    "scenario GUI should parse the live TOML buffer instead of stale disk contents",
  );
  assert.ok(
    scenarioProviderBlock.includes("webviewPanel.onDidChangeViewState((event) => {")
      && scenarioProviderBlock.includes("if (event.webviewPanel.active) {")
      && scenarioProviderBlock.includes("void refresh();"),
    "scenario GUI should refresh from the live buffer when the custom editor becomes active",
  );
  assert.ok(
    scenarioProviderBlock.includes("replaceDirtyOpenDocument: true"),
    "scenario GUI saves should intentionally replace the live editor buffer after parsing it",
  );
  const setScenarioConfigBlock = sliceFrom(
    source,
    "async function setScenarioConfig(",
    "async function reopenEmbeddedModelicaDocuments()",
  );
  assert.ok(
    setScenarioConfigBlock.includes("await renderScenarioConfig(config)")
      && setScenarioConfigBlock.includes("vscode.workspace.applyEdit(edit)")
      && setScenarioConfigBlock.includes("await openDocument.save()"),
    "open scenario files should be saved through the VS Code editor buffer",
  );
});

test("surface contract: VS Code codegen settings are scenario-owned", () => {
  const source = readExtensionSource();
  const sharedSource = readSharedVisualizationSource();
  const scenarioProviderBlock = sliceFrom(
    source,
    "const scenarioEditorProvider: vscode.CustomTextEditorProvider = {",
    "const simulationDiagnostics = vscode.languages.createDiagnosticCollection('rumoca simulation');",
  );
  const renderCodegenBlock = sliceFrom(
    source,
    "const renderConfiguredCodegenScenario = async (",
    "const simulateCommand = vscode.commands.registerCommand('rumoca.simulateModel'",
  );

  assert.equal(
    source.includes("CODEGEN_SETTINGS_STATE_KEY"),
    false,
    "VS Code should not persist codegen target selection outside scenario TOML",
  );
  assert.equal(
    source.includes("rumoca.codegenSettings"),
    false,
    "VS Code should not keep legacy codegen settings workspace state",
  );
  assert.ok(
    scenarioProviderBlock.includes("loadVisualizationShared().applyScenarioConfigEdits")
      && scenarioProviderBlock.includes("await setScenarioConfig(document.uri.toString(), config, {"),
    "VS Code should save codegen settings through full scenario config edits",
  );
  assert.ok(
    renderCodegenBlock.includes("const target = trimMaybeString(scenario.target);")
      && renderCodegenBlock.includes("'rumoca.workspace.renderTarget'"),
    "VS Code should render codegen scenarios from the scenario-owned target",
  );
  assert.ok(
    source.includes("async function getScenarioConfigFull(")
      && source.includes("async function setScenarioConfig(")
      && !source.includes("async function showSimulationSettingsPanel("),
    "codegen settings should be edited through the shared scenario custom editor, not a separate settings panel",
  );
  assert.ok(
    sharedSource.includes("SCENARIO_CODEGEN_TARGET_OPTIONS")
      && sharedSource.includes("data-scenario-section=\"codegen\"")
      && sharedSource.includes("section.hidden = task !== 'codegen'"),
    "shared scenario GUI should expose codegen target controls only for codegen scenarios",
  );
});

test("surface contract: initial language-server startup failure does not skip command registration", () => {
  const source = readExtensionSource();
  const initialStartupBlock = sliceFrom(
    source,
    "const initialLanguageClient = await startLanguageClient();",
    "const wireResultsPanelMessageHandling = (panel: vscode.WebviewPanel) => {",
  );

  assert.equal(
    initialStartupBlock.includes("if (!initialLanguageClient.clientStarted) {\n        return;\n    }"),
    false,
    "initial language-server startup failure should not abort activate() before commands are registered",
  );
  assert.ok(
    initialStartupBlock.includes(
      "log('Continuing activation without a running language server so commands remain available.');",
    ),
    "activation should explicitly continue when the first language-server start fails",
  );
});

test("surface contract: extension registrations and hooks are inventory-mapped", () => {
  const extensionSurface = inventoryExtensionSurface(readExtensionSource());

  assertInventory(
    "lifecycleHooks",
    extensionSurface.lifecycleHooks,
    SURFACE_COVERAGE.lifecycleHooks,
  );
  assertInventory(
    "extensionCommands",
    extensionSurface.extensionCommands,
    SURFACE_COVERAGE.extensionCommands,
  );
  assertInventory("serializers", extensionSurface.serializers, SURFACE_COVERAGE.serializers);
  assertInventory(
    "workspaceHooks",
    extensionSurface.workspaceHooks,
    SURFACE_COVERAGE.workspaceHooks,
  );
  assertInventory(
    "contentProviders",
    extensionSurface.contentProviders,
    SURFACE_COVERAGE.contentProviders,
  );
  assertInventory(
    "languageProviders",
    extensionSurface.languageProviders,
    SURFACE_COVERAGE.languageProviders,
  );
  assertInventory(
    "windowHooks",
    extensionSurface.windowHooks,
    SURFACE_COVERAGE.windowHooks,
  );
  assertInventory(
    "resultsWebviewMessages",
    extensionSurface.resultsWebviewMessages,
    SURFACE_COVERAGE.resultsWebviewMessages,
  );
  assertInventory(
    "scenarioConfigWebviewMessages",
    extensionSurface.scenarioConfigWebviewMessages,
    SURFACE_COVERAGE.scenarioConfigWebviewMessages,
  );
  assertInventory(
    "fileSystemWatchers",
    extensionSurface.fileSystemWatchers,
    SURFACE_COVERAGE.fileSystemWatchers,
  );
  assertInventory(
    "fileSystemWatcherHooks",
    extensionSurface.fileSystemWatcherHooks,
    SURFACE_COVERAGE.fileSystemWatcherHooks,
  );
});
