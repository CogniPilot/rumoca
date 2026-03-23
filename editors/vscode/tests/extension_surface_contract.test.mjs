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
    "rumoca.expandAllAnnotations": [COMMAND_CONTRACT_REF],
    "rumoca.openSettingsMenu": [COMMAND_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.openSimulationSettings": [COMMAND_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.openTemplateSettings": [COMMAND_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.renderTemplate": [COMMAND_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.simulateModel": [COMMAND_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.toggleAnnotation": [COMMAND_CONTRACT_REF],
  },
  activationCommands: {
    "rumoca.openSettingsMenu": [ACTIVATION_CONTRACT_REF],
    "rumoca.openSimulationSettings": [ACTIVATION_CONTRACT_REF],
    "rumoca.openTemplateSettings": [ACTIVATION_CONTRACT_REF],
    "rumoca.renderTemplate": [ACTIVATION_CONTRACT_REF],
    "rumoca.simulateModel": [ACTIVATION_CONTRACT_REF],
  },
  extensionCommands: {
    "rumoca.collapseAllAnnotations": [REGISTRATION_CONTRACT_REF],
    "rumoca.expandAllAnnotations": [REGISTRATION_CONTRACT_REF],
    "rumoca.openSettingsMenu": [REGISTRATION_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.openSimulationSettings": [REGISTRATION_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.openTemplateSettings": [REGISTRATION_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.resyncSidecars": [REGISTRATION_CONTRACT_REF],
    "rumoca.renderTemplate": [REGISTRATION_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.simulateModel": [REGISTRATION_CONTRACT_REF, ACTIVATION_CONTRACT_REF],
    "rumoca.toggleAnnotation": [REGISTRATION_CONTRACT_REF],
  },
  menuEntries: {
    "editor/title:rumoca.openSettingsMenu": [COMMAND_CONTRACT_REF],
    "editor/title:rumoca.renderTemplate": [COMMAND_CONTRACT_REF],
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
    "workspace.onDidCreateFiles": [REGISTRATION_CONTRACT_REF],
    "workspace.onDidDeleteFiles": [REGISTRATION_CONTRACT_REF],
    "workspace.onDidOpenTextDocument": [REGISTRATION_CONTRACT_REF],
    "workspace.onDidRenameFiles": [REGISTRATION_CONTRACT_REF],
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
  settingsWebviewMessages: {
    "settings.openModel": [REGISTRATION_CONTRACT_REF],
    "settings.openUserSettings": [REGISTRATION_CONTRACT_REF],
    "settings.openViewScript": [REGISTRATION_CONTRACT_REF],
    "settings.openWorkspaceSettings": [REGISTRATION_CONTRACT_REF],
    "settings.pickSourceRootPath": [REGISTRATION_CONTRACT_REF],
    "settings.prepareModels": [REGISTRATION_CONTRACT_REF],
    "settings.reset": [REGISTRATION_CONTRACT_REF],
    "settings.resyncSidecars": [REGISTRATION_CONTRACT_REF],
    "settings.save": [REGISTRATION_CONTRACT_REF],
  },
  resultsWebviewMessages: {
    "results.request": [REGISTRATION_CONTRACT_REF],
    "results.response": [REGISTRATION_CONTRACT_REF],
  },
  fileSystemWatchers: {
    "workspace.createFileSystemWatcher:**/*.mo": [REGISTRATION_CONTRACT_REF],
  },
  fileSystemWatcherHooks: {
    "modelicaFsWatcher.onDidCreate": [REGISTRATION_CONTRACT_REF],
    "modelicaFsWatcher.onDidDelete": [REGISTRATION_CONTRACT_REF],
  },
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
  const settingsWebviewHandler = sliceFrom(
    source,
    "panel.webview.onDidReceiveMessage(async (message) => {",
    "}, undefined, context.subscriptions);",
  );
  const settingsWebviewSource = sliceFrom(
    sharedVisualizationSource,
    "function buildHostedSimulationSettingsDocument(args) {",
    "async function handleHostedSimulationSettingsRequest(args) {",
  );
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
    workspaceHooks: collectMatches(
      source,
      /vscode\.workspace\.(onDid(?:ChangeConfiguration|ChangeTextDocument|OpenTextDocument|RenameFiles|CreateFiles|DeleteFiles))\(/g,
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
    settingsWebviewMessages: collectMatches(
      `${settingsWebviewHandler}\n${settingsWebviewSource}`,
      /requestHost\('([^']+)'/g,
      (match) => `settings.${match[1]}`,
    ),
    resultsWebviewMessages: collectMatches(
      `${resultsWebviewHandler}\n${resultsWebviewSource}`,
      /command (?:===|!==) '([^']+)'/g,
      (match) => match[1].startsWith("results.") ? match[1] : `results.${match[1]}`,
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

test("surface contract: editor title command placement stays aligned with the toolbar layout", () => {
  const packageSurface = inventoryPackageSurface(readPackageJson());

  assertInventory("menuEntries", packageSurface.menuEntries, SURFACE_COVERAGE.menuEntries);
});

test("surface contract: shared settings command opens the unified settings panel", () => {
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
  const templateSettingsCommandBlock = sliceFrom(
    source,
    "const templateSettingsCommand = vscode.commands.registerCommand(",
    "context.subscriptions.push(templateSettingsCommand);",
  );

  assert.ok(
    !settingsMenuCommandBlock.includes("vscode.window.showQuickPick("),
    "shared settings command should no longer present a picker",
  );
  assert.ok(
    settingsMenuCommandBlock.includes("await openUnifiedSettingsForEditor(editor);"),
    "shared settings command should open the unified settings panel",
  );
  assert.ok(
    simulationSettingsCommandBlock.includes("await openUnifiedSettingsForEditor(editor);"),
    "simulation settings command should open the unified settings panel",
  );
  assert.ok(
    templateSettingsCommandBlock.includes("await openUnifiedSettingsForEditor(editor);"),
    "template settings command should open the unified settings panel",
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
    "settingsWebviewMessages",
    extensionSurface.settingsWebviewMessages,
    SURFACE_COVERAGE.settingsWebviewMessages,
  );
  assertInventory(
    "resultsWebviewMessages",
    extensionSurface.resultsWebviewMessages,
    SURFACE_COVERAGE.resultsWebviewMessages,
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
