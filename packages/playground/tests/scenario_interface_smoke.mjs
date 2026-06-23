import path from "node:path";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { createWorkspaceFilesystem } from "../src/modules/workspace_fs.js";
import { createScenarioInterface } from "../src/modules/scenario_interface.js";

const repoRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..", "..");

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

async function simulationPresetWritesColocatedConfig() {
  const workspaceFs = createWorkspaceFilesystem();
  workspaceFs.setActiveDocument("Ball.mo", "model Ball\nend Ball;\n");
  const scenarioInterface = createScenarioInterface({
    workspaceFs,
    runtimeBridge: {
      async request(_action, payload) {
        if (payload?.command === "rumoca.scenario.setSimulationPreset") {
          return JSON.stringify({
            writes: [{ path: "rumoca-scenario.ball.toml", content: "scenario content\n" }],
            result: { ok: true },
          });
        }
        if (payload?.command === "rumoca.scenario.getSimulationConfig") {
          return JSON.stringify({
            preset: { tEnd: 12.5, dt: 0.2 },
            defaults: { solver: "auto", tEnd: 10, dt: null, outputDir: "", sourceRootPaths: [] },
            effective: { solver: "auto", tEnd: 12.5, dt: 0.2, outputDir: "", sourceRootPaths: [] },
            diagnostics: [],
          });
        }
        throw new Error(`unexpected runtime command: ${payload?.command}`);
      },
    },
  });

  const response = await scenarioInterface.execute("rumoca.scenario.setSimulationPreset", {
    model: "Ball",
    preset: { tEnd: 12.5, dt: 0.2 },
  });

  assert(response?.ok === true, "expected setSimulationPreset ok response");
  const files = workspaceFs.listFiles().map((file) => file.path);
  assert(
    files.includes("rumoca-scenario.ball.toml"),
    `expected colocated Ball simulation config to be created, got ${files.join(", ")}`,
  );

  const config = await scenarioInterface.execute("rumoca.scenario.getSimulationConfig", {
    model: "Ball",
    fallback: { solver: "auto", tEnd: 10, dt: null, outputDir: "", sourceRootPaths: [] },
  });
  assert(
    config.effective.tEnd === 12.5,
    `expected effective tEnd 12.5, got ${config.effective.tEnd}`,
  );
  assert(
    config.effective.dt === 0.2,
    `expected effective dt 0.2, got ${config.effective.dt}`,
  );
}

async function resetSimulationPresetClearsColocatedSimSection() {
  const workspaceFs = createWorkspaceFilesystem();
  workspaceFs.setActiveDocument("Ball.mo", "model Ball\nend Ball;\n");
  const scenarioInterface = createScenarioInterface({
    workspaceFs,
    runtimeBridge: {
      async request(_action, payload) {
        if (payload?.command === "rumoca.scenario.setSimulationPreset") {
          return JSON.stringify({
            writes: [{ path: "rumoca-scenario.ball.toml", content: "scenario content\n" }],
            result: { ok: true },
          });
        }
        if (payload?.command === "rumoca.scenario.resetSimulationPreset") {
          return JSON.stringify({
            writes: [{ path: "rumoca-scenario.ball.toml", content: "reset scenario content\n" }],
            result: { ok: true },
          });
        }
        throw new Error(`unexpected runtime command: ${payload?.command}`);
      },
    },
  });

  await scenarioInterface.execute("rumoca.scenario.setSimulationPreset", {
    model: "Ball",
    preset: { tEnd: 12.5, dt: 0.2 },
  });
  const response = await scenarioInterface.execute("rumoca.scenario.resetSimulationPreset", { model: "Ball" });
  assert(response?.ok === true, "expected resetSimulationPreset ok response");

  const files = workspaceFs.listFiles().map((file) => file.path);
  assert(
    files.includes("rumoca-scenario.ball.toml"),
    "expected colocated model config to remain after reset",
  );
}

async function scenarioReadsUseRuntimeBridgeWorkspaceSources() {
  const workspaceFs = createWorkspaceFilesystem();
  workspaceFs.loadFileEntries([
    {
      path: "Ball.mo",
      content: "model Ball\nend Ball;\n",
    },
    {
      path: "rumoca-scenario.ball.toml",
      content: [
        "version = 2",
        "[model]",
        'name = "Ball"',
        "",
        "[sim]",
        'solver = "bdf"',
        "t_end = 30.0",
        "dt = 0.5",
        "",
        "[[plot.views]]",
        'id = "states_time"',
        'title = "States vs time"',
        'type = "timeseries"',
        'x = "time"',
        'y = ["x", "v"]',
        "",
        "[[plot.views]]",
        'id = "viewer_3d"',
        'title = "Viewer"',
        'type = "3d"',
        'script_path = "../shared/viewer_3d.js"',
        "",
      ].join("\n"),
    },
    {
      path: "rumoca-workspace.toml",
      content: 'source_roots = ["vendor/Modelica"]\n',
    },
  ]);

  const scenarioInterface = createScenarioInterface({
    workspaceFs,
    runtimeBridge: {
      async request(_action, payload) {
        const sources = JSON.parse(payload?.payload?.workspaceSources || "{}");
        assert(
          Object.prototype.hasOwnProperty.call(sources, "rumoca-scenario.ball.toml"),
          "expected scenario file to be sent to runtime bridge",
        );
        assert(
          Object.prototype.hasOwnProperty.call(sources, "rumoca-workspace.toml"),
          "expected workspace config file to be sent to runtime bridge",
        );
        assert(
          Object.prototype.hasOwnProperty.call(sources, "Ball.mo"),
          "expected model file to be sent to runtime bridge for colocated defaults",
        );
        if (payload?.command === "rumoca.scenario.getSimulationConfig") {
          return JSON.stringify({
            preset: { solver: "bdf", tEnd: 30, dt: 0.5 },
            defaults: { solver: "auto", tEnd: 10, dt: null, outputDir: "", sourceRootPaths: [] },
            effective: { solver: "bdf", tEnd: 30, dt: 0.5, outputDir: "", sourceRootPaths: [] },
            diagnostics: [],
          });
        }
        if (payload?.command === "rumoca.scenario.getVisualizationConfig") {
          return JSON.stringify({
            views: [
              { id: "states_time", title: "States vs time", type: "timeseries", x: "time", y: ["x", "v"] },
              { id: "viewer_3d", title: "Viewer", type: "3d", scriptPath: "../shared/viewer_3d.js" },
            ],
          });
        }
        throw new Error(`unexpected runtime command: ${payload?.command}`);
      },
    },
  });
  const simulation = await scenarioInterface.execute("rumoca.scenario.getSimulationConfig", {
    model: "Ball",
    fallback: { solver: "auto", tEnd: 10, dt: null, outputDir: "", sourceRootPaths: [] },
  });
  const visualization = await scenarioInterface.execute("rumoca.scenario.getVisualizationConfig", { model: "Ball" });

  assert(
    simulation.effective.solver === "bdf",
    `expected solver bdf, got ${simulation.effective.solver}`,
  );
  assert(
    simulation.effective.tEnd === 30,
    `expected effective tEnd 30, got ${simulation.effective.tEnd}`,
  );
  assert(
    simulation.effective.dt === 0.5,
    `expected effective dt 0.5, got ${simulation.effective.dt}`,
  );
  assert(
    Array.isArray(visualization.views) && visualization.views.length === 2,
    "expected visualization views to load from colocated config",
  );
  assert(
    visualization.views[1].scriptPath === "../shared/viewer_3d.js",
    `expected canonical scriptPath field, got ${visualization.views[1].scriptPath}`,
  );
}

async function fullScenarioConfigCommandsUseRuntimeBridgeAndApplyWrites() {
  const workspaceFs = createWorkspaceFilesystem();
  workspaceFs.loadFileEntries([
    {
      path: "Ball.mo",
      content: "model Ball\nend Ball;\n",
    },
    {
      path: "rumoca-scenario.ball.toml",
      content: [
        "[rumoca]",
        'task = "simulate"',
        "",
        "[model]",
        'name = "Ball"',
        "",
      ].join("\n"),
    },
  ]);

  const commands = [];
  const scenarioInterface = createScenarioInterface({
    workspaceFs,
    runtimeBridge: {
      async request(_action, payload) {
        commands.push(payload?.command);
        const sources = JSON.parse(payload?.payload?.workspaceSources || "{}");
        assert(
          sources["rumoca-scenario.ball.toml"]?.includes('name = "Ball"'),
          "expected full scenario config commands to receive live scenario TOML",
        );
        if (payload?.command === "rumoca.scenario.getScenarioConfigFull") {
          return JSON.stringify({
            ok: true,
            config: {
              rumoca: { task: "simulate" },
              model: { name: "Ball" },
              sim: { solver: "auto" },
            },
            descriptor: { model: "Ball", task: "simulate" },
          });
        }
        if (payload?.command === "rumoca.scenario.setScenarioConfig") {
          return JSON.stringify({
            writes: [{ path: "rumoca-scenario.ball.toml", content: "rendered scenario toml\n" }],
            result: { ok: true },
          });
        }
        throw new Error(`unexpected runtime command: ${payload?.command}`);
      },
    },
  });

  const full = await scenarioInterface.execute("rumoca.scenario.getScenarioConfigFull", {
    path: "rumoca-scenario.ball.toml",
  });
  assert(full?.config?.model?.name === "Ball", "expected full config tree from runtime bridge");

  const saved = await scenarioInterface.execute("rumoca.scenario.setScenarioConfig", {
    path: "rumoca-scenario.ball.toml",
    config: full.config,
  });
  assert(saved?.ok === true, "expected setScenarioConfig ok response");
  assert(
    workspaceFs.getFileContent("rumoca-scenario.ball.toml") === "rendered scenario toml\n",
    "expected setScenarioConfig returned write to update workspace file content",
  );
  assert(
    commands.join(",") === "rumoca.scenario.getScenarioConfigFull,rumoca.scenario.setScenarioConfig",
    `expected full-config command sequence, got ${commands.join(",")}`,
  );
}

async function codegenConfigCommandsUseRuntimeBridgeAndApplyWrites() {
  const workspaceFs = createWorkspaceFilesystem();
  workspaceFs.loadFileEntries([
    {
      path: "Ball.mo",
      content: "model Ball\nend Ball;\n",
    },
    {
      path: "rumoca-scenario.ball.toml",
      content: [
        "[rumoca]",
        'task = "simulate"',
        "",
        "[model]",
        'name = "Ball"',
        "",
        "[codegen]",
        'target = "sympy"',
        "",
      ].join("\n"),
    },
  ]);

  const scenarioInterface = createScenarioInterface({
    workspaceFs,
    runtimeBridge: {
      async request(_action, payload) {
        const sources = JSON.parse(payload?.payload?.workspaceSources || "{}");
        if (payload?.command === "rumoca.scenario.getCodegenConfig") {
          assert(
            sources["rumoca-scenario.ball.toml"]?.includes('target = "sympy"'),
            "expected getCodegenConfig to receive initial scenario TOML",
          );
          return JSON.stringify({ target: "sympy", outputDir: null });
        }
        if (payload?.command === "rumoca.scenario.setCodegenConfig") {
          assert(
            sources["rumoca-scenario.ball.toml"]?.includes('target = "sympy"'),
            "expected setCodegenConfig to receive live scenario TOML",
          );
          return JSON.stringify({
            writes: [{ path: "rumoca-scenario.ball.toml", content: "rendered codegen toml\n" }],
            result: { ok: true },
          });
        }
        if (payload?.command === "rumoca.scenario.setSourceRoots") {
          assert(
            sources["rumoca-scenario.ball.toml"] === "rendered codegen toml\n",
            "expected setSourceRoots to receive workspace content from the prior write",
          );
          return JSON.stringify({
            writes: [{ path: "rumoca-scenario.ball.toml", content: "rendered source roots toml\n" }],
            result: { ok: true },
          });
        }
        throw new Error(`unexpected runtime command: ${payload?.command}`);
      },
    },
  });

  const current = await scenarioInterface.execute("rumoca.scenario.getCodegenConfig", {
    model: "Ball",
  });
  assert(current?.target === "sympy", "expected codegen target from scenario");

  const saved = await scenarioInterface.execute("rumoca.scenario.setCodegenConfig", {
    model: "Ball",
    config: { target: "targets/custom" },
  });
  assert(saved?.ok === true, "expected setCodegenConfig ok response");
  assert(
    workspaceFs.getFileContent("rumoca-scenario.ball.toml") === "rendered codegen toml\n",
    "expected setCodegenConfig returned write to update workspace file content",
  );

  const rootsSaved = await scenarioInterface.execute("rumoca.scenario.setSourceRoots", {
    model: "Ball",
    config: { sourceRootPaths: ["../vendor"] },
  });
  assert(rootsSaved?.ok === true, "expected setSourceRoots ok response");
  assert(
    workspaceFs.getFileContent("rumoca-scenario.ball.toml") === "rendered source roots toml\n",
    "expected setSourceRoots returned write to update workspace file content",
  );
}

async function simulationCommandsUseRuntimeBridgeAndKeepSelectedModelInSession() {
  const workspaceFs = createWorkspaceFilesystem();
  workspaceFs.setActiveDocument("PIDMSL.mo", "model PIDMSL\nend PIDMSL;\n");
  workspaceFs.setFile(
    "vendor/Modelica/package.mo",
    "within ;\npackage Modelica\nend Modelica;\n",
  );
  const runtimeRequests = [];
  const scenarioInterface = createScenarioInterface({
    workspaceFs,
    runtimeBridge: {
      async request(action, payload) {
        runtimeRequests.push({ action, payload });
        if (
          action === "scenarioCommand"
          && payload?.command === "rumoca.scenario.getSimulationModels"
        ) {
          return JSON.stringify({
            ok: true,
            models: ["PIDMSL", "Other"],
            selectedModel: "PIDMSL",
            error: null,
          });
        }
        if (
          action === "scenarioCommand"
          && payload?.command === "rumoca.scenario.startSimulation"
        ) {
          return JSON.stringify({
            payload: {
              version: 1,
              names: ["x"],
              allData: [[0, 1], [1, 2]],
              nStates: 1,
              variableMeta: [],
              simDetails: {},
            },
            metrics: {
              simulateSeconds: 0.1,
              points: 2,
              variables: 1,
            },
          });
        }
        if (
          action === "scenarioCommand"
          && payload?.command === "rumoca.scenario.getSimulationConfig"
        ) {
          return JSON.stringify({
            preset: null,
            defaults: { solver: "auto", tEnd: 5, dt: null, outputDir: "", sourceRootPaths: ["vendor/Modelica"] },
            effective: { solver: "auto", tEnd: 5, dt: null, outputDir: "", sourceRootPaths: ["vendor/Modelica"] },
            diagnostics: [],
          });
        }
        throw new Error(`unexpected runtime action: ${action}`);
      },
    },
  });

  const models = await scenarioInterface.execute("rumoca.scenario.getSimulationModels", {
    source: "model PIDMSL\nend PIDMSL;\n",
    defaultModel: "",
  });
  assert(models.ok === true, "expected getSimulationModels ok response");
  assert(
    Array.isArray(models.models) && models.models.length === 2,
    "expected simulation model list from runtime bridge",
  );
  assert(models.selectedModel === "PIDMSL", "expected selected model from runtime bridge");
  assert(
    workspaceFs.getEditorState()?.selectedSimulationModel === undefined,
    "expected selected model to stay out of editor state",
  );

  const setSelection = await scenarioInterface.execute("rumoca.scenario.setSelectedSimulationModel", {
    model: "Other",
  });
  assert(setSelection?.ok === true, "expected setSelectedSimulationModel ok response");
  assert(setSelection.selectedModel === "Other", "expected selected model response");
  assert(
    workspaceFs.getEditorState()?.selectedSimulationModel === undefined,
    "expected setSelectedSimulationModel to keep editor state unchanged",
  );

  const started = await scenarioInterface.execute("rumoca.scenario.startSimulation", {
    source: "model PIDMSL\nend PIDMSL;\n",
    model: "",
    fallback: { solver: "auto", tEnd: 5, dt: null, outputDir: "", sourceRootPaths: ["vendor/Modelica"] },
    timeoutMs: 12345,
  });
  assert(started?.ok === true, "expected startSimulation ok response");
  assert(started.model === "Other", `expected selected model Other, got ${started.model}`);
  assert(started.effective.tEnd === 5, `expected tEnd 5, got ${started.effective.tEnd}`);
  assert(started.payload?.nStates === 1, `expected nStates 1, got ${started.payload?.nStates}`);
  assert(
    started.metrics?.simulateSeconds === 0.1,
    `expected simulateSeconds 0.1, got ${started.metrics?.simulateSeconds}`,
  );
  assert(
    runtimeRequests[2]?.action === "scenarioCommand",
    `expected third runtime request to use scenarioCommand, got ${runtimeRequests[2]?.action}`,
  );
  assert(
    runtimeRequests[2]?.payload?.command === "rumoca.scenario.startSimulation",
    `expected scenario command rumoca.scenario.startSimulation, got ${runtimeRequests[2]?.payload?.command}`,
  );
  assert(
    runtimeRequests[2]?.payload?.payload?.modelName === "Other",
    `expected startSimulation payload for Other, got ${runtimeRequests[2]?.payload?.payload?.modelName}`,
  );
  assert(
    runtimeRequests[2]?.payload?.payload?.solver === "auto",
    `expected startSimulation payload solver auto, got ${runtimeRequests[2]?.payload?.payload?.solver}`,
  );
  assert(
    !Object.prototype.hasOwnProperty.call(runtimeRequests[2]?.payload?.payload || {}, "workspaceSources"),
    "expected startSimulation not to send workspaceSources unless explicitly provided",
  );
  const sourceRoots = JSON.parse(runtimeRequests[2]?.payload?.payload?.sourceRoots || "{}");
  assert(
    Object.prototype.hasOwnProperty.call(sourceRoots, "vendor/Modelica/package.mo"),
    "expected startSimulation to send effective source-root file contents",
  );
}

async function parameterMetadataUsesWorkspaceSourcesWithEffectiveSourceRoots() {
  const workspaceFs = createWorkspaceFilesystem();
  workspaceFs.setActiveDocument(
    "Ball.mo",
    "model Ball\n  parameter Real g = 9.81;\nend Ball;\n",
  );
  workspaceFs.setFile(
    "Other.mo",
    "model Other\nend Other;\n",
  );
  workspaceFs.setFile(
    "vendor/Modelica/package.mo",
    "within ;\npackage Modelica\nend Modelica;\n",
  );
  const runtimeRequests = [];
  const scenarioInterface = createScenarioInterface({
    workspaceFs,
    runtimeBridge: {
      async request(action, payload) {
        runtimeRequests.push({ action, payload });
        if (
          action === "scenarioCommand"
          && payload?.command === "rumoca.scenario.getSimulationConfig"
        ) {
          return JSON.stringify({
            preset: null,
            defaults: { solver: "auto", tEnd: 5, dt: null, outputDir: "", sourceRootPaths: ["vendor/Modelica"] },
            effective: { solver: "auto", tEnd: 5, dt: null, outputDir: "", sourceRootPaths: ["vendor/Modelica"] },
            diagnostics: [],
          });
        }
        if (
          action === "scenarioCommand"
          && payload?.command === "rumoca.model.parameterMetadata"
        ) {
          return JSON.stringify({
            ok: true,
            parameters: [{ name: "g", defaultValue: 9.81 }],
          });
        }
        throw new Error(`unexpected runtime action: ${action}`);
      },
    },
  });

  const metadata = await scenarioInterface.execute("rumoca.model.parameterMetadata", {
    path: "rumoca-scenario.ball.toml",
    sourcePath: "Ball.mo",
    source: workspaceFs.getFileContent("Ball.mo"),
    modelName: "Ball",
    fallback: { solver: "auto", tEnd: 5, dt: null, outputDir: "", sourceRootPaths: ["vendor/Modelica"] },
  });

  assert(metadata.length === 1 && metadata[0].name === "g", "expected normalized parameter metadata");
  const metadataRequest = runtimeRequests.find(
    (request) => request.payload?.command === "rumoca.model.parameterMetadata",
  );
  assert(metadataRequest, "expected parameter metadata runtime request");
  const requestPayload = metadataRequest.payload.payload || {};
  const workspaceSources = JSON.parse(requestPayload.workspaceSources || "{}");
  const sourceRoots = JSON.parse(requestPayload.sourceRoots || "{}");
  assert(
    !Object.prototype.hasOwnProperty.call(workspaceSources, "Ball.mo"),
    "expected parameter metadata workspace sources to exclude the primary model source",
  );
  assert(
    Object.prototype.hasOwnProperty.call(workspaceSources, "Other.mo"),
    "expected parameter metadata to keep other workspace model sources",
  );
  assert(
    !Object.prototype.hasOwnProperty.call(workspaceSources, "vendor/Modelica/package.mo"),
    "expected parameter metadata workspace sources to exclude effective source-root files",
  );
  assert(
    Object.prototype.hasOwnProperty.call(sourceRoots, "vendor/Modelica/package.mo"),
    "expected parameter metadata to include effective source-root file contents",
  );
}

function unifiedSettingsSurfaceUsesScenarioConfigEditor() {
  const mainSource = readFileSync(
    path.join(repoRoot, "packages", "playground", "src", "main.js"),
    "utf8",
  );
  const htmlSource = readFileSync(
    path.join(repoRoot, "packages", "playground", "index.html"),
    "utf8",
  );

  assert(
    !mainSource.includes("codegenSettings: { ...codegenSettings }"),
    "expected editor-state persistence not to duplicate scenario codegen settings",
  );
  assert(
    mainSource.includes("rumoca.scenario.getCodegenConfig")
      && mainSource.includes("rumoca.scenario.setCodegenConfig"),
    "expected playground codegen settings to use scenario commands",
  );
  assert(
    htmlSource.includes('data-editor-codegen-btn')
      && htmlSource.includes('Create Scenario for This Model')
      && mainSource.includes("openScenarioConfigForCurrentModel('simulate'")
      && mainSource.includes("scenarioConfigEditorController.showGui(nextPath)"),
    "expected non-scenario editor action to create/open the shared scenario settings view",
  );
  assert(
    mainSource.includes("createScenarioConfigEditorController"),
    "expected playground to install the shared scenario config editor controller",
  );
  assert(
    mainSource.includes("scenarioConfigEditorController.showGui"),
    "expected scenario settings action to reveal the shared scenario GUI",
  );
  assert(
    mainSource.includes("scenarioConfigEditorController.closePath(nextPath)"),
    "expected closing a scenario file to reset raw-mode state so reopening defaults to GUI",
  );
  const closeDocumentMatch = mainSource.match(/async function closeWorkspaceDocument[\s\S]*?^}/m);
  assert(closeDocumentMatch, "expected closeWorkspaceDocument implementation");
  assert(
    closeDocumentMatch[0].indexOf("persistActivePaneDocument();")
      < closeDocumentMatch[0].indexOf("scenarioConfigEditorController.closePath(nextPath)"),
    "expected raw scenario text to persist before close resets raw-mode state",
  );
  assert(
    htmlSource.includes('id="primaryScenarioConfigFrame"')
      && htmlSource.includes('id="secondaryScenarioConfigFrame"'),
    "expected editor panes to host the shared scenario config iframe",
  );
  assert(
    htmlSource.includes('class="scenario-config-frame"'),
    "expected scenario config frames to use dedicated editor-pane styling",
  );
  assert(
    mainSource.includes("openScenarioConfigForCurrentModel")
      && mainSource.includes("rumoca.scenario.defaultScenarioConfig"),
    "expected non-scenario settings action to create/open a shared scenario config",
  );
  assert(
    !mainSource.includes("openCodegenSettingsModal"),
    "expected dedicated codegen settings modal logic to be removed",
  );
  assert(
    !htmlSource.includes('id="simulationSettingsModal"'),
    "expected legacy simulation settings modal markup to be removed",
  );
  assert(
    !htmlSource.includes('id="codegenSettingsModal"'),
    "expected standalone codegen settings modal markup to be removed",
  );
}

function browserSurfacesRejectLegacyScenarioStateSideChannels() {
  const mainSource = readFileSync(
    path.join(repoRoot, "packages", "playground", "src", "main.js"),
    "utf8",
  );
  const htmlSource = readFileSync(
    path.join(repoRoot, "packages", "playground", "index.html"),
    "utf8",
  );
  const workspaceFsSource = readFileSync(
    path.join(repoRoot, "packages", "playground", "src", "modules", "workspace_fs.js"),
    "utf8",
  );
  const scenarioInterfaceSource = readFileSync(
    path.join(repoRoot, "packages", "playground", "src", "modules", "scenario_interface.js"),
    "utf8",
  );
  const sharedSource = readFileSync(
    path.join(repoRoot, "packages", "rumoca-web", "viz", "visualization_shared.js"),
    "utf8",
  );
  const browserSources = [
    mainSource,
    htmlSource,
    workspaceFsSource,
    scenarioInterfaceSource,
    sharedSource,
  ].join("\n");

  assert(
    !/(^|[^A-Za-z0-9_])\.rum([^A-Za-z0-9_]|$)/.test(browserSources),
    "expected browser sources not to handle .rum files",
  );
  for (const forbidden of [
    "'sim'",
    "'codegenSettings'",
    "'sourceRootPaths'",
    "'selectedSimulationModel'",
    "'template'",
  ]) {
    assert(
      !workspaceFsSource.includes(forbidden),
      `expected workspace editor-state allowlist not to include scenario-owned ${forbidden}`,
    );
  }
  assert(
    !mainSource.includes("getEditorState()?.selectedSimulationModel"),
    "expected selected simulation model fallback not to read persisted editor state",
  );
  assert(
    !scenarioInterfaceSource.includes("nextEditorState.selectedSimulationModel"),
    "expected selected simulation model updates not to write persisted editor state",
  );
  assert(
    !browserSources.includes("generatedConfig")
      && !browserSources.includes("rumoca.project")
      && !browserSources.includes("project_deps.js"),
    "expected browser sources not to expose old generated/project side channels",
  );
}

function languageWorkspaceSyncStaysOnDiagnostics() {
  const mainSource = readFileSync(
    path.join(repoRoot, "packages", "playground", "src", "main.js"),
    "utf8",
  );
  const match = mainSource.match(/function languageCommandNeedsWorkspaceSources\(command\) \{\n([\s\S]*?)\n\}/);
  assert(match, "expected languageCommandNeedsWorkspaceSources helper in main.js");
  const body = match[1];
  assert(
    body.includes("return command === 'rumoca.language.diagnostics';"),
    "expected only diagnostics to sync workspace sources",
  );
  for (const command of [
    "rumoca.language.documentSymbols",
    "rumoca.language.hover",
    "rumoca.language.completion",
    "rumoca.language.completionWithTiming",
    "rumoca.language.definition",
  ]) {
    assert(
      !body.includes(command),
      `expected ${command} not to force full workspace source sync`,
    );
  }
}

function compileFailuresPreferRefreshedModelicaDiagnostics() {
  const mainSource = readFileSync(
    path.join(repoRoot, "packages", "playground", "src", "main.js"),
    "utf8",
  );
  assert(
    mainSource.includes("async function refreshDiagnosticsAfterCompileFailure(source)"),
    "expected compile failures to refresh source diagnostics before rendering compile errors",
  );
  assert(
    mainSource.includes("if (refreshedDiagnostics.hasErrorDiagnostics)"),
    "expected span-bearing diagnostics to suppress duplicate compile-error bucket entries",
  );
  assert(
    mainSource.includes("Compilation skipped due to diagnostics errors."),
    "expected refreshed diagnostics to drive the standard diagnostics-error compile state",
  );
}

function simulationFallbackUsesEffectiveWorkspaceRoots() {
  const mainSource = readFileSync(
    path.join(repoRoot, "packages", "playground", "src", "main.js"),
    "utf8",
  );
  assert(
    mainSource.includes("rumoca.workspace.effectiveSourceRoots"),
    "expected playground to request effective workspace source roots from the worker",
  );
  assert(
    mainSource.includes("async function scenarioSimulationFallbackForFocus"),
    "expected simulation fallback to resolve source roots asynchronously",
  );
  assert(
    mainSource.includes("const simulationFallback = await scenarioSimulationFallbackForFocus")
      && mainSource.includes("fallback: simulationFallback"),
    "expected simulation execution to use effective workspace source roots",
  );
  assert(
    mainSource.includes("scenarioSimulationFallback(await effectiveWorkspaceSourceRootPaths(focusPath))"),
    "expected inherited workspace roots to stay in fallback config instead of scenario editor state",
  );
  assert(
    !mainSource.includes("simulationSettingsEditableCurrent"),
    "expected legacy simulation settings modal state shaping to be removed",
  );
}

await simulationPresetWritesColocatedConfig();
await resetSimulationPresetClearsColocatedSimSection();
await scenarioReadsUseRuntimeBridgeWorkspaceSources();
await fullScenarioConfigCommandsUseRuntimeBridgeAndApplyWrites();
await codegenConfigCommandsUseRuntimeBridgeAndApplyWrites();
await simulationCommandsUseRuntimeBridgeAndKeepSelectedModelInSession();
await parameterMetadataUsesWorkspaceSourcesWithEffectiveSourceRoots();
unifiedSettingsSurfaceUsesScenarioConfigEditor();
browserSurfacesRejectLegacyScenarioStateSideChannels();
languageWorkspaceSyncStaysOnDiagnostics();
compileFailuresPreferRefreshedModelicaDiagnostics();
simulationFallbackUsesEffectiveWorkspaceRoots();
