import path from "node:path";
import { createRequire } from "node:module";
import { readFileSync } from "node:fs";
import { createProjectFilesystem } from "../src/modules/project_fs.js";
import { createProjectInterface } from "../src/modules/project_interface.js";

const require = createRequire(import.meta.url);
globalThis.RumocaVisualizationShared = require(
  path.resolve("crates", "rumoca-viz-web", "web", "visualization_shared.js"),
);

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function simulationPresetWritesColocatedConfig() {
  const projectFs = createProjectFilesystem();
  projectFs.setActiveDocument("Ball.mo", "model Ball\nend Ball;\n");
  const projectInterface = createProjectInterface({ projectFs });

  const response = projectInterface.execute("rumoca.project.setSimulationPreset", {
    model: "Ball",
    preset: { tEnd: 12.5, dt: 0.2 },
  });

  assert(response?.ok === true, "expected setSimulationPreset ok response");
  const files = projectFs.listFiles().map((file) => file.path);
  assert(
    files.includes("ball_sim.rum"),
    `expected colocated Ball simulation config to be created, got ${files.join(", ")}`,
  );

  const config = projectInterface.execute("rumoca.project.getSimulationConfig", {
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

function resetSimulationPresetClearsColocatedSimSection() {
  const projectFs = createProjectFilesystem();
  projectFs.setActiveDocument("Ball.mo", "model Ball\nend Ball;\n");
  const projectInterface = createProjectInterface({ projectFs });

  projectInterface.execute("rumoca.project.setSimulationPreset", {
    model: "Ball",
    preset: { tEnd: 12.5, dt: 0.2 },
  });
  const response = projectInterface.execute("rumoca.project.resetSimulationPreset", { model: "Ball" });
  assert(response?.ok === true, "expected resetSimulationPreset ok response");

  const files = projectFs.listFiles().map((file) => file.path);
  assert(
    files.includes("ball_sim.rum"),
    "expected colocated model config to remain after reset",
  );
}

function parsesExistingColocatedProjectFiles() {
  const projectFs = createProjectFilesystem();
  projectFs.loadFileEntries([
    {
      path: "Ball.mo",
      content: "model Ball\nend Ball;\n",
    },
    {
      path: "ball_sim.rum",
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
  ]);

  const projectInterface = createProjectInterface({ projectFs });
  const simulation = projectInterface.execute("rumoca.project.getSimulationConfig", {
    model: "Ball",
    fallback: { solver: "auto", tEnd: 10, dt: null, outputDir: "", sourceRootPaths: [] },
  });
  const visualization = projectInterface.execute("rumoca.project.getVisualizationConfig", { model: "Ball" });

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

async function simulationCommandsUseRuntimeBridgeAndPersistSelectedModel() {
  const projectFs = createProjectFilesystem();
  projectFs.setActiveDocument("PIDMSL.mo", "model PIDMSL\nend PIDMSL;\n");
  const runtimeRequests = [];
  const projectInterface = createProjectInterface({
    projectFs,
    runtimeBridge: {
      async request(action, payload) {
        runtimeRequests.push({ action, payload });
        if (
          action === "projectCommand"
          && payload?.command === "rumoca.project.getSimulationModels"
        ) {
          return JSON.stringify({
            ok: true,
            models: ["PIDMSL", "Other"],
            selectedModel: "PIDMSL",
            error: null,
          });
        }
        if (
          action === "projectCommand"
          && payload?.command === "rumoca.project.startSimulation"
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
        throw new Error(`unexpected runtime action: ${action}`);
      },
    },
  });

  const models = await projectInterface.execute("rumoca.project.getSimulationModels", {
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
    projectFs.getEditorState()?.selectedSimulationModel === "PIDMSL",
    "expected selected model persisted into editor state",
  );

  const setSelection = projectInterface.execute("rumoca.project.setSelectedSimulationModel", {
    model: "Other",
  });
  assert(setSelection?.ok === true, "expected setSelectedSimulationModel ok response");
  assert(
    projectFs.getEditorState()?.selectedSimulationModel === "Other",
    "expected setSelectedSimulationModel to update editor state",
  );

  const started = await projectInterface.execute("rumoca.project.startSimulation", {
    source: "model PIDMSL\nend PIDMSL;\n",
    model: "Other",
    fallback: { solver: "auto", tEnd: 5, dt: null, outputDir: "", sourceRootPaths: [] },
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
    runtimeRequests[1]?.action === "projectCommand",
    `expected second runtime request to use projectCommand, got ${runtimeRequests[1]?.action}`,
  );
  assert(
    runtimeRequests[1]?.payload?.command === "rumoca.project.startSimulation",
    `expected project command rumoca.project.startSimulation, got ${runtimeRequests[1]?.payload?.command}`,
  );
  assert(
    runtimeRequests[1]?.payload?.payload?.modelName === "Other",
    `expected startSimulation payload for Other, got ${runtimeRequests[1]?.payload?.payload?.modelName}`,
  );
  assert(
    runtimeRequests[1]?.payload?.payload?.solver === "auto",
    `expected startSimulation payload solver auto, got ${runtimeRequests[1]?.payload?.payload?.solver}`,
  );
  assert(
    !Object.prototype.hasOwnProperty.call(runtimeRequests[1]?.payload?.payload || {}, "projectSources"),
    "expected startSimulation not to send projectSources unless explicitly provided",
  );
}

function unifiedSettingsSurfaceUsesSharedModal() {
  const mainSource = readFileSync(
    path.resolve("editors", "wasm", "src", "main.js"),
    "utf8",
  );
  const htmlSource = readFileSync(
    path.resolve("editors", "wasm", "index.html"),
    "utf8",
  );

  assert(
    mainSource.includes("codegen: codegenSettings"),
    "expected shared settings state to include codegen settings",
  );
  assert(
    mainSource.includes("codegenTemplates: templates"),
    "expected shared settings state to include codegen template options",
  );
  assert(
    mainSource.includes("button.title = 'Simulation and codegen settings';"),
    "expected editor settings button to use unified settings copy",
  );
  assert(
    mainSource.includes("window.openFileRunSettings = function() {\n    openSimulationSettingsModal();\n};"),
    "expected run settings button to open the shared simulation/settings modal",
  );
  assert(
    !mainSource.includes("openCodegenSettingsModal"),
    "expected dedicated codegen settings modal logic to be removed",
  );
  assert(
    htmlSource.includes('id="simulationSettingsTitle">Rumoca Settings</h3>'),
    "expected simulation settings modal title to reflect unified Rumoca settings",
  );
  assert(
    !htmlSource.includes('id="codegenSettingsModal"'),
    "expected standalone codegen settings modal markup to be removed",
  );
}

function languageProjectSyncStaysOnDiagnostics() {
  const mainSource = readFileSync(
    path.resolve("editors", "wasm", "src", "main.js"),
    "utf8",
  );
  const match = mainSource.match(/function languageCommandNeedsProjectSources\(command\) \{\n([\s\S]*?)\n\}/);
  assert(match, "expected languageCommandNeedsProjectSources helper in main.js");
  const body = match[1];
  assert(
    body.includes("return command === 'rumoca.language.diagnostics';"),
    "expected only diagnostics to sync workspace project sources",
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
    path.resolve("editors", "wasm", "src", "main.js"),
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

simulationPresetWritesColocatedConfig();
resetSimulationPresetClearsColocatedSimSection();
parsesExistingColocatedProjectFiles();
await simulationCommandsUseRuntimeBridgeAndPersistSelectedModel();
unifiedSettingsSurfaceUsesSharedModal();
languageProjectSyncStaysOnDiagnostics();
compileFailuresPreferRefreshedModelicaDiagnostics();
