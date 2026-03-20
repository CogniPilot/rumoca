import path from "node:path";
import { createRequire } from "node:module";
import { createProjectFilesystem } from "../src/modules/project_fs.js";
import { createProjectInterface } from "../src/modules/project_interface.js";

const require = createRequire(import.meta.url);
globalThis.RumocaVisualizationShared = require(
  path.resolve("crates", "rumoca-sim", "web", "visualization_shared.js"),
);

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function simulationPresetWritesLspCompatibleSidecars() {
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
    files.includes(".rumoca/project.toml"),
    "expected .rumoca/project.toml to be created",
  );
  assert(
    files.some((path) => path.endsWith("/identity.toml")),
    "expected LSP-compatible identity sidecar to be created",
  );
  assert(
    files.some((path) => path.endsWith("/simulation.toml")),
    "expected LSP-compatible simulation sidecar to be created",
  );

  const config = projectInterface.execute("rumoca.project.getSimulationConfig", {
    model: "Ball",
    fallback: { solver: "auto", tEnd: 10, dt: null, outputDir: "", modelicaPath: [] },
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

function resetSimulationPresetRemovesSidecars() {
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
    !files.some((path) => path.endsWith("/simulation.toml")),
    "expected simulation sidecars removed after reset",
  );
  assert(
    !files.some((path) => path.endsWith("/identity.toml")),
    "expected identity sidecar removed when no model settings remain",
  );
}

function parsesExistingLspProjectFiles() {
  const projectFs = createProjectFilesystem();
  projectFs.loadFileEntries([
    {
      path: "Ball.mo",
      content: "model Ball\nend Ball;\n",
    },
    {
      path: ".rumoca/project.toml",
      content: [
        "version = 1",
        "",
        "[simulation.defaults]",
        'solver = "bdf"',
        "t_end = 20.0",
        "",
      ].join("\n"),
    },
    {
      path: ".rumoca/models/by-id/ball_12345678/identity.toml",
      content: [
        "version = 1",
        'uuid = "ball_12345678"',
        'qualified_name = "Ball"',
        'class_name = "Ball"',
        "last_seen_unix_ms = 1",
        "",
      ].join("\n"),
    },
    {
      path: ".rumoca/models/by-id/ball_12345678/simulation.toml",
      content: [
        "t_end = 30.0",
        "dt = 0.5",
        "",
      ].join("\n"),
    },
    {
      path: ".rumoca/models/by-id/ball_12345678/views.toml",
      content: [
        "[[views]]",
        'id = "states_time"',
        'title = "States vs time"',
        'type = "timeseries"',
        'x = "time"',
        'y = ["x", "v"]',
        "",
        "[[views]]",
        'id = "viewer_3d"',
        'title = "Viewer"',
        'type = "3d"',
        'script_path = ".rumoca/models/by-id/ball_12345678/viewer_3d.js"',
        "",
      ].join("\n"),
    },
  ]);

  const projectInterface = createProjectInterface({ projectFs });
  const simulation = projectInterface.execute("rumoca.project.getSimulationConfig", {
    model: "Ball",
    fallback: { solver: "auto", tEnd: 10, dt: null, outputDir: "", modelicaPath: [] },
  });
  const visualization = projectInterface.execute("rumoca.project.getVisualizationConfig", { model: "Ball" });

  assert(
    simulation.defaults.solver === "bdf",
    `expected defaults solver bdf, got ${simulation.defaults.solver}`,
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
    "expected visualization views to load from LSP-compatible sidecar",
  );
  assert(
    visualization.views[1].scriptPath === ".rumoca/models/by-id/ball_12345678/viewer_3d.js",
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
    fallback: { solver: "auto", tEnd: 5, dt: null, outputDir: "", modelicaPath: [] },
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
}

simulationPresetWritesLspCompatibleSidecars();
resetSimulationPresetRemovesSidecars();
parsesExistingLspProjectFiles();
await simulationCommandsUseRuntimeBridgeAndPersistSelectedModel();
