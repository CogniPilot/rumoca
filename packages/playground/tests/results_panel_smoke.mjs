import { createWorkspaceFilesystem } from "../src/modules/workspace_fs.js";
import { createScenarioInterface } from "../src/modules/scenario_interface.js";
import {
  buildScenarioVisualizationViewStorage,
  createResultsPanelController,
} from "../src/modules/results_panel.js";

const resultsAppFactory = {
  createResultsApp(args) {
    globalThis.__lastResultsAppArgs = args;
    return {
      dispose() {},
    };
  },
};

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function assertEqual(actual, expected, message) {
  if (!Object.is(actual, expected)) {
    throw new Error(message || `expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`);
  }
}

function assertNotEqual(actual, expected, message) {
  if (Object.is(actual, expected)) {
    throw new Error(message || `expected values to differ, got ${JSON.stringify(actual)}`);
  }
}

function assertDeepEqual(actual, expected, message) {
  const actualJson = JSON.stringify(actual);
  const expectedJson = JSON.stringify(expected);
  if (actualJson !== expectedJson) {
    throw new Error(message || `expected ${expectedJson}, got ${actualJson}`);
  }
}

function createScenarioInterfaceWithVisualizationStore(workspaceFs) {
  let views = [];
  return createScenarioInterface({
    workspaceFs,
    runtimeBridge: {
      async request(action, payload) {
        assertEqual(action, "scenarioCommand", "expected scenarioCommand runtime bridge");
        const command = payload?.command;
        if (command === "rumoca.scenario.setVisualizationConfig") {
          views = Array.isArray(payload?.payload?.views) ? payload.payload.views : [];
          return JSON.stringify({
            writes: [],
            result: { ok: true },
          });
        }
        if (command === "rumoca.scenario.getVisualizationConfig") {
          return JSON.stringify({ views });
        }
        throw new Error(`unexpected scenario command: ${command}`);
      },
    },
  });
}

function simulationPayloadMatchesSharedResultsContract() {
  const run = sampleSimulationRun({
    names: ["x", "y"],
    allData: [[0, 1], [1, 2], [3, 4]],
    tEnd: 10,
    dt: 0.1,
    simulateSeconds: 0.25,
    variables: 2,
  });

  assert(run.payload.nStates === 1, `expected nStates=1, got ${run.payload.nStates}`);
  assert(run.payload.allData.length === 3, "expected time column + 2 data columns");
  assert(run.payload.simDetails.requested.t_end === 10, "expected requested t_end=10");
  assert(run.metrics.points === 2, `expected 2 points, got ${run.metrics.points}`);
}

function sampleSimulationRun({
  names = ["x"],
  allData = [[0, 1], [1, 2]],
  tEnd = 1,
  dt = null,
  simulateSeconds = 0.01,
  variables = 1,
} = {}) {
  return {
    payload: {
      version: 1,
      names,
      allData,
      nStates: 1,
      variableMeta: [{ name: "x", role: "state", is_state: true }],
      simDetails: {
        requested: {
          solver: "auto",
          t_start: 0,
          t_end: tEnd,
          dt,
          rtol: 1e-6,
          atol: 1e-6,
        },
      },
    },
    metrics: {
      simulateSeconds,
      points: allData[0]?.length ?? 0,
      variables,
    },
  };
}

async function hydrateAndPersistViewerScriptsThroughWorkspaceFs() {
  const workspaceFs = createWorkspaceFilesystem();
  const storage = buildScenarioVisualizationViewStorage({ workspaceFs });
  const views = [
    {
      id: "viewer_3d",
      title: "Viewer",
      type: "3d",
      x: "x",
      y: ["y", "z"],
    },
  ];

  const persisted = await storage.persistViews({
    views,
    model: "Ball",
  });
  const scriptPath = persisted[0].scriptPath;
  assert(scriptPath, "expected persisted 3d view to have a scriptPath");
  const scriptContent = workspaceFs.getFileContent(scriptPath);
  assert(
    typeof scriptContent === "string" && scriptContent.includes("ctx.onInit"),
    "expected default viewer script to be materialized in workspace fs",
  );

  const hydrated = await storage.hydrateViews({
    views: persisted,
    model: "Ball",
  });
  assert(
    typeof hydrated[0].script === "string" && hydrated[0].script.includes("ctx.onFrame"),
    "expected hydrated 3d view to include inline script content",
  );
}

async function persistsSimulationRunsThroughWorkspaceFilesystem() {
  const workspaceFs = createWorkspaceFilesystem();
  const scenarioInterface = createScenarioInterfaceWithVisualizationStore(workspaceFs);
  await scenarioInterface.execute("rumoca.scenario.setVisualizationConfig", {
    model: "Ball",
    views: [
      {
        id: "viewer_3d",
        title: "Viewer",
        type: "3d",
        x: "x",
        y: ["y", "z"],
      },
    ],
  });
  let panelState = {};
  const controller = createResultsPanelController({
    root: {},
    workspaceFs,
    scenarioInterface,
    onStatus() {},
    resultsAppFactory,
    readPanelState() {
      return panelState;
    },
    writePanelState(nextState) {
      panelState = { ...nextState };
    },
  });
  const run = sampleSimulationRun({ simulateSeconds: 0.01 });

  await controller.setSimulationRun("Ball", run);

  const runFiles = workspaceFs.listFiles().filter((entry) =>
    entry.path.startsWith("rumoca-result.") && entry.path.endsWith(".json")
  );
  assert(runFiles.length === 1, `expected one persisted run doc, got ${runFiles.length}`);
  const persistedRunDoc = JSON.parse(runFiles[0].content);
  assert(
    typeof persistedRunDoc.views?.[0]?.script === "string" && persistedRunDoc.views[0].script.includes("ctx.onInit"),
    "expected persisted run doc to hydrate 3d script content",
  );
  assert(
    persistedRunDoc.metrics?.simulateSeconds === 0.01,
    `expected persisted run doc to keep simulateSeconds, got ${persistedRunDoc.metrics?.simulateSeconds}`,
  );

  controller.clear();
  await controller.renderModel("Ball");
  assert(
    globalThis.__lastResultsAppArgs?.payload === null,
    "expected model panel not to reload persisted results without opening the result JSON",
  );
}

async function persistsSeparatePathsForResultSlugCollisions() {
  const workspaceFs = createWorkspaceFilesystem();
  const scenarioInterface = createScenarioInterfaceWithVisualizationStore(workspaceFs);
  let panelState = {};
  const controller = createResultsPanelController({
    root: {},
    workspaceFs,
    scenarioInterface,
    onStatus() {},
    resultsAppFactory,
    readPanelState() {
      return panelState;
    },
    writePanelState(nextState) {
      panelState = { ...nextState };
    },
  });
  const run = sampleSimulationRun({ simulateSeconds: 0.012 });

  await controller.setSimulationRun("A.B", run);
  await controller.setSimulationRun("A B", run);

  const runFiles = workspaceFs.listFiles().filter((entry) =>
    entry.path.startsWith("rumoca-result.") && entry.path.endsWith(".json")
  );
  assertEqual(runFiles.length, 2, "expected two persisted result JSON files");
  const runDocs = runFiles.map((entry) => JSON.parse(entry.content));
  assertNotEqual(
    runDocs[0].runId,
    runDocs[1].runId,
    "expected different run ids for colliding model labels",
  );
}

async function main() {
  simulationPayloadMatchesSharedResultsContract();
  await hydrateAndPersistViewerScriptsThroughWorkspaceFs();
  await persistsSimulationRunsThroughWorkspaceFilesystem();
  await persistsSeparatePathsForResultSlugCollisions();
}

await main();
