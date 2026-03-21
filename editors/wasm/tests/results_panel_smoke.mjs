import { createRequire } from "node:module";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { createProjectFilesystem } from "../src/modules/project_fs.js";
import { createProjectInterface } from "../src/modules/project_interface.js";
import {
  buildProjectVisualizationViewStorage,
  createResultsPanelController,
} from "../src/modules/results_panel.js";

const require = createRequire(import.meta.url);
const __dirname = path.dirname(fileURLToPath(import.meta.url));
globalThis.RumocaVisualizationShared = require(
  path.resolve(__dirname, "..", "..", "..", "crates", "rumoca-sim", "web", "visualization_shared.js"),
);
globalThis.RumocaResultsApp = {
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

async function hydrateAndPersistViewerScriptsThroughProjectFs() {
  const projectFs = createProjectFilesystem();
  const storage = buildProjectVisualizationViewStorage({ projectFs });
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
  const scriptContent = projectFs.getFileContent(scriptPath);
  assert(
    typeof scriptContent === "string" && scriptContent.includes("ctx.onInit"),
    "expected default viewer script to be materialized in project fs",
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

async function persistsSimulationRunsThroughProjectFilesystem() {
  const projectFs = createProjectFilesystem();
  const projectInterface = createProjectInterface({ projectFs });
  projectInterface.execute("rumoca.project.setVisualizationConfig", {
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
    projectFs,
    projectInterface,
    onStatus() {},
    readPanelState() {
      return panelState;
    },
    writePanelState(nextState) {
      panelState = { ...nextState };
    },
  });
  const run = sampleSimulationRun({ simulateSeconds: 0.01 });

  await controller.setSimulationRun("Ball", run);

  const resultEntries = projectFs.listFiles().filter(
    (entry) => entry.path.startsWith(".rumoca/results/") && !entry.path.startsWith(".rumoca/results/runs/"),
  );
  assertDeepEqual(
    resultEntries.map((entry) => entry.path),
    [".rumoca/results/index.json"],
    `expected only results index sidecar, got ${resultEntries.map((entry) => entry.path).join(", ")}`,
  );
  const indexDoc = JSON.parse(projectFs.getFileContent(resultEntries[0].path));
  assertEqual(indexDoc.latestRuns?.length, 1, "expected one latest-run index entry");
  assertEqual(indexDoc.latestRuns?.[0]?.model, "Ball");
  const runFiles = projectFs.listFiles().filter((entry) => entry.path.startsWith(".rumoca/results/runs/"));
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
  assert(globalThis.__lastResultsAppArgs?.payload?.nStates === 1, "expected persisted payload to be reloaded");
  assert(
    globalThis.__lastResultsAppArgs?.metrics?.simulateSeconds === 0.01,
    "expected persisted metrics to be reloaded into shared results app",
  );
}

async function persistsSeparatePathsForResultSlugCollisions() {
  const projectFs = createProjectFilesystem();
  const projectInterface = createProjectInterface({ projectFs });
  let panelState = {};
  const controller = createResultsPanelController({
    root: {},
    projectFs,
    projectInterface,
    onStatus() {},
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

  const resultEntries = projectFs.listFiles().filter(
    (entry) => entry.path.startsWith(".rumoca/results/") && !entry.path.startsWith(".rumoca/results/runs/"),
  );
  assertDeepEqual(
    resultEntries.map((entry) => entry.path),
    [".rumoca/results/index.json"],
    `expected only one results index file, got ${resultEntries.map((entry) => entry.path).join(", ")}`,
  );
  const indexDoc = JSON.parse(resultEntries[0].content);
  assertEqual(indexDoc.latestRuns?.length, 2, "expected two latest-run entries in the shared index");
  assertNotEqual(
    indexDoc.latestRuns[0].runId,
    indexDoc.latestRuns[1].runId,
    "expected different run ids for colliding model labels",
  );
}

async function main() {
  simulationPayloadMatchesSharedResultsContract();
  await hydrateAndPersistViewerScriptsThroughProjectFs();
  await persistsSimulationRunsThroughProjectFilesystem();
  await persistsSeparatePathsForResultSlugCollisions();
}

await main();
