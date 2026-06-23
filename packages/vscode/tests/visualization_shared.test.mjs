import test from "node:test";
import assert from "node:assert/strict";
import path from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

const testDir = path.dirname(fileURLToPath(import.meta.url));
const shared = await import(pathToFileURL(path.join(testDir, "..", "media", "vendor", "visualization_shared.js")));

test("normalizeVisualizationViews preserves scatter config and canonical scriptPath", () => {
  const views = shared.normalizeVisualizationViews([
    {
      id: "scatter_main",
      title: "Scatter",
      type: "scatter",
      x: "time",
      y: ["x"],
      scatterSeries: [
        { name: "x vs time", x: "time", y: "x" },
      ],
    },
    {
      id: "viewer_3d",
      title: "Viewer",
      type: "3d",
      x: "time",
      y: ["x", "y", "z"],
      scriptPath: "../shared/viewer_3d.js",
    },
  ]);

  assert.deepEqual(views[0].scatterSeries, [
    { name: "x vs time", x: "time", y: "x" },
  ]);
  assert.equal(views[1].scriptPath, "../shared/viewer_3d.js");
  assert.deepEqual(views[1].y, ["x", "y"]);
});

test("shared visualization helpers expose the default 3d viewer script", () => {
  const script = shared.defaultThreeDimensionalViewerScript();
  assert.match(script, /ctx\.onInit/);
  assert.match(script, /ctx\.onFrame/);
  assert.match(script, /selectedObjectName: "ball"/);
  assert.match(script, /followSelected: true/);
  assert.match(script, /api\.getValue\("x"/);
  assert.doesNotMatch(script, /api\.getValue\("y"/);
  assert.doesNotMatch(script, /api\.getValue\("z"/);
});

test("shared visualization helpers expose stable viewer and run artifact paths", () => {
  assert.match(
    shared.preferredViewerScriptPathForModel("Pkg.System.Ball", "viewer_3d"),
    /^viewer_3d\.js$/,
  );
  assert.equal(
    shared.simulationRunDocumentPath("run_123"),
    "results/rumoca-result.run_123.json",
  );
  assert.equal(
    shared.simulationRunDocumentPath("run_123", "out/results"),
    "out/results/rumoca-result.run_123.json",
  );
});

test("shared visualization helpers can persist and reload run documents", async () => {
  const files = new Map();
  const saved = await shared.writePersistedSimulationRunDocument({
    model: "Pkg.Ball",
    payload: {
      version: 1,
      names: ["x"],
      allData: [[0, 1], [2, 3]],
      nStates: 1,
      variableMeta: [],
      simDetails: {},
    },
    metrics: {
      simulateSeconds: 0.25,
      points: 2,
      variables: 1,
    },
    views: [{ id: "states_time", title: "States vs Time", type: "timeseries", x: "time", y: ["x"] }],
    pathExists: (runPath) => files.has(runPath),
    writeTextFile: (runPath, content) => {
      files.set(runPath, content);
    },
  });

  const loaded = await shared.readPersistedSimulationRunDocument({
    runId: saved.runId,
    readTextFile: (runPath) => files.get(runPath),
  });

  assert.equal(loaded.runId, saved.runId);
  assert.match(saved.runPath, /^results\/rumoca-result\.\d{8}T\d{6}Z\.pkgball\.json$/);
  assert.equal(loaded.model, "Pkg.Ball");
  assert.equal(loaded.metrics.simulateSeconds, 0.25);
});

test("shared visualization view storage handlers hydrate persist and remove scripts", async () => {
  const files = new Map();
  const storage = shared.buildVisualizationViewStorageHandlers({
    readTextFile: (scriptPath) => {
      if (!files.has(scriptPath)) {
        throw new Error("missing");
      }
      return files.get(scriptPath);
    },
    writeTextFile: (scriptPath, content) => {
      files.set(scriptPath, content);
    },
    removeTextFile: (scriptPath) => {
      files.delete(scriptPath);
    },
    defaultViewerScript: () => "ctx.onInit(() => {}); ctx.onFrame(() => {});",
  });

  const persisted = await storage.persistViews({
    model: "Pkg.Ball",
    views: [{ id: "viewer_3d", title: "Viewer", type: "3d", x: "time", y: ["x", "y"] }],
  });
  assert.equal(files.size, 1);

  const hydrated = await storage.hydrateViews({
    model: "Pkg.Ball",
    views: persisted,
  });
  assert.match(hydrated[0].script, /ctx\.onFrame/);

  await storage.removeStaleViews({
    previousViews: persisted,
    nextViews: [],
  });
  assert.equal(files.size, 0);
});

test("shared visualization helpers can persist and load hosted simulation runs", async () => {
  const files = new Map();
  const persisted = await shared.persistHostedSimulationRun({
    model: "Pkg.Ball",
    payload: {
      version: 1,
      names: ["x"],
      allData: [[0, 1], [2, 3]],
      nStates: 1,
      variableMeta: [],
      simDetails: {},
    },
    metrics: {
      simulateSeconds: 0.25,
      points: 2,
      variables: 1,
    },
    views: [{ id: "states_time", title: "States", type: "timeseries", x: "time", y: ["x"] }],
    hydrateViews: async ({ views }) =>
      views.map((view) => ({ ...view, title: `${view.title} Hydrated` })),
    pathExists: (runPath) => files.has(runPath),
    writeTextFile: (runPath, content) => {
      files.set(runPath, content);
    },
  });

  const byRunId = await shared.loadHostedSimulationRun({
    runId: persisted.runId,
    readTextFile: (runPath) => files.get(runPath),
  });

  assert.equal(byRunId.runId, persisted.runId);
  assert.equal(byRunId.views[0].title, "States Hydrated");
  assert.equal(files.size, 1);
  assert.equal(files.has(persisted.runPath), true);
});

test("shared visualization helpers can persist and load hosted simulation runs with configured views", async () => {
  const files = new Map();
  let configuredViews = [{ id: "viewer_3d", title: "Viewer", type: "3d", x: "time", y: ["x"] }];

  const persisted = await shared.persistHostedSimulationRunWithViews({
    model: "Pkg.Ball",
    workspaceRoot: "/tmp/workspace",
    payload: {
      version: 1,
      names: ["x"],
      allData: [[0, 1], [1, 2]],
      nStates: 1,
      variableMeta: [],
      simDetails: {},
    },
    metrics: {
      simulateSeconds: 0.25,
      points: 2,
      variables: 1,
    },
    loadConfiguredViews: async () => configuredViews,
    hydrateViews: async ({ views }) =>
      views.map((view) => ({ ...view, title: `${view.title} Hydrated`, script: "ctx.onInit(() => {});" })),
    defaultViews: [{ id: "states_time", title: "States", type: "timeseries", x: "time", y: ["x"] }],
    pathExists: (runPath) => files.has(runPath),
    writeTextFile: (runPath, content) => {
      files.set(runPath, content);
    },
  });

  const restored = await shared.loadHostedSimulationRunWithViews({
    model: "Pkg.Ball",
    runId: persisted.runId,
    workspaceRoot: "/tmp/workspace",
    readTextFile: (runPath) => files.get(runPath),
    loadConfiguredViews: async () => configuredViews,
    hydrateViews: async ({ views }) =>
      views.map((view) => ({ ...view, title: `${view.title} Hydrated Again` })),
    defaultViews: [{ id: "states_time", title: "States", type: "timeseries", x: "time", y: ["x"] }],
  });

  assert.equal(restored.run.runId, persisted.runId);
  assert.equal(restored.views[0].title, "Viewer Hydrated Again");
  assert.equal(restored.run.metrics.simulateSeconds, 0.25);
});

test("shared visualization helpers can build the hosted results document", () => {
  const html = shared.buildHostedResultsDocument({
    model: "Pkg.Ball",
    payload: null,
    views: [],
    metrics: null,
    panelState: { runId: "run_123" },
    assets: {
      uplotCss: "uplot.css",
      resultsAppCss: "results_app.css",
      resultsAppJs: "results_app.js",
    },
  });

  assert.match(html, /Rumoca Results: Pkg\.Ball/);
  assert.match(html, /results\.request/);
  assert.match(html, /createResultsApp/);
  assert.match(html, /await import\("results_app\.js"\)/);
});

test("shared visualization helpers normalize hosted results panel state", () => {
  assert.deepEqual(
    shared.buildHostedResultsPanelState({
      runId: " run_123 ",
      model: " Pkg.Ball ",
      workspaceRoot: " /tmp/workspace ",
      title: " Rumoca Results: Pkg.Ball ",
      activeViewId: " viewer_3d ",
    }),
    {
      version: 1,
      runId: "run_123",
      model: "Pkg.Ball",
      workspaceRoot: "/tmp/workspace",
      title: "Rumoca Results: Pkg.Ball",
      activeViewId: "viewer_3d",
    },
  );
  assert.deepEqual(
    shared.normalizeHostedResultsPanelState(
      {
        runId: "run_123",
        model: "Pkg.Ball",
      },
      "/tmp/workspace",
    ),
    {
      version: 1,
      runId: "run_123",
      model: "Pkg.Ball",
      workspaceRoot: "/tmp/workspace",
      title: undefined,
      activeViewId: undefined,
    },
  );
  assert.equal(
    shared.normalizeHostedResultsPanelState({ model: "Pkg.Ball" }, "/tmp/workspace"),
    undefined,
  );
});

test("shared visualization helpers build hosted results panel titles", () => {
  assert.equal(
    shared.buildHostedResultsPanelTitle({ unavailable: true }),
    "Rumoca Results (Unavailable)",
  );
  assert.equal(
    shared.buildHostedResultsPanelTitle({ model: "Pkg.Ball", missingRun: true }),
    "Rumoca Results: Pkg.Ball (Missing Run)",
  );
  assert.equal(
    shared.buildHostedResultsPanelTitle({ model: "Pkg.Ball", timestamp: "12:34:56" }),
    "Rumoca Results: Pkg.Ball (12:34:56)",
  );
  assert.equal(
    shared.buildHostedResultsPanelTitle({ model: "Pkg.Ball", title: " Custom Title " }),
    "Custom Title",
  );
});

test("shared visualization helpers can route hosted results requests", async () => {
  const sent = [];
  const handled = await shared.handleHostedResultsRequest({
    message: {
      command: "results.request",
      requestId: "1",
      method: "loadViews",
      payload: {
        modelRef: {
          model: "Pkg.Ball",
        },
      },
    },
    fallbackWorkspaceRoot: "/tmp/workspace",
    postMessage: async (message) => {
      sent.push(message);
    },
    handlers: {
      loadViews: async ({ modelRef }) => ({
        okModel: modelRef.model,
        workspaceRoot: modelRef.workspaceRoot,
      }),
    },
  });

  assert.equal(handled, true);
  assert.deepEqual(sent, [{
    command: "results.response",
    requestId: "1",
    ok: true,
    value: {
      okModel: "Pkg.Ball",
      workspaceRoot: "/tmp/workspace",
    },
    error: undefined,
  }]);
});

test("shared results helpers normalize export and notify payloads", () => {
  assert.deepEqual(
    shared.normalizeHostedPngExportRequest({
      dataUrl: "data:image/png;base64,QUJD",
      defaultName: "My Plot",
    }),
    {
      base64: "QUJD",
      defaultName: "My_Plot.png",
    },
  );
  assert.deepEqual(
    shared.normalizeHostedWebmExportRequest({
      dataUrl: "data:video/webm;codecs=vp9;base64,REVG",
      defaultName: "Movie Export",
    }),
    {
      base64: "REVG",
      defaultName: "Movie_Export.webm",
    },
  );
  assert.deepEqual(
    shared.normalizeHostedResultsNotifyPayload({ message: "  done  " }),
    { message: "done" },
  );
});

test("shared visualization helpers build the hosted results document", () => {
  const html = shared.buildHostedResultsDocument({
    model: "Pkg.Ball",
    payload: null,
    views: [],
    metrics: null,
    panelState: { runId: "run_123" },
    assets: {
      uplotCss: "uplot.css",
      resultsAppJs: "results_app.js",
      resultsAppCss: "results_app.css",
    },
  });

  assert.match(html, /createResultsApp/);
  assert.match(html, /results\.request/);
  assert.match(html, /results_app\.js/);
});

test("shared run document helpers normalize payload, metrics, and views", () => {
  const runDoc = shared.buildSimulationRunDocument({
    runId: "123_Ball",
    model: "Pkg.Ball",
    savedAtUnixMs: 123,
    payload: {
      version: 1,
      names: ["x", "y"],
      allData: [[0, 1], [2, 3]],
      nStates: 1,
      variableMeta: [],
      simDetails: { actual: { points: 2 } },
    },
    metrics: {
      compileSeconds: 0.5,
      simulateSeconds: 0.25,
      points: 2,
      variables: 2,
      compilePhaseSeconds: {
        instantiate: 0.1,
        typecheck: 0.1,
        flatten: 0.2,
        todae: 0.1,
      },
    },
    views: [{ id: "states_time", title: "States vs Time", type: "timeseries", x: "time", y: ["*states"] }],
  });

  assert.equal(runDoc.runId, "123_Ball");
  assert.equal(runDoc.model, "Pkg.Ball");
  assert.equal(runDoc.metrics.compileSeconds, 0.5);
  assert.equal(runDoc.payload.nStates, 1);

  const normalized = shared.normalizePersistedSimulationRun(runDoc);
  assert.equal(normalized.runId, "123_Ball");
  assert.equal(normalized.views[0].id, "states_time");
});

test("shared run metrics allow simulate-only timings for wasm-hosted runs", () => {
  const metrics = shared.normalizeSimulationRunMetrics({
    simulateSeconds: 0.25,
    points: 2,
    variables: 2,
  });

  assert.deepEqual(metrics, {
    simulateSeconds: 0.25,
    points: 2,
    variables: 2,
  });
});

test("buildVisualizationModel uses canonical persisted payload shape", () => {
  const payload = shared.normalizeSimulationPayload({
    version: 1,
    names: ["x", "y", "z"],
    allData: [
      [0, 1, 2],
      [10, 11, 12],
      [20, 21, 22],
      [30, 31, 32],
    ],
    nStates: 2,
    variableMeta: [],
    simDetails: {},
  });

  const timeseries = shared.buildVisualizationModel(payload, {
    id: "states_time",
    title: "States vs Time",
    type: "timeseries",
    x: "time",
    y: ["*states"],
  });
  assert.equal(timeseries.type, "timeseries");
  assert.equal(timeseries.x.name, "time");
  assert.deepEqual(timeseries.x.values, [0, 1, 2]);
  assert.deepEqual(timeseries.y.map((series) => series.name), ["x", "y"]);
  assert.deepEqual(timeseries.y[0].values, [10, 11, 12]);

  const threeD = shared.buildVisualizationModel(payload, {
    id: "viewer_3d",
    title: "Viewer",
    type: "3d",
    x: "time",
    y: ["x", "z"],
    script: "ctx.onFrame = () => {};",
    scriptPath: "../shared/viewer_3d.js",
  });
  assert.equal(threeD.type, "3d");
  assert.deepEqual(threeD.labels, { x: "time", y: "x", z: "z" });
  assert.deepEqual(threeD.points[0], { x: 0, y: 10, z: 30 });
  assert.deepEqual(threeD.times, [0, 1, 2]);
  assert.equal(threeD.script, "ctx.onFrame = () => {};");
  assert.equal(threeD.scriptPath, "../shared/viewer_3d.js");
});

test("shared module does not expose generated config side-channel commands", () => {
  assert.equal(shared.executeHostedGeneratedConfigCommand, undefined);
});
