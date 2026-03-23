import test from "node:test";
import assert from "node:assert/strict";
import path from "node:path";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";

const require = createRequire(import.meta.url);
const testDir = path.dirname(fileURLToPath(import.meta.url));
const shared = require(path.join(testDir, "..", "media", "vendor", "visualization_shared.js"));

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
      scriptPath: ".rumoca/models/by-id/uuid/viewer_3d.js",
    },
  ]);

  assert.deepEqual(views[0].scatterSeries, [
    { name: "x vs time", x: "time", y: "x" },
  ]);
  assert.equal(views[1].scriptPath, ".rumoca/models/by-id/uuid/viewer_3d.js");
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
    /^\.rumoca\/models\/by-id\/[a-z0-9_]+_[0-9a-f]{8}\/viewer_3d\.js$/,
  );
  assert.equal(
    shared.simulationRunDocumentPath("run_123"),
    ".rumoca/results/runs/run_123.json",
  );
  assert.equal(
    shared.latestSimulationResultsIndexPath(),
    ".rumoca/results/index.json",
  );
  assert.match(
    shared.lastSimulationResultPath("Pkg.Ball"),
    /^\.rumoca\/results\/[a-z0-9_]+_[0-9a-f]{8}\.json$/,
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
    writeLastResultTextFile: (resultPath, content) => {
      files.set(resultPath, content);
    },
  });

  const byRunId = await shared.loadHostedSimulationRun({
    runId: persisted.runId,
    readTextFile: (runPath) => files.get(runPath),
  });
  const byModel = await shared.loadHostedSimulationRun({
    model: "Pkg.Ball",
    readTextFile: (runPath) => files.get(runPath),
  });

  assert.equal(byRunId.runId, persisted.runId);
  assert.equal(byRunId.views[0].title, "States Hydrated");
  assert.equal(byModel.metrics.simulateSeconds, 0.25);
  const indexDoc = JSON.parse(files.get(".rumoca/results/index.json"));
  assert.deepEqual(indexDoc.latestRuns, [
    {
      model: "Pkg.Ball",
      runId: persisted.runId,
      savedAtUnixMs: persisted.savedAtUnixMs,
    },
  ]);
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
    writeLastResultTextFile: (resultPath, content) => {
      files.set(resultPath, content);
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
      uplotJs: "uplot.js",
      threeJs: "three.js",
      visualizationSharedJs: "visualization_shared.js",
      resultsAppJs: "results_app.js",
    },
  });

  assert.match(html, /Rumoca Results: Pkg\.Ball/);
  assert.match(html, /results\.request/);
  assert.match(html, /RumocaResultsApp\.createResultsApp/);
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

test("shared results bridge helpers load save and reset project views", async () => {
  let configuredViews = [
    { id: "configured", title: "Configured", type: "timeseries", x: "time", y: ["x"] },
  ];
  const hydrate = async ({ views }) =>
    views.map((view) => ({ ...view, title: `${view.title} Hydrated` }));

  const loaded = await shared.loadHostedProjectResultsViews({
    model: "Pkg.Ball",
    workspaceRoot: "/tmp/workspace",
    loadConfiguredViews: async () => configuredViews,
    hydrateViews: hydrate,
    defaultViews: [{ id: "default", title: "Default", type: "timeseries", x: "time", y: ["x"] }],
  });
  assert.deepEqual(loaded.views, [
    { id: "configured", title: "Configured Hydrated", type: "timeseries", x: "time", y: ["x"] },
  ]);

  const saved = await shared.saveHostedProjectResultsViews({
    model: "Pkg.Ball",
    workspaceRoot: "/tmp/workspace",
    views: [{ id: "next", title: "Next", type: "timeseries", x: "time", y: ["y"] }],
    loadConfiguredViews: async () => configuredViews,
    persistViews: async ({ views }) => views,
    writeConfiguredViews: async ({ views }) => {
      configuredViews = views;
      return true;
    },
    hydrateViews: hydrate,
  });
  assert.deepEqual(saved.views, [
    { id: "next", title: "Next Hydrated", type: "timeseries", x: "time", y: ["y"] },
  ]);

  const reset = await shared.resetHostedProjectResultsViews({
    model: "Pkg.Ball",
    workspaceRoot: "/tmp/workspace",
    loadConfiguredViews: async () => configuredViews,
    writeConfiguredViews: async ({ views }) => {
      configuredViews = views;
      return true;
    },
    hydrateViews: hydrate,
    defaultViews: [{ id: "default", title: "Default", type: "timeseries", x: "time", y: ["x"] }],
  });
  assert.deepEqual(reset.views, [
    { id: "default", title: "Default Hydrated", type: "timeseries", x: "time", y: ["x"] },
  ]);
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

test("shared simulation settings document supports browser hosts and hidden host actions", () => {
  const html = shared.buildHostedSimulationSettingsDocument({
    activeModel: "Pkg.Ball",
    availableModels: ["Pkg.Ball"],
    current: {
      solver: "auto",
      tEnd: 10,
      dt: null,
      outputDir: "",
      sourceRootOverrides: [],
    },
    codegen: {
      mode: "custom",
      builtinTemplateId: "sympy.py.jinja",
      customTemplatePath: "templates/custom.py.jinja",
    },
    codegenTemplates: [
      { id: "sympy.py.jinja", label: "SymPy" },
      { id: "casadi.py.jinja", label: "CasADi" },
    ],
    views: [],
    features: {
      addSourceRootPath: false,
      prepareModels: false,
      resyncSidecars: false,
      workspaceSettings: false,
      userSettings: false,
      openViewScript: false,
    },
  });

  assert.match(html, /RumocaSimulationSettingsHost/);
  assert.match(html, /id="addSourceRootPath" class="ghost" style="display:none;"/);
  assert.match(html, /id="prepareModels" class="secondary" style="display:none;"/);
  assert.match(html, /id="workspaceSettings" class="secondary" style="display:none;"/);
  assert.match(html, /var\(--vscode-foreground, #d4d4d4\)/);
  assert.match(html, /var\(--vscode-input-background, #313131\)/);
  assert.match(html, /<h3>Codegen<\/h3>/);
  assert.match(html, /id="codegenMode"/);
  assert.match(html, /id="codegenBuiltinTemplateId"/);
  assert.match(html, /id="codegenCustomTemplatePath"/);
  assert.match(html, /templates\/custom\.py\.jinja/);
  assert.match(html, /split\(\/\\r\?\\n\|,\/\)/);
  assert.match(html, /split\(\/\\r\?\\n\/\)/);
  assert.match(html, /join\('\\n'\)/);
});

test("shared simulation settings helpers normalize state and build host handlers", async () => {
  const state = shared.normalizeHostedSimulationSettingsState({
    activeModel: "Pkg.Ball",
    availableModels: ["Pkg.Ball"],
    current: {
      solver: "bdf",
      tEnd: "12.5",
      dt: "0.25",
      outputDir: "out",
      sourceRootPaths: ["MSL"],
    },
    views: [{ id: "states_time", title: "States", type: "timeseries", x: "time", y: ["x"] }],
  });
  assert.deepEqual(state.current, {
    solver: "bdf",
    tEnd: 12.5,
    dt: 0.25,
    outputDir: "out",
    sourceRootOverrides: ["MSL"],
  });
  assert.deepEqual(state.codegen, {
    mode: "builtin",
    builtinTemplateId: "sympy.py.jinja",
    customTemplatePath: "",
  });

  const builtState = shared.buildHostedSimulationSettingsState({
    activeModel: "Pkg.Ball",
    availableModels: [],
    current: null,
    fallbackCurrent: {
      solver: "auto",
      tEnd: 10,
      dt: null,
      outputDir: "",
      sourceRootPaths: ["Fallback"],
    },
    fallbackCodegen: {
      mode: "custom",
      builtinTemplateId: "casadi.py.jinja",
      customTemplatePath: "templates/generated.py.jinja",
    },
    codegenTemplates: [
      { id: "sympy.py.jinja", label: "SymPy" },
      { id: "casadi.py.jinja", label: "CasADi" },
    ],
    views: [],
    defaultViews: [{ id: "states_time", title: "States", type: "timeseries", x: "time", y: ["x"] }],
  });
  assert.deepEqual(builtState.availableModels, ["Pkg.Ball"]);
  assert.deepEqual(builtState.current, {
    solver: "auto",
    tEnd: 10,
    dt: null,
    outputDir: "",
    sourceRootOverrides: ["Fallback"],
  });
  assert.deepEqual(builtState.codegen, {
    mode: "custom",
    builtinTemplateId: "casadi.py.jinja",
    customTemplatePath: "templates/generated.py.jinja",
  });
  assert.deepEqual(builtState.codegenTemplates, [
    { id: "sympy.py.jinja", label: "SymPy" },
    { id: "casadi.py.jinja", label: "CasADi" },
  ]);
  assert.equal(builtState.views.length, 1);

  const handlers = shared.buildHostedSimulationSettingsHandlers({
    getActiveModel: () => "Pkg.Ball",
    save: async ({ model, preset, codegenSettings, views }) => ({
      model,
      preset,
      codegenSettings,
      viewCount: views.length,
    }),
    reset: async () => ({
      current: {
        solver: "auto",
        tEnd: 10,
        dt: null,
        outputDir: "",
        sourceRootPaths: ["MSL"],
      },
      codegen: {
        mode: "builtin",
        builtinTemplateId: "casadi.py.jinja",
        customTemplatePath: "",
      },
      views: [],
    }),
  });

  const saved = await handlers.save({
    method: "save",
    payload: {
      solver: "bdf",
      tEnd: 12.5,
      dt: "0.25",
      outputDir: "out",
      sourceRootPaths: ["MSL"],
      codegen: {
        mode: "custom",
        builtinTemplateId: "sympy.py.jinja",
        customTemplatePath: "templates/generated.py.jinja",
      },
      views: [{ id: "states_time", title: "States", type: "timeseries", x: "time", y: ["x"] }],
    },
  });
  assert.deepEqual(saved, {
    model: "Pkg.Ball",
    preset: {
      solver: "bdf",
      tEnd: 12.5,
      dt: 0.25,
      outputDir: "out",
      sourceRootOverrides: ["MSL"],
    },
    codegenSettings: {
      mode: "custom",
      builtinTemplateId: "sympy.py.jinja",
      customTemplatePath: "templates/generated.py.jinja",
    },
    viewCount: 1,
  });

  const reset = await handlers.reset({ method: "reset", payload: {} });
  assert.deepEqual(reset, {
    solver: "auto",
    tEnd: 10,
    dt: null,
    outputDir: "",
    sourceRootPaths: ["MSL"],
    codegen: {
      mode: "builtin",
      builtinTemplateId: "casadi.py.jinja",
      customTemplatePath: "",
    },
    views: [],
  });

  let reopenedModel = null;
  const openHandlers = shared.buildHostedSimulationSettingsHandlers({
    getActiveModel: () => "Pkg.Ball",
    selectModel: async ({ model }) => ({ selectedModel: `${model}.Resolved` }),
    afterOpenModel: async ({ model }) => {
      reopenedModel = model;
    },
  });
  const opened = await openHandlers.openModel({
    method: "openModel",
    payload: {
      model: "Pkg.Other",
    },
  });
  assert.deepEqual(opened, {
    ok: true,
    model: "Pkg.Other.Resolved",
  });
  assert.equal(reopenedModel, "Pkg.Other.Resolved");

  const actionHandlers = shared.buildHostedSimulationSettingsHandlers({
    getActiveModel: () => "Pkg.Ball",
    pickSourceRootPath: async () => "/tmp/MSL",
    resyncSidecars: async () => ({
      remapped_models: 3,
      parse_failures: 1,
    }),
    prepareModels: async () => ({
      preparedModels: ["Pkg.Ball"],
      failures: [{ model: "Pkg.Other", error: "boom" }],
      totalModels: 2,
    }),
    openViewScript: async () => "scripts/viewer_3d.js",
  });
  assert.deepEqual(
    await actionHandlers.pickSourceRootPath({ method: "pickSourceRootPath", payload: {} }),
    { path: "/tmp/MSL" },
  );
  assert.deepEqual(
    await actionHandlers.resyncSidecars({ method: "resyncSidecars", payload: {} }),
    { message: "Resync complete: remapped=3, parseFailures=1" },
  );
  assert.deepEqual(
    await actionHandlers.prepareModels({ method: "prepareModels", payload: {} }),
    { message: "Prepared 1/2 simulation models (1 failed)." },
  );
  assert.deepEqual(
    await actionHandlers.openViewScript({ method: "openViewScript", payload: { viewId: "viewer_3d" } }),
    { path: "scripts/viewer_3d.js" },
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
      uplotJs: "uplot.js",
      threeJs: "three.js",
      visualizationSharedJs: "visualization_shared.js",
      resultsAppJs: "results_app.js",
      resultsAppCss: "results_app.css",
    },
  });

  assert.match(html, /RumocaResultsApp\.createResultsApp/);
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
    scriptPath: ".rumoca/models/by-id/uuid/viewer_3d.js",
  });
  assert.equal(threeD.type, "3d");
  assert.deepEqual(threeD.labels, { x: "time", y: "x", z: "z" });
  assert.deepEqual(threeD.points[0], { x: 0, y: 10, z: 30 });
  assert.deepEqual(threeD.times, [0, 1, 2]);
  assert.equal(threeD.script, "ctx.onFrame = () => {};");
  assert.equal(threeD.scriptPath, ".rumoca/models/by-id/uuid/viewer_3d.js");
});

test("shared project sidecar commands persist and reload simulation settings and views", () => {
  function applySnapshotResponse(snapshot, response) {
    const next = {
      files: [...snapshot.files],
      editorState: structuredClone(snapshot.editorState ?? {}),
    };
    for (const write of response.writes ?? []) {
      const index = next.files.findIndex((file) => file.path === write.path);
      const entry = { path: write.path, content: write.content };
      if (index >= 0) {
        next.files[index] = entry;
      } else {
        next.files.push(entry);
      }
    }
    for (const removePath of response.removes ?? []) {
      next.files = next.files.filter((file) => file.path !== removePath);
    }
    if (Object.prototype.hasOwnProperty.call(response, "editorState")) {
      next.editorState = structuredClone(response.editorState ?? {});
    }
    return next;
  }

  let snapshot = { files: [], editorState: {} };
  const presetResponse = shared.executeHostedProjectSidecarCommand(
    "rumoca.project.setSimulationPreset",
    snapshot,
    {
      model: "Ball",
      preset: { tEnd: 12.5, dt: 0.2 },
    },
  );
  assert.equal(presetResponse.result.ok, true);
  assert.ok(
    presetResponse.writes.some((file) => file.path === ".rumoca/project.toml"),
  );
  assert.ok(
    presetResponse.writes.some((file) => file.path.endsWith("/identity.toml")),
  );
  assert.ok(
    presetResponse.writes.some((file) => file.path.endsWith("/simulation.toml")),
  );
  snapshot = applySnapshotResponse(snapshot, presetResponse);

  const viewsResponse = shared.executeHostedProjectSidecarCommand(
    "rumoca.project.setVisualizationConfig",
    snapshot,
    {
      model: "Ball",
      views: [{
        id: "viewer_3d",
        title: "Viewer",
        type: "3d",
        x: "time",
        y: ["x", "y", "z"],
        scriptPath: ".rumoca/models/by-id/ball_12345678/viewer_3d.js",
      }],
    },
  );
  assert.equal(viewsResponse.result.ok, true);
  assert.ok(
    viewsResponse.writes.some((file) => file.path.endsWith("/views.toml")),
  );
  snapshot = applySnapshotResponse(snapshot, viewsResponse);

  const simulation = shared.executeHostedProjectSidecarCommand(
    "rumoca.project.getSimulationConfig",
    snapshot,
    {
      model: "Ball",
      fallback: { solver: "auto", tEnd: 10, dt: null, outputDir: "", sourceRootPaths: [] },
    },
  ).result;
  assert.equal(simulation.effective.tEnd, 12.5);
  assert.equal(simulation.effective.dt, 0.2);

  const visualization = shared.executeHostedProjectSidecarCommand(
    "rumoca.project.getVisualizationConfig",
    snapshot,
    { model: "Ball" },
  ).result;
  assert.equal(visualization.views.length, 1);
  assert.equal(
    visualization.views[0].scriptPath,
    ".rumoca/models/by-id/ball_12345678/viewer_3d.js",
  );

  const selection = shared.executeHostedProjectSidecarCommand(
    "rumoca.project.setSelectedSimulationModel",
    snapshot,
    { model: "Ball" },
  );
  assert.equal(selection.result.selectedModel, "Ball");
  assert.equal(selection.editorState.selectedSimulationModel, "Ball");
});
