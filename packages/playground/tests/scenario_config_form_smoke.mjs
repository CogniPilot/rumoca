import path from "node:path";
import { pathToFileURL } from "node:url";
import { fileURLToPath } from "node:url";

const repoRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..", "..");
const shared = await import(pathToFileURL(path.join(
  repoRoot,
  "packages",
  "rumoca-web",
  "viz",
  "visualization_shared.js",
)));

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

// A rumoca-scenario.toml-as-JSON tree with nested interactive-IO, like
// scenario_get_scenario_config_full returns from the current WASM ABI.
const configTree = {
  rumoca: { version: "1", task: "simulate" },
  model: { name: "Rover", file: "Rover.mo" },
  sim: { solver: "auto", mode: "realtime" },
  input: {
    keyboard: {
      decay: { factor: 0.85, ref_dt: 0.016, targets: ["throttle"] },
      integrators: {
        throttle: { source: "local:throttle_input", write: "throttle", rate: 0.7, clamp: [0.0, 1.0] },
      },
      keys: { ArrowUp: { action: "set", target: "throttle", value: 1.0 } },
    },
    gamepad: {
      integrators: {
        throttle: { source: "LeftStickY", write: "throttle", deadband: 0.1, rate: 0.7, clamp: [0.0, 1.0] },
      },
    },
  },
  signals: { viewer: { theta: "model:theta" } },
};

function flattenExposesNestedLeaves() {
  const fields = shared.flattenScenarioConfig(configTree);
  const byLabel = new Map(fields.map((field) => [field.label, field]));

  assert(byLabel.get("sim.solver")?.value === "auto", "expected sim.solver leaf");
  assert(byLabel.get("sim.solver")?.kind === "string", "expected string kind");
  assert(byLabel.get("sim.solver")?.section === "sim", "expected top-level section");
  assert(
    byLabel.get("input.keyboard.keys.ArrowUp.value")?.value === 1.0,
    "expected deeply nested interactive-IO leaf",
  );
  assert(
    byLabel.get("input.keyboard.keys.ArrowUp.value")?.kind === "number",
    "expected number kind for numeric leaf",
  );
  assert(
    byLabel.get("signals.viewer.theta")?.value === "model:theta",
    "expected signal-routing leaf",
  );
}

function editsApplyByPathAndPreserveSiblings() {
  const updated = shared.applyScenarioConfigEdits(configTree, [
    { path: ["sim", "solver"], value: "bdf" },
    { path: ["sim", "t_end"], value: 5.0 },
  ]);

  assert(updated.sim.solver === "bdf", "expected solver updated");
  assert(updated.sim.t_end === 5.0, "expected new key added");
  assert(updated.sim.mode === "realtime", "expected sibling preserved");
  // Deeply nested IO untouched.
  assert(
    updated.input.keyboard.keys.ArrowUp.target === "throttle",
    "expected nested IO preserved through edits",
  );
  // Original tree not mutated (structural sharing / immutability).
  assert(configTree.sim.solver === "auto", "expected original tree unchanged");
}

function undefinedEditDeletesKey() {
  const updated = shared.applyScenarioConfigEdits(configTree, [
    { path: ["sim", "mode"], value: undefined },
  ]);
  assert(!("mode" in updated.sim), "expected key removed when value is undefined");
  assert(configTree.sim.mode === "realtime", "expected original tree unchanged");
}

function documentRendersSectionsAndControls() {
  const html = shared.buildScenarioConfigDocument({
    path: "rumoca-scenario.Rover.toml",
    config: configTree,
    descriptor: { model: "Rover", task: "simulate" },
  });
  assert(typeof html === "string" && html.includes("<!DOCTYPE html>"), "expected an HTML document");
  assert(html.includes("Rover"), "expected the model name in the header");
  assert(html.includes('data-scenario-section="sim"'), "expected a sim section card");
  assert(html.includes('id="field_4"'), "expected typed field ids");
  assert(html.includes("<select"), "expected dropdowns for finite choices");
  assert(html.includes('<option value="auto" selected>auto</option>'), "expected solver dropdown");
  assert(html.includes('id="plotViewsCard"'), "expected typed plot editor");
  assert(html.includes('data-view-field="type"'), "expected plot type dropdown");
  assert(html.includes('data-view-field="scriptPath"'), "expected viewer script path control");
  assert(!html.includes('data-view-field="script"'), "expected scenario GUI not to expose viewer script text");
  assert(html.includes('data-add-path'), "expected source roots to use add-row controls");
  assert(html.includes('data-browse-path'), "expected source roots to expose a browse action");
  assert(html.includes('data-input-enabled'), "expected input mapping to be explicitly enabled");
  assert(html.includes('data-add-keyboard-key'), "expected typed keyboard input mapping controls");
  assert(html.includes('data-add-keyboard-integrator'), "expected typed keyboard integrator controls");
  assert(html.includes('"keyboardDecay"'), "expected keyboard decay to be preserved in scenario state");
  assert(html.includes('data-add-gamepad-axis'), "expected typed gamepad input mapping controls");
  assert(html.includes('data-add-gamepad-integrator'), "expected typed gamepad integrator controls");
  assert(html.includes('data-add-model-input'), "expected typed Modelica input routing controls");
  assert(html.includes('id="parametersSummary"'), "expected parameter summary to be refreshable");
  assert(html.includes('id="parametersList"'), "expected parameter list to be refreshable");
  assert(html.includes("requestHost('parameterMetadata'"), "expected parameter metadata refresh from the shared form");
  assert(html.includes('id="summaryModel"'), "expected runnable scenario summary model field");
  assert(html.includes('id="summaryStatus"'), "expected runnable scenario status summary");
  assert(html.includes('data-run-step="running"'), "expected visible run progress strip");
  assert(html.includes(">Run Simulation</button>"), "expected primary action to be user-goal oriented");
  assert(!html.includes("initial_selection"), "expected plot defaults to stay out of the main GUI");
  assert(!html.includes("show_sidebar"), "expected plot sidebar defaults to stay out of the main GUI");
  assert(html.includes('id="saveBtn"'), "expected a Save button");
  assert(html.includes('id="rawBtn"'), "expected a Raw TOML toggle");
  assert(html.includes("RumocaScenarioConfigHost"), "expected the host bridge");
  assert(!html.includes('data-kind="json"'), "normal scenario GUI should not expose JSON text blobs");
}

function documentHandlesEmptyConfig() {
  const html = shared.buildScenarioConfigDocument({ path: "rumoca-scenario.toml", config: {} });
  assert(html.includes("Scenario"), "expected default scenario title");
  assert(html.includes("<select"), "expected default typed controls");
}

function documentUsesPlaygroundThemeTokens() {
  const html = shared.buildScenarioConfigDocument({
    path: "rumoca-scenario.toml",
    config: configTree,
    theme: "light",
  });
  assert(html.includes('<body data-theme="light">'), "expected playground theme on scenario body");
  assert(html.includes('body[data-theme="light"]'), "expected playground light theme CSS");
  assert(
    html.includes("--vscode-editor-background: #f8fafc"),
    "expected scenario form to use the playground light background",
  );
}

function threeDPanelsAreScriptDrivenWithNoYEntries() {
  // A 3D panel is a script-driven THREE.js scene: it has no y-entry concept.
  // Any inherited y (including the timeseries default *states, or explicit
  // channels) must normalize to an empty list so the panel stays valid and
  // never blocks Run/Save on a presentation setting.
  const normalized = shared.normalizeVisualizationViews([
    { id: "viewer_1", title: "3D View", type: "3d", x: "time", y: ["*states"], scriptPath: "viewer.js" },
    { id: "viewer_2", title: "3D Explicit", type: "3d", x: "time", y: ["x", "y"] },
    { id: "states_time", title: "States vs Time", type: "timeseries", x: "time", y: ["*states"] },
    { id: "scatter_1", title: "Scatter", type: "scatter", x: "time", y: ["x"] },
  ]);
  const byId = new Map(normalized.map((view) => [view.id, view]));

  const wildcard3d = byId.get("viewer_1");
  assert(Array.isArray(wildcard3d.y) && wildcard3d.y.length === 0, "expected 3D wildcard y to normalize to []");
  assert(wildcard3d.type === "3d", "expected 3D type preserved");
  assert(wildcard3d.scriptPath === "viewer.js", "expected 3D viewer script path preserved");

  const explicit3d = byId.get("viewer_2");
  assert(Array.isArray(explicit3d.y) && explicit3d.y.length === 0, "expected explicit 3D y entries to normalize to []");

  // Non-3D panels keep their y series unchanged.
  assert(
    JSON.stringify(byId.get("states_time").y) === JSON.stringify(["*states"]),
    "expected timeseries y to keep *states",
  );
  assert(
    JSON.stringify(byId.get("scatter_1").y) === JSON.stringify(["x"]),
    "expected scatter y unchanged",
  );
}

function scenarioFormNeverBlocksOnThreeDPresentation() {
  // The webview form logic lives inside the generated document. Assert the
  // scenario editor no longer hard-fails a 3D panel over y entries, normalizes
  // 3D y to [], hides the y series field for 3D, and clears the y field when a
  // panel is switched to 3D so it starts script-driven instead of inheriting
  // the timeseries *states default.
  const html = shared.buildScenarioConfigDocument({
    path: "rumoca-scenario.Rover.toml",
    config: configTree,
    descriptor: { model: "Rover", task: "simulate" },
  });
  assert(
    !html.includes("3D panels need two y entries"),
    "expected the blocking 3D two-y validation to be removed",
  );
  assert(
    !html.includes("y.length < 2"),
    "expected no residual 3D y-count validation",
  );
  assert(
    html.includes("y: type === '3d' ? [] : y"),
    "expected collectPlotViews to normalize 3D y to []",
  );
  assert(
    html.includes("data-view-kind=\"series\"") && html.includes("field.hidden = type === '3d'"),
    "expected the y series field to be hidden for 3D panels",
  );
  assert(
    html.includes("yEl.value = ''"),
    "expected switching a panel to 3D to clear the y field",
  );
  // Timeseries still requires at least one y series (validation not weakened).
  assert(
    html.includes("Add at least one y series for each plot panel."),
    "expected timeseries panels to still require a y series",
  );
}

function threeDViewerRuntimeResolvesOutputChannelsByName() {
  // The 3D viewer script reads channels through api.getValue(name, sampleIndex),
  // which resolves against payload.names (states AND outputs) and payload.allData
  // via names.indexOf(name) -> allData[index + 1]. buildVisualizationModel and
  // resolveSeries share that exact indexing, so probe it to confirm a non-state
  // output channel is readable by name.
  const payload = {
    names: ["h", "v_out"],
    nStates: 1,
    allData: [
      [0, 1, 2], // time
      [10, 11, 12], // h (state)
      [0.5, 0.6, 0.7], // v_out (output, beyond nStates)
    ],
  };
  assert(
    JSON.stringify(shared.availableStateNames(payload)) === JSON.stringify(["h"]),
    "expected h exposed as a state channel",
  );
  assert(
    JSON.stringify(shared.availableOutputNames(payload)) === JSON.stringify(["v_out"]),
    "expected v_out exposed as an output channel by name",
  );

  const model = shared.buildVisualizationModel(payload, { type: "3d", x: "time", y: ["v_out", "h"] });
  assert(model.type === "3d", "expected a 3D visualization model");
  assert(Array.isArray(model.points) && model.points.length === 3, "expected resolved 3D points");
  // The non-state output channel v_out resolves by name to its sampled values.
  assert(model.points[0].y === 0.5 && model.points[2].y === 0.7, "expected output channel v_out read by name");
  assert(model.points[0].z === 10 && model.points[2].z === 12, "expected state channel h read by name");
}

flattenExposesNestedLeaves();
editsApplyByPathAndPreserveSiblings();
undefinedEditDeletesKey();
documentRendersSectionsAndControls();
documentHandlesEmptyConfig();
documentUsesPlaygroundThemeTokens();
threeDPanelsAreScriptDrivenWithNoYEntries();
scenarioFormNeverBlocksOnThreeDPresentation();
threeDViewerRuntimeResolvesOutputChannelsByName();
console.log("scenario_config_form_smoke: ok");
