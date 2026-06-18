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
  signals: { viewer: { theta: "stepper:theta" } },
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
    byLabel.get("signals.viewer.theta")?.value === "stepper:theta",
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
  assert(html.includes('data-add-gamepad-axis'), "expected typed gamepad input mapping controls");
  assert(html.includes('data-add-gamepad-integrator'), "expected typed gamepad integrator controls");
  assert(html.includes('data-add-stepper-input'), "expected typed Modelica input routing controls");
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

flattenExposesNestedLeaves();
editsApplyByPathAndPreserveSiblings();
undefinedEditDeletesKey();
documentRendersSectionsAndControls();
documentHandlesEmptyConfig();
console.log("scenario_config_form_smoke: ok");
