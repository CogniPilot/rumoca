import path from "node:path";
import { createRequire } from "node:module";

const require = createRequire(import.meta.url);
const shared = require(
  path.resolve("crates", "rumoca-viz-web", "web", "visualization_shared.js"),
);

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

// A rum.toml-as-JSON tree with nested interactive-IO, like
// project_get_scenario_config_full returns.
const configTree = {
  rumoca: { version: "1", task: "simulate" },
  model: { name: "Rover", file: "Rover.mo" },
  sim: { solver: "auto", mode: "realtime" },
  input: { keyboard: { keys: { ArrowUp: { action: "set", target: "throttle", value: 1.0 } } } },
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
    path: "rum.Rover.toml",
    config: configTree,
    descriptor: { model: "Rover", task: "simulate" },
  });
  assert(typeof html === "string" && html.includes("<!DOCTYPE html>"), "expected an HTML document");
  assert(html.includes("Rover"), "expected the model name in the header");
  assert(html.includes(">sim<") || html.includes(">sim</h3>"), "expected a sim section card");
  assert(html.includes(">signals<") || html.includes("signals"), "expected the interactive signals section");
  assert(html.includes('id="saveBtn"'), "expected a Save button");
  assert(html.includes('id="rawBtn"'), "expected a Raw TOML toggle");
  assert(html.includes("RumocaScenarioConfigHost"), "expected the host bridge");
  // Deeply nested IO leaf must be rendered as an editable field.
  assert(html.includes("keyboard.keys.ArrowUp.value"), "expected nested IO leaf label");
}

function documentHandlesEmptyConfig() {
  const html = shared.buildScenarioConfigDocument({ path: "rum.toml", config: {} });
  assert(html.includes("no editable fields"), "expected an empty-state message");
}

flattenExposesNestedLeaves();
editsApplyByPathAndPreserveSiblings();
undefinedEditDeletesKey();
documentRendersSectionsAndControls();
documentHandlesEmptyConfig();
console.log("scenario_config_form_smoke: ok");
