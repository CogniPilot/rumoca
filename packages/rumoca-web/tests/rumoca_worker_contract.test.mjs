import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import test from "node:test";

const workerSource = fs.readFileSync(
  path.resolve("runtime", "rumoca_worker.js"),
  "utf8",
);

test("BDF simulations sync workspace sources before the diffsol runtime path", () => {
  const startSimulation = workerSource.slice(
    workerSource.indexOf("case 'rumoca.scenario.startSimulation':"),
    workerSource.indexOf("case 'rumoca.workspace.getVersion':"),
  );
  assert.match(startSimulation, /simulate_model_with_workspace_sources/);
  assert.match(startSimulation, /solver === 'bdf'/);
  assert.match(startSimulation, /syncWorkspaceSources\(payload\.workspaceSources\)/);
  assert.match(startSimulation, /load_source_roots\(payload\.sourceRoots\)/);
  assert.match(startSimulation, /simulateModelWithRuntime/);

  const bdfBranch = startSimulation.slice(
    startSimulation.indexOf("solver === 'bdf'"),
    startSimulation.indexOf('} else if (useWorkspaceSources)'),
  );
  assert.doesNotMatch(bdfBranch, /simulate_model_with_workspace_sources/);
});

test("worker exposes effective workspace source-root command", () => {
  assert.match(workerSource, /workspace_effective_source_roots = mod\.workspace_effective_source_roots/);
  assert.match(workerSource, /case 'rumoca\.workspace\.effectiveSourceRoots':/);
  assert.match(workerSource, /workspace_effective_source_roots\(\s*payload\.workspaceSources \|\| '\{\}'/);
});
