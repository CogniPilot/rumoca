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

test("GALEC codegen routes through the lazy addon, not the core render_target", () => {
  const renderGalec = workerSource.slice(
    workerSource.indexOf("case 'rumoca.workspace.renderGalec':"),
    workerSource.indexOf("default:", workerSource.indexOf("case 'rumoca.workspace.renderGalec':")),
  );
  assert.match(renderGalec, /renderGalecFilesWithRuntime/);
  assert.match(renderGalec, /pkgBase: '\.\/'/);
  // GALEC must never reach the core DAE-JSON render_target path.
  assert.doesNotMatch(renderGalec, /render_target\(/);
  assert.match(workerSource, /import \{[\s\S]*renderGalecFilesWithRuntime,[\s\S]*\} from '\.\/rumoca_runtime\.js'/);
});

test("worker exposes effective workspace source-root command", () => {
  assert.match(workerSource, /workspace_effective_source_roots = mod\.workspace_effective_source_roots/);
  assert.match(workerSource, /case 'rumoca\.workspace\.effectiveSourceRoots':/);
  assert.match(workerSource, /workspace_effective_source_roots\(\s*payload\.workspaceSources \|\| '\{\}'/);
});

test("parameter metadata loads source roots before compiling full workspace sources", () => {
  const parameterMetadata = workerSource.slice(
    workerSource.indexOf("case 'rumoca.model.parameterMetadata':"),
    workerSource.indexOf("case 'rumoca.scenario.renderDaeText':"),
  );
  assert.match(parameterMetadata, /hasSourceRoots\(payload\.sourceRoots\)/);
  assert.match(parameterMetadata, /load_source_roots\(payload\.sourceRoots\)/);
  assert.match(parameterMetadata, /hasWorkspaceSources\(payload\.workspaceSources\)/);
  assert.match(parameterMetadata, /model_parameter_metadata_with_workspace_sources/);
  assert.match(parameterMetadata, /model_parameter_metadata_with_source_roots/);

  const sourceRootLoad = parameterMetadata.indexOf("load_source_roots(payload.sourceRoots)");
  const workspaceCompile = parameterMetadata.indexOf("model_parameter_metadata_with_workspace_sources");
  const sourceRootOnlyCompile = parameterMetadata.indexOf("model_parameter_metadata_with_source_roots");
  assert(sourceRootLoad >= 0, "expected source roots to be loaded into the session");
  assert(workspaceCompile > sourceRootLoad, "expected workspace-source compile after source-root load");
  assert(
    workspaceCompile < sourceRootOnlyCompile,
    "expected full workspace sources to take precedence over source-root-only metadata",
  );
});
