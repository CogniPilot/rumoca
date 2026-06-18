import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import path from "node:path";
import test from "node:test";

const repoRoot = path.resolve(import.meta.dirname, "../../..");

async function readRepoFile(relativePath) {
  return readFile(path.join(repoRoot, relativePath), "utf8");
}

test("book live runner merges inherited workspace roots outside scenario settings", async () => {
  const liveSource = await readRepoFile("docs/user-guide/live/rumoca-live.js");

  assert.match(liveSource, /loadWorkspaceSourcesForFocus/);
  assert.match(liveSource, /wasm\.workspace_effective_source_roots/);

  const sourceRootPathsStart = liveSource.indexOf("async sourceRootPaths(wasm)");
  const sourceRootPaths = liveSource.slice(
    sourceRootPathsStart,
    liveSource.indexOf("sourceRootCacheUrl(wasm)", sourceRootPathsStart)
  );
  const workspaceIndex = sourceRootPaths.indexOf("widget.workspaceSourceRootPaths(wasm)");
  const scenarioIndex = sourceRootPaths.indexOf("widget.scenarioSourceRootPaths()");
  assert(
    workspaceIndex >= 0 && scenarioIndex > workspaceIndex,
    "effective source roots should merge workspace roots before scenario-local roots"
  );

  const settingsFrame = liveSource.slice(
    liveSource.indexOf("async function renderScenarioSettingsFrame()"),
    liveSource.indexOf("function renderScenarioRawSettings")
  );
  assert.match(
    settingsFrame,
    /effectiveSourceRootPaths,\s*\}\)/,
    "book settings should pass resolved roots as read-only context to the shared GUI"
  );
});

test("book live runner allows dependency-free examples without staged roots", async () => {
  const liveSource = await readRepoFile("docs/user-guide/live/rumoca-live.js");
  const workspaceRoots = liveSource.slice(
    liveSource.indexOf("async workspaceSourceRootPaths(wasm)"),
    liveSource.indexOf("async sourceRootPaths(wasm)")
  );
  assert.match(
    workspaceRoots,
    /if \(Object\.keys\(workspaceSources\)\.length === 0\) \{\s+return \[\];\s+\}/,
    "missing staged workspace config should not block self-contained examples"
  );
  const sourceRootPathsStart = liveSource.indexOf("async sourceRootPaths(wasm)");
  const sourceRootPaths = liveSource.slice(
    sourceRootPathsStart,
    liveSource.indexOf("sourceRootCacheUrl(wasm)", sourceRootPathsStart)
  );
  assert.match(
    sourceRootPaths,
    /if \(widget\.scenarioSourceRootPaths\(\)\.length > 0 && sourceRoots\.length === 0\)/,
    "empty effective roots should only be fatal when the scenario declares source_roots"
  );
});

test("book live runner round-trips scenario TOML through WASM", async () => {
  const liveSource = await readRepoFile("docs/user-guide/live/rumoca-live.js");

  assert.doesNotMatch(liveSource, /function parseSimpleToml/);
  assert.doesNotMatch(liveSource, /function patchTomlKey/);
  assert.match(liveSource, /scenario_get_scenario_config_full/);
  assert.match(liveSource, /scenario_set_scenario_config/);

  const applyScenario = liveSource.slice(
    liveSource.indexOf("async function applyScenarioText"),
    liveSource.indexOf("async function loadScenarioSource")
  );
  assert.match(applyScenario, /parseScenarioTextWithWasm/);
  assert.match(applyScenario, /scenarioConfig = parsed\.config/);
});

test("docs staging writes workspace config and strips generated scenario roots", async () => {
  const docsCmd = await readRepoFile("crates/xtask/src/docs_cmd.rs");

  assert.match(docsCmd, /write_user_guide_workspace_config/);
  assert.match(docsCmd, /WORKSPACE_CONFIG_FILE/);
  assert.match(docsCmd, /strip_staged_scenario_source_roots/);
  assert.match(docsCmd, /table\.remove\("source_roots"\)/);
});
