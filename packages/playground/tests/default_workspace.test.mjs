import test from "node:test";
import assert from "node:assert/strict";
import { readFile, readdir } from "node:fs/promises";

import { loadDefaultWorkspaceEntries } from "../src/modules/default_workspace.js";

function fakeFetchResponse(path) {
  return {
    ok: true,
    async text() {
      if (String(path).endsWith("examples/models/Ball.mo")) {
        return "model Ball\nend Ball;\n";
      }
      return "";
    },
    async arrayBuffer() {
      return new Uint8Array([0]).buffer;
    },
  };
}

const REQUIRED_CMM_FILES = [
  "target/cmm/CMM-a642c381/LieGroups/package.mo",
  "target/cmm/CMM-a642c381/LieGroups/SE23/package.mo",
  "target/cmm/CMM-a642c381/LieGroups/SE23/Quat/exp_map.mo",
  "target/cmm/CMM-a642c381/LieGroups/SE23/Quat/exp_mixed.mo",
  "target/cmm/CMM-a642c381/LieGroups/SE23/Quat/inverse.mo",
  "target/cmm/CMM-a642c381/LieGroups/SE23/Quat/left_jacobian.mo",
  "target/cmm/CMM-a642c381/LieGroups/SE23/Quat/log_map.mo",
  "target/cmm/CMM-a642c381/LieGroups/SE23/Quat/package.mo",
  "target/cmm/CMM-a642c381/LieGroups/SE23/Quat/product.mo",
  "target/cmm/CMM-a642c381/LieGroups/SE3/package.mo",
  "target/cmm/CMM-a642c381/LieGroups/SE3/Quat/left_Q.mo",
  "target/cmm/CMM-a642c381/LieGroups/SE3/Quat/package.mo",
  "target/cmm/CMM-a642c381/LieGroups/SO3/package.mo",
  "target/cmm/CMM-a642c381/LieGroups/SO3/Quat/inverse.mo",
  "target/cmm/CMM-a642c381/LieGroups/SO3/Quat/exp_map.mo",
  "target/cmm/CMM-a642c381/LieGroups/SO3/Quat/kinematics.mo",
  "target/cmm/CMM-a642c381/LieGroups/SO3/Quat/left_jacobian.mo",
  "target/cmm/CMM-a642c381/LieGroups/SO3/Quat/left_jacobian_inv.mo",
  "target/cmm/CMM-a642c381/LieGroups/SO3/Quat/log_map.mo",
  "target/cmm/CMM-a642c381/LieGroups/SO3/Quat/normalize.mo",
  "target/cmm/CMM-a642c381/LieGroups/SO3/Quat/package.mo",
  "target/cmm/CMM-a642c381/LieGroups/SO3/Quat/product.mo",
  "target/cmm/CMM-a642c381/LieGroups/SO3/Quat/rotate.mo",
  "target/cmm/CMM-a642c381/LieGroups/SO3/Quat/to_DCM.mo",
  "target/cmm/CMM-a642c381/LieGroups/SO3/Quat/wedge.mo",
  "target/cmm/CMM-a642c381/RigidBody/package.mo",
];

test("default workspace seeds every tracked codegen scenario from repository files", async () => {
  const directory = new URL("../../../examples/codegen/", import.meta.url);
  const expected = (await readdir(directory))
    .filter((name) => name.startsWith("rumoca-scenario.") && name.endsWith(".toml"))
    .map((name) => `examples/codegen/${name}`)
    .sort();
  assert.ok(expected.length > 0, "tracked workshop codegen scenarios");
  const originalFetch = globalThis.fetch;
  const originalRepoAssetBase = globalThis.rumocaRepoAssetBase;
  delete globalThis.rumocaRepoAssetBase;
  globalThis.fetch = async (url) => {
    const file = String(url).replace(/^\.\.\/\.\.\//, "").replace(/^\//, "");
    if (!file.startsWith("examples/codegen/")) return fakeFetchResponse(file);
    try {
      const content = await readFile(new URL(file.slice("examples/codegen/".length), directory), "utf8");
      return { ok: true, async text() { return content; } };
    } catch (error) {
      if (error.code !== "ENOENT") throw error;
      return { ok: false };
    }
  };
  try {
    const entries = await loadDefaultWorkspaceEntries();
    const actual = entries.filter((entry) => expected.includes(entry.path));
    assert.deepEqual(actual.map((entry) => entry.path).sort(), expected,
      "every actual scenario must be available in the seeded editor workspace");
    for (const entry of actual) {
      assert.equal(entry.content,
        await readFile(new URL(entry.path.slice("examples/codegen/".length), directory), "utf8"),
        "seeded configuration must preserve the actual tracked file");
      assert.ok(entry.content.includes('task = "codegen"'), "actual codegen configuration");
    }
  } finally {
    globalThis.fetch = originalFetch;
    globalThis.rumocaRepoAssetBase = originalRepoAssetBase;
  }
});

test("default workspace requests only required CMM package files", async () => {
  const originalFetch = globalThis.fetch;
  const originalRepoAssetBase = globalThis.rumocaRepoAssetBase;
  const requestedUrls = [];
  globalThis.fetch = async (url) => {
    requestedUrls.push(String(url));
    return fakeFetchResponse(url);
  };
  delete globalThis.rumocaRepoAssetBase;

  try {
    const entries = await loadDefaultWorkspaceEntries();
    const requestedCmmPaths = requestedUrls
      .map((url) => String(url).replace(/^\.\.\/\.\.\//, "").replace(/^\//, ""))
      .filter((url) => url.startsWith("target/cmm/"));
    assert.deepEqual(
      requestedCmmPaths,
      REQUIRED_CMM_FILES,
      `unexpected CMM cache request set: ${JSON.stringify(requestedUrls)}`,
    );
    assert.deepEqual(
      entries
        .map((entry) => String(entry.path || ""))
        .filter((path) => path.startsWith("target/cmm/")),
      REQUIRED_CMM_FILES,
      "default workspace should seed only the required CMM package files",
    );

    const workspaceMetadata = entries.find((entry) => entry.path === "rumoca-workspace.json");
    assert.ok(workspaceMetadata, "expected generated workspace metadata");
    const parsed = JSON.parse(workspaceMetadata.content);
    assert.deepEqual(
      parsed.folders.filter((folder) => String(folder).startsWith("target/cmm")),
      ["target/cmm", "target/cmm/CMM-a642c381"],
      "default workspace should list the CMM source-root folders",
    );

    const editorStateEntry = entries.find((entry) => entry.path === "rumoca-editor-state.json");
    assert.ok(editorStateEntry, "expected generated editor state");
    const editorState = JSON.parse(editorStateEntry.content);
    assert.ok(
      editorState.explorerCollapsedNodeIds.includes("explorer:examples/interactive/fixedwing"),
      "default workspace should recursively collapse nested example folders",
    );
    assert.ok(
      editorState.explorerCollapsedNodeIds.includes("explorer:target/cmm/CMM-a642c381"),
      "default workspace should recursively collapse nested package folders",
    );
  } finally {
    globalThis.fetch = originalFetch;
    globalThis.rumocaRepoAssetBase = originalRepoAssetBase;
  }
});

test("default workspace fetches from configured repo asset base first", async () => {
  const originalFetch = globalThis.fetch;
  const originalRepoAssetBase = globalThis.rumocaRepoAssetBase;
  const requestedUrls = [];
  globalThis.rumocaRepoAssetBase = "http://localhost:8080/rumoca/";
  globalThis.fetch = async (url) => {
    requestedUrls.push(String(url));
    return fakeFetchResponse(url);
  };

  try {
    await loadDefaultWorkspaceEntries();
    assert.equal(
      requestedUrls[0],
      "http://localhost:8080/rumoca/examples/.gitignore",
      `expected configured asset base to be tried first, got ${requestedUrls[0]}`,
    );
    assert.ok(
      requestedUrls.every((url) => url.startsWith("http://localhost:8080/rumoca/")),
      `expected no fallback fetches after configured asset base succeeds: ${JSON.stringify(requestedUrls)}`,
    );
  } finally {
    globalThis.fetch = originalFetch;
    globalThis.rumocaRepoAssetBase = originalRepoAssetBase;
  }
});
