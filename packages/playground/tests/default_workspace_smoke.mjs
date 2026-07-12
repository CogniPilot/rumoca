import test from "node:test";
import assert from "node:assert/strict";

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
