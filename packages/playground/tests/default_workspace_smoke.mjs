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

const REQUIRED_CMM_PACKAGE_FILES = [
  "target/cmm/CMM-v0.0.2/LieGroup/package.mo",
  "target/cmm/CMM-v0.0.2/LieGroups/package.mo",
  "target/cmm/CMM-v0.0.2/RigidBody/package.mo",
];

test("default workspace requests only required CMM package files", async () => {
  const originalFetch = globalThis.fetch;
  const requestedUrls = [];
  globalThis.fetch = async (url) => {
    requestedUrls.push(String(url));
    return fakeFetchResponse(url);
  };

  try {
    const entries = await loadDefaultWorkspaceEntries();
    const requestedCmmPaths = requestedUrls
      .map((url) => String(url).replace(/^\.\.\/\.\.\//, "").replace(/^\//, ""))
      .filter((url) => url.startsWith("target/cmm/"));
    assert.deepEqual(
      requestedCmmPaths,
      REQUIRED_CMM_PACKAGE_FILES,
      `unexpected CMM cache request set: ${JSON.stringify(requestedUrls)}`,
    );
    assert.deepEqual(
      entries
        .map((entry) => String(entry.path || ""))
        .filter((path) => path.startsWith("target/cmm/")),
      REQUIRED_CMM_PACKAGE_FILES,
      "default workspace should seed only the required CMM package files",
    );

    const workspaceMetadata = entries.find((entry) => entry.path === "rumoca-workspace.json");
    assert.ok(workspaceMetadata, "expected generated workspace metadata");
    const parsed = JSON.parse(workspaceMetadata.content);
    assert.deepEqual(
      parsed.folders.filter((folder) => String(folder).startsWith("target/cmm")),
      ["target/cmm", "target/cmm/CMM-v0.0.2"],
      "default workspace should list the CMM source-root folders",
    );
  } finally {
    globalThis.fetch = originalFetch;
  }
});
