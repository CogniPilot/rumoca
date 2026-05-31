import test from "node:test";
import assert from "node:assert/strict";

import {
  preferredViewerScriptPathForModel,
  resolvePreferredViewerScriptPath,
} from "../out/results_paths.js";

test("resolvePreferredViewerScriptPath uses the reusable config script default", async () => {
  const resolved = await resolvePreferredViewerScriptPath(
    "/tmp/workspace",
    "Pkg.System.Ball",
    "viewer_3d",
  );

  assert.equal(
    resolved,
    preferredViewerScriptPathForModel("Pkg.System.Ball", "viewer_3d"),
  );
  assert.equal(resolved, "viewer_3d.js");
});

test("resolvePreferredViewerScriptPath still rejects missing workspace roots", async () => {
  await assert.rejects(
    () => resolvePreferredViewerScriptPath(undefined, "Pkg.System.Ball", "viewer_3d"),
    /without a workspace root/,
  );
});
