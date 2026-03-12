import test from "node:test";
import assert from "node:assert/strict";
import path from "node:path";

import {
  changedRumocaRestartKeys,
  notebookExecutableFromServerPath,
  resolveModelicaPathSources,
} from "../out/modelica_paths.js";

test("resolveModelicaPathSources expands home paths and preserves relative entries", () => {
  const homeDir = path.join(path.sep, "tmp", "rumoca-home");
  const envDir = path.join(path.sep, "opt", "modelica");
  const legacyDir = path.join(path.sep, "legacy", "modelica");
  const relativeLib = path.join("vendor", "Modelica");
  const sources = resolveModelicaPathSources(
    ["~/ModelicaStandardLibrary", relativeLib, "~/ModelicaStandardLibrary"],
    {
      MODELICAPATH: [envDir, envDir].join(path.delimiter),
      MODELICPATH: legacyDir,
    },
    homeDir,
  );

  assert.deepEqual(sources.configuredPaths, [
    path.join(homeDir, "ModelicaStandardLibrary"),
    path.normalize(relativeLib),
  ]);
  assert.deepEqual(sources.environmentPaths, [envDir, legacyDir]);
  assert.deepEqual(sources.mergedPaths, [
    path.join(homeDir, "ModelicaStandardLibrary"),
    path.normalize(relativeLib),
    envDir,
    legacyDir,
  ]);
  assert.equal(sources.usedLegacyAlias, true);
});

test("changedRumocaRestartKeys only returns restart-relevant settings", () => {
  const changed = new Set(["rumoca.modelicaPath", "rumoca.debug", "rumoca.simulation.dt"]);
  const keys = changedRumocaRestartKeys((section) => changed.has(section));
  assert.deepEqual(keys, ["modelicaPath", "debug"]);
});

test("notebookExecutableFromServerPath tracks the current server binary", () => {
  assert.equal(
    notebookExecutableFromServerPath(path.join(path.sep, "tmp", "rumoca-lsp")),
    path.join(path.sep, "tmp", "rumoca"),
  );
  assert.equal(notebookExecutableFromServerPath(undefined), undefined);
});
