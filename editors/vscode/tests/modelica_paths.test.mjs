import test from "node:test";
import assert from "node:assert/strict";
import path from "node:path";

import modelicaPathsModule from "../out/modelica_paths.js";

const {
  changedRumocaRestartKeys,
  notebookExecutableFromServerPath,
  resolveSourceRootPaths,
} = modelicaPathsModule;

test("resolveSourceRootPaths expands home paths and preserves relative entries", () => {
  const homeDir = path.join(path.sep, "tmp", "rumoca-home");
  const envDir = path.join(path.sep, "opt", "modelica");
  const relativeLib = path.join("vendor", "Modelica");
  const sources = resolveSourceRootPaths(
    ["~/ModelicaStandardLibrary", relativeLib, "~/ModelicaStandardLibrary"],
    {
      MODELICAPATH: [envDir, envDir].join(path.delimiter),
    },
    homeDir,
  );

  assert.deepEqual(sources.configuredPaths, [
    path.join(homeDir, "ModelicaStandardLibrary"),
    path.normalize(relativeLib),
  ]);
  assert.deepEqual(sources.environmentPaths, [envDir]);
  assert.deepEqual(sources.mergedPaths, [
    path.join(homeDir, "ModelicaStandardLibrary"),
    path.normalize(relativeLib),
    envDir,
  ]);
});

test("resolveSourceRootPaths resolves configured relative entries against a workspace base", () => {
  const homeDir = path.join(path.sep, "tmp", "rumoca-home");
  const workspaceDir = path.join(path.sep, "tmp", "rumoca-workspace");
  const sources = resolveSourceRootPaths(
    ["target/msl/Modelica", "target/cmm/CMM-v0.0.1"],
    {},
    homeDir,
    workspaceDir,
  );

  assert.deepEqual(sources.configuredPaths, [
    path.join(workspaceDir, "target", "msl", "Modelica"),
    path.join(workspaceDir, "target", "cmm", "CMM-v0.0.1"),
  ]);
});

test("changedRumocaRestartKeys only returns restart-relevant settings", () => {
  const changed = new Set(["rumoca.sourceRootPaths", "rumoca.debug", "rumoca.simulation.dt"]);
  const keys = changedRumocaRestartKeys((section) => changed.has(section));
  assert.deepEqual(keys, ["sourceRootPaths", "debug"]);
});

test("notebookExecutableFromServerPath tracks the current server binary", () => {
  assert.equal(
    notebookExecutableFromServerPath(path.join(path.sep, "tmp", "rumoca-lsp")),
    path.join(path.sep, "tmp", "rumoca"),
  );
  assert.equal(notebookExecutableFromServerPath(undefined), undefined);
});
