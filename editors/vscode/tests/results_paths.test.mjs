import test from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs/promises";
import os from "node:os";
import path from "node:path";

import {
  modelScopedViewerScriptRelativePath,
  preferredViewerScriptPathForModel,
  resolveModelIdentityUuid,
  resolvePreferredViewerScriptPath,
} from "../out/results_paths.js";

async function makeWorkspaceWithIdentity(entries) {
  const workspaceRoot = await fs.mkdtemp(path.join(os.tmpdir(), "rumoca-results-paths-"));
  const byIdRoot = path.join(workspaceRoot, ".rumoca", "models", "by-id");
  await fs.mkdir(byIdRoot, { recursive: true });
  for (const entry of entries) {
    const modelDir = path.join(byIdRoot, entry.uuid);
    await fs.mkdir(modelDir, { recursive: true });
    const aliases = Array.isArray(entry.aliases) ? entry.aliases : [];
    const lines = [
      'version = 1',
      `uuid = ${JSON.stringify(entry.uuid)}`,
      `qualified_name = ${JSON.stringify(entry.qualifiedName)}`,
      `class_name = ${JSON.stringify(entry.className)}`,
      `aliases = [${aliases.map((value) => JSON.stringify(value)).join(", ")}]`,
    ];
    await fs.writeFile(path.join(modelDir, "identity.toml"), `${lines.join("\n")}\n`, "utf-8");
  }
  return workspaceRoot;
}

test("resolveModelIdentityUuid matches qualified model names before aliases", async () => {
  const workspaceRoot = await makeWorkspaceWithIdentity([
    {
      uuid: "uuid-qualified",
      qualifiedName: "Pkg.System.Ball",
      className: "Ball",
      aliases: ["Ball"],
    },
    {
      uuid: "uuid-alias",
      qualifiedName: "Pkg.Other.Ball",
      className: "Ball",
      aliases: ["Pkg.System.Ball"],
    },
  ]);

  const resolved = await resolveModelIdentityUuid(workspaceRoot, "Pkg.System.Ball");
  assert.equal(resolved, "uuid-qualified");
});

test("resolveModelIdentityUuid falls back to aliases and then unique class names", async () => {
  const aliasWorkspace = await makeWorkspaceWithIdentity([
    {
      uuid: "uuid-alias",
      qualifiedName: "Pkg.Library.Decay",
      className: "Decay",
      aliases: ["Decay"],
    },
  ]);
  assert.equal(await resolveModelIdentityUuid(aliasWorkspace, "Decay"), "uuid-alias");

  const classWorkspace = await makeWorkspaceWithIdentity([
    {
      uuid: "uuid-class",
      qualifiedName: "Pkg.Library.Sample1",
      className: "Sample1",
      aliases: [],
    },
  ]);
  assert.equal(await resolveModelIdentityUuid(classWorkspace, "Pkg.Other.Sample1"), "uuid-class");
});

test("resolvePreferredViewerScriptPath prefers the model-scoped by-id sidecar directory", async () => {
  const workspaceRoot = await makeWorkspaceWithIdentity([
    {
      uuid: "1234-uuid",
      qualifiedName: "Pkg.System.Ball",
      className: "Ball",
      aliases: [],
    },
  ]);

  const resolved = await resolvePreferredViewerScriptPath(
    workspaceRoot,
    "Pkg.System.Ball",
    "viewer_3d",
  );

  assert.equal(
    resolved,
    modelScopedViewerScriptRelativePath("1234-uuid", "viewer_3d"),
  );
});

test("resolvePreferredViewerScriptPath falls back to the stable model path when no sidecar exists", async () => {
  const workspaceRoot = await makeWorkspaceWithIdentity([]);

  const resolved = await resolvePreferredViewerScriptPath(
    workspaceRoot,
    "Pkg.System.Ball",
    "viewer_3d",
  );

  assert.equal(
    resolved,
    preferredViewerScriptPathForModel("Pkg.System.Ball", "viewer_3d"),
  );
});

test("resolvePreferredViewerScriptPath still rejects missing workspace roots", async () => {
  await assert.rejects(
    () => resolvePreferredViewerScriptPath(undefined, "Pkg.System.Ball", "viewer_3d"),
    /without a workspace root/,
  );
});
