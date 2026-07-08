import assert from "node:assert/strict";
import fs from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import test from "node:test";

import { patchWasmPackageJson } from "../patch-wasm-pkg.mjs";

test("patched wasm package includes staged runtime dependencies", async () => {
  const pkgDir = await fs.mkdtemp(path.join(os.tmpdir(), "rumoca-wasm-pkg-"));
  const runtimeFiles = [
    "rumoca_runtime.js",
    "modelica_language.js",
    "rumoca_worker.js",
    "parse_worker.js",
  ];
  await fs.writeFile(
    path.join(pkgDir, "package.json"),
    JSON.stringify({ name: "rumoca-bind-wasm", files: [] }),
  );
  for (const file of [
    "rumoca_bind_wasm.js",
    "rumoca_bind_wasm_bg.wasm",
    "rumoca_bind_wasm.d.ts",
    ...runtimeFiles,
  ]) {
    await fs.writeFile(path.join(pkgDir, file), "");
  }

  await patchWasmPackageJson(pkgDir, "full-web", runtimeFiles);

  const patched = JSON.parse(await fs.readFile(path.join(pkgDir, "package.json"), "utf8"));
  for (const file of runtimeFiles) {
    assert(patched.files.includes(file), `expected ${file} in package files`);
  }
  assert.deepEqual(patched.repository, {
    type: "git",
    url: "https://github.com/CogniPilot/rumoca",
  });
});
