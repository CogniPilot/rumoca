import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import test from "node:test";

import {
  GALEC_TARGETS,
  galecResultToFiles,
  isGalecTarget,
  renderGalec,
} from "../runtime/rumoca_galec.js";

test("isGalecTarget recognizes exactly the Algorithm Code target", () => {
  assert.deepEqual([...GALEC_TARGETS], ["galec"]);
  for (const target of GALEC_TARGETS) {
    assert.equal(isGalecTarget(target), true, target);
  }
  for (const other of ["sympy", "jax", "fmi3", "", null, undefined]) {
    assert.equal(isGalecTarget(other), false, String(other));
  }
});

test("galec target yields only the .alg Algorithm Code", () => {
  const files = galecResultToFiles({
    model_identifier: "pkg_Model",
    alg: "DoStep{}",
  });
  assert.deepEqual(files, [{ path: "pkg_Model.alg", content: "DoStep{}" }]);
});

test("galecResultToFiles tolerates missing fields and identifier", () => {
  assert.deepEqual(galecResultToFiles({}), [{ path: "model.alg", content: "" }]);
});

test("renderGalec rejects a non-GALEC target before touching the addon wasm", async () => {
  const workspaceSources = JSON.stringify({ "M.mo": "model M end M;" });
  await assert.rejects(
    () => renderGalec("./", workspaceSources, "M", "sympy"),
    /not a GALEC codegen target/,
  );
});

test("runtime re-exports the GALEC helper", () => {
  const runtimeSource = fs.readFileSync(
    path.resolve("runtime", "rumoca_runtime.js"),
    "utf8",
  );
  assert.match(
    runtimeSource,
    /import \{[^}]*\brenderGalecTargetFiles\b[^}]*\} from '\.\/rumoca_galec\.js'/s,
  );
  assert.match(runtimeSource, /export async function renderGalecFilesWithRuntime/);
});
