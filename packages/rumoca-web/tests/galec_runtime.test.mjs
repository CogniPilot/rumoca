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

test("isGalecTarget recognizes exactly the three GALEC targets", () => {
  assert.deepEqual([...GALEC_TARGETS], [
    "galec",
    "efmi",
    "galec-c",
  ]);
  for (const target of GALEC_TARGETS) {
    assert.equal(isGalecTarget(target), true, target);
  }
  for (const other of ["sympy", "jax", "fmi3", "", null, undefined]) {
    assert.equal(isGalecTarget(other), false, String(other));
  }
});

test("galec target yields only the .alg (Algorithm Code), C fields ignored", () => {
  const files = galecResultToFiles("galec", {
    model_identifier: "pkg_Model",
    alg: "DoStep{}",
    c_header: "",
    c_source: "",
  });
  assert.deepEqual(files, [{ path: "pkg_Model.alg", content: "DoStep{}" }]);
});

test("C targets add .h and .c named by the model identifier (matches the #include)", () => {
  for (const target of ["efmi", "galec-c"]) {
    const files = galecResultToFiles(target, {
      model_identifier: "a_b_Demo",
      alg: "ALG",
      c_header: "HDR",
      c_source: "SRC",
    });
    assert.deepEqual(
      files.map((file) => file.path),
      ["a_b_Demo.alg", "a_b_Demo.h", "a_b_Demo.c"],
      target,
    );
    assert.equal(files[1].content, "HDR", target);
    assert.equal(files[2].content, "SRC", target);
  }
});

test("galecResultToFiles tolerates missing fields and identifier", () => {
  const files = galecResultToFiles("efmi", {});
  assert.deepEqual(files, [
    { path: "model.alg", content: "" },
    { path: "model.h", content: "" },
    { path: "model.c", content: "" },
  ]);
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
