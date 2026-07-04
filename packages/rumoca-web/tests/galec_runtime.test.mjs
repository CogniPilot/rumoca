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
    "galec-production",
    "embedded-c-galec",
  ]);
  for (const target of GALEC_TARGETS) {
    assert.equal(isGalecTarget(target), true, target);
  }
  for (const other of ["sympy", "jax", "fmi3", "", null, undefined]) {
    assert.equal(isGalecTarget(other), false, String(other));
  }
});

test("galec target yields only the .alg (Algorithm Code), C fields ignored", () => {
  const files = galecResultToFiles("pkg.Model", "galec", {
    alg: "DoStep{}",
    c_header: "",
    c_source: "",
  });
  assert.deepEqual(files, [{ path: "Model.alg", content: "DoStep{}" }]);
});

test("C targets add .h and .c alongside the .alg, keyed on the model leaf", () => {
  for (const target of ["galec-production", "embedded-c-galec"]) {
    const files = galecResultToFiles("a.b.Demo", target, {
      alg: "ALG",
      c_header: "HDR",
      c_source: "SRC",
    });
    assert.deepEqual(
      files.map((file) => file.path),
      ["Demo.alg", "Demo.h", "Demo.c"],
      target,
    );
    assert.equal(files[1].content, "HDR", target);
    assert.equal(files[2].content, "SRC", target);
  }
});

test("galecResultToFiles tolerates missing fields and empty model name", () => {
  const files = galecResultToFiles("", "galec-production", {});
  assert.deepEqual(files, [
    { path: "model.alg", content: "" },
    { path: "model.h", content: "" },
    { path: "model.c", content: "" },
  ]);
});

test("renderGalec rejects a non-GALEC target before touching the addon wasm", async () => {
  await assert.rejects(
    () => renderGalec("./", "model M end M;", "M", "sympy"),
    /not a GALEC codegen target/,
  );
});

test("runtime re-exports the GALEC helper", () => {
  const runtimeSource = fs.readFileSync(
    path.resolve("runtime", "rumoca_runtime.js"),
    "utf8",
  );
  assert.match(runtimeSource, /import \{ renderGalecTargetFiles \} from '\.\/rumoca_galec\.js'/);
  assert.match(runtimeSource, /export async function renderGalecFilesWithRuntime/);
});
