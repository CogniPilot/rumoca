import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { ensureNodeSelfForWasmBindgenRayon } from "./node_rayon_shim.mjs";

const subdirIndex = process.argv.indexOf("--pkg-subdir");
const pkgSubdir = subdirIndex < 0 ? "release-full-web" : process.argv[subdirIndex + 1];
assert(pkgSubdir, "--pkg-subdir requires the actual WASM package directory");

function assertFiles(rendered, label) {
  assert.equal(rendered.ok, true, label);
  assert(Array.isArray(rendered.files) && rendered.files.length > 0, `${label}: no files`);
  for (const file of rendered.files) {
    assert.equal(typeof file.path, "string", `${label}: missing path`);
    assert.equal(typeof file.content, "string", `${label}: missing content`);
    assert(file.path.trim() && file.content.trim(), `${label}: empty output`);
  }
  return rendered.files;
}

function assertRefusal(render, expected, label) {
  let failure;
  try {
    render();
  } catch (error) {
    failure = String(error);
  }
  assert(failure !== undefined, `${label}: rendering unexpectedly succeeded`);
  assert.match(failure, expected, label);
}

function algorithmManifest(path, events = true) {
  return `version = 1
name = "browser-source-preview"
ir = "algorithm-code"
[[files]]
path = "${path}"
template = "source.jinja"
[capabilities]
continuous_states = false
events = ${events}
clocks = true
`;
}

async function run() {
  ensureNodeSelfForWasmBindgenRayon();
  const wasm = await import(`../../rumoca/dist/${pkgSubdir}/rumoca_bind_wasm.js`);
  const bytes = await readFile(
    new URL(`../../rumoca/dist/${pkgSubdir}/rumoca_bind_wasm_bg.wasm`, import.meta.url),
  );
  await wasm.default({ module_or_path: bytes });
  const targets = wasm.get_builtin_targets();
  assert(Array.isArray(targets), "the exported target list must be a JavaScript array");
  const ids = targets.map((target) => target.id);
  for (const id of ["base-modelica", "dae-modelica", "c-ode", "jax-ode", "casadi-ode", "galec"]) {
    assert(ids.includes(id), `full-web is missing ${id}`);
  }
  assert(!ids.includes("fmi2") && !ids.includes("fmi3"), "packaging targets need the CLI");

  const decay = await readFile(new URL("../../../examples/models/SympyDecay.mo", import.meta.url), "utf8");
  for (const target of ["base-modelica", "dae-modelica", "c-ode", "jax-ode", "casadi-ode"]) {
    const files = assertFiles(
      wasm.render_target_for_source(decay, "SympyDecay", target, "", "{}"),
      `actual SympyDecay/${target}`,
    );
    assert(files.some((file) => file.content.includes("SympyDecay")), `${target}: model identity lost`);
  }
  const compiled = JSON.parse(wasm.compile(decay, "SympyDecay"));
  assert.equal(compiled.balance.is_balanced, true, "the actual workshop model must compile");
  const daeJson = JSON.stringify(compiled.dae);
  assertFiles(wasm.render_target(daeJson, "SympyDecay", "dae-modelica", "", "{}"), "checked DAE API");
  assertRefusal(
    () => wasm.render_target(daeJson, "SympyDecay", "base-modelica", "", "{}"),
    /renders from model source/,
    "a checked DAE alone cannot supply a Flat source view",
  );
  assertRefusal(() => wasm.render_target("{}", "SympyDecay", "dae-modelica", "", "{}"), /Invalid DAE JSON/, "invalid checked DAE");
  assertRefusal(() => wasm.render_target_for_source(decay, "SympyDecay", "fmi3", "", "{}"), /browser.*cannot|not available in the browser/, "packaging requires the CLI");

  const workspace = JSON.stringify({
    "Shared/package.mo": "within ; package Shared model Cell parameter Real k=3; Real x(start=2); equation der(x)=-k*x; end Cell; end Shared;",
  });
  const source = "model BrowserWorkspace Shared.Cell cell; end BrowserWorkspace;";
  const manifest = `version = 1
name = "workspace-flat"
ir = "flat"
[[files]]
path = "{{ model_name }}.txt"
template = "source.jinja"
`;
  const templates = JSON.stringify({ "source.jinja": "{{ model_name }}" });
  const files = assertFiles(
    wasm.render_target_with_workspace_sources(source, "BrowserWorkspace", workspace, "workspace-flat", manifest, templates),
    "workspace source namespace and custom target",
  );
  assert.deepEqual(files, [{ path: "BrowserWorkspace.txt", content: "BrowserWorkspace" }]);

  const counter = await readFile(new URL("../../../examples/models/GalecCounter.mo", import.meta.url), "utf8");
  const algorithmFiles = assertFiles(wasm.render_target_for_source(counter, "GalecCounter", "galec", "", "{}"), "actual GalecCounter source preview");
  assert(algorithmFiles.every((file) => file.path.endsWith(".alg")), "source previews must not fabricate packaging metadata");
  assert(algorithmFiles.some((file) => file.content.includes("DoStep")), "actual counter step body is absent");
  const customAlgorithm = (path, template, events = true) => wasm.render_target_for_source(
    counter, "GalecCounter", "browser-source-preview", algorithmManifest(path, events), JSON.stringify({ "source.jinja": template }),
  );
  assertRefusal(() => customAlgorithm("Counter.alg", "{{ model_name }}", false), /unsupported-feature:events/, "custom target capability admission");
  assertRefusal(() => customAlgorithm("manifest.xml", "{{ model_name }}"), /source files/, "metadata-only source preview");
  for (const fact of ["generated_at", "generation_tool", "identities", "checksums"]) {
    assertRefusal(() => customAlgorithm("Counter.alg", `{{ artifact.${fact} }}`), /artifact/, `unavailable artifact fact ${fact}`);
  }
  console.log("[wasm-smoke] actual targets, workshop sources, workspace ownership, and source preview refusals passed");
}

run().catch((error) => {
  console.error("[wasm-smoke] target rendering failed:");
  console.error(error);
  process.exitCode = 1;
});
