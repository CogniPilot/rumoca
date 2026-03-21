import { readFile } from "node:fs/promises";
import path from "node:path";
import init, {
  clear_source_root_cache,
  compile,
  compile_with_source_roots,
  get_source_root_document_count,
  lsp_completion,
  lsp_definition,
  lsp_diagnostics,
  lsp_hover,
  load_source_roots,
} from "../../../pkg/rumoca.js";

const MINI_MODELICA_SOURCE_ROOT = `
within ;
package Modelica
  package Blocks
    package Sources
      model Constant
        parameter Real k = 1.0;
        output Real y;
      equation
        y = k;
      end Constant;
    end Sources;
  end Blocks;
end Modelica;
`;

const USES_MODELICA_SOURCE = `
model UsesModelica
  import Modelica.Blocks.Sources.Constant;
  Constant c(k = 2.0);
  Real y;
equation
  y = c.y;
end UsesModelica;
`;

const USES_REAL_MSL_SOURCE = `
model UsesRealMsl
  import Modelica;
end UsesRealMsl;
`;

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function miniSourceRootJson() {
  return JSON.stringify({
    "Modelica/package.mo": MINI_MODELICA_SOURCE_ROOT,
  });
}

async function realMslSliceJson() {
  const cacheRoot = process.env.RUMOCA_MSL_CACHE_DIR
    ? path.resolve(process.env.RUMOCA_MSL_CACHE_DIR)
    : path.resolve("target/msl");
  const mslRoot = path.join(
    cacheRoot,
    "ModelicaStandardLibrary-4.1.0",
    "Modelica 4.1.0",
  );
  const [packageMo, iconsMo] = await Promise.all([
    readFile(path.join(mslRoot, "package.mo"), "utf8"),
    readFile(path.join(mslRoot, "Icons.mo"), "utf8"),
  ]);
  return JSON.stringify({
    "Modelica/package.mo": packageMo,
    "Modelica/Icons.mo": iconsMo,
  });
}

function assertBalancedCompilation(raw, label) {
  const parsed = JSON.parse(raw);
  const balanced = parsed.balance?.is_balanced;
  assert(balanced === true, `${label}: expected balanced compilation, got ${raw}`);
}

function completionLabels(raw) {
  return JSON.parse(raw).map((item) => item.label);
}

function runLspSmoke() {
  clear_source_root_cache();
  load_source_roots(miniSourceRootJson());

  const namespaceSource = "model UsesModelica\n  Modelica.\nend UsesModelica;\n";
  const namespaceLabels = completionLabels(
    lsp_completion(namespaceSource, 1, "  Modelica.".length),
  );
  assert(
    namespaceLabels.includes("Blocks"),
    `lsp_completion: expected namespace completion for Modelica.Blocks, got ${JSON.stringify(namespaceLabels)}`,
  );

  const importedSource = `
model UsesModelica
  import Modelica.Blocks.Sources.Constant;
  Constant c(k = 2.0);
  Real y;
equation
  y = c.y;
end UsesModelica;
`;
  const importedLine = importedSource.split("\n")[2];
  const importChar = importedLine.indexOf("Constant") + 1;
  const hover = JSON.parse(lsp_hover(importedSource, 2, importChar));
  const hoverText = JSON.stringify(hover);
  assert(
    hoverText.includes("Constant"),
    `lsp_hover: expected imported class hover payload, got ${hoverText}`,
  );

  const definition = JSON.parse(lsp_definition(importedSource, 2, importChar));
  assert(
    definition && definition.uri && String(definition.uri).includes("Modelica/package.mo"),
    `lsp_definition: expected source-root definition location, got ${JSON.stringify(definition)}`,
  );

  const diagnostics = JSON.parse(lsp_diagnostics(importedSource));
  assert(
    Array.isArray(diagnostics) && diagnostics.length === 0,
    `lsp_diagnostics: expected clean diagnostics, got ${JSON.stringify(diagnostics)}`,
  );
}

function runLoadSourceRootsSmoke() {
  clear_source_root_cache();

  const result = JSON.parse(load_source_roots(miniSourceRootJson()));
  assert(
    result.parsed_count === 1,
    `load_source_roots: expected parsed_count=1, got ${JSON.stringify(result)}`,
  );
  assert(
    result.error_count === 0,
    `load_source_roots: expected error_count=0, got ${JSON.stringify(result)}`,
  );
  assert(get_source_root_document_count() >= 1, "load_source_roots: expected cached source-root documents");

  const raw = compile(USES_MODELICA_SOURCE, "UsesModelica");
  assertBalancedCompilation(raw, "compile after load_source_roots");
}

function runCompileWithSourceRootsSmoke() {
  clear_source_root_cache();

  const raw = compile_with_source_roots(
    USES_MODELICA_SOURCE,
    "UsesModelica",
    miniSourceRootJson(),
  );
  assertBalancedCompilation(raw, "compile_with_source_roots");
  assert(
    get_source_root_document_count() >= 1,
    "compile_with_source_roots: expected supplied source roots to populate cache",
  );
}

async function runRealMslSliceSmoke() {
  if (process.env.RUMOCA_WASM_MSL_SMOKE !== "1") {
    return;
  }

  clear_source_root_cache();

  const librariesJson = await realMslSliceJson();
  const result = JSON.parse(load_source_roots(librariesJson));
  assert(
    result.parsed_count === 2,
    `real MSL slice: expected parsed_count=2, got ${JSON.stringify(result)}`,
  );
  assert(
    result.error_count === 0,
    `real MSL slice: expected error_count=0, got ${JSON.stringify(result)}`,
  );
  assert(
    get_source_root_document_count() >= 2,
    "real MSL slice: expected cached source-root documents",
  );

  const raw = compile(USES_REAL_MSL_SOURCE, "UsesRealMsl");
  assertBalancedCompilation(raw, "compile against real MSL slice");
}

async function run() {
  const wasmBytes = await readFile(new URL("../../../pkg/rumoca_bg.wasm", import.meta.url));
  await init({ module_or_path: wasmBytes });
  runLoadSourceRootsSmoke();
  runCompileWithSourceRootsSmoke();
  runLspSmoke();
  await runRealMslSliceSmoke();
  clear_source_root_cache();
}

run().catch((error) => {
  console.error("[wasm-smoke] source-root smoke test failed:");
  console.error(error);
  process.exit(1);
});
