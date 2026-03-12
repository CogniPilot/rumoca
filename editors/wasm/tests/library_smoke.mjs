import { readFile } from "node:fs/promises";
import init, {
  clear_library_cache,
  compile,
  compile_with_libraries,
  get_library_count,
  load_libraries,
} from "../../../pkg/rumoca.js";

const MINI_MODELICA_LIBRARY = `
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

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function miniLibraryJson() {
  return JSON.stringify({
    "Modelica/package.mo": MINI_MODELICA_LIBRARY,
  });
}

function assertBalancedCompilation(raw, label) {
  const parsed = JSON.parse(raw);
  const balanced = parsed.balance?.is_balanced;
  assert(balanced === true, `${label}: expected balanced compilation, got ${raw}`);
}

function runLoadLibrariesSmoke() {
  clear_library_cache();

  const result = JSON.parse(load_libraries(miniLibraryJson()));
  assert(
    result.parsed_count === 1,
    `load_libraries: expected parsed_count=1, got ${JSON.stringify(result)}`,
  );
  assert(
    result.error_count === 0,
    `load_libraries: expected error_count=0, got ${JSON.stringify(result)}`,
  );
  assert(get_library_count() >= 1, "load_libraries: expected cached library documents");

  const raw = compile(USES_MODELICA_SOURCE, "UsesModelica");
  assertBalancedCompilation(raw, "compile after load_libraries");
}

function runCompileWithLibrariesSmoke() {
  clear_library_cache();

  const raw = compile_with_libraries(
    USES_MODELICA_SOURCE,
    "UsesModelica",
    miniLibraryJson(),
  );
  assertBalancedCompilation(raw, "compile_with_libraries");
  assert(
    get_library_count() >= 1,
    "compile_with_libraries: expected supplied libraries to populate cache",
  );
}

async function run() {
  const wasmBytes = await readFile(new URL("../../../pkg/rumoca_bg.wasm", import.meta.url));
  await init(wasmBytes);
  runLoadLibrariesSmoke();
  runCompileWithLibrariesSmoke();
  clear_library_cache();
}

run().catch((error) => {
  console.error("[wasm-smoke] library smoke test failed:");
  console.error(error);
  process.exit(1);
});
