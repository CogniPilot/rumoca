import fs from "node:fs/promises";
import path from "node:path";

// Published under the @cognipilot org scope. The headline package
// `@cognipilot/rumoca` is the browser-safe full build (variant `full-web`:
// compiler + LSP + all-IR codegen + the rk45 solver runtime);
// `@cognipilot/rumoca-core` is the same minus the bundled solver runtime. Any
// other variant (only used for local/experimental builds) appends its name.
const NPM_SCOPE = "@cognipilot";
const REPOSITORY_URL = "https://github.com/CogniPilot/rumoca";
const packageNameForVariant = (variant) => {
  switch (variant) {
    case "full-web":
      return `${NPM_SCOPE}/rumoca`;
    case "core":
      return `${NPM_SCOPE}/rumoca-core`;
    default:
      return `${NPM_SCOPE}/rumoca-${variant}`;
  }
};

export const patchWasmPackageJson = async (pkgDir, variant, runtimeFiles = []) => {
  const pkgJsonPath = path.join(pkgDir, "package.json");
  const raw = await fs.readFile(pkgJsonPath, "utf8");
  const pkg = JSON.parse(raw);

  const exists = async (p) => {
    try {
      await fs.access(p);
      return true;
    } catch {
      return false;
    }
  };

  pkg.name = packageNameForVariant(variant);
  pkg.repository = { type: "git", url: REPOSITORY_URL };
  // Scoped packages default to restricted; force public so `npm publish`
  // (both the manual scripts and CI) publishes openly without a flag.
  pkg.publishConfig = { ...(pkg.publishConfig || {}), access: "public" };
  pkg.files = pkg.files || [];

  const addFile = (entry) => {
    if (!pkg.files.includes(entry)) {
      pkg.files.push(entry);
    }
  };

  const hasDefaultJs = await exists(path.join(pkgDir, "rumoca_bind_wasm.js"));
  const hasDefaultWasm = await exists(path.join(pkgDir, "rumoca_bind_wasm_bg.wasm"));

  if (await exists(path.join(pkgDir, "rumoca.js"))) {
    await fs.rm(path.join(pkgDir, "rumoca.js"));
  }
  if (await exists(path.join(pkgDir, "rumoca_bg.wasm"))) {
    await fs.rm(path.join(pkgDir, "rumoca_bg.wasm"));
  }
  if (!hasDefaultJs || !hasDefaultWasm) {
    throw new Error(
      "Expected canonical wasm-pack outputs rumoca_bind_wasm.js and rumoca_bind_wasm_bg.wasm",
    );
  }

  pkg.main = "rumoca_bind_wasm.js";
  pkg.module = "rumoca_bind_wasm.js";
  // Subpath exports so the WASM glue and the WebGPU driver are both reachable:
  //   import init, { prepare_gpu_simulation } from "rumoca";
  //   import { runGpuSimulation, probeGpu } from "rumoca/gpu";
  pkg.exports = {
    ".": { import: "./rumoca_bind_wasm.js", types: "./rumoca_bind_wasm.d.ts" },
  };
  addFile("rumoca_bind_wasm.js");
  addFile("rumoca_bind_wasm_bg.wasm");
  addFile("rumoca_bind_wasm.d.ts");

  if (await exists(path.join(pkgDir, "rumoca_gpu.js"))) {
    pkg.exports["./gpu"] = { import: "./rumoca_gpu.js" };
    addFile("rumoca_gpu.js");
  }
  if (await exists(path.join(pkgDir, "rumoca_interactive.js"))) {
    pkg.exports["./interactive"] = { import: "./rumoca_interactive.js" };
    addFile("rumoca_interactive.js");
  }
  // Lazy diffsol (stiff/implicit) addon, exposed as `@cognipilot/rumoca/diffsol`
  // (only present in the full-web build). The driver is the entry point; it
  // lazy-loads the separate relaxed-SIMD addon module on demand.
  if (await exists(path.join(pkgDir, "rumoca_diffsol.js"))) {
    pkg.exports["./diffsol"] = { import: "./rumoca_diffsol.js" };
    addFile("rumoca_diffsol.js");
    addFile("rumoca_bind_wasm_diffsol.js");
    addFile("rumoca_bind_wasm_diffsol_bg.wasm");
    addFile("rumoca_bind_wasm_diffsol.d.ts");
  }
  // Lazy GALEC / eFMI codegen addon, exposed as `@cognipilot/rumoca/galec`
  // (only present in the full-web build). The driver is the entry point; it
  // lazy-loads the separate GALEC codegen addon module on demand.
  if (await exists(path.join(pkgDir, "rumoca_galec.js"))) {
    pkg.exports["./galec"] = { import: "./rumoca_galec.js" };
    addFile("rumoca_galec.js");
    addFile("rumoca_bind_wasm_galec.js");
    addFile("rumoca_bind_wasm_galec_bg.wasm");
    addFile("rumoca_bind_wasm_galec.d.ts");
  }
  if (await exists(path.join(pkgDir, "rumoca_worker.js"))) {
    addFile("rumoca_worker.js");
  }
  if (await exists(path.join(pkgDir, "parse_worker.js"))) {
    addFile("parse_worker.js");
  }
  for (const file of runtimeFiles) {
    if (await exists(path.join(pkgDir, file))) {
      addFile(file);
    }
  }
  if (await exists(path.join(pkgDir, "rumoca_package_meta.json"))) {
    addFile("rumoca_package_meta.json");
  }
  if (await exists(path.join(pkgDir, "README.md"))) {
    addFile("README.md");
  }
  addFile("snippets");

  const unique = [...new Set(pkg.files)];
  const existence = await Promise.all(
    unique.map(async (entry) => [entry, await exists(path.join(pkgDir, entry))]),
  );
  pkg.files = existence.filter(([, ok]) => ok).map(([entry]) => entry);

  await fs.writeFile(pkgJsonPath, `${JSON.stringify(pkg, null, 2)}\n`, "utf8");
};
