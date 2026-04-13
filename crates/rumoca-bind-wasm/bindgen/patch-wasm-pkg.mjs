import fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// pkg/package.json is at <repo>/pkg/package.json
const pkgJsonPath = path.join(__dirname, "..", "..", "..", "pkg", "package.json");
const pkgDir = path.dirname(pkgJsonPath);

async function exists(p) {
  try {
    await fs.access(p);
    return true;
  } catch {
    return false;
  }
}

async function main() {
  console.log("Patching generated package.json for wasm package");
  const raw = await fs.readFile(pkgJsonPath, "utf8");
  const pkg = JSON.parse(raw);
  pkg.name = "rumoca";

  pkg.files = pkg.files || [];

  const addFile = (entry) => {
    if (!pkg.files.includes(entry)) {
      pkg.files.push(entry);
    }
  };

  const hasDefaultJs = await exists(path.join(pkgDir, "rumoca_bind_wasm.js"));
  const hasDefaultWasm = await exists(path.join(pkgDir, "rumoca_bind_wasm_bg.wasm"));

  // Strict canonical artifacts only (no aliases, no fallback file names).
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
  addFile("rumoca_bind_wasm.js");
  addFile("rumoca_bind_wasm_bg.wasm");
  addFile("rumoca_bind_wasm.d.ts");

  // Include optional worker helpers when present.
  if (await exists(path.join(pkgDir, "rumoca_worker.js"))) {
    addFile("rumoca_worker.js");
  }
  if (await exists(path.join(pkgDir, "parse_worker.js"))) {
    addFile("parse_worker.js");
  }

  // Ensure snippets dir is included when wasm-bindgen emits JS snippets
  addFile("snippets");

  // Drop stale entries so package metadata reflects current build artifacts.
  const unique = [...new Set(pkg.files)];
  const existence = await Promise.all(
    unique.map(async (entry) => [entry, await exists(path.join(pkgDir, entry))]),
  );
  pkg.files = existence.filter(([, ok]) => ok).map(([entry]) => entry);

  await fs.writeFile(pkgJsonPath, JSON.stringify(pkg, null, 2) + "\n", "utf8");
  console.log("Successfully patched package.json for rumoca wasm package");
}

main().catch((err) => {
  console.error("Failed to patch pkg/package.json:", err);
  process.exit(1);
});
