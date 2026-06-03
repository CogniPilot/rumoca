import fs from "node:fs/promises";
import path from "node:path";

const packageNameForVariant = (variant) => (variant === "core" ? "rumoca" : `rumoca-${variant}`);

export const patchWasmPackageJson = async (pkgDir, variant) => {
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
  addFile("rumoca_bind_wasm.js");
  addFile("rumoca_bind_wasm_bg.wasm");
  addFile("rumoca_bind_wasm.d.ts");

  if (await exists(path.join(pkgDir, "rumoca_worker.js"))) {
    addFile("rumoca_worker.js");
  }
  if (await exists(path.join(pkgDir, "parse_worker.js"))) {
    addFile("parse_worker.js");
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
