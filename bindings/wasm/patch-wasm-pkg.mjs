// <root>/wasm/patch-wasm-pkg.mjs
import fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// pkg/package.json is at <root>/pkg/package.json
const pkgJsonPath = path.join(__dirname, "..", "..", "pkg", "package.json");

async function main() {
  console.log("patch generated package.json for wasm package");
  const raw = await fs.readFile(pkgJsonPath, "utf8");
  const pkg = JSON.parse(raw);

  pkg.files = pkg.files || [];

  const addFile = (entry) => {
    if (!pkg.files.includes(entry)) {
      pkg.files.push(entry);
    }
  };

  // Ensure snippets dir is included
  addFile("snippets");

  // Future: add extra helpers you generate into pkg/, e.g.:
  // addFile("rumoca-init.js");

  await fs.writeFile(pkgJsonPath, JSON.stringify(pkg, null, 2) + "\n", "utf8");
  console.log("successfully patched package.json for rumoca");
}

main().catch((err) => {
  console.error("Failed to patch pkg/package.json:", err);
  process.exit(1);
});
