import fs from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// pkg/package.json is at <repo>/pkg/package.json
const pkgJsonPath = path.join(__dirname, "..", "..", "..", "pkg", "package.json");

async function main() {
  console.log("Patching generated package.json for wasm package");
  const raw = await fs.readFile(pkgJsonPath, "utf8");
  const pkg = JSON.parse(raw);

  pkg.files = pkg.files || [];

  const addFile = (entry) => {
    if (!pkg.files.includes(entry)) {
      pkg.files.push(entry);
    }
  };

  // Ensure snippets dir is included when wasm-bindgen emits JS snippets
  addFile("snippets");

  await fs.writeFile(pkgJsonPath, JSON.stringify(pkg, null, 2) + "\n", "utf8");
  console.log("Successfully patched package.json for rumoca wasm package");
}

main().catch((err) => {
  console.error("Failed to patch pkg/package.json:", err);
  process.exit(1);
});
