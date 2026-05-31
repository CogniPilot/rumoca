import path from "node:path";
import { fileURLToPath } from "node:url";

import { downloadAndUnzipVSCode } from "@vscode/test-electron";

const thisDir = path.dirname(fileURLToPath(import.meta.url));
const vscodeDir = path.resolve(thisDir, "..");

// Read a `--flag value` argument passed by `cargo xtask` (the harness's argv
// channel for the smoke executable path).
function argValue(name) {
  const idx = process.argv.indexOf(name);
  return idx >= 0 && idx + 1 < process.argv.length
    ? process.argv[idx + 1]
    : undefined;
}

const cachePathArg = argValue("--cache-path");
const cachePath = cachePathArg
  ? path.resolve(cachePathArg)
  : path.resolve(vscodeDir, ".vscode-test");

async function main() {
  const executable = await downloadAndUnzipVSCode({
    extensionDevelopmentPath: vscodeDir,
    cachePath,
  });
  process.stdout.write(`${executable}\n`);
}

main().catch((error) => {
  console.error("[resolve-vscode-smoke-executable] failed");
  console.error(error);
  process.exit(1);
});
