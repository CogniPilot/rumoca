import path from "node:path";
import { fileURLToPath } from "node:url";

import { downloadAndUnzipVSCode } from "@vscode/test-electron";

const thisDir = path.dirname(fileURLToPath(import.meta.url));
const vscodeDir = path.resolve(thisDir, "..");
const cachePath = process.env.RUMOCA_VSCODE_SMOKE_CACHE_PATH
  ? path.resolve(process.env.RUMOCA_VSCODE_SMOKE_CACHE_PATH)
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
