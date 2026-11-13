import net from "node:net";
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

// Allow non-final address attempts more time before Node tries the next one.
// The smoke resolver hit Node's attempt timer and an IPv6 route failure
// (main 63c44acd, job 131399986644). The log does not distinguish a slow
// handshake from an unreachable endpoint. Keep errors visible; the package's
// existing request timeout and cache fallback remain in effect.
net.setDefaultAutoSelectFamilyAttemptTimeout(5_000);

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
