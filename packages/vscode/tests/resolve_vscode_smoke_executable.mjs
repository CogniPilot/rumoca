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

// Node tries each resolved address for only 250 ms by default before moving
// to the next. Without an IPv6 route, one slow IPv4 handshake to the VS Code
// release API then fails the lookup long before its 15 s request timeout
// (main 63c44acd, job 131399986644: connect ETIMEDOUT, then ENETUNREACH).
// Give each address up to 5 s; a host that stays unreachable still fails.
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
