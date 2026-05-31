import { mkdir, readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { runTests } from "@vscode/test-electron";

// Read a required `--flag value` argument passed by `cargo xtask` (the
// harness's argv channel; editor configuration travels in the workspace file).
function argPath(name) {
  const idx = process.argv.indexOf(name);
  const value =
    idx >= 0 && idx + 1 < process.argv.length ? process.argv[idx + 1] : undefined;
  if (!value) {
    throw new Error(`missing required argument: ${name}`);
  }
  return path.resolve(value);
}

function argValue(name) {
  const idx = process.argv.indexOf(name);
  return idx >= 0 && idx + 1 < process.argv.length
    ? process.argv[idx + 1]
    : undefined;
}

const thisDir = path.dirname(fileURLToPath(import.meta.url));
const vscodeDir = path.resolve(thisDir, "..");
const harnessDir = path.resolve(vscodeDir, "tests", "install_check_harness");
const suitePath = path.resolve(vscodeDir, "tests", "installed_extension_check_suite.cjs");

async function main() {
  const workspaceFile = argPath("--workspace");
  const documentPath = argPath("--document");
  const userDataDir = argPath("--user-data-dir");
  const extensionsDir = argPath("--extensions-dir");
  const resultPath = argPath("--result");

  await mkdir(userDataDir, { recursive: true });
  await mkdir(extensionsDir, { recursive: true });

  // Hand the document/result paths to the extension-host suite via the
  // launched workspace's settings (the suite reads them with getConfiguration;
  // no environment variables).
  const workspace = JSON.parse(await readFile(workspaceFile, "utf8"));
  workspace.settings = {
    ...(workspace.settings ?? {}),
    "rumoca.benchmark.installCheck.document": documentPath,
    "rumoca.benchmark.installCheck.result": resultPath,
  };
  await writeFile(workspaceFile, `${JSON.stringify(workspace, null, 2)}\n`, "utf8");

  process.env.ELECTRON_DISABLE_SANDBOX = "1";

  const launchArgs = [
    workspaceFile,
    "--user-data-dir",
    userDataDir,
    "--extensions-dir",
    extensionsDir,
    "--no-sandbox",
    "--disable-setuid-sandbox",
    "--disable-workspace-trust",
    "--disable-gpu",
    "--disable-dev-shm-usage",
    "--disable-updates",
  ];

  const options = {
    extensionDevelopmentPath: harnessDir,
    extensionTestsPath: suitePath,
    launchArgs,
    extensionTestsEnv: { ...process.env },
  };

  const executable = argValue("--smoke-executable");
  if (executable) {
    options.vscodeExecutablePath = executable;
  }

  await runTests(options);
  const raw = await readFile(resultPath, "utf8");
  await mkdir(path.dirname(resultPath), { recursive: true });
  await writeFile(resultPath, raw, "utf8");
}

main().catch((error) => {
  console.error("[vscode-install-check] failed");
  console.error(error);
  process.exit(1);
});
