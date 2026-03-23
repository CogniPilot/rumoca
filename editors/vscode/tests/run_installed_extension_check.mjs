import { mkdir, readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { runTests } from "@vscode/test-electron";

function envPath(name) {
  const value = process.env[name];
  if (!value) {
    throw new Error(`missing required env var: ${name}`);
  }
  return path.resolve(value);
}

const thisDir = path.dirname(fileURLToPath(import.meta.url));
const vscodeDir = path.resolve(thisDir, "..");
const harnessDir = path.resolve(vscodeDir, "tests", "install_check_harness");
const suitePath = path.resolve(vscodeDir, "tests", "installed_extension_check_suite.cjs");

async function main() {
  const workspaceFile = envPath("RUMOCA_VSCODE_INSTALL_CHECK_WORKSPACE");
  const documentPath = envPath("RUMOCA_VSCODE_INSTALL_CHECK_DOCUMENT");
  const userDataDir = envPath("RUMOCA_VSCODE_INSTALL_CHECK_USER_DATA_DIR");
  const extensionsDir = envPath("RUMOCA_VSCODE_INSTALL_CHECK_EXTENSIONS_DIR");
  const resultPath = envPath("RUMOCA_VSCODE_INSTALL_CHECK_RESULT");

  await mkdir(userDataDir, { recursive: true });
  await mkdir(extensionsDir, { recursive: true });

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
    extensionTestsEnv: {
      ...process.env,
      RUMOCA_VSCODE_INSTALL_CHECK_DOCUMENT: documentPath,
      RUMOCA_VSCODE_INSTALL_CHECK_RESULT: resultPath,
    },
  };

  const executable = process.env.RUMOCA_VSCODE_SMOKE_EXECUTABLE;
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
