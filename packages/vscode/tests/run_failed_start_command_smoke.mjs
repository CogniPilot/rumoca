import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { runTests } from "@vscode/test-electron";

const thisDir = path.dirname(fileURLToPath(import.meta.url));
const vscodeDir = path.resolve(thisDir, "..");
const suitePath = path.resolve(vscodeDir, "tests", "failed_start_extension_suite.cjs");

// Read a `--flag value` argument passed by `cargo xtask` (the harness's argv
// channel; the editor configuration travels in the launched workspace file).
function argValue(name) {
  const idx = process.argv.indexOf(name);
  return idx >= 0 && idx + 1 < process.argv.length
    ? process.argv[idx + 1]
    : undefined;
}

async function createWorkspace(rootDir, resultPath) {
  const workspaceRoot = path.join(rootDir, "workspace");
  const workspaceFile = path.join(rootDir, "failed-start.code-workspace");
  const documentPath = path.join(workspaceRoot, "Failure.mo");
  const missingServerPath = path.join(
    rootDir,
    process.platform === "win32" ? "missing-rumoca-lsp.exe" : "missing-rumoca-lsp",
  );
  await mkdir(workspaceRoot, { recursive: true });
  await writeFile(
    documentPath,
    [
      "model Failure",
      "  Real x(start=1);",
      "equation",
      "  der(x) = -x;",
      "end Failure;",
      "",
    ].join("\n"),
    "utf8",
  );
  await writeFile(
    workspaceFile,
    JSON.stringify(
      {
        folders: [{ path: workspaceRoot }],
        settings: {
          "rumoca.debug": false,
          "rumoca.serverPath": missingServerPath,
          "rumoca.sourceRootPaths": [],
          // Config for the extension-host suite (read via getConfiguration; no env).
          "rumoca.benchmark.failedStart.document": documentPath,
          "rumoca.benchmark.failedStart.result": resultPath,
        },
      },
      null,
      2,
    ),
    "utf8",
  );
  return { workspaceFile, documentPath };
}

async function main() {
  const tempRoot = await mkdtemp(path.join(os.tmpdir(), "rumoca-vscode-failed-start-"));
  const userDataDir = path.join(tempRoot, "user-data");
  const extensionsDir = path.join(tempRoot, "extensions");
  const resultPath = path.join(tempRoot, "result.json");
  const artifactResultPath = argValue("--artifact-result");
  const { workspaceFile } = await createWorkspace(tempRoot, resultPath);
  await mkdir(userDataDir, { recursive: true });
  await mkdir(extensionsDir, { recursive: true });

  process.env.ELECTRON_DISABLE_SANDBOX = "1";

  const launchArgs = [
    workspaceFile,
    "--no-sandbox",
    "--disable-setuid-sandbox",
    "--disable-workspace-trust",
    "--disable-gpu",
    "--disable-dev-shm-usage",
    "--disable-updates",
  ];

  const options = {
    extensionDevelopmentPath: vscodeDir,
    extensionTestsPath: suitePath,
    launchArgs,
    extensionTestsEnv: { ...process.env },
  };

  const executable = argValue("--smoke-executable");
  if (executable) {
    options.vscodeExecutablePath = executable;
  }

  try {
    await runTests(options);
  } finally {
    try {
      const raw = await readFile(resultPath, "utf8");
      if (artifactResultPath) {
        await mkdir(path.dirname(artifactResultPath), { recursive: true });
        await writeFile(artifactResultPath, raw, "utf8");
      }
    } catch {
      // Ignore missing/partial result files when the suite fails before persisting metrics.
    }
    await rm(tempRoot, { recursive: true, force: true });
  }
}

main().catch((error) => {
  console.error("[vscode-failed-start-smoke] failed");
  console.error(error);
  process.exit(1);
});
