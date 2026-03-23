import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { runTests } from "@vscode/test-electron";

const thisDir = path.dirname(fileURLToPath(import.meta.url));
const vscodeDir = path.resolve(thisDir, "..");
const suitePath = path.resolve(vscodeDir, "tests", "failed_start_extension_suite.cjs");

async function createWorkspace(rootDir) {
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
  const artifactResultPath = process.env.RUMOCA_VSCODE_FAILED_START_ARTIFACT_RESULT;
  const { workspaceFile, documentPath } = await createWorkspace(tempRoot);
  await mkdir(userDataDir, { recursive: true });
  await mkdir(extensionsDir, { recursive: true });

  process.env.RUMOCA_VSCODE_FAILED_START_DOCUMENT = documentPath;
  process.env.RUMOCA_VSCODE_FAILED_START_RESULT = resultPath;
  process.env.ELECTRON_DISABLE_SANDBOX = "1";
  process.env.RUMOCA_VSCODE_FAILED_START_ACTIVATE_MAX_MS =
    process.env.RUMOCA_VSCODE_FAILED_START_ACTIVATE_MAX_MS ?? "15000";
  process.env.RUMOCA_VSCODE_FAILED_START_COMMAND_MAX_MS =
    process.env.RUMOCA_VSCODE_FAILED_START_COMMAND_MAX_MS ?? "10000";

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

  const executable = process.env.RUMOCA_VSCODE_SMOKE_EXECUTABLE;
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
    if (process.env.RUMOCA_VSCODE_SMOKE_KEEP_TEMP !== "1") {
      await rm(tempRoot, { recursive: true, force: true });
    }
  }
}

main().catch((error) => {
  console.error("[vscode-failed-start-smoke] failed");
  console.error(error);
  process.exit(1);
});
