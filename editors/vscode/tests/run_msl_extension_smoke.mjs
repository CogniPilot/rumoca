import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { runTests } from "@vscode/test-electron";
import {
  assertMslCompletionCacheProof,
  collectDocumentCompletionTimings,
} from "./msl_extension_smoke_support.mjs";

function envMs(name, fallback) {
  const raw = process.env[name];
  if (!raw) {
    return fallback;
  }
  const parsed = Number.parseInt(raw, 10);
  if (!Number.isFinite(parsed) || parsed <= 0) {
    throw new Error(`invalid timeout env var ${name}=${raw}`);
  }
  return parsed;
}

const thisDir = path.dirname(fileURLToPath(import.meta.url));
const vscodeDir = path.resolve(thisDir, "..");
const repoRoot = path.resolve(vscodeDir, "..", "..");

const mslArchiveRoot = process.env.RUMOCA_VSCODE_MSL_ROOT
  ? path.resolve(process.env.RUMOCA_VSCODE_MSL_ROOT)
  : path.resolve(repoRoot, "target", "msl", "ModelicaStandardLibrary-4.1.0");
const mslModelicaRoot = path.join(mslArchiveRoot, "Modelica 4.1.0");
const mslServicesRoot = path.join(mslArchiveRoot, "ModelicaServices 4.1.0");
const mslComplexFile = path.join(mslArchiveRoot, "Complex.mo");
const mslLibraryPaths = [mslModelicaRoot, mslServicesRoot, mslComplexFile];
const serverPath = process.env.RUMOCA_VSCODE_SMOKE_SERVER_PATH
  ? path.resolve(process.env.RUMOCA_VSCODE_SMOKE_SERVER_PATH)
  : path.resolve(vscodeDir, "bin", process.platform === "win32" ? "rumoca-lsp.exe" : "rumoca-lsp");
const suitePath = path.resolve(vscodeDir, "tests", "msl_extension_suite.cjs");
const documentPath = path.join(
  mslModelicaRoot,
  "Electrical",
  "Analog",
  "Examples",
  "Resistor.mo",
);

async function createWorkspaceFile(rootDir) {
  const smokeDebug = process.env.RUMOCA_VSCODE_SMOKE_DEBUG === "1";
  await mkdtemp(path.join(rootDir, "workspace-"));
  const workspaceFile = path.join(rootDir, "msl-smoke.code-workspace");
  await writeFile(
    workspaceFile,
    JSON.stringify(
      {
        folders: [{ path: mslModelicaRoot }],
        settings: {
          "rumoca.debug": smokeDebug,
          "rumoca.serverPath": serverPath,
          "rumoca.modelicaPath": mslLibraryPaths,
        },
      },
      null,
      2,
    ),
  );
  return workspaceFile;
}

async function main() {
  const tempRoot = await mkdtemp(path.join(os.tmpdir(), "rumoca-vscode-msl-"));
  const userDataDir = path.join(tempRoot, "user-data");
  const extensionsDir = path.join(tempRoot, "extensions");
  const resultPath = path.join(tempRoot, "result.json");
  const completionTimingPath = path.join(tempRoot, "completion-timings.jsonl");
  const workspaceFile = await createWorkspaceFile(tempRoot);
  const artifactResultPath = process.env.RUMOCA_VSCODE_SMOKE_ARTIFACT_RESULT;
  const artifactTimingPath = process.env.RUMOCA_VSCODE_SMOKE_ARTIFACT_TIMINGS;
  await mkdir(userDataDir, { recursive: true });
  await mkdir(extensionsDir, { recursive: true });

  process.env.RUMOCA_VSCODE_SMOKE_DOCUMENT = documentPath;
  process.env.RUMOCA_VSCODE_SMOKE_RESULT = resultPath;
  process.env.RUMOCA_LSP_COMPLETION_TIMING_FILE = completionTimingPath;
  process.env.MODELICAPATH = mslLibraryPaths.join(path.delimiter);
  process.env.ELECTRON_DISABLE_SANDBOX = "1";
  process.env.RUMOCA_VSCODE_SMOKE_ACTIVATE_MAX_MS = String(
    envMs("RUMOCA_VSCODE_SMOKE_ACTIVATE_MAX_MS", 15000),
  );
  process.env.RUMOCA_VSCODE_SMOKE_CODELENS_MAX_MS = String(
    envMs("RUMOCA_VSCODE_SMOKE_CODELENS_MAX_MS", 5000),
  );
  process.env.RUMOCA_VSCODE_SMOKE_COMPLETION_MAX_MS = String(
    envMs("RUMOCA_VSCODE_SMOKE_COMPLETION_MAX_MS", 20000),
  );
  process.env.RUMOCA_VSCODE_SMOKE_HOVER_MAX_MS = String(
    envMs("RUMOCA_VSCODE_SMOKE_HOVER_MAX_MS", 10000),
  );
  process.env.RUMOCA_VSCODE_SMOKE_DEFINITION_MAX_MS = String(
    envMs("RUMOCA_VSCODE_SMOKE_DEFINITION_MAX_MS", 10000),
  );

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
    let summary = null;
    try {
      summary = JSON.parse(await readFile(resultPath, "utf8"));
    } catch {
      // Ignore missing/partial result files when the suite fails before persisting metrics.
    }
    if (summary) {
      const rawTimings = await readFile(completionTimingPath, "utf8");
      Object.assign(summary, collectDocumentCompletionTimings(rawTimings, documentPath));
      assertMslCompletionCacheProof(summary, documentPath);
      const summaryOutPath = process.env.RUMOCA_VSCODE_SMOKE_SUMMARY_OUT;
      if (summaryOutPath) {
        await mkdir(path.dirname(summaryOutPath), { recursive: true });
        await writeFile(summaryOutPath, `${JSON.stringify(summary, null, 2)}\n`, "utf8");
      }
      if (artifactResultPath) {
        await mkdir(path.dirname(artifactResultPath), { recursive: true });
        await writeFile(artifactResultPath, `${JSON.stringify(summary, null, 2)}\n`, "utf8");
      }
      if (artifactTimingPath) {
        await mkdir(path.dirname(artifactTimingPath), { recursive: true });
        await writeFile(artifactTimingPath, rawTimings, "utf8");
      }
    }
    if (process.env.RUMOCA_VSCODE_SMOKE_KEEP_TEMP !== "1") {
      await rm(tempRoot, { recursive: true, force: true });
    }
  }
}

main().catch((error) => {
  console.error("[vscode-msl-smoke] failed");
  console.error(error);
  process.exit(1);
});
