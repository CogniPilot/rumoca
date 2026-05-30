import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { runTests } from "@vscode/test-electron";
import {
  assertMslCompletionCacheProof,
  collectDocumentCompletionTimings,
} from "./msl_extension_smoke_support.mjs";

// Read a `--flag value` argument passed by `cargo xtask` (the harness's argv
// channel; editor configuration travels in the launched workspace file).
function argValue(name) {
  const idx = process.argv.indexOf(name);
  return idx >= 0 && idx + 1 < process.argv.length
    ? process.argv[idx + 1]
    : undefined;
}

const thisDir = path.dirname(fileURLToPath(import.meta.url));
const vscodeDir = path.resolve(thisDir, "..");
const repoRoot = path.resolve(vscodeDir, "..", "..");

const mslRootArg = argValue("--msl-root");
const mslArchiveRoot = mslRootArg
  ? path.resolve(mslRootArg)
  : path.resolve(repoRoot, "target", "msl", "ModelicaStandardLibrary-4.1.0");
const mslModelicaRoot = path.join(mslArchiveRoot, "Modelica 4.1.0");
const mslServicesRoot = path.join(mslArchiveRoot, "ModelicaServices 4.1.0");
const mslComplexFile = path.join(mslArchiveRoot, "Complex.mo");
const mslSourceRootPaths = [mslModelicaRoot, mslServicesRoot, mslComplexFile];
const serverPath = path.resolve(
  vscodeDir,
  "bin",
  process.platform === "win32" ? "rumoca-lsp.exe" : "rumoca-lsp",
);
const suitePath = path.resolve(vscodeDir, "tests", "msl_extension_suite.cjs");
const documentPath = path.join(
  mslModelicaRoot,
  "Electrical",
  "Analog",
  "Examples",
  "Resistor.mo",
);

async function createWorkspaceFile(rootDir, { completionTimingPath, documentPath, resultPath }) {
  await mkdtemp(path.join(rootDir, "workspace-"));
  const workspaceFile = path.join(rootDir, "msl-smoke.code-workspace");
  // The `.code-workspace` is the smoke's config file: the suite and extension
  // read these `rumoca.benchmark.*` keys via `getConfiguration` (no env vars).
  await writeFile(
    workspaceFile,
    JSON.stringify(
      {
        folders: [{ path: mslModelicaRoot }],
        settings: {
          "rumoca.debug": false,
          "rumoca.serverPath": serverPath,
          "rumoca.sourceRootPaths": mslSourceRootPaths,
          // Document/result are consumed by the extension-host test suite.
          "rumoca.benchmark.smoke.document": documentPath,
          "rumoca.benchmark.smoke.result": resultPath,
          // The extension forwards this to rumoca-lsp as `--completion-timing-file`.
          "rumoca.benchmark.completionTimingFile": completionTimingPath,
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
  const workspaceFile = await createWorkspaceFile(tempRoot, {
    completionTimingPath,
    documentPath,
    resultPath,
  });
  const artifactResultPath = argValue("--artifact-result");
  const artifactTimingPath = argValue("--artifact-timings");
  await mkdir(userDataDir, { recursive: true });
  await mkdir(extensionsDir, { recursive: true });

  process.env.MODELICAPATH = mslSourceRootPaths.join(path.delimiter);
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
      const summaryOutPath = argValue("--summary-out");
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
    await rm(tempRoot, { recursive: true, force: true });
  }
}

main().catch((error) => {
  console.error("[vscode-msl-smoke] failed");
  console.error(error);
  process.exit(1);
});
