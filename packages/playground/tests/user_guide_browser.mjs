// Mandatory actual user-guide browser entrypoints, using the built npm artifact.
import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { readFile } from "node:fs/promises";
import path from "node:path";
import { resolveBrowserExecutable } from "./browser_launcher.mjs";

const root = process.cwd();
const port = 8731;
const base = `http://127.0.0.1:${port}`;
const browserArg = process.argv.indexOf("--browser-binary");
const browser = resolveBrowserExecutable(browserArg >= 0 ? process.argv[browserArg + 1] : undefined);

async function run(command, args) {
  const { terminal } = start(command, args);
  const { code, signal, error } = await terminal;
  if (error) throw error;
  assert.equal(code, 0, `${command} ${args.join(" ")} failed: ${code}/${signal}`);
}

function start(command, args) {
  const child = spawn(command, args, { cwd: root, stdio: "inherit" });
  let launchError = null;
  const terminal = new Promise((resolve) => {
    child.once("error", (error) => { launchError = error; resolve({ error }); });
    child.once("exit", (code, signal) => resolve({ code, signal }));
  });
  return { child, terminal, launchError: () => launchError };
}

await run(process.execPath, ["--test", "packages/playground/tests/book_live_loading.test.mjs"]);

// Reuse the same package built and shipped by the PR WASM job. No replacement
// models, parser, fake library caches, or missing-capability fallback.
const packageBase = path.join(root, "packages/rumoca/dist/release-full-web");
const manifest = JSON.parse(await readFile(path.join(packageBase, "package.json"), "utf8"));
assert.equal(manifest.name, "@cognipilot/rumoca", "the shipped FullWeb npm package must exist");
assert.ok(manifest.files.includes("rumoca_interactive.js"), "the actual package includes interactive sessions");
// The package build stages these local assets; a retained dist/ alone is not
// enough to run the real guide. Fail before compiling the docs if incomplete.
for (const asset of ["packages/rumoca-web/vendor/monaco/vs/loader.js",
  "packages/playground/vendor/monaco/vs/loader.js", "packages/rumoca-web/vendor/three_viewer.js"]) {
  assert.ok((await readFile(path.join(root, asset))).length > 0,
    `missing staged ${asset}; build the current FullWeb package with packages/rumoca/build.mjs`);
}
await run("cargo", ["xtask", "repo", "modelica-deps", "ensure"]);
await run("cargo", ["xtask", "docs", "build", "--book", "user"]);
const roots = JSON.parse(await readFile(path.join(root,
  "docs/user-guide/book/repo-examples/source-roots/manifest.json"), "utf8"));
assert.equal(roots.roots.length, 2, "the actual pinned MSL and CMM must be staged");
assert.ok(roots.cache, "the checked binary source-root cache must be staged");

// The canonical server supplies the isolation headers used by the live book.
// Refuse an existing server so tests cannot accidentally hit another checkout.
try {
  await fetch(`${base}/docs/user-guide/book/introduction.html`, { signal: AbortSignal.timeout(1000) });
  throw new Error(`refuse existing server on ${base}`);
} catch (error) {
  if (error.message.startsWith("refuse existing server")) throw error;
}
const targetRoot = path.resolve(root, process.env.CARGO_TARGET_DIR || "target");
const xtask = path.join(targetRoot, "debug", process.platform === "win32" ? "xtask.exe" : "xtask");
// docs build already built this shell-only xtask. Spawn it directly so normal
// teardown owns the server PID, without leaving a Cargo-spawned child alive.
const serverProcess = start(xtask, ["docs", "serve", "--book", "user",
  "--port", String(port), "--skip-build", "--skip-wasm-build"]);
const { child: server, terminal } = serverProcess;
try {
  let ready = false;
  for (let attempt = 0; attempt < 100 && server.exitCode === null; attempt++) {
    if (serverProcess.launchError()) throw serverProcess.launchError();
    try {
      const response = await fetch(`${base}/docs/user-guide/book/introduction.html`,
        { signal: AbortSignal.timeout(1000) });
      await response.text();
      if (response.ok) { ready = true; break; }
    } catch { /* Wait for the owned server to bind. */ }
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
  assert.ok(ready, "the owned canonical docs server must become ready");
  for (const script of ["book_live_smoke.mjs", "fixedwing_book_smoke.mjs",
    "interactive_scenario_parameters_smoke.mjs", "user_guide_scenarios_smoke.mjs"]) {
    await run(process.execPath, [path.join("packages/playground/tests", script),
      "--base-url", base, "--browser-binary", browser]);
  }
} finally {
  if (server.pid && server.exitCode === null) server.kill("SIGTERM");
  const timeout = setTimeout(() => {
    if (server.pid && server.exitCode === null) server.kill("SIGKILL");
  }, 5000);
  await terminal;
  clearTimeout(timeout);
}
console.log("PASS: all four exact guide visualizers and real FixedWing controls");
