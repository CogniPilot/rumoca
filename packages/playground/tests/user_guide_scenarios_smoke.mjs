// Runtime cells for every scenario issued by the sole Rust guide fence owner.
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { createHash } from "node:crypto";
import { launchGuideBrowser } from "./browser_launcher.mjs";
import { observeShippedArtifact } from "./browser_artifact_identity.mjs";
import { createBrowserFailureCollector } from "./browser_failures.mjs";
import { driveBoosterMission } from "./user_guide_booster_mission.mjs";

function argument(name, fallback) {
  const index = process.argv.indexOf(name);
  return index < 0 ? fallback : process.argv[index + 1];
}
const base = argument("--base-url", "http://127.0.0.1:8731");
const roster = JSON.parse(await readFile("crates/rumoca-bind-wasm/src/tests/fixtures/user_guide/linked-scenarios.json", "utf8"));
assert.equal(roster.length, 17);
assert.equal(new Set(roster).size, 17);
const browser = await launchGuideBrowser(argument("--browser-binary"));
try {
  const page = await browser.newPage();
  const browserFailures = createBrowserFailureCollector();
  browserFailures.watch(page);
  const verifyArtifact = await observeShippedArtifact(page, ["rumoca_interactive.js",
    "rumoca_runtime.js", "rumoca_bind_wasm_galec.js", "rumoca_bind_wasm_galec_bg.wasm"]);
  await page.goto(`${base}/packages/playground/index.html`, { waitUntil: "domcontentloaded" });
  const receipts = [];
  const failures = [];
  for (const path of roster) {
    const started = performance.now();
    try {
      const boosterMission = path === "examples/interactive/reusable_booster/rumoca-scenario.toml";
      const pendingReceipt = page.evaluate(async ({ path, boosterMission }) => {
        const pkgBase = "/packages/rumoca/dist/release-full-web/";
        const wasm = await import(pkgBase + "rumoca_bind_wasm.js");
        await wasm.default();
        const runtime = await import(pkgBase + "rumoca_runtime.js");
        const interactive = await import(pkgBase + "rumoca_interactive.js");
        const THREE = await import("/packages/rumoca-web/vendor/three_viewer.js");
        const root = new URL("/docs/user-guide/book/repo-examples/", location.href);
        const scenarioUrl = new URL(path.slice("examples/".length), root);
        async function text(url) {
          const response = await fetch(url);
          if (!response.ok) throw new Error(`${url}: HTTP ${response.status}`);
          return response.text();
        }
        async function sha(source) {
          const bytes = new Uint8Array(await crypto.subtle.digest("SHA-256", new TextEncoder().encode(source)));
          return [...bytes].map((value) => value.toString(16).padStart(2, "0")).join("");
        }
        const scenarioText = await text(scenarioUrl);
        const parsed = JSON.parse(wasm.scenario_get_scenario_config_full(
          JSON.stringify({ [path]: scenarioText }), path));
        if (!parsed.ok) throw new Error(JSON.stringify(parsed));
        const config = parsed.config;
        const sourceUrl = new URL(config.model.file, scenarioUrl);
        const source = await text(sourceUrl);
        const cache = runtime.createSourceRootCacheResolver({ manifestUrl: new URL("source-roots/manifest.json", root).href });
        const sourceRootResolutionStarted = performance.now();
        const sourceRootCacheUrl = await cache.cacheUrlFor(config.source_roots ?? []);
        const sourceRootResolutionWallMs = performance.now() - sourceRootResolutionStarted;
        const common = { path, model: config.model.name, sourcePath: "examples/" + sourceUrl.href.slice(root.href.length),
          scenarioSha256: await sha(scenarioText), sourceSha256: await sha(source),
          authoredHorizon: config.sim?.t_end ?? null, authoredDt: config.sim?.dt ?? null,
          solver: config.sim?.solver ?? "auto", sourceRootResolutionWallMs };
        if (config.rumoca.task === "codegen") {
          const files = await runtime.renderGalecFilesWithRuntime({ pkgBase,
            workspaceSources: JSON.stringify({ [common.sourcePath]: source }),
            modelName: config.model.name, target: config.codegen.target, sourceRootCacheUrl });
          if (!Array.isArray(files) || files.length !== 3
            || files.some((file) => !file.path || !file.content?.trim())) throw new Error("incomplete GALEC production files");
          return { ...common, kind: "codegen", files: files.map((file) => ({ path: file.path, bytes: file.content.length })) };
        }
        if (config.rumoca.task !== "simulate") throw new Error("unhandled authored scenario task");
        if (config.sim?.solver === "gpu" || config.sim?.gpu) throw new Error("GPU scenario requires its actual GPU execution owner");
        function snapshot(session) {
          const state = JSON.parse(session.state_json());
          const values = Object.values(state.values);
          if (!Number.isFinite(state.time) || !values.length || !values.every(Number.isFinite)) throw new Error("nonfinite/empty session state");
          return state;
        }
        const sourceRootStagingStarted = performance.now();
        await runtime.ensureParsedSourceRootCache(wasm, sourceRootCacheUrl);
        common.parsedSourceRootStagingWallMs = performance.now() - sourceRootStagingStarted;
        if (interactive.scenarioUsesInputRuntime(config)) {
          if (!(config.sim?.dt > 0)) throw new Error("interactive scenario lacks an authored positive interval");
          const host = document.createElement("div");
          host.style.cssText = "width:600px;height:400px";
          document.body.append(host);
          const construct = wasm.WasmSimulationSession.withInteractiveConfiguration;
          let session;
          let configuredSessions = 0;
          // This actual call includes compilation, preparation and initialization,
          // while source-root staging and controller simulation are timed separately.
          const sessionConstructionWallMs = [];
          wasm.WasmSimulationSession.withInteractiveConfiguration = (...args) => {
            configuredSessions++;
            const constructionStarted = performance.now();
            try {
              session = construct(...args);
              return session;
            } finally {
              sessionConstructionWallMs.push(performance.now() - constructionStarted);
            }
          };
          let controller;
          let timer;
          let rejectRuntime;
          const samples = [];
          try {
            const scriptText = config.transport?.http?.scene
              ? await text(new URL(config.transport.http.scene, scenarioUrl)) : "";
            controller = await interactive.createInteractiveSimulation({ wasm, THREE, source,
              modelName: config.model.name, config, container: host, scriptText, sourceRootCacheUrl,
              assetBaseUrl: config.transport?.http?.asset_dir ? new URL(config.transport.http.asset_dir + "/", scenarioUrl).href : "",
              onError: (error) => rejectRuntime?.(error) });
            if (!session) throw new Error("canonical configured session not constructed");
            samples.push(snapshot(session));
            if (boosterMission) {
              const { observeBoosterMission } = await import("/packages/playground/tests/user_guide_booster_mission.mjs");
              const mission = await observeBoosterMission({ controller, session, host, snapshot,
                getSession: () => {
                  if (configuredSessions !== 1) throw new Error("Booster controller reconstructed its configured session");
                  return session;
                },
                dt: config.sim.dt, setRuntimeReject: (reject) => { rejectRuntime = reject; } });
              return { ...common, kind: "interactive", sessionConstructions: configuredSessions, sessionConstructionWallMs, boosterMission: mission, samples };
            }
            await new Promise((resolve, reject) => {
              rejectRuntime = reject;
              const step = session.step.bind(session);
              session.step = (dt) => {
                step(dt);
                samples.push(snapshot(session));
                if (samples.length >= 4) resolve();
              };
              timer = setTimeout(() => reject(new Error("authored scenario did not advance three intervals")), 30000);
              controller.start();
            });
            controller.stop();
            return { ...common, kind: "interactive", sessionConstructions: configuredSessions, sessionConstructionWallMs, samples };
          } finally {
            clearTimeout(timer);
            controller?.dispose();
            session?.free();
            host.remove();
            wasm.WasmSimulationSession.withInteractiveConfiguration = construct;
          }
        }
        // Explicit output intervals get a short three-interval runtime check.
        // Otherwise retain the authored/annotation horizon and default interval.
        if (["atol", "rtol", "t_start"].some((key) => config.sim?.[key] !== undefined)) {
          throw new Error("canonical batch browser API does not admit these authored settings");
        }
        const dt = config.sim?.dt ?? 0;
        const tEnd = dt > 0 ? Math.min(config.sim?.t_end ?? 3 * dt, 3 * dt) : config.sim?.t_end ?? 0;
        const result = await runtime.simulateModelWithRuntime({ wasm, pkgBase, source,
          modelName: config.model.name, sourceRootCacheUrl, parameterOverrides: config.parameters ?? {},
          solver: config.sim?.solver ?? "auto", dt, tEnd });
        const payload = result.payload ?? result;
        return { ...common, kind: "batch", requestedHorizon: tEnd, requestedDt: dt, payload };
      }, { path, boosterMission });
      pendingReceipt.catch(() => {});
      if (boosterMission) {
        try {
          await Promise.race([driveBoosterMission(page), pendingReceipt]);
        } catch (error) {
          await page.evaluate((message) => globalThis.__rumocaGuideBoosterMission?.fail(message), String(error));
          await pendingReceipt.catch(() => {});
          throw error;
        }
      }
      const receipt = await pendingReceipt;
      const sha = (bytes) => createHash("sha256").update(bytes).digest("hex");
      assert.equal(receipt.scenarioSha256, sha(await readFile(path)), `authored configuration bytes: ${path}`);
      assert.equal(receipt.sourceSha256, sha(await readFile(receipt.sourcePath)), `authored model bytes: ${path}`);
      if (receipt.kind === "batch") {
        const { names, allData } = receipt.payload;
        assert(Array.isArray(names) && names.length > 0);
        assert.equal(new Set(names).size, names.length);
        assert.equal(allData.length, names.length + 1);
        assert(allData[0].length >= 2, "initialization and updates required");
        for (const column of allData) {
          assert.equal(column.length, allData[0].length, "rectangular trace");
          assert(column.every(Number.isFinite), "finite trace");
        }
        assert(allData[0].every((time, index, times) => index === 0 || time > times[index - 1]), "ordered trace");
        const requested = receipt.payload.simDetails.requested;
        const close = (actual, expected) => Math.abs(actual - expected) <= 1e-10 * Math.max(1, Math.abs(expected));
        assert.equal(requested.t_start, 0, "authored scenario initializes at zero");
        assert(Number.isFinite(requested.t_end) && requested.t_end > requested.t_start);
        assert(close(allData[0][0], requested.t_start), "trace includes initialization");
        assert(close(allData[0].at(-1), requested.t_end), "trace completes requested horizon");
        if (receipt.requestedHorizon > 0) assert(close(requested.t_end, receipt.requestedHorizon));
        if (receipt.requestedDt > 0) assert(close(requested.dt, receipt.requestedDt));
        assert(requested.dt === null || (Number.isFinite(requested.dt) && requested.dt > 0), "valid issued output interval");
        if (requested.dt > 0) {
          assert(Math.ceil(requested.t_end / requested.dt) <= allData[0].length, "complete grid size");
          let observed = 0;
          for (let index = 0; index * requested.dt < requested.t_end; index++) {
            const expected = requested.t_start + index * requested.dt;
            while (observed < allData[0].length && allData[0][observed] < expected && !close(allData[0][observed], expected)) observed++;
            assert(observed < allData[0].length && close(allData[0][observed], expected), `requested output grid at ${expected}`);
          }
        }
        receipt.sampledHorizon = [allData[0][0], allData[0].at(-1)];
        receipt.samples = allData[0].length;
        delete receipt.payload;
      } else if (receipt.boosterMission) {
        assert.equal(receipt.sessionConstructions, 1);
        assert.equal(receipt.boosterMission.resetCalls, 1);
        assert(receipt.boosterMission.postResetSteps > 0);
        assert.deepEqual(receipt.boosterMission.nominal.phases, [0, 1, 2, 3, 4]);
        const trace = receipt.boosterMission.trace;
        assert(trace.length > 4, "full authored mission, not startup only");
        assert(trace.every((sample, index) => index === 0
          || Math.abs(sample.time - trace[index - 1].time - receipt.authoredDt) < 1e-9), "complete mission interval grid");
        receipt.sampledHorizon = [trace[0].time, trace.at(-1).time];
        receipt.samples = trace.length;
      } else if (receipt.kind === "interactive") {
        assert(receipt.samples.length >= 4);
        assert.equal(receipt.samples[0].time, 0, "interactive initialization");
        assert(receipt.samples.every((sample, index, samples) => index === 0 || sample.time > samples[index - 1].time));
        assert(receipt.samples.every((sample, index) => Math.abs(sample.time - index * receipt.authoredDt) <= 1e-10),
          "controller uses every authored interval");
        receipt.sampledHorizon = [receipt.samples[0].time, receipt.samples.at(-1).time];
        receipt.samples = receipt.samples.length;
      }
      assert(Number.isFinite(receipt.sourceRootResolutionWallMs) && receipt.sourceRootResolutionWallMs >= 0);
      if (receipt.kind !== "codegen") {
        assert(Number.isFinite(receipt.parsedSourceRootStagingWallMs) && receipt.parsedSourceRootStagingWallMs >= 0);
      }
      if (receipt.kind === "interactive") {
        assert.equal(receipt.sessionConstructionWallMs.length, receipt.sessionConstructions);
        assert(receipt.sessionConstructionWallMs.length > 0);
        assert(receipt.sessionConstructionWallMs.every((elapsed) => Number.isFinite(elapsed) && elapsed > 0));
      }
      receipt.compileAndRuntimeWallMs = performance.now() - started;
      receipts.push(receipt);
      console.log("PASS guide-scenario", JSON.stringify(receipt));
    } catch (error) {
      failures.push(`${path}: ${error.stack ?? error}`);
      console.error("FAIL guide-scenario", path, error);
    }
  }
  assert.equal(failures.length, 0, failures.join("\n"));
  assert.equal(receipts.filter((cell) => cell.kind === "codegen").length, 2);
  assert.equal(receipts.filter((cell) => cell.kind !== "codegen").length, 15);
  await verifyArtifact();
  await browserFailures.check();
} finally {
  await browser.close();
}
