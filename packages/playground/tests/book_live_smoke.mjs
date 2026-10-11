// Browser smoke test for the mdBook live examples (docs/user-guide/live/).
//
// Required by the PR WASM job through user_guide_browser.mjs. Vendor assets
// are staged by the actual npm package build, so this gate uses local Monaco.
// The wrapper builds the book and serves it with the canonical xtask server.
//
// Verifies: widgets mount with Monaco editors, Simulate produces a plot,
// LSP diagnostics produce markers, Show DAE renders Modelica text, the
// editable turkey visualization animates to "done", and the Wave2D GPU widget
// renders a surface.
import { chromium } from "playwright-core";
import { spawnSync } from "node:child_process";
import path from "node:path";
import { observeShippedArtifact } from "./browser_artifact_identity.mjs";

function argValue(name, fallback) {
  const index = process.argv.indexOf(name);
  return index >= 0 ? process.argv[index + 1] : fallback;
}

const base = argValue("--base-url", "http://127.0.0.1:8731");
const browserBinary = argValue("--browser-binary", "google-chrome");
const browserExecutablePath = path.isAbsolute(browserBinary)
  ? browserBinary
  : spawnSync("which", [browserBinary], { encoding: "utf8" }).stdout.trim();
if (!browserExecutablePath) {
  throw new Error(`failed to resolve browser executable path for ${browserBinary}`);
}

const browser = await chromium.launch({
  executablePath: browserExecutablePath,
  headless: true,
  args: ["--no-sandbox", "--enable-unsafe-webgpu", "--enable-unsafe-swiftshader"],
});

let failures = 0;
const check = (ok, label) => {
  console.log(`${ok ? "PASS" : "FAIL"}: ${label}`);
  if (!ok) failures++;
};

try {
  const page = await browser.newPage();
  const verifyArtifact = await observeShippedArtifact(page, ["rumoca_gpu.js"]);
  await page.addInitScript(() => {
    window.guideGpuReceipt = { submissions: 0, computeDispatches: 0, readbacks: 0, sampledValues: 0, nonfiniteValues: 0 };
    if (!window.GPUQueue || !window.GPUBuffer || !window.GPUComputePassEncoder) return;
    const submit = GPUQueue.prototype.submit;
    GPUQueue.prototype.submit = function (...args) {
      const result = Reflect.apply(submit, this, args);
      window.guideGpuReceipt.submissions++;
      return result;
    };
    const dispatch = GPUComputePassEncoder.prototype.dispatchWorkgroups;
    GPUComputePassEncoder.prototype.dispatchWorkgroups = function (...args) {
      const result = Reflect.apply(dispatch, this, args);
      if (args[0] > 0 && (args[1] ?? 1) > 0 && (args[2] ?? 1) > 0) window.guideGpuReceipt.computeDispatches++;
      return result;
    };
    const read = GPUBuffer.prototype.getMappedRange;
    GPUBuffer.prototype.getMappedRange = function (...args) {
      const result = Reflect.apply(read, this, args);
      // The actual canonical GPU driver's only mapped-range read is its
      // Float32 state readback. Preserve the native buffer and its contents.
      const values = new Float32Array(result);
      window.guideGpuReceipt.readbacks++;
      window.guideGpuReceipt.sampledValues += values.length;
      window.guideGpuReceipt.nonfiniteValues += values.filter((value) => !Number.isFinite(value)).length;
      return result;
    };
  });
  page.on("pageerror", (e) => console.error("[pageerror]", e.message));

  async function completedGpuRun(widget, name, before) {
    const status = widget.locator(".rumoca-live-toolbar > .rumoca-live-status");
    await status.filter({ hasText: /solver wgsl-ode RK4 \(f32\)/ }).waitFor({ timeout: 180000 });
    const summary = await status.textContent();
    const points = Number(summary.match(/(\d+) points/)?.[1] ?? 0);
    check(points > 1, `${name} completed a sampled WebGPU solve: ${summary}`);
    const receipt = await page.evaluate(() => ({ ...window.guideGpuReceipt }));
    check(receipt.submissions > before.submissions, `${name} submitted real GPU command buffers`);
    check(receipt.computeDispatches > before.computeDispatches, `${name} encoded positive compute workgroups`);
    check(receipt.readbacks > before.readbacks && receipt.sampledValues > before.sampledValues,
      `${name} read actual computed state samples`);
    check(receipt.nonfiniteValues === before.nonfiniteValues, `${name} GPU state samples are finite`);
    console.log("WebGPU API receipt (software adapter permitted; no hardware timing claim)",
      name, JSON.stringify(receipt));
  }

  // --- Introduction page: Monaco editor, simulate, diagnostics, DAE ---
  await page.goto(`${base}/docs/user-guide/book/introduction.html`, {
    waitUntil: "domcontentloaded",
  });
  await page.waitForSelector(".rumoca-live", { timeout: 30000 });
  const widgets = await page.locator(".rumoca-live").count();
  check(widgets >= 1, `introduction.html has ${widgets} widget(s) (>=1)`);

  const monacoLoaded = await page
    .waitForSelector(".rumoca-live .monaco-editor", { timeout: 30000 })
    .then(() => true)
    .catch(() => false);
  check(monacoLoaded, "Monaco editor mounted");

  await page.locator(".rumoca-live-run").first().click();
  const plot = await page
    .waitForSelector(".rumoca-live-plot-svg", { timeout: 120000 })
    .then(() => true)
    .catch(() => false);
  check(plot, "Simulate produced an SVG plot");
  const balloon = await page.locator(".rumoca-live").first()
    .locator(".rumoca-live-surface canvas").waitFor({ timeout: 30000 })
    .then(() => true).catch(() => false);
  check(balloon, "HotAirBalloon exact visualization rendered its Three.js canvas");

  // Show DAE before the diagnostics mutation below; dae-modelica renders a
  // `class <Name> ... end <Name>;` document.
  await page.locator(".rumoca-live button:has-text('Show DAE')").first().click();
  const dae = await page
    .waitForSelector(".rumoca-live-dae", { timeout: 120000 })
    .then((el) => el.textContent())
    .catch(() => null);
  check(!!dae && dae.includes("class"), "Show DAE rendered Modelica text");

  if (monacoLoaded) {
    // LSP diagnostics: inject a syntax error into the first editor and
    // wait for rumoca-owned markers.
    const markerCount = await page.evaluate(async () => {
      const model = window.monaco.editor.getModels()[0];
      model.setValue("model Broken\n  Real x = ;\nequation\nend Broken;");
      for (let i = 0; i < 50; i++) {
        await new Promise((resolve) => setTimeout(resolve, 200));
        const markers = window.monaco.editor.getModelMarkers({ owner: "rumoca" });
        if (markers.length > 0) return markers.length;
      }
      return 0;
    });
    check(markerCount > 0, `LSP diagnostics produced ${markerCount} markers`);
  }

  // --- Turkey page: editable visualization scripts ---
  await page.goto(`${base}/docs/user-guide/book/language/arrays-pde.html`, {
    waitUntil: "domcontentloaded",
  });
  await page.waitForSelector(".rumoca-live", { timeout: 30000 });
  const arrayWidgets = await page.locator(".rumoca-live").count();
  check(arrayWidgets >= 3, `arrays-pde.html has ${arrayWidgets} widgets (>=3)`);
  const vizEditors = await page.locator(".rumoca-live-viz").count();
  check(vizEditors >= 2, `arrays-pde.html has ${vizEditors} editable viz scripts (>=2)`);

  await page.locator(".rumoca-live-run").first().click();
  const canvas = await page
    .waitForSelector(".rumoca-live-radial canvas", { timeout: 180000 })
    .then(() => true)
    .catch(() => false);
  check(canvas, "Turkey custom viz rendered a canvas");

  const colorbar = await page.locator(".rumoca-live-radial-colorbar").count();
  check(colorbar >= 1, "colorbar present");

  if (canvas) {
    const label = await page.evaluate(() => {
      const slider = document.querySelector(".rumoca-live-radial-slider");
      slider.value = slider.max;
      slider.dispatchEvent(new Event("input"));
      return document.querySelector(
        ".rumoca-live-radial-controls .rumoca-live-status"
      ).textContent;
    });
    check(/t = /.test(label), `scrubbed to end: "${label}"`);
  }

  // Each widget has separate GPU and Interactive checkboxes. A checkbox's
  // position is not a widget identity; use the exact three authored models.
  check(arrayWidgets === 3, "exact Turkey, Wave2D and AirfoilFlow widget roster");
  const models = await page.evaluate(() => window.monaco.editor.getModels()
    .map((model) => model.getValue()).filter((source) => source.includes("model ")));
  check(models.some((source) => source.includes("model Wave2D")), "authored Wave2D source present");
  check(models.some((source) => source.includes("model AirfoilFlow")), "authored AirfoilFlow source present");
  const waveWidget = page.locator(".rumoca-live").nth(1);
  check(await waveWidget.locator(".rumoca-live-gpu input").first().isChecked(), "Wave2D authored GPU default selected");
  const beforeWave = await page.evaluate(() => ({ ...window.guideGpuReceipt }));
  await waveWidget.locator(".rumoca-live-run").click();
  const surface = await waveWidget.locator(".rumoca-live-surface canvas")
    .waitFor({ timeout: 180000 }).then(() => true).catch(() => false);
  check(surface, "Wave2D GPU widget rendered the exact surface visualization");
  await completedGpuRun(waveWidget, "Wave2D", beforeWave);

  const airfoilWidget = page.locator(".rumoca-live").nth(2);
  check(await airfoilWidget.locator(".rumoca-live-gpu input").first().isChecked(), "AirfoilFlow authored GPU default selected");
  const beforeAirfoil = await page.evaluate(() => ({ ...window.guideGpuReceipt }));
  await airfoilWidget.locator(".rumoca-live-run").click();
  const airfoil = await airfoilWidget.locator("canvas")
    .waitFor({ timeout: 180000 }).then(() => true).catch(() => false);
  check(airfoil, "AirfoilFlow GPU widget rendered the exact heatmap visualization");
  await completedGpuRun(airfoilWidget, "AirfoilFlow", beforeAirfoil);
  const fields = airfoilWidget.locator(".rumoca-live-tuner select");
  check(await fields.count() >= 1, "AirfoilFlow field selector is available");
  if (await fields.count()) {
    await fields.first().selectOption("q");
    check(await fields.first().inputValue() === "q", "AirfoilFlow switches to pressure field");
    await fields.first().selectOption("speed");
    check(await fields.first().inputValue() === "speed", "AirfoilFlow switches to speed field");
  }

  await verifyArtifact();
} finally {
  await browser.close();
}

if (failures > 0) {
  console.error(`${failures} checks failed`);
  process.exit(1);
}
console.log("All book live-example checks passed");
