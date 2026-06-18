// Browser smoke test for the mdBook live examples (docs/user-guide/live/).
//
// Not part of the `cargo xtask playground test` gate because it depends on the
// Monaco CDN. Run it manually when touching the live runner:
//
//   cargo xtask docs serve --port 8731
//   node packages/playground/tests/book_live_smoke.mjs \
//     --base-url http://127.0.0.1:8731 --browser-binary google-chrome
//
// Verifies: widgets mount with Monaco editors, Simulate produces a plot,
// LSP diagnostics produce markers, Show DAE renders Modelica text, and the
// editable turkey visualization animates to "done".
import { chromium } from "playwright-core";
import { spawnSync } from "node:child_process";
import path from "node:path";

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
  args: ["--no-sandbox", "--disable-gpu"],
});

let failures = 0;
const check = (ok, label) => {
  console.log(`${ok ? "PASS" : "FAIL"}: ${label}`);
  if (!ok) failures++;
};

try {
  const page = await browser.newPage();
  page.on("pageerror", (e) => console.error("[pageerror]", e.message));

  // --- Live examples page: Monaco editors, simulate, diagnostics, DAE ---
  await page.goto(`${base}/docs/user-guide/book/examples/live.html`, {
    waitUntil: "domcontentloaded",
  });
  await page.waitForSelector(".rumoca-live", { timeout: 30000 });
  const widgets = await page.locator(".rumoca-live").count();
  check(widgets >= 4, `live.html has ${widgets} widgets (>=4)`);

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

  // Show DAE on the second (untouched) widget; dae-modelica renders a
  // `class <Name> ... end <Name>;` document.
  await page.locator(".rumoca-live button:has-text('Show DAE')").nth(1).click();
  const dae = await page
    .waitForSelector(".rumoca-live-dae", { timeout: 120000 })
    .then((el) => el.textContent())
    .catch(() => null);
  check(!!dae && dae.includes("class"), "Show DAE rendered Modelica text");

  // --- Turkey page: editable visualization scripts ---
  await page.goto(`${base}/docs/user-guide/book/language/arrays-pde.html`, {
    waitUntil: "domcontentloaded",
  });
  await page.waitForSelector(".rumoca-live", { timeout: 30000 });
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
} finally {
  await browser.close();
}

if (failures > 0) {
  console.error(`${failures} checks failed`);
  process.exit(1);
}
console.log("All book live-example checks passed");
