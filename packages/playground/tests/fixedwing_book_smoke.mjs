// Exercise the user guide's actual scenario, keyboard capture, and WASM model.
// cargo xtask repo modelica-deps ensure
// cargo xtask docs serve --book user --port 8731
// node packages/playground/tests/fixedwing_book_smoke.mjs \
//   --browser-binary /path/to/chromium --base-url http://127.0.0.1:8731
import assert from "node:assert/strict";
import { chromium } from "playwright-core";

function argument(name, fallback) {
  const index = process.argv.indexOf(name);
  return index < 0 ? fallback : process.argv[index + 1];
}

const base = argument("--base-url", "http://127.0.0.1:8731");
const browser = await chromium.launch({
  executablePath: argument("--browser-binary", "google-chrome"),
  headless: true,
  ignoreDefaultArgs: ["--disable-popup-blocking"],
  args: ["--no-sandbox", "--enable-unsafe-swiftshader"],
});
const context = await browser.newContext();
const page = await context.newPage();
page.on("pageerror", (error) => console.error("pageerror:", error.message));
let viewer = page;
context.on("page", (opened) => {
  viewer = opened;
  opened.on("pageerror", (error) => console.error("viewer error:", error.message));
});

async function waitForModel(predicate) {
  await page.waitForFunction(predicate, null, { timeout: 120000 });
}

try {
  // Library startup may outlive transient user activation on a slow connection.
  await page.route("**/source-roots/cache.bin.gz", async (route) => {
    await new Promise((resolve) => setTimeout(resolve, 6000));
    await route.continue();
  });
  await page.goto(`${base}/docs/user-guide/book/simulation/interactive.html`, {
    waitUntil: "domcontentloaded",
  });
  const widget = page.locator("#fixed-wing-sil + .rumoca-live");
  await widget.locator(".rumoca-live-run").waitFor({ timeout: 120000 });

  // Observe the real session without replacing compilation, inputs, or stepping.
  await page.evaluate(async () => {
    const wasm = await import("/packages/rumoca/dist/release-full-web/rumoca_bind_wasm.js");
    const create = wasm.WasmSimulationSession.withInteractiveOptions;
    wasm.WasmSimulationSession.withInteractiveOptions = (...args) => {
      const session = create(...args);
      if (args[1] === "FixedWing") window.fixedWingSession = session;
      return session;
    };
  });
  await widget.locator(".rumoca-live-run").click();
  await page.waitForFunction(() => window.fixedWingSession
    || document.querySelector("#fixed-wing-sil + .rumoca-live .rumoca-live-error"),
  null, { timeout: 120000 });
  assert.ok(await page.evaluate(() => !!window.fixedWingSession),
    await widget.locator(".rumoca-live-output").textContent());
  await waitForModel(() => window.fixedWingSession?.time() > 0);
  assert.notEqual(viewer, page, "the scenario launches its external interactive viewer");
  const capture = viewer.locator(".rumoca-interactive-capture-toggle");
  await capture.waitFor({ timeout: 120000 });
  assert.equal(await page.evaluate(() => window.fixedWingSession.get("armed")), 0);
  const armedReadout = viewer.locator(".rumoca-interactive-armed-state");
  assert.equal(await armedReadout.textContent(), "Aircraft: Disarmed");

  await capture.click();
  assert.equal(await capture.getAttribute("aria-pressed"), "true");
  await viewer.keyboard.press("Space");
  await waitForModel(() => window.fixedWingSession.get("armed") === 1);
  assert.equal(await armedReadout.textContent(), "Aircraft: Armed");
  console.log("PASS: Capture + Space arms the exact FixedWing model");

  await viewer.keyboard.down("w");
  await waitForModel(() => window.fixedWingSession.get("stick_throttle") > 0.1);
  await viewer.keyboard.up("w");
  const thrust = await page.evaluate(() => {
    const session = window.fixedWingSession;
    return { commanded: session.get("stick_throttle"), actual: session.get("thr_out") };
  });
  assert.ok(thrust.actual > 0.1, JSON.stringify(thrust));
  assert.ok(Math.abs(thrust.actual - thrust.commanded) < 1e-10, JSON.stringify(thrust));
  console.log("PASS: armed keyboard throttle reaches the plant output", thrust);

  // Wait past debounce so this checks the throttle precondition itself.
  await viewer.waitForTimeout(600);
  await viewer.keyboard.press("Space");
  const before = await page.evaluate(() => window.fixedWingSession.time());
  await page.waitForFunction((t) => window.fixedWingSession.time() > t + 0.05, before);
  assert.equal(await page.evaluate(() => window.fixedWingSession.get("armed")), 1);
  console.log("PASS: high throttle blocks the arm toggle");

  await viewer.keyboard.press("r");
  await waitForModel(() => window.fixedWingSession.get("armed") === 0
    && window.fixedWingSession.get("stick_throttle") === 0
    && window.fixedWingSession.get("thr_out") === 0);
  await viewer.waitForTimeout(600);
  await viewer.keyboard.press("Space");
  await waitForModel(() => window.fixedWingSession.get("armed") === 1);
  const state = await page.evaluate(() => JSON.parse(window.fixedWingSession.state_json()));
  assert.ok(Object.values(state.values).every(Number.isFinite));
  console.log("PASS: reset restores low throttle and permits rearming; all model values finite");
  await viewer.keyboard.press("Escape");
  assert.equal(await capture.getAttribute("aria-pressed"), "false");
  await widget.getByRole("button", { name: "Stop", exact: true }).click();
} catch (error) {
  console.error(await page.locator(".rumoca-live-status").allTextContents());
  console.error(await page.locator(".rumoca-live-error").allTextContents());
  throw error;
} finally {
  await browser.close();
}
