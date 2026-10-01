// Browser smoke test for the on-screen thumb sticks of the user guide's
// interactive vehicle examples (quadrotor, rover, fixed-wing).
//
// Not part of the `cargo xtask playground test` gate because it compiles the
// vehicle models in the browser and depends on the Monaco CDN. Run it manually
// when touching the touch controls or the interactive runtime:
//
//   cargo xtask repo modelica-deps ensure
//   cargo xtask docs serve --port 8731
//   node packages/playground/tests/book_touch_controls_smoke.mjs \
//     --base-url http://127.0.0.1:8731 --browser-binary google-chrome \
//     [--screenshot-dir target/touch-controls]
//
// Verifies: the overlay stays hidden on a desktop (fine pointer) viewer; on an
// emulated phone it shows two sticks plus one button per scenario gamepad
// button, accepts a simultaneous two-finger drag that switches the runtime to
// touch input without scrolling the page, and re-centers on release.
import { chromium } from "playwright-core";
import { spawnSync } from "node:child_process";
import { mkdirSync } from "node:fs";
import path from "node:path";

function argValue(name, fallback) {
  const index = process.argv.indexOf(name);
  return index >= 0 ? process.argv[index + 1] : fallback;
}

const base = argValue("--base-url", "http://127.0.0.1:8731");
const browserBinary = argValue("--browser-binary", "google-chrome");
const screenshotDir = argValue("--screenshot-dir", "");
const browserExecutablePath = path.isAbsolute(browserBinary)
  ? browserBinary
  : spawnSync("which", [browserBinary], { encoding: "utf8" }).stdout.trim();
if (!browserExecutablePath) {
  throw new Error(`failed to resolve browser executable path for ${browserBinary}`);
}
if (screenshotDir) {
  mkdirSync(screenshotDir, { recursive: true });
}

const PAGE = `${base}/docs/user-guide/book/simulation/interactive.html`;
const COMPILE_TIMEOUT_MS = 600000;
const EXAMPLES = [
  { heading: "Quadrotor SIL", slug: "quadrotor", buttons: ["Arm", "Log", "Reset"] },
  { heading: "Rover", slug: "rover", buttons: [] },
  { heading: "Fixed-Wing SIL", slug: "fixedwing", buttons: ["Arm", "Reset"], external: true },
];

const browser = await chromium.launch({
  executablePath: browserExecutablePath,
  headless: true,
  args: ["--no-sandbox", "--enable-unsafe-webgpu"],
});

let failures = 0;
const check = (ok, label) => {
  console.log(`${ok ? "PASS" : "FAIL"}: ${label}`);
  if (!ok) failures++;
};

async function widgetIndex(page, heading) {
  return page.evaluate((title) => {
    const widgets = Array.from(document.querySelectorAll(".rumoca-live"));
    return widgets.findIndex((widget) => {
      let node = widget.previousElementSibling;
      while (node && !/^H[1-6]$/.test(node.tagName)) {
        node = node.previousElementSibling;
      }
      return node?.textContent.trim() === title;
    });
  }, heading);
}

// Start an example and return the page that hosts its interactive viewer.
async function startExample(context, page, example) {
  await page.goto(PAGE, { waitUntil: "domcontentloaded" });
  await page.waitForSelector(".rumoca-live .monaco-editor", { timeout: 60000 });
  const index = await widgetIndex(page, example.heading);
  if (index < 0) {
    throw new Error(`no live widget under "${example.heading}"`);
  }
  const widget = page.locator(".rumoca-live").nth(index);
  const popup = example.external ? context.waitForEvent("page") : null;
  await widget.locator(".rumoca-live-run").click();
  const host = popup ? await popup : page;
  await host.waitForSelector(".rumoca-interactive-root", { timeout: COMPILE_TIMEOUT_MS });
  const status = popup ? host.locator("#status") : widget.locator(".rumoca-live-status").first();
  return { host, status };
}

async function stickCenters(host) {
  return host.evaluate(() => Array.from(document.querySelectorAll(".rumoca-touch-stick")).map((stick) => {
    const rect = stick.getBoundingClientRect();
    return { x: rect.left + rect.width / 2, y: rect.top + rect.height / 2, size: rect.width };
  }));
}

async function knobTransforms(host) {
  return host.evaluate(() => Array.from(document.querySelectorAll(".rumoca-touch-stick-knob"))
    .map((knob) => knob.style.transform));
}

async function checkPhone(context, example) {
  const page = await context.newPage();
  page.on("pageerror", (e) => console.error("[pageerror]", e.message));
  const { host, status } = await startExample(context, page, example);
  const overlay = host.locator(".rumoca-touch-controls");
  check(await overlay.isVisible(), `${example.slug}: touch overlay visible on a phone`);
  await host.locator(".rumoca-interactive-root").scrollIntoViewIfNeeded();

  const sticks = await stickCenters(host);
  check(sticks.length === 2, `${example.slug}: two thumb sticks`);
  check(sticks.every((stick) => stick.size >= 88), `${example.slug}: sticks are at least 88 px`);
  const viewport = host.viewportSize();
  check(
    sticks.length === 2 && sticks[0].x < viewport.width / 2 && sticks[1].x > viewport.width / 2,
    `${example.slug}: left and right sticks sit on their own halves`,
  );
  const labels = await host.locator(".rumoca-touch-buttons button").allTextContents();
  check(
    JSON.stringify(labels) === JSON.stringify(example.buttons),
    `${example.slug}: buttons ${JSON.stringify(labels)} match the gamepad bindings`,
  );

  const scrollBefore = await host.evaluate(() => window.scrollY);
  const cdp = await context.newCDPSession(host);
  const [left, right] = sticks;
  const travel = left.size * 0.4;
  await cdp.send("Input.dispatchTouchEvent", {
    type: "touchStart",
    touchPoints: [{ x: left.x, y: left.y, id: 0 }, { x: right.x, y: right.y, id: 1 }],
  });
  for (let step = 1; step <= 6; step++) {
    const k = step / 6;
    await cdp.send("Input.dispatchTouchEvent", {
      type: "touchMove",
      touchPoints: [
        { x: left.x, y: left.y - travel * k, id: 0 },
        { x: right.x + travel * k, y: right.y, id: 1 },
      ],
    });
    await host.waitForTimeout(50);
  }
  await host.waitForTimeout(1500);
  const held = await knobTransforms(host);
  check(held.length === 2 && held.every((t) => t.startsWith("translate(")), `${example.slug}: both knobs follow their fingers`);
  const statusText = await status.textContent();
  check(/\btouch\b/.test(statusText || ""), `${example.slug}: runtime input mode is touch ("${statusText}")`);
  check(await host.evaluate(() => window.scrollY) === scrollBefore, `${example.slug}: dragging does not scroll the page`);
  if (screenshotDir) {
    await host.screenshot({ path: path.join(screenshotDir, `${example.slug}-touch.png`) });
  }
  await cdp.send("Input.dispatchTouchEvent", { type: "touchEnd", touchPoints: [] });
  await host.waitForTimeout(200);
  const released = await knobTransforms(host);
  check(released.every((t) => t === ""), `${example.slug}: knobs re-center on release`);
  await page.close();
  if (host !== page) {
    await host.close();
  }
}

try {
  const desktop = await browser.newContext({ viewport: { width: 1280, height: 900 } });
  const desktopPage = await desktop.newPage();
  const { host } = await startExample(desktop, desktopPage, EXAMPLES[1]);
  check(
    !(await host.locator(".rumoca-touch-controls").isVisible()),
    "desktop: touch overlay hidden for a fine pointer",
  );
  await desktop.close();

  const phone = await browser.newContext({
    viewport: { width: 390, height: 844 },
    deviceScaleFactor: 3,
    isMobile: true,
    hasTouch: true,
  });
  for (const example of EXAMPLES) {
    await checkPhone(phone, example);
  }
  await phone.close();
} finally {
  await browser.close();
}

if (failures > 0) {
  console.error(`${failures} checks failed`);
  process.exit(1);
}
console.log("All touch-control checks passed");
