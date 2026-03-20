import { writeFile } from "node:fs/promises";
import { spawnSync } from "node:child_process";
import path from "node:path";

import { chromium } from "playwright-core";

function requiredEnv(name) {
  const value = process.env[name];
  if (!value) {
    throw new Error(`missing required env var ${name}`);
  }
  return value;
}

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

async function main() {
  const browserBinary = requiredEnv("RUMOCA_BROWSER_BINARY");
  const smokeUrl = requiredEnv("RUMOCA_WASM_SMOKE_URL");
  const resultPath = requiredEnv("RUMOCA_WASM_SMOKE_RESULT");
  const timeoutMs = envMs("RUMOCA_WASM_SMOKE_TIMEOUT_MS", 330000);
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

  try {
    const page = await browser.newPage();
    page.on("console", (message) => {
      console.error(`[wasm-browser] ${message.type()}: ${message.text()}`);
    });
    page.on("pageerror", (error) => {
      console.error("[wasm-browser] pageerror:", error);
    });

    await page.goto(smokeUrl, {
      waitUntil: "domcontentloaded",
      timeout: timeoutMs,
    });
    await page.waitForFunction(
      () => {
        const status = document.body?.dataset?.rumocaSmokeStatus;
        return status === "pass" || status === "fail";
      },
      undefined,
      { timeout: timeoutMs },
    );

    const [status, rawPayload] = await Promise.all([
      page.evaluate(() => document.body?.dataset?.rumocaSmokeStatus || ""),
      page.locator("#rumocaSmokeResult").textContent(),
    ]);
    const payload = rawPayload ? JSON.parse(rawPayload) : {};
    await writeFile(
      resultPath,
      `${JSON.stringify({ status, payload }, null, 2)}\n`,
      "utf8",
    );

    if (status !== "pass") {
      throw new Error(`wasm editor smoke reported ${status}`);
    }
  } finally {
    await browser.close();
  }
}

main().catch((error) => {
  console.error("[wasm-browser] failed");
  console.error(error);
  process.exit(1);
});
