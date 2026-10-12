// One test-owned browser policy: CPU WebGL is required; hardware WebGPU is optional.
import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import path from "node:path";
import { chromium } from "playwright-core";

export function resolveBrowserExecutable(binary) {
  for (const candidate of binary ? [binary] : ["chromium", "google-chrome", "chromium-browser"]) {
    if (path.isAbsolute(candidate)) return candidate;
    const found = spawnSync("which", [candidate], { encoding: "utf8" });
    if (found.status === 0 && found.stdout.trim()) return found.stdout.trim();
  }
  throw new Error(`a real Chrome/Chromium executable is required: ${binary ?? "PATH"}`);
}

export async function launchGuideBrowser(binary, options = {}) {
  const browser = await chromium.launch({
    ...options,
    executablePath: resolveBrowserExecutable(binary),
    headless: true,
    // Chromium's documented SwANGLE CPU path supports headless WebGL on CI.
    // Do not force a WebGPU adapter: optional GPU runs require actual hardware.
    args: ["--no-sandbox", "--use-gl=angle", "--use-angle=swiftshader",
      "--enable-unsafe-swiftshader", "--enable-unsafe-webgpu"],
  });
  console.log("Browser receipt", JSON.stringify({ version: browser.version(), webgl: "SwANGLE CPU" }));
  return browser;
}

export async function hardwareWebGpuAdmission(page) {
  const receipt = await page.evaluate(async () => {
    if (!navigator.gpu) return { admitted: false, reason: "WebGPU API unavailable" };
    const adapter = await navigator.gpu.requestAdapter({ forceFallbackAdapter: false });
    if (!adapter) return { admitted: false, reason: "no WebGPU adapter" };
    const info = adapter.info;
    if (!info || typeof info.isFallbackAdapter !== "boolean") {
      return { admitted: false, reason: "adapter hardware/fallback identity unavailable" };
    }
    return { admitted: !info.isFallbackAdapter,
      reason: info.isFallbackAdapter ? "fallback/software adapter" : "hardware adapter",
      info: { isFallbackAdapter: info.isFallbackAdapter, vendor: info.vendor,
        architecture: info.architecture, device: info.device, description: info.description } };
  });
  assert.equal(typeof receipt.admitted, "boolean");
  console.log("WebGPU admission receipt", JSON.stringify(receipt));
  return receipt;
}
