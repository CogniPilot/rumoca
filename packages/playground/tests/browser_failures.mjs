// Test-owned observers; they do not replace application or GPU execution.
import assert from "node:assert/strict";

export function installGpuFailureObserver() {
  if (!globalThis.GPUAdapter) throw new Error("admitted GPU adapter API is unavailable");
  const failures = [];
  const devices = [];
  const request = GPUAdapter.prototype.requestDevice;
  GPUAdapter.prototype.requestDevice = async function (...args) {
    const device = await Reflect.apply(request, this, args);
    const state = { device, destroyed: false };
    devices.push(state);
    device.addEventListener("uncapturederror", (event) => {
      failures.push(`GPU uncaptured error: ${event.error?.message ?? event.error}`);
    });
    const destroy = device.destroy;
    device.destroy = function (...args) {
      const result = Reflect.apply(destroy, this, args);
      if (this === device) state.destroyed = true;
      return result;
    };
    device.lost.then((info) => {
      if (state.destroyed && info.reason === "destroyed") return;
      failures.push(`GPU device lost: ${info.reason}: ${info.message}`);
    }, (error) => failures.push(`GPU device loss observer: ${error}`));
    return device;
  };
  globalThis.guideGpuFailureSnapshot = async () => {
    const completion = Promise.all(devices.map(async (state) => {
      if (!state.destroyed) {
        try { await state.device.queue.onSubmittedWorkDone(); }
        catch (error) {
          if (!state.destroyed) failures.push(`GPU queue completion: ${error}`);
        }
      }
    }));
    let timer;
    try {
      await Promise.race([completion, new Promise((_, reject) => {
        timer = setTimeout(() => reject(new Error("GPU queue completion exceeded browser 180000ms budget")), 180000);
      })]);
    } catch (error) {
      failures.push(String(error));
    } finally {
      clearTimeout(timer);
    }
    // Device errors are delivered asynchronously; observe a task turn after
    // the actual submitted work completes, before browser teardown begins.
    await new Promise((resolve) => setTimeout(resolve, 0));
    return [...failures];
  };
}

export function createBrowserFailureCollector() {
  const pageErrors = [];
  const pages = new Set();
  const gpuPages = new Set();
  return {
    watch(page) {
      if (pages.has(page)) return;
      pages.add(page);
      page.on("pageerror", (error) => pageErrors.push(`page error: ${error.message}`));
    },
    async admitGpu(page) {
      assert(!gpuPages.has(page), "GPU observer must be installed once per admitted page");
      await page.evaluate(installGpuFailureObserver);
      gpuPages.add(page);
    },
    async check() {
      const gpuErrors = [];
      for (const page of gpuPages) {
        gpuErrors.push(...await page.evaluate(() => globalThis.guideGpuFailureSnapshot()));
      }
      for (const page of pages) {
        if (!page.isClosed()) {
          await page.evaluate(() => new Promise((resolve) => setTimeout(resolve, 0)));
        }
      }
      await new Promise((resolve) => setImmediate(resolve));
      const failures = [...pageErrors, ...gpuErrors];
      assert.equal(failures.length, 0, failures.join("\n"));
    },
  };
}
