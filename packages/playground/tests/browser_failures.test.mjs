import test from "node:test";
import assert from "node:assert/strict";
import { EventEmitter } from "node:events";
import { createContext, runInContext } from "node:vm";
import { createBrowserFailureCollector } from "./browser_failures.mjs";

// Notification fixtures test the observer boundary, not GPU execution.
function fixture({ gpu = true, schedule = setTimeout } = {}) {
  let lose;
  const device = new EventTarget();
  device.lost = new Promise((resolve) => { lose = resolve; });
  device.queue = { onSubmittedWorkDone: async () => {} };
  device.destroy = () => { lose({ reason: "destroyed", message: "explicit teardown" }); return 17; };
  class GPUAdapter { async requestDevice() { return device; } }
  const context = createContext({ setTimeout: schedule, clearTimeout, ...(gpu ? { GPUAdapter } : {}) });
  const page = new EventEmitter();
  page.isClosed = () => false;
  page.evaluate = (fn) => runInContext(`(${fn.toString()})()`, context);
  const collector = createBrowserFailureCollector();
  collector.watch(page);
  return {
    page, collector, device, lose,
    async request() {
      await collector.admitGpu(page);
      return new GPUAdapter().requestDevice();
    },
    uncaptured(message) {
      const event = new Event("uncapturederror");
      Object.defineProperty(event, "error", { value: { message } });
      device.dispatchEvent(event);
    },
  };
}

test("page errors survive navigation and closed popup teardown", async () => {
  const { page, collector } = fixture({ gpu: false });
  collector.watch(page);
  assert.equal(page.listenerCount("pageerror"), 1);
  page.emit("pageerror", new Error("before navigation"));
  page.emit("pageerror", new Error("after navigation"));
  page.isClosed = () => true;
  await assert.rejects(collector.check(), /before navigation[\s\S]*after navigation/);
});

test("software-only pages remain mandatory without a GPU API", async () => {
  const { collector, page } = fixture({ gpu: false });
  await collector.check();
  await assert.rejects(collector.admitGpu(page), /admitted GPU adapter API is unavailable/);
});

test("admitted uncaptured GPU errors fail the final check", async () => {
  const f = fixture();
  assert.equal(await f.request(), f.device);
  f.uncaptured("invalid command");
  await assert.rejects(f.collector.check(), /GPU uncaptured error: invalid command/);
});

test("unexpected loss and unobserved destruction fail", async () => {
  for (const reason of ["unknown", "destroyed"]) {
    const f = fixture();
    await f.request();
    f.lose({ reason, message: "unexpected" });
    await assert.rejects(f.collector.check(), /GPU device lost: .*unexpected/);
  }
});

test("successful explicit destruction preserves return value without a teardown failure", async () => {
  const f = fixture();
  await f.request();
  assert.equal(f.device.destroy(), 17);
  await f.collector.check();
});

test("explicit destruction does not excuse an unrelated device loss", async () => {
  const f = fixture();
  f.device.destroy = () => { f.lose({ reason: "unknown", message: "reset" }); };
  await f.request();
  f.device.destroy();
  await assert.rejects(f.collector.check(), /GPU device lost: unknown: reset/);
});

test("explicit destruction during completion does not manufacture a queue failure", async () => {
  const f = fixture();
  await f.request();
  f.device.queue.onSubmittedWorkDone = async () => {
    f.device.destroy();
    throw new Error("device was intentionally destroyed");
  };
  await f.collector.check();
});

test("queue failures and notifications arriving during work completion fail", async () => {
  const f = fixture();
  await f.request();
  f.device.queue.onSubmittedWorkDone = async () => {
    await new Promise((resolve) => setTimeout(resolve, 0));
    f.uncaptured("late validation");
    f.page.emit("pageerror", new Error("late page failure"));
    throw new Error("submission failed");
  };
  await assert.rejects(f.collector.check(), /late page failure[\s\S]*late validation[\s\S]*submission failed/);
});

test("asynchronous page errors are flushed before the final assertion", async () => {
  const { collector, page } = fixture({ gpu: false });
  setTimeout(() => page.emit("pageerror", new Error("next task failure")), 0);
  await assert.rejects(collector.check(), /next task failure/);
});

test("a never-resolving GPU queue fails at the existing browser deadline", async () => {
  const deadlines = [];
  const f = fixture({ schedule(callback, delay) {
    if (delay === 180000) {
      deadlines.push(callback);
      return undefined;
    }
    return setTimeout(callback, delay);
  } });
  await f.request();
  f.device.queue.onSubmittedWorkDone = () => new Promise(() => {});
  const checked = f.collector.check();
  assert.equal(deadlines.length, 1);
  deadlines[0]();
  await assert.rejects(checked, /GPU queue completion exceeded browser 180000ms budget/);
});
