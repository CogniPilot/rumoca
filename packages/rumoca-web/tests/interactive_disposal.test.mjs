import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import vm from "node:vm";

// Run the actual factory and controller stop body with environmental stubs.
const runtime = readFileSync(
  new URL("../runtime/rumoca_interactive.js", import.meta.url), "utf8",
);
const factory = runtime.slice(runtime.indexOf("export async function createInteractiveSimulation("))
  .replace("export async function", "async function");
const stop = runtime.match(/  stop\(\) \{\n    this\.stopAnimation\(\);\n    this\.ui\.updateRunState\('Stopped', 'stopped'\);\n    this\.onStatus\('stopped'\);\n  \}/)?.[0];
assert.ok(stop, "exercise the actual stop body, including its caller callback");

for (const kind of ["normal", "scene-setup-throws", "controls-dispose-throws", "status-on-stop-throws"]) {
  test(`interactive disposal releases exactly once: ${kind}`, async () => {
    let freed = 0;
    let controlsDisposed = 0;
    const context = {
      Map, Promise,
      scenarioUsesTouchControls: () => false,
      createInputRuntime: () => ({}),
      createInteractiveSession: async () => ({ free() { freed++; } }),
      createPointerState: () => ({}),
      createViewerRuntime: () => ({ api: {} }),
      compileSceneScript: () => {
        if (kind === "scene-setup-throws") throw new Error("scene failure");
        return {};
      },
      InteractiveControls: class {
        dispose() {
          controlsDisposed++;
          if (kind === "controls-dispose-throws") throw new Error("control cleanup failure");
        }
      },
    };
    vm.createContext(context);
    context.InteractiveSimulationController = vm.runInContext(
      `(class { constructor(data) { Object.assign(this, data); this.ui = { updateRunState() {} }; }
        stopAnimation() {} ${stop} start() {} reset() {} })`, context,
    );
    const create = vm.runInContext(`${factory}; createInteractiveSimulation`, context);
    const setup = create({ config: {}, onStatus(status) {
      if (kind === "status-on-stop-throws" && status === "stopped") {
        throw new Error("consumer status callback failure");
      }
    } });
    if (kind === "scene-setup-throws") {
      await assert.rejects(setup, /scene failure/);
      assert.equal(controlsDisposed, 0);
    } else {
      const controller = await setup;
      if (kind === "normal") controller.dispose();
      if (kind === "controls-dispose-throws") {
        assert.throws(() => controller.dispose(), /control cleanup failure/);
      }
      if (kind === "status-on-stop-throws") {
        assert.throws(() => controller.dispose(), /consumer status callback failure/);
      }
      controller.dispose();
      assert.equal(controlsDisposed, 1);
    }
    assert.equal(freed, 1);
  });
}
