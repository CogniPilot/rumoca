import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

import {
  createInputRuntime,
  createViewerSignalReader,
  takeRuntimeControlSignal,
} from "../runtime/rumoca_interactive.js";

test("interactive sessions are user-terminated instead of stopping at t_end", async () => {
  const runtime = await readFile(
    new URL("../runtime/rumoca_interactive.js", import.meta.url),
    "utf8",
  );

  assert.match(runtime, /WasmSimulationSession\.withInteractiveOptions\(/);
  assert.doesNotMatch(runtime, /simConfig\.t_end/);
  assert.doesNotMatch(runtime, /session\.end_time\(\)/);
  assert.doesNotMatch(runtime, /updateRunState\('Finished'/);
});

test("scenario UI distinguishes batch horizon from live duration", async () => {
  const scenarioUi = await readFile(
    new URL("../viz/visualization_shared.js", import.meta.url),
    "utf8",
  );

  assert.match(scenarioUi, /label: 'Batch end time'/);
  assert.match(scenarioUi, /Live runs continue until explicitly stopped/);
  assert.match(scenarioUi, /'external_web' \? 'Until stopped'/);
});

function keyEvent(key) {
  return {
    key,
    prevented: false,
    preventDefault() {
      this.prevented = true;
    },
  };
}

test("keyboard decay eases set-style controls back toward neutral after release", () => {
  const input = createInputRuntime({
    locals: {
      steering: { type: "float", default: 0 },
    },
    input: {
      keyboard: {
        decay: {
          factor: 0.85,
          ref_dt: 0.016,
          targets: ["steering"],
        },
        keys: {
          ArrowLeft: {
            action: "set",
            target: "steering",
            value: -1,
          },
        },
      },
    },
  });

  const down = keyEvent("ArrowLeft");
  assert.equal(input.keyDown(down), true);
  assert.equal(down.prevented, true);
  assert.equal(input.locals.get("steering"), -1);

  input.update(0.016);
  assert.equal(input.locals.get("steering"), -1);

  const up = keyEvent("ArrowLeft");
  assert.equal(input.keyUp(up), true);
  input.update(0.016);
  assert.equal(input.locals.get("steering"), -0.85);

  input.update(0.016);
  assert(Math.abs(input.locals.get("steering") + 0.7225) < 1e-12);
});

test("keyboard decay infers set-style targets when no decay block is configured", () => {
  const input = createInputRuntime({
    locals: {
      pitch_cmd: { type: "float", default: 0 },
    },
    input: {
      keyboard: {
        keys: {
          ArrowUp: {
            action: "set",
            target: "pitch_cmd",
            value: -0.5,
          },
        },
      },
    },
  });

  assert.equal(input.keyDown(keyEvent("ArrowUp")), true);
  assert.equal(input.locals.get("pitch_cmd"), -0.5);
  assert.equal(input.keyUp(keyEvent("ArrowUp")), true);

  input.update(0.016);
  assert.equal(input.locals.get("pitch_cmd"), -0.425);
});

test("held keyboard signal actions fire once per key press", () => {
  const input = createInputRuntime({
    input: {
      keyboard: {
        keys: {
          r: {
            action: "signal",
            signal: "reset",
          },
        },
      },
    },
  });

  assert.equal(input.keyDown(keyEvent("r")), true);
  assert.equal(input.takeSignal("reset"), true);

  assert.equal(input.keyDown(keyEvent("r")), true);
  assert.equal(input.takeSignal("reset"), false);

  input.update(0.016);
  assert.equal(input.takeSignal("reset"), false);

  assert.equal(input.keyUp(keyEvent("r")), true);
  assert.equal(input.keyDown(keyEvent("r")), true);
  assert.equal(input.takeSignal("reset"), true);
});

test("runtime reset and quit signals are consumed without waiting for a simulation tick", () => {
  const config = {
    input: {
      keyboard: {
        keys: {
          r: { action: "signal", signal: "restart" },
          q: { action: "signal", signal: "halt" },
        },
      },
    },
    reset: {
      on_signal: "restart",
      reset_locals: true,
      reset_session: true,
    },
    quit: { on_signal: "halt" },
  };
  const input = createInputRuntime(config);

  input.keyDown(keyEvent("r"));
  assert.deepEqual(takeRuntimeControlSignal(config, input), {
    action: "reset",
    resetLocals: true,
    resetSession: true,
  });
  assert.equal(takeRuntimeControlSignal(config, input), null);

  input.keyUp(keyEvent("r"));
  input.keyDown(keyEvent("q"));
  assert.deepEqual(takeRuntimeControlSignal(config, input), { action: "quit" });
  assert.equal(takeRuntimeControlSignal(config, input), null);
});

test("viewer refresh reads one stable snapshot of only configured model signals", () => {
  const config = {
    locals: {
      stick: { type: "float", default: 0.25 },
    },
    signals: {
      viewer: {
        duplicate: "model:vehicle.x",
        frame: "runtime:frame_num",
        local: "local:stick",
        position: "model:vehicle.x",
        raw: "vehicle.y",
        simulation_time: "model:time",
      },
    },
  };
  const input = createInputRuntime(config);
  const reads = [];
  let time = 4;
  const values = new Map([
    ["vehicle.x", 12],
    ["vehicle.y", -3],
    ["unused", 99],
  ]);
  const session = {
    time: () => time,
    get(name) {
      reads.push(name);
      return values.get(name);
    },
    state_json() {
      throw new Error("the full simulation state must not be serialized");
    },
  };

  const readViewerSignals = createViewerSignalReader(config, input, session);
  const target = new Map([["stale", 1]]);
  const snapshot = readViewerSignals(7, target);
  time = 8;
  values.set("vehicle.x", 20);

  assert.strictEqual(snapshot, target);
  assert.deepEqual(reads, ["vehicle.x", "vehicle.y"]);
  assert.deepEqual(Object.fromEntries(snapshot), {
    duplicate: 12,
    frame: 7,
    local: 0.25,
    position: 12,
    raw: -3,
    simulation_time: 4,
  });
});
