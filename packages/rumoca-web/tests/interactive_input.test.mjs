import assert from "node:assert/strict";
import test from "node:test";

import { createInputRuntime } from "../runtime/rumoca_interactive.js";

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
