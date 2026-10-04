import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

import { createInputRuntime } from "../runtime/rumoca_interactive.js";
import {
  createVirtualGamepad,
  scenarioUsesTouchControls,
  shapeStickAxis,
  stickAxes,
  touchAxisShapes,
  touchControlButtons,
} from "../runtime/rumoca_touch_controls.js";

const close = (actual, expected, label) => {
  assert(Math.abs(actual - expected) < 1e-12, `${label}: ${actual} != ${expected}`);
};

// The acro quadrotor bindings: left stick Y integrates throttle with a 0.1
// deadband, right stick X/Y are set-style attitude commands, Start arms.
function quadrotorConfig() {
  return {
    locals: {
      armed: { type: "bool", default: false },
      pitch_cmd: { type: "float", default: 0 },
      roll_cmd: { type: "float", default: 0 },
      throttle: { type: "float", default: 0 },
      yaw_cmd: { type: "float", default: 0 },
    },
    input: {
      mode: "auto",
      gamepad: {
        axes: {
          pitch: { invert: true, source: "RightStickX", write: "pitch_cmd" },
          roll: { source: "RightStickY", write: "roll_cmd" },
          yaw: { invert: true, source: "LeftStickX", write: "yaw_cmd" },
        },
        buttons: {
          arm: {
            action: "toggle",
            debounce_ms: 500,
            precondition: "throttle <= 0.05",
            source: "Start",
            state: "armed",
          },
          log: { action: "signal", signal: "log", source: "North" },
          reset: { action: "signal", signal: "reset", source: "South" },
        },
        integrators: {
          throttle: {
            clamp: [0, 1],
            deadband: 0.1,
            rate: 0.7,
            source: "LeftStickY",
            write: "throttle",
          },
        },
      },
      keyboard: {
        keys: {
          ArrowRight: { action: "set", target: "roll_cmd", value: 0.6 },
        },
      },
    },
  };
}

function keyEvent(key) {
  return { key, preventDefault() {} };
}

test("stick deflection maps to gamepad axes with +x right and +y down", () => {
  assert.deepEqual(stickAxes(0, 0, 40), { x: 0, y: 0 });
  assert.deepEqual(stickAxes(40, 0, 40), { x: 1, y: 0 });
  assert.deepEqual(stickAxes(0, -20, 40), { x: 0, y: -0.5 });
  assert.deepEqual(stickAxes(-10, 30, 40), { x: -0.25, y: 0.75 });
});

test("stick deflection is clamped to the unit travel circle", () => {
  const far = stickAxes(400, 0, 40);
  assert.deepEqual(far, { x: 1, y: 0 });

  const diagonal = stickAxes(100, -100, 40);
  close(Math.hypot(diagonal.x, diagonal.y), 1, "diagonal magnitude");
  close(diagonal.x, Math.SQRT1_2, "diagonal x");
  close(diagonal.y, -Math.SQRT1_2, "diagonal y");
});

test("degenerate stick geometry reports a centered stick", () => {
  assert.deepEqual(stickAxes(10, 10, 0), { x: 0, y: 0 });
  assert.deepEqual(stickAxes(Number.NaN, 5, 40), { x: 0, y: 0.125 });
});

test("touch sticks drive the scenario gamepad axes and integrators", () => {
  const pad = createVirtualGamepad();
  const input = createInputRuntime(quadrotorConfig(), { virtualGamepad: pad });

  // Untouched: the virtual pad is not sampled at all.
  input.update(0.1);
  assert.equal(input.runtimeFields(0).input_mode, "keyboard");

  // Pushing the left stick fully up raises throttle at the configured rate.
  pad.engage();
  const up = stickAxes(0, -50, 50);
  pad.setStick("left", up.x, up.y);
  const right = stickAxes(25, -25, 50);
  pad.setStick("right", right.x, right.y);
  input.update(0.5);
  close(input.locals.get("throttle"), 0.35, "throttle after 0.5 s");
  close(input.locals.get("roll_cmd"), 0.5, "stick up rolls positive like ArrowUp");
  close(input.locals.get("pitch_cmd"), -0.5, "stick right pitches negative like ArrowRight");
  assert.equal(input.runtimeFields(1).input_mode, "touch");

  // Releasing re-centers the set-style axes; the integrator holds its value.
  pad.setStick("left", 0, 0);
  pad.setStick("right", 0, 0);
  input.update(0.5);
  close(input.locals.get("throttle"), 0.35, "throttle holds");
  close(input.locals.get("roll_cmd"), 0, "roll re-centers");
  close(input.locals.get("pitch_cmd"), 0, "pitch re-centers");
});

test("touch deflection inside the scenario deadband leaves the integrator untouched", () => {
  const pad = createVirtualGamepad();
  const input = createInputRuntime(quadrotorConfig(), { virtualGamepad: pad });
  pad.engage();
  const nudge = stickAxes(0, -4, 50);
  pad.setStick("left", nudge.x, nudge.y);
  input.update(1.0);
  assert.equal(input.locals.get("throttle"), 0);
});

test("a tap shorter than a frame still fires its gamepad button once", () => {
  const pad = createVirtualGamepad();
  const input = createInputRuntime(quadrotorConfig(), { virtualGamepad: pad });
  pad.engage();
  pad.pressButton(9);
  pad.releaseButton(9);
  input.update(0.016);
  assert.equal(input.locals.get("armed"), true);
  input.update(0.016);
  assert.equal(input.locals.get("armed"), true);

  pad.pressButton(0);
  pad.releaseButton(0);
  input.update(0.016);
  assert.equal(input.takeSignal("reset"), true);
  input.update(0.016);
  assert.equal(input.takeSignal("reset"), false);
});

test("a bound key press hands control back to the keyboard", () => {
  const pad = createVirtualGamepad();
  const input = createInputRuntime(quadrotorConfig(), { virtualGamepad: pad });
  pad.engage();
  input.update(0.016);
  assert.equal(input.keyDown(keyEvent("ArrowRight")), true);
  assert.equal(pad.engaged, false);
  input.update(0.016);
  close(input.locals.get("roll_cmd"), 0.6, "keyboard roll is not overwritten");
});

test("a connected physical gamepad takes precedence over the touch pad", () => {
  const pad = createVirtualGamepad();
  const input = createInputRuntime(quadrotorConfig(), { virtualGamepad: pad });
  pad.engage();
  pad.setStick("right", 1, 0);
  const physical = {
    axes: [0, 0, -0.5, 0],
    buttons: [],
  };
  const descriptor = Object.getOwnPropertyDescriptor(globalThis, "navigator");
  Object.defineProperty(globalThis, "navigator", {
    configurable: true,
    value: { getGamepads: () => [physical] },
  });
  try {
    input.update(0.016);
  } finally {
    if (descriptor) {
      Object.defineProperty(globalThis, "navigator", descriptor);
    } else {
      delete globalThis.navigator;
    }
  }
  close(input.locals.get("pitch_cmd"), 0.5, "physical pitch wins over the touch stick");
  assert.equal(input.runtimeFields(0).input_mode, "gamepad");
});

test("touch buttons mirror the scenario gamepad buttons", () => {
  assert.deepEqual(
    touchControlButtons(quadrotorConfig()).map(({ name, source, index, label }) => [name, source, index, label]),
    [
      ["arm", "Start", 9, "Arm"],
      ["log", "North", 3, "Log"],
      ["reset", "South", 0, "Reset"],
    ],
  );
  assert.deepEqual(touchControlButtons({}), []);
  assert.deepEqual(
    touchControlButtons({ input: { gamepad: { buttons: { odd: { source: "Guide" } } } } }),
    [],
  );
});

test("touch controls are offered only to scenarios with gamepad bindings", () => {
  assert.equal(scenarioUsesTouchControls(quadrotorConfig()), true);
  assert.equal(scenarioUsesTouchControls({ input: { mode: "keyboard", gamepad: quadrotorConfig().input.gamepad } }), false);
  assert.equal(scenarioUsesTouchControls({ input: { keyboard: { keys: { a: {} } } } }), false);
  assert.equal(scenarioUsesTouchControls({}), false);
});

test("the guide's quadrotor, fixed-wing and rover scenarios bind gamepad sticks", async () => {
  for (const scenario of [
    "quadrotor/rumoca-scenario.acro.toml",
    "fixedwing/rumoca-scenario.toml",
    "rover/rumoca-scenario.toml",
  ]) {
    const text = await readFile(
      new URL(`../../../examples/interactive/${scenario}`, import.meta.url),
      "utf8",
    );
    assert.match(text, /^mode = "auto"$/m, `${scenario} must accept gamepad input`);
    assert.match(text, /^\[input\.gamepad\.(axes|integrators)\./m, `${scenario} binds stick axes`);
  }
});

test("touch overlay hides on fine pointers and does not block the capture handlers", async () => {
  const touch = await readFile(new URL("../runtime/rumoca_touch_controls.js", import.meta.url), "utf8");
  const runtime = await readFile(new URL("../runtime/rumoca_interactive.js", import.meta.url), "utf8");
  assert.match(touch, /matchMedia\(COARSE_POINTER_QUERY\)/);
  assert.match(touch, /const COARSE_POINTER_QUERY = '\(pointer: coarse\)'/);
  assert.match(touch, /touch-action: none/);
  assert.match(touch, /env\(safe-area-inset-bottom/);
  assert.match(runtime, /closest\?\.\('\.rumoca-interactive-controls, \.rumoca-touch-controls'\)/);
});

test("stick shaping removes the deadzone, keeps full range and softens center", () => {
  assert.equal(shapeStickAxis(0.1, 0.12, 0.7), 0);
  assert.equal(shapeStickAxis(-0.12, 0.12, 0.7), 0);
  close(shapeStickAxis(1, 0.12, 0.7), 1, "full deflection");
  close(shapeStickAxis(-1, 0.12, 0.7), -1, "full negative deflection");
  // Linear shaping rescales the live range so the output starts at zero.
  close(shapeStickAxis(0.56, 0.12, 0), 0.5, "rescaled midpoint");
  // Expo lowers the response near center and is odd-symmetric.
  const soft = shapeStickAxis(0.56, 0.12, 0.7);
  assert(soft < 0.5 && soft > 0, `expo response ${soft}`);
  close(shapeStickAxis(-0.56, 0.12, 0.7), -soft, "odd symmetry");
  assert.equal(shapeStickAxis(Number.NaN, 0.12, 0.7), 0);
});

test("touch axis shaping follows what the scenario binds each stick axis to", () => {
  const shapes = touchAxisShapes(quadrotorConfig());
  // Left stick Y integrates throttle with its own deadband: no extra shaping.
  assert.deepEqual(shapes[1], { deadzone: 0, expo: 0 });
  // Attitude axes get the deadzone and expo.
  assert.equal(shapes[0].expo > 0 && shapes[0].deadzone > 0, true);
  assert.equal(shapes[2].expo > 0 && shapes[3].expo > 0, true);

  const rover = touchAxisShapes({
    input: {
      gamepad: {
        axes: {
          steering: { source: "RightStickX", write: "steering" },
          throttle: { source: "LeftStickY", write: "throttle" },
        },
      },
    },
  });
  assert.equal(rover[2].expo > 0, true, "steering is shaped");
  assert.equal(rover[1].expo, 0, "a throttle axis stays linear");
  assert.equal(rover[1].deadzone > 0, true, "a throttle axis still has a deadzone");

  const unbound = touchAxisShapes({});
  assert.equal(unbound.length, 4);
  assert(unbound.every((shape) => shape.expo === 0 && shape.deadzone > 0));
});

test("the quadrotor stick axes point the same way as its keyboard keys", async () => {
  const text = await readFile(
    new URL("../../../examples/interactive/quadrotor/rumoca-scenario.acro.toml", import.meta.url),
    "utf8",
  );
  const section = (name) => {
    const match = new RegExp(`^\\[${name.replace(/\./g, "\\.")}\\]\\n((?:[^\\[\\n][^\\n]*\\n?)*)`, "m").exec(text);
    assert(match, `missing [${name}]`);
    return match[1];
  };
  const axis = (name) => {
    const body = section(`input.gamepad.axes.${name}`);
    return {
      source: /^source = "(\w+)"/m.exec(body)?.[1],
      invert: /^invert = true$/m.test(body),
    };
  };
  const key = (name) => Number(/^value = (-?[\d.]+)/m.exec(section(`input.keyboard.keys.${name}`))[1]);
  // Stick up reads +1 on RightStickY / LeftStickY after the runtime's sign flip,
  // and stick right reads +1 on the X axes.
  assert.deepEqual(axis("roll"), { source: "RightStickY", invert: false });
  assert(key("ArrowUp") > 0, "ArrowUp drives roll_cmd positive, as the stick does when pushed up");
  assert.deepEqual(axis("pitch"), { source: "RightStickX", invert: true });
  assert(key("ArrowRight") < 0, "ArrowRight drives pitch_cmd negative, as the inverted X axis does");
  assert.deepEqual(axis("yaw"), { source: "LeftStickX", invert: true });
  assert(key("d") < 0, "d (right) drives yaw_cmd negative, as the inverted X axis does");
});
