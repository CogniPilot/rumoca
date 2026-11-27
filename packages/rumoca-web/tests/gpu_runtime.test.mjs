import assert from "node:assert/strict";
import test from "node:test";

import {
  gpuPIndexForName,
  gpuPeriodicClock,
  gpuRk4StepPlan,
} from "../runtime/rumoca_gpu.js";

test("GPU RK4 plan separates output interval from internal fixed step", () => {
  const plan = gpuRk4StepPlan(0, 30, 0.1, 0.0125);

  assert.equal(plan.outputCount, 300);
  assert.equal(plan.internalSteps, 2400);
  assert.equal(plan.outputDt, 0.1);
  assert.equal(plan.internalDt, 0.0125);
});

test("GPU RK4 plan handles a final partial output interval", () => {
  const plan = gpuRk4StepPlan(0, 1.05, 0.1, 0.0125);

  assert.equal(plan.outputCount, 11);
  assert.equal(plan.internalSteps, 84);
});

test("GPU input binding helper finds named P slots from solve layout", () => {
  const prep = {
    var_layout: {
      bindings: {
        x: { Y: { index: 0, byte_offset: 0 } },
        aoa_cmd: { P: { index: 3, byte_offset: 24 } },
      },
    },
  };

  assert.equal(gpuPIndexForName(prep, "aoa_cmd"), 3);
  assert.equal(gpuPIndexForName(prep, "x"), null);
  assert.equal(gpuPIndexForName(prep, "missing"), null);
});

test("periodic GPU events keep absolute phase across partial RK4 steps", () => {
  const clock = gpuPeriodicClock({period: [1, 10], phase: [1, 20], anchor: "absolute"}, 0);
  assert.equal(clock.next, 0.05);
  assert.equal(clock.consume(0.049), false);
  for (let i = 0; i < 1200; i++) {
    const t = 0.05 + i * 0.1;
    assert.equal(clock.consume(t), true);
    assert.equal(clock.consume(t), false);
  }
  const restart = gpuPeriodicClock({period: [1, 10], phase: [1, 20], anchor: "absolute"}, 0.23);
  assert.equal(restart.next, 0.25);
  const relative = gpuPeriodicClock({period: [1, 10], phase: [0, 1], anchor: "simulation_start"}, 5);
  assert.equal(relative.consume(5), true);
  assert.equal(relative.consume(5), false);
  assert.throws(() => gpuPeriodicClock({period: [0, 1], phase: [1, 1], anchor: "absolute"}, 0));
});
