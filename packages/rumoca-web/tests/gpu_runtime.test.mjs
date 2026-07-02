import assert from "node:assert/strict";
import test from "node:test";

import {
  gpuPIndexForName,
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
