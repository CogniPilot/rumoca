import { readFile } from "node:fs/promises";
import init, { simulate_model } from "../../../pkg/rumoca.js";

const SOURCE = `
model WasmSmoke
  Real x(start = 1.0);
equation
  der(x) = -x;
end WasmSmoke;
`;

const BALL_SOURCE = `
model BallWasmSmoke
  parameter Real e = 0.8;
  Real x(start = 1.0);
  Real v(start = 0.0);
equation
  der(x) = v;
  der(v) = -9.81;
  when x < 0 then
    reinit(v, -e * pre(v));
  end when;
end BallWasmSmoke;
`;

const ROOT_CROSS_SOURCE = `
model RootCrossWasmSmoke
  Real x(start = 0.0);
  Real s(start = 0.0);
  Real y;
equation
  der(x) = 1.0;
  der(s) = if x < 0.53 then 0.0 else 1.0;
  y = if x < 0.53 then 0.0 else 1.0;
end RootCrossWasmSmoke;
`;

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function parseSimulationJson(raw) {
  const parsed = JSON.parse(raw);
  assert(Array.isArray(parsed.times), "simulate_model: times must be an array");
  assert(Array.isArray(parsed.names), "simulate_model: names must be an array");
  assert(Array.isArray(parsed.data), "simulate_model: data must be an array");
  assert(
    parsed.data.length === parsed.names.length,
    "simulate_model: data row count must match names count",
  );
  return parsed;
}

function seriesByName(parsed, name) {
  const idx = parsed.names.indexOf(name);
  assert(idx >= 0, `simulate_model: missing output series '${name}'`);
  const values = parsed.data[idx];
  assert(Array.isArray(values), `simulate_model: series '${name}' must be an array`);
  assert(
    values.length === parsed.times.length,
    `simulate_model: series '${name}' length must match time samples`,
  );
  return values;
}

function runLinearSmoke() {
  const raw = simulate_model(SOURCE, "WasmSmoke", 0.2, 0.05);
  const parsed = parseSimulationJson(raw);

  assert(parsed.times.length >= 2, "simulate_model: expected at least 2 time samples");
  assert(
    parsed.names.includes("x"),
    `simulate_model: expected output names to include 'x', got ${JSON.stringify(parsed.names)}`,
  );
}

function runBouncingBallSmoke() {
  const raw = simulate_model(BALL_SOURCE, "BallWasmSmoke", 1.5, 0.01);
  const parsed = parseSimulationJson(raw);
  assert(
    parsed.times.length >= 20,
    `bouncing ball: expected at least 20 samples, got ${parsed.times.length}`,
  );
  const x = seriesByName(parsed, "x");
  const v = seriesByName(parsed, "v");

  let sawDownwardSpeed = false;
  let sawBounceJump = false;
  for (let i = 1; i < v.length; i += 1) {
    if (v[i - 1] < -0.5) {
      sawDownwardSpeed = true;
    }
    const dv = v[i] - v[i - 1];
    if (dv > 1.0 && v[i] > 0.2 && x[i] < 0.05) {
      sawBounceJump = true;
      break;
    }
  }

  const minX = Math.min(...x);
  assert(
    minX > -0.25,
    `bouncing ball: penetration too large (min x=${minX}), expected near-ground contact`,
  );
  assert(sawDownwardSpeed, "bouncing ball: expected downward motion before bounce");
  assert(sawBounceJump, "bouncing ball: expected at least one bounce velocity jump");
}

function runRootCrossingSmoke() {
  const raw = simulate_model(ROOT_CROSS_SOURCE, "RootCrossWasmSmoke", 1.0, 0.05);
  const parsed = parseSimulationJson(raw);
  const x = seriesByName(parsed, "x");
  const s = seriesByName(parsed, "s");
  const y = seriesByName(parsed, "y");

  const maxY = Math.max(...y);
  const finalY = y[y.length - 1];
  const maxX = Math.max(...x);
  const finalS = s[s.length - 1];

  assert(maxY >= 0.9, `root crossing: expected relation branch activation, max(y)=${maxY}`);
  assert(finalY >= 0.9, `root crossing: expected active branch at end, final(y)=${finalY}`);
  assert(maxX >= 0.9, `root crossing: expected x to advance through threshold, max(x)=${maxX}`);
  assert(
    finalS >= 0.3 && finalS <= 0.7,
    `root crossing: expected post-crossing derivative activation (final s in [0.3,0.7]), got ${finalS}`,
  );
}

async function run() {
  const wasmBytes = await readFile(new URL("../../../pkg/rumoca_bg.wasm", import.meta.url));
  await init(wasmBytes);
  runLinearSmoke();
  runBouncingBallSmoke();
  runRootCrossingSmoke();
}

run().catch((error) => {
  console.error("[wasm-smoke] simulation smoke test failed:");
  console.error(error);
  process.exit(1);
});
