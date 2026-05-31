import { readFile } from "node:fs/promises";
import { ensureNodeSelfForWasmBindgenRayon } from "./node_rayon_shim.mjs";

const SOURCE = `
model WasmSmoke
  Real x(start = 1.0);
equation
  der(x) = -x;
end WasmSmoke;
`;

const BALL_SOURCE = `
model BallWasmSmoke
  Real x(start = 1.0);
  Real v(start = 0.0);
equation
  der(x) = v;
  der(v) = -9.81;
end BallWasmSmoke;
`;

let simulate_model_fn = null;
const pkgSubdirArgIndex = process.argv.indexOf("--pkg-subdir");
const wasmPkgSubdir =
  pkgSubdirArgIndex >= 0 ? process.argv[pkgSubdirArgIndex + 1] : "release-full-web";

const COUPLED_STATE_SOURCE = `
model CoupledStateWasmSmoke
  Real x(start = 0.0);
  Real s(start = 0.0);
equation
  der(x) = 1.0;
  der(s) = x;
end CoupledStateWasmSmoke;
`;

function assert(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function parseSimulationJson(raw) {
  const parsed = JSON.parse(raw);
  const payload = parsed?.payload ?? parsed;
  assert(Array.isArray(payload.names), "simulate_model: names must be an array");
  assert(Array.isArray(payload.allData), "simulate_model: allData must be an array");
  assert(Array.isArray(payload.allData[0]), "simulate_model: allData[0] must be the time column");
  assert(
    payload.allData.length === payload.names.length + 1,
    "simulate_model: allData must contain one time column plus one data column per name",
  );
  return {
    payload,
    times: payload.allData[0],
    names: payload.names,
    data: payload.allData.slice(1),
    nStates: payload.nStates,
  };
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
  const raw = simulate_model_fn(SOURCE, "WasmSmoke", 0.2, 0.05, "auto");
  const parsed = parseSimulationJson(raw);

  assert(parsed.times.length >= 2, "simulate_model: expected at least 2 time samples");
  assert(
    parsed.names.includes("x"),
    `simulate_model: expected output names to include 'x', got ${JSON.stringify(parsed.names)}`,
  );
}

function runBouncingBallSmoke() {
  const raw = simulate_model_fn(BALL_SOURCE, "BallWasmSmoke", 1.5, 0.01, "auto");
  const parsed = parseSimulationJson(raw);
  assert(
    parsed.times.length >= 20,
    `bouncing ball: expected at least 20 samples, got ${parsed.times.length}`,
  );
  const x = seriesByName(parsed, "x");
  const v = seriesByName(parsed, "v");

  let sawDownwardSpeed = false;
  let sawFallingPosition = false;
  for (let i = 1; i < v.length; i += 1) {
    if (v[i] < -0.5 || v[i - 1] < -0.5) {
      sawDownwardSpeed = true;
    }
    if (x[i] < x[i - 1]) {
      sawFallingPosition = true;
    }
  }

  assert(sawDownwardSpeed, "bouncing ball: expected downward motion before bounce");
  assert(sawFallingPosition, "bouncing ball: expected position to decrease under gravity");
}

function runCoupledStateSmoke() {
  const raw = simulate_model_fn(COUPLED_STATE_SOURCE, "CoupledStateWasmSmoke", 1.0, 0.05, "auto");
  const parsed = parseSimulationJson(raw);
  const x = seriesByName(parsed, "x");
  const s = seriesByName(parsed, "s");

  const maxX = Math.max(...x);
  const finalS = s[s.length - 1];

  assert(maxX >= 0.9, `coupled state: expected x to advance to end time, max(x)=${maxX}`);
  assert(
    finalS >= 0.45 && finalS <= 0.55,
    `coupled state: expected integral of x to be near 0.5, got ${finalS}`,
  );
}

async function run() {
  ensureNodeSelfForWasmBindgenRayon();
  const wasmModule = await import(`../../../pkg/${wasmPkgSubdir}/rumoca_bind_wasm.js`);
  const init = wasmModule.default;
  simulate_model_fn = wasmModule.simulate_model;
  const wasmBytes = await readFile(
    new URL(`../../../pkg/${wasmPkgSubdir}/rumoca_bind_wasm_bg.wasm`, import.meta.url),
  );
  await init({ module_or_path: wasmBytes });
  runLinearSmoke();
  runBouncingBallSmoke();
  runCoupledStateSmoke();
}

run().catch((error) => {
  console.error("[wasm-smoke] simulation smoke test failed:");
  console.error(error);
  process.exit(1);
});
