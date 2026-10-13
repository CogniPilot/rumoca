// Runs the module exported by `native_step ... EXPORT_DIR` on V8 and times each call.
//   node native_step.mjs EXPORT_DIR [STEPS] [--init INIT_EXPORT_DIR]...
// With `--frames DIR` at export time, each step first writes the captured RGB/Z16
// frame into the input slots (and, with `--carry-state`, moves `next.*` outputs
// into `previous.*` inputs); without it the inputs keep their exported values.
// Each `--init` (repeatable, in order) first runs one step of another export, for
// example the state constructor then the initializing entry point, and loads its
// `next.*` outputs into the following module's `previous.*` inputs by name; those
// steps are reported separately and are not timed as steps.
import { readFileSync } from "node:fs";

const MATH = ["abs", "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "exp", "log", "log10", "pow", "atan2"];
const args = process.argv.slice(2);
const initDirs = [];
for (let at = args.indexOf("--init"); at >= 0; at = args.indexOf("--init")) initDirs.push(args.splice(at, 2)[1]);
const [dir, stepsArg] = args;

function readState(exportDir, layout) {
  const count = layout.state?.count;
  if (!count) return null;
  const typed = (name, Type) => { const b = readFileSync(`${exportDir}/${name}`); return new Type(b.buffer, b.byteOffset, b.byteLength / Type.BYTES_PER_ELEMENT); };
  const init = typed("state_init.bin", Float64Array);
  return { count, in: typed("state_in.bin", Uint32Array), out: typed("state_out.bin", Uint32Array),
    lane: typed("state_lane.bin", Uint8Array), init: init.length === count ? init : null };
}

async function load(exportDir) {
  const layout = JSON.parse(readFileSync(`${exportDir}/layout.json`, "utf8"));
  const memory = new WebAssembly.Memory({ initial: layout.pages });
  const env = { memory };
  for (const name of MATH) env[name] = Math[name];
  const started = process.hrtime.bigint();
  const module = await WebAssembly.compile(readFileSync(`${exportDir}/module.wasm`));
  const compileSeconds = Number(process.hrtime.bigint() - started) / 1e9;
  const instance = await WebAssembly.instantiate(module, { env });
  const bytes = new Uint8Array(memory.buffer);
  bytes.set(readFileSync(`${exportDir}/y.bin`), 0);
  bytes.set(readFileSync(`${exportDir}/p.bin`), layout.p_offset);
  bytes.set(readFileSync(`${exportDir}/lanes.bin`), layout.typed_offset);
  const view = new DataView(memory.buffer);
  for (const [, offset, value] of layout.settings ?? []) view.setFloat64(offset, value, true);
  const state = readState(exportDir, layout);
  if (state?.init) {
    const live = new DataView(memory.buffer);
    for (let i = 0; i < state.count; i++) {
      const at = state.in[i], value = state.init[i];
      if (state.lane[i] === 0) live.setFloat64(at, value, true);
      else if (state.lane[i] === 1) live.setBigInt64(at, BigInt(value), true);
      else live.setUint8(at, value !== 0 ? 1 : 0);
    }
  }
  return { layout, memory, state, entry: instance.exports.eval_assignments, compileSeconds };
}

const toReal = (raw, width) => {
  const out = new Float64Array(raw.length / width);
  if (width === 1) for (let i = 0; i < out.length; i++) out[i] = raw[i];
  else { const v = new DataView(raw.buffer, raw.byteOffset, raw.byteLength); for (let i = 0; i < out.length; i++) out[i] = v.getUint16(2 * i, true); }
  return out;
};

function feed({ layout, memory }, step) {
  const frames = layout.frames;
  if (!frames) return;
  if (step > 0 && frames.carry_state) {
    const live = new Uint8Array(memory.buffer), carry = layout.carry;
    for (let i = 0; i < carry.length; i += 3) live.copyWithin(carry[i + 1], carry[i], carry[i] + carry[i + 2]);
  }
  const every = frames.epoch_every ?? 1, image = Math.floor(step / every), slots = frames.slots;
  const k = (frames.first_frame + image) % frames.count;
  const put = (slot, values) => { if (slot) new Float64Array(memory.buffer, slot[0], values.length).set(values); };
  if (step % every === 0) {
    put(slots.rgb, toReal(readFileSync(`${frames.dir}/frame-${k}.rgb8`), 1));
    put(slots.depth, toReal(readFileSync(`${frames.dir}/frame-${k}.z16-le`), 2));
  }
  const view = new DataView(memory.buffer);
  if (slots.interval_time != null) view.setFloat64(slots.interval_time, frames.interval_time + frames.interval_step * step, true);
  if (slots.image_epoch != null) view.setBigInt64(slots.image_epoch, BigInt(frames.first_epoch + image), true);
}

const laneValue = (view, lane, at) =>
  lane === "f64" ? view.getFloat64(at, true) : lane === "i64" ? Number(view.getBigInt64(at, true)) : view.getUint8(at) !== 0;

// Scalar non-state outputs plus true counts of Boolean arrays.
function summary({ layout, memory }) {
  const view = new DataView(memory.buffer);
  const scalars = {}, counts = {};
  for (const o of layout.outputs) {
    const name = o.name ?? "";
    if (name.startsWith("next.")) continue;
    const value = laneValue(view, o.lane, layout.typed_offset + o.offset);
    const bracket = name.indexOf("[");
    if (bracket < 0) scalars[name] = value;
    else if (o.lane === "u8") { const c = (counts[name.slice(0, bracket)] ??= [0, 0]); c[0] += value ? 1 : 0; c[1]++; }
  }
  return { scalars, boolean_arrays_true_of_total: counts };
}

// Occupancy flags of the carried state: true count over total per array.
function stateCounts({ layout, memory }) {
  const view = new DataView(memory.buffer), counts = {};
  for (const o of layout.outputs) {
    const name = o.name ?? "";
    const bracket = name.indexOf("[");
    if (!name.startsWith("next.") || bracket < 0 || o.lane !== "u8") continue;
    const c = (counts[name.slice(5, bracket)] ??= [0, 0]);
    c[0] += view.getUint8(layout.typed_offset + o.offset) !== 0 ? 1 : 0;
    c[1]++;
  }
  return counts;
}

function call(runner, step) {
  const { layout, entry } = runner;
  const started = process.hrtime.bigint();
  const status = entry(0, layout.p_offset, step / 30, layout.scratch_offset, layout.typed_bytes ? layout.typed_offset : 0);
  return { status, seconds: Number(process.hrtime.bigint() - started) / 1e9 };
}

// Copies `next.*` outputs of one module into the `previous.*` inputs of the next.
function transfer(from, to) {
  if (from.layout.state.next_hash !== to.layout.state.previous_hash) throw new Error("state name sets differ between modules");
  const source = new Uint8Array(from.memory.buffer), target = new Uint8Array(to.memory.buffer);
  const { count, out } = from.state;
  for (let i = 0; i < count; i++) {
    const width = to.state.lane[i] === 2 ? 1 : 8;
    const f = out[i], t = to.state.in[i];
    for (let b = 0; b < width; b++) target[t + b] = source[f + b];
  }
  return { transferred_scalars: count };
}

const runner = await load(dir);
const { layout, memory } = runner;
const n = Number(stepsArg ?? 0) || layout.steps;

const init = [];
let upstream = null;
for (const initDir of initDirs) {
  const stage = await load(initDir);
  if (upstream) init.push({ ...transfer(upstream, stage), dir: initDir });
  feed(stage, 0);
  const { status, seconds } = call(stage, 0);
  console.error("init step", initDir, status, seconds);
  if (status !== 0) throw new Error(`init step failed with status ${status}`);
  upstream = stage;
  init.push({ dir: initDir, status, seconds, summary: summary(stage), state_counts: stateCounts(stage) });
}
if (upstream) init.push({ ...transfer(upstream, runner), dir: "main" });

const times = [], summaries = [], stateAfter = [];
let status = 0;
for (let step = 0; step < n; step++) {
  feed(runner, step);
  const timed = call(runner, step);
  status = timed.status;
  times.push(timed.seconds);
  console.error("step", step, status, timed.seconds);
  if (status !== 0) break;
  if (layout.frames) { summaries.push(summary(runner)); stateAfter.push(stateCounts(runner)); }
}
const view = new DataView(memory.buffer);
const outputs = layout.outputs.slice(0, 8).map((o) => ({ name: o.name, lane: o.lane, value: o.lane === "i64" ? view.getBigInt64(layout.typed_offset + o.offset, true).toString() : laneValue(view, o.lane, layout.typed_offset + o.offset) }));
const yHead = layout.y_names.slice(0, 8).map((name, i) => ({ name, value: view.getFloat64(i * 8, true) }));
const every = layout.frames?.epoch_every ?? 1;
const split = (select) => {
  const t = times.filter((_, i) => select(i)).sort((a, b) => a - b);
  return t.length ? { n: t.length, min: t[0], median: t[Math.floor(t.length / 2)], max: t[t.length - 1] } : null;
};
const epochSplit = { epoch_change_steps: split((i) => i % every === 0), same_epoch_steps: split((i) => i % every !== 0) };
const steady = (times.length > 1 ? times.slice(1) : times).slice().sort((a, b) => a - b);
console.log(JSON.stringify({
  engine: `node ${process.version} (V8 ${process.versions.v8})`, steps_run: times.length, final_status: status,
  v8_compile_seconds: runner.compileSeconds, first_step: times[0], steady_min: steady[0], steady_median: steady[Math.floor(steady.length / 2)],
  steady_max: steady[steady.length - 1], all: times, peak_rss_kib: Number(/VmHWM:\s+(\d+)/.exec(readFileSync("/proc/self/status", "utf8"))[1]),
  frames: layout.frames ? { dir: layout.frames.dir, count: layout.frames.count, first_frame: layout.frames.first_frame, carry_state: layout.frames.carry_state, carry_runs: layout.carry.length / 3 } : null,
  epoch_split: epochSplit, init, y_head: yHead, derived_outputs_head: outputs, output_summaries: summaries, state_counts_after_step: stateAfter,
}, null, 1));
