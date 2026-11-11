import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { createHash } from 'node:crypto';
import { spawn } from 'node:child_process';
import { readFullSnapshot, readMainBaseline, compareModels, writeComparison } from './msl-main-baseline.mjs';

export const CONFIRMATION_LIMITS = Object.freeze({ models: 4, totalMs: 600000, attemptMs: 300000 });
const payloads = ['msl_quality_current.json', 'msl_results.json'];
const digest = file => createHash('sha256').update(fs.readFileSync(file)).digest('hex');
const read = (dir, name) => JSON.parse(fs.readFileSync(path.join(dir, name), 'utf8'));

export function isTimeout(row) {
  return row?.sim_status === 'sim_timeout' || (typeof row?.timeout_phase === 'string' && row.timeout_phase.length > 0)
    || ['error_code', 'sim_error_code', 'failure_error_code'].some(key => row?.[key] === 'EMSL_TIMEOUT_MODEL_ATTEMPT');
}

export function confirmationArgs(binaryDir, dir, model) {
  return ['xtask', 'verify', 'msl-parity', '--results-dir', dir,
    '--budget-profile', 'ci', '--sim-match-exact', '--sim-match', model,
    '--stage-parallelism', '1', '--sim-parallelism', '1', '--sim-worker-memory-mb', '2048',
    '--sim-total-memory-mb', '6144', '--monitor-interval-secs', '10', '--no-remote-quality-baseline',
    '--allow-unmeasured-parity', '--prebuilt-test-binary', path.join(binaryDir, 'msl_tests'),
    '--prebuilt-model-worker', path.join(binaryDir, 'rumoca-worker'),
    '--prebuilt-sim-worker', path.join(binaryDir, 'rumoca-sim-worker')];
}

export function timeoutLossModels(comparison, current) {
  const byModel = new Map(current.results.model_results.map(row => [row.model_name, row]));
  return [...new Set(comparison.losses.map(loss => loss.model))].filter(model => isTimeout(byModel.get(model)));
}

// The job runs on Ubuntu. A watchdog terminates the whole new process group,
// including model workers; it never retries or reuses an attempt directory.
export async function runConfirmation({ cwd, binaryDir, dir, model, timeoutMs }) {
  assert.equal(process.platform, 'linux', 'confirmation requires POSIX process groups');
  const out = fs.openSync(path.join(dir, 'stdout.log'), 'wx');
  const err = fs.openSync(path.join(dir, 'stderr.log'), 'wx');
  try {
    return await new Promise(resolve => {
      const child = spawn('cargo', confirmationArgs(binaryDir, dir, model), {
        cwd, detached: true, stdio: ['ignore', out, err],
      });
      let timedOut = false;
      const kill = () => { if (child.pid) { try { process.kill(-child.pid, 'SIGKILL'); } catch (e) { if (e.code !== 'ESRCH') throw e; } } };
      const timer = setTimeout(() => { timedOut = true; kill(); }, timeoutMs);
      child.once('error', error => { clearTimeout(timer); kill(); resolve({ code: null, error: error.message }); });
      child.once('close', (code, signal) => { clearTimeout(timer); kill(); resolve({ code, signal, timedOut }); });
    });
  } finally { fs.closeSync(out); fs.closeSync(err); }
}

function confirmedRow(dir, current, model) {
  const quality = read(dir, payloads[0]), results = read(dir, payloads[1]);
  assert.equal(results.git_commit, current.results.git_commit, 'confirmation source differs');
  assert.equal(quality.git_commit, current.quality.git_commit, 'confirmation quality source differs');
  assert.equal(quality.run_scope, 'partial', 'confirmation must be an explicit focused run');
  for (const key of ['msl_version', 'quality_gate_version',
    'model_attempt_timeout_seconds', 'sim_timeout_seconds']) {
    assert.deepEqual(quality[key], current.quality[key], 'confirmation context differs: ' + key);
  }
  assert.equal(results.msl_version, quality.msl_version, 'confirmation MSL source differs');
  assert.equal(results.model_results?.length, 1, 'confirmation must contain exactly one model');
  assert.equal(results.model_results[0].model_name, model, 'confirmation selected another model');
  let omc = { version: quality.omc_version, source: payloads[0] };
  if (omc.version === undefined) {
    assert.equal(quality.parity_measured, false, 'only explicit unmeasured confirmations may use reference metadata');
    const name = 'omc_simulation_reference.json', reference = read(dir, name);
    assert.equal(reference.git_commit, results.git_commit, 'confirmation OMC reference source differs');
    assert.equal(reference.msl_version?.replace(/^v/, ''), quality.msl_version.replace(/^v/, ''), 'confirmation OMC reference MSL differs');
    assert.equal(reference.total_models, 1, 'confirmation OMC reference must cover one model');
    assert.deepEqual(Object.keys(reference.models ?? {}), [model], 'confirmation OMC reference selected another model');
    omc = { version: reference.omc_version, source: name, sha256: digest(path.join(dir, name)) };
  }
  assert.equal(omc.version, current.quality.omc_version, 'confirmation context differs: omc_version');
  return { row: results.model_results[0], omc };
}

export async function confirmAndAnnotate({ baselineDir, resultsDir, binding, currentSha,
  binaryDir, cwd = process.cwd(), runner = runConfirmation, now = Date.now }) {
  const baseline = readMainBaseline(baselineDir, binding), current = readFullSnapshot(resultsDir, currentSha);
  const initial = compareModels(baseline, current), confirmations = [], recovered = new Map();
  const byModel = new Map(current.results.model_results.map(row => [row.model_name, row]));
  const candidates = timeoutLossModels(initial, current);
  const pins = Object.fromEntries(payloads.map(name => [name, digest(path.join(resultsDir, name))]));
  const deadline = now() + CONFIRMATION_LIMITS.totalMs;
  const root = path.join(resultsDir, 'timeout-confirmations');
  if (candidates.length) {
    assert.equal(current.quality.model_attempt_timeout_seconds, 20, 'confirmation requires the CI phase budget');
    assert.equal(current.quality.sim_timeout_seconds, 45, 'confirmation requires the CI solver budget');
    assert.equal(baseline.quality.model_attempt_timeout_seconds, 20, 'main phase budget differs');
    assert.equal(baseline.quality.sim_timeout_seconds, 45, 'main solver budget differs');
    fs.mkdirSync(root); // Existing evidence is an error, never a retry/default path.
  }
  for (const [index, model] of candidates.entries()) {
    const attempt = { model, original: byModel.get(model), row: null, outcome: 'unconfirmed',
      recoveredStages: [], limits: CONFIRMATION_LIMITS };
    confirmations.push(attempt);
    const remainingMs = deadline - now();
    if (index >= CONFIRMATION_LIMITS.models || remainingMs <= 0) {
      attempt.reason = 'confirmation job limit reached'; continue;
    }
    const dir = path.join(root, String(index + 1));
    fs.mkdirSync(dir);
    fs.writeFileSync(path.join(dir, 'REQUEST.json'), JSON.stringify({ model, currentSha,
      command: 'cargo', args: confirmationArgs(binaryDir, dir, model),
      budgetProfile: 'ci', oneConfirmation: true, timeoutMs: Math.min(CONFIRMATION_LIMITS.attemptMs, remainingMs) }, null, 2), { flag: 'wx' });
    try {
      attempt.execution = await runner({ cwd, binaryDir, dir, model,
        timeoutMs: Math.min(CONFIRMATION_LIMITS.attemptMs, remainingMs) });
      if (fs.existsSync(path.join(dir, payloads[1]))) attempt.row = read(dir, payloads[1]).model_results?.[0] ?? null;
      assert.equal(attempt.execution.code, 0, 'confirmation process failed');
      assert.ok(!attempt.execution.timedOut, 'confirmation watchdog expired');
      const verified = confirmedRow(dir, current, model);
      attempt.row = verified.row; attempt.omcContext = verified.omc;
      attempt.payloads = Object.fromEntries(payloads.map(name => [name, digest(path.join(dir, name))]));
      const single = snapshot => ({ ...snapshot,
        results: { ...snapshot.results, model_results: snapshot.results.model_results.filter(row => row.model_name === model) } });
      const after = single(current); after.results.model_results = [attempt.row];
      assert.equal(compareModels(single(baseline), after).losses.length, 0,
        'confirmation did not recover every stage successful on main');
      attempt.outcome = 'recovered';
      attempt.recoveredStages = initial.losses.filter(loss => loss.model === model).map(loss => loss.stage);
      recovered.set(model, attempt.row);
    } catch (error) { attempt.reason = error.message; }
  }
  for (const name of payloads) assert.equal(digest(path.join(resultsDir, name)), pins[name], 'original measurement changed');
  const effective = { ...current, results: { ...current.results,
    model_results: current.results.model_results.map(row => recovered.get(row.model_name) ?? row) } };
  const confirmed = compareModels(baseline, effective);
  return writeComparison(resultsDir, binding, { ...initial, initialLosses: initial.losses,
    losses: confirmed.losses, confirmedCounts: confirmed.counts, confirmations, originalPayloads: pins }, baseline.provenance);
}
