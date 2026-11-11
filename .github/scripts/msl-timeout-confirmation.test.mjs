import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { test } from 'node:test';
import { spawnSync } from 'node:child_process';
import { confirmAndAnnotate, confirmationArgs, isTimeout, runConfirmation } from './msl-timeout-confirmation.mjs';

const MAIN = '1'.repeat(40), MERGE = '2'.repeat(40);
const binding = { sha: MAIN, runId: 7, artifactId: 9 };
const row = (model_name, sim_status = 'sim_ok') => ({ model_name, phase_reached: 'Success',
  is_balanced: true, initial_balance_ok: true, ir_solve_seconds: 0.1, ic_status: 'ic_ok', sim_status });
const data = (sha, rows) => ({ quality: { git_commit: sha, run_scope: 'full', parity_measured: true,
  msl_version: '4.1.0', omc_version: '1.26', quality_gate_version: 14, sim_target_models: rows.length,
  model_attempt_timeout_seconds: 20, sim_timeout_seconds: 45 },
results: { git_commit: sha, msl_version: '4.1.0', model_results: rows } });
function write(dir, snapshot) {
  fs.mkdirSync(dir, { recursive: true });
  fs.writeFileSync(path.join(dir, 'msl_quality_current.json'), JSON.stringify(snapshot.quality));
  fs.writeFileSync(path.join(dir, 'msl_results.json'), JSON.stringify(snapshot.results));
}
function setup(t, headRows = [row('Modelica.A', 'sim_timeout')], baseRows = headRows.map(r => row(r.model_name))) {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'msl-confirmation-test-'));
  t.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const baselineDir = path.join(root, 'main'), resultsDir = path.join(root, 'pr');
  write(baselineDir, data(MAIN, baseRows)); write(resultsDir, data(MERGE, headRows));
  fs.writeFileSync(path.join(resultsDir, 'msl_pr_comment.md'), '<!-- rumoca-msl-quality-summary -->\nOriginal report');
  return { baselineDir, resultsDir, currentSha: MERGE, binding, binaryDir: '/prebuilt/bin' };
}
function runnerWith(make = name => data(MERGE, [row(name)])) {
  const calls = [];
  const runner = async request => { calls.push(request); const d = make(request.model);
    d.quality.run_scope = 'partial'; write(request.dir, d); return { code: 0 }; };
  return { calls, runner };
}

test('typed timeout markers cover simulation and earlier phases, never error text', () => {
  for (const marker of [{ sim_status: 'sim_timeout' }, { timeout_phase: 'Flatten' },
    { error_code: 'EMSL_TIMEOUT_MODEL_ATTEMPT' }, { sim_error_code: 'EMSL_TIMEOUT_MODEL_ATTEMPT' },
    { failure_error_code: 'EMSL_TIMEOUT_MODEL_ATTEMPT' }]) assert.ok(isTimeout(marker));
  for (const marker of [{}, { timeout_phase: '' }, { error: 'timeout' },
    { error_code: 'EMSL_MODEL_WORKER_MEMORY_LIMIT' }]) assert.ok(!isTimeout(marker));
});

test('one confirmation retains original counts and both rows while recovering coverage', async t => {
  const options = setup(t), { runner, calls } = runnerWith();
  const original = fs.readFileSync(path.join(options.resultsDir, 'msl_results.json'));
  const result = await confirmAndAnnotate({ ...options, runner });
  assert.equal(calls.length, 1); assert.equal(result.losses.length, 0);
  assert.equal(result.initialLosses.length, 1);
  assert.deepEqual(result.counts.Simulated, { main: 1, current: 0 });
  assert.deepEqual(result.confirmedCounts.Simulated, { main: 1, current: 1 });
  assert.equal(result.confirmations[0].original.sim_status, 'sim_timeout');
  assert.equal(result.confirmations[0].row.sim_status, 'sim_ok');
  assert.deepEqual(fs.readFileSync(path.join(options.resultsDir, 'msl_results.json')), original);
  const comment = fs.readFileSync(path.join(options.resultsDir, 'msl_pr_comment.md'), 'utf8');
  assert.match(comment, /sim_timeout.*sim_ok/); assert.match(comment, /Initial measurements.*unchanged/);
  await assert.rejects(confirmAndAnnotate({ ...options, runner }), /EEXIST/);
  assert.equal(calls.length, 1);
});

test('a repeated timeout stays red, without a third attempt', async t => {
  const options = setup(t), { runner, calls } = runnerWith(name => data(MERGE, [row(name, 'sim_timeout')]));
  const result = await confirmAndAnnotate({ ...options, runner });
  assert.equal(calls.length, 1); assert.equal(result.losses.length, 1);
  assert.equal(result.confirmations[0].outcome, 'unconfirmed');
});

test('recovering Flatten but missing later stages from main cannot clear any loss', async t => {
  const options = setup(t, [{ model_name: 'Modelica.A', phase_reached: 'Flatten', timeout_phase: 'Flatten' }]);
  const { runner } = runnerWith(name => data(MERGE, [{ model_name: name, phase_reached: 'ToDae' }]));
  const result = await confirmAndAnnotate({ ...options, runner });
  assert.equal(result.confirmations[0].outcome, 'unconfirmed');
  assert.ok(result.losses.some(loss => loss.stage === 'Flattened'));
});

test('compile timeout can recover every main stage even when main never simulated', async t => {
  const base = row('Modelica.A', 'sim_solver_fail');
  const options = setup(t, [{ model_name: base.model_name, phase_reached: 'Flatten', timeout_phase: 'Flatten' }], [base]);
  const { runner } = runnerWith(() => data(MERGE, [base]));
  const result = await confirmAndAnnotate({ ...options, runner });
  assert.equal(result.losses.length, 0); assert.equal(result.confirmations[0].outcome, 'recovered');
});

test('non-timeout losses remain red and gains never offset them', async t => {
  const options = setup(t, [row('Modelica.A', 'sim_solver_fail'), row('Modelica.B')],
    [row('Modelica.A'), row('Modelica.B', 'sim_solver_fail')]);
  const { runner, calls } = runnerWith(); const result = await confirmAndAnnotate({ ...options, runner });
  assert.equal(calls.length, 0); assert.equal(result.losses.length, 1);
  assert.deepEqual(result.counts.Simulated, { main: 1, current: 1 });
});

test('wrong source, model, context, budgets and extra model rows cannot recover a loss', async t => {
  for (const mutate of [d => { d.results.git_commit = MAIN; }, d => { d.quality.git_commit = MAIN; },
    d => { d.results.model_results[0].model_name = 'Modelica.Other'; },
    d => { d.quality.sim_timeout_seconds = 90; }, d => { d.quality.omc_version = 'other'; },
    d => { d.results.model_results.push(row('Modelica.Other')); }]) {
    const options = setup(t), { runner } = runnerWith(name => { const d = data(MERGE, [row(name)]); mutate(d); return d; });
    const result = await confirmAndAnnotate({ ...options, runner });
    assert.equal(result.losses.length, 1); assert.equal(result.confirmations[0].outcome, 'unconfirmed');
  }
});

test('unmeasured quality binds the actual focused OMC reference without altering either raw payload', async t => {
  const options = setup(t);
  const runner = async ({dir, model}) => {
    const d = data(MERGE, [row(model)]); d.quality.run_scope = 'partial';
    d.quality.parity_measured = false; delete d.quality.omc_version; write(dir, d);
    fs.writeFileSync(path.join(dir, 'omc_simulation_reference.json'), JSON.stringify({
      git_commit: MERGE, msl_version: 'v4.1.0', omc_version: '1.26', total_models: 1, models: {[model]: {}},
    }));
    return {code: 0};
  };
  const result = await confirmAndAnnotate({...options, runner});
  assert.equal(result.losses.length, 0);
  const c = result.confirmations[0];
  assert.equal(c.omcContext.source, 'omc_simulation_reference.json');
  assert.match(c.omcContext.sha256, /^[0-9a-f]{64}$/);
  assert.equal(JSON.parse(fs.readFileSync(path.join(options.resultsDir, 'timeout-confirmations/1/msl_quality_current.json'))).omc_version, undefined);
});

test('missing or mismatched reference provenance never clears an unmeasured confirmation', async t => {
  for (const mutate of [r => {delete r.omc_version;}, r => {r.omc_version = 'wrong';},
    r => {r.git_commit = MAIN;}, r => {r.msl_version = 'other';}, r => {r.total_models = 2;},
    r => {r.models = {'Modelica.Other': {}};}, () => null]) {
    const options = setup(t);
    const runner = async ({dir, model}) => {
      const d = data(MERGE, [row(model)]); d.quality.run_scope = 'partial';
      d.quality.parity_measured = false; delete d.quality.omc_version; write(dir, d);
      const reference = {git_commit: MERGE, msl_version: '4.1.0', omc_version: '1.26', total_models: 1, models: {[model]: {}}};
      if (mutate(reference) !== null) fs.writeFileSync(path.join(dir, 'omc_simulation_reference.json'), JSON.stringify(reference));
      return {code: 0};
    };
    const result = await confirmAndAnnotate({...options, runner});
    assert.equal(result.losses.length, 1); assert.equal(result.confirmations[0].outcome, 'unconfirmed');
  }
});

test('real BatchPlant confirmation recovers all five compile stages only with its bound OMC reference', async t => {
  const real = JSON.parse(fs.readFileSync(new URL('./fixtures/msl-batchplant-confirmation.json', import.meta.url)));
  for (const withReference of [true, false]) {
    const options = setup(t, [real.currentRow], [real.mainRow]);
    const current = data(real.quality.git_commit, [real.currentRow]);
    Object.assign(current.quality, {msl_version: real.quality.msl_version, omc_version: real.reference.omc_version});
    current.results.msl_version = real.quality.msl_version;
    write(options.resultsDir, current); options.currentSha = real.quality.git_commit;
    const main = data(MAIN, [real.mainRow]);
    Object.assign(main.quality, {msl_version: real.quality.msl_version, omc_version: real.reference.omc_version});
    main.results.msl_version = real.quality.msl_version; write(options.baselineDir, main);
    const runner = async ({dir}) => {
      write(dir, {quality: real.quality, results: real.results});
      if (withReference) fs.writeFileSync(path.join(dir, 'omc_simulation_reference.json'), JSON.stringify(real.reference));
      return {code: 0};
    };
    const result = await confirmAndAnnotate({...options, runner});
    assert.equal(result.initialLosses.length, 5);
    assert.equal(result.losses.length, withReference ? 0 : 5);
    assert.deepEqual(result.confirmations[0].row, real.results.model_results[0]);
    assert.equal(result.confirmations[0].row.sim_status, 'sim_solver_fail');
    if (withReference) assert.equal(result.confirmations[0].recoveredStages.length, 5);
  }
});

test('failed execution cannot supply a success row; missing output and runner failure stay red', async t => {
  for (const mode of ['exit', 'missing', 'throw']) {
    const options = setup(t);
    const runner = async request => {
      if (mode === 'throw') throw Error('spawn failed');
      if (mode === 'exit') write(request.dir, data(MERGE, [row(request.model)]));
      return { code: mode === 'exit' ? 1 : 0 };
    };
    const result = await confirmAndAnnotate({ ...options, runner });
    assert.equal(result.losses.length, 1); assert.equal(result.confirmations[0].outcome, 'unconfirmed');
  }
});

test('four-model cap leaves excess losses red and total deadline prevents extra launches', async t => {
  const head = Array.from({ length: 6 }, (_, i) => row('Modelica.M' + i, 'sim_timeout'));
  const options = setup(t, head), { runner, calls } = runnerWith();
  const result = await confirmAndAnnotate({ ...options, runner });
  assert.equal(calls.length, 4); assert.equal(result.losses.length, 2);
  assert.match(fs.readFileSync(path.join(options.resultsDir, 'msl_pr_comment.md'), 'utf8'), /2 unconfirmed/);
  let clock = 0; const other = setup(t, head), next = runnerWith();
  const bounded = await confirmAndAnnotate({ ...other, now: () => clock,
    runner: async request => { const r = await next.runner(request); clock = 600001; return r; } });
  assert.equal(next.calls.length, 1); assert.equal(bounded.losses.length, 5);
});

test('confirmation arguments bind exact model, unchanged profile and all prebuilt workers', () => {
  const args = confirmationArgs('/artifact/bin', '/results/1', 'Modelica.A');
  assert.deepEqual(args.slice(0, 3), ['xtask', 'verify', 'msl-parity']);
  for (const token of ['--sim-match-exact', '--budget-profile', 'ci', '/artifact/bin/msl_tests',
    '/artifact/bin/rumoca-worker', '/artifact/bin/rumoca-sim-worker']) assert.ok(args.includes(token));
  assert.ok(!args.includes('--rerun-timeouts-alone')); assert.ok(!args.includes('--sim-timeout-secs'));
});

test('actual Actions loss step launches exactly one confirmation and retains the initial timeout', t => {
  const options = setup(t), root = path.dirname(options.resultsDir);
  const target = path.join(root, 'target/msl');
  fs.mkdirSync(target, { recursive: true });
  fs.renameSync(options.baselineDir, path.join(target, 'pr-main-baseline'));
  fs.renameSync(options.resultsDir, path.join(target, 'results'));
  const scripts = path.join(root, '.github/scripts'); fs.mkdirSync(scripts, { recursive: true });
  for (const name of ['msl-main-baseline.mjs', 'msl-timeout-confirmation.mjs']) {
    fs.copyFileSync(new URL(name, import.meta.url), path.join(scripts, name));
  }
  const bin = path.join(root, 'fake-bin'); fs.mkdirSync(bin);
  const fixture = data(MERGE, [row('Modelica.A')]); fixture.quality.run_scope = 'partial';
  const fake = path.join(bin, 'cargo');
  fs.writeFileSync(fake, '#!' + process.execPath + '\nconst fs=require("node:fs"),path=require("node:path");'
    + 'const args=process.argv.slice(2),dir=args[args.indexOf("--results-dir")+1],d=' + JSON.stringify(fixture) + ';'
    + 'fs.writeFileSync(path.join(dir,"msl_results.json"),JSON.stringify(d.results));'
    + 'fs.writeFileSync(path.join(dir,"msl_quality_current.json"),JSON.stringify(d.quality));'
    + 'fs.writeFileSync(path.join(dir,"ARGV.json"),JSON.stringify(args));\n', { mode: 0o755 });
  const workflow = fs.readFileSync(new URL('../workflows/ci.yml', import.meta.url), 'utf8');
  const script = workflow.split('      - name: Check PR model losses against tested main')[1]
    .split('      - name: Publish MSL PR summary comment')[0].split('          script: |\n')[1]
    .split('\n').map(line => line.slice(12)).join('\n');
  const runner = path.join(root, 'run.cjs');
  fs.writeFileSync(runner, 'const core={setFailed(m){console.error(m);process.exitCode=1}};'
    + '(async()=>{const F=Object.getPrototypeOf(async function(){}).constructor;await new F("require","core",'
    + JSON.stringify(script) + ')(require,core)})().catch(e=>{console.error(e);process.exitCode=1});');
  const result = spawnSync(process.execPath, [runner], { cwd: root, encoding: 'utf8',
    env: { ...process.env, PATH: bin + ':' + process.env.PATH, CI_SOURCE_SHA: MERGE,
      MAIN_BASELINE_SHA: MAIN, MAIN_BASELINE_RUN_ID: '7', MAIN_BASELINE_ARTIFACT_ID: '9' } });
  assert.equal(result.status, 0, result.stderr);
  const dir = path.join(target, 'results'), report = JSON.parse(fs.readFileSync(path.join(dir, 'msl_main_comparison.json')));
  assert.equal(report.initialLosses.length, 1); assert.equal(report.losses.length, 0);
  assert.equal(report.confirmations.length, 1);
  assert.equal(JSON.parse(fs.readFileSync(path.join(dir, 'msl_results.json'))).model_results[0].sim_status, 'sim_timeout');
  assert.deepEqual(fs.readdirSync(path.join(dir, 'timeout-confirmations')), ['1']);
  const args = JSON.parse(fs.readFileSync(path.join(dir, 'timeout-confirmations/1/ARGV.json')));
  assert.equal(args[args.indexOf('--sim-match') + 1], 'Modelica.A');
  assert.equal(args[args.indexOf('--budget-profile') + 1], 'ci');
});

test('real launcher captures a failed command and kills its process group at the watchdog', async t => {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'msl-launcher-test-'));
  t.after(() => fs.rmSync(root, { recursive: true, force: true }));
  const fake = path.join(root, 'cargo'), heartbeat = path.join(root, 'heartbeat');
  fs.writeFileSync(fake, '#!' + process.execPath + '\n' +
    'const {spawn}=require("node:child_process"); const fs=require("node:fs");\n' +
    'spawn(process.execPath,["-e",' + JSON.stringify('const fs=require("node:fs");setInterval(()=>fs.writeFileSync(' + JSON.stringify(heartbeat) + ',String(Date.now())),10)') + '],{stdio:"ignore"});setInterval(()=>{},1000);\n');
  fs.chmodSync(fake, 0o755);
  const oldPath = process.env.PATH; process.env.PATH = root + ':' + oldPath;
  t.after(() => { process.env.PATH = oldPath; });
  const dir = path.join(root, 'attempt'); fs.mkdirSync(dir);
  const result = await runConfirmation({ cwd: root, binaryDir: '/artifact/bin', dir, model: 'Modelica.A', timeoutMs: 500 });
  assert.equal(result.timedOut, true); assert.notEqual(result.code, 0);
  const before = fs.readFileSync(heartbeat, 'utf8');
  await new Promise(resolve => setTimeout(resolve, 80));
  assert.equal(fs.readFileSync(heartbeat, 'utf8'), before, 'descendant kept running');
});
