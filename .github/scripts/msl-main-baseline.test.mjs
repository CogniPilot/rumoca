import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { test } from 'node:test';
import { spawnSync } from 'node:child_process';
import { ARTIFACT_NAME, testedMainSha, findMainArtifact, readFullSnapshot,
  recordMainSnapshot, readMainBaseline, compareModels, compareAndAnnotate,
  writeBaselineFailure } from './msl-main-baseline.mjs';

const MAIN = '1'.repeat(40), MERGE = '2'.repeat(40), HEAD = '3'.repeat(40);
const repo = { owner: 'owner', repo: 'project' };
const binding = { sha: MAIN, runId: 7, artifactId: 9 };
const marker = '<!-- rumoca-msl-quality-summary -->';

function row(model_name, sim_status = 'sim_ok') {
  return { model_name, phase_reached: 'Success', is_balanced: true,
    initial_balance_ok: true, ir_solve_seconds: 0.1, ic_status: 'ic_ok', sim_status };
}

function snapshot(sha = MAIN, rows = [row('Modelica.A'), row('Modelica.B')]) {
  return { quality: { git_commit: sha, run_scope: 'full', parity_measured: true,
    msl_version: '4.1.0', omc_version: '1.26', quality_gate_version: 14, sim_target_models: 2 },
  results: { git_commit: sha, msl_version: '4.1.0', model_results: rows } };
}

function directory(t, data) {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'msl-main-baseline-test-'));
  t.after(() => fs.rmSync(dir, { recursive: true, force: true }));
  fs.writeFileSync(path.join(dir, 'msl_quality_current.json'), JSON.stringify(data.quality));
  fs.writeFileSync(path.join(dir, 'msl_results.json'), JSON.stringify(data.results));
  return dir;
}

function run(overrides = {}) {
  return { id: 7, head_sha: MAIN, head_branch: 'main', event: 'push', status: 'completed',
    conclusion: 'failure', head_repository: { full_name: 'owner/project' }, ...overrides };
}

function artifact(overrides = {}) {
  return { id: 9, name: ARTIFACT_NAME, expired: false, workflow_run: { head_sha: MAIN }, ...overrides };
}

function api(runs, artifacts) {
  const calls = [];
  return { calls, rest: { actions: { listWorkflowRuns: 'runs', listWorkflowRunArtifacts: 'artifacts' } },
    paginate: async (method, args) => {
      calls.push({ method, args });
      return method === 'runs' ? runs() : artifacts(args.run_id);
    } };
}

test('PR baseline is the test merge first parent, never the branch point or head', () => {
  assert.equal(testedMainSha(MERGE, MERGE + ' ' + MAIN + ' ' + HEAD), MAIN);
  assert.throws(() => testedMainSha(MERGE, HEAD + ' ' + MAIN + ' ' + HEAD), /checkout does not match/);
  assert.throws(() => testedMainSha(MAIN, MAIN + ' ' + HEAD), /test merge/);
  assert.throws(() => testedMainSha(MERGE, MERGE), /first parent/);
});

test('exact push/main artifact is used even when unrelated CI failed', async () => {
  const github = api(() => [run()], () => [artifact()]);
  assert.deepEqual(await findMainArtifact({ github, repo, sha: MAIN }), binding);
  assert.equal(github.calls[0].args.head_sha, MAIN);
  assert.equal(github.calls[0].args.workflow_id, 'ci.yml');
});

test('pending main snapshot waits and accepts its artifact before unrelated jobs finish', async () => {
  let time = 0, attempts = 0;
  const github = api(() => [run({ status: 'in_progress' })], () => ++attempts === 2 ? [artifact()] : []);
  const result = await findMainArtifact({ github, repo, sha: MAIN, now: () => time,
    sleep: async ms => { time += ms; }, pollMs: 5, timeoutMs: 10 });
  assert.deepEqual(result, binding);
  assert.equal(time, 5);
});

test('no different SHA, fork, PR event or branch artifact can substitute for main', async () => {
  for (const overrides of [{ head_sha: HEAD }, { event: 'pull_request' },
    { head_branch: 'feature' }, { head_repository: { full_name: 'fork/project' } }]) {
    const github = api(() => [run(overrides)], () => [artifact()]);
    await assert.rejects(findMainArtifact({ github, repo, sha: MAIN, timeoutMs: 0 }), /baseline pending/);
    assert.equal(github.calls.filter(c => c.method === 'artifacts').length, 0);
  }
});

test('completed missing, expired, pending timeout and API failure stay distinct', async () => {
  await assert.rejects(findMainArtifact({ github: api(() => [run()], () => []), repo, sha: MAIN }), /unavailable.*no MSL snapshot/);
  await assert.rejects(findMainArtifact({ github: api(() => [run()], () => [artifact({ expired: true })]), repo, sha: MAIN }), /expired; rebase/);
  let time = 0;
  await assert.rejects(findMainArtifact({ github: api(() => [run({ status: 'queued' })], () => []), repo,
    sha: MAIN, now: () => time, sleep: async ms => { time += ms; }, pollMs: 5, timeoutMs: 10 }), /pending.*bounded wait/);
  assert.equal(time, 10);
  await assert.rejects(findMainArtifact({ github: api(() => { throw Error('403'); }, () => []), repo, sha: MAIN }), /403/);
});

test('rerun artifact lookup does not confuse an expired attempt with the available exact run', async () => {
  const github = api(() => [run({ id: 8 }), run()], id => id === 8 ? [artifact({ expired: true })] : [artifact()]);
  assert.deepEqual(await findMainArtifact({ github, repo, sha: MAIN }), binding);
  await assert.rejects(findMainArtifact({ github: api(() => [run()], () => [artifact({ workflow_run: { head_sha: HEAD } })]),
    repo, sha: MAIN }), /artifact workflow source differs/);
});

test('main publication records every full measured snapshot, with no improvement requirement', t => {
  const dir = directory(t, snapshot());
  const manifest = recordMainSnapshot(dir, MAIN, 7);
  assert.equal(manifest.sha, MAIN);
  assert.equal(manifest.runId, 7);
  assert.equal(Object.keys(manifest.payloads).length, 2);
  assert.match(readMainBaseline(dir, binding).provenance, /payload-hash manifest/);
  recordMainSnapshot(dir, MAIN, 7); // Equal counts still publish, unlike a ratchet.
});

test('pre-cutover main artifact is admitted only with both exact embedded commits', t => {
  const dir = directory(t, snapshot());
  assert.match(readMainBaseline(dir, binding).provenance, /Actions push\/main/);
  const data = snapshot();
  data.results.git_commit = HEAD;
  assert.throws(() => readMainBaseline(directory(t, data), binding), /model results are not/);
  data.results.git_commit = MAIN;
  data.quality.git_commit = HEAD;
  assert.throws(() => readMainBaseline(directory(t, data), binding), /quality snapshot is not/);
  assert.throws(() => readMainBaseline(path.join(dir, 'missing'), binding), /ENOENT/);
});

test('partial, unmeasured, duplicate and malformed result evidence refuses', t => {
  const bad = [data => { data.quality.run_scope = 'focused'; },
    data => { data.quality.partial = true; }, data => { data.quality.parity_measured = false; },
    data => { data.results.msl_version = 'other'; }, data => { data.results.model_results = []; },
    data => { data.results.model_results.push(row('Modelica.A')); },
    data => { data.results.model_results[0].is_balanced = 'true'; },
    data => { delete data.quality.sim_target_models; }];
  for (const mutate of bad) {
    const data = snapshot(); mutate(data);
    assert.throws(() => readFullSnapshot(directory(t, data), MAIN));
  }
});

test('manifest tampering and wrong run cannot bind current main', t => {
  const dir = directory(t, snapshot());
  recordMainSnapshot(dir, MAIN, 7);
  assert.throws(() => readMainBaseline(dir, { ...binding, runId: 8 }), /manifest run differs/);
  fs.appendFileSync(path.join(dir, 'msl_results.json'), ' ');
  assert.throws(() => readMainBaseline(dir, binding), /payload hash differs/);
});

test('one lost model fails even if a new success holds the aggregate count constant', () => {
  const base = snapshot(MAIN, [row('Modelica.A'), row('Modelica.B', 'sim_solver_fail')]);
  const head = snapshot(MERGE, [row('Modelica.A', 'sim_solver_fail'), row('Modelica.B')]);
  const comparison = compareModels(base, head);
  assert.deepEqual(comparison.counts.Simulated, { main: 1, current: 1 });
  assert.deepEqual(comparison.losses, [{ model: 'Modelica.A', stage: 'Simulated', status: 'sim_solver_fail' }]);
});

test('missing row and earlier-stage failures retain every lost stage', () => {
  const base = snapshot();
  const missing = compareModels(base, snapshot(MERGE, [row('Modelica.B')]));
  assert.equal(missing.losses.length, 7);
  assert.ok(missing.losses.every(loss => loss.model === 'Modelica.A' && loss.status === 'missing-result'));
  const failed = row('Modelica.A');
  Object.assign(failed, { phase_reached: 'Flatten', is_balanced: false, initial_balance_ok: false,
    ir_solve_seconds: null, ic_status: null, sim_status: null });
  assert.equal(compareModels(base, snapshot(MERGE, [failed, row('Modelica.B')])).losses.length, 7);
});

test('producer stage predicates distinguish IR and IC timeouts from completed simulations', () => {
  const base = snapshot(MAIN, [{ model_name: 'M', phase_reached: 'Success', timeout_phase: 'Sim', sim_status: 'sim_timeout' }]);
  const comparison = compareModels(base, snapshot(MERGE, [{ model_name: 'M', phase_reached: 'Success', timeout_phase: 'IC' }]));
  assert.deepEqual(comparison.counts['IR-Solve'], { main: 1, current: 1 });
  assert.deepEqual(comparison.losses.map(l => l.stage), ['IC solved']);
  assert.deepEqual(comparison.counts.Simulated, { main: 0, current: 0 });
});

test('changed corpus/reference/schema context fails explicitly', () => {
  for (const key of ['msl_version', 'omc_version', 'quality_gate_version', 'sim_target_models']) {
    const current = snapshot(MERGE); current.quality[key] = 'changed';
    assert.throws(() => compareModels(snapshot(), current), /incomparable main baseline context/);
  }
});

test('actual comment pipeline prints exact main, flags the lost model and preserves full report', t => {
  const base = directory(t, snapshot());
  recordMainSnapshot(base, MAIN, 7);
  const current = directory(t, snapshot(MERGE, [row('Modelica.A', 'sim_solver_fail'), row('Modelica.B')]));
  fs.writeFileSync(path.join(current, 'msl_pr_comment.md'), marker + '\n### Existing quality details\n');
  const comparison = compareAndAnnotate({ baselineDir: base, resultsDir: current, binding, currentSha: MERGE });
  assert.equal(comparison.losses.length, 1);
  const comment = fs.readFileSync(path.join(current, 'msl_pr_comment.md'), 'utf8');
  assert.ok(comment.startsWith(marker));
  assert.ok(comment.includes('vs main at ' + String.fromCharCode(96) + MAIN));
  assert.match(comment, /🔴 Model regression/);
  assert.match(comment, /Modelica.A/);
  assert.match(comment, /Existing quality details/);
  assert.equal(JSON.parse(fs.readFileSync(path.join(current, 'msl_main_comparison.json'))).losses.length, 1);
});

test('missing baseline writes red unavailable status without stale counts', t => {
  const dir = directory(t, snapshot(MERGE));
  writeBaselineFailure(dir, 'main baseline pending', MAIN);
  const text = fs.readFileSync(path.join(dir, 'msl_pr_comment.md'), 'utf8');
  assert.ok(text.startsWith(marker));
  assert.match(text, /No PR-vs-main deltas/);
  assert.match(text, /main baseline pending/);
  assert.doesNotMatch(text, /quality gate v14|Δ\+|Compiled \|/);
});

test('executed Actions comparison step fails red on loss and passes equal results', t => {
  const workflow = fs.readFileSync(new URL('../workflows/ci.yml', import.meta.url), 'utf8');
  const script = workflow.split('      - name: Check PR model losses against tested main')[1]
    .split('      - name: Publish MSL PR summary comment')[0].split('          script: |\n')[1]
    .split('\n').map(line => line.slice(12)).join('\n');
  for (const loss of [false, true]) {
    const root = directory(t, snapshot());
    const base = path.join(root, 'target/msl/pr-main-baseline');
    const current = path.join(root, 'target/msl/results');
    fs.mkdirSync(base, { recursive: true }); fs.mkdirSync(current, { recursive: true });
    for (const [dir, data] of [[base, snapshot()], [current, snapshot(MERGE,
      [row('Modelica.A', loss ? 'sim_solver_fail' : 'sim_ok'), row('Modelica.B')])]]) {
      fs.writeFileSync(path.join(dir, 'msl_quality_current.json'), JSON.stringify(data.quality));
      fs.writeFileSync(path.join(dir, 'msl_results.json'), JSON.stringify(data.results));
    }
    fs.writeFileSync(path.join(current, 'msl_pr_comment.md'), marker + '\nCurrent report\n');
    const helper = path.join(root, '.github/scripts/msl-main-baseline.mjs');
    fs.mkdirSync(path.dirname(helper), { recursive: true });
    fs.copyFileSync(new URL('./msl-main-baseline.mjs', import.meta.url), helper);
    const runner = path.join(root, 'runner.cjs');
    fs.writeFileSync(runner, 'const core={setFailed(m){console.error(m);process.exitCode=1}};'
      + '(async()=>{const F=Object.getPrototypeOf(async function(){}).constructor;'
      + 'await new F("require","core",' + JSON.stringify(script) + ')(require,core)})()'
      + '.catch(e=>{console.error(e);process.exitCode=1});');
    const result = spawnSync(process.execPath, [runner], { cwd: root, encoding: 'utf8',
      env: { ...process.env, CI_SOURCE_SHA: MERGE, MAIN_BASELINE_SHA: MAIN,
        MAIN_BASELINE_RUN_ID: '7', MAIN_BASELINE_ARTIFACT_ID: '9' } });
    assert.equal(result.status, loss ? 1 : 0, result.stderr);
    const comment = fs.readFileSync(path.join(current, 'msl_pr_comment.md'), 'utf8');
    assert.match(comment, loss ? /🔴 Model regression/ : /No previously successful/);
    if (loss) assert.match(result.stderr, /Model regression vs tested main: 1/);
  }
});

test('CI removes checked-in report fallback, preserves main runs and failure comments', () => {
  const workflow = fs.readFileSync(new URL('../workflows/ci.yml', import.meta.url), 'utf8');
  const summary = workflow.split('- name: Generate MSL PR summary comment')[1].split('- name: Check PR model losses')[0];
  assert.match(summary, /--baseline target\/msl\/pr-main-baseline\/msl_quality_current.json/);
  assert.doesNotMatch(summary, /crates\/rumoca-test-msl|target\/msl\/baselines|if \[\[/);
  assert.match(workflow, /Record full main MSL snapshot\n\s+if:.*always\(\)/);
  assert.match(workflow, /github.head_ref \|\| format\('\{0\}-\{1\}', github.ref_name, github.sha\)/);
  assert.match(workflow, /cancel-in-progress:.*github.event_name != 'push' \|\| github.ref != 'refs\/heads\/main'/);
  assert.match(workflow, /retention-days:.*90 \|\| 7/);
  assert.match(workflow, /const body = report.startsWith\(marker\)/);
});

test('executed summary step never selects v14, even when that file exists and main is missing', t => {
  const workflow = fs.readFileSync(new URL('../workflows/ci.yml', import.meta.url), 'utf8');
  const script = workflow.split('      - name: Generate MSL PR summary comment')[1]
    .split('      - name: Check PR model losses')[0].split('        run: |\n')[1]
    .split('\n').map(line => line.slice(10)).join('\n');
  const root = directory(t, snapshot());
  const current = path.join(root, 'target/msl/results/msl_quality_current.json');
  const main = path.join(root, 'target/msl/pr-main-baseline/msl_quality_current.json');
  const stale = path.join(root, 'crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json');
  const binary = path.join(root, 'result-msl-artifacts/bin/rumoca-msl-tools');
  for (const file of [current, main, stale, binary]) fs.mkdirSync(path.dirname(file), { recursive: true });
  for (const file of [current, main, stale]) fs.writeFileSync(file, '{}');
  fs.writeFileSync(binary, '#!/usr/bin/env node\nrequire("fs").writeFileSync("ARGS.json",JSON.stringify(process.argv.slice(2)))\n', { mode: 0o755 });
  const result = spawnSync('bash', ['-c', script], { cwd: root, encoding: 'utf8' });
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(JSON.parse(fs.readFileSync(path.join(root, 'ARGS.json'))), [
    'pr-comment', '--results-dir', 'target/msl/results', '--baseline',
    'target/msl/pr-main-baseline/msl_quality_current.json',
  ]);
  fs.unlinkSync(main); fs.unlinkSync(path.join(root, 'ARGS.json'));
  const missing = spawnSync('bash', ['-c', script], { cwd: root, encoding: 'utf8' });
  assert.notEqual(missing.status, 0);
  assert.equal(fs.existsSync(path.join(root, 'ARGS.json')), false);
  assert.equal(fs.existsSync(stale), true);
});
