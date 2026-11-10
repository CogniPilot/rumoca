import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { createHash } from 'node:crypto';

export const ARTIFACT_NAME = 'msl-compatibility-report';
const PAYLOADS = ['msl_quality_current.json', 'msl_results.json'];
const MANIFEST = 'msl_main_snapshot.json';
const SHA = /^[0-9a-f]{40}$/;

export function testedMainSha(sourceSha, parentLine) {
  assert.match(sourceSha, SHA, 'invalid CI source SHA');
  const [commit, first, second, ...extra] = parentLine.trim().split(/\s+/);
  assert.equal(commit, sourceSha, 'checkout does not match CI source');
  assert.match(first ?? '', SHA, 'PR checkout must contain its first parent');
  assert.match(second ?? '', SHA, 'PR checkout must be the test merge, not PR head');
  assert.equal(extra.length, 0, 'expected a two-parent PR test merge');
  return first;
}

// Artifacts are measured history, not the separately promoted quality floor.
// A complete MSL job is usable even if another job in main's CI run failed.
export async function findMainArtifact({ github, repo, sha, timeoutMs = 600000,
  pollMs = 15000, now = Date.now, sleep = ms => new Promise(r => setTimeout(r, ms)),
  log = () => {} }) {
  assert.match(sha, SHA, 'invalid main SHA');
  const deadline = now() + timeoutMs;
  for (;;) {
    const runs = await github.paginate(github.rest.actions.listWorkflowRuns, {
      ...repo, workflow_id: 'ci.yml', head_sha: sha, branch: 'main', event: 'push', per_page: 100,
    });
    const exact = runs.filter(run => run.head_sha === sha && run.head_branch === 'main'
      && run.event === 'push' && run.head_repository?.full_name === repo.owner + '/' + repo.repo);
    const found = await artifactInRuns(github, repo, sha, exact);
    if (found.artifact) return found.artifact;
    const pending = exact.some(run => run.status !== 'completed');
    if (exact.length && !pending) {
      throw new Error('main baseline unavailable for ' + sha + ': '
        + (found.expired ? 'artifact expired; rebase onto main' : 'main CI produced no MSL snapshot; rebase onto main'));
    }
    if (now() >= deadline) throw new Error('main baseline pending for ' + sha + ': bounded wait expired');
    log('Waiting for the MSL snapshot of main at ' + sha);
    await sleep(Math.min(pollMs, deadline - now()));
  }
}

async function artifactInRuns(github, repo, sha, runs) {
  let expired = false;
  for (const run of runs) {
    const artifacts = await github.paginate(github.rest.actions.listWorkflowRunArtifacts, {
      ...repo, run_id: run.id, per_page: 100,
    });
    const matches = artifacts.filter(a => a.name === ARTIFACT_NAME);
    expired ||= matches.some(a => a.expired);
    const artifact = matches.find(a => !a.expired);
    if (!artifact) continue;
    assert.ok(!artifact.workflow_run || artifact.workflow_run.head_sha === sha,
      'artifact workflow source differs from requested main');
    return { artifact: { sha, runId: run.id, artifactId: artifact.id }, expired };
  }
  return { expired };
}

function readJson(dir, name) {
  return JSON.parse(fs.readFileSync(path.join(dir, name), 'utf8'));
}

function digest(dir, name) {
  return createHash('sha256').update(fs.readFileSync(path.join(dir, name))).digest('hex');
}

export function readFullSnapshot(dir, sha) {
  assert.match(sha, SHA, 'invalid snapshot source SHA');
  const quality = readJson(dir, PAYLOADS[0]);
  const results = readJson(dir, PAYLOADS[1]);
  assert.equal(quality.git_commit, sha, 'quality snapshot is not the requested commit');
  assert.equal(results.git_commit, sha, 'model results are not the requested commit');
  assert.equal(quality.run_scope, 'full', 'main baseline must be a full MSL run');
  assert.ok(quality.partial === undefined || quality.partial === false, 'partial MSL snapshot');
  assert.equal(quality.parity_measured, true, 'MSL parity unmeasured');
  assert.equal(results.msl_version, quality.msl_version, 'MSL versions differ within snapshot');
  assert.ok(typeof quality.msl_version === 'string' && quality.msl_version.length > 0, 'missing MSL version');
  assert.ok(typeof quality.omc_version === 'string' && quality.omc_version.length > 0, 'missing OMC version');
  assert.ok(Number.isInteger(quality.quality_gate_version), 'missing quality schema');
  assert.ok(Number.isInteger(quality.sim_target_models) && quality.sim_target_models > 0, 'missing simulation target count');
  modelMap(results);
  return { quality, results };
}

export function recordMainSnapshot(dir, sha, runId) {
  readFullSnapshot(dir, sha);
  assert.ok(Number.isSafeInteger(Number(runId)) && Number(runId) > 0, 'invalid main run ID');
  const manifest = { version: 1, sha, runId: Number(runId),
    payloads: Object.fromEntries(PAYLOADS.map(name => [name, digest(dir, name)])) };
  fs.writeFileSync(path.join(dir, MANIFEST), JSON.stringify(manifest, null, 2) + '\n');
  return manifest;
}

export function readMainBaseline(dir, binding) {
  const snapshot = readFullSnapshot(dir, binding.sha);
  // The already-published artifact of the exact pre-cutover main bootstraps
  // this change. Its Actions run AND both embedded commits bind the source;
  // no checked-in file or another run's SHA is a substitute.
  const manifestPath = path.join(dir, MANIFEST);
  let provenance = 'Actions push/main run and both embedded source commits';
  if (fs.existsSync(manifestPath)) {
    const manifest = readJson(dir, MANIFEST);
    assert.equal(manifest.version, 1, 'unsupported main snapshot manifest');
    assert.equal(manifest.sha, binding.sha, 'manifest main SHA differs');
    assert.equal(manifest.runId, Number(binding.runId), 'manifest run differs');
    assert.deepEqual(Object.keys(manifest.payloads).sort(), [...PAYLOADS].sort(), 'manifest payload set differs');
    for (const name of PAYLOADS) assert.equal(manifest.payloads[name], digest(dir, name), 'snapshot payload hash differs');
    provenance += ' plus payload-hash manifest';
  }
  return { ...snapshot, provenance };
}

function modelMap(results) {
  assert.ok(Array.isArray(results.model_results) && results.model_results.length > 0, 'missing per-model results');
  const map = new Map();
  for (const row of results.model_results) {
    assert.ok(typeof row.model_name === 'string' && row.model_name.length > 0, 'invalid model identity');
    assert.ok(!map.has(row.model_name), 'duplicate model identity: ' + row.model_name);
    assert.ok(typeof row.phase_reached === 'string' && row.phase_reached.length > 0, 'missing model phase');
    for (const key of ['is_partial', 'is_balanced', 'initial_balance_ok']) {
      assert.ok(row[key] == null || typeof row[key] === 'boolean', 'invalid model boolean: ' + key);
    }
    map.set(row.model_name, row);
  }
  return map;
}

// Match balance_pipeline_reporting.rs and process_success_result's balance
// cohort: only successfully compiled, non-partial classes count as balanced.
// Simulation completion is never a claim of trace parity/certification.
function stages(row) {
  const phase = row.phase_reached;
  const balanceEligible = phase === 'Success' && row.is_partial !== true;
  const solve = row.ir_solve_file != null || row.ir_solve_seconds != null
    || ['SimBuild', 'IC', 'Sim'].includes(row.timeout_phase);
  const ic = solve && (row.ic_status === 'ic_ok' || row.timeout_phase === 'Sim');
  return { Flattened: ['ToDae', 'Success'].includes(phase), Compiled: phase === 'Success',
    Balanced: balanceEligible && row.is_balanced === true,
    'Initial balance': balanceEligible && row.initial_balance_ok === true,
    'IR-Solve': solve, 'IC solved': ic, Simulated: ic && row.sim_status === 'sim_ok' };
}

export function compareModels(baseline, current) {
  const previous = modelMap(baseline.results), next = modelMap(current.results);
  for (const key of ['msl_version', 'omc_version', 'quality_gate_version', 'sim_target_models']) {
    assert.deepEqual(current.quality[key], baseline.quality[key], 'incomparable main baseline context: ' + key);
  }
  const losses = [], counts = {};
  for (const stage of Object.keys(stages(previous.values().next().value))) counts[stage] = { main: 0, current: 0 };
  for (const row of next.values()) for (const [stage, ok] of Object.entries(stages(row))) counts[stage].current += Number(ok);
  for (const [model, row] of previous) {
    const after = next.has(model) ? stages(next.get(model)) : {};
    for (const [stage, ok] of Object.entries(stages(row))) {
      counts[stage].main += Number(ok);
      if (ok && !after[stage]) losses.push({ model, stage,
        status: next.get(model)?.sim_status ?? next.get(model)?.phase_reached ?? 'missing-result' });
    }
  }
  return { counts, losses };
}

function code(value) {
  return '`' + String(value).replaceAll('`', '&#96;').replaceAll('|', '\\|')
    .replace(/[\r\n]/g, ' ').replaceAll('@', '@\u200b') + '`';
}

export function renderMainComparison(binding, comparison, provenance) {
  const lines = ['**vs main at ' + code(binding.sha) + '**', '',
    'Main snapshot: Actions run ' + binding.runId + ', artifact ' + binding.artifactId + '.',
    'Source binding: ' + provenance + '.', '',
    comparison.losses.length ? '**🔴 Model regression: ' + comparison.losses.length + ' successful model/stage memberships lost.**'
      : '**No previously successful model/stage membership lost.**', '',
    '| Stage | Main | PR test merge | Δ |', '|---|---:|---:|---:|'];
  for (const [stage, counts] of Object.entries(comparison.counts)) {
    const delta = counts.current - counts.main;
    lines.push('| ' + stage + ' | ' + counts.main + ' | ' + counts.current + ' | ' + (delta > 0 ? '+' : '') + delta + ' |');
  }
  if (comparison.losses.length) {
    lines.push('', '| Lost model | Stage | Current status |', '|---|---|---|');
    for (const loss of comparison.losses.slice(0, 20)) lines.push('| ' + code(loss.model) + ' | ' + loss.stage + ' | ' + code(loss.status) + ' |');
    lines.push('', 'Every loss is recorded in msl_main_comparison.json in the current run artifact. Gains never offset losses.');
  }
  lines.push('', 'Simulated means execution completed, not trace parity. The promoted quality-floor gate is separate.', '');
  return lines.join('\n');
}

export function compareAndAnnotate({ baselineDir, resultsDir, binding, currentSha }) {
  const baseline = readMainBaseline(baselineDir, binding);
  const current = readFullSnapshot(resultsDir, currentSha);
  const comparison = compareModels(baseline, current);
  fs.writeFileSync(path.join(resultsDir, 'msl_main_comparison.json'), JSON.stringify({ binding, ...comparison }, null, 2) + '\n');
  const file = path.join(resultsDir, 'msl_pr_comment.md');
  const marker = '<!-- rumoca-msl-quality-summary -->';
  const body = fs.readFileSync(file, 'utf8');
  assert.ok(body.startsWith(marker), 'missing PR comment marker');
  const note = renderMainComparison(binding, comparison, baseline.provenance);
  fs.writeFileSync(file, marker + '\n' + note + '\n' + body.slice(marker.length).trimStart());
  return comparison;
}

export function writeBaselineFailure(resultsDir, message, sha = 'unresolved') {
  fs.mkdirSync(resultsDir, { recursive: true });
  fs.writeFileSync(path.join(resultsDir, 'msl_pr_comment.md'), [
    '<!-- rumoca-msl-quality-summary -->', '### MSL main comparison unavailable', '',
    '**🔴 No PR-vs-main deltas are reported.**', '', 'Tested main: ' + code(sha) + '.', '',
    code(message), '', 'No checked-in or different-commit baseline was substituted.', '',
  ].join('\n'));
}
