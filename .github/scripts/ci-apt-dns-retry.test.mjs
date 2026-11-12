import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { retryAptDnsRun } from './ci-apt-dns-retry.mjs';

function scenario() {
  const run = { id: 50, workflow_id: 3, repository: { id: 7 }, head_repository: { id: 7,
    owner: { login: 'example' } }, name: 'CI', path: '.github/workflows/ci.yml',
    status: 'completed', conclusion: 'failure', run_attempt: 1,
    event: 'push', head_branch: 'main', head_sha: 'a'.repeat(40) };
  const jobs = [{ id: 80, run_id: 50, run_attempt: 1, head_sha: run.head_sha,
    status: 'completed', conclusion: 'failure',
    check_run_url: 'https://api.github.com/repos/example/project/check-runs/90',
    steps: [{ name: 'Install system dependencies', conclusion: 'failure' }] }];
  const state = { run, jobs, writes: [], waits: [], annotations: [
    { title: 'APT mirror DNS failure', annotation_level: 'failure' }],
  pulls: [{ draft: false, head: { sha: run.head_sha } }] };
  const github = {
    rest: {
      actions: { getWorkflowRun: async () => ({ data: structuredClone(state.run) }),
        listJobsForWorkflowRunAttempt() {} },
      checks: { listAnnotations: async ({ check_run_id }) => ({ data:
        state.annotationsByCheck?.[check_run_id] ?? state.annotations }) },
      pulls: { list: async () => ({ data: state.pulls }) },
    },
    paginate: async (_method, args) => {
      assert.equal(args.attempt_number, 1);
      return state.jobs;
    },
    request: async (route, args) => { state.writes.push({ route, args }); },
  };
  const context = { repo: { owner: 'example', repo: 'project' },
    payload: { repository: { id: 7, default_branch: 'main' }, workflow_run: { id: 50 } } };
  state.execute = () => retryAptDnsRun({ github, context,
    core: { info() {}, notice() {} }, wait: async ms => {
      state.waits.push(ms);
      state.afterWait?.();
    } });
  return state;
}

test('all marked DNS setup failures get one delayed failed-job request', async () => {
  const s = scenario();
  s.jobs.push({ ...s.jobs[0], id: 81,
    check_run_url: 'https://api.github.com/repos/example/project/check-runs/91' });
  s.jobs[0].steps.push({ name: 'Upload GNU static-PIE qualification (Linux)', conclusion: 'failure' });
  assert.equal(await s.execute(), true);
  assert.deepEqual(s.waits, [120_000]);
  assert.deepEqual(s.writes, [{
    route: 'POST /repos/{owner}/{repo}/actions/runs/{run_id}/rerun-failed-jobs',
    args: { owner: 'example', repo: 'project', run_id: 50 },
  }]);
});

for (const [name, change] of [
  ['a source failure beside DNS setup', s => s.jobs.push({ ...s.jobs[0], id: 82,
    steps: [{ name: 'Run workspace tests', conclusion: 'failure' }] })],
  ['an unmarked package failure', s => { s.annotations = []; }],
  ['a non-DNS error annotation', s => { s.annotations[0].title = 'Package not found'; }],
  ['a warning instead of a failure marker', s => { s.annotations[0].annotation_level = 'warning'; }],
  ['a later source failure in the setup job', s => s.jobs[0].steps.push({
    name: 'Run workspace tests', conclusion: 'failure' })],
  ['a cancelled job', s => { s.jobs[0].conclusion = 'cancelled'; }],
  ['a live job', s => { s.jobs[0].status = 'in_progress'; }],
  ['foreign job metadata', s => { s.jobs[0].run_id = 51; }],
  ['a job from another attempt', s => { s.jobs[0].run_attempt = 2; }],
  ['a job from another source', s => { s.jobs[0].head_sha = 'b'.repeat(40); }],
  ['an unknown failure step', s => { s.jobs[0].steps[0].name = 'Unknown setup'; }],
  ['a missing check identity', s => { delete s.jobs[0].check_run_url; }],
  ['a second attempt', s => { s.run.run_attempt = 2; }],
  ['a manual dispatch', s => { s.run.event = 'workflow_dispatch'; }],
  ['a release tag', s => { s.run.head_branch = 'v1.0.0'; }],
  ['another workflow', s => { s.run.path = '.github/workflows/release.yml'; }],
  ['another repository', s => { s.run.repository.id = 8; }],
  ['another run identity', s => { s.run.id = 51; }],
  ['no failed jobs', s => { s.jobs[0].conclusion = 'success'; }],
]) {
  test(`does not retry ${name}`, async () => {
    const s = scenario();
    change(s);
    assert.equal(await s.execute(), false);
    assert.deepEqual(s.writes, []);
    assert.deepEqual(s.waits, []);
  });
}

test('every failed setup job must independently carry the DNS marker', async () => {
  const s = scenario();
  s.jobs.push({ ...s.jobs[0], id: 81,
    check_run_url: 'https://api.github.com/repos/example/project/check-runs/91' });
  s.annotationsByCheck = { 90: s.annotations, 91: [] };
  assert.equal(await s.execute(), false);
  assert.deepEqual(s.waits, []);
  assert.deepEqual(s.writes, []);
});

test('an open current fork PR can recover without executing fork code', async () => {
  const s = scenario();
  s.run.event = 'pull_request';
  s.run.head_repository = { id: 8, owner: { login: 'contributor' } };
  assert.equal(await s.execute(), true);
  assert.equal(s.writes.length, 1);
});

for (const [name, change] of [
  ['a newer PR head', s => { s.pulls[0].head.sha = 'b'.repeat(40); }],
  ['a draft PR', s => { s.pulls[0].draft = true; }],
  ['a closed PR', s => { s.pulls = []; }],
]) {
  test(`does not retry ${name}`, async () => {
    const s = scenario();
    s.run.event = 'pull_request';
    change(s);
    assert.equal(await s.execute(), false);
    assert.deepEqual(s.writes, []);
  });
}

test('a manual rerun during backoff prevents another request', async () => {
  const s = scenario();
  s.afterWait = () => { s.run.run_attempt = 2; s.run.status = 'in_progress'; };
  assert.equal(await s.execute(), false);
  assert.deepEqual(s.writes, []);
});

test('a PR update during backoff prevents rerunning the obsolete source', async () => {
  const s = scenario();
  s.run.event = 'pull_request';
  s.afterWait = () => { s.pulls = []; };
  assert.equal(await s.execute(), false);
  assert.deepEqual(s.writes, []);
});

test('a changed workflow identity during backoff prevents a request', async () => {
  const s = scenario();
  s.afterWait = () => { s.run.workflow_id = 4; };
  assert.equal(await s.execute(), false);
  assert.deepEqual(s.writes, []);
});

test('every local apt caller in the effective CI workflow can recover DNS', async () => {
  const workflow = readFileSync(new URL('../workflows/ci.yml', import.meta.url), 'utf8');
  const uses = /^(?:uses:| {8}uses:)\s*\.\/\.github\/actions\/apt-install\s*$/m;
  const names = workflow.split(/^ {6}- /m).slice(1)
    .filter(block => uses.test(block))
    .map(block => /^(?:name:| {8}name:)\s*(.+)$/m.exec(block)?.[1]
      ?? 'Run ./.github/actions/apt-install');
  const count = (workflow.match(/^\s*(?:- )?uses:\s*\.\/\.github\/actions\/apt-install\s*$/gm) ?? []).length;
  assert.ok(count > 0);
  assert.equal(names.length, count, 'the census must account for every workflow apt call');
  for (const name of new Set(names)) {
    const s = scenario();
    s.jobs[0].steps[0].name = name;
    assert.equal(await s.execute(), true, `marked DNS setup must recover: ${name}`);
    assert.equal(s.writes.length, 1);
  }
});
