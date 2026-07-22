import assert from 'node:assert/strict';
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawnSync } from 'node:child_process';
import { afterEach, test } from 'node:test';

import { validateRecoveryContract } from './recovery-exact-head-contract.mjs';

const script = fileURLToPath(new URL('./recovery-exact-head-contract.mjs', import.meta.url));
const headSha = '1'.repeat(40);
const mergeSha = '2'.repeat(40);
const repo = 'climamind/rumoca';
const roots = [];

afterEach(() => {
  for (const root of roots.splice(0)) rmSync(root, { recursive: true, force: true });
});

const recoveryBody = ({
  batch = 'climamind-rumoca-broken-main-2026-07',
  head = headSha,
  expiresAt = '2099-07-29T23:59:59Z',
} = {}) => [
  `recovery_batch_id: ${batch}`,
  `recovery_head_sha: ${head}`,
  `recovery_expires_at: ${expiresAt}`,
].join('\n');

const pullRequestEvent = (overrides = {}) => ({
  action: 'labeled',
  label: { name: 'recovery-exact-head-ci' },
  pull_request: {
    author_association: 'OWNER',
    base: { repo: { full_name: repo } },
    body: recoveryBody(),
    draft: true,
    head: {
      ref: 'integration/recovery-clean-v4-20260722',
      repo: { full_name: repo },
      sha: headSha,
    },
    labels: [{ name: 'recovery-exact-head-ci' }],
    ...overrides,
  },
  repository: { full_name: repo },
});

const validateRecovery = (event = pullRequestEvent(), selectedSha = headSha) =>
  validateRecoveryContract({
    event,
    eventName: 'pull_request',
    githubSha: mergeSha,
    repository: repo,
    selectedSha,
    now: new Date('2026-07-22T12:00:00Z'),
  });

test('accepts the exact authorized same-repository recovery head', () => {
  const event = pullRequestEvent({ author_association: 'NONE' });
  assert.deepEqual(validateRecovery(event), { mode: 'recovery', sha: headSha });
  const fractional = pullRequestEvent({
    body: recoveryBody({ expiresAt: '2099-07-29T23:59:59.123Z' }),
  });
  assert.deepEqual(validateRecovery(fractional), { mode: 'recovery', sha: headSha });
});

test('accepts normal CI only on GitHub synthetic merge SHA', () => {
  const event = pullRequestEvent({
    body: 'ordinary pull request',
    draft: false,
    head: {
      ref: 'fix/ordinary-change',
      repo: { full_name: repo },
      sha: headSha,
    },
    labels: [],
  });
  assert.deepEqual(
    validateRecoveryContract({
      event,
      eventName: 'pull_request',
      githubSha: mergeSha,
      repository: repo,
      selectedSha: mergeSha,
      now: new Date('2026-07-22T12:00:00Z'),
    }),
    { mode: 'normal', sha: mergeSha },
  );
  assert.throws(
    () => validateRecoveryContract({
      event: {}, eventName: 'push', githubSha: mergeSha, repository: repo,
      selectedSha: headSha, now: new Date('2026-07-22T12:00:00Z'),
    }),
    /normal CI must select GITHUB_SHA/,
  );
});

for (const [name, mutate, message] of [
  ['fork head', (pr) => { pr.head.repo.full_name = 'outsider/rumoca'; }, /same repository/],
  ['foreign base', (pr) => { pr.base.repo.full_name = 'other/rumoca'; }, /same repository/],
  ['non-Draft PR', (pr) => { pr.draft = false; }, /Draft/],
  ['absent label', (pr) => { pr.labels = []; }, /label/],
  ['wrong branch prefix', (pr) => { pr.head.ref = 'feature/recovery'; }, /branch prefix/],
  ['absent batch id', (pr) => { pr.body = 'recovery_head_sha: 111\n'; }, /recovery_batch_id/],
  ['wrong batch id', (pr) => { pr.body = recoveryBody({ batch: 'other' }); }, /batch/],
  ['absent recorded head', (pr) => { pr.body = 'recovery_batch_id: climamind-rumoca-broken-main-2026-07\n'; }, /recovery_head_sha/],
  ['wrong recorded head', (pr) => { pr.body = recoveryBody({ head: '3'.repeat(40) }); }, /recorded head/],
  ['absent expiry', (pr) => { pr.body = recoveryBody().split('\n').slice(0, 2).join('\n'); }, /recovery_expires_at/],
  ['invalid expiry', (pr) => { pr.body = recoveryBody({ expiresAt: 'next-week' }); }, /RFC 3339 UTC/],
  ['invalid calendar date', (pr) => { pr.body = recoveryBody({ expiresAt: '2027-02-29T00:00:00Z' }); }, /RFC 3339 UTC/],
  ['expired authorization', (pr) => { pr.body = recoveryBody({ expiresAt: '2026-07-21T23:59:59Z' }); }, /expired/],
  ['duplicate body field', (pr) => { pr.body += `\nrecovery_head_sha: ${headSha}`; }, /duplicate recovery_head_sha/],
]) {
  test(`rejects recovery with ${name}`, () => {
    const event = structuredClone(pullRequestEvent());
    mutate(event.pull_request);
    assert.throws(() => validateRecovery(event), message);
  });
}

for (const action of ['opened', 'edited', 'synchronize']) {
  test('rejects a persistent recovery label on ' + action, () => {
    const event = structuredClone(pullRequestEvent());
    event.action = action;
    assert.throws(() => validateRecovery(event), /labeled event/);
  });
}

test('rejects a different label action even when the PR retains the recovery label', () => {
  const event = pullRequestEvent({ author_association: 'NONE' });
  event.label.name = 'other-label';
  assert.throws(() => validateRecovery(event), /must apply recovery-exact-head-ci/);
});

test('rejects recovery when selected SHA differs from payload head', () => {
  assert.throws(() => validateRecovery(pullRequestEvent(), '4'.repeat(40)), /selected SHA/);
});

test('does not reinterpret a malformed recovery request as normal merge-ref CI', () => {
  const event = pullRequestEvent({ labels: [] });
  assert.throws(() => validateRecovery(event, mergeSha), /label/);
});

test('CLI independently reads and validates GITHUB_EVENT_PATH', () => {
  const root = mkdtempSync(join(tmpdir(), 'rumoca-recovery-contract-'));
  roots.push(root);
  const eventPath = join(root, 'event.json');
  writeFileSync(eventPath, JSON.stringify(pullRequestEvent()));
  const result = spawnSync(process.execPath, [script], {
    encoding: 'utf8',
    env: {
      ...process.env,
      GITHUB_EVENT_NAME: 'pull_request',
      GITHUB_EVENT_PATH: eventPath,
      GITHUB_REPOSITORY: repo,
      GITHUB_SHA: mergeSha,
      RUMOCA_CI_HEAD_SHA: headSha,
    },
  });
  assert.equal(result.status, 0, result.stderr);
  assert.match(result.stdout, new RegExp(`recovery.*${headSha}`));
});

test('workflow selects recovery head narrowly and proves checked-out provenance', () => {
  const workflow = readFileSync(new URL('../workflows/ci.yml', import.meta.url), 'utf8');
  for (const type of [
    'opened', 'synchronize', 'reopened', 'ready_for_review',
    'converted_to_draft', 'edited', 'labeled', 'unlabeled',
  ]) assert.match(workflow, new RegExp(`\\b${type}\\b`));
  const selection = workflow.slice(
    workflow.indexOf('  RUMOCA_CI_HEAD_SHA:'),
    workflow.indexOf('\n\n# Prevent duplicate runs'),
  );
  assert.match(selection, /github\.event_name == 'pull_request'/);
  assert.match(selection, /github\.event\.action == 'labeled'/);
  assert.match(selection, /github\.event\.label\.name == 'recovery-exact-head-ci'/);
  assert.match(selection, /pull_request\.draft == true/);
  assert.match(selection, /integration\/recovery-/);
  assert.match(selection, /head\.repo\.full_name == github\.repository/);
  assert.match(selection, /base\.repo\.full_name == github\.repository/);
  assert.match(selection, /recovery-exact-head-ci/);
  assert.doesNotMatch(selection, /author_association/);
  assert.match(selection, /climamind-rumoca-broken-main-2026-07/);
  assert.match(selection, /recovery_head_sha: \{0\}.*head\.sha/);
  assert.match(selection, /recovery_expires_at:/);
  assert.match(selection, /head\.sha[\s\S]*\|\| github\.sha/);
  assert.match(workflow, /recovery-exact-head-contract\.mjs/);
  assert.match(workflow, /node --test \.github\/scripts\/recovery-exact-head-contract\.test\.mjs/);
  assert.match(workflow, /git rev-parse HEAD/);
});
