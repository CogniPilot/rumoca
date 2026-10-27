import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync, mkdtempSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { spawnSync } from 'node:child_process';

const workflow = readFileSync(new URL('../workflows/ci.yml', import.meta.url), 'utf8');
const source = 'd01ad41f0a01e8fe529f4f78358c5c2947d52f0d';
const tree = '44e7f5a097b4215b03c6213b1014db8d3dc53028';
function runDetection({ event = 'workflow_dispatch', ref = 'refs/heads/main', input = 'v0.10.3', tagSha = source, actualTree = tree } = {}) {
  const dir = mkdtempSync(join(tmpdir(), 'release-recovery-'));
  try {
    writeFileSync(join(dir, 'git'), `#!/bin/sh
case "$*" in
  "rev-parse refs/tags/v0.10.3^{commit}") echo "$TAG_SHA" ;;
  "rev-parse HEAD^{tree}") echo "$ACTUAL_TREE" ;;
  "tag --points-at "*) echo v0.10.3 ;;
  *) exit 19 ;;
esac
`, { mode: 0o755 });
    const block = workflow.split('      - name: Detect v* tag for current commit\n')[1].split('      - name: Retain release')[0];
    let script = block.split('        run: |\n')[1].replace(/^          /gm, '');
    script = script.replaceAll("${{ github.event_name }}", event).replaceAll("${{ github.ref }}", ref).replaceAll("${{ inputs.recovery_release }}", input);
    const result = spawnSync('bash', ['-e', '-c', script], {
      encoding: 'utf8', env: { ...process.env, PATH: `${dir}:${process.env.PATH}`, CI_SOURCE_SHA: source,
        CI_WORKFLOW_SHA: 'a'.repeat(40), TAG_SHA: tagSha, ACTUAL_TREE: actualTree,
        GITHUB_OUTPUT: join(dir, 'output'), RUNNER_TEMP: dir },
    });
    return { ...result, provenance: result.status === 0 ? readFileSync(join(dir, 'release-provenance.txt'), 'utf8') : null };
  } finally { rmSync(dir, { recursive: true, force: true }); }
}
test('recovery records distinct fixed-workflow and immutable source identities', () => {
  const result = runDetection(); assert.equal(result.status, 0, result.stderr);
  assert.match(result.provenance, new RegExp(`source=${source}`));
  assert.match(result.provenance, new RegExp(`workflow=${'a'.repeat(40)}`));
  assert.match(result.provenance, /tag=v0\.10\.3/);
});
for (const [name, options] of [
  ['moved tag', { tagSha: 'b'.repeat(40) }],
  ['changed source tree', { actualTree: 'b'.repeat(40) }],
  ['different recovery release', { input: 'v0.10.4' }],
  ['dispatch from another branch', { ref: 'refs/heads/other' }],
]) test(`${name} refuses recovery`, () => assert.notEqual(runDetection(options).status, 0));
test('ordinary tag detection still records its content identity', () => {
  assert.equal(runDetection({ event: 'push', ref: 'refs/tags/v0.10.3', input: '' }).status, 0);
});
test('all own-source checkouts bind and verify the content SHA; external corpus stays pinned', () => {
  const steps = workflow.split(/(?=^      - )/m).filter(x => x.includes('uses: actions/checkout@v5'));
  let own = 0, helpers = 0, external = 0;
  for (const step of steps) {
    if (step.includes('repository: CogniPilot/modelica_models')) { assert.match(step, /ref: a41f7c0/); external++; }
    else if (step.includes('path: .release-workflow')) { assert.match(step, /ref: \$\{\{ env.CI_WORKFLOW_SHA \}\}/); helpers++; }
    else { assert.match(step, /ref: \$\{\{ env.CI_SOURCE_SHA \}\}/); own++; }
  }
  assert.equal(own, 24); assert.equal(helpers, 2); assert.equal(external, 1);
  assert.equal((workflow.match(/name: Verify source checkout identity/g) || []).length, own);
  assert.equal((workflow.match(/test "\$\(git rev-parse HEAD\)" = "\$CI_SOURCE_SHA"/g) || []).length, own);
});
test('publish keeps all required gates and uses content SHA for tag-on-main verification', () => {
  const deploy = workflow.split('\n  deploy:\n')[1];
  for (const gate of ['build-rust-binaries', 'build-python-wheels', 'build-python-sdist', 'coverage-gate', 'msl-merge', 'template-runtime-tests', 'musl-setup-smoke']) assert.ok(deploy.includes(gate));
  assert.match(deploy, /git tag --points-at "\$\{\{ env.CI_SOURCE_SHA \}\}"/);
  assert.match(deploy, /git merge-base --is-ancestor "\$\{\{ env.CI_SOURCE_SHA \}\}" origin\/main/);
  assert.ok(!deploy.includes('${{ github.sha }}'));
});

test('the executed checkout guard refuses a main checkout under the tagged content identity', () => {
  const dir = mkdtempSync(join(tmpdir(), 'release-head-'));
  try {
    writeFileSync(join(dir, 'git'), '#!/bin/sh\nprintf \'%s\\n\' "$ACTUAL_HEAD"\n', { mode: 0o755 });
    const guard = workflow.split('      - name: Verify source checkout identity\n')[1].split('      - ')[0].split('        run: |\n')[1].replace(/^          /gm, '');
    for (const [actual, expected] of [[source, 0], ['a'.repeat(40), 1]]) {
      const result = spawnSync('bash', ['-e', '-c', guard], { env: { ...process.env, PATH: `${dir}:${process.env.PATH}`, ACTUAL_HEAD: actual, CI_SOURCE_SHA: source, CI_WORKFLOW_SHA: 'b'.repeat(40) } });
      assert.equal(result.status, expected);
    }
  } finally { rmSync(dir, { recursive: true, force: true }); }
});
