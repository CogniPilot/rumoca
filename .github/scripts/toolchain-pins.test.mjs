import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { spawnSync } from 'node:child_process';
import { test } from 'node:test';

const read = (path) => readFileSync(new URL(`../../${path}`, import.meta.url), 'utf8');

const workflowJob = (workflow, job) => {
  const start = workflow.indexOf(`  ${job}:`);
  assert.notEqual(start, -1, `${job} must exist`);
  const next = workflow.slice(start + 1).search(/^  [a-zA-Z0-9_-]+:$/m);
  return workflow.slice(start, next === -1 ? undefined : start + 1 + next);
};

test('OpenModelica uses one exact repository pin', () => {
  const version = read('toolchains/openmodelica-version').trim();
  assert.match(version, /^\d+\.\d+\.\d+~1-g[0-9a-f]+-\d+$/);
  assert.equal(version, '1.27.0~1-gd7e2907-1');
});

test('local package tooling defaults to the same Node major as CI', () => {
  assert.equal(read('.nvmrc').trim(), '20');
});

test('all MSL jobs use the shared OpenModelica installer', () => {
  const workflow = read('.github/workflows/ci.yml');
  const installerCalls = workflow.match(/scripts\/ci\/install-openmodelica\.sh/g) ?? [];

  assert.equal(installerCalls.length, 2);
  assert.doesNotMatch(workflow, /omc_channel=/);
  assert.doesNotMatch(workflow, /apt-get install[^\n]*\bom[c]?\b/);
});

test('every MSL consumer installs OpenModelica before verifying omc', () => {
  const workflow = read('.github/workflows/ci.yml');
  for (const job of ['msl-shards', 'modelicatest-gate']) {
    const jobBody = workflowJob(workflow, job);
    const install = jobBody.indexOf('scripts/ci/install-openmodelica.sh');

    assert.notEqual(install, -1, `${job} must use the shared installer`);
    for (const verification of jobBody.matchAll(/omc --version/g)) {
      assert.ok(
        install < verification.index,
        `${job} must install OpenModelica before verification`,
      );
    }
  }
});

test('the installer validates the installed version against the pin', () => {
  const installer = read('scripts/ci/install-openmodelica.sh');

  assert.match(installer, /toolchains\/openmodelica-version/);
  assert.match(installer, /omc --version/);
  assert.match(installer, /installed_version/);
  assert.match(installer, /expected_version/);
});

test('the installer accepts the release output implied by the exact package pin', () => {
  assert.equal(read('toolchains/openmodelica-version').trim(), '1.27.0~1-gd7e2907-1');
  const result = spawnSync(
    'scripts/ci/install-openmodelica.sh',
    ['--check-output', 'OpenModelica 1.27.0'],
    { encoding: 'utf8' },
  );

  assert.equal(result.status, 0, result.stderr);
  assert.equal(result.stdout, 'OpenModelica 1.27.0\n');
});

test('the installer rejects pre-release or build drift', () => {
  const result = spawnSync(
    'scripts/ci/install-openmodelica.sh',
    ['--check-output', 'OpenModelica 1.27.0~dev.beta.1-4-ge5d8071'],
    { encoding: 'utf8' },
  );

  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /OpenModelica version mismatch/);
});
