import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';

const read = (path) => readFileSync(new URL(`../../${path}`, import.meta.url), 'utf8');

test('OpenModelica uses one exact repository pin', () => {
  const version = read('toolchains/openmodelica-version').trim();
  assert.match(version, /^\d+\.\d+\.\d+$/);
  assert.equal(version, '1.27.0');
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

test('the installer validates the installed version against the pin', () => {
  const installer = read('scripts/ci/install-openmodelica.sh');

  assert.match(installer, /toolchains\/openmodelica-version/);
  assert.match(installer, /omc --version/);
  assert.match(installer, /installed_version/);
  assert.match(installer, /expected_version/);
});
