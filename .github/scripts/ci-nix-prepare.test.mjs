import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { mkdtempSync, mkdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import test from 'node:test';

const script = join(dirname(fileURLToPath(import.meta.url)), 'ci-nix-prepare.sh');
const mock = `#!${process.execPath}
const fs = require('node:fs');
const path = require('node:path');
const state = JSON.parse(fs.readFileSync(process.env.PROBE_STATE, 'utf8'));
const name = path.basename(process.argv[1]);
const args = process.argv.slice(2);
state.calls.push([name, ...args]);
let code = 0;
if (name === 'curl') {
  if (args.includes('--version')) process.stdout.write('mock curl\\n');
  else {
    const result = state.probes.shift() ?? [0, '200'];
    code = result[0]; process.stdout.write(result[1]);
  }
} else if (name === 'nix') {
  if (args[0] === '--version') process.stdout.write('mock Nix\\n');
  else if (args[0] === 'build') {
    code = state.buildCode ?? 0;
    process.stdout.write(state.output ?? state.root);
  } else if (args[0] === 'print-dev-env') code = state.envCode ?? 0;
  else code = 99;
}
fs.writeFileSync(process.env.PROBE_STATE, JSON.stringify(state));
process.exit(code);
`;

function run(overrides = {}, args = ['x86_64-darwin']) {
  const root = mkdtempSync(join(tmpdir(), 'nix-prepare-control-'));
  try {
    const bin = join(root, 'commands');
    const prepared = join(root, 'prepared Bash');
    mkdirSync(bin);
    mkdirSync(join(prepared, 'bin'), { recursive: true });
    for (const name of ['curl', 'nix', 'sleep']) {
      writeFileSync(join(bin, name), mock, { mode: 0o755 });
    }
    writeFileSync(join(prepared, 'bin', 'bash'), mock, { mode: 0o755 });
    const state = join(root, 'state.json');
    writeFileSync(state, JSON.stringify({ root: prepared, calls: [], probes: [], ...overrides }));
    const result = spawnSync('/bin/bash', [script, ...args], {
      env: { PATH: bin, PROBE_STATE: state }, encoding: 'utf8', timeout: 5000,
    });
    assert.equal(result.error, undefined);
    return { ...result, state: JSON.parse(readFileSync(state, 'utf8')) };
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
}

const calls = (r, command) => r.state.calls.filter(c => c[0] === command &&
  !(command === 'curl' && c.includes('--version')));
const preparationCalls = r => calls(r, 'nix').filter(c => c[1] !== '--version');

test('prepares locked native Bash then the exact wheel environment once', () => {
  const r = run();
  assert.equal(r.status, 0, r.stderr);
  const nix = calls(r, 'nix');
  assert.deepEqual(nix.map(c => c[1]), ['--version', 'build', 'print-dev-env']);
  assert.deepEqual(nix[1].slice(2), ['--option', 'substituters', 'https://cache.nixos.org',
    '--no-update-lock-file', '--no-link', '--print-out-paths', '--inputs-from', '.', 'nixpkgs#bashInteractive']);
  assert.deepEqual(nix[2].slice(2), ['--option', 'substituters', 'https://cache.nixos.org',
    '--no-update-lock-file', '.#devShells.x86_64-darwin.ci-python-wheel']);
  assert.equal(calls(r, 'bash').length, 1);
  assert.equal(calls(r, 'curl').length, 1);
  assert.deepEqual(calls(r, 'curl')[0].slice(1), ['--disable', '--silent', '--show-error',
    '--fail', '--connect-timeout', '5', '--max-time', '10', '--output', '/dev/null',
    '--write-out', '%{http_code}', 'https://cache.nixos.org/nix-cache-info']);
});

for (const [code, http] of [[6, '000'], [7, '000'], [28, '000'], [22, '429'],
  [22, '500'], [22, '502'], [22, '503'], [22, '504']]) {
  test(`recovers only transport probe ${code}/${http} before any preparation`, () => {
    const r = run({ probes: [[code, http], [0, '200']] });
    assert.equal(r.status, 0, r.stderr);
    assert.equal(calls(r, 'curl').length, 2);
    assert.deepEqual(calls(r, 'sleep'), [['sleep', '5']]);
    assert.ok(r.state.calls.findIndex(c => c[0] === 'sleep') <
      r.state.calls.findIndex(c => c[0] === 'nix' && c[1] === 'build'));
  });
}

test('transport exhaustion is bounded and does not reject a valid source realization', () => {
  const r = run({ probes: [[6, '000'], [6, '000'], [6, '000']] });
  assert.equal(r.status, 0, r.stderr);
  assert.equal(calls(r, 'curl').length, 3);
  assert.deepEqual(calls(r, 'sleep'), [['sleep', '5'], ['sleep', '10']]);
  assert.deepEqual(preparationCalls(r).map(c => c[1]), ['build', 'print-dev-env']);
  assert.match(r.stderr, /actual pinned realization decides success/);
});

for (const [code, http] of [[35, '000'], [60, '000'], [22, '403'], [22, '404'], [22, '418']]) {
  test(`does not retry probe ${code}/${http} or reject a valid cached realization`, () => {
    const r = run({ probes: [[code, http]] });
    assert.equal(r.status, 0, r.stderr);
    assert.equal(calls(r, 'curl').length, 1);
    assert.equal(preparationCalls(r).length, 2);
    assert.match(r.stderr, /::warning::/);
  });
}

test('Bash preparation failure is preserved, not ignored or retried', () => {
  const r = run({ buildCode: 17 });
  assert.equal(r.status, 17);
  assert.deepEqual(calls(r, 'nix').map(c => c[1]), ['--version', 'build']);
  assert.match(r.stderr, /refusing nix develop fallback/);
});

test('dev-shell derivation failure is preserved and never retries build/test/wheel', () => {
  const r = run({ envCode: 23 });
  assert.equal(r.status, 23);
  assert.deepEqual(calls(r, 'nix').map(c => c[1]), ['--version', 'build', 'print-dev-env']);
  assert.match(r.stderr, /no compiler\/test\/wheel command started/);
});

for (const output of ['', 'relative', '/missing', '/one\n/two']) {
  test(`rejects malformed or unavailable Bash output ${JSON.stringify(output)}`, () => {
    const r = run({ output });
    assert.equal(r.status, 1);
    assert.ok(!calls(r, 'nix').some(c => c[1] === 'print-dev-env'));
  });
}

test('rejects unsupported system and extra arguments before any command', () => {
  for (const args of [[], ['unsupported'], ['x86_64-linux', 'extra']]) {
    const r = run({}, args);
    assert.equal(r.status, 2);
    assert.deepEqual(r.state.calls, []);
  }
});

test('reports an HTTP redirect without rejecting valid source realization', () => {
  const r = run({ probes: [[0, '302']] });
  assert.equal(r.status, 0, r.stderr);
  assert.match(r.stderr, /curl=22 HTTP=302/);
});

test('all four wheel systems retain the exact dev-shell selector', () => {
  for (const system of ['x86_64-linux', 'aarch64-linux', 'x86_64-darwin', 'aarch64-darwin']) {
    const r = run({}, [system]);
    assert.equal(r.status, 0, r.stderr);
    assert.equal(calls(r, 'nix').at(-1).at(-1), `.#devShells.${system}.ci-python-wheel`);
  }
});

test('tool diagnostics precede even a failed transport probe', () => {
  const r = run({ probes: [[60, '000']] });
  assert.equal(r.status, 0, r.stderr);
  assert.match(r.stdout, /Runner Bash=\d/);
  assert.match(r.stdout, /mock Nix/);
  assert.match(r.stdout, /mock curl/);
  assert.deepEqual(r.state.calls.slice(0, 2), [['nix', '--version'], ['curl', '--disable', '--version']]);
  assert.equal(preparationCalls(r).length, 2);
});

test('cache outage cannot mask or retry the real preparation failure', () => {
  const r = run({ probes: [[6, '000'], [6, '000'], [6, '000']], buildCode: 29 });
  assert.equal(r.status, 29);
  assert.equal(calls(r, 'curl').length, 3);
  assert.deepEqual(preparationCalls(r).map(c => c[1]), ['build']);
});

test('an interrupted transport probe stops without starting realization', () => {
  const r = run({ probes: [[130, '000']] });
  assert.equal(r.status, 130);
  assert.deepEqual(preparationCalls(r), []);
});
