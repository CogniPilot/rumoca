import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import test from 'node:test';

const workflow = fs.readFileSync(new URL('../workflows/ci.yml', import.meta.url), 'utf8');
const archives = ['target/nix-cache/openmodelica-cli-closure.nar.zst', 'target/nix-cache/msl-artifacts-closure.nar.zst'];
const sdkPaths = ['/usr/share/dotnet', '/usr/local/lib/android', '/opt/ghc', '/usr/local/.ghcup', '/opt/hostedtoolcache/CodeQL'];

function job(name) {
  const marker = `  ${name}:\n`;
  const start = workflow.indexOf(marker);
  assert.ok(start >= 0, `missing job ${name}`);
  const tail = workflow.slice(start + marker.length);
  const next = tail.search(/^  [a-z0-9-]+:\s*$/m);
  return marker + (next < 0 ? tail : tail.slice(0, next));
}

function script(owner, name) {
  const marker = `      - name: ${name}\n`;
  const start = owner.indexOf(marker);
  assert.ok(start >= 0, `missing step ${name}`);
  const tail = owner.slice(start + marker.length);
  const next = tail.indexOf('\n      - ');
  const step = next < 0 ? tail : tail.slice(0, next);
  const body = step.indexOf('        run: |\n');
  assert.ok(body >= 0, `missing literal run body ${name}`);
  return step.slice(body + '        run: |\n'.length).split('\n').map(line => {
    if (!line.trim()) return '';
    assert.ok(line.startsWith('          '), 'nonliteral run body');
    return line.slice(10);
  }).join('\n');
}

const mock = `#!${process.execPath}
const fs = require('node:fs');
const path = require('node:path');
const name = path.basename(process.argv[1]);
const args = process.argv.slice(2);
const trace = process.env.FIXTURE_TRACE;
const prior = fs.existsSync(trace) ? fs.readFileSync(trace, 'utf8').trim().split('\\n').filter(Boolean).map(JSON.parse) : [];
fs.appendFileSync(trace, JSON.stringify({name,args}) + '\\n');
const mode = process.env.FIXTURE_MODE;
if (name === 'nix-store') {
  process.stdin.resume();
  process.stdin.on('end', () => {
    const count = prior.filter(x => x.name === name).length;
    process.exitCode = mode === 'first-import' || (mode === 'second-import' && count === 1) ? 37 : 0;
  });
} else if (name === 'zstd') {
  if (mode === 'decompress') process.exitCode = 29;
  else process.stdout.write('synthetic-not-a-NAR');
} else if (name === 'nix' && mode === 'build') process.exitCode = 43;
else if (name === 'sudo' && mode === 'cleanup') process.exitCode = 47;
else if ((name === 'df' || name === 'du') && mode === 'diagnostics') process.exitCode = 9;
`;

function run(source, mode = 'ok') {
  // Only owned fixtures and mocked external commands. There is no PATH fallback
  // to real sudo/rm/zstd/Nix/OMC; no cleanup/import/build is performed.
  const root = fs.mkdtempSync(path.join(os.tmpdir(), 'modelicatest-disk-control-'));
  try {
    const bin = path.join(root, 'bin');
    fs.mkdirSync(bin);
    for (const name of ['sudo', 'docker', 'df', 'du', 'zstd', 'nix-store', 'rm', 'nix']) {
      fs.writeFileSync(path.join(bin, name), mock, { mode: 0o700 });
    }
    fs.mkdirSync(path.join(root, 'result-openmodelica/bin'), { recursive: true });
    fs.writeFileSync(path.join(root, 'result-openmodelica/bin/omc'), mock, { mode: 0o700 });
    const trace = path.join(root, 'trace');
    const result = spawnSync('/bin/bash', ['-euo', 'pipefail', '-c', source], {
      cwd: root, timeout: 10000, encoding: 'utf8',
      env: { PATH: bin, HOME: root, GITHUB_PATH: path.join(root, 'github-path'), FIXTURE_TRACE: trace, FIXTURE_MODE: mode },
    });
    assert.ifError(result.error);
    const rows = fs.existsSync(trace) ? fs.readFileSync(trace, 'utf8').trim().split('\n').filter(Boolean).map(JSON.parse) : [];
    return { status: result.status, rows, stderr: result.stderr };
  } finally {
    fs.rmSync(root, { recursive: true, force: true });
  }
}

function released(rows) {
  const deletions = rows.filter(row => row.name === 'rm');
  for (const row of deletions) {
    assert.deepEqual(row.args.slice(0, 1), ['--']);
    assert.equal(row.args.length, 2);
    assert.ok(archives.includes(row.args[1]), `unsafe deletion ${row.args[1]}`);
  }
  return deletions.map(row => row.args[1]);
}

const modelica = job('modelicatest-gate');
const imported = script(modelica, 'Import shared Nix closures');
const reclaim = () => script(modelica, 'Reclaim runner disk before ModelicaTest inputs');

test('fixed SDK reclaim precedes every cache and Nix input, with the existing shard body', () => {
  const at = modelica.indexOf('Reclaim runner disk before ModelicaTest inputs');
  for (const label of ['Install system dependencies', 'Rust cache (msl-gate)', 'Cache MSL', 'Download shared Nix closures']) {
    assert.ok(at < modelica.indexOf(`- name: ${label}`));
  }
  assert.equal(reclaim().trim(), script(job('msl-shards'), 'Reclaim runner disk').trim());
  const result = run(reclaim());
  assert.equal(result.status, 0, result.stderr);
  const calls = result.rows.filter(row => row.name === 'sudo');
  assert.deepEqual(calls.map(row => row.args), [['rm', '-rf', ...sdkPaths], ['apt-get', 'clean']]);
});

test('each successful import releases only its archive before the next import or build', () => {
  const result = run(imported);
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(released(result.rows), archives);
  const order = result.rows.filter(row => ['zstd', 'nix-store', 'rm', 'nix', 'omc'].includes(row.name)).map(row => row.name);
  assert.ok(order.indexOf('rm') > order.indexOf('nix-store'));
  assert.deepEqual(order.filter(name => name !== 'zstd'), ['nix-store', 'rm', 'nix-store', 'rm', 'nix', 'nix', 'omc']);
});

test('first failed import retains both archives and propagates its status', () => {
  const result = run(imported, 'first-import');
  assert.equal(result.status, 37, result.stderr);
  assert.deepEqual(released(result.rows), []);
  assert.equal(result.rows.some(row => row.name === 'nix'), false);
});

test('second failed import retains that archive; only the first succeeded archive is released', () => {
  const result = run(imported, 'second-import');
  assert.equal(result.status, 37, result.stderr);
  assert.deepEqual(released(result.rows), archives.slice(0, 1));
  assert.equal(result.rows.some(row => row.name === 'nix'), false);
});

test('decompression failure remains fatal under pipefail, without deleting its input', () => {
  const result = run(imported, 'decompress');
  assert.equal(result.status, 29, result.stderr);
  assert.deepEqual(released(result.rows), []);
});

test('build failure stays fatal after successful imports and does not reach OMC', () => {
  const result = run(imported, 'build');
  assert.equal(result.status, 43, result.stderr);
  assert.deepEqual(released(result.rows), archives);
  assert.equal(result.rows.some(row => row.name === 'omc'), false);
});

test('diagnostic failures do not suppress the real import/build path', () => {
  const result = run(imported, 'diagnostics');
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(released(result.rows), archives);
});

test('unsafe deletion target is rejected by the policy witness using mocked rm', () => {
  const result = run(imported.replace('rm -- "$archive"', 'rm -rf target'));
  assert.equal(result.status, 0, result.stderr);
  assert.throws(() => released(result.rows));
});

test('cleanup failure is fatal before later cleanup commands', () => {
  const result = run(reclaim(), 'cleanup');
  assert.equal(result.status, 47, result.stderr);
  assert.equal(result.rows.some(row => row.name === 'docker'), false);
});
