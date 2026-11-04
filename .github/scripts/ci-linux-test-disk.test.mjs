import assert from 'node:assert/strict';
import { mkdtempSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { spawnSync } from 'node:child_process';
import test from 'node:test';

const workflow = readFileSync(new URL('../workflows/ci.yml', import.meta.url), 'utf8');
const helper = readFileSync(new URL('./qualify-issue422-static-pie.sh', import.meta.url), 'utf8');
const sdkName = '      - name: Reclaim unused SDKs before Linux tests\n';
const sdkPaths = ['/usr/share/dotnet', '/usr/local/lib/android', '/opt/ghc', '/usr/local/.ghcup', '/opt/hostedtoolcache/CodeQL'];
function reclaimStep(text) {
  const start = text.indexOf(sdkName);
  assert.notEqual(start, -1, 'Linux Test must declare its bounded SDK reclaim');
  const end = text.indexOf('      - name:', start + sdkName.length);
  const step = text.slice(start, end);
  assert.match(step, /if: runner.os == 'Linux'/);
  const run = step.split('        run: |\n')[1];
  const body = run.split('\n').map(line => line.startsWith('          ') ? line.slice(10) : line).join('\n');
  // Commands and paths are closed source, never supplied by a workflow input.
  const args = body.slice(body.indexOf('sudo rm -rf'), body.indexOf('\necho "== disk after')).replace(/\\\n/g, ' ').trim().split(/\s+/);
  assert.deepEqual(args, ['sudo', 'rm', '-rf', ...sdkPaths]);
  assert.equal((body.match(/sudo /g) || []).length, 1);
  assert.doesNotMatch(body, /docker|apt-get|cargo clean|\$\{|\$HOME|rm -rf target/);
  return body;
}
function fixture() {
  const root = mkdtempSync(join(tmpdir(), 'rumoca-disk-control-'));
  const bin = join(root, 'bin'); mkdirSync(bin);
  const calls = join(root, 'calls'); writeFileSync(calls, '');
  for (const name of ['sudo', 'df', 'du']) {
    writeFileSync(join(bin, name), `#!/usr/bin/env bash\nprintf '%s' '${name}' >> "$CALLS"\nprintf ' <%s>' "$@" >> "$CALLS"\nprintf '\\n' >> "$CALLS"\nexit "\${${name.toUpperCase()}_STATUS:-0}"\n`, { mode: 0o755 });
  }
  return { root, calls, env: { ...process.env, PATH: `${bin}:${process.env.PATH}`, CALLS: calls } };
}
function runReclaim(body, overrides = {}) {
  const f = fixture();
  const result = spawnSync('bash', ['-c', body], { cwd: f.root, env: { ...f.env, ...overrides }, encoding: 'utf8', timeout: 5000 });
  assert.ifError(result.error);
  return { ...result, calls: readFileSync(f.calls, 'utf8') };
}
function prepareFunction(text) {
  return text.slice(text.indexOf('prepare() {'), text.indexOf('\nexecute() {'));
}
function runPrepare(source, cargoStatus, options = {}) {
  const f = fixture();
  const evidence = join(f.root, 'evidence'); mkdirSync(evidence);
  const exe = join(f.root, 'synthetic-owned-executable');
  writeFileSync(exe, '#!/bin/false\n', { mode: 0o755 }); // Never executed.
  const script = `set -euo pipefail
    evidence="$EVIDENCE"
    CARGO_BUILD_JOBS=4
    cargo() {
      printf '%s\\n' "$@" > "$EVIDENCE/cargo-argv"
      printf '%s\\n' '{"reason":"compiler-message","message":{"level":"error","rendered":"injected link diagnostic"}}'
      printf 'injected Cargo stderr\\n' >&2
      return "$CARGO_STATUS"
    }
    jq() {
      if test "$1" = -r; then
        printf 'injected link diagnostic\\n'
        return "$DIAGNOSTIC_STATUS"
      fi
      printf '%s\\n' "$FAKE_EXECUTABLE"
    }
    tail() { command tail "$@"; return "$TAIL_STATUS"; }
    file() { printf 'synthetic owned fixture (not a real ELF)\\n'; }
    readelf() {
      case "$1" in
        -h) printf 'Class: ELF64\\nType: DYN\\nMachine: Advanced Micro Devices X86-64\\n';;
        -l|-d) printf 'no interpreter or dependencies in synthetic fixture\\n';;
        *) return 2;;
      esac
    }
    ${prepareFunction(source)}
    prepare fixture target lib selector -p selected --lib
    printf 'AFTER_PREPARE\\n'
  `;
  const result = spawnSync('bash', ['-c', script], { cwd: f.root, env: { ...f.env, EVIDENCE: evidence, CARGO_STATUS: String(cargoStatus), DIAGNOSTIC_STATUS: String(options.jqStatus ?? 0), TAIL_STATUS: String(options.tailStatus ?? 0), FAKE_EXECUTABLE: exe }, encoding: 'utf8', timeout: 5000 });
  assert.ifError(result.error);
  return { ...result, evidence, argv: readFileSync(join(evidence, 'cargo-argv'), 'utf8') };
}

test('Linux SDK reclaim uses only the existing fixed list before native caches/builds', () => {
  const body = reclaimStep(workflow);
  assert.ok(workflow.indexOf(sdkName) < workflow.indexOf('      - name: Rust cache (native)'));
  const r = runReclaim(body);
  assert.equal(r.status, 0);
  assert.ok(r.calls.indexOf('df ') < r.calls.indexOf('sudo '));
  assert.match(r.calls, /sudo <rm> <-rf> <\/usr\/share\/dotnet>/);
  assert.equal((r.calls.match(/^sudo /gm) || []).length, 1);
  assert.equal((r.calls.match(/^df /gm) || []).length, 2);
  for (const path of sdkPaths) assert.ok(r.calls.includes(`<${path}>`));
});
test('an SDK deletion failure remains fatal without a following success census', () => {
  const r = runReclaim(reclaimStep(workflow), { SUDO_STATUS: '19' });
  assert.equal(r.status, 19);
  assert.equal((r.calls.match(/^df /gm) || []).length, 1);
});
test('a failed initial disk observation cannot reach reclaim', () => {
  const r = runReclaim(reclaimStep(workflow), { DF_STATUS: '23' });
  assert.equal(r.status, 23); assert.doesNotMatch(r.calls, /^sudo /m);
});
test('predecessor omission and unsafe/target-path mutants fail the same closed-list control', () => {
  assert.throws(() => reclaimStep(workflow.replace(sdkName, '      - name: absent\n')));
  assert.throws(() => reclaimStep(workflow.slice(0, workflow.indexOf(sdkName)) + workflow.slice(workflow.indexOf(sdkName)).replaceAll('/usr/share/dotnet', '/home/runner/.cargo')));
  assert.throws(() => reclaimStep(workflow.slice(0, workflow.indexOf(sdkName)) + workflow.slice(workflow.indexOf(sdkName)).replaceAll('/usr/share/dotnet', 'target/debug')));
});
test('nested build-size observations use separate invocations and remain advisory', () => {
  const native = workflow.slice(workflow.indexOf('    name: Test (${{ matrix.os }})'), workflow.indexOf('\n  template-runtime-tests:'));
  assert.equal((native.match(/du -sh -- "\$path" 2>\/dev\/null \|\| true/g) || []).length, 2);
  assert.equal((native.match(/target\/debug\/deps target\/debug\/build target\/debug\/incremental/g) || []).length, 2);
});
for (const status of [101, 2, 130, 143]) {
  test(`Cargo failure ${status} is preserved and its redirected diagnostic is surfaced`, () => {
    const r = runPrepare(helper, status);
    assert.equal(r.status, status); assert.doesNotMatch(r.stdout, /AFTER_PREPARE/);
    assert.match(r.stderr, /injected Cargo stderr/); assert.match(r.stderr, /injected link diagnostic/);
    assert.match(r.stderr, new RegExp(`Cargo exit ${status}`));
    assert.match(r.argv, /--locked\n--jobs\n4\n--profile\ntest\n--message-format=json\n-p\nselected\n--lib\n--\n-C\ntarget-feature=\+crt-static/);
  });
}
test('diagnostic helper failures cannot replace the original Cargo failure', () => {
  const r = runPrepare(helper, 101, { jqStatus: 31, tailStatus: 32 });
  assert.equal(r.status, 101); assert.doesNotMatch(r.stdout, /AFTER_PREPARE/);
});
test('successful preparation retains artifact selection and hash creation', () => {
  const r = runPrepare(helper, 0);
  assert.equal(r.status, 0); assert.match(r.stdout, /AFTER_PREPARE/);
  assert.doesNotMatch(r.stderr, /preparation failed/);
  assert.match(readFileSync(join(r.evidence, 'fixture.elf.sha256'), 'utf8'), /fixture\.elf/);
});
test('old silent failure and status-swallowing mutants fail the diagnostic/status oracle', () => {
  const old = helper.replace('  local build_status=0\n', '').replace(' || build_status=$?', '');
  const start = old.indexOf('  if test "$build_status" -ne 0; then\n');
  const end = old.indexOf('  local executable\n', start);
  const predecessor = old.slice(0, start) + old.slice(end);
  const red = runPrepare(predecessor, 101);
  assert.equal(red.status, 101); assert.doesNotMatch(red.stderr, /injected link diagnostic/);
  const swallowed = runPrepare(helper.replace('return "$build_status"', 'return 0'), 101);
  assert.notEqual(swallowed.status, 101);
});
