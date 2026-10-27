import { test } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, mkdirSync, writeFileSync, readlinkSync, readdirSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { spawnSync } from 'node:child_process';

const script = new URL('./prepare-linux-uapi.sh', import.meta.url).pathname;
function fixture(arch, run) {
  const root = mkdtempSync(join(tmpdir(), 'linux-uapi-'));
  try {
    const headers = join(root, 'headers');
    for (const dir of ['linux', 'asm-generic', `${arch}/asm`, 'bits'])
      mkdirSync(join(headers, dir), { recursive: true });
    writeFileSync(join(headers, 'stdio.h'), '#error glibc must not be exposed\n');
    const bin = join(root, 'bin'); mkdirSync(bin);
    writeFileSync(join(bin, 'dpkg-architecture'), `#!/bin/sh\nprintf '%s\\n' '${arch}'\n`, { mode: 0o755 });
    const output = join(root, 'uapi');
    const invoke = () => spawnSync('bash', [script, output], {
      encoding: 'utf8', env: { ...process.env, LINUX_UAPI_ROOT: headers, PATH: `${bin}:${process.env.PATH}` },
    });
    run({ root, headers, output, invoke });
  } finally { rmSync(root, { recursive: true, force: true }); }
}
for (const arch of ['x86_64-linux-gnu', 'aarch64-linux-gnu']) {
  test(`${arch}: exposes only kernel namespaces and matching asm`, () => fixture(arch, ({ headers, output, invoke }) => {
    const result = invoke(); assert.equal(result.status, 0, result.stderr);
    assert.deepEqual(readdirSync(output).sort(), ['asm', 'asm-generic', 'linux']);
    assert.equal(readlinkSync(join(output, 'asm')), join(headers, arch, 'asm'));
    assert.equal(readlinkSync(join(output, 'linux')), join(headers, 'linux'));
    assert.notEqual(invoke().status, 0, 'stale output must refuse');
  }));
}
test('missing architecture headers refuse before output is created', () => fixture('aarch64-linux-gnu', ({ headers, output, invoke }) => {
  rmSync(join(headers, 'aarch64-linux-gnu'), { recursive: true });
  const result = invoke(); assert.notEqual(result.status, 0);
  assert.match(result.stderr, /Missing architecture UAPI/);
  assert.throws(() => readdirSync(output));
}));
test('missing generic kernel headers refuse', () => fixture('x86_64-linux-gnu', ({ headers, invoke }) => {
  rmSync(join(headers, 'asm-generic'), { recursive: true });
  assert.notEqual(invoke().status, 0);
}));
test('unsupported native architecture refuses rather than guessing asm', () => fixture('riscv64-linux-gnu', ({ invoke }) => {
  const result = invoke(); assert.notEqual(result.status, 0);
  assert.match(result.stderr, /Unsupported native Linux architecture/);
}));
