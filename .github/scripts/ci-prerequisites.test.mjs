import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync, mkdtempSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { spawnSync } from 'node:child_process';

const action = readFileSync(new URL('../actions/apt-install/action.yml', import.meta.url), 'utf8');
const script = action.split('      run: |\n')[1].replace(/^        /gm, '');

function runApt(statuses) {
  const dir = mkdtempSync(join(tmpdir(), 'rumoca-apt-'));
  try {
    const commands = {
      'dpkg-query': 'exit 1',
      sleep: 'exit 0',
      sudo: `case " $* " in
  *" apt-get "*) ;;
  *) exit 0 ;;
esac
printf '%s\\n' "$*" >> "$LOG"
n=$(cat "$COUNT" 2>/dev/null || echo 0)
n=$((n + 1)); echo "$n" > "$COUNT"
status=$(sed -n "\${n}p" "$STATUSES")
exit "\${status:-99}"`,
    };
    for (const [name, body] of Object.entries(commands)) {
      writeFileSync(join(dir, name), '#!/bin/bash\n' + body + '\n', { mode: 0o755 });
    }
    writeFileSync(join(dir, 'statuses'), statuses.join('\n') + '\n');
    const result = spawnSync('bash', ['-c', script], {
      encoding: 'utf8',
      env: { ...process.env, PATH: `${dir}:${process.env.PATH}`, APT_PACKAGES: 'libudev-dev',
        APT_TIMEOUT_SECONDS: '300', LOG: join(dir, 'log'), COUNT: join(dir, 'count'), STATUSES: join(dir, 'statuses') },
    });
    return { ...result, commands: readFileSync(join(dir, 'log'), 'utf8') };
  } finally { rmSync(dir, { recursive: true, force: true }); }
}

test('successful install stops immediately and retains separate network/wall limits', () => {
  const r = runApt([0]);
  assert.equal(r.status, 0);
  assert.equal(r.commands.trim().split('\n').length, 1);
  assert.match(r.commands, /300s apt-get/);
  assert.match(r.commands, /Acquire::http::Timeout=20/);
  assert.match(r.stdout, /stage=install exit=0 elapsed=\d+s/);
});
test('timeout is diagnosed and a successful refresh/install recovers', () => {
  const r = runApt([124, 0, 0]);
  assert.equal(r.status, 0);
  assert.match(r.stdout, /stage=install exit=124/);
  assert.match(r.stdout, /exceeded its wall-clock limit/);
  assert.equal(r.commands.trim().split('\n').length, 3);
});
test('persistent install failure remains fatal after bounded retries', () => {
  const r = runApt([100, 0, 100, 0, 100, 0, 100]);
  assert.equal(r.status, 1);
  assert.equal(r.commands.trim().split('\n').length, 7);
  assert.match(r.stdout, /installation failed after bounded retries/);
});
