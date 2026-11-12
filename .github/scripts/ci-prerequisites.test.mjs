import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync, mkdtempSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { spawnSync } from 'node:child_process';

const action = readFileSync(new URL('../actions/apt-install/action.yml', import.meta.url), 'utf8');
const script = action.split('      run: |\n')[1].replace(/^        /gm, '');

function runApt(statuses, dnsAttempts = []) {
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
case ",${dnsAttempts.join(',')}," in
  *",$n,"*) echo "E: Failed to fetch archive: Temporary failure resolving 'archive.ubuntu.com'" >&2 ;;
esac
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

test('exhausted DNS setup is explicitly marked without changing failure status', () => {
  const r = runApt([100, 100, 100, 100], [1, 2, 3, 4]);
  assert.equal(r.status, 1);
  assert.match(r.stdout, /::error title=APT mirror DNS failure::/);
});

test('an earlier DNS error cannot mark a later permanent package failure', () => {
  const r = runApt([100, 0, 100, 0, 100, 0, 100], [1]);
  assert.equal(r.status, 1);
  assert.doesNotMatch(r.stdout, /::error title=APT mirror DNS failure::/);
});

test('recovered DNS setup does not emit a failure marker', () => {
  const r = runApt([100, 0, 0], [1]);
  assert.equal(r.status, 0);
  assert.doesNotMatch(r.stdout, /::error title=APT mirror DNS failure::/);
});

test('the macOS/Windows clippy step runs after setup-python with its interpreter bound', () => {
  const workflow = readFileSync(new URL('../workflows/ci.yml', import.meta.url), 'utf8');
  const job = workflow.slice(workflow.indexOf('    name: Test (${{ matrix.os }})'));
  const python = job.indexOf('        id: python\n        uses: actions/setup-python@');
  const clippy = job.indexOf('      - name: Clippy with warnings denied (macOS and Windows)');
  assert.ok(python > 0, 'the test job sets up Python with id python');
  assert.ok(clippy > python, 'clippy runs after Python setup');
  const step = job.slice(clippy, job.indexOf('\n      - name:', clippy + 1));
  assert.match(step, /PYO3_PYTHON: \$\{\{ steps\.python\.outputs\.python-path \}\}/);
  assert.match(step, /run: cargo xtask verify clippy/);
});
