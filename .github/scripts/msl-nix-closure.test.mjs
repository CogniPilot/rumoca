import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import {
  chmodSync,
  existsSync,
  mkdtempSync,
  mkdirSync,
  readFileSync,
  readdirSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawnSync } from 'node:child_process';
import { test } from 'node:test';

const repoFile = (path) => new URL(`../../${path}`, import.meta.url);
const helper = fileURLToPath(repoFile('.github/scripts/msl-nix-closure.sh'));
const requiredBinaries = [
  'msl_tests',
  'rumoca-worker',
  'rumoca-sim-worker',
  'rumoca-msl-tools',
];
const commit = '0123456789abcdef0123456789abcdef01234567';
const system = 'x86_64-linux';

const sha256 = (value) => createHash('sha256').update(value).digest('hex');

const workflowJob = (workflow, job) => {
  const start = workflow.indexOf(`  ${job}:`);
  assert.notEqual(start, -1, `${job} must exist`);
  const next = workflow.slice(start + 1).search(/^  [a-zA-Z0-9_-]+:$/m);
  return workflow.slice(start, next === -1 ? undefined : start + 1 + next);
};

const fixture = ({ exportFails = false, importFails = false } = {}) => {
  const root = realpathSync(mkdtempSync(join(tmpdir(), 'rumoca-msl-closure-')));
  const artifactDir = join(root, 'artifact');
  const outPath = join(root, 'nix-store', 'msl-output');
  const outLink = join(root, 'result-msl-artifacts');
  const fakeBin = join(root, 'fake-bin');
  const nixLog = join(root, 'nix-store.log');
  const tempDir = join(root, 'tmp');
  mkdirSync(artifactDir, { recursive: true });
  mkdirSync(join(outPath, 'bin'), { recursive: true });
  mkdirSync(join(root, 'nix-store', 'dependency'));
  mkdirSync(fakeBin);
  mkdirSync(tempDir);
  for (const binary of requiredBinaries) {
    writeFileSync(join(outPath, 'bin', binary), `${binary}\n`);
    chmodSync(join(outPath, 'bin', binary), 0o755);
  }
  const fakeNixStore = `#!/usr/bin/env bash
set -euo pipefail
printf '%s\\n' "$*" >> "${nixLog}"
case "\${1-}" in
  --query)
    printf '%s\\n' "${join(root, 'nix-store', 'dependency')}" "${outPath}"
    ;;
  --export)
    ${exportFails ? "printf 'partial-archive'; exit 24" : "printf 'complete-closure-archive'"}
    ;;
  --import)
    cat >/dev/null
    ${importFails ? 'exit 23' : "printf '%s\\n' '${outPath}'"}
    ;;
  --verify-path)
    ;;
  *)
    echo "unexpected nix-store invocation: $*" >&2
    exit 97
    ;;
esac
`;
  writeFileSync(join(fakeBin, 'nix-store'), fakeNixStore);
  chmodSync(join(fakeBin, 'nix-store'), 0o755);
  return {
    artifactDir,
    env: { ...process.env, PATH: `${fakeBin}:${process.env.PATH}`, TMPDIR: tempDir },
    nixLog,
    outLink,
    outPath,
    root,
    tempDir,
  };
};

const writeArtifact = (fx, overrides = {}) => {
  const archive = overrides.archive ?? 'complete-closure-archive';
  writeFileSync(join(fx.artifactDir, 'closure.nar'), archive);
  const values = {
    version: '1',
    commit,
    system,
    out_path: fx.outPath,
    archive_sha256: sha256(archive),
    ...Object.fromEntries(
      requiredBinaries.map((binary) => [
        `binary_${binary}_sha256`,
        sha256(`${binary}\n`),
      ]),
    ),
    ...overrides,
  };
  delete values.archive;
  writeFileSync(
    join(fx.artifactDir, 'manifest'),
    `${Object.entries(values).map(([key, value]) => `${key}=${value}`).join('\n')}\n`,
  );
};

const restore = (fx, expectedCommit = commit, expectedSystem = system) =>
  spawnSync(
    'bash',
    [helper, 'restore', fx.artifactDir, fx.outLink, expectedCommit, expectedSystem],
    { encoding: 'utf8', env: fx.env },
  );

test('CI architecture is permanently independent of Cachix', () => {
  const workflow = readFileSync(repoFile('.github/workflows/ci.yml'), 'utf8');
  const flake = readFileSync(repoFile('flake.nix'), 'utf8');
  assert.doesNotMatch(workflow, /cachix\/cachix-action/i);
  assert.doesNotMatch(workflow, /CACHIX_AUTH_TOKEN/);
  assert.doesNotMatch(workflow, /rumoca\.cachix\.org/i);
  assert.doesNotMatch(flake, /Cachix/i);
});

test('workflow transports one rerun-stable MSL Nix closure artifact', () => {
  const workflow = readFileSync(repoFile('.github/workflows/ci.yml'), 'utf8');
  const artifactName = 'msl-nix-closure-${{ github.run_id }}-${{ github.sha }}';
  assert.match(workflow, /GitHub Actions artifact[^\n]*complete Nix closure/i);

  const producer = workflowJob(workflow, 'nix-build-msl');
  assert.match(
    producer,
    /nix build --print-build-logs \.#msl-artifacts --out-link result-msl-artifacts/,
  );
  assert.match(producer, /\.github\/scripts\/msl-nix-closure\.sh pack/);
  assert.ok(producer.includes(`name: ${artifactName}`));
  assert.match(producer, /uses: actions\/upload-artifact@v6/);
  assert.match(producer, /path: target\/msl-nix-closure/);
  assert.match(producer, /if-no-files-found: error/);
  assert.match(producer, /overwrite: true/);
  assert.match(producer, /retention-days: [1-3]/);

  for (const job of ['msl-shards', 'msl-merge', 'modelicatest-gate']) {
    const body = workflowJob(workflow, job);
    assert.match(body, /needs: (?:\[?[^\n]*\b)nix-build-msl\b/);
    assert.match(body, /uses: actions\/download-artifact@v8/);
    assert.ok(body.includes(`name: ${artifactName}`));
    assert.match(body, /\.github\/scripts\/msl-nix-closure\.sh restore/);
    assert.doesNotMatch(body, /nix build[^\n]*\.#msl-artifacts/);
    const restoreIndex = body.indexOf('.github/scripts/msl-nix-closure.sh restore');
    const useIndex = body.indexOf('result-msl-artifacts/bin/');
    assert.ok(restoreIndex !== -1 && restoreIndex < useIndex, `${job} must restore before use`);
  }

  const artifactNames = workflow
    .split('\n')
    .filter((line) => line.trimStart().startsWith('name: msl-nix-closure-'));
  assert.deepEqual(
    artifactNames.map((line) => line.trim()),
    Array(4).fill(`name: ${artifactName}`),
    'producer overwrite and partial consumer reruns must share one stable artifact name',
  );
});

test('manifest line count is derived from fixed fields and required binaries', () => {
  const source = readFileSync(helper, 'utf8');
  assert.doesNotMatch(source, /wc -l[^\n]*-eq\s+9/);
  assert.match(
    source,
    /expected_manifest_lines=\$\(\(\$\{#manifest_fields\[@\]\} \+ \$\{#required_binaries\[@\]\}\)\)/,
  );
});

test('helper cleanup is process-scoped instead of relying on RETURN traps', () => {
  const source = readFileSync(helper, 'utf8');
  assert.doesNotMatch(source, /trap[^\n]*RETURN/);
  assert.match(source, /trap\s+cleanup\s+EXIT/);
});

test('pack exports the complete Nix requisites closure and records provenance', () => {
  const fx = fixture();
  const result = spawnSync(
    'bash',
    [helper, 'pack', fx.outPath, fx.artifactDir, commit, system],
    { encoding: 'utf8', env: fx.env },
  );
  assert.equal(result.status, 0, result.stderr);
  const nixLog = readFileSync(fx.nixLog, 'utf8');
  assert.match(nixLog, new RegExp(`--query --requisites ${fx.outPath}`));
  assert.match(nixLog, /--export .*dependency.*msl-output/);
  const manifest = readFileSync(join(fx.artifactDir, 'manifest'), 'utf8');
  assert.match(manifest, new RegExp(`commit=${commit}`));
  assert.match(manifest, new RegExp(`system=${system}`));
  assert.match(manifest, new RegExp(`out_path=${fx.outPath}`));
  assert.match(manifest, /archive_sha256=[0-9a-f]{64}/);
  for (const binary of requiredBinaries) {
    assert.match(manifest, new RegExp(`binary_${binary}_sha256=[0-9a-f]{64}`));
  }
});

test('restore rejects a missing manifest', () => {
  const fx = fixture();
  writeFileSync(join(fx.artifactDir, 'closure.nar'), 'archive');
  const result = restore(fx);
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /missing manifest/i);
});

test('restore rejects a missing closure archive', () => {
  const fx = fixture();
  writeArtifact(fx);
  rmSync(join(fx.artifactDir, 'closure.nar'));
  const result = restore(fx);
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /missing closure archive/i);
});

test('restore rejects an archive checksum mismatch before Nix import', () => {
  const fx = fixture();
  writeArtifact(fx, { archive_sha256: '0'.repeat(64) });
  const result = restore(fx);
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /archive checksum mismatch/i);
  assert.equal(existsSync(fx.nixLog), false);
});

test('restore rejects a commit mismatch', () => {
  const fx = fixture();
  writeArtifact(fx, { commit: 'f'.repeat(40) });
  const result = restore(fx);
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /commit mismatch/i);
});

test('restore rejects a Nix system mismatch', () => {
  const fx = fixture();
  writeArtifact(fx, { system: 'aarch64-linux' });
  const result = restore(fx);
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /system mismatch/i);
});

test('restore fails closed when Nix import fails', () => {
  const fx = fixture({ importFails: true });
  writeArtifact(fx);
  const result = restore(fx);
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /Nix closure import failed/i);
});

test('pack failure removes all temporary closure files', () => {
  const fx = fixture({ exportFails: true });
  const result = spawnSync(
    'bash',
    [helper, 'pack', fx.outPath, fx.artifactDir, commit, system],
    { encoding: 'utf8', env: fx.env },
  );
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /failed to export Nix requisites closure/i);
  assert.deepEqual(readdirSync(fx.artifactDir), []);
});

test('restore failure removes its mktemp file', () => {
  const fx = fixture();
  writeArtifact(fx);
  rmSync(join(fx.root, 'nix-store', 'dependency'), { recursive: true });
  const result = restore(fx);
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /imported Nix requisite is missing/i);
  assert.deepEqual(readdirSync(fx.tempDir), []);
});

test('restore rejects a missing required binary', () => {
  const fx = fixture();
  writeArtifact(fx);
  rmSync(join(fx.outPath, 'bin', 'rumoca-worker'));
  const result = restore(fx);
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /missing required executable.*rumoca-worker/i);
});

test('restore rejects a non-executable required binary', () => {
  const fx = fixture();
  writeArtifact(fx);
  chmodSync(join(fx.outPath, 'bin', 'rumoca-sim-worker'), 0o644);
  const result = restore(fx);
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /missing required executable.*rumoca-sim-worker/i);
});

test('restore rejects a required binary checksum mismatch', () => {
  const fx = fixture();
  writeArtifact(fx, { binary_msl_tests_sha256: 'f'.repeat(64) });
  const result = restore(fx);
  assert.notEqual(result.status, 0);
  assert.match(result.stderr, /required executable checksum mismatch.*msl_tests/i);
});

test('restore imports, verifies, and recreates the exact output link', () => {
  const fx = fixture();
  writeArtifact(fx);
  const result = restore(fx);
  assert.equal(result.status, 0, result.stderr);
  assert.equal(readFileSync(fx.nixLog, 'utf8').includes('--import'), true);
  assert.equal(realpathSync(fx.outLink), fx.outPath);
});
