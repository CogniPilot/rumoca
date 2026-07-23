import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import {
  chmodSync,
  mkdtempSync,
  mkdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import { spawnSync } from 'node:child_process';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { test } from 'node:test';

const root = fileURLToPath(new URL('../..', import.meta.url));
const read = (path) => readFileSync(new URL(`../../${path}`, import.meta.url), 'utf8');
const pin = '1.27.0~1-gd7e2907-1';
const pool = 'https://build.openmodelica.org/apt/pool/contrib-noble';
const packages = [
  ['omc', 'amd64'],
  ['omc-common', 'all'],
  ['libomc', 'amd64'],
  ['libomcsimulation', 'amd64'],
];

const fixture = ({ metadata = {}, rows = packages } = {}) => {
  const directory = mkdtempSync(join(tmpdir(), 'rumoca-omc-fixture-'));
  const packagesDirectory = join(directory, 'packages');
  const binDirectory = join(directory, 'bin');
  const manifest = join(directory, 'manifest');
  mkdirSync(packagesDirectory);
  mkdirSync(binDirectory);

  const manifestRows = packages.map(([packageName, architecture]) => {
    const filename = `${packageName}_${pin}_${architecture}.deb`;
    const fields = {
      Package: packageName,
      Version: pin,
      Architecture: architecture,
      ...metadata[packageName],
    };
    const contents = Object.entries(fields)
      .map(([name, value]) => `${name}: ${value}`)
      .join('\n');
    writeFileSync(join(packagesDirectory, filename), `${contents}\npayload\n`);
    const sha256 = createHash('sha256')
      .update(readFileSync(join(packagesDirectory, filename)))
      .digest('hex');
    return `${packageName} ${pin} ${architecture} ${filename} ${sha256} ${pool}/${filename}`;
  });
  writeFileSync(manifest, `${rows.map((row) => (
    Array.isArray(row) ? manifestRows[packages.indexOf(row)] : row
  )).join('\n')}\n`);
  const dpkgDeb = join(binDirectory, 'dpkg-deb');
  writeFileSync(dpkgDeb, `#!/usr/bin/env bash
set -euo pipefail
[[ "$1" == "-f" ]]
sed -n "s/^$3: //p" "$2"
`);
  chmodSync(dpkgDeb, 0o755);

  return {
    directory,
    manifest,
    packagesDirectory,
    run: () => spawnSync(
      join(root, 'scripts/ci/verify-openmodelica-packages.sh'),
      [manifest, pin, packagesDirectory],
      {
        encoding: 'utf8',
        env: { ...process.env, PATH: `${binDirectory}:${process.env.PATH}` },
      },
    ),
  };
};

const withFixture = (options, check) => {
  const value = fixture(options);
  try {
    check(value);
  } finally {
    rmSync(value.directory, { recursive: true, force: true });
  }
};

const workflowJob = (workflow, job) => {
  const start = workflow.indexOf(`  ${job}:`);
  assert.notEqual(start, -1, `${job} must exist`);
  const next = workflow.slice(start + 1).search(/^  [a-zA-Z0-9_-]+:$/m);
  return workflow.slice(start, next === -1 ? undefined : start + 1 + next);
};

test('OpenModelica uses one exact repository pin', () => {
  const version = read('toolchains/openmodelica-version').trim();
  assert.match(version, /^\d+\.\d+\.\d+~1-g[0-9a-f]+-\d+$/);
  assert.equal(version, pin);
});

test('OpenModelica manifest pins the four official package artifacts', () => {
  assert.equal(
    read('toolchains/openmodelica-packages.txt'),
    `# source_commit d7e2907f419d8061ce0a461ac9709dcb84fa70a1
omc ${pin} amd64 omc_${pin}_amd64.deb a4511acb19f7377275347f9fc27af92c307db0903e7f46e44085410a060d86ab ${pool}/omc_${pin}_amd64.deb
omc-common ${pin} all omc-common_${pin}_all.deb 9781f4efa44274ecff4538227ef8054b10d3b9abe6cf3d36bb9b23237c0b1119 ${pool}/omc-common_${pin}_all.deb
libomc ${pin} amd64 libomc_${pin}_amd64.deb 90b650ecf9da174cd477115a8f61d54f8bf22b6c87ba1707cb5482d933ae3f5f ${pool}/libomc_${pin}_amd64.deb
libomcsimulation ${pin} amd64 libomcsimulation_${pin}_amd64.deb bdc2c5c307aacf516017f8b64fc4878e6a75d7b0587366d2b93ec8ae3590632d ${pool}/libomcsimulation_${pin}_amd64.deb
`,
  );
});

test('offline package verifier accepts the exact artifact set', () => {
  withFixture({}, ({ run }) => {
    const result = run();
    assert.equal(result.status, 0, result.stderr);
  });
});

test('offline package verifier rejects a bad checksum', () => {
  withFixture({}, ({ manifest, run }) => {
    const contents = readFileSync(manifest, 'utf8');
    writeFileSync(manifest, contents.replace(/[0-9a-f]{64}/, '0'.repeat(64)));
    const result = run();
    assert.notEqual(result.status, 0);
    assert.match(result.stderr, /checksum mismatch/i);
  });
});

test('offline package verifier rejects incorrect Debian metadata', () => {
  for (const [field, value] of [
    ['Package', 'wrong-package'],
    ['Version', '1.27.0~2-g4470062-1'],
    ['Architecture', 'arm64'],
  ]) {
    withFixture({ metadata: { omc: { [field]: value } } }, ({ run }) => {
      const result = run();
      assert.notEqual(result.status, 0, `${field}=${value}`);
      assert.match(result.stderr, new RegExp(`${field} mismatch`, 'i'));
    });
  }
});

test('offline package verifier rejects missing and duplicate manifest entries', () => {
  for (const rows of [packages.slice(0, -1), [...packages, packages[0]]]) {
    withFixture({ rows }, ({ run }) => {
      const result = run();
      assert.notEqual(result.status, 0, result.stderr);
      assert.match(result.stderr, /missing|duplicate/i);
    });
  }
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

test('the installer uses content-verified pool artifacts without a rolling index', () => {
  const installer = read('scripts/ci/install-openmodelica.sh');

  assert.match(installer, /openmodelica-packages\.txt/);
  assert.match(installer, /verify-openmodelica-packages\.sh/);
  assert.match(installer, /mktemp -d/);
  assert.match(installer, /trap .*EXIT/);
  assert.match(installer, /curl .*--proto ['"]?=https/);
  assert.doesNotMatch(installer, /apt-cache madison/);
  assert.doesNotMatch(installer, /openmodelica\.list|openmodelica-keyring/);
  assert.doesNotMatch(installer, /build\.openmodelica\.org\/apt .*stable/);
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
  assert.equal(read('toolchains/openmodelica-version').trim(), pin);
  const result = spawnSync(
    'scripts/ci/install-openmodelica.sh',
    ['--check-output', 'OpenModelica 1.27.0~1-gd7e2907'],
    { encoding: 'utf8' },
  );

  assert.equal(result.status, 0, result.stderr);
  assert.equal(result.stdout, 'OpenModelica 1.27.0~1-gd7e2907\n');
});

test('the installer rejects runtime identities other than the exact pin-derived release', () => {
  for (const output of [
    'OpenModelica 1.27.0',
    'OpenModelica 1.27.0~1-g0000000',
    'OpenModelica 1.27.0~2-g4470062',
    'OpenModelica 1.27.0~dev.beta.1-4-ge5d8071',
  ]) {
    const result = spawnSync(
      'scripts/ci/install-openmodelica.sh',
      ['--check-output', output],
      { encoding: 'utf8' },
    );

    assert.notEqual(result.status, 0, output);
    assert.match(result.stderr, /OpenModelica version mismatch/, output);
  }
});
