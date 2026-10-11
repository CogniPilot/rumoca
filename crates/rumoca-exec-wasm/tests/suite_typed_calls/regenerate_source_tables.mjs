// Run under the owned resource guardian with a fresh scratch output directory.
// Produces evidence only; tracked fixtures and their old identities stay intact.
import { createHash } from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';

const [repositoryArgument, outputArgument, ...extra] = process.argv.slice(2);
if (!repositoryArgument || !outputArgument || extra.length) {
  throw new Error('REPOSITORY FRESH_OUTPUT_DIRECTORY required');
}
const repository = fs.realpathSync(repositoryArgument);
const output = path.resolve(outputArgument);
const target = process.env.CARGO_TARGET_DIR;
if (!target || !path.isAbsolute(target)) {
  throw new Error('CARGO_TARGET_DIR must name the leased absolute cache directory');
}
const sha256 = (bytes) => createHash('sha256').update(bytes).digest('hex');
const fileHash = (file) => sha256(fs.readFileSync(file));
const git = (...args) => {
  const result = spawnSync('git', args, { cwd: repository, encoding: 'utf8' });
  if (result.status !== 0) throw new Error(result.stderr || 'git failed');
  return result.stdout;
};
if (git('status', '--porcelain', '--untracked-files=normal').trim()) {
  throw new Error('compiler checkout must be clean before issuance');
}
const revision = git('rev-parse', 'HEAD').trim();
const tracked = git('ls-files', '-z').split('\0').filter(Boolean);
const sourceManifestFormat = 'git ls-files order; sha256(raw regular-file bytes or raw symlink-target bytes), two spaces, path, newline';
const trackedBytes = (file) => {
  const stat = fs.lstatSync(file);
  if (stat.isSymbolicLink()) return fs.readlinkSync(file, { encoding: 'buffer' });
  if (stat.isFile()) return fs.readFileSync(file);
  throw new Error(`unsupported tracked source entry type: ${file}`);
};
const sourceManifest = () => tracked.map((file) => `${sha256(trackedBytes(path.join(repository, file)))}  ${file}\n`).join('');
const before = sourceManifest();
const compilerSourceSha256 = sha256(before);
fs.mkdirSync(output); // Require a new evidence directory; never overwrite a receipt.
fs.writeFileSync(path.join(output, 'compiler-source.sha256'), before);

const run = (command, args, log) => {
  const fd = fs.openSync(log, 'wx');
  try {
    const result = spawnSync(command, args, {
      cwd: repository, env: process.env, stdio: ['ignore', fd, fd],
    });
    if (result.error) throw result.error;
    if (result.status !== 0) {
      throw new Error(`${command} failed: status=${result.status} signal=${result.signal}; see ${log}`);
    }
  } finally {
    fs.closeSync(fd);
  }
};
const buildCommand = [
  'build', '--locked', '--offline', '-p', 'rumoca', '--no-default-features',
  '--example', 'issue_wasm_source_table',
];
run(process.env.CARGO || 'cargo', buildCommand, path.join(output, 'producer-build.log'));
if (sourceManifest() !== before || git('rev-parse', 'HEAD').trim() !== revision) {
  throw new Error('compiler source changed during the issuing build');
}
const executable = path.join(target, 'debug', 'examples',
  `issue_wasm_source_table${process.platform === 'win32' ? '.exe' : ''}`);
const producerBinarySha256 = fileHash(executable);
const sourceDirectory = path.join(repository, 'crates', 'rumoca-exec-wasm', 'tests', 'suite_typed_calls');
const roster = [
  ['OrderedFrameFold.mo', 'NativeOrderedFrameFold', 'fixed_tables', ['baseline', 'gain-two', 'subtract']],
  ['IntegerSeedFold.mo', 'NativeIntegerSeedFold', 'fixed_integer_tables', ['baseline']],
  ['OrderedTensorFrameFold.mo', 'NativeOrderedTensorFrameFold', 'tensor_tables', ['baseline', 'gain-two']],
];
const artifacts = [];
for (const [filename, model, destination, variants] of roster) {
  const original = fs.readFileSync(path.join(sourceDirectory, filename), 'utf8');
  for (const variant of variants) {
    let source = original;
    if (variant !== 'baseline') {
      const [from, to] = variant === 'gain-two'
        ? ['gain = 1.0', 'gain = 2.0']
        : ['total + gain*values', 'total - gain*values'];
      if (source.split(from).length !== 2) {
        throw new Error(`${filename}/${variant}: expected one declared variant site`);
      }
      source = source.replace(from, to);
    }
    const directory = path.join(output, destination, variant);
    fs.mkdirSync(directory, { recursive: true });
    const sourcePath = path.join(directory, filename);
    const artifactPath = path.join(directory, 'issued.json');
    fs.writeFileSync(sourcePath, source);
    const sourceSha256 = sha256(Buffer.from(source));
    run(executable, [sourcePath, model, variant, artifactPath,
      '--source-sha256', sourceSha256,
      '--compiler-revision', revision,
      '--compiler-source-sha256', compilerSourceSha256,
      '--producer-binary-sha256', producerBinarySha256,
    ], path.join(directory, 'producer.log'));
    artifacts.push({
      source: path.relative(output, sourcePath), sourceSha256,
      artifact: path.relative(output, artifactPath), artifactSha256: fileHash(artifactPath),
      fixture: path.relative(repository, path.join(sourceDirectory, destination, `${variant}.json`)),
    });
  }
}
if (sourceManifest() !== before || fileHash(executable) !== producerBinarySha256
    || git('rev-parse', 'HEAD').trim() !== revision) {
  throw new Error('issuing source or producer executable changed during generation');
}
fs.writeFileSync(path.join(output, 'issuance.json'), JSON.stringify({
  compilerRevision: revision, compilerSourceSha256, producerBinarySha256,
  sourceManifestFormat,
  executable, buildCommand,
  buildEnvironment: Object.fromEntries([
    'CARGO_TARGET_DIR', 'CARGO_HOME', 'CARGO_BUILD_JOBS', 'CARGO_INCREMENTAL',
    'CARGO_PROFILE_DEV_DEBUG', 'RUSTFLAGS', 'RUSTUP_TOOLCHAIN',
  ].filter((name) => process.env[name] !== undefined).map((name) => [name, process.env[name]])),
  artifacts,
  qualification: 'UNRUN: artifacts require checked replay and executor tests before fixture replacement',
}, null, 2));
process.stdout.write(`Issued ${artifacts.length} exact Rust-serialized tables into ${output}\n`);
