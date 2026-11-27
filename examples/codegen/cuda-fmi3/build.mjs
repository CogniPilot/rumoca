// Compile the generated FMI step, including its solver and event iteration, for CUDA.
import {execFileSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {mkdirSync, readFileSync, writeFileSync} from 'node:fs';
import {dirname, join, resolve} from 'node:path';
import {fileURLToPath} from 'node:url';
import {lowerBulkCopies} from './bulk-copy.mjs';
import {lowerLaneWorkspace} from './arena.mjs';
import {checkNumericInstance} from './layout.mjs';

const [fmu, output, option] = process.argv.slice(2);
const blockCopies = option === '--block-copies';
if (option && !blockCopies) throw Error('Unknown CUDA build option: ' + option);
const kernel = blockCopies ? 'rmc_fmi3_step_block' : 'rmc_fmi3_step';
if (!fmu || !output) throw Error('Usage: node build.mjs EXTRACTED_FMU OUTPUT_DIRECTORY');

const here = dirname(fileURLToPath(import.meta.url));
const source = resolve(fmu, 'sources');
const out = resolve(output);
const xml = readFileSync(resolve(fmu, 'modelDescription.xml'), 'utf8');
if (!xml.includes('<CoSimulation') || !xml.includes('fmiVersion="3.')) {
  throw Error('Expected an FMI 3 co-simulation component.');
}
if (/<(?:String|Binary)\s/.test(xml)) throw Error('CUDA export requires numeric FMI variables.');
const token = xml.match(/instantiationToken="([^"]+)"/)?.[1];
if (!token) throw Error('Missing FMI instantiation token.');

const {LLVM_BIN, CUDA_LIBDEVICE, FMI_INCLUDE} = process.env;
if (!LLVM_BIN || !CUDA_LIBDEVICE || !FMI_INCLUDE) {
  throw Error('Set LLVM_BIN, CUDA_LIBDEVICE and FMI_INCLUDE.');
}
const clang = process.env.CLANG ?? 'clang';
const arch = process.env.CUDA_ARCH ?? 'sm_86';
const optimization = process.env.PTXAS_OPT_LEVEL ?? '3';
if (!/^[0-3]$/.test(optimization)) throw Error('PTXAS_OPT_LEVEL must be 0, 1, 2, or 3.');
const run = (executable, ...args) => execFileSync(executable, args, {stdio: 'inherit'});
const artifact = name => join(out, name);
mkdirSync(out, {recursive: true});

const deviceFlags = ['--target=nvptx64-nvidia-cuda', '-march=' + arch, '-x', 'c', '-std=c11',
  '-DNDEBUG', '-DRMC_FMI3_INLINE_FACTORS', ...(blockCopies ? ['-DRMC_CUDA_BLOCK_COPIES'] : []), '-ffreestanding',
  '-I' + join(here, 'freestanding'), '-I' + source, '-I' + FMI_INCLUDE];
const layout = execFileSync(clang, [...deviceFlags, '-Xclang', '-ast-dump=json', '-fsyntax-only',
  join(source, 'model.h')], {encoding: 'utf8', maxBuffer: 128 * 1024 * 1024});
checkNumericInstance(JSON.parse(layout));

const entryFile = artifact('rmc_cuda_step.c');
writeFileSync(entryFile, readFileSync(join(source, 'rmc_cuda_step.c.inc')));
const units = ['rmc_assign', 'rmc_functions', 'rmc_rows', 'rmc_isolators', 'rmc_jacobian'];
const sources = [entryFile, ...units.map(unit => join(source, unit + '.c')), join(here, 'support.c')];
const intermediates = sources.map((path, index) => {
  const destination = artifact('unit-' + index + '.ll');
  run(clang, ...deviceFlags, '-O2', '-ffp-contract=off', '-fno-math-errno',
      '-S', '-emit-llvm', path, '-o', destination);
  return destination;
});

const entry = readFileSync(intermediates[0], 'utf8');
const marked = entry.replace(new RegExp('^define [^\\n@]*void @' + kernel + '\\(', 'm'), 'define ptx_kernel void @' + kernel + '(');
if (marked === entry) throw Error('No FMI CUDA entry in generated source.');
writeFileSync(intermediates[0], marked);

run(join(LLVM_BIN, 'llvm-link'), ...intermediates, CUDA_LIBDEVICE, '-o', artifact('linked.bc'));
run(join(LLVM_BIN, 'opt'), '-passes=internalize,globaldce,default<O2>',
    '-internalize-public-api-list=' + kernel + ',rmc_fmi3_token,rmc_fmi3_instance_bytes',
    artifact('linked.bc'), '-o', artifact('optimized.bc'));
run(join(LLVM_BIN, 'llvm-dis'), artifact('optimized.bc'), '-o', artifact('optimized.ll'));
const workspace = lowerLaneWorkspace(readFileSync(artifact('optimized.ll'), 'utf8'), blockCopies);
writeFileSync(artifact('arena.ll'), lowerBulkCopies(workspace.ir));
run(clang, '--target=nvptx64-nvidia-cuda', '-march=' + arch, '-O2', '-ffreestanding',
    ...(blockCopies ? ['-DRMC_CUDA_BLOCK_COPIES'] : []), '-emit-llvm', '-c', join(here, 'bulk-copy.c'), '-o', artifact('bulk-copy.bc'));
const copyModules = [artifact('bulk-copy.bc')];
if (blockCopies) {
  run(clang, '--target=nvptx64-nvidia-cuda', '-march=' + arch, '-O2', '-ffreestanding',
      '-emit-llvm', '-c', join(here, 'block-copy.c'), '-o', artifact('block-copy.bc'));
  copyModules.push(artifact('block-copy.bc'));
}
run(join(LLVM_BIN, 'llvm-link'), artifact('arena.ll'), ...copyModules, '-o', artifact('arena.bc'));
run(join(LLVM_BIN, 'llc'), '-O2', '-mtriple=nvptx64-nvidia-cuda', '-mcpu=' + arch, '-mattr=+ptx80',
    artifact('arena.bc'), '-o', artifact('ensemble.ptx'));
run(process.env.PTXAS ?? 'ptxas', '-O' + optimization, '-v', '-arch=' + arch,
    artifact('ensemble.ptx'), '-o', artifact('ensemble.cubin'));
const sha256 = createHash('sha256').update(readFileSync(artifact('ensemble.cubin'))).digest('hex');
writeFileSync(artifact('ensemble.json'), JSON.stringify({
  hostDefines: ['RMC_FMI3_INLINE_FACTORS'],
  token, arch, sha256, kernel, ...(blockCopies ? {blocksPerInstance: 1, threadsPerBlock: 33} : {}), workspacePointer: 'rmc_scratch', workspaceStride: 'rmc_stride',
  functionsWithWorkspace: workspace.functions, maxFrame: workspace.maxFrame, ptxasOptimization: Number(optimization),
}, null, 2) + '\n');
