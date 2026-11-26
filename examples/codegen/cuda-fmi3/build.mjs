// Compile the generated FMI step, including its solver and event iteration, for CUDA.
import {execFileSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {mkdirSync, readFileSync, writeFileSync} from 'node:fs';
import {dirname, join, resolve} from 'node:path';
import {fileURLToPath} from 'node:url';
import {lowerLaneWorkspace} from './arena.mjs';

const [fmu, output] = process.argv.slice(2);
if (!fmu || !output) throw Error('Usage: node build.mjs EXTRACTED_FMU OUTPUT_DIRECTORY');

const here = dirname(fileURLToPath(import.meta.url));
const source = resolve(fmu, 'sources');
const out = resolve(output);
const xml = readFileSync(resolve(fmu, 'modelDescription.xml'), 'utf8');
if (!xml.includes('<CoSimulation') || !xml.includes('fmiVersion="3.')) {
  throw Error('Expected an FMI 3 co-simulation component.');
}
const header = readFileSync(join(source, 'model.h'), 'utf8');
const layout = header.match(/typedef struct \{ bool assertions_initialized;([\s\S]*?)\} ModelInstance;/)?.[1];
if (!layout || layout.includes('*') || /<(?:String|Binary)\s/.test(xml)) {
  throw Error('CUDA lanes require a numeric component with inline state and no host-owned storage.');
}
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

const entryFile = artifact('rmc_cuda_step.c');
writeFileSync(entryFile, readFileSync(join(source, 'rmc_cuda_step.c.inc')));
const units = ['rmc_assign', 'rmc_functions', 'rmc_rows', 'rmc_isolators', 'rmc_jacobian'];
const sources = [entryFile, ...units.map(unit => join(source, unit + '.c')), join(here, 'support.c')];
const intermediates = sources.map((path, index) => {
  const destination = artifact('unit-' + index + '.ll');
  run(clang, '--target=nvptx64-nvidia-cuda', '-march=' + arch, '-x', 'c', '-std=c11', '-DNDEBUG',
      '-O2', '-ffp-contract=off', '-ffreestanding', '-fno-math-errno',
      '-I' + join(here, 'freestanding'), '-I' + source, '-I' + FMI_INCLUDE,
      '-S', '-emit-llvm', path, '-o', destination);
  return destination;
});

const entry = readFileSync(intermediates[0], 'utf8');
const marked = entry.replace(/^define [^\n@]*void @rmc_fmi3_step/m, 'define ptx_kernel void @rmc_fmi3_step');
if (marked === entry) throw Error('No FMI CUDA entry in generated source.');
writeFileSync(intermediates[0], marked);

run(join(LLVM_BIN, 'llvm-link'), ...intermediates, CUDA_LIBDEVICE, '-o', artifact('linked.bc'));
run(join(LLVM_BIN, 'opt'), '-passes=internalize,globaldce,default<O2>',
    '-internalize-public-api-list=rmc_fmi3_step,rmc_fmi3_token,rmc_fmi3_instance_bytes',
    artifact('linked.bc'), '-o', artifact('optimized.bc'));
run(join(LLVM_BIN, 'llvm-dis'), artifact('optimized.bc'), '-o', artifact('optimized.ll'));
const workspace = lowerLaneWorkspace(readFileSync(artifact('optimized.ll'), 'utf8'));
writeFileSync(artifact('arena.ll'), workspace.ir);
run(join(LLVM_BIN, 'llc'), '-O2', '-mtriple=nvptx64-nvidia-cuda', '-mcpu=' + arch, '-mattr=+ptx80',
    artifact('arena.ll'), '-o', artifact('ensemble.ptx'));
run(process.env.PTXAS ?? 'ptxas', '-O' + optimization, '-v', '-arch=' + arch,
    artifact('ensemble.ptx'), '-o', artifact('ensemble.cubin'));
const sha256 = createHash('sha256').update(readFileSync(artifact('ensemble.cubin'))).digest('hex');
writeFileSync(artifact('ensemble.json'), JSON.stringify({
  token, arch, sha256, kernel: 'rmc_fmi3_step', workspacePointer: 'rmc_scratch', workspaceStride: 'rmc_stride',
  functionsWithWorkspace: workspace.functions, maxFrame: workspace.maxFrame, ptxasOptimization: Number(optimization),
}, null, 2) + '\n');
