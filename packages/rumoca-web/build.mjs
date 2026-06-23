#!/usr/bin/env node
import fs from 'node:fs/promises';
import path from 'node:path';
import { createRequire } from 'node:module';
import { fileURLToPath } from 'node:url';
import { build } from 'esbuild';

const __filename = fileURLToPath(import.meta.url);
const webRoot = path.dirname(__filename);
const require = createRequire(import.meta.url);

function parseArgs(argv) {
  let outDir = path.join(webRoot, 'vendor');
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === '--out-dir') {
      outDir = path.resolve(argv[++i] || '');
    } else if (arg === '--help') {
      console.log('Usage: node packages/rumoca-web/build.mjs [--out-dir <dir>]');
      process.exit(0);
    } else {
      throw new Error(`Unknown argument: ${arg}`);
    }
  }
  return { outDir };
}

const { outDir: vendorRoot } = parseArgs(process.argv.slice(2));

const files = [
  ['uplot/dist/uPlot.min.css', 'uplot.min.css'],
];

async function copyPackageFile(packageRelativePath, vendorRelativePath) {
  const src = require.resolve(packageRelativePath);
  const dst = path.join(vendorRoot, vendorRelativePath);
  try {
    await fs.copyFile(src, dst);
  } catch (error) {
    if (error && error.code === 'ENOENT') {
      throw new Error(
        `Missing ${packageRelativePath}; run \`npm --prefix packages/rumoca-web ci\` before staging web vendor assets.`,
      );
    }
    throw error;
  }
}

async function bundleBrowserModule(entryPoint, outfile, options = {}) {
  await build({
    entryPoints: [entryPoint],
    outfile,
    bundle: true,
    format: 'esm',
    platform: 'browser',
    target: 'es2020',
    minify: true,
    legalComments: 'none',
    external: options.external || [],
  });
}

async function bundleStdinModule(contents, outfile, options = {}) {
  await build({
    stdin: {
      contents,
      resolveDir: webRoot,
      sourcefile: options.sourcefile || 'generated-entry.js',
      loader: 'js',
    },
    outfile,
    bundle: true,
    format: options.format || 'esm',
    platform: options.platform || 'browser',
    target: options.target || 'es2020',
    minify: options.minify !== false,
    legalComments: 'none',
    globalName: options.globalName,
  });
}

async function stripSourceMapComments(rootDir) {
  const entries = await fs.readdir(rootDir, { withFileTypes: true });
  for (const entry of entries) {
    const entryPath = path.join(rootDir, entry.name);
    if (entry.isDirectory()) {
      await stripSourceMapComments(entryPath);
      continue;
    }
    if (!entry.isFile() || !['.css', '.js'].includes(path.extname(entry.name))) {
      continue;
    }
    const contents = await fs.readFile(entryPath, 'utf8');
    const stripped = contents.replace(/\n?\/\/# sourceMappingURL=.*\.map\s*$/u, '');
    if (stripped !== contents) {
      await fs.writeFile(entryPath, stripped);
    }
  }
}

async function main() {
  await fs.rm(vendorRoot, { recursive: true, force: true });
  await fs.mkdir(vendorRoot, { recursive: true });
  await fs.writeFile(
    path.join(vendorRoot, 'package.json'),
    `${JSON.stringify({ type: 'module' }, null, 2)}\n`,
  );

  for (const [src, dst] of files) {
    await copyPackageFile(src, dst);
  }

  await fs.copyFile(
    path.join(webRoot, 'viz', 'visualization_shared.js'),
    path.join(vendorRoot, 'visualization_shared.js'),
  );
  await fs.copyFile(
    path.join(webRoot, 'runtime', 'modelica_language.js'),
    path.join(vendorRoot, 'modelica_language.js'),
  );
  await bundleStdinModule(
    `export * from './viz/visualization_shared.js';\n`,
    path.join(vendorRoot, 'visualization_shared.cjs'),
    {
      format: 'cjs',
      platform: 'node',
      minify: false,
      sourcefile: 'visualization-shared-cjs-entry.js',
    },
  );
  await bundleBrowserModule(
    path.join(webRoot, 'viz', 'results_app.js'),
    path.join(vendorRoot, 'results_app.js'),
    { external: ['./visualization_shared.js'] },
  );
  await bundleBrowserModule(
    path.join(webRoot, 'viz', 'markdown_renderer.js'),
    path.join(vendorRoot, 'markdown_renderer.js'),
  );
  await bundleBrowserModule(
    path.join(webRoot, 'viz', 'results_mount.js'),
    path.join(vendorRoot, 'results_report.js'),
  );
  await bundleStdinModule(
    [
      `import { mount } from './viz/results_mount.js';`,
      `globalThis.__rumocaResultsReportMount = mount;`,
    ].join('\n'),
    path.join(vendorRoot, 'results_report_inline.js'),
    { format: 'iife', sourcefile: 'results-report-inline-entry.js' },
  );
  await bundleStdinModule(
    [
      `import uPlot from 'uplot';`,
      `globalThis.uPlot = uPlot;`,
    ].join('\n'),
    path.join(vendorRoot, 'uplot_global.js'),
    { format: 'iife', sourcefile: 'uplot-global-entry.js' },
  );
  await bundleStdinModule(
    [
      `import * as THREE from 'three';`,
      `globalThis.THREE = THREE;`,
    ].join('\n'),
    path.join(vendorRoot, 'three_global.js'),
    { format: 'iife', sourcefile: 'three-global-entry.js' },
  );
  await bundleStdinModule(
    [
      `import JSZip from 'jszip';`,
      `export { JSZip };`,
    ].join('\n'),
    path.join(vendorRoot, 'web_deps.js'),
    { sourcefile: 'web-deps-entry.js' },
  );
  await bundleStdinModule(
    [
      `import * as THREE from 'three';`,
      `import { GLTFLoader } from 'three/examples/jsm/loaders/GLTFLoader.js';`,
      `export * from 'three';`,
      `export { GLTFLoader, THREE };`,
    ].join('\n'),
    path.join(vendorRoot, 'three_viewer.js'),
    { sourcefile: 'three-viewer-entry.js' },
  );
  await fs.copyFile(
    path.join(webRoot, 'viz', 'results_app.css'),
    path.join(vendorRoot, 'results_app.css'),
  );

  const monacoVsDir = path.join(vendorRoot, 'monaco', 'vs');
  await fs.cp(
    path.dirname(require.resolve('monaco-editor/min/vs/loader.js')),
    monacoVsDir,
    { recursive: true },
  );
  await stripSourceMapComments(monacoVsDir);
}

main().catch((error) => {
  console.error(error && error.message ? error.message : error);
  process.exit(1);
});
