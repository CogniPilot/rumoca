#!/usr/bin/env node
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';

const root = process.cwd();
// During the staged VS Code smoke build, `cargo xtask` runs this from a temp
// copy and leaves the real repo root in a marker file (argv can't thread
// through the nested npm scripts). Fall back to the in-repo layout otherwise.
const repoRootMarker = path.join(root, '.rumoca-smoke-repo-root');
const repoRoot = fs.existsSync(repoRootMarker)
  ? path.resolve(fs.readFileSync(repoRootMarker, 'utf8').trim())
  : path.resolve(root, '..', '..');
const outDir = path.join(root, 'media', 'vendor');
const webRoot = path.join(repoRoot, 'packages', 'rumoca-web');
const iconSrc = path.join(repoRoot, 'assets', 'brand', 'rumoca.png');
const iconDst = path.join(root, 'icons', 'rumoca.png');

const assets = [
  {
    src: path.join(webRoot, 'vendor', 'package.json'),
    dst: path.join(outDir, 'package.json'),
  },
  {
    src: path.join(webRoot, 'vendor', 'uplot.min.css'),
    dst: path.join(outDir, 'uplot.min.css'),
  },
  {
    src: path.join(webRoot, 'vendor', 'results_app.css'),
    dst: path.join(outDir, 'results_app.css'),
  },
  {
    src: path.join(webRoot, 'vendor', 'visualization_shared.js'),
    dst: path.join(outDir, 'visualization_shared.js'),
  },
  {
    src: path.join(webRoot, 'vendor', 'visualization_shared.cjs'),
    dst: path.join(outDir, 'visualization_shared.cjs'),
  },
  {
    src: path.join(webRoot, 'vendor', 'results_app.js'),
    dst: path.join(outDir, 'results_app.js'),
  },
];

function runWebCommand(args) {
  const result = spawnSync('npm', args, { cwd: webRoot, stdio: 'inherit' });
  if (result.status !== 0) {
    process.exit(result.status || 1);
  }
}

function fileMtimeMs(file) {
  return fs.statSync(file).mtimeMs;
}

function listFilesRecursive(dir) {
  if (!fs.existsSync(dir)) {
    return [];
  }
  const files = [];
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const entryPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      files.push(...listFilesRecursive(entryPath));
    } else if (entry.isFile()) {
      files.push(entryPath);
    }
  }
  return files;
}

function dependencyManifestFiles() {
  return [
    path.join(webRoot, 'package.json'),
    path.join(webRoot, 'package-lock.json'),
  ];
}

function webSourceFiles() {
  return [
    ...dependencyManifestFiles(),
    path.join(webRoot, 'build.mjs'),
    ...listFilesRecursive(path.join(webRoot, 'runtime')),
    ...listFilesRecursive(path.join(webRoot, 'viz')),
  ];
}

function webVendorAssetsFresh() {
  if (!assets.every((asset) => fs.existsSync(asset.src))) {
    return false;
  }
  const newestSource = Math.max(...webSourceFiles().map(fileMtimeMs));
  const oldestVendor = Math.min(...assets.map((asset) => fileMtimeMs(asset.src)));
  return oldestVendor >= newestSource;
}

function webDependenciesFresh() {
  const nodeModulesLock = path.join(webRoot, 'node_modules', '.package-lock.json');
  const esbuildPackage = path.join(webRoot, 'node_modules', 'esbuild', 'package.json');
  if (!fs.existsSync(nodeModulesLock) || !fs.existsSync(esbuildPackage)) {
    return false;
  }
  const newestManifest = Math.max(...dependencyManifestFiles().map(fileMtimeMs));
  return fileMtimeMs(nodeModulesLock) >= newestManifest;
}

if (!webVendorAssetsFresh()) {
  if (!webDependenciesFresh()) {
    runWebCommand(['ci']);
  }
  runWebCommand(['run', 'build']);
}

fs.rmSync(outDir, { recursive: true, force: true });
fs.mkdirSync(outDir, { recursive: true });
fs.mkdirSync(path.dirname(iconDst), { recursive: true });
fs.copyFileSync(iconSrc, iconDst);

for (const asset of assets) {
  if (!fs.existsSync(asset.src)) {
    console.error(`Missing webview asset: ${path.relative(root, asset.src)}`);
    process.exit(1);
  }
  fs.copyFileSync(asset.src, asset.dst);
}

console.log('Vendored webview assets into media/vendor');
