#!/usr/bin/env node
import fs from 'node:fs';
import path from 'node:path';

const root = process.cwd();
const repoRoot = process.env.RUMOCA_REPO_ROOT
  ? path.resolve(process.env.RUMOCA_REPO_ROOT)
  : path.resolve(root, '..', '..');
const outDir = path.join(root, 'media', 'vendor');

const assets = [
  {
    src: path.join(root, 'node_modules', 'uplot', 'dist', 'uPlot.min.css'),
    dst: path.join(outDir, 'uPlot.min.css'),
  },
  {
    src: path.join(root, 'node_modules', 'uplot', 'dist', 'uPlot.iife.min.js'),
    dst: path.join(outDir, 'uPlot.iife.min.js'),
  },
  {
    src: path.join(root, 'node_modules', 'three', 'build', 'three.min.js'),
    dst: path.join(outDir, 'three.min.js'),
  },
  {
    src: path.join(repoRoot, 'crates', 'rumoca-viz-web', 'web', 'visualization_shared.js'),
    dst: path.join(outDir, 'visualization_shared.js'),
  },
  {
    src: path.join(repoRoot, 'crates', 'rumoca-viz-web', 'web', 'results_app.js'),
    dst: path.join(outDir, 'results_app.js'),
  },
  {
    src: path.join(repoRoot, 'crates', 'rumoca-viz-web', 'web', 'results_app.css'),
    dst: path.join(outDir, 'results_app.css'),
  },
];

fs.mkdirSync(outDir, { recursive: true });

for (const asset of assets) {
  if (!fs.existsSync(asset.src)) {
    // Newer uplot package layouts may omit the prebuilt IIFE bundle.
    // Keep an existing vendored file in that case so local dev can continue.
    if (fs.existsSync(asset.dst)) {
      console.warn(
        `Missing source asset (keeping existing vendored file): ${path.relative(root, asset.src)}`,
      );
      continue;
    }
    console.error(`Missing webview asset: ${path.relative(root, asset.src)}`);
    process.exit(1);
  }
  fs.copyFileSync(asset.src, asset.dst);
}

console.log('Vendored webview assets into media/vendor');
