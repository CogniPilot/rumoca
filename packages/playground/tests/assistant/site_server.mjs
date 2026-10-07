import http from 'node:http';
import fs from 'node:fs/promises';
import path from 'node:path';

// Serves the playground with the same layout as the Pages site and the
// `cargo xtask playground edit` server (see crates/xtask/src/static_server.rs),
// including the loopback `/callback` hand-off used by Sign in with ChatGPT.

const MIME = {
  '.html': 'text/html', '.js': 'text/javascript', '.mjs': 'text/javascript', '.css': 'text/css',
  '.json': 'application/json', '.wasm': 'application/wasm', '.svg': 'image/svg+xml',
  '.mo': 'text/plain', '.toml': 'text/plain',
};

function resolvePages(root, distDir, relative) {
  if (relative === '' || relative === 'index.html') return path.join(root, 'packages/playground/index.html');
  const [head, ...rest] = relative.split('/');
  const tail = rest.join('/');
  const bases = {
    src: path.join(root, 'packages/playground/src'),
    vendor: path.join(root, 'packages/playground/vendor'),
    pkg: distDir,
    assets: path.join(root, 'assets'),
    examples: path.join(root, 'examples'),
  };
  if (relative === 'coi-serviceworker.js') return path.join(root, 'packages/playground/coi-serviceworker.js');
  return bases[head] ? path.join(bases[head], tail) : path.join(root, relative);
}

export async function startSiteServer({ root, distDir, pkgSubdir = 'release-full-web', host = '127.0.0.1' }) {
  const server = http.createServer(async (request, response) => {
    const url = new URL(request.url, 'http://x');
    const headers = {
      'Cross-Origin-Opener-Policy': 'same-origin',
      'Cross-Origin-Embedder-Policy': 'require-corp',
      'Access-Control-Allow-Origin': '*',
      'Cache-Control': 'no-store',
    };
    if (url.pathname === '/callback') {
      response.writeHead(302, { ...headers, Location: `/rumoca/?${url.searchParams}` });
      response.end();
      return;
    }
    if (!url.pathname.startsWith('/rumoca')) {
      response.writeHead(404, headers);
      response.end('not found');
      return;
    }
    const relative = url.pathname.replace(/^\/rumoca\/?/u, '');
    let file = resolvePages(root, distDir, relative);
    try {
      if ((await fs.stat(file)).isDirectory()) file = path.join(file, 'index.html');
      let body = await fs.readFile(file);
      if (file.endsWith('packages/playground/index.html')) {
        body = Buffer.from(body.toString()
          .replace("window.rumocaRepoAssetBase = '../../';", "window.rumocaRepoAssetBase = './';")
          .replace("window.rumocaWasmPkgBase = '../rumoca/dist';", "window.rumocaWasmPkgBase = './pkg';")
          .replace("window.rumocaWasmPkgSubdir = 'release-full-web';", `window.rumocaWasmPkgSubdir = '${pkgSubdir}';`)
          .replaceAll('../../assets/brand/rumoca.svg', 'assets/brand/rumoca.svg'));
      }
      response.writeHead(200, { ...headers, 'Content-Type': MIME[path.extname(file)] ?? 'application/octet-stream' });
      response.end(body);
    } catch {
      response.writeHead(404, headers);
      response.end('not found');
    }
  });
  await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
  return { port: server.address().port, close: () => new Promise((resolve) => server.close(resolve)) };
}
