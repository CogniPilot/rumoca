import { diffsolAvailable as diffsolAddonAvailable, simulateWithDiffsol } from './rumoca_diffsol.js';
import { renderGalecTargetFiles } from './rumoca_galec.js';

const loadedSourceRootCaches = new WeakMap();

function trimMaybeString(value) {
  return typeof value === 'string' ? value.trim() : '';
}

function encodeUrlPath(path) {
  return path.split('/').map(encodeURIComponent).join('/');
}

function normalizeStringArray(values) {
  return Array.isArray(values)
    ? values.map(trimMaybeString).filter(Boolean)
    : [];
}

function sourceRootMatchesEntry(entry, sourceRoot) {
  return Array.isArray(entry?.matches)
    && entry.matches.some((needle) => sourceRoot.includes(needle));
}

export function normalizeSimulationSolver(solver) {
  const normalized = trimMaybeString(solver).toLowerCase();
  return normalized === 'bdf' || normalized === 'rk-like' ? normalized : 'auto';
}

export function createSourceRootCacheResolver({ manifestUrl, fetchImpl = fetch }) {
  let manifestPromise = null;

  async function loadManifest() {
    if (!manifestPromise) {
      manifestPromise = (async () => {
        const response = await fetchImpl(manifestUrl);
        if (!response.ok) {
          throw new Error(`Source-root manifest is unavailable (${response.status})`);
        }
        return { url: manifestUrl, payload: await response.json() };
      })().catch((error) => {
        manifestPromise = null;
        throw error;
      });
    }
    return manifestPromise;
  }

  return {
    async cacheUrlFor(sourceRoots) {
      const roots = normalizeStringArray(sourceRoots);
      if (roots.length === 0) {
        return '';
      }
      const { url, payload } = await loadManifest();
      const entries = Array.isArray(payload?.roots) ? payload.roots : [];
      for (const sourceRoot of roots) {
        if (!entries.some((entry) => sourceRootMatchesEntry(entry, sourceRoot))) {
          throw new Error(`Source root is not staged for this runtime: ${sourceRoot}`);
        }
      }
      const cache = trimMaybeString(payload?.cache);
      if (!cache) {
        throw new Error('Parsed source-root cache is missing from the runtime manifest.');
      }
      return new URL(encodeUrlPath(cache), url).href;
    },
  };
}

export async function fetchGzipBytes(url, fetchImpl = fetch) {
  const response = await fetchImpl(url);
  if (!response.ok) {
    throw new Error(`Parsed source-root cache is unavailable (${response.status})`);
  }
  if (!url.endsWith('.gz')) {
    throw new Error('Parsed source-root cache must be gzip-compressed.');
  }
  if (typeof DecompressionStream !== 'function') {
    throw new Error('This browser cannot decompress the parsed source-root cache.');
  }
  const stream = response.body || new Blob([await response.arrayBuffer()]).stream();
  const decompressed = stream.pipeThrough(new DecompressionStream('gzip'));
  return new Uint8Array(await new Response(decompressed).arrayBuffer());
}

export async function ensureParsedSourceRootCache(wasm, cacheUrl) {
  if (!cacheUrl) {
    return false;
  }
  if (!wasm || typeof wasm.merge_parsed_source_roots_binary !== 'function') {
    throw new Error('merge_parsed_source_roots_binary missing in this WASM build');
  }
  const cached = loadedSourceRootCaches.get(wasm);
  if (cached?.url === cacheUrl) {
    await cached.promise;
    return true;
  }
  const promise = (async () => {
    wasm.merge_parsed_source_roots_binary(await fetchGzipBytes(cacheUrl));
    if (typeof wasm.prime_source_root_completion_cache === 'function') {
      wasm.prime_source_root_completion_cache();
    }
  })();
  loadedSourceRootCaches.set(wasm, { url: cacheUrl, promise });
  try {
    await promise;
  } catch (error) {
    if (loadedSourceRootCaches.get(wasm)?.promise === promise) {
      loadedSourceRootCaches.delete(wasm);
    }
    throw error;
  }
  return true;
}

export async function prepareGpuSimulationWithRuntime({ wasm, source, modelName, sourceRootCacheUrl = '' }) {
  await ensureParsedSourceRootCache(wasm, sourceRootCacheUrl);
  if (typeof wasm.prepare_gpu_simulation !== 'function') {
    throw new Error('prepare_gpu_simulation missing in this WASM build');
  }
  return wasm.prepare_gpu_simulation(source, modelName);
}

export async function simulateModelWithRuntime({
  wasm,
  pkgBase,
  source,
  modelName,
  tEnd = 0,
  dt = 0,
  solver = 'auto',
  sourceRootCacheUrl = '',
  parameterOverrides = {},
}) {
  await ensureParsedSourceRootCache(wasm, sourceRootCacheUrl);
  const normalizedSolver = normalizeSimulationSolver(solver);
  const parameterOverridesJson = JSON.stringify(parameterOverrides || {});
  if (normalizedSolver === 'bdf') {
    return simulateWithDiffsol(wasm, pkgBase, source, modelName, tEnd, dt, parameterOverridesJson);
  }
  if (typeof wasm.simulate_model !== 'function') {
    throw new Error('simulate_model missing in this WASM build');
  }
  return JSON.parse(wasm.simulate_model(source, modelName, tEnd, dt, normalizedSolver, parameterOverridesJson));
}

export async function renderDaeTextWithRuntime({
  wasm,
  source,
  modelName,
  sourceRootCacheUrl = '',
}) {
  await ensureParsedSourceRootCache(wasm, sourceRootCacheUrl);
  const compiled = wasm.compile(source, modelName);
  const envelope = JSON.parse(compiled);
  const daeJson = JSON.stringify(envelope.dae_native || envelope.dae);
  const rendered = wasm.render_target(daeJson, modelName, 'dae-modelica', '', '{}');
  const files = Array.isArray(rendered?.files) ? rendered.files : [];
  return files.map((file) => file.content || '').join('\n');
}

export async function diffsolAvailable(pkgBase) {
  return diffsolAddonAvailable(pkgBase);
}

// Render a GALEC codegen target (`galec` / `galec-production` /
// `embedded-c-galec`) via the separate GALEC addon and return the
// presentation-ready `{ path, content }[]` file list. Mirrors
// `renderDaeTextWithRuntime`, but the GALEC projection needs the flat model —
// so it drives the addon's in-memory compile (which owns the projection)
// instead of the core module's DAE-JSON `render_target` path.
export async function renderGalecFilesWithRuntime({
  pkgBase = './',
  source,
  modelName,
  target,
}) {
  return renderGalecTargetFiles(pkgBase, source, modelName, target);
}
