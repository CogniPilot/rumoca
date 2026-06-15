// Lazy diffsol (stiff/implicit) simulation driver for the @cognipilot/rumoca
// package.
//
// Why this exists: the main rumoca WASM module is SIMD-free and loads on every
// browser. The diffsol solver pulls relaxed-SIMD (via faer/pulp), and WASM
// validates a whole module at instantiation — so a single combined module
// would hard-fail the entire package on browsers without relaxed-SIMD. Instead
// the diffsol code ships as a *separate* tiny module (~0.4 MB gz,
// `rumoca_bind_wasm_diffsol.js`) that is loaded only when (a) the browser
// supports relaxed-SIMD and (b) the user actually runs a stiff solve.
//
// Usage (the main module is already initialized by the caller):
//   import { diffsolAvailable, simulateWithDiffsol } from "@cognipilot/rumoca/diffsol";
//   const ok = await diffsolAvailable(pkgBase);          // grey out the UI if false
//   const result = await simulateWithDiffsol(mainModule, pkgBase, src, model, tEnd, dt);
//
// `pkgBase` is the same base URL the main `rumoca_bind_wasm.js` was imported
// from (the addon wasm sits next to it).

// Minimal module that uses a 1-operand relaxed-SIMD op
// (`i32x4.relaxed_trunc_f32x4_s`, 0xFD 0x101). `WebAssembly.validate` returns
// false on engines without relaxed-SIMD. This is a cheap *hint* for greying out
// the UI; the authoritative check is whether the addon actually instantiates
// (see `diffsolAvailable`). NOTE: verify in a real browser as part of the
// editor smoke test.
const RELAXED_SIMD_PROBE = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, // \0asm v1
  0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7b,       // type: () -> v128
  0x03, 0x02, 0x01, 0x00,                         // func 0 : type 0
  0x0a, 0x19, 0x01, 0x17, 0x00,                   // code: 1 body, size 23, 0 locals
  0xfd, 0x0c, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // v128.const 0
  0xfd, 0x81, 0x02,                               // i32x4.relaxed_trunc_f32x4_s
  0x0b,                                           // end
]);

/** Quick, non-authoritative hint: does this engine validate relaxed-SIMD? */
export function relaxedSimdSupported() {
  try {
    return WebAssembly.validate(RELAXED_SIMD_PROBE);
  } catch {
    return false;
  }
}

let addonPromise = null;

/**
 * Lazily import + initialize the diffsol addon module from `pkgBase`. Resolves
 * to the module's exports, or rejects (e.g. `CompileError` on a browser without
 * relaxed-SIMD). Cached after the first call.
 */
export function loadDiffsolAddon(pkgBase) {
  if (!addonPromise) {
    addonPromise = (async () => {
      const mod = await import(pkgBase + "rumoca_bind_wasm_diffsol.js");
      await mod.default(); // run wasm-bindgen init (instantiates the wasm)
      return mod;
    })();
    // Allow a retry if loading failed (e.g. transient network).
    addonPromise.catch(() => {
      addonPromise = null;
    });
  }
  return addonPromise;
}

/**
 * Authoritative check: true iff the diffsol addon actually instantiates in this
 * browser. Downloads/compiles the addon once (cached). Use this to enable or
 * grey out the stiff-solver options.
 */
export async function diffsolAvailable(pkgBase) {
  try {
    await loadDiffsolAddon(pkgBase);
    return true;
  } catch {
    return false;
  }
}

/**
 * Run a stiff (diffsol) simulation: the main module lowers the model to a
 * SolveModel JSON, the addon simulates it. Returns the parsed
 * `{ payload, ... }` object (same shape as the main module's `simulate_model`).
 * Throws a clear error if the addon can't load (old browser) — callers should
 * fall back to rk45 / the GPU path or surface the message.
 */
export async function simulateWithDiffsol(mainModule, pkgBase, source, modelName, tEnd, dt) {
  if (typeof mainModule.lower_model_to_solve_json !== "function") {
    throw new Error(
      "this rumoca build predates the diffsol addon; rebuild the package",
    );
  }
  let addon;
  try {
    addon = await loadDiffsolAddon(pkgBase);
  } catch (err) {
    throw new Error(
      "the stiff (diffsol) solver needs a browser with WebAssembly relaxed-SIMD " +
        "(Chrome 114+, Firefox 120+, Safari 16.4+). Use the rk45 solver or the GPU " +
        `path instead. (${err && err.message ? err.message : err})`,
    );
  }
  // The payload carries the lowered model plus the resolved t_end/dt (so an
  // annotation-driven duration is honored even when the caller passes 0).
  const prepJson = mainModule.lower_model_to_solve_json(source, modelName, tEnd, dt);
  const resultJson = addon.simulate_solve_model_diffsol(prepJson);
  return JSON.parse(resultJson);
}
