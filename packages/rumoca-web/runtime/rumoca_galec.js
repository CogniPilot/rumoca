// Lazy GALEC / eFMI codegen driver for the @cognipilot/rumoca package.
//
// Why this exists: the main rumoca WASM module (Modelica / template /
// simulation workflows) must stay small and universal. The GALEC → eFMI
// Algorithm Code (.alg) + GALEC-derived embedded C projection ships as a
// *separate* module (`rumoca_bind_wasm_galec.js`) that is imported only when a
// user actually selects a GALEC codegen target. This mirrors the lazy diffsol
// addon (`rumoca_diffsol.js`) and keeps the core module untouched.
//
// Usage (the main module does NOT need to be initialized first — the addon
// carries its own in-memory compile + projection):
//   import { renderGalecTargetFiles } from "@cognipilot/rumoca/galec";
//   const workspaceSources = JSON.stringify({ "Model.mo": src });
//   const files = await renderGalecTargetFiles(pkgBase, workspaceSources, model, "galec");
//
// `pkgBase` is the same base URL the main `rumoca_bind_wasm.js` was imported
// from (the addon wasm sits next to it); pass "./" from a worker co-located
// with the package.

/** The three GALEC codegen targets served by the addon (all ir = "dae"). */
export const GALEC_TARGETS = Object.freeze([
  "galec",
  "efmi",
  "galec-c",
]);

/** True iff `target` is one of the GALEC codegen targets. */
export function isGalecTarget(target) {
  return GALEC_TARGETS.includes(String(target || ""));
}

let addonPromise = null;

/**
 * Lazily import + initialize the GALEC addon module from `pkgBase`. Resolves to
 * the module's exports (with `render_galec`), or rejects on a load failure.
 * Cached after the first successful call; a failed load is retryable.
 */
export function loadGalecAddon(pkgBase = "./") {
  if (!addonPromise) {
    addonPromise = (async () => {
      const mod = await import(pkgBase + "rumoca_bind_wasm_galec.js");
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

async function requireGalecAddon(pkgBase) {
  try {
    return await loadGalecAddon(pkgBase);
  } catch (err) {
    throw new Error(
      "the GALEC / eFMI codegen addon could not be loaded; rebuild the " +
        `package (cargo xtask playground build). (${err && err.message ? err.message : err})`,
    );
  }
}

function parseAddonJson(text, context) {
  try {
    return JSON.parse(text);
  } catch (err) {
    throw new Error(
      `${context} returned invalid JSON: ${err && err.message ? err.message : err}`,
    );
  }
}

/**
 * Compile the workspace sources in the addon and project the model to
 * `target`, returning the parsed success payload
 * `{ ok, target, model_identifier, alg, c_header, c_source }`.
 *
 * `workspaceSources` is a JSON object string mapping each document path to its
 * Modelica text — the same map the core compile uses — so a model spanning
 * several files projects to GALEC just as it compiles for other targets.
 *
 * Throws with the addon's diagnostic message when the model is inadmissible
 * (e.g. continuous dynamics) or the target is unknown, and when this rumoca
 * build predates the addon.
 */
export async function renderGalec(pkgBase, workspaceSources, modelName, target) {
  if (!isGalecTarget(target)) {
    throw new Error(
      `'${target}' is not a GALEC codegen target ` +
        `(expected one of ${GALEC_TARGETS.join(", ")})`,
    );
  }
  const addon = await requireGalecAddon(pkgBase);
  if (typeof addon.render_galec !== "function") {
    throw new Error(
      "this rumoca build predates the GALEC addon; rebuild the package",
    );
  }
  const parsed = parseAddonJson(
    addon.render_galec(workspaceSources, modelName, target),
    "render_galec",
  );
  if (!parsed || parsed.ok !== true) {
    throw new Error((parsed && parsed.error) || "GALEC code generation failed");
  }
  return parsed;
}

async function callGalecLanguageService(pkgBase, exportName, args) {
  const addon = await requireGalecAddon(pkgBase);
  if (typeof addon[exportName] !== "function") {
    throw new Error(
      `this rumoca build predates ${exportName}; rebuild the package`,
    );
  }
  return parseAddonJson(addon[exportName](...args), exportName);
}

/** Compute GALEC `.alg` diagnostics via the lazy addon LSP surface. */
export async function galecDiagnostics(pkgBase, source, fileName = "generated.alg") {
  const diagnostics = await callGalecLanguageService(pkgBase, "galec_diagnostics", [
    String(source ?? ""),
    String(fileName || "generated.alg"),
  ]);
  return Array.isArray(diagnostics) ? diagnostics : [];
}

/** Return GALEC hover information for a zero-based LSP position, or `null`. */
export async function galecHover(
  pkgBase,
  source,
  fileName,
  line,
  character,
) {
  return callGalecLanguageService(pkgBase, "galec_hover", [
    String(source ?? ""),
    String(fileName || "generated.alg"),
    Number(line) || 0,
    Number(character) || 0,
  ]);
}

/** Return a GALEC definition location for a zero-based LSP position, or `null`. */
export async function galecDefinition(
  pkgBase,
  source,
  fileName,
  uri,
  line,
  character,
) {
  return callGalecLanguageService(pkgBase, "galec_definition", [
    String(source ?? ""),
    String(fileName || "generated.alg"),
    String(uri || "file:///generated.alg"),
    Number(line) || 0,
    Number(character) || 0,
  ]);
}

function galecCResultToFiles(result) {
  const base = String(result?.model_identifier || "model");
  return [
    { path: `${base}.h`, content: String(result?.c_header ?? "") },
    { path: `${base}.c`, content: String(result?.c_source ?? "") },
  ];
}

/**
 * Parse edited GALEC `.alg` text and render the derived C header/source.
 * Unlike `renderGalecTargetFiles`, this path does not recompile Modelica:
 * the current `.alg` editor contents are the source of truth.
 */
export async function renderGalecCFromAlg(
  pkgBase,
  algSource,
  fileName,
  modelName,
  target = "galec-c",
) {
  const addon = await requireGalecAddon(pkgBase);
  if (typeof addon.render_galec_c_from_alg !== "function") {
    throw new Error(
      "this rumoca build predates render_galec_c_from_alg; rebuild the package",
    );
  }
  const parsed = parseAddonJson(
    addon.render_galec_c_from_alg(
      String(algSource ?? ""),
      String(fileName || "generated.alg"),
      String(modelName || "model"),
      String(target || "galec-c"),
    ),
    "render_galec_c_from_alg",
  );
  if (!parsed || parsed.ok !== true) {
    throw new Error((parsed && parsed.error) || "GALEC-to-C generation failed");
  }
  return galecCResultToFiles(parsed);
}

/**
 * Shape a GALEC addon result into the `{ path, content }[]` file list used by
 * the codegen presentation (the same shape `render_target` returns), so the
 * generated artifacts can be written and inspected like any other target.
 *
 * The `.alg` (eFMI Algorithm Code) is always produced; the two C tracks add the
 * `.h` header and `.c` source. Paths are flat leaf names — the identity-free
 * addon does not mint the eFMU container (manifests / __content.xml / SHA-1
 * checksums), so it would be dishonest to wrap these in an AlgorithmCode/…
 * container layout; that packaging is the native CLI's job.
 */
export function galecResultToFiles(target, result) {
  // Name the files with the SAME identifier the addon used (dots -> underscores)
  // so the generated `#include "<id>.h"` resolves. Using the bare model leaf
  // would break C compilation for a package-qualified model (`MyLib.Demo` emits
  // `#include "MyLib_Demo.h"`, not `Demo.h`). The addon always supplies
  // `model_identifier`.
  const base = String(result?.model_identifier || "model");
  const files = [{ path: `${base}.alg`, content: String(result?.alg ?? "") }];
  if (target === "efmi" || target === "galec-c") {
    files.push({ path: `${base}.h`, content: String(result?.c_header ?? "") });
    files.push({ path: `${base}.c`, content: String(result?.c_source ?? "") });
  }
  return files;
}

/**
 * Convenience: render a GALEC target and return the presentation-ready file
 * list. Mirrors `renderDaeTextWithRuntime` but drives the separate addon
 * (never the core module's DAE-JSON `render_target` path, which drops the flat
 * model the projection needs).
 */
export async function renderGalecTargetFiles(
  pkgBase,
  workspaceSources,
  modelName,
  target,
) {
  const parsed = await renderGalec(pkgBase, workspaceSources, modelName, target);
  return galecResultToFiles(target, parsed);
}
