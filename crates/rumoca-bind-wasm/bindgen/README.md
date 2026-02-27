# Rumoca WebAssembly (WASM) npm helpers

This directory contains npm helper scripts for building and publishing Rumoca's
WASM package from the `rumoca-bind-wasm` crate.

WASM bindings source crate (current directory parent):
- `crates/rumoca-bind-wasm`

Generated npm package output:
- `pkg/`

## Build and Pack

```sh
cd crates/rumoca-bind-wasm/bindgen
npm run build
```

This runs:
1. `wasm-pack build .. --release --target web --out-dir ../../../pkg`
2. Patches `pkg/package.json` via `patch-wasm-pkg.mjs`
3. Creates a local tarball via `npm pack ../../../pkg`

## Publish

```sh
cd crates/rumoca-bind-wasm/bindgen
npm run publish
```

This rebuilds and then publishes from `pkg/`.
