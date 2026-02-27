# Rumoca WebAssembly (WASM)

This directory contains npm helper scripts for building and publishing Rumoca's
WASM package from the `rumoca-bind-wasm` crate.

## Usage Example (Browser / ESM)

```javascript
import init, { compile, render_template } from 'rumoca-bind-wasm'

await init()

const modelica = `
  model Test
    Real x(start=0);
  equation
    der(x) = 1;
  end Test;
`

// Compile to DAE IR JSON
const result = compile(modelica, 'Test')
console.log(result)

// Render custom template
const template = 'Model: {{ dae.model_name }}'
const output = render_template(modelica, 'Test', template)
console.log(output)
```

## Building

### 1. npm-based workflow (recommended for npm publish)

This workflow builds the `rumoca-bind-wasm` crate, patches `pkg/package.json`
to match current artifacts, and packs/publishes from `pkg/`.

```sh
# Build and pack the npm package
cd crates/rumoca-bind-wasm/bindgen && npm run build

# Publish the package to npm
cd crates/rumoca-bind-wasm/bindgen && npm run publish
```

### 2. Rust-based workflow (pure WASM via `wasm-pack`)

You can also build the WASM artifacts directly with `wasm-pack`, targeting different environments:

```sh
# For the web (native ESM in browsers), output to repo pkg/
wasm-pack build crates/rumoca-bind-wasm --release --target web --out-dir pkg

# For bundlers (e.g., webpack, Rollup, Vite in bundler mode)
wasm-pack build crates/rumoca-bind-wasm --release --target bundler --out-dir pkg

# For Node.js
wasm-pack build crates/rumoca-bind-wasm --release --target nodejs --out-dir pkg
```

## Notes

- For browser usage with Vite, the `web` target generally works best.
- To use `rumoca-bind-wasm` with the Vite bundler, you likely need
  [`vite-plugin-wasm`](https://www.npmjs.com/package/vite-plugin-wasm) plugin.
- If you use `rumoca-dev-tools wasm-test build`, output naming is normalized to
  `rumoca.js` / `rumoca_bg.wasm`; the `bindgen` patch script handles this.

## Debug Build

To debug Rumoca in the browser, build a development WASM bundle:

```sh
wasm-pack build crates/rumoca-bind-wasm --dev --target web --out-dir pkg
```
