# Rumoca WebAssembly (WASM)

This directory contains the WebAssembly (WASM) build of Rumoca.  
The WASM target allows Rumoca to run in the browser without additional native dependencies.

> **TODO:** Document npm-based inclusion and usage (e.g., `import` from the published npm package).

## Usage Example (Browser / ESM)

```javascript
import init, { compile, render_template } from './pkg/rumoca.js'

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

### 1. npm-based workflow (for bundlers / npm publish)

This workflow builds the Rumoca WASM target and packages it as an npm module that can be used with bundlers and published to npm.

```sh
# Build the npm package
cd wasm && npm run build

# Publish the package to npm
cd wasm && npm run publish
```

### 2. Rust-based workflow (pure WASM via `wasm-pack`)

You can also build the WASM artifacts directly with `wasm-pack`, targeting different environments:

```sh
# For the web (native ESM in browsers)
wasm-pack build . --release --target web --no-default-features --features wasm

# For bundlers (e.g., webpack, Rollup, Vite in bundler mode)
wasm-pack build . --release --target bundler --no-default-features --features wasm

# For Node.js
wasm-pack build . --release --target nodejs --no-default-features --features wasm
```

## Notes

- For browser usage with Vite, the `web` target generally works best.
- To use `rumoca-wasm` with the Vite bundler (as of December 2025), you must use the  
  [`vite-plugin-wasm`](https://www.npmjs.com/package/vite-plugin-wasm) plugin.

## Debug Build

To debug Rumoca in the browser, build a development WASM bundle:

```sh
wasm-pack build . --dev --target web --no-default-features --features wasm
```
