# npm Packaging

This directory is the canonical npm entrypoint for Rumoca WASM packaging.
The build copies this README into each generated `packages/rumoca/dist/<profile>-<variant>`
package so the published npm package links back to the packaging workflow.

## Common commands

```sh
npm run build
npm run build:pack
npm run build:release:core
npm run build:release:sim-diffsol:pack
npm run build:dev:full-web:rayon
```

## Notes

- Shared build logic is implemented in `packages/rumoca/build.mjs`.
- Non-pack builds land in `packages/rumoca/dist/<profile>-<variant>[-rayon]`.
- Packed tarballs are moved to `packages/rumoca/dist/`.
- Publishing uses the generated `packages/rumoca/dist/*` package metadata and artifacts.
