# npm Packaging

This directory is the canonical npm entrypoint for Rumoca WASM packaging.

## Common commands

```sh
npm run build
npm run build:pack
npm run build:release:core
npm run build:release:sim-diffsol:pack
npm run build:dev:full-web:rayon
```

## Notes

- Shared build logic is implemented in `packaging/npm/build.mjs`.
- Non-pack builds land in `pkg/<profile>-<variant>[-rayon]` at repo root.
- Packed tarballs are moved to `pkg/` at repo root.
- Publishing uses the generated `pkg/*` package metadata and artifacts.
