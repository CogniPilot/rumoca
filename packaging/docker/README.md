# Rumoca Docker Images

This directory contains the canonical Docker build for Rumoca packaging targets.

Current targets:

- `foundation`: minimal shared container substrate, not a user-facing Rumoca runtime
- `core`: CPU-first Rumoca runtime with Tier 1 non-OMC backend stacks
- `ci`: CI validation image built on `core` with OpenModelica
- `dev`: contributor/devcontainer image built on `ci`

Current policy:

- `core` is a runtime image, not a full contributor build environment
- OpenModelica belongs in `ci`, not in `core`
- `dev` inherits OpenModelica from `ci` so contributors can run local parity sanity checks
- `dev` is the default offline-exported image target
- Jupyter and devcontainer tooling belong in the later `dev` image
- GPU-enabled stacks remain a later opt-in target and do not inflate the default CPU images

## Build Targets

From the repository root:

```bash
docker build --target foundation -t rumoca-foundation:test -f packaging/docker/Dockerfile .
docker build --target core -t rumoca-core:test -f packaging/docker/Dockerfile .
docker build --target dev -t rumoca-dev:test -f packaging/docker/Dockerfile .
docker build --target ci -t rumoca-ci:test -f packaging/docker/Dockerfile .
```

## Smoke Tests

Run the committed smoke checks from the repository root:

```bash
packaging/docker/smoke/foundation.sh
packaging/docker/smoke/core.sh
packaging/docker/smoke/ci.sh
packaging/docker/smoke/dev.sh
packaging/docker/smoke/offline-dev.sh
```

What they validate today:

- `foundation`
  - container starts in `/workspace`
  - shared shell/runtime tools are present
- `core`
  - Python Tier 1 runtime imports work
  - a small CasADi DAE solve runs
  - ONNX + ONNX Runtime execute a tiny in-memory model
  - PyTorch and SymPy runtime checks work
  - Julia SciML solves a small DAE with `IDA()`
  - `ModelingToolkit` loads and builds a minimal symbolic system offline
- `dev`
  - Rust nightly and wasm tooling are present
  - Node and npm are present for editor/frontend work
  - JupyterLab / Notebook / ipykernel import and report versions
  - OpenModelica is available for local sanity checks
  - contributor shell starts in `/workspace`
- `ci`
  - inherited Python Tier 1 runtime checks still work
  - inherited Julia SciML DAE smoke still works
  - OpenModelica compiles and simulates a tiny deterministic model offline
- `offline-dev`
  - exports the canonical `dev` image to a tarball
  - reloads that tarball into Docker
  - reuses the loaded image for the full contributor/devcontainer smoke without network access

All smoke scripts run the containers with `--network none` so they validate offline runtime behavior after the image has already been built or loaded from a tarball.

The `core` image keeps `ModelingToolkit` installed because it is the primary Rumoca Julia target. It also retains a targeted Julia precompile cache for the exact offline SciML runtime path we validate here, instead of a much larger generic depot snapshot. Later `ci` work can add further Julia-specific acceleration without forcing every exported runtime image to carry a full development cache.

## Current Scope

This is the current in-progress Docker roadmap state:

- `foundation`, `core`, `ci`, and `dev`
- basic devcontainer wiring exists
- offline export/load scripts exist

Later phases will add:

- `dev-gpu` as an opt-in developer image

## Offline Export and Load

The default offline export target is `dev`, because it matches the canonical VS Code
devcontainer/contributor environment.

Export the default image:

```bash
packaging/docker/export-image.sh
```

Export a specific supported target:

```bash
packaging/docker/export-image.sh core
packaging/docker/export-image.sh ci target/docker/rumoca-ci-custom.tar.gz
```

Load a previously exported archive:

```bash
packaging/docker/load-image.sh target/docker/rumoca-dev.tar.gz
```

Validate the full default offline round trip locally:

```bash
packaging/docker/smoke/offline-dev.sh
```

Rules:

- only declared supported targets may be exported
- the export script rebuilds the canonical target from `packaging/docker/Dockerfile`
- once the tarball has been created, loading it does not require GHCR or any other registry access
- offline users should prefer the exported `dev` image unless they intentionally want a smaller specialist target

## Devcontainer Usage

The committed devcontainer configuration uses the prebuilt GitHub Packages image:

- `ghcr.io/climamind/rumoca-dev:main`

That image is refreshed by the nightly `Docker Publish` workflow for all three
packaged targets:

- `ghcr.io/climamind/rumoca-core:main`
- `ghcr.io/climamind/rumoca-ci:main`
- `ghcr.io/climamind/rumoca-dev:main`

Open the repository in VS Code and choose "Reopen in Container" to use the canonical contributor image. The devcontainer mounts the repository at `/workspace` and uses:

- Python: `/opt/rumoca/python/bin/python`
- Julia: `/opt/julia/bin/julia`
