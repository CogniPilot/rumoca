#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
image_tag="${RUMOCA_DEV_IMAGE:-rumoca-dev:test}"
skip_build="${RUMOCA_DOCKER_SKIP_BUILD:-0}"

cd "${repo_root}"

if [ "${skip_build}" = "1" ]; then
  echo "[dev] reusing prebuilt image ${image_tag}"
else
  echo "[dev] building image ${image_tag}"
  docker build --progress=plain \
    --target dev \
    -t "${image_tag}" \
    -f packaging/docker/Dockerfile \
    .
fi

echo "[dev] image size bytes: $(docker image inspect "${image_tag}" --format '{{.Size}}')"

echo "[dev] running contributor toolchain checks"
docker run --rm \
  --network none \
  "${image_tag}" \
  bash -lc '
    set -euo pipefail
    test "$PWD" = "/workspace"
    cargo --version
    rustc --version
    rustup show active-toolchain
    wasm-bindgen --version
    wasm-pack --version
    node --version
    npm --version
    omc --version | head -n1
    python - <<'"'"'PY'"'"'
import ipykernel
import jupyterlab
import notebook
print("python-jupyter-imports-ok")
PY
    jupyter --version
  '

echo "[dev] smoke passed"
