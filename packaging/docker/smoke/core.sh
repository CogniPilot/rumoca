#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
image_tag="${RUMOCA_CORE_IMAGE:-rumoca-core:test}"

cd "${repo_root}"

echo "[core] building image ${image_tag}"
docker build --progress=plain \
  --target core \
  -t "${image_tag}" \
  -f packaging/docker/Dockerfile \
  .

echo "[core] image size bytes: $(docker image inspect "${image_tag}" --format '{{.Size}}')"

echo "[core] running python backend smoke"
docker run --rm \
  --network none \
  -v "${repo_root}:/workspace:ro" \
  "${image_tag}" \
  python /workspace/packaging/docker/examples/core_backend_smoke.py

echo "[core] running julia SciML smoke"
julia_log="$(mktemp)"
if ! docker run --rm \
  --network none \
  -v "${repo_root}:/workspace:ro" \
  "${image_tag}" \
  julia /workspace/packaging/docker/examples/core_dae_smoke.jl >"${julia_log}" 2>&1; then
  cat "${julia_log}"
  rm -f "${julia_log}"
  exit 1
fi
tail -n 5 "${julia_log}"
rm -f "${julia_log}"

echo "[core] smoke passed"
