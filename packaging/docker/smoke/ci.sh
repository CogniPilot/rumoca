#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
image_tag="${RUMOCA_CI_IMAGE:-rumoca-ci:test}"
skip_build="${RUMOCA_DOCKER_SKIP_BUILD:-0}"

cd "${repo_root}"

if [ "${skip_build}" = "1" ]; then
  echo "[ci] reusing prebuilt image ${image_tag}"
else
  echo "[ci] building image ${image_tag}"
  docker build --progress=plain \
    --target ci \
    -t "${image_tag}" \
    -f packaging/docker/Dockerfile \
    .
fi

echo "[ci] image size bytes: $(docker image inspect "${image_tag}" --format '{{.Size}}')"

echo "[ci] running inherited Python backend smoke"
docker run --rm \
  --network none \
  -v "${repo_root}:/workspace:ro" \
  "${image_tag}" \
  python /workspace/packaging/docker/examples/core_backend_smoke.py

echo "[ci] running inherited Julia SciML smoke"
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

echo "[ci] running OpenModelica smoke"
omc_log="$(mktemp)"
if ! docker run --rm \
  --network none \
  -v "${repo_root}:/workspace:ro" \
  "${image_tag}" \
  bash -lc '
    set -euo pipefail
    workdir="$(mktemp -d)"
    cp /workspace/packaging/docker/examples/ci_omc_smoke.mo "${workdir}/ci_omc_smoke.mo"
    cp /workspace/packaging/docker/examples/ci_omc_smoke.mos "${workdir}/ci_omc_smoke.mos"
    cd "${workdir}"
    omc --version | head -n1
    omc ci_omc_smoke.mos
    test -f DockerSmoke_res.mat
  ' >"${omc_log}" 2>&1; then
  cat "${omc_log}"
  rm -f "${omc_log}"
  exit 1
fi
tail -n 10 "${omc_log}"
rm -f "${omc_log}"

echo "[ci] smoke passed"
