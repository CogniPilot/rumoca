#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
image_tag="${RUMOCA_FOUNDATION_IMAGE:-rumoca-foundation:test}"

cd "${repo_root}"

echo "[foundation] building image ${image_tag}"
docker build --progress=plain \
  --target foundation \
  -t "${image_tag}" \
  -f packaging/docker/Dockerfile \
  .

echo "[foundation] running runtime checks"
docker run --rm --network none "${image_tag}" bash -lc '
  set -euo pipefail
  test "$PWD" = "/workspace"
  command -v bash curl git tini >/dev/null
  git --version
  curl --version | head -n1
'

echo "[foundation] smoke passed"
