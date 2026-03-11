#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
archive_path="${RUMOCA_OFFLINE_DEV_ARCHIVE:-${repo_root}/target/docker/rumoca-dev-offline-smoke.tar.gz}"
image_tag="${RUMOCA_OFFLINE_DEV_IMAGE:-rumoca-dev:offline}"

cd "${repo_root}"

echo "[offline-dev] exporting default dev image to ${archive_path}"
packaging/docker/export-image.sh dev "${archive_path}"

if docker image inspect "${image_tag}" >/dev/null 2>&1; then
  echo "[offline-dev] removing existing loaded tag ${image_tag} before reload"
  docker image rm -f "${image_tag}" >/dev/null
fi

echo "[offline-dev] loading ${archive_path}"
packaging/docker/load-image.sh "${archive_path}"

echo "[offline-dev] validating loaded image ${image_tag}"
RUMOCA_DOCKER_SKIP_BUILD=1 \
RUMOCA_DEV_IMAGE="${image_tag}" \
bash packaging/docker/smoke/dev.sh

echo "[offline-dev] smoke passed"
