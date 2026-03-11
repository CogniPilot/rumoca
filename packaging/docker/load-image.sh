#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: packaging/docker/load-image.sh <archive>

Load a Rumoca Docker image tarball previously produced by packaging/docker/export-image.sh.

Supported archive formats:
  .tar
  .tar.gz
  .tgz
EOF
}

archive_path="${1:-}"
if [[ -z "${archive_path}" || "${archive_path}" == "-h" || "${archive_path}" == "--help" ]]; then
  usage
  [[ -z "${archive_path}" ]] && exit 1 || exit 0
fi

if [[ ! -f "${archive_path}" ]]; then
  echo "error: archive not found: ${archive_path}" >&2
  exit 1
fi

echo "[load] loading archive ${archive_path}"
case "${archive_path}" in
  *.tar)
    docker load -i "${archive_path}"
    ;;
  *.tar.gz|*.tgz)
    gzip -dc "${archive_path}" | docker load
    ;;
  *)
    echo "error: unsupported archive format: ${archive_path}" >&2
    usage >&2
    exit 1
    ;;
esac
