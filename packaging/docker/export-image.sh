#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: packaging/docker/export-image.sh [target] [output]

Build and export a canonical Rumoca Docker image target as a gzip-compressed tarball.

Targets:
  foundation
  core
  ci
  dev (default)

Defaults:
  target: dev
  output: target/docker/rumoca-<target>.tar.gz
EOF
}

target="${1:-dev}"
if [[ "${target}" == "-h" || "${target}" == "--help" ]]; then
  usage
  exit 0
fi

case "${target}" in
  foundation|core|ci|dev) ;;
  *)
    echo "error: unsupported export target '${target}'" >&2
    usage >&2
    exit 1
    ;;
esac

repo_root="$(git rev-parse --show-toplevel)"
image_tag="rumoca-${target}:offline"
output_path="${2:-${repo_root}/target/docker/rumoca-${target}.tar.gz}"

mkdir -p "$(dirname "${output_path}")"
tmp_output="$(mktemp "${output_path}.tmp.XXXXXX")"
trap 'rm -f "${tmp_output}"' EXIT

cd "${repo_root}"

echo "[export] building canonical target '${target}' as ${image_tag}"
docker build --progress=plain \
  --target "${target}" \
  -t "${image_tag}" \
  -f packaging/docker/Dockerfile \
  .

echo "[export] saving ${image_tag} to ${output_path}"
docker save "${image_tag}" | gzip -c > "${tmp_output}"
mv "${tmp_output}" "${output_path}"
trap - EXIT

echo "[export] archive size bytes: $(wc -c < "${output_path}" | tr -d '[:space:]')"
echo "[export] archive sha256: $(sha256sum "${output_path}" | cut -d' ' -f1)"
echo "[export] done: ${output_path}"
