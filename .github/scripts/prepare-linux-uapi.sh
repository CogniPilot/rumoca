#!/usr/bin/env bash
# Expose Linux UAPI headers to musl without adding glibc's include directory.
set -euo pipefail
output=${1:?usage: prepare-linux-uapi.sh OUTPUT}
headers=${LINUX_UAPI_ROOT:-/usr/include}
multiarch=$(dpkg-architecture -qDEB_HOST_MULTIARCH)
case "$multiarch" in
  x86_64-linux-gnu|aarch64-linux-gnu) ;;
  *) echo "Unsupported native Linux architecture: $multiarch" >&2; exit 1 ;;
esac
for entry in linux asm-generic; do
  test -d "$headers/$entry" || { echo "Missing Linux UAPI: $headers/$entry" >&2; exit 1; }
done
test -d "$headers/$multiarch/asm" || { echo "Missing architecture UAPI: $multiarch/asm" >&2; exit 1; }
# Refuse reuse: stale or mixed-architecture headers must never enter a release.
mkdir "$output"
ln -s "$headers/linux" "$output/linux"
ln -s "$headers/asm-generic" "$output/asm-generic"
ln -s "$headers/$multiarch/asm" "$output/asm"
