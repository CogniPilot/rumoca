#!/usr/bin/env bash
set -euo pipefail
: "${RUNNER_TEMP:?RUNNER_TEMP required}"
: "${GITHUB_ENV:?GITHUB_ENV required}"
# The default gamepad feature requires musl libudev. This ISC source
# pin matches nixpkgs' libudev-zero 1.0.3 used by our static toolchain.
udev_source="$RUNNER_TEMP/libudev-zero-1.0.3"
udev_prefix="$RUNNER_TEMP/musl-libudev"
curl --fail --location --max-time 60 \
  https://codeload.github.com/illiliti/libudev-zero/tar.gz/refs/tags/1.0.3 \
  --output "$RUNNER_TEMP/libudev-zero.tar.gz"
echo "0bd89b657d62d019598e6c7ed726ff8fed80e8ba092a83b484d66afb80b77da5  $RUNNER_TEMP/libudev-zero.tar.gz" | sha256sum --check
tar -xzf "$RUNNER_TEMP/libudev-zero.tar.gz" -C "$RUNNER_TEMP"
uapi="$RUNNER_TEMP/musl-linux-uapi"
bash "$(dirname "${BASH_SOURCE[0]}")/prepare-linux-uapi.sh" "$uapi"
# Include only kernel UAPI; /usr/include would mix glibc with musl.
printf '#include <linux/netlink.h>\n#include <linux/input.h>\n' |
  musl-gcc -isystem "$uapi" -x c -fsyntax-only -
make -C "$udev_source" -j4 CC=musl-gcc CPPFLAGS="-isystem $uapi" PREFIX="$udev_prefix" install-static
echo "PKG_CONFIG_ALLOW_CROSS=1" >> $GITHUB_ENV
echo "PKG_CONFIG_ALL_STATIC=1" >> $GITHUB_ENV
echo "PKG_CONFIG_LIBDIR=$udev_prefix/lib/pkgconfig" >> $GITHUB_ENV
