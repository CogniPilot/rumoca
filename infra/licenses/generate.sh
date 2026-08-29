#!/usr/bin/env bash
# Regenerate THIRD_PARTY_LICENSES.md, the attribution notice that ships with
# the published rumoca binaries.
#
# The binaries are statically linked, so they carry the code of every crate the
# file lists, and every one of those licenses conditions redistribution on its
# notice being reproduced. The file is the reproduction.
#
# Usage (from the repository root):
#
#   infra/licenses/generate.sh          write THIRD_PARTY_LICENSES.md
#   infra/licenses/generate.sh --check  fail if the committed file is stale
#
# Requires cargo-about. It is not in the dev shell yet; until it is, run:
#
#   nix run nixpkgs#cargo-about -- about generate ...
#
# or install it with `cargo install cargo-about`. This script finds it either
# way: on PATH first, then through `nix run`.

set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
config="${root}/infra/licenses/about.toml"
template="${root}/infra/licenses/about.hbs"
output="${root}/THIRD_PARTY_LICENSES.md"

mode="write"
if [ "${1:-}" = "--check" ]; then
    mode="check"
elif [ $# -gt 0 ]; then
    echo "usage: $0 [--check]" >&2
    exit 2
fi

run_about() {
    if command -v cargo-about >/dev/null 2>&1; then
        cargo-about generate --config "${config}" "${template}"
    elif command -v cargo >/dev/null 2>&1 && cargo about --version >/dev/null 2>&1; then
        cargo about generate --config "${config}" "${template}"
    elif command -v nix >/dev/null 2>&1; then
        nix run nixpkgs#cargo-about -- generate --config "${config}" "${template}"
    else
        echo "cargo-about not found: install it, or make nix available" >&2
        exit 1
    fi
}

cd "${root}"

generated="$(mktemp)"
trap 'rm -f "${generated}"' EXIT
run_about >"${generated}"

if [ ! -s "${generated}" ]; then
    echo "cargo-about produced no output" >&2
    exit 1
fi

if [ "${mode}" = "check" ]; then
    if ! diff -u "${output}" "${generated}"; then
        echo >&2
        echo "THIRD_PARTY_LICENSES.md is stale. Run infra/licenses/generate.sh." >&2
        exit 1
    fi
    echo "THIRD_PARTY_LICENSES.md is current."
    exit 0
fi

mv "${generated}" "${output}"
trap - EXIT
echo "wrote ${output}"
