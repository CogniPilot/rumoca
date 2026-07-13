#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
expected_package_version="$(tr -d '[:space:]' < "$repo_root/toolchains/openmodelica-version")"
if [[ ! "$expected_package_version" =~ ^([0-9]+\.[0-9]+\.[0-9]+)-[0-9]+$ ]]; then
  echo "Invalid OpenModelica package pin: $expected_package_version" >&2
  exit 1
fi
expected_version="${BASH_REMATCH[1]}"
if [[ "${EUID:-$(id -u)}" -eq 0 ]]; then
  SUDO=()
else
  SUDO=(sudo)
fi

installed_version() {
  local version_output="$1"
  [[ "$version_output" =~ ^OpenModelica\ ([0-9]+\.[0-9]+\.[0-9]+)$ ]] || return 1
  printf '%s\n' "${BASH_REMATCH[1]}"
}

check_version() {
  local version_output="$1"
  local actual_version
  actual_version="$(installed_version "$version_output" || true)"
  if [[ "$actual_version" != "$expected_version" ]]; then
    echo "OpenModelica version mismatch: expected $expected_version, got ${actual_version:-unknown}" >&2
    return 1
  fi
  printf '%s\n' "$version_output"
}

if [[ "${1:-}" == "--check-output" ]]; then
  [[ "$#" -eq 2 ]] || { echo "usage: $0 --check-output '<omc --version output>'" >&2; exit 64; }
  check_version "$2"
  exit
fi

"${SUDO[@]}" apt-get update
"${SUDO[@]}" apt-get install -y --no-install-recommends \
  build-essential ca-certificates clang cmake curl gnupg \
  libexpat1-dev liblapack-dev lsb-release unzip zip
curl -fsSL https://build.openmodelica.org/apt/openmodelica.asc \
  | "${SUDO[@]}" gpg --dearmor -o /usr/share/keyrings/openmodelica-keyring.gpg
echo "deb [arch=amd64 signed-by=/usr/share/keyrings/openmodelica-keyring.gpg] https://build.openmodelica.org/apt $(lsb_release -cs) stable" \
  | "${SUDO[@]}" tee /etc/apt/sources.list.d/openmodelica.list
"${SUDO[@]}" apt-get update

package_version="$(
  apt-cache madison omc \
    | awk -v expected="$expected_package_version" '$3 == expected { print $3; exit }'
)"
if [[ -z "$package_version" ]]; then
  echo "OpenModelica package $expected_package_version is unavailable from the configured repository" >&2
  apt-cache madison omc >&2
  exit 1
fi

"${SUDO[@]}" apt-get install -y --no-install-recommends "omc=$expected_package_version"
check_version "$(omc --version)"
