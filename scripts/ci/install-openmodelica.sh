#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
expected_package_version="$(tr -d '[:space:]' < "$repo_root/toolchains/openmodelica-version")"
package_manifest="$repo_root/toolchains/openmodelica-packages.txt"
package_verifier="$repo_root/scripts/ci/verify-openmodelica-packages.sh"
if [[ ! "$expected_package_version" =~ ^([0-9]+\.[0-9]+\.[0-9]+)~1-g[0-9a-f]+-[0-9]+$ ]]; then
  echo "Invalid OpenModelica package pin: $expected_package_version" >&2
  exit 1
fi
expected_version="${expected_package_version%-*}"
if [[ "${EUID:-$(id -u)}" -eq 0 ]]; then
  SUDO=()
else
  SUDO=(sudo)
fi

installed_version() {
  local version_output="$1"
  [[ "$version_output" =~ ^OpenModelica\ (.+)$ ]] || return 1
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

package_list="$("$package_verifier" "$package_manifest" "$expected_package_version")"
package_directory="$(mktemp -d)"
trap 'rm -rf -- "$package_directory"' EXIT

"${SUDO[@]}" apt-get update
"${SUDO[@]}" apt-get install -y --no-install-recommends \
  build-essential ca-certificates clang cmake curl \
  libexpat1-dev liblapack-dev unzip zip
while read -r filename url; do
  curl --proto '=https' --tlsv1.2 --fail --location --show-error \
    --output "$package_directory/$filename" "$url"
done <<< "$package_list"
"$package_verifier" "$package_manifest" "$expected_package_version" "$package_directory"

"${SUDO[@]}" apt-get install -y --no-install-recommends \
  "$package_directory/omc_${expected_package_version}_amd64.deb" \
  "$package_directory/omc-common_${expected_package_version}_all.deb" \
  "$package_directory/libomc_${expected_package_version}_amd64.deb" \
  "$package_directory/libomcsimulation_${expected_package_version}_amd64.deb"
for package in omc omc-common libomc libomcsimulation; do
  installed_package_version="$(dpkg-query -W -f='${Version}' "$package")"
  if [[ "$installed_package_version" != "$expected_package_version" ]]; then
    echo "Installed $package version mismatch: expected $expected_package_version, got $installed_package_version" >&2
    exit 1
  fi
done
check_version "$(omc --version)"
