#!/usr/bin/env bash
set -euo pipefail

if [[ "$#" -lt 2 || "$#" -gt 3 ]]; then
  echo "usage: $0 MANIFEST EXPECTED_VERSION [PACKAGE_DIRECTORY]" >&2
  exit 64
fi

manifest="$1"
expected_version="$2"
package_directory="${3:-}"
pool="https://build.openmodelica.org/apt/pool/contrib-noble"
declare -a names architectures filenames hashes urls
count=0
seen=" "

while read -r name version architecture filename hash url extra; do
  [[ -z "$name" || "$name" == \#* ]] && continue
  [[ -z "${extra:-}" ]] || { echo "Unexpected manifest fields for $name" >&2; exit 1; }
  case "$name" in
    omc|libomc|libomcsimulation) required_architecture="amd64" ;;
    omc-common) required_architecture="all" ;;
    *) echo "Unexpected OpenModelica package: $name" >&2; exit 1 ;;
  esac
  [[ "$seen" != *" $name "* ]] || { echo "Duplicate OpenModelica package: $name" >&2; exit 1; }
  [[ "$version" == "$expected_version" ]] || { echo "Manifest version mismatch for $name" >&2; exit 1; }
  [[ "$architecture" == "$required_architecture" ]] || { echo "Manifest architecture mismatch for $name" >&2; exit 1; }
  expected_filename="${name}_${expected_version}_${architecture}.deb"
  [[ "$filename" == "$expected_filename" ]] || { echo "Manifest filename mismatch for $name" >&2; exit 1; }
  [[ "$hash" =~ ^[0-9a-f]{64}$ ]] || { echo "Invalid SHA-256 for $name" >&2; exit 1; }
  [[ "$url" == "$pool/$filename" ]] || { echo "Manifest URL mismatch for $name" >&2; exit 1; }
  names[count]="$name"
  architectures[count]="$architecture"
  filenames[count]="$filename"
  hashes[count]="$hash"
  urls[count]="$url"
  count=$((count + 1))
  seen+="$name "
done < "$manifest"

for required in omc omc-common libomc libomcsimulation; do
  [[ "$seen" == *" $required "* ]] || { echo "Missing OpenModelica package: $required" >&2; exit 1; }
done
[[ "$count" -eq 4 ]] || { echo "Expected four OpenModelica packages, got $count" >&2; exit 1; }

if [[ -z "$package_directory" ]]; then
  for ((index = 0; index < count; index++)); do
    printf '%s %s\n' "${filenames[index]}" "${urls[index]}"
  done
  exit
fi

deb_count="$(find "$package_directory" -maxdepth 1 -type f -name '*.deb' | wc -l | tr -d '[:space:]')"
[[ "$deb_count" == "4" ]] || { echo "Package directory must contain exactly four .deb files" >&2; exit 1; }

for ((index = 0; index < count; index++)); do
  artifact="$package_directory/${filenames[index]}"
  [[ -f "$artifact" ]] || { echo "Missing OpenModelica artifact: ${filenames[index]}" >&2; exit 1; }
  if command -v sha256sum >/dev/null 2>&1; then
    actual_hash="$(sha256sum "$artifact" | awk '{print $1}')"
  else
    actual_hash="$(shasum -a 256 "$artifact" | awk '{print $1}')"
  fi
  [[ "$actual_hash" == "${hashes[index]}" ]] || { echo "Checksum mismatch for ${filenames[index]}" >&2; exit 1; }

  actual_package="$(dpkg-deb -f "$artifact" Package)"
  actual_version="$(dpkg-deb -f "$artifact" Version)"
  actual_architecture="$(dpkg-deb -f "$artifact" Architecture)"
  [[ "$actual_package" == "${names[index]}" ]] || { echo "Package mismatch for ${filenames[index]}" >&2; exit 1; }
  [[ "$actual_version" == "$expected_version" ]] || { echo "Version mismatch for ${filenames[index]}" >&2; exit 1; }
  [[ "$actual_architecture" == "${architectures[index]}" ]] || { echo "Architecture mismatch for ${filenames[index]}" >&2; exit 1; }
done
