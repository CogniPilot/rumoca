#!/usr/bin/env bash
set -euo pipefail

readonly archive_name=closure.nar
readonly manifest_name=manifest
readonly -a required_binaries=(
  msl_tests
  rumoca-worker
  rumoca-sim-worker
  rumoca-msl-tools
)

die() {
  echo "msl-nix-closure: $*" >&2
  exit 1
}

require_inputs() {
  local commit=$1
  local system=$2
  [[ $commit =~ ^[0-9a-fA-F]{40}$ ]] || die "invalid Git commit: $commit"
  [[ $system =~ ^[a-zA-Z0-9._+-]+$ ]] || die "invalid Nix system: $system"
}

manifest_value() {
  local manifest=$1
  local key=$2
  local -a values
  mapfile -t values < <(
    awk -v key="$key" 'index($0, key "=") == 1 { print substr($0, length(key) + 2) }' "$manifest"
  )
  [[ ${#values[@]} -eq 1 && -n ${values[0]} ]] || die "invalid manifest field: $key"
  printf '%s\n' "${values[0]}"
}

verify_executables() {
  local out_path=$1
  local manifest=${2-}
  local binary path actual expected
  for binary in "${required_binaries[@]}"; do
    path="$out_path/bin/$binary"
    [[ -f $path && -x $path ]] || die "missing required executable: $binary"
    if [[ -n $manifest ]]; then
      expected=$(manifest_value "$manifest" "binary_${binary}_sha256")
      [[ $expected =~ ^[0-9a-f]{64}$ ]] || die "invalid executable checksum: $binary"
      actual=$(sha256sum "$path" | awk '{ print $1 }')
      [[ $actual == "$expected" ]] || die "required executable checksum mismatch: $binary"
    fi
  done
}

pack() {
  [[ $# -eq 4 ]] || die "usage: $0 pack OUT_LINK ARTIFACT_DIR COMMIT SYSTEM"
  local out_link=$1
  local artifact_dir=$2
  local commit=$3
  local system=$4
  local out_path archive manifest paths_file archive_tmp binary
  local -a closure_paths

  require_inputs "$commit" "$system"
  command -v nix-store >/dev/null || die "nix-store is required"
  command -v sha256sum >/dev/null || die "sha256sum is required"
  [[ -e $out_link ]] || die "missing realized MSL output: $out_link"
  out_path=$(realpath "$out_link")
  [[ $out_path == /* && $out_path != *[[:space:]]* ]] || die "invalid Nix output path: $out_path"
  verify_executables "$out_path"

  mkdir -p "$artifact_dir"
  archive="$artifact_dir/$archive_name"
  manifest="$artifact_dir/$manifest_name"
  paths_file="$artifact_dir/closure-paths.tmp"
  archive_tmp="$archive.tmp"
  rm -f "$archive" "$manifest" "$paths_file" "$archive_tmp"
  trap 'rm -f "$paths_file" "$archive_tmp"' RETURN

  nix-store --query --requisites "$out_path" > "$paths_file" ||
    die "failed to query Nix requisites"
  grep -Fxq "$out_path" "$paths_file" || printf '%s\n' "$out_path" >> "$paths_file"
  LC_ALL=C sort -u -o "$paths_file" "$paths_file"
  mapfile -t closure_paths < "$paths_file"
  [[ ${#closure_paths[@]} -gt 0 ]] || die "Nix requisites closure is empty"
  for out_path in "${closure_paths[@]}"; do
    [[ $out_path == /* && $out_path != *[[:space:]]* ]] ||
      die "invalid path in Nix requisites closure: $out_path"
  done
  nix-store --export "${closure_paths[@]}" > "$archive_tmp" ||
    die "failed to export Nix requisites closure"
  [[ -s $archive_tmp ]] || die "exported Nix closure archive is empty"
  mv "$archive_tmp" "$archive"

  {
    echo 'version=1'
    echo "commit=$commit"
    echo "system=$system"
    echo "out_path=$(realpath "$out_link")"
    echo "archive_sha256=$(sha256sum "$archive" | awk '{ print $1 }')"
    for binary in "${required_binaries[@]}"; do
      echo "binary_${binary}_sha256=$(sha256sum "$(realpath "$out_link")/bin/$binary" | awk '{ print $1 }')"
    done
  } > "$manifest"
  rm -f "$paths_file"
  trap - RETURN
}

restore() {
  [[ $# -eq 4 ]] || die "usage: $0 restore ARTIFACT_DIR OUT_LINK COMMIT SYSTEM"
  local artifact_dir=$1
  local out_link=$2
  local expected_commit=$3
  local expected_system=$4
  local archive="$artifact_dir/$archive_name"
  local manifest="$artifact_dir/$manifest_name"
  local version commit system out_path expected_sha actual_sha paths_file path
  local -a closure_paths

  require_inputs "$expected_commit" "$expected_system"
  command -v nix-store >/dev/null || die "nix-store is required"
  command -v sha256sum >/dev/null || die "sha256sum is required"
  [[ -f $manifest ]] || die "missing manifest: $manifest"
  [[ -f $archive ]] || die "missing closure archive: $archive"
  [[ $(wc -l < "$manifest") -eq 9 ]] || die "invalid manifest structure"

  version=$(manifest_value "$manifest" version)
  commit=$(manifest_value "$manifest" commit)
  system=$(manifest_value "$manifest" system)
  out_path=$(manifest_value "$manifest" out_path)
  expected_sha=$(manifest_value "$manifest" archive_sha256)
  [[ $version == 1 ]] || die "unsupported manifest version: $version"
  require_inputs "$commit" "$system"
  [[ $out_path == /* && $out_path != *[[:space:]]* ]] || die "invalid Nix output path: $out_path"
  [[ $expected_sha =~ ^[0-9a-f]{64}$ ]] || die "invalid archive checksum"
  [[ $commit == "$expected_commit" ]] || die "commit mismatch: expected $expected_commit, got $commit"
  [[ $system == "$expected_system" ]] || die "system mismatch: expected $expected_system, got $system"
  actual_sha=$(sha256sum "$archive" | awk '{ print $1 }')
  [[ $actual_sha == "$expected_sha" ]] || die "archive checksum mismatch"

  nix-store --import < "$archive" || die "Nix closure import failed"
  [[ -e $out_path ]] || die "imported Nix output is missing: $out_path"
  paths_file=$(mktemp)
  trap 'rm -f "$paths_file"' RETURN
  nix-store --query --requisites "$out_path" > "$paths_file" ||
    die "failed to query imported Nix requisites"
  mapfile -t closure_paths < "$paths_file"
  [[ ${#closure_paths[@]} -gt 0 ]] || die "imported Nix requisites closure is empty"
  for path in "${closure_paths[@]}"; do
    [[ -e $path ]] || die "imported Nix requisite is missing: $path"
  done
  verify_executables "$out_path" "$manifest"

  if [[ -e $out_link && ! -L $out_link ]]; then
    die "refusing to replace non-symlink output: $out_link"
  fi
  rm -f "$out_link"
  ln -s "$out_path" "$out_link"
  rm -f "$paths_file"
  trap - RETURN
}

case ${1-} in
  pack)
    shift
    pack "$@"
    ;;
  restore)
    shift
    restore "$@"
    ;;
  *)
    die "usage: $0 {pack|restore} ..."
    ;;
esac
