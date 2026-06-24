#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -eq 0 ]; then
  echo "usage: $0 [apt-get-install-flags...] <package>..." >&2
  exit 64
fi

if [ "${EUID:-$(id -u)}" -eq 0 ]; then
  SUDO=()
else
  SUDO=(sudo)
fi

disable_unstable_runner_sources() {
  local source_path

  # GitHub's ubuntu-24.04 runner can ship an Azure CLI apt source that returns
  # 403, which makes apt-get update fail before repository packages are used.
  while IFS= read -r source_path; do
    if [ -n "$source_path" ] && [ -e "$source_path" ]; then
      echo "Disabling unstable apt source: $source_path"
      "${SUDO[@]}" mv "$source_path" "$source_path.disabled"
    fi
  done < <(
    grep -rl "packages.microsoft.com/repos/azure-cli" \
      /etc/apt/sources.list /etc/apt/sources.list.d 2>/dev/null || true
  )
}

apt_update_with_retry() {
  local attempt

  for attempt in 1 2 3; do
    if "${SUDO[@]}" apt-get update; then
      return 0
    fi
    if [ "$attempt" -lt 3 ]; then
      sleep "$((attempt * 5))"
    fi
  done

  return 1
}

disable_unstable_runner_sources
apt_update_with_retry
"${SUDO[@]}" env DEBIAN_FRONTEND=noninteractive apt-get install -y "$@"
