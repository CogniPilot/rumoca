#!/usr/bin/env bash
# Prepare the wheel environment before nix develop can select a fallback shell.
set -euo pipefail

case "${1-}" in
  x86_64-linux|aarch64-linux|x86_64-darwin|aarch64-darwin) system="$1" ;;
  *) echo 'usage: ci-nix-prepare.sh <wheel Nix system>' >&2; exit 2 ;;
esac
if [ "$#" -ne 1 ]; then
  echo 'expected exactly one wheel Nix system' >&2
  exit 2
fi

printf 'Runner Bash=%s; Nix=%s; curl=%s\n' "$BASH_VERSION" "$(command -v nix)" "$(command -v curl)"
nix --version
curl --disable --version

# Retry only this read-only transport probe: at most 3 ten-second transfers
# and 5+10 seconds of backoff. Do not retry any Nix build or payload command.
probe_status=0
for attempt in 1 2 3; do
  if http_status="$(curl --disable --silent --show-error --fail --connect-timeout 5 \
    --max-time 10 --output /dev/null --write-out '%{http_code}' \
    https://cache.nixos.org/nix-cache-info)"; then
    if [ "$http_status" = 200 ]; then
      break
    fi
    probe_status=22
    break
  else
    status=$?
  fi
  case "$status:$http_status" in
    6:*|7:*|28:*|22:429|22:500|22:502|22:503|22:504) ;;
    130:*|143:*) exit "$status" ;;
    *) probe_status="$status"; break ;;
  esac
  if [ "$attempt" -eq 3 ]; then
    probe_status="$status"
    break
  fi
  sleep "$((attempt * 5))"
done
if [ "$probe_status" -ne 0 ]; then
  echo "::warning::Public-cache probe unavailable (curl=$probe_status HTTP=$http_status); actual pinned realization decides success" >&2
fi

printf 'Preparing wheel shell for %s\n' "$system"
# Bypass the optional throttled loopback cache for preparation. Keep normal
# signature verification and the project's locked nixpkgs input.
options=(--option substituters https://cache.nixos.org --no-update-lock-file)
if bash_root="$(nix build "${options[@]}" --no-link --print-out-paths \
  --inputs-from . 'nixpkgs#bashInteractive^out')"; then
  :
else
  status=$?
  echo '::error::Locked native Bash preparation failed; refusing nix develop fallback' >&2
  exit "$status"
fi
case "$bash_root" in
  /*) ;;
  *) echo '::error::Nix Bash output must be one absolute path' >&2; exit 1 ;;
esac
if [[ "$bash_root" == *$'\n'* ]] || [ ! -x "$bash_root/bin/bash" ]; then
  echo '::error::Nix Bash output is missing or ambiguous' >&2
  exit 1
fi
printf 'Prepared native Bash: %s/bin/bash\n' "$bash_root"
"$bash_root/bin/bash" --version
if nix print-dev-env "${options[@]}" ".#devShells.$system.ci-python-wheel" >/dev/null; then
  echo 'Wheel dev shell prepared; compiler/test/wheel commands will run once.'
else
  status=$?
  echo '::error::Wheel dev-shell preparation failed; no compiler/test/wheel command started' >&2
  exit "$status"
fi
