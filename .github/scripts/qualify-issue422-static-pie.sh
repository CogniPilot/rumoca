#!/usr/bin/env bash
# Run only through a bounded owner; this builds tests, not release artifacts.
set -euo pipefail
export GIT_OPTIONAL_LOCKS=0
test "$#" -ge 1 && test "$#" -le 2
evidence="$1"
mode="${2:-all}"
case "$mode" in all|prepare|run) ;; *) exit 2 ;; esac
if test "$mode" != all && ! git diff --quiet HEAD; then
  echo "tracked files modified before $mode:" >&2
  git status --short >&2
  exit 1
fi
if test "$mode" != run; then
  mkdir "$evidence"
  rustc -vV > "$evidence/rustc-version.txt"
  grep -qx 'host: x86_64-unknown-linux-gnu' "$evidence/rustc-version.txt"
  git rev-parse HEAD > "$evidence/source.txt"
  sha256sum Cargo.lock .github/scripts/qualify-issue422-static-pie.sh \
    crates/rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_422.mo \
    > "$evidence/input-sha256.txt"
else
  test -d "$evidence" && test ! -L "$evidence"
  test "$(git rev-parse HEAD)" = "$(cat "$evidence/source.txt")"
  sha256sum -c "$evidence/input-sha256.txt"
fi
export CARGO_INCREMENTAL=0
export CARGO_BUILD_JOBS="${CARGO_BUILD_JOBS:-4}"
export RUST_TEST_THREADS="${RUST_TEST_THREADS:-4}"
export RAYON_NUM_THREADS="${RAYON_NUM_THREADS:-4}"
export CARGO_PROFILE_DEV_DEBUG=0 CARGO_PROFILE_TEST_DEBUG=0

prepare() {
  local name="$1" target="$2" kind="$3" selector="$4"
  shift 4
  cargo rustc --locked --jobs "$CARGO_BUILD_JOBS" --profile test \
    --message-format=json "$@" -- \
    -C target-feature=+crt-static -C relocation-model=pic -C link-arg=-static-pie \
    > "$evidence/$name.cargo.jsonl" 2> "$evidence/$name.build.log"
  local executable
  executable="$(jq -s -er --arg target "$target" --arg kind "$kind" '
    [ .[] | select(.reason == "compiler-artifact")
      | select(.target.name == $target and (.target.kind | index($kind)) != null
      and .profile.test == true and (.executable | type) == "string"
      and (.executable | length) > 0) | .executable ]
    | if length == 1 then .[0] else error("expected exactly one test executable") end
    ' "$evidence/$name.cargo.jsonl")"
  test -f "$executable" && test -x "$executable" && test ! -L "$executable"
  file "$executable" > "$evidence/$name.file.txt"
  readelf -h "$executable" > "$evidence/$name.elf-header.txt"
  readelf -l "$executable" > "$evidence/$name.elf-segments.txt"
  readelf -d "$executable" > "$evidence/$name.elf-dynamic.txt"
  grep -Eq 'Class:[[:space:]]+ELF64' "$evidence/$name.elf-header.txt"
  grep -Eq 'Type:[[:space:]]+DYN' "$evidence/$name.elf-header.txt"
  grep -Eq 'Machine:[[:space:]]+Advanced Micro Devices X86-64' "$evidence/$name.elf-header.txt"
  if grep -Eq 'INTERP' "$evidence/$name.elf-segments.txt" \
    || grep -Eq '\(NEEDED\)' "$evidence/$name.elf-dynamic.txt"; then
    echo "$name is not a static PIE executable" >&2
    return 1
  fi
  cp "$executable" "$evidence/$name.elf"
  (cd "$evidence" && sha256sum "$name.elf") > "$evidence/$name.elf.sha256"
}

execute() {
  local name="$1" selector="$2"
  (cd "$evidence" && sha256sum -c "$name.elf.sha256")
  if ! timeout --foreground --kill-after=5s 60s "$evidence/$name.elf" \
    "$selector" --exact --nocapture --test-threads="$RUST_TEST_THREADS" \
    > "$evidence/$name.test.log" 2>&1; then
    cat "$evidence/$name.test.log"
    return 1
  fi
  cat "$evidence/$name.test.log"
  grep -Eq '^test result: ok\. 1 passed; 0 failed; 0 ignored;' "$evidence/$name.test.log"
}

qualify() {
  local name="$1" selector="$4"
  if test "$mode" != run; then prepare "$@"; fi
  if test "$mode" != prepare; then execute "$name" "$selector"; fi
}

qualify memcmp rumoca_exec_cranelift lib \
  emit::host_runtime::tests::issue_422_memcmp_relocation_needs_no_process_symbol_export \
  -p rumoca-exec-cranelift --lib
qualify memory rumoca_exec_cranelift lib \
  emit::host_runtime::tests::issue_422_memory_libcalls_relocate_without_process_symbol_export \
  -p rumoca-exec-cranelift --lib
qualify original-u4 suite_core test \
  reported_bug_runtime::issue_422_original_array_function_runs_without_native_fallback \
  -p rumoca --no-default-features --features fmu-packaging --test suite_core
if test "$mode" != prepare; then
  grep -Eq '^original U4 checked pure-call owners: [1-9][0-9]*$' "$evidence/original-u4.test.log"
  test "$(grep -Ec '^original U4/[0-9]+ time=.*engine: Cranelift' "$evidence/original-u4.test.log")" -eq 11
fi
