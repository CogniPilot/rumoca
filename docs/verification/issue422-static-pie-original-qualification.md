# Issue 422: original U4 static-PIE qualification preparation

Source-only preparation based on `db18e3dd9b3c86475f1810428a09f9ab700c861f`.
No Cargo, linker, ELF inspection, or simulation gate in this document has run.
The original fixture is unchanged:
`crates/rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_422.mo`,
SHA-256 `bd73c220986a56968363ceae5b5671a3eaf69c4ec7b03aaa605bed520ad4d550`.

## Proof boundaries

The previously passing
`issue_422_memcmp_relocation_needs_no_process_symbol_export` test proves the
canonical host builder registers the real native-ABI memcmp address without a
process symbol lookup. Its retained `a137` receipt is not a static-PIE receipt.

The new existing-suite test
`reported_bug_runtime::issue_422_original_array_function_runs_without_native_fallback`
compiles the unchanged U4, lowers through the normal simulation entry, directly
compiles every retained checked pure-call owner, and checks eleven samples at
0, 0.1, ..., 1 against `exp(-t)`. Every sample must retain Cranelift execution,
no admission refusal, and no declined request. The direct table compilation is
necessary: `native_execution::backend` otherwise catches its failure and stores
`None` without adding it to the session receipt. The test logs the actual owner
count; if constant folding removes all owners, this test does not prove an
array-helper relocation was exercised. Keep the independent relocation gate.

The API offers Auto and Interpreter, not a RequireNative selector. Auto is used
only with mandatory postconditions that refuse an interpreter or fallback
result. No new backend, environment switch, or configuration surface is added.

## Smallest local attempt (GNU, not release-musl)

Use the existing source-keyed owned-cache guard, frozen tool and input hashes,
and exclusive lease. Preserve 600 seconds, 4 GiB aggregate RSS, 8 GiB owned
cache, 8 GiB scratch free, 16 GiB available memory, four jobs/test threads,
`CARGO_INCREMENTAL=0`, and `CARGO_PROFILE_DEV_DEBUG=0` /
`CARGO_PROFILE_TEST_DEBUG=0`. Large files and temporary directories stay under
`$HOME/scratch/rumoca`. Never change global RUSTFLAGS or loosen the guard.

First the ordinary one-test gate can establish an independent native-model
control on the same frozen source:

```sh
cargo test --locked --offline --jobs 4 -p rumoca \
  --no-default-features --features fmu-packaging --test suite_core \
  reported_bug_runtime::issue_422_original_array_function_runs_without_native_fallback \
  -- --exact --nocapture
```

Then select just the final integration-test target:

```sh
cargo rustc --locked --offline --jobs 4 --profile test -p rumoca \
  --no-default-features --features fmu-packaging --test suite_core \
  --message-format=json -- \
  -C target-feature=+crt-static -C relocation-model=pic -C link-arg=-static-pie
```

Capture stdout JSON and stderr separately. Extract exactly one executable from
this invocation's `compiler-artifact` message with target name `suite_core`,
kind `test`, and `profile.test=true`; do not pick a glob or an older executable.
Keep its SHA-256, `file`, `readelf -h`, `readelf -l`, and `readelf -d` receipts.
Require ELF type DYN, x86-64 machine, no INTERP segment, and no NEEDED dynamic
library entries before running that exact executable:

```sh
"$issue422_executable" \
  reported_bug_runtime::issue_422_original_array_function_runs_without_native_fallback \
  --exact --nocapture
```

Require exactly one passed test, zero ignored. A linker refusal or wrong ELF
kind is an unsupported/unmeasured local static-PIE attempt, not a model pass.
The pinned local Rust has GNU and WASM standard libraries, no musl standard
library, and no musl-gcc. GNU static CRT link support is not yet measured.

Cargo documents that arguments after `cargo rustc --` apply only to the final
compiler invocation, which avoids changing all dependency fingerprints:
[Cargo rustc](https://doc.rust-lang.org/cargo/commands/cargo-rustc.html).
Actual ELF inspection is still required because link behavior depends on the
target and CRT settings:
[rustc codegen options](https://doc.rust-lang.org/rustc/codegen-options/index.html).
This is one final link plus one small simulation if dependencies are reusable;
time, additional bytes, and link feasibility remain unmeasured. The mandatory
source-key transition may rebuild first-party artifacts. If admission fails,
retain the refusal and stop; do not create an unbounded alternate cache.

## PR-reachable GNU qualification

The existing Linux `test` job now invokes
`.github/scripts/qualify-issue422-static-pie.sh` under one 600-second GNU
timeout. It reuses that job's owned native target and the pinned toolchain;
four Cargo/test/Rayon workers are explicit unless the owner already supplied
a budget. The helper passes static-PIE flags only to each final compiler
invocation, with `--profile test`, so the unit/integration targets are actual
test harnesses. No global RUSTFLAGS or separate dependency cache is introduced.

It selects exactly one Cargo JSON executable for each expected target name,
kind and test profile, verifies both real ELF files as x86-64 ET_DYN without
INTERP/NEEDED, and executes the existing memcmp unit and original U4 test.
Both must report exactly one pass, no failures and no ignored tests. U4 must
also retain at least one checked pure-call owner and eleven Cranelift receipts.
Both ELFs, source/input hashes, build/test logs and readelf output are uploaded
even on failure. This is source-only CI preparation; neither static link nor
execution has been qualified by this change. It is GNU static-PIE evidence,
not musl release/CLI packaging evidence.

## Authentic Linux release route

The existing `build-rust-binaries` job provisions the pinned Rust musl target
and musl-gcc, but is tag-only and checks no PT_INTERP rather than ET_DYN.
A separate Linux x86-64 musl qualification is needed before claiming the musl
release artifact is covered. Reuse that provisioning and pinned Cargo.lock,
the same existing suite_core feature set, and a dedicated musl target cache.
Use the above final-target flags with `--target x86_64-unknown-linux-musl` and
`CARGO_TARGET_X86_64_UNKNOWN_LINUX_MUSL_LINKER=musl-gcc` plus
`CC_x86_64_unknown_linux_musl=musl-gcc`. Do not add the release job's separate
`link-arg=-static` to this qualification, since the ELF must be static PIE.
Inspect the resulting artifact and run the exact same one-test gate.

A fresh musl dependency graph cannot reuse GNU target artifacts. Cost and
whether it fits the unchanged local bounds are unmeasured; do not initiate it
under a GNU cache lease. Existing tag-release CLI smoke can additionally run
the exact issue CLI command and inspect all eleven CSV samples, but it does
not replace the direct no-fallback test. Static-PIE harness success is distinct
from a release-artifact/CLI packaging receipt.
