# Issue 422: linked memory libcalls in GNU static PIE

Source `ec97bb40d86452d12ff18d4d0859caaf9faf8ce2` actually passed the
GNU static-PIE qualifier after the canonical host builder registered linked
C ABI addresses for memcpy, memmove, memset and memcmp. The same Cranelift
`default_libcall_names` callback supplies registration names and relocation
names. No process symbol export or model-specific path was added.

The [3653 BEFORE](issue422-gnu-static-pie-3653-before.md) remains retained:
memcmp passed, but original U4 failed resolving memcpy before simulation.
The unchanged original U4 now retains one checked pure-call owner, explicitly
compiles that table and completes all eleven samples at t=0,0.1,...,1.
Every sample checks the independent exp(-t) oracle and records Cranelift,
no refusal and no declined operation. Existing tolerances are unchanged.

| Actual exact selector | Passed | Failed | Ignored |
|---|---:|---:|---:|
| `emit::host_runtime::tests::issue_422_memcmp_relocation_needs_no_process_symbol_export` | 1 | 0 | 0 |
| `emit::host_runtime::tests::issue_422_memory_libcalls_relocate_without_process_symbol_export` | 1 | 0 | 0 |
| `reported_bug_runtime::issue_422_original_array_function_runs_without_native_fallback` | 1 | 0 | 0 |

Both relocation controls deny resolver fallback. The added control uses
Cranelift 0.125.4's `LibCall::signature` and `ExternalName::LibCall` for
actual native calls: disjoint memcpy, overlapping memmove in both directions,
and memset's int-to-byte truncation and zero-count behavior. Exact destination
contents, untouched sentinels and C pointer return values are checked. The
module remains alive throughout all calls. The original memcmp control is
unchanged. Cranelift's pinned primary ABI definitions are
`cranelift-codegen/src/ir/libcall.rs` and its default name mapping is
`cranelift-module/src/lib.rs`; their C pointer/size/int signatures match the
linked declarations. Registration stays in the existing execution adapter,
consistent with SPEC0007, SPEC0029 and the earliest-owner requirement of
SPEC0033.

The existing mandatory Linux CI step executes the extended
`.github/scripts/qualify-issue422-static-pie.sh`. It builds the two Rust test
targets with `-C target-feature=+crt-static -C relocation-model=pic
-C link-arg=-static-pie`. U4 uses `-p rumoca --no-default-features --features
fmu-packaging --test suite_core`. Each exact test must report one pass and
zero failures/ignored tests; U4 additionally requires eleven native receipts.
All three retained executables passed ELF64/x86-64/DYN, no INTERP segment
and no NEEDED entry checks. The two relocation tests share one library test
executable. This is actual GNU Linux qualification; macOS and Windows runtime
qualification and final combined CI are separate, pending gates.

The matched Nix full shell used GNU Rust 1.95.0-nightly
`6a979b3e32522049d0acb4a47f7ae44b7c8abfd5`, GCC 15.2 and glibc 2.42-67
static libraries. Only the matched static output was prepended to the existing
LIBRARY_PATH. Limits remained 600 seconds, 4 GiB aggregate RSS, 8 GiB owned
cache, 16 GiB available host memory and 8 GiB scratch reserve; jobs, Rayon
and test threads were four, debug info zero and incremental compilation off.

This reused the owned BEFORE target. The complete guardian exited zero in
244.506 seconds with no stop or signal. Peak aggregate RSS was 2,877,460 KiB;
peak allocated cache was 1,961,476,096 bytes. The library build took 37.94s
and the U4 target build 3m20s; U4's Rust test took 0.02s. These are distinct
build and test observations, not cold build or model throughput claims.

Local evidence is under `$HOME/scratch/rumoca/workshop427-staticpie-20261012`,
with a new `artifacts/gnu-static-pie-ec97-after` directory. The owned target
had zero process references before the run. All 4032 tracked entries matched
the frozen inventory after termination. The complete receipt hashes raw
Cargo output, toolchain preflight, ELF checks, binaries and test logs.
The original model SHA256 is
`bd73c220986a56968363ceae5b5671a3eaf69c4ec7b03aaa605bed520ad4d550`;
Cargo.lock SHA256 is
`f25d24bea4d1a06aece3263cd654a74e1fe9b6c5731d80603e2d23abbc2c01c6`.

SHA256 bindings:

- recipe: `951c4358467eebfa9d8a63d0bccfe8c37d8fa0605eeb6a22f69449cf94f4cbfe`
- complete receipt: `8fc1d0cbd4c4b244241a1b28c700ac4ae47d764c879eb769c74bd235f233dcc5`
- guardian: `1dd6b89ad4120501b5d5d22f448f1fa178c4393b91b15c5384d6cf073739f54d`
- tracked source inventory: `02aff6ad026e9d769db7ecbc7a3a7a7772167c5799a4aad687985cc6c48406c3`
- wrapper: `d2e0334246648dfdb0af0f4441c6d941e2586a590d6be3b5e305d04b08138668`
- memcmp/memory executable: `a0c05a824b6a79924c19eccfde019d4e55f39ccbf4c96dc51e7d7f5b96dd7871`
- memcmp test log: `325b007e530f30d5fe78fe3141245a2ab09adcc71885c1d99b04ef2c38016062`
- memory test log: `e5ff864bb89fdd90be4abaa2281cf2282c302b2782688f4df8a0985626620722`
- U4 executable: `54766256acbbd407dcf45dda61f5fb76dffb5005fb556f38a2f2e7a13db66a24`
- U4 test log: `ed92bf98f4b54b4f30a996adccbe8b1f6bc9f6675fa055886a95950edbfde0e5`

Issue closure remains contingent on the qualified fix landing and fresh CI.
