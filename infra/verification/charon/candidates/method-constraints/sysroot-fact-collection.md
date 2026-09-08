# Fact collection in the full-MIR sysroot

Diagnostic record. Describes how the pinned full-MIR sysroot is produced, why a
`RUSTC_WRAPPER`-based fact collector never runs on `core`/`alloc`/`std` along
that path, and which supported cargo entry does run one on them.

Changes no live pin and edits no driver or library. Every claim below is either
a quoted line from a pinned file or a recorded execution.

## 1. Where the sysroot comes from

Pinned Charon source: `/nix/store/2058cyr4b25jmx5809k7i36ijfh4cflw-source`
(`CHARON_COMMIT = b82d2748...`, 0.1.248).

`nix/full-mir-sysroots.nix` builds the sysroot by looping `cargo miri setup
--target=<t> --print-sysroot` over the eight targets in
`charon/rust-toolchain`, on `nightly-2026-08-18`. Line 19 of that file reads:

    unset CHARON_ARGS CHARON_USING_CARGO RUSTC_WORKSPACE_WRAPPER RUSTC_WRAPPER

so any wrapper the caller had set is cleared before the sysroot build starts.

## 2. The wrapper path is closed twice over

The `unset` is only the outer closure. Even with it removed, `cargo miri setup`
does not route sysroot compilation through a wrapper.

Measured by replicating the recipe outside Nix: same toolchain, same vendored
`CARGO_HOME/config.toml` written from `$(rustc --print sysroot)`,
`CARGO_NET_OFFLINE=true`, `--target=x86_64-unknown-linux-gnu`, with a logging
wrapper binary substituted for the real `rustc` and each invocation's
`--crate-name` appended to a log.

| cell | environment | exit | wrapper invocations |
| --- | --- | --- | --- |
| A | `RUSTC_WRAPPER` only | 0 | 0 |
| B | `RUSTC_WORKSPACE_WRAPPER` only | 0 | 6 |
| C | both set | 0 | 6 |

The six invocations in cells B and C carry `--crate-name` values `?`, `___`, and
`custom_local_sysroot`: cargo's own probe invocations and the synthetic sysroot
root package. No invocation names `core`, `alloc`, or `std`. The sysroot crates
are compiled by the miri driver itself, not by anything cargo hands to a wrapper.

The documented extension point on this path is `MIRI=<driver>`, which
*replaces* the miri driver rather than wrapping `rustc`. Taking it means
supplying a binary that satisfies the full miri driver contract, including
`MIRI_BE_RUSTC` mode.

## 3. The supported entry that does see the sysroot

`-Zbuild-std` compiles the sysroot crates as ordinary dependencies of the
caller's own build, so they pass through the caller's `RUSTC_WRAPPER` like any
other dependency.

Measured with the same logging wrapper, a one-function library crate as the
root package, and the same vendored `CARGO_HOME`:

    cargo build -Zbuild-std=core --target x86_64-unknown-linux-gnu
      exit 0, 13 wrapper invocations
      crates seen: core, compiler_builtins, build_script_build, bstd_root

    cargo build -Zbuild-std=std --target x86_64-unknown-linux-gnu
      exit 0, 37 wrapper invocations
      crates seen: addr2line, adler2, alloc, cfg_if, compiler_builtins, core,
        gimli, hashbrown, libc, memchr, miniz_oxide, object, panic_abort,
        panic_unwind, proc_macro, rustc_demangle, rustc_literal_escaper,
        rustc_std_workspace_alloc, rustc_std_workspace_core, std, std_detect,
        unwind, plus the root package and cargo's own probes

`core`, `alloc`, and `std` are each compiled through the wrapper. A fact
collector installed as `RUSTC_WRAPPER` therefore observes them, which is exactly
what the `cargo miri setup` path denies.

## 4. Limits of that entry

Stated so the result is not read as more than it is.

- `-Zbuild-std` is unstable. It is available on the pinned nightly, but it is
  not a stability-guaranteed interface.
- It produces sysroot artifacts inside the caller's target directory as part of
  a build, not a relocatable tree of the shape `full-mir-sysroots.nix` copies out
  of `--print-sysroot`. Section 6 shows that gap is closed by a copy.
- Cache invalidation does not cover the wrapper for these crates. They are
  non-workspace dependencies, so `RUSTC_WRAPPER` is invoked for them but is not
  part of their fingerprint: changing the collector binary alone will not force
  them to rebuild. `RUSTC_WORKSPACE_WRAPPER` closes that gap only for workspace
  members, which the sysroot crates are not. Any production use needs an
  explicit staleness input rather than relying on cargo to notice.
- Only `x86_64-unknown-linux-gnu` was measured. The pinned toolchain lists eight
  targets.

## 5. Probe identity and reproduction

The logging wrapper is retained beside this file as `sysroot-wrapper-probe.rs`.
It is a single Rust source, no shell script: it appends `crate=<value following
--crate-name>` to the path in `MIRI_PROBE_LOG`, runs the real `rustc` with the
arguments it was given, and propagates the child's exit status. Its unwraps are
deliberate, for the reason its header records: a low invocation count is the
interesting outcome here, so a probe that failed to log while letting the build
succeed would manufacture exactly that outcome.

The binary used for the recorded runs was built from a scratch copy of that
source and hashes to

    sha256 97be6471e7b163e6a99750cf262656b8a240def7ee320465fbe396ddc96853e3

Rebuilding from the retained path does not reproduce that hash, because rustc
embeds the source path; the two sources are token-identical modulo line
wrapping. The hash identifies the artifact that produced the numbers above, not
a reproducible build target.

Reproducing section 2 requires the vendored `CARGO_HOME/config.toml` from
`full-mir-sysroots.nix` lines 22 to 27 verbatim; without it `cargo miri setup`
fails offline before reaching any compilation, and a run that fails that way
also logs zero invocations. Read cell A's zero only alongside its exit status.

## 6. The output is usable as a sysroot, and RUSTFLAGS reach the sysroot crates

Two properties the entry in section 3 needs before it can replace the recipe.

**RUSTFLAGS reach the sysroot crates.** Building `-Zbuild-std=core` twice into
separate target directories, differing only in `RUSTFLAGS`:

| RUSTFLAGS | exit | `libcore-*.rlib` bytes |
| --- | --- | --- |
| `-Zalways-encode-mir` | 0 | 70075212 |
| unset | 0 | 69293860 |

The flag changes `core` itself by 781352 bytes, so it is applied to the sysroot
crate and is not silently confined to the root package. This establishes that the
flag arrives. It does not by itself establish that the resulting MIR coverage
equals what `cargo miri setup` produces; that comparison is open.

**The output assembles into a consumable sysroot.** From the
`-Zbuild-std=std,panic_abort` build with `-Zalways-encode-mir`, copying the 22
sysroot `*.rlib` files (every one except the root package's) into
`<sr>/lib/rustlib/x86_64-unknown-linux-gnu/lib/` yields a tree that
`rustc --sysroot=<sr>` accepts:

- a library crate using `Vec`, `String`, iterator adapters and `to_string`
  compiled against it, exit 0;
- a binary doing the same linked against it, exit 0, and ran, printing `0,1,2,3`.

No renaming or metadata rewriting was needed. Cargo emits the sysroot rlibs with
the `lib<name>-<hash>.rlib` names rustc already looks for.

Not covered: only `x86_64-unknown-linux-gnu`, only rlib crates, no proc-macro
host libraries, and no file-by-file comparison of the assembled tree against a
real `--print-sysroot` tree.

**The flag reaches every sysroot crate, not just `core`.** Two matched
`-Zbuild-std=std,panic_abort` builds into separate target directories, assembled
into two sysroots, compared crate by crate. Bytes, `-Zalways-encode-mir` minus
unset:

```
libcompiler_builtins  +834488    libstd                +1207072
libcore               +781352    libgimli               +509000
libproc_macro         +489984    librustc_demangle      +197056
libobject             +187592    libminiz_oxide         +130136
liballoc               +81480    libmemchr               +32544
libstd_detect          +28464    libaddr2line            +12488
liblibc                +12440    libhashbrown            +10712
libadler2               +9192    libpanic_unwind          +2776
librustc_literal_escaper  +776   libpanic_abort            +520
libcfg_if                   0    libunwind                    0
librustc_std_workspace_core 0    librustc_std_workspace_alloc 0
```

Total `+4528168` bytes across 155856410, about 3 percent. The four zero deltas
are facade and shim crates carrying essentially no non-generic, non-inline
bodies, which is the distribution the flag's stated effect predicts.

**One coverage test tried and rejected as uninformative.** Compiling the same
crate against both assembled sysroots with `-Zinline-mir=yes` and inline
thresholds raised to 5000, calling four non-generic std functions
(`env::current_dir`, `thread::yield_now`, `thread::available_parallelism`,
`env::var`), produced byte-identical MIR at both `-C opt-level=0` and `-O`.
That is recorded as inconclusive, not as a negative result: the MIR inliner
declines these callees for reasons unrelated to whether their bodies are
present, so the test does not discriminate and cannot be used either way. A real
coverage comparison against a `cargo miri setup` sysroot still needs a
metadata-level or Charon-level reader.

## 7. Verification status

RELAYED-UNVERIFIED. Every execution recorded in sections 2, 3 and 6 was run once,
in a scratch tree outside this repository, and has not been replayed
independently. The `unset` recipe in section 1 has been independently confirmed.
The invocation counts, crate-name sets, artifact sizes, and the assembled-sysroot
compile, link and run results have not.
