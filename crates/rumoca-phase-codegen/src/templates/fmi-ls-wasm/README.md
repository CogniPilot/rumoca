# `fmi-ls-wasm`

## Use case

Use this experimental target to package a Rumoca Co-Simulation component for a
WebAssembly Component Model host implementing the pinned FMI-LS-Wasm WIT draft.
It is intended for sandboxed simulation services and portable component hosts,
including browser hosts that transpile the component ABI; it is not a
`wasm-bindgen` module.

## Contract

- Readiness 0: this is a tested implementation of a pinned non-normative draft,
  not an adopted layered standard.
- Input: one checked FMI component aggregate and its executable Solve profile.
- Output: a Rust `cdylib` crate that compiles the shared FMI 3 C kernel and
  adapts the pinned WIT world onto its ABI, the exact pinned WIT tree, the
  vendored FMI 3 headers, and upstream license and revision evidence.
- Build target: `wasm32-wasip2`; `wasm32-unknown-unknown` is not this target's
  ABI.
- Upstream contract: `modelica/fmi-ls-wasm` commit
  `c1aac17d392bec989fe2d059db3cc57bb7a0fff5`, a non-normative draft.

## Kernel reuse

The component holds no solve decision. The C sources under `csrc/` are the same
translation units the `fmi3` target renders from the same Solve IR facts, so
initialization projection (settled parameter initialization), algebraic
projection, and scalar state-event location all run inside the C kernel. The
crate's `build.rs` compiles those sources for `wasm32-wasip2` with the `cc`
crate, and `src/lib.rs` is a thin adapter that forwards each WIT call
(`instantiate-co-simulation`, `enter`/`exit-initialization-mode`, `do-step`,
`get`/`set-float64`, `terminate`, `reset`) to the kernel's prefixed FMI 3 ABI,
holding the `fmi3Instance` pointer per resource.

Because it renders the same kernel, this target advertises the same capability
profile as `fmi3`. Periodic Modelica `sample` clocks are internal time events
located and executed inside `do-step`, just as in the native Co-Simulation
kernel. They do not expose FMI Clock variables or require WIT Clock accessors.
A model outside that profile (general events, runtime
event history, external calls or tables, random operators, or an algebraic
system the shared projection cannot admit) is refused at export with the same
`unsupported-feature` diagnostic as `fmi3`.

## Unsupported

The implemented profile is FMI 3 Co-Simulation with settled parameter
initialization, algebraic projection, static parameter-dependent assertions, and
scalar state events located inside `do-step`. Model Exchange, Scheduled
Execution, early return, intermediate update, state serialization, public FMI
Clock accessors, and
the derivative APIs are not advertised; those WIT calls reject without mutation.
Float64, Int32, Int64 enumeration, and Boolean accessors forward typed scalar
and array batches to the shared kernel. Other typed accessors reject. Models needing
capabilities outside the shared C profile are refused at export as described
above.

## C compiler

`build.rs` uses the standard cargo/cc environment for the wasm target:
`CC_wasm32_wasip2` and `AR_wasm32_wasip2` select the `wasm32-wasip2` C compiler
and archiver (a wasi-sysroot clang). With the nixpkgs `pkgsCross.wasi32` clang
set `NIX_CC_WRAPPER_SUPPRESS_TARGET_WARNING=1` so the wrapper's multi-target
advisory does not appear.

The C kernel enables WebAssembly's fixed-width SIMD (`simd128`). Independent
tensor lanes and copies can therefore use native vector instructions when
Clang proves them safe. The component host must support this WebAssembly
feature. Fast math and floating-point contraction are disabled: reductions
retain their evaluation order, and checked stores retain their first-fault
behavior. Enabling SIMD does not establish that a particular loop vectorized;
inspect the built component and measure its runtime for that claim.
The kernel uses fixed compiler flags. Nonempty `CFLAGS`, `TARGET_CFLAGS`,
`CFLAGS_wasm32_wasip2` or `CFLAGS_wasm32-wasip2` are refused at build time,
because `cc` would append them after the kernel flags. Compiler and archiver
selection through `CC_wasm32_wasip2` and `AR_wasm32_wasip2` remains supported.

## Verification

- The FMI-LS runtime suite checks vendored WIT byte identity, WIT parsing,
  warning-clean `wasm32-wasip2` compilation of the C kernel and the Rust adapter,
  component validation, and Wasmtime traces of decay, bouncing-ball and periodic
  sampled-output models
  against the native linked runtime, plus typed-array access,
  transactional rejection, and reset through the shared FMI 3 accessors.
- MSL `Fourbar1` retains an independent native BDF trace against pinned OMC,
  and an explicit export refusal for its 27 published String variables. This
  profile has no String accessor; that refusal gives no component trace credit.
  Fourbar component qualification remains pending implemented String support.
- Focused gate: `cargo xtask verify template-runtimes --backend wasm`.

## Example

```sh
rumoca compile Plant.mo --model Plant --target fmi-ls-wasm --output generated
```

The default CLI's `component-build` feature prepares an immutable inventory,
builds its exact slot with Cargo and wasm-tools, then publishes `generated/Plant.fmu`.
Set `TMPDIR` for owned build staging and `CARGO_TARGET_DIR` for a reusable
Cargo component cache under its `rumoca-component-build` child directory.
The archive contains Co-Simulation-only metadata,
`extra/org.modelica.fmi-ls-wasm/manifest.xml`, and
`binaries/wasm32-wasip2/<modelIdentifier>.wasm`. Source-only preparation is explicit
through `prepare_packaged_target`; it does not produce a successful FMU export.
Published String variables are refused because this profile has no String accessor.
