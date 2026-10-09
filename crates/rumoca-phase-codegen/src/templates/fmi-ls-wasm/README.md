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
Non-Float64 typed accessors reject in this draft mapping. Models needing
capabilities outside the shared C profile are refused at export as described
above.

## C compiler

`build.rs` uses the standard cargo/cc environment for the wasm target:
`CC_wasm32_wasip2` and `AR_wasm32_wasip2` select the `wasm32-wasip2` C compiler
and archiver (a wasi-sysroot clang). With the nixpkgs `pkgsCross.wasi32` clang
set `NIX_CC_WRAPPER_SUPPRESS_TARGET_WARNING=1` so the wrapper's multi-target
advisory does not appear.

## Verification

- The FMI-LS runtime suite checks vendored WIT byte identity, WIT parsing,
  warning-clean `wasm32-wasip2` compilation of the C kernel and the Rust adapter,
  component validation, and Wasmtime traces of a decay model, MSL `Fourbar1`, and
  a bouncing ball and periodic sampled outputs against the native linked runtime.
- Focused gate: `cargo xtask verify template-runtimes --backend wasm`.

## Example

```sh
rumoca compile Plant.mo --model Plant --target fmi-ls-wasm --output generated
cargo build --release --target wasm32-wasip2 --manifest-path generated/Plant/Cargo.toml
wasm-tools validate generated/Plant/target/wasm32-wasip2/release/plant_fmi_ls_wasm.wasm
```
