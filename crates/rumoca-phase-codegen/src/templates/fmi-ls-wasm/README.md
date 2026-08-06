# `fmi-ls-wasm`

## Use case

Use this experimental target to package a Rumoca Co-Simulation component for a
WebAssembly Component Model host implementing the pinned FMI-LS-Wasm WIT draft.
It is intended for sandboxed simulation services and portable component hosts,
not browser `wasm-bindgen` applications.

## Contract

- Readiness 0: this is a tested implementation of a pinned non-normative draft,
  not an adopted layered standard.
- Input: one checked FMI component aggregate and its executable ODE profile.
- Output: a Rust `cdylib` crate, the exact pinned WIT tree, and upstream license
  and revision evidence.
- Build target: `wasm32-wasip2`; `wasm32-unknown-unknown` is not this target's
  ABI.
- The implemented profile is FMI 3 Co-Simulation without events, early return,
  intermediate update, state serialization, clocks, or derivative APIs.
- Upstream contract: `modelica/fmi-ls-wasm` commit
  `c1aac17d392bec989fe2d059db3cc57bb7a0fff5`, a non-normative draft.

## Unsupported

This target makes no FMI layered-standard conformance claim while the upstream
mapping is non-normative. Unsupported optional calls reject without mutation;
the component never reports them as successful no-ops. Model Exchange and
Scheduled Execution are not advertised by this target.

## Verification

- The FMI-LS runtime suite checks vendored WIT byte identity, WIT parsing,
  warning-clean `wasm32-wasip2` compilation, component validation, interface
  inventory, and a Wasmtime lifecycle/trace against the checked native kernel.
- Lifecycle and setter negative controls prove rejected calls are transactional.
- CI pins `wit-bindgen`, `wasm-tools`, the Rust target, and the upstream commit.
- Focused gate: `cargo xtask verify template-runtimes --backend wasm` runs
  `fmi_ls_wasm_component_validates_and_executes_pinned_lifecycle` and
  `fmi_ls_wasm_vendored_contract_matches_pinned_upstream_bytes`.

## Example

```sh
rumoca compile Plant.mo --model Plant --target fmi-ls-wasm --output generated
cargo build --release --target wasm32-wasip2 --manifest-path generated/Plant/Cargo.toml
wasm-tools validate --features component-model generated/Plant/target/wasm32-wasip2/release/plant_fmi_ls_wasm.wasm
```
