# CPU WASM sign semantics

`UnaryOp::Sign` previously imported JavaScript `Math.sign`. That preserves
negative zero and propagates NaN, unlike the canonical
`rumoca_core::modelica_sign` three-way comparison used by the shared scalar
evaluator. CPU WASM now emits the same ordered comparisons and selections:
positive values yield 1, negative values yield -1, and other values yield +0.
It no longer imports `env.sign` or installs `Math.sign` in the browser adapter.

The executable Wasmi regression first failed on the old `env.sign` import.
After the fix it passes without a host sign function and checks every output
bit against both the canonical core helper and shared scalar evaluation.
Inputs cover signed zero, signed subnormal/minimum-normal values, ordinary
values, infinities, and two NaN encodings.

```sh
cargo test -p rumoca-exec-wasm
cargo clippy -p rumoca-exec-wasm --all-targets --all-features -- -D warnings
```

All 13 execution-adapter tests pass, and strict linting passes. The canonical
semantics remain owned by the core/evaluation layers; no new public API or
Solve wire schema is introduced. The emitter follows MLS §3.7.1 and
SPEC_0022 EXPR-019. The complete combined-branch gates are reported separately.

Code budget excluding this note: 17 production lines added and 7 removed;
57 test lines added; three source/test files touched; net 67 lines added;
zero public items added or removed. The direct emitter helper keeps the
three-way operation in one bounded function rather than adding another host
semantics implementation.
