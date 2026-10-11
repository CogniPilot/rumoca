# Issue 424: parameter extents and Boolean controls

Test-only candidate based on `e69072c2a831f227099f5cd6eed585acb0f6a878`.
All three added tests are **UNRUN**. No Cargo, OMC, native execution or memory
measurement was performed while preparing them; no production fix is added.

The retained original C3 remains unchanged and intentionally rejects its
declared [3,2] versus constructed [2,3] mismatch. The issue also reports a
conforming reverse-order constructor, parameter ranges and Boolean masks.
Existing compiler regressions cover the literal ranges and reverse-order Real
parameter values, but not parameter extents or Boolean mask constructors.

[MLS 3.6 §10.4.1.2](https://specification.modelica.org/maint/3.6/arrays.html#constructor-with-several-iterators)
defines multiple iterators through nested constructors, with the last textual
iterator outermost. The new controls use compatible declared dimensions for
each order, without changing the original invalid model. Their independently
specified row-major values include both true and false Boolean entries.

Three ordinary compiler-unit tests add five conforming source cases:

- Parameter extents nx=3,nr=2, both iterator orders, exact dimensions and all
  six Real values through the existing checked DAE NumericEvaluator.
- Boolean parameter masks nx=3,nr=4, both iterator orders, exact dimensions
  and all twelve Boolean values through the same checked evaluator. Every
  index is valid; this imposes no Boolean short-circuit requirement.
- The reported ordinary Boolean binding with parameter extents and the valid
  reverse order, checked through compilation and its [3,4] declaration shape.
  This third control is not claimed to prove runtime mask values.

The existing module is already registered in rumoca-compile's unit suite.
No new test target, feature, dependency or custom CI lane is needed. Ordinary
`cargo xtask verify workspace` runs workspace nextest and includes this crate.

Smallest combined source gate, exactly seven tests and zero ignored:

```sh
cargo test --locked --offline --jobs 4 -p rumoca-compile --lib \
  session::tests::reported_bug_regressions::issue_424_ \
  -- --nocapture --test-threads=4
```

The new source cases still require actual qualification; the retained original
OMC rejection and previously measured compiler passes are historical receipts
at their own source hashes. They do not measure this candidate.

Governing ownership/process contracts remain SPEC0007, SPEC0022, SPEC0032 and
SPEC0033. Only test observations use small scalar views; no canonical tensor
expansion, admission relaxation or source declaration reauthoring is added.
