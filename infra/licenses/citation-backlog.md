# Citation backlog

Sites where a published method is implemented without a citation, and the
citation is not applied here because another working tree already has the file
open. Each entry carries the exact text to paste, so applying it later is
mechanical. Delete an entry when it lands.

Everything else from the same pass is already applied; see the commit that
added this file.

---

## `crates/rumoca-phase-autodiff/src/engine.rs`

Forward-mode source-transformation AD with no citation of any kind. Append to
the module header, after the paragraph ending `makes it well typed.`:

```rust
//!
//! # References
//!
//! The method is forward-mode algorithmic differentiation by source
//! transformation: A. Griewank and A. Walther, "Evaluating Derivatives:
//! Principles and Techniques of Algorithmic Differentiation", 2nd ed., SIAM
//! 2008, doi:10.1137/1.9780898717761, chapter 3. Emitting the tangent
//! statement before the primal it belongs to is what chapter 3 calls the
//! statement-level tangent recursion, and is why the derivative of a
//! self-assignment reads the incoming value. The structural-zero treatment,
//! where a derivative known to vanish is absent rather than a literal zero, is
//! the activity analysis of chapter 6.
```

## `crates/rumoca-phase-autodiff/src/emit.rs`

Same crate, same method, no citation. Append to the module header, after the
paragraph ending `it was minted for.`:

```rust
//!
//! The tangent function this module writes is the forward-mode source
//! transformation of Griewank and Walther, "Evaluating Derivatives", 2nd ed.,
//! chapter 3; see [`crate::engine`] for the full reference.
```

## `crates/rumoca-eval-solve/src/sparsity.rs`

Jacobian sparsity derivation with no citation. Prepend a module header (the
file currently starts with `use std::collections::BTreeSet;`):

```rust
//! Derive the sparsity pattern of a compute block's Jacobian.
//!
//! Propagating index sets forward through the operation list to obtain, for
//! each output, the inputs it can depend on is the standard sparsity-pattern
//! derivation of A. Griewank and A. Walther, "Evaluating Derivatives:
//! Principles and Techniques of Algorithmic Differentiation", 2nd ed., SIAM
//! 2008, doi:10.1137/1.9780898717761, chapter 7. The pattern this produces is
//! structural, therefore conservative: an entry can be structurally present and
//! numerically zero, never the reverse, which is exactly the guarantee the
//! column coloring in `rumoca_ir_solve::StructuralPattern::column_coloring`
//! needs. The bit-vector propagation form is C. H. Bischof, A. Carle, P.
//! Khademi and A. Mauer, "ADIFOR 2.0: automatic differentiation of Fortran 77
//! programs", IEEE Computational Science and Engineering 3(3):18-32, 1996,
//! doi:10.1109/99.537089.
```

## `crates/rumoca-contracts/data/formal_statements.toml`, row `FS-EXPR-003`

Not a missing citation: a wording fix. The `oracle` field reads

```
oracle = 'OpenModelica (omc), read from its generated C for Modelica.Thermal.FluidHeatFlow.Examples.PumpDropOut'
```

"read from its generated C" is the phrase in this repository most likely to be
misread as source derivation, even though what it records is a behavioural fact
about one model, and the rule itself is grounded in MLS section 3.7.5 by the
row's own `latitude_note`. Replace with:

```
oracle = 'OpenModelica (omc), read off its zero-crossing table for Modelica.Thermal.FluidHeatFlow.Examples.PumpDropOut'
```

The module header this row points at has already been clarified to say the rule
is derived from the specification and that OMC is an observed second opinion.

---

## Not deferred, but not done here either

These are follow-ups for whoever lands the license-attribution branch. They
touch files another working tree holds open.

- `.github/workflows/ci.yml`: ship `THIRD_PARTY_LICENSES.md` and `NOTICE` in
  the release asset set, alongside the `rumoca` and `rumoca-lsp` binaries, and
  add a job step running `infra/licenses/generate.sh --check` so a new
  dependency cannot land without regenerating the attribution file.
- `flake.nix`: add `cargo-about` to the dev shell so
  `infra/licenses/generate.sh` runs without `nix run`.
- `crates/rumoca/tests/architecture_hardening_test/source_comment_hygiene.rs`:
  extend the existing gate to also reject `ported from <external project>`,
  `copied from <external project>`, and bare `see <path>.cpp` / `.hpp`
  references, which makes the no-derivation-from-copyleft-source property
  self-policing instead of audited by hand.
- `infra/verification/embedded-head-to-head/` and
  `embedded-head-to-head.json` are untracked and not gitignored, so a blanket
  `git add -A` would commit an 80 KB benchmark rig unreviewed. Decide: commit
  deliberately with a README recording that the CasADi comparator is generated
  at run time and pinned by digest, and that the CasADi banner on any generated
  file is the license record and must never be stripped; or gitignore it.
