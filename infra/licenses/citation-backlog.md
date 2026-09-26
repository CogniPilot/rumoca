# Citation backlog

Sites where a published method is implemented without a citation, and the
citation is not applied yet because a pending change rewrites the file. Each
entry carries the exact text to paste, so applying it later is mechanical.
Delete an entry when it lands.

Every other site of the same pass is already cited in the source.

---

## `crates/rumoca-solver/src/runtime/event_newton.rs`

Damped Newton with a finite-difference Jacobian, no citation. Deferred while
the algorithmic-differentiation series replaces its finite-difference parts;
re-check the constant names against the rewritten file. Prepend a module
header:

```rust
//! Damped Newton solve for a discrete-frozen event system.
//!
//! Newton's method with a finite-difference Jacobian and a backtracking line
//! search: J. E. Dennis Jr. and R. B. Schnabel, "Numerical Methods for
//! Unconstrained Optimization and Nonlinear Equations", SIAM Classics in
//! Applied Mathematics 16, 1996. Chapter 5 covers the method and its local
//! convergence; section 5.4 gives the forward-difference step, whose optimal
//! relative size is the square root of the unit roundoff, which is what
//! `FINITE_DIFFERENCE_RELATIVE_STEP` is; section 6.3 covers the backtracking
//! line search that `NEWTON_LINE_SEARCH_STEPS` bounds.
```

## `crates/rumoca-solver/src/runtime/projection/tearing.rs`

The module header names OpenModelica as the source of the tearing method.
Deferred while the algorithmic-differentiation series rewrites the file.
Replace the sentences from `This is the causalized solve OpenModelica
performs:` to `where the dense block Newton diverges.` with:

```rust
//! row's exact explicit assignment. The method is the tearing of H. Elmqvist
//! and M. Otter, "Methods for tearing systems of equations in object-oriented
//! modelling", Proceedings of ESM'94, European Simulation Multiconference,
//! Barcelona, 1994, pp. 326-332, as presented in F. E. Cellier and E. Kofman,
//! "Continuous System Simulation", Springer 2006, chapter 7. It keeps the
//! nonlinear system at the tear dimension and never forms the dense, often
//! ill-conditioned Jacobian over the whole loop, so the block converges from
//! starts where the dense block Newton diverges. OpenModelica causalizes the
//! same loops the same way, which is a useful behavioural cross-reference when
//! a block's tear set is compared against it, not the authority for the method.
```

## `crates/rumoca-phase-autodiff/src/engine.rs`

Forward-mode source-transformation AD with no citation of any kind. Deferred
with the algorithmic-differentiation series. Append to the module header,
after the paragraph ending `makes it well typed.`:

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

---

## Attribution follow-ups

- `.github/workflows/ci.yml`: ship `THIRD_PARTY_LICENSES.md` and `NOTICE` in
  the release asset set, alongside the `rumoca` and `rumoca-lsp` binaries, and
  add a job step running `cargo xtask licenses --check` so a new dependency
  cannot land without regenerating the attribution file.
- `flake.nix`: add `cargo-about` to the dev shell so `cargo xtask licenses`
  runs without `nix run`.
- `crates/rumoca/tests/architecture_hardening_test/source_comment_hygiene.rs`:
  extend the existing gate to also reject `ported from <external project>`,
  `copied from <external project>`, and bare `see <path>.cpp` / `.hpp`
  references, which makes the no-derivation-from-copyleft-source property
  self-policing instead of audited by hand.
