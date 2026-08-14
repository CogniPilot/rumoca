# rumoca-reference roadmap

Slice 1 (the discrete/event core) is implemented. This file states what slices
2–4 have to add, what each one is allowed to assume, and what the Lean 4 port
would have to discharge.

The governing spec is
[SPEC_0037](../../spec/SPEC_0037_FORMALLY_VERIFIED_COMPILER.md). The wave that
added this crate moved it from `archive/deferred/` to `DRAFT`, because a source
file may only cite an active spec — that status change is **provisional and
awaiting maintainer sign-off**, and `DRAFT` means work has started under it, not
that any claim in it holds. SPEC_0037's adoption sequence puts "Initialization
and discrete events" at **V2**, which is the slice implemented here, and its
promotion criteria list "One IR semantics implemented — executable formal
definition" as a requirement. This crate is a candidate for that criterion; the
criterion also wants a proof assistant selected by maintainer vote, and that has
not happened.

## What slice 1 established

Two rules were not obvious from reading MLS §8.3.5.1 in isolation, and both were
found by the reference failing rather than by reasoning:

1. **`pre` is seeded, not redefined, at initialization.** Reading §8.6's
   "for all variables `v`, `v = pre(v)`" as "`pre` is the identity during
   initialization" makes `edge(b)` identically false and leaves
   `when initial() then ...` unable to fire, contradicting §8.6's own rule that
   such a clause is active there. The reading that satisfies both is that §8.6
   constrains the *values* `pre` starts from.
2. **Only the condition's `pre` value is seeded; the condition itself stays
   live.** `pre(b)` is evaluated once per instant at the left limit; `b` is
   solved with the other equations. This took two wrong turns, both worth
   recording because each looked right:

   - Re-solving the whole system to an inner fixed point spins forever on a
     self-rescheduling guard `when time >= nextTime` whose body advances
     `nextTime`. With `pre` held fixed that inner system has **no solution** —
     the condition is true exactly when the body has not run.
   - Latching *both* limits and freezing the buffer terminates, but is broader
     than any section supports, and it breaks a cascade: `when time >= 0.5 then
     x = 3` beside `when x > 2 then y = 1` samples `x > 2` before `x` is
     written, so the second clause never fires. Both compiler sessions give
     `y = 1`, and so does Appendix B.

   What resolves both is doing one *ordered pass* per Appendix B iteration
   rather than an inner fixed point, and letting the outer loop — which advances
   `pre` — carry the propagation. Registry row FS-EQN-019, filed `SpecSilent`
   because Appendix B says "solve" without saying what that means for an inner
   system with no solution.

Both rules are stated at their implementation sites, not only here. The second
is also why `tests/differential.rs` carries `StateConditionCascade` and
`SelfReschedulingCounter` together: they pull in opposite directions, and a
change that satisfies only one fails the other.

## Slice 2 — continuous coupling via supplied trajectories

Scope: state events, `reinit`, and `noEvent`.

* **State events.** Slice 1 schedules only crossings whose instant is
  computable from `time`. Slice 2 must locate a crossing of a condition over a
  supplied continuous trajectory. The reference must not acquire a root finder:
  the host supplies the trajectory *and* the crossing instants it located, and
  the reference checks the crossing is consistent with the trajectory it was
  given. That keeps root-finding accuracy out of the oracle, which is what
  makes a disagreement adjudicable — see registry FS-SIM-011, a divergence that
  is entirely about search accuracy.
* **`reinit`.** MLS §8.3.6. Needs a notion of a state whose value the event may
  overwrite, which slice 1 does not have because its continuous variables are
  read-only.
* **`noEvent`.** MLS §3.7.5. Requires distinguishing relations that generate
  events from relations that do not — slice 1 treats every relation in a
  when-condition as event-generating.
* **Assumption slice 2 may make:** the supplied trajectory is exact between
  events. Discretization error is out of scope in every slice.

Exit evidence: the differential harness runs models with state events on both
solver sessions, and every disagreement is either fixed or has a registry row.

## Slice 3 — clocked partitions

Scope: MLS §16. `Clock()`, `sample`/`hold`, `subSample`/`superSample`/
`shiftSample`/`backSample`, `previous`, and clocked `when`.

The exact rational lattice already exists in the compiler
(`rumoca_core::ClockLattice`, `ClockRational`), and slice 3 must **not** reuse
it: the reference re-deriving the tick grid independently is the only way the
comparison means anything. A reference importing the compiler's lattice would
agree with the compiler about tick instants by construction.

Registry rows in reach: FS-CLK-001 … FS-CLK-005.

Exit evidence: reference tick grids agree with `PeriodicEventSchedule` over a
generated set of lattices, including the `2^63` tick index the compiler's own
tests pin.

## Slice 4 — arrays and vector activation

Scope: MLS §8.3.5's vector activation, and enough of §10 to express it.

This is the slice that reaches **FS-EQN-006** — "a vector activation is realised
as one Boolean buffer per element with the activation `edge(b1) or … or
edge(bn)`, which is not the edge of the disjunction of the elements". Slice 1 is
scalar-only and therefore cannot say anything about it, which is worth stating
plainly: the vector-when semantics is one of the harder-won results in this tree
and the reference does **not** currently cover it.

Exit evidence: a hand-written case for the `Modelica.Blocks.Logical.LogicalDelay`
shape, where folding the vector into one `or` deletes the activation outright.

## The Lean 4 port

**Not a decision this crate may make.** SPEC_0037's promotion criteria require
"Proof assistant selected — recorded evaluation and maintainer vote". What
follows is a proposal for that evaluation, not a selection.

### Why the port is cheap from here

Slice 1 was written to be transliterable. Every function is total or returns an
explicit error; there is no mutation that is not a `BTreeMap` insert; there is
no iterator whose termination is not bounded by an explicit ceiling; and the two
loops are the only recursion-adjacent constructs. The transliteration target is
roughly:

| Rust | Lean 4 |
|---|---|
| `Value`, `Type`, `Expr`, `Equation`, `Model` | inductive types, near-verbatim |
| `eval` | a total function into `Except EvalError Value` |
| `expand` | a total function; its output shape is a refinement (no `When` remains) |
| `sweep_to_fixed_point`, `event_iteration` | fuel-indexed recursion; the `Options` ceilings are the fuel |
| `simulate` | fuel-indexed, returning a `Trace` |

### The proof obligations, in dependency order

1. **`expand` is total and eliminating.** Every equation in `expand(m).model` is
   `Assign` or `InitialAssign`. Discharges the "no `when` survives" assumption
   every later lemma makes. *Small; a structural induction.*
2. **`eval` is deterministic and total on a well-typed expression.** Needs a
   typing judgement the crate currently only enforces dynamically, so this
   obligation drags a type system in with it. *Medium.*
3. **The iteration settles in `n` passes for an acyclic equation set.** State
   it as: if the dependency graph of the `Assign` equations is acyclic, the
   Appendix B loop reaches `z == pre(z)` in at most `n` iterations for `n`
   equations, because each pass propagates at least one more level of the
   topological order. This is the lemma that turns the `max_iterations` ceiling
   from a safety net into a bound. *Medium; the interesting one.*
4. **The event iteration terminates.** Given 3, each iteration either changes a
   discrete value or stops; over finitely many discrete variables this needs a
   well-founded measure. It is **not** true in general, and finding an honest
   witness took two attempts:

   - `when b then b = not pre(b)` is **not** a witness. It converges, because
     advancing `pre` clears the edge after one iteration, so the flip happens at
     most once. Pinned as
     `an_activation_driven_flip_converges_because_the_edge_clears`.
   - A *bare* discrete equation `a = not pre(a)` **is** one: it is outside any
     activation, so nothing clears it, and every iteration flips `a`. The
     reference reports `NotConverged` at its ceiling. Pinned as
     `a_self_negating_discrete_equation_does_not_converge`.

   So the theorem must be conditional, and identifying the right condition —
   plausibly "every cyclic `pre` dependency passes through an activation" — is
   the real work.
5. **`edge` at the initialization instant is false for an unchanged
   condition.** The formal statement of registry rows FS-EQN-001 and FS-EQN-002.
   Given the seeding rule, this is nearly by definition — which is the point of
   having chosen that seeding rule.
6. **A crossing fires at most once per instant.** FS-EQN-005. Follows from
   buffers being latched once per instant.

### Which registry rows become lemmas

Rows whose statement is a property of the *semantics* can become lemmas about
the Lean model. Rows whose statement is a property of the *compiler* cannot —
they need a refinement theorem between the compiler and the model, which is
SPEC_0037's per-phase obligation, not this crate's.

| Row | Becomes |
|---|---|
| FS-EQN-001, FS-EQN-002 | Lemma 5 above. |
| FS-EQN-003, FS-EQN-004 | Lemmas about `initial()` and the condition-memory seed. |
| FS-EQN-019 | Lemma 4's side condition, and the reason it is `SpecSilent`. |
| FS-SIM-017 | Lemma 5's hypothesis: it *is* the seeding rule, stated as an interpretation of §8.6. |
| FS-EQN-005 | Lemma 6 above. |
| FS-EQN-006 | Nothing until slice 4 exists. |
| FS-SIM-009 | Already a *consequence* in slice 1 rather than an oracle result — see `tests/semantics.rs`. A candidate for re-tiering from `OracleImplied` once machine-checked. |
| FS-SIM-010, FS-SIM-011 | Neither. Both are solver-session divergences; a semantics model has nothing to say about them. |

### The `MachineChecked` status proposal

`EnforcementStatus` today is `Enforced | RecordedDivergence | Unimplemented`
(`crates/rumoca-contracts/src/registry/formal.rs`). A fourth variant is worth
adding **only when the first proof actually checks**, and it should be defined
narrowly:

> `MachineChecked` — the statement is a theorem about the reference semantics,
> proved in the named proof assistant at the named revision, and the reference
> semantics is differentially validated against the compiler. It does **not**
> assert that the compiler implements the statement; that requires a refinement
> theorem, and until one exists a `MachineChecked` row must also carry a
> `Test` pin like an `Enforced` one.

That last sentence is the load-bearing one. SPEC_0037 is explicit that
"OMC/MSL parity, fuzzing, property tests, and differential traces remain
validation evidence for the formal definitions. They are not proof evidence" —
so a status that let a proof about the *model* be read as a claim about the
*compiler* would be exactly the overstatement SPEC_0037's trusted-computing-base
section exists to prevent.

Adding the variant needs, at minimum: the proof-assistant version pinned in the
build manifest, a reproducible proof CI job with a bounded runtime (SPEC_0037
promotion criterion "Proof CI bounded"), and a new invariant in
`tests/formal_statement_invariants.rs` requiring a `MachineChecked` row to name
its theorem and its revision.
