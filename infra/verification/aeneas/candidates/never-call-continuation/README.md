# Never-returning calls preserve their outcome

## R4 result — 2026-09-08 20:07 UTC

Ready for independent bounded review; **not accepted or adopted**. Patch
`29e7916c`, binary `3e48c1cc`. Exact inputs, tool/artifact hashes, commands
and terminal results are in [run-evidence-r4.json](run-evidence-r4.json).
Writer Codex; reviewer Claude. Reservation through 2026-09-08T20:20:00Z.
Governing anchors: SPEC_0033 §§2/2a/4/6 and experimental SPEC_0037 §§2/6.

### Claude's R3 review requests

1. The patch lost its final blank context line: GNU patch accepted it, but
   git rejected it. R4 preserves that line, passes
   `git apply --check --whitespace=error-all`, and was rebuilt/replayed with
   the new identity. This changes no OCaml source semantics.
2. R4 records SHA-256 identities for both frozen Charon executables and fresh
   extraction rows for **both** Rust fixtures, not just mutable path names.
3. [Laws.lean](Laws.lean) now includes the positive-count ordinary-loop body
   and whole-function step laws, plus a successful one-iteration witness.

The [R3 receipt](run-evidence-r3.json) is frozen historical evidence, not a
claim about current patch/Laws bytes. Its original patch is retained at
`dev/never-call-review-9p1kiK/r3-corrupt-eof.patch`; the R4 receipt binds both.

### Executed evidence

- Both actual Rust fixtures emit with zero opaque functions; their unedited
  generated Lean Types/Funs compile. Thirteen guarded laws pass over those
  functions, not a handwritten replacement compiler.
- Seven native tests, strict Clippy and Rust/OCaml format checks pass. Seven
  OCaml documentation warnings also occur on the unchanged upstream source;
  they are recorded, not suppressed.
- A real Rust 7-to-9 mutation emits/typechecks but fails the unchanged native
  oracle and Lean law. Its separate wrong-answer witness compiles.
- Replacing the Never call with a panic still passes all three native tests,
  but fails the unchanged Lean error/divergence laws. A separate witness proves
  the mutant erased the call.
- Both unchanged DAE-shaped source-retention diagnostics emit past `Cps.Unit`.
  The first retains external `expect_failed`; the deeper route retains two
  `Display::fmt` methods and `panic_display`. No external axioms were supplied;
  those complete modules are **not** admitted or Lean-typechecked.
- Three previous loop fixtures emit/typecheck; all thirteen of their unchanged
  guarded laws pass. Coq explicitly refuses the new Never elimination.

### Producer repair

The original symbolic call constructor gave every call an ordinary Unit
continuation, including calls declared to return Never. Final evaluation then
refused the source-retained Option::expect failure branch with `Cps.Unit`.
The repair retains the actual call and its failure/divergence and eliminates
only its impossible successful result. It never guesses that an arbitrary
Never-returning call means panic.

Existing symbolic empty expansion and Pure empty-match vocabulary suffice.
The scoped expected result type is explicit: initialized at function entry
and rebound from the owning backward-function, loop and join signatures.
It is not inferred from a hypothetical panic, whose wrapper can differ.
Symbolic/Pure checks require a Never scrutinee; Lean prints `nomatch`.

Nine declared OCaml owners are changed (82 insertions, 26 deletions):
`interp/InterpStatements.ml`, `symbolic/SymbolicToPureExpressions.ml`,
`pure/PureTypeCheck.ml`, `extract/Extract.ml`, `Translate.ml`,
`symbolic/SymbolicToPureCore.ml`, `symbolic/SymbolicToPure.ml`,
`pure/PureMicroPassesLoops.ml`, and `pure/PureMicroPassesGeneral.ml`.
Predecessor: float-R2 plus loop-R3, Aeneas `8abfa150`.
R1/R2 exposed empty-match assumptions in loop-output rewrites; the final
owning passes preserve or transform the result type according to their contract.
Ordinary matches retain branch-derived result types.

### Explicit limits

The backward-function rebinding is **not exercised** by these fixtures:
the borrowed-return example eliminates Never in its forward scope.
The ordinary positive-loop law proves the exact one-step recurrence, not
termination for every count; count=1 has a separate success proof.
Claude's extra five-site probe remains relayed, not independently counted here.

The pinned Rust/Charon/Aeneas translation, Lean kernel and existing library
semantics remain trusted. Panic formatting, hooks, I/O and unwinding are not
modelled by these laws. No production Rumoca source, brand, live tool pin,
serialized IR format or existing library model changed.

Diagnostics and mutations live in `dev/never-call-review-9p1kiK/`.
This is bounded translator evidence, **not** another UnitDerivative relation,
a proof of the whole translator, full upstream-suite success or quick/full
gate success. UnitDerivative remains 1/18.
