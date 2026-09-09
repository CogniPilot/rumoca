# Lean builtin-constructor identity

Candidate, not adopted or accepted. Composes the frozen `drop-control`
candidate, which itself awaits review. No production Rumoca source, live pin,
model allowance or UnitDerivative proof count changes. SPEC_0033 §§2a/4/6
governs; SPEC_0037 §6 (DRAFT) identifies the trusted translation boundary.

## Concrete defect

The actual DAE path reaches source `core::result::Result::ok`. Once drop-control
translation succeeds, Aeneas emits its body using bare `ok` for the Lean
result constructor. Lean instead resolves that name to the source method being
defined. Generated Lean refuses with an argument type mismatch.

The smaller Rust fixture `charon/fixtures/builtin-constructor-identity.rs`
demonstrates both refusal and wrong translation:

1. `shadow::ok` is an ordinary identity function. Its translated bare `ok`
   becomes a recursive self-call, so Lean cannot prove termination.
2. `shadow::fail` and `shadow::panic` must panic. Their names capture bare
   emitted failure constructors, producing ill-typed Lean.
3. The separate `capture` module contains an ordinary identity function and
   a neighboring function named `ok` that panics. Its predecessor translation
   **type-checks**, but `capture.identity` resolves its bare constructor to
   `capture.ok` and consequently always panics. Native Rust returns its input.

`CaptureCounterexample.lean` kernel-checks that wrong outcome for every U32
input and proves it differs from the Rust identity contract. Both theorems have
empty axiom sets. The unchanged `CaptureContract.lean` refuses that predecessor
output. This is semantic evidence, not just an extraction error or text diff.
The original full fixture remains in scope; selecting the capture module is a
separate diagnostic showing why successful Lean elaboration is insufficient.

## Earliest owner and change

`ExtractBase.builtin_variants` owns the five compiler-generated Lean result,
error and loop constructors. The patch maps them to their absolute
`_root_.Aeneas.Std` names. User definitions and local bindings therefore cannot
capture those references. Other backends' constructor maps are unchanged.

One trait-constant emitter in `Extract.ml` hard-coded `ok`/`Ok` instead of using
the map. It now obtains the return constructor through `ctx_get_variant`, the
same owner already used by the other emitters. This removes a duplicate policy;
the non-Lean mappings retain their previous `Ok` spelling.

There is no Rust renaming, generated-Lean editing, additional axiom, semantic
model replacement, relaxed borrow check, or new interpreter fallback. Compiler
runtime and generated C are unaffected: only proof-backend identifier spelling
and the duplicate spelling lookup change. This does not claim to fix every
possible identifier-hygiene problem in Aeneas.

## Evidence on the final bytes

`run-evidence.json` records 25 actual terminal results, commands, inputs and
hashes. Expected refusals are negative controls, not passing capability claims.

| Check | Result |
|---|---|
| Native fixture | 3 tests pass |
| Strict Clippy, Rust formatting, OCaml formatting | Pass, no added lint suppressions |
| Predecessor full fixture | Aeneas exits 0; Lean rejects captured constructors |
| Predecessor capture module | Generated Lean passes; wrong-answer theorem passes; correct source contract fails |
| Candidate full fixture and capture module | Generated Lean passes; all source constructor/identity/failure laws pass |
| Candidate against predecessor wrong-answer theorem | Refuses, as required |
| Actual source `Result::ok` and caller | Generated Lean and both laws pass |
| Actual DAE all-four root | Aeneas succeeds; whole DAE Lean admission remains open |

Six positive laws use no axioms. The composed Result/Option caller uses only
Lean's `propext`; every law guards its exact axiom set. Rust source bodies are
generated through Charon/Aeneas. The authored Lean states expected outcomes,
not a replacement implementation. Claims trust the pinned toolchain and
Aeneas's existing primitive/result/reference/drop abstraction. In particular,
the generic Result law does not prove observable Rust destructor effects.

The no-capture check over the existing compiled proof namespace observes 5,671
declarations and finds no last-component collision with these five names.
An injected colliding declaration makes the unchanged audit fail. This is only
a focused exposure check, not a proof that all names or translation steps in
the existing pilot are correct; compiled/source hashes are recorded separately.

## Upstream comparison

[Aeneas PR1326](https://github.com/AeneasVerif/aeneas/pull/1326), inspected at
head `b790499017c65d3f14a85829fd8af443d2592266`, addresses a related problem:
local variables shadowing a namespace prefix, reported in issue1098. Its
patch changes local basename collision tests, not builtin constructor
registration. By that mechanism it cannot repair the bare `ok` reference in
this fixture. That conclusion is code-based inference, not a replay of PR1326.
Broader name hygiene and full upstream regression remain separate work.

## Reproduction and boundaries

```console
nix build --impure --no-link --print-out-paths --file infra/verification/aeneas/candidates/builtin-identity/package.nix --max-jobs 1 --cores 4
```

Final candidate:
`/nix/store/78nc9mgx035096pdn34czgh5gzl7gb9w-ocaml5.2.1-aeneas-0.1.0/bin/aeneas`.
Patch SHA256: `e582e802919472a277478e06a439b4c889af1882f8f2f337a13c0a71ce6f5ebf`.
Complete extraction and Lean commands are in the receipt. Source fixture,
patch, package recipe and all maintained proofs are retained under `infra`.

Full upstream backends, canonical live-tool adoption, whole DAE Lean admission,
`cargo xtask verify quick/full`, and remote CI have not been established for
this candidate. The periodic production UnitDerivative tracer was rerun and
passes all five tests (0eafdb/0); the compiler spine remains working. Supporting
translator laws do not add another complete UnitDerivative relation: still1/18.
