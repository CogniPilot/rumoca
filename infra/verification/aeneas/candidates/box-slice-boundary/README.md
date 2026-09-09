# Shared-slice to boxed-slice library boundary

Status: **REVIEW REQUESTED, not accepted or adopted**, 2026-09-09T01:56:14Z.
Writer: Codex. Independent reviewer: Claude (no verdict yet).
Branch: `msl-trace-parity-50`, compiler HEAD
`a1daf47556c1a6ffd7f4b203235ff09711089a85`.

## Invariant, scope and first divergence

The original Rust `Box::from(&[T])` and `Into` callers return the ordered
element-Clone results and propagate Clone failure/divergence under the declared
value-level library contract. Returning the input unchanged is justified only
when Clone is identity on the elements. This is a library boundary required by
the actual DAE/Solve catalog proof, not another completed UnitDerivative relation.

Governing anchors: SPEC_0033 §§2, 2a, 4 and 6; experimental DRAFT SPEC_0037 §§2
and 6. This changes neither Modelica semantics nor a Rumoca IR/phase. MLS/eFMI
compliance is not established by these library tests.

The unchanged reducer first emits six function records, including one opaque
`From<&[T]> for Box<[T]>::from`. Five caller/Clone bodies are already available.
The earliest missing owner is Aeneas's Std operation model/binding registry, not
Rumoca. The actual production caller uses `CloneU32` for compact dimensions.
This is not a missing Clone implementation or a tensor scalarization problem.

Pinned Rust nightly1.100.0 (2026-08-18) source establishes the boundary:

- `alloc/src/boxed/convert.rs:61–79` delegates shared-slice From to
  `Box::clone_from_ref`.
- `alloc/src/boxed.rs:797,843,870` reaches the allocation/CloneToUninit path.
- `core/src/clone/uninit.rs` generically clones elements in order, with cleanup
  on failure. Its optimized specialization requires **TrivialClone**, not Copy.
- `core/src/clone.rs:253–283` explicitly requires TrivialClone's Clone operation
  to equal copying. TrivialClone does not itself require Copy. Arbitrary Clone
  cannot be replaced by copying.

The previous bounded upstream issue search for boxed found unrelated closed
469/376; it was not an exhaustive absence claim.

## Small producer change

`stdlib.patch` adds one typed operation delegating to the existing
`Slice.clone cloneInst.clone`; it adds no second clone loop or new semantic axiom.
Its conditional identity theorem reuses `Slice.clone_spec`. One Std import
makes the declaration visible to Aeneas's official registry generator.
`binding-registry.patch` is the formatted output of that unchanged generator,
with exactly one additional function identity.

`package.nix` layers that registry patch over the frozen Vec-to-box candidate
(binary SHA256 `4b802364fa7371a1cb37091b997722937b4d215eeb4f9bf2954e49a8b9af7be7`).
**It builds the extractor registry only.** The Std patch is compiled separately
in the isolated overlay; the Nix recipe is not evidence of live Std adoption.

Candidate extractor:
`/nix/store/k76irscwl12kzk6fgqw6mkhxb82jz9x0-ocaml5.2.1-aeneas-0.1.0/bin/aeneas`
SHA256 `e91ca476f9f9569d8f944ada9f853bd1626db32f3973c3e9ba7b85b6dc9fbeb5`.

## Closed evidence

All commands, outputs, original expectations and observed exits are in
[run-evidence.json](run-evidence.json). Generated evidence and isolated sources:
`/home/jgoppert/git/rumoca/dev/box-slice-boundary-FJqIbo`. No generated Lean body was edited.

| Check | Result |
|---|---|
| Original native tests | 4/4, including nonidentity Clone, panicking Clone, empty/ZST and U32 boundaries |
| Rust fixture strict Clippy and rustfmt | Pass; no lint suppression |
| Original and Rust-mutant generated modules | Five transparent functions, zero opaque; Types/Funs compile |
| Eight unchanged caller laws | Pass with exact guarded foundation `propext, Classical.choice, Quot.sound` |
| Rust caller discards successful cloned payload | Unchanged preservation/nonidentity laws reject; independent Into and failure/divergence controls remain |
| Rust wrong-result witness | Proves returned empty slice after successful clone only for mutant; rejects original |
| Library body skips Clone and returns input | Mutated Std and fresh generated modules compile; unchanged nonidentity/failure/divergence laws reject |
| Library wrong-result witness | Proves clone is ignored only for bad model; rejects correct model |
| Earlier Option regression | Fresh source extraction/module compilation; seven existing guarded laws pass |
| Four patch applicability/whitespace checks | Pass |
| Actual UnitDerivative production tests | Fresh3cb441/0: 5/5; no compiler edit |

The copy-only model mutation deliberately retains the conditional identity
theorem's *statement*, with a proof valid for its incorrect body. That theorem
alone therefore cannot establish cloning semantics. The caller laws expose
concrete false outcomes: flipped booleans become unflipped, and failure/divergence
become success. Some other unchanged tactics/axiom guards also fail, but those
extra failures are not used as semantic counterexamples.

The Rust mutation preserves the initial clone call, then returns an empty box.
Native output loses three tests while the panicking-Clone control still passes.
Both fault witnesses reject the positive implementation for an unsatisfied
result equality, not a missing module or syntax error. Their exact axiom guards
also reject any introduced `sorryAx`.

Resolved intermediate failures remain in the receipt: abstract Boolean negation
required case analysis in one proof; an unused premise name in the bad-model
theorem failed strict Lean lint; a census command used a split-output path for
a flat baseline. Bare blank context markers in the two generated mutation
patches were normalized for the repository's whitespace check; both normalized
patches still pass strict applicability checks. Each has a closed correction.
The initial Rust witness's
positive refusal also reported an unused simp argument; final unchanged
two-sided replay removes that incidental lint failure. These are not hidden
successful runs or credited mutation kills.

## Actual production delta

Same LLBC SHA256
`f71424dab48b332d8f17d004c6b8a2a7ea0bbaf2a1a4aa0ffd8ac31944312a17`,
same frozen Charon4 and same three selected production roots:

- `DaeView::variable_refinement`
- `SolveModel::variable_refinement`
- `check_variable_catalog_refinement`

Emission changes **126 functions / 2 opaque libraries → 125 / 1**. Exactly the
newly bound From external leaves emitted metadata; all 125 retained function
records and all Types declarations are identical. The sole Funs delta binds
the existing From trait dictionary to the new typed model, preserving its Clone
argument. No opaque first-party function appears. Remaining opaque operation:
generic NonZero equality.

**This is emission, not full production Lean admission.** HashMap/HashSet
storage correspondence, iterator/Step interfaces and tool-stack adoption remain
open. Four existing iterator/Step model warnings persist; they are not silently
waived or treated as proof of missing callee models. Fewer opaque metadata
entries do not measure remaining proof effort.

## Trusted boundary and limits

VERIFIED: generated caller laws, negative controls, metadata/type delta, native
tests and scoped checks above. INFERRED from pinned Rust source: the authored
value-level contract corresponds to the shared-slice conversion's Clone behavior.

Lean's pinned kernel and Rust/Charon/Aeneas translation remain trusted. The
existing Slice/List clone abstraction carries the listed foundational axioms;
these are not zero-axiom proofs. Allocation success, pointer/capacity/layout
observations, custom allocator effects, destructor cleanup, global/interior
mutation and unwinding behavior are not proved. Abstract failure/divergence
propagation is checked; it is not a theorem about Rust's unwinder or allocator.
The TrivialClone unsafe contract is trusted, not inferred for arbitrary Clone.

Mechanism-backed cost/safety assessment: no Rumoca source, live pin, range,
branding, tensor representation or runtime path changed; compiler cost and
construction guarantees are unaffected by this isolated candidate.

Not run: complete production Lean proof/admission, canonical tool/pilot adoption,
`cargo xtask verify quick/full`, cohort MSL parity and remote CI. This cut is not
a dependency-closed adoption milestone; broad gate success is not claimed.
The second UD relation and M1 remain open; accepted count stays1/18.

## Replay and review

Use the exact pinned commands and working directories in the receipt. Apply
Std changes only in an isolated source/compiled overlay; compile Box, Std and
Aeneas before extracting/compiling fixtures. The final reducer omits the unused
Debug derive present in early exploratory runs; final hashes identify review
bytes. Generated Types/Funs are never patched.

Review the pinned Rust Clone/TrivialClone distinction, registry identity and
trait argument, eight law statements, both independently compiled mutants,
their two-sided wrong-result witnesses, and the exact production metadata delta.
An ACCEPT is bounded to these bytes and named library assumptions, not allocator
correctness, the full translator stack, golden admission or another UD relation.
