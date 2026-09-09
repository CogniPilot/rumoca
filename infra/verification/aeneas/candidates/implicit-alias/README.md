# Infer generic arguments through emitted tuple aliases

Status: producer logic reviewed; evidence corrections pending recheck; not adopted.
Branch: msl-trace-parity-50, precursor a1daf47556c1a6ffd7f4b203235ff09711089a85.
Governing anchors: SPEC_0033 §§2a/4/6 and experimental SPEC_0037 §6.
Predecessor: [specialized-name-presentation](../specialized-name-presentation/README.md).
No Rumoca production source, brands, live tool pins or final guards changed.

## Reproduction and earliest owner

The predecessor emits derived branded equality, but Lean cannot infer the
phantom type parameter. `compute_explicit_info` regards occurrence in an input
type as sufficient evidence for omission. The emitter represents positional
structures as transparent tuple aliases: `PhantomData<T>` becomes `Unit`, which
does not determine T. Its caller supplies `()` and loses that information.

The native-valid `alias-parameters.rs` reducer extends the same failure to
const parameters that affect the result: `Tag<N> = Unit`, `WrappedTag<N> =
Tag<N>`, and `PairTag<N> = Tag<N> × U32`. The unmodified predecessor emits all
three, then Lean refuses their omitted N arguments. Nominal typed arguments,
ordinary identity, payload-bearing tuple aliases and arrays are positive
controls. A separate nominal record-constructor annotation defect lives in
`constructor-inference.rs`; both tools emit identical rejected output for it.

This is not invalid Rust, a naming collision, or a need to remove branding.
The pure-IR signature still contains the generic argument. The first divergence
is its inferability classification, before the printer omits it.

## Producer change

`alias-inference.patch` changes five upstream source files, +86/-33 lines:

- `PureUtils`: both parameter visitors require a type-normalization function.
- `SymbolicToPureTypes`: one context-backed normalizer follows only the tuple
  aliases already selected by type analysis. It substitutes the actual generic
  arguments into the emitted fields using existing translation/substitution
  helpers. Nominal, recursive nominal and externally modelled types are intact.
- `SymbolicToPure`, `SymbolicToPureExpressions`, `PureMicroPassesLoops`: thread
  the existing declaration context into signatures and binders. No new IR shape.

The same inference policy supplies declaration binders and call emission.
There is no PhantomData spelling branch, all-generics-explicit fallback,
arity padding or generated-Lean edit. The existing tuple-selection analysis
excludes recursive aliases; a general normalization/termination theorem is not
claimed. External model non-injectivity, associated-type projections and the
independent constructor-annotation defect are outside this repair.

Patch SHA256: 487ef0120e152757801612bcedcdd3c5035023eb9d993c32157b7e8707a12e98.
Binary SHA256: 10916246796f8fb7360a4eeebf440b85ff154699392ef9651bc799fae2e0798f.
Nix build1641d4/0 passes; all five installed files match the edited source.
Patch dry-run9aa8eb/0 uses zero fuzz. Older dependency patch fuzz is not blessed.

## Closed evidence

Exact commands, exit statuses and final byte bindings are in `run-evidence.json`.
Its 71 records now label expected exits and superseded development attempts;
two accidental failures (strict lint and a nonexistent comparison path) remain
unexpected failures, not passing controls. Observation wait times, session IDs
and token counts have been removed: they were not command durations.

| Check | Terminal evidence | Scope |
|---|---|---|
| Final Rust/Charon | db106d/0, ba3a38/0 | Native behavior and valid input |
| Same final source, predecessor | 3c283e/0, e4b61b/1 | Emission succeeds; Lean rejects omitted arguments |
| Candidate | 6d4f95/0, 95a6ec/0 | Untouched generated Lean checks |
| Eight preservation laws | a5c73b/0 | All declared Usize/type/payload inputs; each axiom inventory is empty |
| Actual derived equality | 5268e3/0, 4f2e9c/0, 4b4cd2/0 | Payload equality theorem; guarded axiom inventory is `[propext]` |
| Wrong-result Rust mutation | 1807df/101, 0bc610/1 | Returning zero fails native test and unchanged Lean theorem/guard |
| Mutant output and witness | 73abca/0, ffd9f9/0 | Wrong-source translation checks; it always returns zero |
| Wrong witness on original | 7d3877/1 | Clean candidate translation refuses the wrong-answer theorem |
| Brand / arity refusals | 29d56c/1, 02c3ac/2 | Rust brand rejection and an uncaught malformed-LLBC arity exception, respectively |
| Previous impl-name control | 540b3e/0, e43c8f/0 | Output is byte-identical |
| Nominal-constructor red control | 16a777/0, f414ba/1 | Identical predecessor/candidate output; Lean still rejects |
| Final fixture lint / format | 3b7519/0, b00467/0, 917c97/0, 628b91/0 | No handwritten lint suppression |
| Production UnitDerivative | 8e8bcd/0 at 12:32:30Z | Five compilation/simulation/receipt/refusal tests pass |

The numeric mutant witness names `[propext, Classical.choice, Quot.sound]`
through the typed Usize literal; it is not axiom-free. All final laws are guarded.
An initial fixture used a lifetime only in its body and failed strict Clippy;
the final public input carries the brand, and every final native/translation/
proof/mutation check was rerun. Earlier diagnostic runs are history, not evidence
over that later source. No failed Lean output is admitted.

The receipt now retains the actual final failure text: the unchanged
`AliasLaws.tag_preserves_parameter` fails at `AliasLaws.lean:8:2` because
`read_tag n` is not `Result.ok n`; the wrong-answer theorem fails at
`MutationWitness.lean:8:2` on the clean candidate because it is not
`Result.ok 0#usize`. Both axiom guards reject the failed proof's `sorryAx`.
Here "original" means the clean candidate, not predecessor output, which
does not Lean-check. These are source-contract failures, not missing imports.

Actual DAE Aeneas d0d997/0 completes in 81.073356 seconds. Lean852a38/1 now has
20 diagnostic headers, down from21: the StateId phantom inference error is gone.
Missing iterator/exact-size fields, F64 universe issues and downstream fallout
remain. These are error groups, not20 independent root causes. The timing is a
single replay, not a benchmark; the predecessor took79.785695 seconds. This
tools-only change adds no Rust compiler execution or representation cost.

Still **1/18** UnitDerivative relations; this packet is translator validation,
not a second relation, whole-DAE Lean admission, golden admission or L4 selection.
Broad upstream Aeneas/non-Lean suites, canonical tool adoption, quick/full and
remote CI were not run: dependency review/adoption is not closed. Earlier
candidate dependency reviews remain open. The drop-control fixture/evidence
corrections have a separate bounded acceptance; that does not adopt this chain.

Upstream-first search on 2026-09-08 found [PR942](https://github.com/AeneasVerif/aeneas/pull/942)
(closed, unmerged), which addresses constructor return parameters rather than
input alias inferability. No direct ready-made repair was identified in that
bounded search. This is not a claim that no related upstream work exists.

Build: `nix-build infra/verification/aeneas/candidates/implicit-alias/package.nix --no-out-link --cores 4 --max-jobs 1`.
