# Declaration-owned closure contracts: unadopted candidate

Updated 2026-09-08T00:05Z. Status: **WIP, not accepted for adoption**.
Governing process: SPEC_0033 §§2a/4; verification trust boundary: SPEC_0037 §§2/6
(DRAFT). No Modelica semantics or Rumoca IR changes. Predecessors are collector
711522b7, capture construction 593eec2c and static lifecycle 0bce0c42.

## First divergence and producer change

The original `simpler_closures::named_call` fails Aeneas at
`InterpBorrows.ml:1206`. A six-cell diagnostic established that deleting the
method-signature guess alone is insufficient: the declaration needs the forward
input-outlives-output constraint. Reversing that constraint still fails. The
diagnostic's edited LLBC is **not** an implementation or proof input.

The candidate now obtains this constraint from the original Rust source:

- The reviewed early collector owns required paths from rustc's checked body.
- `closure-regions/src/binding.rs` issues erased free occurrences in an explicit
  binder before Hax expands type structure. Genuine bound regions stay bound.
  Capture types and occurrence provenance receive the same substitution.
  `BoundOccurrences` has private fields and a consuming projection: unrelated
  binders cannot be presented as an issued source-correlated pair.
- Hax's closure `FullDef` uses its existing declaration substitution for inherited
  parent arguments: rustc's expression type has already erased these at writeback.
  This is not replacement of arbitrary caller arguments with declaration identity.
- `translate_closure_constraints` maps all three source occurrence partitions
  before emitting directed predicates. Missing facts, unchecked extraction,
  occurrence mismatches and unresolved regions refuse explicitly. The old Hax
  un-erasure visitor and Charon's erased-capture scan are removed.
- The companion Aeneas candidate removes the method-signature guessing pass.

This is not a complete closure contract solution. In particular, an inherited
parent lifetime absent from every capture field can still be erased in a caller
instantiation. `declared-vs-required.rs` exercises this and remains failing.
Declared bounds are not interchangeable with body-required paths.

## Retained source and reproduction

`constructor.patch` applies after the capture patch; `constructor.nix` composes
the reviewed Box/capture patches and copies only the retained fact library's
Cargo manifest and source. Its tool identity binds the constructor patch and
filtered fact-library source. It exports an **unwrapped** candidate, not a live
Rumoca toolchain change. No patch depends on source stored under `target/`.

Evaluate/build with the same explicit Aeneas input as the predecessor:

```console
nix build --impure --no-link --print-out-paths --cores 4 --max-jobs 1 --expr 'let aeneas = builtins.getFlake "github:AeneasVerif/aeneas/f9a8e338188447c77f31246892cb9a7a742e58ef"; in import /home/jgoppert/git/rumoca/infra/verification/charon/candidates/method-constraints/constructor.nix { inherit aeneas; system = "x86_64-linux"; }'
```

Recipe evaluation and patch application have been checked; the complete Nix
build/check phase has **not** passed. The source build uses pinned rustc
nightly 2026-08-18, its rustc-dev runtime, and four build/test workers.
The scratch build is `/tmp/rumoca-charon-captures-5tXAoU`. Full Box patch
composition is now present; its formerly omitted code did not explain the
remaining `consume_once` failure.

From that source tree, focused commands are:

```console
cargo test --offline --test closure_required_regions --test closure_capture_regions -- --nocapture
cargo clippy --offline --all-targets -- -D warnings
cargo fmt --all --check
```

Use `CARGO_PROFILE_DEV_DEBUG=0`, `CARGO_BUILD_JOBS=4`, `RUST_TEST_THREADS=4`,
`RAYON_NUM_THREADS=4`, `CHARON_TOOLCHAIN_IS_IN_PATH=1`, and the matching
full-MIR sysroot/runtime environment. The independent collection driver now
also runs `binding_checks.rs`; compile it with edition 2024 as documented in
the collector's source instructions.

## Executed evidence

Logs and generated artifacts: `/tmp/rumoca-closure-collection-jUn7qq`.

| Claim | Result and boundary |
|---|---|
| Actual source-to-Lean named call | VERIFIED: strict Charon8f7ec7/0, Aeneas65eab5/0 with checks/strict joins, Lean45d758/0. Original Rust, no intermediate editing; generated named call has no external closure axiom |
| Binder scope and provenance | VERIFIED: final private-pair source passes26 closures across eight files, two actual rustc sessions each, under BOTH editions2021/2024; binding-final-* logs. Synthetic nested binders preserve genuine/escaping variables, static positions and fresh occurrence order. Real source checks retain type shape and non-erased identity |
| Pair construction escape | VERIFIED: forged-binding-rejected.rs compiles before the privacy change9364c8/0, then rejects specifically E0451 at both fields8dadd4/1. Final library/driver strict Clippy47875a/0 and Charon27eae2/0; focused tests e3a387/0 |
| Emitted callable predicates | VERIFIED: all three Fn/FnMut/FnOnce methods for free and genuinely bound identity closures; required direction, absence of converse, exact expected predicates including receiver well-formedness. Restored fresh test14021e→fb2103/0 |
| Caller capture identity | VERIFIED: existing first/second/generic capture regression passes in the same restored run |
| Reverse producer mutation | VERIFIED: copy compiles, unchanged typed regression fails53d5fc/101 at the required-path assertion; original source extraction a30275/0 then Aeneas0f5413/1 at1206 |
| Drop producer mutation | VERIFIED: copy compiles, unchanged typed regression fails0604c4/101 at the required-path assertion |
| Eight full source fixtures | VERIFIED: eight strict Charon successes; four complete Aeneas successes (simple, consumer, lifetime, iterator). Generated Lean checks all four:4f3557/0, e66d50/0, f34cf8/0,8e3a81/0 |
| Other four fixtures | VERIFIED refusals: declared parent bound gets an erased argument; FnOnce Box copy unsupported even after full Box patch; invariant fixture's Debug bodies unsupported; early-const function pointers unsupported. No partial Lean file is credited |
| Scoped lint | VERIFIED: complete Charon all-target Clippy1cbca1/0; fact library and collection/binding driver0332b1/0, no new suppression |
| Actual DAE integration | VERIFIED refusal: latest source extraction596967/101 stops at missing foreign closure facts,54 diagnostics including cascades. No Aeneas invocation on its partial output |
| Complete Charon UI suite | VERIFIED final run59dc43/101:422 passed,18 failed,2 ignored.16 snapshot differences remain unapproved; two actual extraction refusals need foreign facts, one from alloc's sysroot. No snapshots or exclusions changed |

The predicate-only parent case is edition-dependent. Fresh raw rustc consumer
runs show DefinitionSiteHiddenType containing an erased first parent argument
under2021 (c9bcba/0), but both named parent arguments under2024 (897233/0).
The2024 source translates4e91ef/0, Aeneas eccb57/0 and Lean323af3/0. This is a
separate discriminator, NOT replacing the original2021 fixture or changing the
four-of-eight matched-fixture census. Source slot existence never established
that the original argument was non-erased. Logs: declared-parent-source*.log.

Mutation patches live in `mutations/`. Apply each to a separate candidate source
copy; use isolated build directories. The initial experiment shared a target
directory, which contaminated the attempted restore (e39ca8/101). A scoped
`cargo clean --package charon` removed only generated Charon artifacts, then
the untouched original rebuilt and both regressions passed. The contaminated
attempt is not restoration evidence.

## Unfinished obligations and review

1. Full caller substitution, including parent lifetimes that appear only in
   declared predicates; monomorphization must not invent identity arguments.
2. Cross-crate fact publication/consumption tied to the resolved artifact,
   compiler/tool/schema identity and crate content hash. Missing cannot mean empty.
3. Correct Cargo cache ownership for nonworkspace dependencies and standard
   library fact production.
4. Preservation of requested MIR; the pre-existing optimized-MIR substitution
   remains unfixed, not an accepted compatibility path.
5. Remaining Aeneas refusals, upstream UI/native/OCaml regression gates, final
   adversarial acceptance, then live tool integration and Rumoca quick/full.

The previous complete DAE run's Aeneas4-diagnostics/1-site census belongs to
the predecessor static/capture pair, not this new constructor's integration.
No whole-core success, additional semantic proof, golden admission or quick/full
green is claimed. UnitDerivative remains **1/18**. Claude supplied the executable
declared-bound counterexample. Its 2026-09-08T00:08:56Z verdict accepts retention
and checkpoint of the final source, extended to all companions at00:12:59Z.
This is byte/mechanism review of an explicitly unadopted candidate, not general
contract acceptance or tool adoption. Claude independently checked binder scope
and the UI failure split; it did not independently rerun the reported gates.
