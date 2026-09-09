# Closure method constraint candidate

Unadopted consumer-side deletion, 2026-09-07. `remove-method-guess.patch` deletes
the complete `fix_closure_lifetimes` function and its registration. It does not
replace the guess with another signature-rewriting pass. `package.nix` composes
this deletion after the reviewed static-lifecycle/capture pair; both closure
guessing passes are absent in this candidate. No live tool pin changes.

Build from the repository root:

```console
nix build --impure --no-link --print-out-paths --cores 4 --max-jobs 1 --file infra/verification/aeneas/candidates/method-constraints/package.nix
```

Recorded binary:
`/nix/store/v2br63mjn55pkcfdzf6inyg007l36sj7-ocaml5.2.1-aeneas-0.1.0/bin`.
As with the predecessor, record both Charon and Aeneas identities for a paired run.
The deletion alone does **not** fix translation: Charon still needs to issue
the missing declaration constraints. Full upstream regression and live integration
gates are pending that producer implementation; this is not semantic adoption.

The deletion also regresses two retained fixtures the predecessor translates,
so this binary is not an unconditional replacement of its predecessor for
closure inputs: `two-region-iterator.rs` (LLBC abae485b), accepted earlier as
translating with no errors, now fails at `InterpBorrows.ml:1206` and emits
`two_regions_ref_item::call_mut` as `sorry` (predecessor exit 0, candidate
exit 1); `captured_move` goes 0 to 1 while `unrelated_outer` now translates.
Other retained closure and static-regions fixtures are unchanged. Independent
review receipt: `review-evidence.json` (commands, exit codes, LLBC, log and
Lean hashes for the six cells, the Lean checks and the regression pair).
Patch sha256: 80955670afb1f8e6 (full hash in the receipt).

## Controlled causal experiment

Extract the existing `simpler-closures.rs` fixture with root
`simpler_closures::named_call`, explicit crate name `simpler_closures`, and
`--preset=aeneas --mir=optimized` into `NamedClosure.llbc`.
The source-owned experiment
`../../../charon/candidates/method-constraints/named-outlives-diagnostic.jq`
applies to a **disposable LLBC copy only**. It refuses unexpected fixture/declaration
layouts, then inserts one specified outlives relation in the FnOnce/FnMut
declarations and method declarations. This is a diagnostic intervention, **not
a production pass**, proof input, or substitute for the Charon constructor fix.

The real borrow-check graph contains the one-way signature-input-to-output path.
The parent parameter is independent. The predecessor's `PrePasses` trace shows
the guess replacing the output with that unused parent region anyway.

| Consumer | No added constraint | Input outlives output | Output outlives input |
| --- | --- | --- | --- |
| Guess retained | Rejects | Rejects | Rejects |
| Guess deleted | Rejects | Translates and Lean-checks | Rejects |

All six runs use `-checks -strict-joins -sequential -print-error-emitters` (the
last flag is what prints the emitter location); all five rejections reach
`InterpBorrows.ml:1206`. Only the forward relation with the guess deleted
translates. Its generated code returns the supplied value and unit closure state,
without an external axiom. Pinned Lean checks with `-j4 -M4096
-DwarningAsError=true` pass. Logs and generated data are under
`/tmp/rumoca-closure-collection-jUn7qq/diagnostic-*`.

This isolates two independent causes and the direction of the required relation.
It does not establish automatic source-to-Lean translation or formal proof credit:
the diagnostic changed an IR copy by hand. The real fix must emit this relation
in Charon from declaration-owned, binder-correct facts and remove the guess.
