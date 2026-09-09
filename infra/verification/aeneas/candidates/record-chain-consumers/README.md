# Record-chain consumers (charon-ml and Aeneas)

Consumers of the instantiation-record chains that the trait-identity Charon
revision `explicit-role` emits: an `Extended` record (associated-type lifting
extended the item's generics in place; the item keeps its identity) followed,
when the item is later copied, by an `Instantiated` record (a distinct copy
with its own arguments). Ledger: channel posts of 2026-09-08 from 19:11Z.

## Contract

1. A name may end with a chain of records. Each record maps the item's
   generics after that step (`params`) to the generics before it (`args`).
2. The role is explicit in the schema: `PeExtended` is provenance of the
   original identity; `PeInstantiated` is a copy. No shape classifier.
3. Matching folds every trailing record last to first with the full generics
   (Self included) before any self-type stripping; the result is the declared
   identity and its arguments. Sites: charon-ml `match_name_with_generics`,
   `match_name_with_generics_prefix`, `find_with_generics_opt`,
   `type_decl_ref_generics`; Aeneas trait-impl naming (the impl's trait
   arguments through the trait declaration's records).
4. Naming folds copy records only and drops extension records: a copy shows
   the arguments that distinguish it, at the arity of the item it was copied
   from, and an extended original stays bare. Site: charon-ml
   `name_with_generics_to_pattern_aux` (patterns made from names), which
   Aeneas's `name_to_simple_name` and `name_with_generics_to_simple_name` use.
   A bare pattern matches an extended original; no pattern ignores a copy.
5. Printing: `PeExtended` prints nothing; `PeInstantiated` prints its
   arguments (as the Rust printer does). `LlbcAstUtils` strips both.

## Patches

`charon-ml.patch` (paths under `charon-ml/`, applied to the Charon flake's
charon-ml derivation, built with the Aeneas flake's OCaml package set): the
generated `PeExtended` variant from `generate-asts` (Types, OfJson,
OfPostcard, formatted with the repository's ocamlformat 0.27.0); helpers
`is_name_record`, `fold_name_records`, `fold_copy_records`,
`split_name_records` in `Substitute.ml`; the four matching sites and the
naming site in `NameMatcher.ml`; the printer arm in `PrintFmt.ml`.

`aeneas.patch` (applied after the loop-return-order chain): the strip helper
in `llbc/LlbcAstUtils.ml`; `instantiated_name_generics` in
`extract/ExtractName.ml` returns nothing for an extended original so it is
looked up bare; the trait-impl naming site in `extract/ExtractBase.ml` folds
every record with the full trait arguments before Self is removed.

## Recipes

`replay.nix`: the loop-return-order Aeneas with both patches (the measured
binary). `package.nix`: the same with the trait-identity Charon, which has no
store path until the ui expectations lane lands. `charon-ml-only.nix`: the
patched library alone, which compiles and passes the format check.
`replay-ml-only.nix`: bisection recipe, charon-ml patch without the Aeneas
patch.

## Measurements (`run-evidence.json`)

Attempt 1 failed in the recipe (OCaml package conflict between two nixpkgs).
Attempt 2 built on the specialized-impl-names base and failed on the law file
and on the composition; the unpatched base fails identically on the law file
(rows `attribution-*`), and the sibling without the loop patch fails the
replace-semantics composition, while loop-return-order emits it at the M0's
24 of 140. Attempt 2 also exposed a real consumer defect, retained: folding
every record for naming made the copied `FnOnce` (differing from the original
only in its lifted output position) clash with the original; contract point 4
is the correction.

Attempt 3 (loop-return-order base, copy-only fold in every pattern) exposed a
second consumer defect, retained: the pattern sanity check in charon-ml
re-matches a generated pattern against its name with the full-fold matcher,
and a pattern built with the copy-only fold does not match, so 22 methods of
impls of lifted traits were dropped (`fnonce-chain.rs` reproduces it in six
lines with abort-on-error). The correction is contract point 4 as stated:
extraction names (target kinds name and pretty) fold copy records only;
patterns (target kind pattern) fold every record, as the matcher does.
Attempt 4 failed on a non-exhaustive match. Attempt 5 (that correction) is the
first clean binary: the fixture emits with the copy named beside the
original; the law reproducer binds the Std `branch` in the polymorphic and
`--monomorphize-mut` runs with zero errors and no opaque function, and under
`--monomorphize` the two copies bind their own distinctly named
implementations (the pre-existing copies-at-lifted-arity boundary); the
three-root composition emits with zero errors at 24 of 140 opaque, the
baseline count. Attempt 6 added the root-carrier readers of root-identity candidate11 (rows
`attempt6-*`, superseded). Attempt 7 added the candidate12 disposition
readers (rows `attempt7-*`, superseded). Attempt 8 failed in charon-ml because
candidate13 first named a field `initializer`, an OCaml keyword, which the
generator emitted verbatim (rows `attempt8-*`). Attempt 9, the final binary,
carries the closed disposition variants of candidate13 and repeats every
measurement on the candidate13 explicit-role Charon (rows `final-*`): the
fixture, the law reproducer in three modes and the composition give the same
results, and the composition artifact carries its roots and dispositions.
