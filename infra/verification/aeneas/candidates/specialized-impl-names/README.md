# Complete specialization arguments before naming a trait implementation

Status: candidate frozen for Claude's adversarial review, **not adopted**.
Branch `msl-trace-parity-50`, precursor `a1daf47556c1a6ffd7f4b203235ff09711089a85`.
Governing process: SPEC_0033 §§2a, 4, 6; experimental SPEC_0037 §6.
This is a translator dependency repair, not another UnitDerivative relation.

## Invariant and first divergence

Trait-implementation naming must recover the complete original trait arguments
before classifying or removing `Self`. Partial specialization places a substitution
in the declaration's final `PeInstantiated` component. The remaining argument
list can start with another parameter, or contain no types at all.

The actual DAE LLBC `527bb939…` is valid with mutable specialization enabled.
Predecessor Aeneas fails in `ctx_compute_trait_impl_name_raw`, after removing a
type from the still-specialized arguments and passing that incomplete list to
`Substitute.instantiate_name_generics`. `branded-trait.rs` reproduces the exact
exception with only four functions (Charon7684b5/0, Aeneas9520cd/2). It places
the invariant branded type in both `Self` and `Rhs` positions.

The separate `branded-equality.rs` retains the original derived PartialEq/Eq
shape. Its zero-remaining-type references expose the same owner's incorrect
assumption that `args.types` must start with Self. Do not remove Eq from Rumoca
to make this diagnostic disappear.

Competing hypotheses rejected:

- Invalid Rust or invalid Charon input: native tests pass and Charon accepts
  both source fixtures without warnings; the reduced failure is in naming.
- A branding/runtime-layout defect: the invariant PhantomData brand is retained,
  native size tests pass, and an independent-brand comparison fails rustc.
- The generic-arity check is too strict: reconstructing the full arguments fixes
  valid input; deliberately deleting an actual argument still fails that check.
- Disabling specialization is a fix: it does not repair the required specialized
  path. The unspecialized run is used only as a non-regression control.

Upstream [issue1032](https://github.com/AeneasVerif/aeneas/issues/1032#issuecomment-4451595178)
records that monomorphization was unsupported. Repository issue/PR searches on
2026-09-08 found no ready repair for this caller; that is not proof none exists.

## Producer delta

`complete-trait-arguments.patch` changes only Aeneas
`extract/ExtractBase.ml::ctx_compute_trait_impl_name_raw`:

1. Apply the existing name binder to the complete supplied arguments.
2. Consume that name component once and retain the reconstructed name/argument pair.
3. Classify Self, then omit it only from the trait portion of the presentation name.
4. Reuse the reconstructed pair in the blanket branch instead of rebinding the
   old specialized arguments.

No Charon substitution rule, public AST shape, checker, Rust compiler source,
brand, or live tool pin changes. Unspecialized names follow the existing path.
The new work is metadata substitution per specialized implementation name;
Rumoca execution/IR layout is unaffected. Extraction cost is not benchmarked.
General naming injectivity and translator semantic preservation are not proved.

Final patch SHA256: `16ac1aae87438b5421b9d57c12ac30bb1fc55adef6d2ed8ac4891421e270b594`.
Installed producer SHA256: `889d0111d441f50cf9fc70c989fa5d36a659e67b252bf04c0a0ae58a52fc989b`.
Final binary SHA256: `3a49e2a43cf83a67b2121833a5a526492d5a40f0ac7ae7c9012b741fbc242905`.

## Closed focused evidence

Exact commands, terminal statuses and source/artifact hashes are in
`run-evidence.json`; raw extraction logs are under the receipt's artifact root.

| Check | Result | Scope |
|---|---|---|
| Nix build19e26b | Pass | Final recipe, composed frozen dependencies; no adoption |
| Reduced specialized source c6edf2 | Pass | Four functions, both argument positions |
| Generated Lean a56866 | Pass | Untouched generated BrandedTrait module |
| Guarded comparison laws76064d | Pass | Two universal fixture laws, no axioms reported |
| Blanket source fa5040 / Lean48b0ea / law90d039 | Pass | Specialized and ordinary blanket implementations; receiver preserved |
| Native mutation2c0621 | Refuses | Inverted source equality fails the unchanged native test |
| Lean mutation115e83 | Refuses | Mutant generated module type-checks; unchanged law and axiom guard fail |
| Mutant witness9918e0 | Pass | Concrete equal values incorrectly compare false; names standard Lean axioms |
| Malformed argument e96802 | Refuses | Existing generic-arity check remains enforced |
| Independent brands2e24b0 | Refuses | Rust reports both incompatible outlives requirements |
| Unspecialized563d6c/01c6cd, cmp93eb37, Lean4c6a9b | Pass | Exact generated Lean bytes unchanged |
| Rust formatting/lint, OCaml format | Pass | Scoped sources, no lint allowance added |
| UnitDerivative9ec23a | 5/5 pass | Actual production compile, simulation, receipt and typed refusals |

The three positive laws use the generated Rust translation, not a handwritten
replacement implementation. They do not depend on axioms; guards enforce that
inventory. The concrete mutant witness constructs Aeneas U32 literals and names
`propext`, `Classical.choice`, and `Quot.sound`. An initial ordinary Lean numeral
was rejected; the source now uses the library's typed `7#u32` literal. No failed
proof or generated partial file is admitted. Rust lifetime constructibility is
not established by these Lean value-domain laws; its refusal is a native control.

## Remaining red boundaries and review

Final derived-Eq replay e071e5/1 still rejects the collision between ordinary
and specialized `Eq::assert_fields_are_eq.default`. The former Self failures
are gone, but this module remains partial and is not passed off as valid Lean.

Actual DAE final replay fc9682/2 passes the old impl-argument failure, then
reports seven specialized `Iterator::map.default` name collisions (fourteen
error messages), missing library `Step`/`ExactSizeIterator` fields, and an
exception in `ctx_compute_var_basename`: it treats a final specialization
component as an identifier. These are next-owner diagnostics, not fixed here.
No whole-DAE Lean admission, second UD relation, golden admission, or quick/full
green is claimed. No broader Aeneas suite or non-Lean backend verification was
run for this cut; this packet provides focused source/mutation controls only.

Review must check the actual binder substitution, blanket handling, unchanged
arity refusal, source-bound mutation, and remaining extraction failures. The
predecessor packets (including Claude's drop-control FIX) still need independent
review. Current disposition is **PENDING**, not ACCEPT. The active candidate
does not bless predecessor patch fuzz or change their frozen bytes.

Build with `nix-build infra/verification/aeneas/candidates/specialized-impl-names/package.nix --no-out-link --cores 4 --max-jobs 1`.
Use the receipt's pinned Charon, Lean search path, and exact source commands
for focused replay. The patch applies to the installed predecessor with
`patch --dry-run --batch --fuzz=0` (796b45/0).
