# f64 type binding at the Lean boundary

Read-only diagnostic. No producer, model, pin or generated-Lean edit; no adoption or proof credit.
It asks why the DAE root's generated Lean reports `Constructor field F64 ... contains universe level
metavariables` at `Literal.Real`, `DaeLiteral.Real` and `PositiveParameterEntry.mk`, and which of
four candidate causes is real: the model absent, shadowed, incorrectly typed, or wrong emitter
metadata.

## Reproducer

`f64-binding.rs` reduces the three DAE sites to an enum with an `f64` payload and a struct with an
`f64` field, plus two functions that only move the value. `f64-binding-with-external.rs` is the same
with one inventoried external type added (`std::sync::Arc<u32>`, which the pilot already models), so
the external-type seam is emitted. Both compile with zero warnings, pass their native test, and pass
`clippy -Dwarnings -Dclippy::all` and `rustfmt --check`. Tools are the candidate Aeneas package bound
in `implicit-alias/run-evidence.json` and its own Charon; Lean is the pinned toolchain with the
pilot's `LEAN_PATH`. Every run, exit and log is in `run-evidence.json` and `logs/`.

## What the runs show

| step | result |
| --- | --- |
| single-file translation of `f64-binding.rs` | generated Lean references bare `F64` five times, imports only `Aeneas`; Lean fails at `Literal.Real` and `Entry.mk` with the DAE's exact universe-metavariable error |
| split-file translation of `f64-binding.rs` | `Types.lean`, `Funs.lean`, lib entry; **no** `TypesExternal_Template.lean`; `Types.lean` imports only `Aeneas`; `translation.json` never mentions `f64` or `F64` |
| split-file translation with `Arc<u32>` | `TypesExternal_Template.lean` present, mentions `Arc` four times and `F64` zero times; `Types.lean` imports `<crate>.TypesExternal` |
| Lean on that output with the template as-is | every inventoried external satisfied; **fails** on `F64` at `Types.lean:26` |
| Lean on that output with the pilot's `models/TypesExternal.lean` placed unmodified | **passes** |

## Owner and cause

The emitter is `ExtractBase.ml:536-552` `float_name`: for the Lean backend it prints `F%d`, so
`f64` becomes the bare identifier `F64`, exactly as `int_name` prints `U32` and `I64`; `scalar_name`
routes `TFloat` through it and `ExtractTypes.ml:301` prints it wherever the type appears. The Aeneas
`Std` library defines `U32`, `I64` and the other integer scalars under `Aeneas.Std`, and defines no
`F64` anywhere; there is no builtin mapping for `f64` in `ExtractBuiltin*.ml` either. The pilot's
reviewed model `structure Aeneas.Std.F64 where storageBits : U64` fills the gap under the same
namespace, so `open Aeneas.Std` resolves the bare name.

The seam that would carry that model is conditional. `Translate.ml` emits the `TypesExternal` module,
its template and the `import <crate>.TypesExternal` line in `Types.lean` only when the crate has
opaque type declarations. `f64` is a literal type, not a declaration, so it never counts: it is never
inventoried in `translation.json`, never requested in the template, and a crate whose only foreign
type is `f64` gets no seam at all.

So, of the four candidate causes:

- **Absent.** Yes. In the DAE run that produced the error the model was not in the import graph
  (a single-file run with nothing placed). With the model placed, the reducer's generated Lean
  checks; the DAE output itself was not re-run here and still carries its iterator errors.
- **Shadowed.** No. Nothing else in the library or the generated code defines `F64`.
- **Incorrectly typed.** Not tested here. The fixtures only move values, so they exercise type
  elaboration and nothing about `to_bits`, bitcast or arithmetic; a model of any shape named
  `Aeneas.Std.F64` resolves the observed error. Correctness of the reviewed storage/`to_bits`
  model is outside this packet and unchanged by it.
- **Wrong emitter metadata.** Consistent but incomplete. The emitted name matches the model's
  convention, but the emitter declares no dependency for it, so the external inventory cannot admit
  or refuse it. The template-as-is run is the proof: all inventoried externals satisfied, Lean still
  fails.

## Consequences

The canonical pilot pipeline works because the DAE root has other opaque types, so the seam exists
and placing the reviewed models supplies `Aeneas.Std.F64` alongside them. The pipeline records that trust decision explicitly: `external_inventory.rs:47-49` names `F64` as
a separately trusted storage primitive, and SPEC_0037 names the same assumption. What the
structured manifest and template omit is the dependency itself, so that decision is enforced by
the model file being placed rather than by the manifest naming the primitive; the template-as-is
run shows the omission, not the absence of a decision, and neither it nor the Lean run is an
executed test of the pipeline's admission gate. A crate with `f64` as its only foreign type cannot be repaired by placement
without editing generated Lean, which the pipeline forbids.

## Not proposed here

A repair belongs to the emitter or the pipeline, not to this packet: either the backend declares the
float type as an external dependency so the template and `translation.json` carry it and the
inventory decides it, or the pipeline records `F64` as an expected external explicitly and always
places the model. No floating-point arithmetic axiom, no hand-edited Lean, no widened allowance.

# Candidate: typed float dependency registration

`float-dependencies.patch` (Aeneas, four files) and `external-inventory-consumer.patch` (Rumoca's
`crates/xtask/src/verify_cmd/lean_pilot/external_inventory.rs`). Composed on the `implicit-alias`
candidate through `package.nix`. Both are candidates; neither is adopted, and nothing changes what
`F64` means.

## The change

- `pure/PureUtils.ml`: `crate_float_types`, a typed walk over type declarations, function
  signatures and bodies, globals and trait constants collecting every `TFloat` width through the
  pure visitors. Nothing is inferred from emitted text or from the presence of another external.
- `Translate.ml`: the widths are computed once per crate. In split-file mode the `TypesExternal`
  module, its template and the `import <crate>.TypesExternal` line are now emitted when the crate
  has opaque types **or** float widths. In single-file mode a crate with any float width is refused
  with a named message, because that mode has no model module to import; a comment would not be a
  dependency.
- `extract/ExtractTypes.ml`: for each width the template carries a doc comment and an
  `axiom Aeneas.Std.F<w> : Type` placeholder, the same shape the template uses for opaque types,
  under the name `float_name` already prints. `float_name` itself is unchanged.
- `EmitJson.ml`: `translation.json` gains `float_dependencies`, one `{width, lean_name, lean_file}`
  per width.
- Consumer: `Manifest` requires the `float_dependencies` member (a manifest without it fails to
  deserialize), entries are `deny_unknown_fields`, `PERMITTED_FLOAT` binds the existing reviewed
  decision to exactly width 64 named `Aeneas.Std.F64` in `TypesExternal_Template.lean`, duplicate,
  unreviewed-width, renamed and relocated entries refuse, and the reviewed width must be listed or
  its model would apply to nothing. The comment at the old `:47-49` now says the manifest names the
  primitive. Seven tests added; 23 run and pass in the module.

## Evidence

Every cell is in `run-evidence.json` with its observed exit, command, and log under
`logs/candidate/`; the consumer test run is `logs/consumer-tests.log`.

| crate | split-file | single-file | Lean, no external module | Lean, template as-is | Lean, pilot model placed |
| --- | --- | --- | --- | --- | --- |
| `f64-binding` | template names `F64`; `Types.lean` imports `TypesExternal`; manifest lists width 64 | refused, message names `F64` | fails: module `F64Binding.TypesExternal` does not exist | passes (placeholder axiom) | passes |
| `f64-binding-with-external` | template names `F64` and `Arc`; import; manifest lists 64 | refused | fails, missing module | passes | passes |
| `integer-only` | no template, no import, manifest `[]`; output byte-identical to the predecessor in `Types.lean`, `Funs.lean`, `IntegerOnly.lean` | unchanged, passes | passes standalone | n/a | n/a |
| `f32-width` | template names `F32`; import; manifest lists width 32 | refused, message names `F32` | fails, missing module | passes (placeholder) | fails: `F32` unbound, the reviewed model defines only `F64` |

Mutations of the pass, each built the same way:

| mutant | change | observation on `f64-binding`, split | killed by |
| --- | --- | --- | --- |
| `ignore-float-seam` | drop the float term from the seam condition | no template, no import, manifest `[]` | the split positive control |
| `drop-manifest-record` | drop the manifest record | template and import present, manifest `[]` | consumer test `the_reviewed_float_width_must_be_listed`, which refuses exactly that array |

## What this does and does not establish

It makes the float dependency explicit in typed IR, in the template, in the manifest and in the
consumer's decision, and it refuses the one output mode that cannot carry a model. It does not
give any float a meaning: the reviewed storage-only `Aeneas.Std.F64` is untouched and no width
gains arithmetic. A template placed unfilled still typechecks, for floats as for every opaque type;
refusing that is the inventory consumer's job and its existing template check does it. Widths other
than 64 are named and refused by the consumer, not modelled. The candidate is measured on four
reduced crates and one predecessor identity check, not on the DAE root, and the consumer patch was
tested in a throwaway worktree at `a1daf475`, not adopted. Upstream PR 1073 remains unadopted; this
layer is independent of whether it lands.

## Correction r2

Independent review of the first candidate (patch `9042edc1`) produced two
executed counterexamples and a follow-up; all three are answered by
`float-dependencies-r2.patch` (`281c2d4c`) and `external-inventory-consumer-r2.patch`.
The first patch and its receipt records are retained unchanged; everything
below is recorded under `r2_records` in `run-evidence.json` with the r2 binary
hash.

1. A float reachable only through a trait implementation's associated type
   (`associated-float.rs`, `type Scalar = f64`) was not inventoried. The walk
   was a field-by-field special case. r2 replaces it with a complete typed
   traversal (`PureUtils.FloatTypes.collect`): every declaration record
   (`fun_decl`, `fun_sig`, `fun_body`, `global_decl`, `trait_decl`,
   `trait_method`, `trait_impl`, `binder`) is destructured exhaustively with
   warning 9 promoted to an error, so a field added later fails the build
   instead of being skipped, and every typed field goes through the derived
   `iter_type_decl` and `iter_expr` visitors (generics, predicates, trait
   references, associated types and constants, signatures, bodies).
2. The seam, refusal and placeholder emitter were unguarded across backends:
   a Coq split of `float-field.rs` gained a Lean template. r2 computes the
   inventory only when `Config.backend () = Lean`; every other backend keeps
   its previous output. Both of the reviewer's fixtures now produce Coq output
   byte-identical to the predecessor (`coq_pred_*` versus `coq_r2_*`).
3. The reviewer found that the predecessor's Lean output typechecks without
   any model because Lean auto-binds the undeclared `F64` as an implicit
   parameter, so a green Lean check cannot show the binding exists. The
   negative control now prints the instance signature:

| output | model module | Lean result | `#check` of the instance |
|---|---|---|---|
| predecessor split | none | passes | `{F64 : Type} → ScalarKind RealKind F64` |
| r2 split | none | fails at `import AssociatedFloat.TypesExternal` | not reached |
| r2 split | template installed | passes | `ScalarKind RealKind Aeneas.Std.F64` |

r2 inventory on every fixture: `f64-binding`, `f64-binding-with-external`,
`associated-float` and `float-field` list width 64 as `Aeneas.Std.F64` in
`TypesExternal_Template.lean`; `f32-width` lists width 32 as `Aeneas.Std.F32`;
`integer-only` lists nothing. Single-file output refuses (exit 2) for every
float crate and passes for `integer-only`, as before.

Consumer (`external-inventory-consumer-r2.patch`, diffed from checkpoint
`a1daf475`): `check_manifest` is split so strict clippy passes
(`too_many_lines` failed on the first patch), and a new test feeds the
production `check_manifest` the `translation.json` that the
`drop-manifest-record` mutant actually wrote
(`logs/mutant-drop-manifest-record.translation.json`); it is rejected with
"permitted float width 64 (`Aeneas.Std.F64`) is missing". Bound sources for
that test are taken from the manifest itself so the float check is what
decides. Evidence rows `consumer_*` in `r2_records` carry the literal argv and
cwd: rustfmt check exit 0, `clippy --all-targets -- -D warnings` exit 0,
24 tests passed. Limit: the mutant bytes reach the consumer through a unit
test, not through a full `cargo xtask verify lean-pilot` replay.
