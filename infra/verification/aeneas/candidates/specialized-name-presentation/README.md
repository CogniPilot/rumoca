# Preserve specialized identities through name presentation

Status: frozen candidate, **PENDING Claude adversarial review; not adopted**.
Precursor: `a1daf47556c1a6ffd7f4b203235ff09711089a85`, `msl-trace-parity-50`.
Process anchors: SPEC_0033 §§2a/4/6 and experimental SPEC_0037 §6.
The predecessor is [specialized-impl-names](../specialized-impl-names/README.md).
This packet does not change its bytes, Rumoca source, brands, or live tool pins.

## Reproductions and producer

The predecessor's actual DAE replay failed at two name-presentation owners:

- Appending `PeIdent("default")` after `PeInstantiated` hid the specialization
  binder from name conversion. Different method bodies acquired the same name.
  `default-methods.rs` independently reproduces this with a generic default and
  two branded specializations: Charon95cfa4/0, predecessor137267/1.
- `ctx_compute_var_basename` assumed the final path component was an identifier.
  `local-basename.rs` reproduces the exact exception with two functions and a
  specialized wrapper: Charonee6504/0, predecessord24957/2.

Native behavior tests8beac4/a1b8c4 pass before the producer change. These are
valid source/LLBC inputs, not malformed brands or missing user annotations.
Disabling specialization is not a repair. Changing the clash checker is also
unnecessary: complete-name conversion fixes valid input, while deliberately
conflicting user names still refuse.

`prepared-name-components.patch` changes only `extract/ExtractBase.ml`:

1. The shared function-name converter returns prepared string components before
   flattening, preserving the original specialization during conversion.
2. Synthetic constructor/default/impl components are added to those prepared
   components, never spliced into a still-bound Rust identity.
3. One formatter handles flattening and backend casing for both callers.
4. Local-variable hints reuse `name_last_elem_as_ident`, which uses the existing
   logical-name suffix helper. This hint is not a declaration identity.

The superseded string-converter helper is removed, not retained as an adapter.
The installed producer has41 inserted/71 deleted lines (net **30 fewer**).
Only metadata presentation changes; no Rust executable/IR layout cost is added.
Translator runtime is not benchmarked. General name injectivity, renamed or
multi-target specialization, and non-Lean backend verification remain unproved.

Final patch: `bcbd909f4633ae9e46508e71479b06ff46298bf9e1af4f73a890ac106b33b918`.
Installed producer: `2a46bb480a2a4e9ef3feb0ea38b9685d610176ce65cd7601e8103e1748b5309b`.
Binary: `63e1c7db3e7fff34b1066017f290839b3efa6d91a58de45ec89d35487ee7900d`.
Nix build34d1f8/0 and exact predecessor zero-fuzz patch check649637/0 pass.

## Focused validation

Exact commands, terminal statuses and byte bindings are in `run-evidence.json`.

| Check | Result | Meaning |
|---|---|---|
| Default bodies13c10e, Lean6479a1, lawse32f7d | Pass | Both specialized comparisons preserve inequality |
| Local temporary2b649f, Lean75eb83, law5f8445 | Pass | Every U32 payload is preserved |
| Native source mutation57eeac | Expected failure | Removing the default body's negation fails the unchanged native test |
| Mutant generated Lean40567f | Pass | Wrong-source translation itself is well typed |
| Unchanged mutant laws329763 | Expected failure | Both equality cases reduce to False; axiom guards also refuse |
| Mutant witness2763a6 | Pass | Concrete equal inputs incorrectly report that they differ |
| Wrong witness on original6c86f5 | Expected failure | Original generated code does not support that wrong answer |
| Ordinary constructor/method controls | Pass | Before/after output byte-identical6524e6; explicit namespace changes only the intended method/call |
| Ordinary and namespace lawsdf5204/a92ea7 | Pass | Both modes preserve every payload |
| Unspecialized default control80ccdc/63ac6b | Pass | Byte-identical6720ab and Lean-validbbea9e; not a substitute execution path |
| Prior impl-name regressioncdc783 | Pass | Generated Lean byte-identical5255fd |
| Explicit duplicate names5d5480 | Expected failure | Collision check remains enabled; no partial-file admission |
| Scoped Rust lint/format, OCaml format | Pass | No lint suppressions added |
| UnitDerivative01869f | 5/5 pass | Actual production compile, simulation, receipt and typed refusals |

The two default-method laws use `propext` and `Quot.sound`; local and control
laws report no axioms. The concrete mutant witness additionally names
`Classical.choice` through the library's typed U32 literals. Exact inventories
are guarded. No generated Lean is hand-edited. Initial `rfl` proofs failed on
Boolean negation; explicit equality case analysis closes them. Unused simp
arguments were removed instead of disabling the warning.

`constructor-control.patch` derives a specialized constructor-function case
from the retained local source without duplicating the whole fixture. Its native
testf7bb2e passes. Both predecessor71e136 and candidatee4cef3 still fail earlier
at `Interp.ml:621`; the candidate removes the extra constructor-name collision
and local-basename exception. This is a retained red control, not a successful
constructor translation or proof. Its partial module is never admitted.

## Actual DAE and next boundaries

The exact valid DAE LLBC527bb939 now completes Aeneas emission: fb1649/0,
79.785695 seconds, no Aeneas error, six missing-library-field warnings.
Generated Lean SHA256:
`4415ce228c7e80d09a692c1c01f0aee2c771c24cee750a9d606a0858ca8c4b3c`.
Lean04e8ab/1 rejects it with21 diagnostic headers (e5f2b4):

- Missing iterator `map`/`fold`/`sum` and exact-size `len` fields.
- F64-related universe inference in literal/parameter declarations.
- An uninferred phantom type argument in derived StateId equality.
- Cascading unknown declarations, invalid patterns, and sorry-dependent uses.

These are observed error groups, not a completed root-cause proof for each.
The full derived-Eq fixture also now emits bf7e8d/0 but Lean a3602b/1 rejects
the isolated phantom-type inference issue. Do not erase Rust brands or edit the
generated call to bypass it; fix the responsible translator/library owner next.

No whole-DAE Lean admission, second UD relation, golden admission, or quick/full
green is claimed. The broader Aeneas suite and non-Lean verifier gates were not
run. Canonical live-tool adoption, predecessor reviews, signed checkpoint and
remote CI remain pending. Review must assess the prepared-name conversion,
namespace/collision controls, source mutation, and these explicit remaining limits.

Build: `nix-build infra/verification/aeneas/candidates/specialized-name-presentation/package.nix --no-out-link --cores 4 --max-jobs 1`.
