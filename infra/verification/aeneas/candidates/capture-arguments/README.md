# Producer-supplied capture arguments

Unadopted test companion for the Charon capture-arguments candidate. It removes
the entire `fix_closure_signature_regions` heuristic, including its registration.
The independent closure-method lifetime pass is unchanged; its removal requires
the separate method-constraint producer fix.

The package inherits the adopted Aeneas patches and explicitly depends on the
capture-arguments candidate Charon. Both executables in its `bin` directory
belong to the candidate pair. No live tool pin is changed by this recipe.

```console
nix build --impure --no-link --print-out-paths --cores 4 --max-jobs 1 --file infra/verification/aeneas/candidates/capture-arguments/package.nix
```

Acceptance requires positive controls to translate and Lean-check without the
signature guess, source-faithful lifetime arguments, upstream regression gates,
and adversarial review. A successful package build alone establishes none of
those semantic properties. No current golden-model proof count changes.

On 2026-09-07, the paired whole iterator and cross-crate controls translate and
Lean-check, with the existing external iterator-library axiom disclosed. The
higher-ranked controls retain only their separate five closure-method errors.
Claude independently reproduced both fixture criteria. The real Rumoca DAE
replay drops from seven diagnostics/four sites to five/two, without this
signature rewrite. This is not complete DAE translation; live adoption remains
separate from retaining the reviewed implementation.
