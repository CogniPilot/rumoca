# Required iterator method interfaces

Read-only producer diagnostic under SPEC_0033 §§2a/4/6 and experimental
SPEC_0037 §6. No producer, library model, live pin, Rumoca production source,
proof allowance or generated Lean was changed. Independent review is pending.

## Reproduction

The actual DAE admission output fails Lean when constructing an Iterator
implementation with `map`, `fold`, and `sum`, constructing ExactSizeIterator
with `len`, and calling a generic iterator's `fold`. These are not twenty
independent bugs: several other diagnostics are fallout or float-type binding.

`iterator-contract.rs` reduces the required caller interfaces to two functions:
`iterator_len<I: ExactSizeIterator>` and
`fold_from<I: Iterator<Item = usize>>`. No DAE, lifetime brands, user-defined
trait, or custom iterator implementation is needed. The second takes an initial
accumulator; this is a legitimate fold operation rather than a redundant
spelling of sum. The native oracle uses explicit empty/singleton/two-item
expected lengths and sums. It does not compare fold to the same library sum.

Final source compiles, passes its native test, strict Clippy and rustfmt.
Charon and Aeneas both succeed. Generated Lean fails exactly these calls:

```
IteratorContractSource.lean:56:48: Invalid field len
  coreitertraitsexact_sizeExactSizeIteratorInst.len values
IteratorContractSource.lean:124:43: Invalid field fold
  coreitertraitsiteratorIteratorIUsizeInst.fold ...
```

The independent `-filter-trait-methods` diagnostic produces byte-identical
Lean and the same two errors. The option filters implementation members;
it cannot supply a field used by a caller. It is not adopted as a fix.

## Earliest mismatch and competing causes

The imported Aeneas model in `Aeneas/Std/Core/Iter.lean:76` gives Iterator
`next`, `step_by`, `enumerate`, and `take`, but no `fold`. At line216,
ExactSizeIterator contains its parent iterator and no `len`. The generated
callers require fields outside those model interfaces. The installed emitter's
`Extract.ml:3653` explicitly limits filtering to implementation member emission.

This rules out DAE construction, branding, and an implementation-record-only
filter omission as necessary causes of the reduced failure. This is required
library-interface/translation support, not a reason to erase a Rust call or
weaken the final Lean checker. The generated default methods are opaque axioms
in this diagnostic, so merely making its module type-check would not constitute
source-preservation proof or whole-DAE admission.

Extending the model requires design work, not guessed fields: Iterator and
Sum have dependent interfaces, and a generic Rust implementor can override
methods. Replacing its supplied fold by a hand-written next-based loop would
need a separate source-backed refinement argument; the trait name alone does
not justify that replacement. No such repair or theorem is claimed here.

## Evidence and boundaries

`run-evidence.json` binds the exact commands, working directories, expected and
observed exits, terminal diagnostics, and source/tool/output hashes. The first
fixture used a zero-initialized sum fold and failed strict Clippy; its source
is retained as history, not hidden by a lint allowance. All final checks were
rerun after introducing the explicit initial accumulator. No native or Lean
mutation proof is claimed: this is an unresolved translation counterexample.

`observation_chunks` are observation identifiers, not file paths. Each record
retains its terminal output directly in `output`; redirected logs are also
hash-bound in `inputs`. No agent tool-history lookup is needed to read them.

The Charon launcher/driver come from the frozen
`charon/candidates/associated-demand-retention` source/recipe/build chain,
hashes `10ca62cd`/`d9ae2041`. Aeneas comes from the frozen
`implicit-alias/package.nix` chain, binary `10916246`. These are unadopted
candidate tools, not the canonical pins.

The temporary evidence has a completed, gzip-verified durable backup:
`/home/jgoppert/rumoca-checkpoints/2026-09-08-review-closure-Eg9ZAT/sources-evidence-and-gates.tar.gz`,
SHA256 `eebd61fade811fa216a3064a39094559489171171675c373fe247ae74e7c250e`.
Archive prefix `rumoca-iterator-contract-AWqaUC/` contains the initial source,
LLBC, generated Lean and logs at the paths recorded by this receipt. Candidate
source/receipt chains are also archived. Compiled tools/sysroots are not;
they require the named builds/pins. The archive predates this clarification,
not the frozen evidence. The original command paths remain recorded honestly.

Separate production gate/count status is in
`dev/2026-09-08-review-closure-gates.json` and the main roadmap, not established
by this receipt. This fixture does not establish a new capability, general iterator semantics,
or a correct model for arbitrary overridden methods. Full upstream suites,
canonical adoption, and whole DAE Lean admission were not run here.
