# Early query collection

Candidate checkpoint, 2026-09-07. Not integrated into Charon's live declaration
constructor or Rumoca's toolchain. These are owned required-path observations,
not callable predicates or a successful-compilation certificate.

## Boundary

`src/collection.rs` installs a borrow-checking query observer while retaining
the actual previously installed provider. The public rustc consumer must run
before MIR can be stolen, including by constant evaluation. It does not return
the ordinary query's hidden-type result, so the ordinary provider still runs.
The collector requests `RegionInferenceContext`, sufficient for the existing
required-path extractor; it does not run Polonius or collect declared bounds.

A once-computed HIR partition identifies type-checking roots containing closures.
Other roots delegate directly to the ordinary query. Thus additional borrow
checking is limited to closure-bearing roots, not every nontrivial function.
Remaining overhead on those roots still needs measurement during integration.

An invocation guard owns the registration and must outlive the entire compiler
run, including configuration and worker callbacks. Concurrent invocations are
rejected. The shared store contains no rustc-lifetime data, unsafe casts or
borrowed compiler pointers. Retrieval checks the declaration kind, requests its
own type-checking root, then returns its owned fact record. `NotClosure`,
`CompilationRejected`, `MissingFacts`, and `InvalidFacts` are distinct from a
successfully collected empty relation set. A duplicate producing query is an
internal invariant failure, not a silent record replacement.

Publication follows the ordinary query's success and refuses already diagnosed
errors. A **later** compiler error can still reject the invocation: exporters
must additionally require the whole compiler run to succeed before publishing
an artifact. Early observations do not by themselves certify that final state.

## Executable controls

Build the diagnostic Cargo package as documented in [README.md](README.md),
using the same pinned rustc-dev toolchain. Its `debug/collection-driver`
links the fact library and its dependencies. It is a driver executable, not a shell
script. Pass ordinary rustc arguments, including an explicit crate name,
`--edition=2021 --crate-type=rlib --emit=metadata --out-dir <output> -Zthreads=4`
and optionally `-C incremental=<output>/incremental`.

The driver executes each source in **two actual compiler sessions**. In each it
retrieves facts in `after_expansion`, then retrieves the same issued `Arc` after
forcing optimized MIR in `after_analysis`. Between sessions the first facts stay
alive: identical new facts must have different `Arc` identities, preventing stale
registration reuse. The second session also composes another query provider and
checks that the collector delegates to it; configuring over an existing query
override is rejected instead of silently replacing it.

The seven existing source files listed in the parent README plus
`fixtures/early-const.rs` exercise **26 closure bodies**: named/free and truly
higher-ranked signatures, captures, invariant and covariant positions, phantom
brands, nested closures, and a closure created by constant evaluation. All pass
the two-session checks with four rustc workers and incremental compilation.
The source fixture's native test also passes. This checks collection and
retirement, not preservation of Charon's explicitly requested MIR level.

`fixtures/dangling-rejected.rs` is intentionally invalid Rust. The driver must
reject it with rustc E0373 and `COLLECTION_COMPILATION_REJECTED`, not publish
facts for it. The driver catches rustc's fatal-error signal and exits 1.

`mutations/drop-collected-records.patch` applies to a **copy** of this candidate.
It discards records after successful checking. Both mutated library and unchanged
driver must compile, but `simpler-closures.rs` must then fail specifically with
`MissingFacts`. This demonstrates that successful Rust compilation without issued
facts cannot masquerade as an empty checked record. The live source is unchanged.

Strict Clippy covers the library, driver and native `--test` configuration with
`-Dwarnings -Dclippy::all -Dclippy::too_many_lines -Dclippy::excessive_nesting`.
No suppressions are added. Disposable evidence is under
`/tmp/rumoca-closure-collection-jUn7qq`; source inputs remain here.

## Remaining integration obligations

Preserve any explicitly requested Built/Promoted MIR before collection consumes
it; the present driver proves retained facts survive later queries, not that
Charon's source representation is preserved. Do not repair already emitted IR
or silently substitute optimized MIR for the requested representation.

Map every free structural occurrence to its declaration's existing parameter
binder, preserve true higher-ranked regions and caller substitution, and issue
the required outlives constraints at the owning constructor. Cross-crate records
must bind schema/tool/compiler identity and the resolved artifact's crate hash;
stable item identity alone does not detect changed-source staleness. Missing
records are not empty facts. Cargo cache ownership and full-MIR standard-library
production remain unfinished. No DAE diagnostic, golden admission, or additional
UnitDerivative relation is closed by the collector alone.
