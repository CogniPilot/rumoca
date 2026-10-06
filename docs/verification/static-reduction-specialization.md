# Static reduction domains and compact-template certificates

The frozen `SPD6Solve` counterexample uses static outer for-equations with
`sum(L[i,k]*L[i,k] for k in 1:i-1)`. A six-element `PrefixSum` reproduces its
ToDae rejection without Cholesky. Flatten already emitted exact concrete rows,
but its optional symbolic family template retained an outer index whose value
was unavailable to the reduction-domain proof.

Flatten now omits that template when a nested comprehension domain reads an
enclosing binder. The existing materialized rows and checked family domain
remain authoritative; corner-only cheapening requires an eligible template.
Comprehension binders shadow enclosing names in their bodies, while their
ranges still read the enclosing scope. Fixed reduction domains retain their
compact templates. Runtime input extents continue to fail during Flatten.

The fixed-domain controls exposed a second defect: comprehension planning
visited materialized rows but omitted symbolic continuous/initial templates.
Those rows contained expanded arrays, so construction later reached a valid
template comprehension without its exact certificate and panicked. Planning now
uses the shared equation-owner visitor. Domain, provenance, and complete-index
identity checks remain intact.

Focused verification: eight native tests pass, including exact prefix lengths
`[1,2,3,4,5]`, state-derivative rows, fixed compact templates, lexical shadowing,
initialization templates, runtime-extent rejection, the unchanged generic
six-by-six Cholesky/sixteen-RHS source, and the unchanged fifteen-state covariance
source with nested fixed reductions.

The complete native test suites for `rumoca-phase-flatten`, `rumoca-phase-dae`,
and `rumoca-compile` also pass. Strict all-target/all-feature Clippy passes for
the same packages, and the changed Rust files pass the pinned formatter check.

```sh
CARGO_BUILD_JOBS=2 RUST_TEST_THREADS=2 RAYON_NUM_THREADS=2 \
  cargo test -p rumoca-compile static_reduction
CARGO_BUILD_JOBS=2 RUST_TEST_THREADS=2 RAYON_NUM_THREADS=2 \
  cargo clippy -p rumoca-phase-flatten -p rumoca-phase-dae -p rumoca-compile \
  --all-targets --all-features -- -D warnings
```

These checks establish compiler admission and ownership, rather than numerical
cohort parity. A combined fixed-20 MSL/OMC canary delta and final workspace gates
remain pending on the integrated branch. Empty reductions with lost element
types are outside this change.
