# B1c source-domain binder authority

The `817d814ce` candidate's source-family recognition does not positively
compare an LHS iterator declaration with the source domain's iterator
declaration. `StructuredIndexBinder.id` is an axis ordinal. The current
domain retains display spelling and bounds, but no declaration identity.
`flatten::equations::structured_domain::compact_binder` drops the existing
`ast::ForIndex.binder` provenance.

The retained PR421 published `f84a4304e` and reviewed `955d8696b` use the same
domain representation. They do not already supply this missing proof.
The reusable representation is Core's `IndexBinderProvenance::Source(DefId)` /
`Generated`, introduced for source iterator lowering. Ownership is recorded
in SPEC_0041's iterator-provenance row, linked by SPEC_0029. SPEC_0001's
identity domains and semantic identity keys prohibit treating display text
or an unrelated nonzero DefId as the source declaration's identity.
SPEC_0032's compact-domain/scalar-view contract retains the source domain;
MLS §§8.3.3 and 10.6.1 define iterator scope.

The test-only witness keeps a complete ordered 2×2 Boolean assignment,
matching constant source and materialized bodies, and its valid LHS control.
It changes only the first LHS iterator's declaration to a foreign declaration
with the same displayed spelling. No RHS binder reference can accidentally
reject the foreign LHS. The recognizer must decline this candidate.

Qualification is **UNRUN**. Source inspection predicts that the final
rejection assertion fails at 817; this is not a measured runtime result.
Root owns the heavy lane. Exact selector (one test):

```sh
cargo test --locked --offline -p rumoca-phase-dae --lib source_family_rejects_foreign_lhs_binder_even_without_rhs_binder_reads
```

The consistent repair is to retain the already-issued provenance in the
canonical Core structured binder, propagate `ForIndex.binder` at the Flatten
producer, preserve it while offsetting/lifting axes, and compare actual source
DefIds at source-family admission. Ordinal ids and bounds stay unchanged.
Absent provenance must decline the shortcut. `Generated` must stay distinct
from source identity; without an issued generated-reference identity, it also
must decline this source-template shortcut rather than match on spelling.

At this source pin, `StructuredIndexBinder {` appears 189 times in 110 Rust
files (including the definition/impl). Most sites are constructor fixtures;
all need inspection if a field is added. Existing geometry-only constructors
must explicitly retain absent provenance rather than invent source IDs.
The source producer and provenance-aware fixtures carry existing actual IDs.
Adding a parallel provenance list to the template would touch fewer sites but
would require a second domain-axis alignment invariant; that is not proposed.
