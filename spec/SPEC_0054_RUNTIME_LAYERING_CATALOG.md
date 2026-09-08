# SPEC_0054: Runtime Layering Catalog

## Status
REFERENCE

## Summary
Lookup catalog of exact runtime, backend, artifact, FMI, packaging, and
presentation carriers governed by SPEC_0029 §12.

## Specification

Each catalog entry supplies the exact owner, closed carrier, or enforcement
surface for an affirmative SPEC_0029 §12 obligation. An entry is normative only
through that parent link and introduces no additional requirement.

### 1. Detailed Layering Boundary Catalog

```
compiler/session → DAE structural → checked Algorithm Code / Solve lowering → checked export/runtime contracts → execution backend → simulation session → reporting → visualization
```

Ownership of each link in that chain is
[SPEC_0041 §4](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md#4-layering-ownership-catalog-spec_0029-12).

Execution adapters are not phases. Non-codegen phases must not depend on target
encoders, JITs, toolchains, or device APIs. Textual target policy lives in
`target.toml` and templates; Rust provides generic rendering, validation, and
IR capability probes. Unsupported capabilities report
`unsupported-feature:<feature_id>`. JIT/device adapters consume Solve IR or
generated artifacts through stable execution ABIs and equivalence tests.

FMI deployment is a checked export, not a textual DAE or Solve projection.
`rumoca-ir-solve::fmi` owns the checked component beside its kernel; no parallel
FMI IR crate or phase exists. The sole DAE-to-`SolveModel` construction in
`rumoca-phase-solve` atomically retains one sealed declaration/value catalog;
`rumoca-phase-solve::fmi` projects a completed `SolveModel` only and never
accepts matching DAE metadata, independent FMI inputs, or a separately
evaluated value vector. This checked, target-neutral semantic projection is
unconditional: feature-gating it would create two construction shapes for one
authority. Non-FMI consumers MUST NOT enable outward FMI-only packaging,
export, or runtime dependencies. Codegen retains FMI 2/3 lifecycle and ABI
text and MUST NOT repeat Modelica, DAE, or Solve lowering.
FMI 3 Model Exchange and Co-Simulation share this checked component view but
derive distinct sealed preparation arms. A concrete Co-Simulation solver choice
or configuration belongs to preparation above the IR and is linked through a
stable runtime ABI plus declared target assets; `rumoca-ir-solve` MUST NOT
depend on or expose facts from `rumoca-solver`, and a template MUST NOT choose
the solver or its behavioral policy.

Each rendered file in a target manifest selects exactly one IR crate through
its mandatory closed semantic context. The context enum is exactly `ast`,
`flat`, `dae`, `galec`, and `solve`, in one-to-one correspondence, in both
directions, with every `rumoca-ir-*` crate. CI enumerates the crates and proves
that equality mechanically. A root, checked view, output format, target
identity, package, or product name is never a semantic context. A file's
optional closed
`view` names one actual checked type inside its declared IR crate; construction
resolves absence explicitly to the canonical root for that crate and never
uses a Rust `Default`. Current noncanonical Solve views are `FmiComponent` and
`SolveAlgorithmBlock`. Below the manifest, one required product derives once
from the checked `(context, view)` file plans. A sole `SolveAlgorithmBlock`
rejects; only `AlgorithmCodePackage` files paired with correlated
`SolveAlgorithmBlock` files select one non-cloneable `SolveAlgorithmProduct`.
Mixed `SolveModel`/`FmiComponent` and every other independently provisioned
root mixture reject. No target-wide IR or root field, suffix inference, or
target-name dispatch exists.
The renderer selected from that required product consumes one complete checked
file plan and is the sole authority that may seal its final path and bytes.
The sealed rendered-file value retains the originating file plan's artifact
kind, semantic context, and checked view through checksum and package assembly;
those generic layers may order, hash, validate declared paths, and write it but
cannot construct, relabel, swap, or re-render it. A public path/content result
is projected only after this sealed build boundary. A callback returning an
independently constructible path/content pair is forbidden because the
checksum web would otherwise authenticate relabeled bytes.
One higher-ranked callback issues a fresh invariant target-invocation brand
whose constructor is private and whose lifetime cannot escape the callback.
The checked semantic input, trace authority, model identity, artifact session,
and every declared, prepared, and rendered phase carrier retain that same
brand; every cross-layer join requires one identical brand at the type level.
`StrictCompilation` alone issues the SPEC_0008 canonical artifact stem from its
resolved qualified model identity, and frames both that identity and stem into
the artifact-identity preimage; no consumer receives a free model-name string
or derives an alternate stem.
An ordinary borrow lifetime, integer declaration index, pointer/path equality,
or independently supplied artifact-facts value is not origin authority.
For DAE -> Algorithm Code projection, the origin session snapshots the exact
DAE-owned `SourceMap` before invoking one higher-ranked construction callback;
an affine issuer consumes the emitted block and package metadata into a
session-branded candidate, and no existing raw package can enter that session.
Every trust- or origin-authority-establishing operation first snapshots or
consumes plain checked inputs; before that authority step it MUST NOT execute a
caller-defined `Into`, `From`, `AsRef`, or other trait conversion. This rule is
scoped to trust/origin construction boundaries, not ordinary utility
constructors.
The invariant candidate proves candidate/session non-substitution only. Rust
does not prove the semantics of the callback or map-to-block correspondence,
so the architecture catalog's sole production call is load-bearing for that
remaining leg: phase-galec must wrap the complete DAE inspection/lowering and
select only `input.dae.source_map()`. Closure validates every retained exact or
nearest span against the source's actual byte range and UTF-8 character
boundaries, and records a deterministic digest of the exact ordered map bytes
for audit. The nonserialized traced
product retains package, brand, map, digest, and semantic model identity through
Solve and prepared rendering; no renderer accepts a replacement map or model
identity.
For a non-packaged target, manifest file declaration order is the sole render
and checksum order. A packaged target instead declares one explicit ordered
`[[package.members]]` closed sum containing every rendered file and every
expanded asset member exactly once. Checked construction resolves each member
to its exact file-plan or snapshotted-asset witness; that one mixed,
producer-first sequence is the sole render, checksum, directory, archive, and
authentication order. Construction resolves every checksum edge once and
requires the producer member to be strictly earlier than the consumer member;
an unknown, missing, or duplicate member, unknown producer, duplicate injection
key, self edge, or backward edge rejects before a checked target exists, naming
the responsible member and declaration facts. The issued member/carrier
sequence retains that order and its exact producer relations. Artifact
assembly consumes the sequence directly; it does not render a separate file
sequence and positionally join it later, topologically or lexically sort,
reorder, re-resolve identities, infer files/assets interleaving, repeat graph
validation, recover ordering from paths, or keep a parallel or optional copy
whose completeness needs a later assertion.
The checked bundle is affine: one consuming operation separates narrow
non-member metadata from exactly one owned render plan. Metadata exposes no
file declaration, template, member path, asset, manifest, or render-step
authority. A packaged plan is consumed once into an exhaustive product
alternative whose checked policy, owned role/path-only product layout, and
unique member fold are siblings; renderer preparation may borrow the
transferred layout without borrowing the fold it later consumes. The fold has
no member iterator and issues every rendered file in the retained sequence
under a fresh invariant higher-ranked member lifetime. Its non-`Clone`,
non-`Copy` issuer is the sole source of that member's declaration, template,
kind, context, view, and path facts. A private invocation-root closer
inseparably joins that issuer with the already-branded artifact session and
narrowed semantic renderer. The fold mapper may derive only the declared
checksum map from prior sealed payloads and then invoke that private closer;
the closer alone invokes strict rendering from the sealed template and its
own narrowed facts and consumes the issuer into an issuer-created completion
with fixed, non-higher-ranked payload and error types. The mapper and its
callback never author or return raw bytes. Only the fold erases the fresh
lifetime while adding the target-issued position and retaining all member
facts. A completion from one callback cannot close another callback, raw
payload cannot close a member, and the consumed plan cannot be folded again or
reentrantly. Assets have no caller-authored close operation: the fold itself
issues an opaque mapped asset retaining the exact snapshotted bytes, normalized
path, source/member identity, and position. Checksums and assets are therefore
fold-computed and the residual external byte-authoring class is empty. The
member fold, member issuer, completion, checksum arena, asset witness, and
completion close remain private to `rumoca-compile`; no downstream crate can
implement a mapper, obtain a member issuer, invoke completion, or supply
payload bytes. Compile invokes the strict phase-codegen engine but does not own
that engine's presentation semantics. Rust has no friend visibility across the
`rumoca-compile`/`rumoca-phase-codegen` crate boundary, so the presentation
authority issuer is mechanically public and is the explicit residual trusted
boundary. Its non-`Clone`, non-`Copy`, private-field authority is the only way
to satisfy the mechanically public strict-engine methods; the prepared direct
renderer, artifact-session, and direct product-specific member carriers named
by that seam have private fields and no independent public constructors. The
issuer grants presentation authority
only: it accepts no DAE/Algorithm Code/Solve parts, performs no semantic
lowering, and cannot mint a completed artifact. A syntax-tree architecture
gate admits exactly one non-test workspace call to the issuer, during complete
checked target construction; that sole-call gate is load-bearing because an
external crate can still invoke the mechanically public issuer. The only
public result of the compiler path is an affine completed artifact whose
ordered path/byte projection is passive publication authority. The architecture
catalog pins the typed carriers, the structural sole-call census, and the
absence of any alternate public byte-admission route.
`rumoca-phase-codegen` exposes a typed, read-only semantic view for each
supported IR and dispatches that view generically. The complete ownership chain
is `IR vocabulary -> checked root/view -> derived sealed prepared artifact
plan -> template`: semantic views contain facts only, while the prepared plan
owns target legality, layout, ABI, storage, legalization, solver binding, and
other refinement receipts. A prepared plan is derived from the checked view and
target profile; it is never independently selected, supplied, or reconstructed
by a template. Rendering never performs a compiler transformation or repairs
an artifact.

A new syntax over an existing prepared plan requires only a target directory;
a new certified projection adds one checked view; a new legalization adds a
preparation arm with its checker and receipt. A new IR crate is admitted only
for an independently meaningful semantic vocabulary with its own construction
discipline, evaluator/proof relation, and multiple non-presentation consumers.
Products, standards, package layouts, view counts, and lifecycle wrappers do
not meet that bar: per-product and per-standard IR crates are forbidden. A view
count may trigger review but cannot decide a crate split. An export IR selectable
by a file remains outside the canonical compiler pipeline.

Template composition cannot widen that view. Every `include`, `import`,
`from`, or `extends` lookup MUST resolve inside the current file's closed
semantic context. A global cross-target or cross-context semantic-template
registry is prohibited. Support partials and fallback template lookup remain
forbidden. A checked file plan MAY borrow one complete artifact template from
one explicitly named registered built-in owner only when the borrower retains
the same artifact kind and semantic context. Manifest construction validates
the exact owner and path once, rejects self/unknown/retired/suspended owners,
and retains the borrowing edge in canonical target identity. Built-in and
directory targets then consume the owner's same embedded bytes; they never
search another target or copy the source into a second owner. Copying template
source text into another renderer is not a substitute.

`rumoca-phase-codegen` Rust may derive target-neutral typed contexts, schedules,
shapes, dependency/bounds proofs, symbols, and provenance. It MUST NOT spell or
assemble target-language tokens, expressions, statements, declarations, or
files. Those belong entirely to each target's `target.toml` and MiniJinja
templates, so adding a textual target does not require a Rust dialect or
renderer. Generic template operations consume semantic IR vocabulary and fail
closed; they do not return pre-rendered language fragments.

Target-specific package/schema models, constants, filenames, and artifact
graphs also belong in the owning target directory, not in IR or phase Rust.
Generic documented artifact commands may hash rendered bytes, validate a
declared schema, and assemble the declared graph without understanding eFMI or
another target format. Generic on-disk package assembly is owned by the
`fmu-packaging` feature and MUST NOT depend on scheduled simulation, transports,
input devices, viewers, or process control.

Target assets follow the same ownership rule. Builtin target discovery embeds
arbitrary assets declared beneath a target directory; external targets resolve
declared asset sources relative to their own directory. Rust MUST NOT maintain
a target-format bundle registry or map names such as an eFMI schema bundle to
hardcoded files.

Target-specific semantic lowering is a compiler phase, not code generation.
`rumoca-phase-codegen/src` MUST NOT contain target-named subsystems such as
`galec/`, C lowering, XML manifest models, target manglers, or target dispatch.
It MAY contain small IR-specific adapters under `views/` when they expose only
typed, read-only semantic data. Checked export data and constructors belong to
their `rumoca-ir-*` crate; semantic projection belongs to its
`rumoca-phase-*` crate; all target syntax and presentation belong to the target
directory.

Within `rumoca-phase-codegen`, `src/codegen/` is reserved for the public
MiniJinja extension-command surface. Rendering orchestration belongs in generic
renderer modules and IR adapters belong under `src/views/`. Every registered
command MUST be pure, deterministic, target-neutral, fail closed, and have
documented template syntax, typed inputs/outputs, failure behavior, complexity,
and focused tests. A single registry is the source of truth for registration
and user-facing command documentation. Commands may return semantic values or
checked arithmetic/query results; they MUST NOT return target-language
fragments or perform lowering, name resolution, type repair, target dispatch,
file assembly, or escaping for a particular output language.

A registered lexical-codec filter is the sole exception to the escaping ban.
It remains target-neutral only under every constraint in this table:

| Constraint | Required behavior |
|---|---|
| Delegation | The filter body MUST delegate byte-for-byte to the canonical grammar-owned codec recorded in SPEC_0041; it MUST NOT contain a second operation or alternate implementation. |
| Semantic boundary | The filter MUST NOT inspect semantic IR. |
| Dialect boundary | The filter MUST NOT select or branch on a target dialect. |
| Compiler boundary | The filter MUST NOT infer, lower, repair, or otherwise transform compiler meaning. |
| Presentation boundary | The filter MUST NOT assemble syntax; surrounding delimiters and all other target-language tokens remain in the owning template. |
| Enforcement | Registry identity, exact delegate shape, byte-for-byte behavior, fail-closed input typing, and mutations that add branching or syntax assembly MUST be covered by architecture and focused tests. |

Architecture CI MUST reject production `rumoca-phase-codegen` Rust that builds
generated or template-context text with formatting, concatenation, replacement,
writer, or incremental string-assembly APIs. Diagnostic messages and generic
template/file transport are the only string-handling exceptions; their values
must not enter semantic template contexts. Target names are rendered from typed
identity/path segments in templates, not pre-mangled Rust strings.

Steady-state CI rejects reverse dependencies across this chain. `rumoca-compile`
MUST NOT depend on concrete solvers or visualization assets; backend-selection
APIs MUST affect runtime behavior, not only metadata.

## References

- [SPEC_0029 §12](SPEC_0029_CRATE_BOUNDARIES.md#12-runtime-backend-simulation-session-and-visualization-layering) — governing layering rule
- [SPEC_0041](SPEC_0041_CRATE_OWNERSHIP_CATALOG.md) — concise ownership lookup
