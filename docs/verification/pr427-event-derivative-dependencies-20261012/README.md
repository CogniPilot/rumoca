# Ordinary event derivative dependencies and final scheduling

The focused production source is `050609ac9758265bd9355e4bf1a713dc89475d3c`.
All nine counted groups pass, with 72 executions covering 68 unique tests.
The single merge control is included in the 36 structural tests; the three
alias controls are included in the 12 event tests. Scoped all-target,
all-feature Clippy and workspace formatting also pass. The exact commands,
source/input hashes, raw logs, guardians and limits are in [receipt.json](receipt.json).
Source and external library guards held throughout. No resource stop occurred.

At `fcccc0b72`, the unchanged independent tensor-alias case computed `b=1`
instead of `b=11`: projection found `rhs[1]`, then declaration-only alias
closure reopened the whole `rhs={10,b}` and added the unused second lane.
The source query now retains matched `(state, scalar)` authority and the
matched expression's scalar/domain point, following an exact algebraic alias
while that tuple is available. A requested second lane still contributes its
real dependency, and the both-lanes control prevents first-lane suppression.
Derivative-free roots retain the existing raw query. Full-value occurrences
remain full-value reads; no tensor IR or equation family is scalar-expanded.

Final ordinary-event scheduling consumes the issued current/entry facts once,
including dependencies on unconditional producers. Alias refresh liveness is
consumer-qualified. Its IDs identify context and liveness, not private storage:
refreshes retain the canonical physical variable slots. Ordinary observation
cycles retain iteration semantics and trigger entry constraints, including
alias paths and an external entry reader. Actual variable clock ownership
selects the MLS 16 boundary; periodic task metadata alone does not. Exact,
opaque and direct clock reads preserve that boundary.

At `f48afd076`, actual canonical lane collectors prove `dependencies(rhs)` is
empty for the first lane and `{b}` for the second. Reverse fusion then loses
`{b}` because map extension overwrites the earlier adjacency. The union repair
retains both issued sets in either merge order; the unchanged control passes.
The preceding nonexistent-literal-API and scalar-owner fixture refusals are
retained separately as invalid harness attempts, not merge counterexamples.

The governing contracts are SPEC_0007/SPEC_0040 SOLVE-C28, C57 and C58,
SPEC_0032 §1, SPEC_0029 §§1/3, and SPEC_0033 §§2–3. They preserve MLS 3.7
§8.3.5.1/§8.6 ordinary when/event-iteration semantics and §16.5.1 sampled
left-limit boundaries. No wire/schema, source model, tolerance or budget changed.

Runtime controls select Interpreter and Auto; this receipt does not prove a
native no-decline backend. It does not establish blanket per-lane precision
for opaque writers or all implicit model-arm relation masks. Root integration
must preserve its existing `events/roots.rs` extraction and
`ordinary_trigger_edges`; do not restore the older continuous-root functions.
Combined-source architecture, broad tests, original Booster mission and final
CI remain separate qualification work.
