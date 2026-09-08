# Golden model: `UnitDerivative`

## Record identity

[`registry.toml`](registry.toml) is authoritative for this record's claim kind,
review time, canonical source path and digest, declared scenarios and endpoint
dispositions, coverage-footprint binding, and outstanding receipt ledger. The
independent `REJECT` verdict on the earlier candidate revision is recorded in
`dev/comm_channel.md` under its `2026-09-03T22:52Z` heading. The canonical
`.mo` fixture is consumed directly by both the core and FMI3 tests; this
record does not duplicate its source text.

## Declared endpoint cone

The machine-readable DAG in `registry.toml` is the cone authority. It records
each shared IR/transformation/product node, predecessor edge, current
disposition, evidence reference, and the exact observable leaves below:

1. The production DAE-to-Solve equation-refinement receipt.
2. Native execution through the selected RK45 backend, with the independent
   analytic oracle `x(t) = 2 + t` evaluated at each time the runtime actually
   reports on `0 <= t <= 1`.
3. FMI3 `modelDescription.xml`.
4. FMI3 `sources/buildDescription.xml`.
5. FMI3 `sources/model.c`, compiled and executed with `cc`.
6. FMI3 archive identity and exact member inventory.
7. FMI3 ME derivative/state lifecycle behavior.
8. FMI3 CS `doStep` lifecycle behavior.
9. Direct phase-GALEC continuous-dynamics refusal.
10. Registered `galec` continuous-dynamics refusal.
11. Registered `efmu` continuous-dynamics refusal.

CasADi, CUDA, DAE-Modelica, FMI 2, FMI-LS, JAX, MLIR, Rust, WGSL, and every
other unrequested backend are outside this record. They require separate
golden scenarios even when they reuse the same source.

## Intended semantic facts

- Resolved `x` is a scalar `Real`; the `der(x)` reference resolves to that
  declaration; `start=2.0` and `fixed=true` are retained exactly.
- Flat and DAE retain one state and one residual equivalent to
  `der(x) - 1.0 = 0`, joined by construction-issued identities rather than
  display names.
- Solve retains one state initialized bit-exactly to `2.0` and one derivative
  program equivalent to `[Const(1.0), StoreOutput(0)]`.
- Native and FMI3 CS execution produce `x(t)=2+t`; FMI3 ME exposes the same
  derivative and state through the declared value references.
- No exercised phase silently drops, defaults, repairs, or reconstructs a
  semantic fact. Unsupported eFMI semantics refuse at the owner required by
  the active target and GALEC diagnostic contracts.

The native observation is deliberately narrow. This constant-derivative model
checks backend selection, finite strictly increasing reported times with the
requested interval endpoints, and `x = 2 + t` within `1e-13` at each reported
time. It does **not** witness the requested interior output grid, RK45 order,
dense-interpolation coefficients or accuracy, rejected-step behavior, the PI
controller, or error-control behavior. Constant RK stages make all of those
claims vacuous; a nonconstant-dynamics golden model must carry them.

## Governing specifications

- `SPEC_0007`: IR stage and target-emission contracts.
- `SPEC_0008`: typed phase diagnostics.
- `SPEC_0022`: Modelica derivative and initialization semantics.
- `SPEC_0029`: crate and authority ownership.
- `SPEC_0033` §6c: working-model proof admission.
- `SPEC_0034`: eFMI/GALEC restriction and refusal behavior.
- `SPEC_0036`: valid-by-construction aggregates.
- `SPEC_0037`: proof claim levels and checker discipline.
- `SPEC_0040`, `SPEC_0043`, `SPEC_0044`, `SPEC_0053`, and `SPEC_0055`: cited
  catalog rows for the reached IR, construction, FMI execution, diagnostics,
  and target refinement.

## Claim and outstanding debt

This record is a `candidate`. Its scenario dispositions describe captured
observations or typed refusals from the exact tests bound in `registry.toml`;
they are not reviewed admission. `UnitDerivative.footprint.json` is a
separately versioned schema-3 candidate coverage capture. It remains untrusted,
never contributes to the aggregate golden numerator, and is not a production
snapshot merely because its digest is bound here. The command rejects stale
outer source, review-record, and footprint bindings before making a new
candidate capture.

Schema-2 reviewed admission is intentionally unavailable: a structurally valid
`reviewed` record receives a typed `ReviewedAdmissionNotImplemented` error
until semantic obligation evidence and a non-self-referential production
snapshot checker exist. The `dae-solve-production-refinement` endpoint remains
`blocked`: the C61 receipt mint can currently be separated from its check, so
no machine receipt is claimed for equation refinement. External conformance
beyond the pinned local validators is outside this record and can never become
a green skip.

## Evidence observed in this compliance check

- `cargo test -p rumoca --test suite_core unit_derivative --jobs 1`:
  `5 passed, 0 failed`. This inspects resolved AST, Flat, DAE, Solve, the
  narrow analytic native state/time relation, default-`fixed=false` rejection,
  direct plus registered eFMI refusal behavior, and the C61 receipt carried by
  the sole complete production lowering. It does not establish any RK45 or
  requested-interior-grid property listed in the non-claims above. These are
  focused behavioral results, not a golden admission.
- `cli_target_fmi::unit_derivative_fmi3_package_matches_me_and_cs_lifecycle_contract`
  under the `fmi` development shell: `1 passed, 0 failed`. This is the first
  execution of this test in the local environment. It inspects the archived
  `modelDescription.xml` and `sources/buildDescription.xml`, the declared
  variable inventory, archive membership, and the generated `sources/model.c`
  compiled and driven through the FMI3 ME and CS lifecycles, with the pinned
  external validators present.
- The FMI3 conformance prerequisites are satisfied locally: `bash`, `cc`,
  `cmake`, `fmpy` at the pinned `0.3.30`, `java`, and `xmllint`, together with
  the pinned FMI 2.0.5, FMI 3.0.2, VDMCheck2 and VDMCheck3 archives verified
  against their recorded SHA-256 digests.
- The generated-C write-mode route test passes, giving the FMI3 setter gate
  non-vacuous evidence on emitted C rather than on the Rust table alone.
- `cargo xtask verify quick` and `cargo xtask verify full` have not run on a
  dependency-closed frozen tree for this review.

## Defect found and corrected during this check

The FMI3 endpoint initially failed on `modelDescription/@modelName`, which was
emitted as the artifact identity stem `rm1_14_556e697444657269766174697665`
rather than as the model name. FMI 3.0.2 defines `modelName` as the name of the
model as used in the originating modeling environment, and its schema types the
attribute as `xs:string`, so no schema check can detect the substitution.

The cause was that one value served two distinct facts: `model_name` was bound
to the artifact stem, which is the value interpolated into output paths. The
stem grammar is length-framed and injective over arbitrary Modelica names, so
it necessarily stops coinciding with the source spelling. The correction adds
`model_qualified_name` as a separate bound fact taken from the canonical model
identity, and the FMI2 and FMI3 model descriptions take `modelName` from it,
while every path role continues to use the stem. Regeneration now emits
`modelName="UnitDerivative"`.

## Review notes and open blockers

1. `C61` now rides the sole complete `lower_solve_model` construction and
   inspects the derivative kernel, full JVP, visible row, and per-kind metadata,
   but no normative `SOLVE-C61` row exists yet. Its current fact projection
   aliases `MassMatrix::Identity` with empty diagonal/sparse representations,
   checks only derivative-pattern presence, and can miss an extra empty scalar
   compute node. Its negative-trait and root-binding architecture checks are
   also not yet structural enough to close manual implementations or coordinated
   binding substitutions: in particular, C60 can be changed to check a second
   Solve root while C61 checks and installs the original without tripping the
   current C61 gate. This is an L2 runtime-witness boundary, not a theorem or an
   L3/L4 formal proof.
2. The exercised Flat-to-DAE path still assigns roles by `VarName`, retrieves a
   reusable derivative plan by reparsing an expression, and previously
   recomputed equation partitions during construction. A root-correlated
   source-ordered equation-sequence cut is in progress; occurrence-keyed affine
   role and derivative products remain required afterward.
3. The candidate record binds a schema-3 measured coverage footprint under
   `coverage_footprint` in `registry.toml`. The capture identifies production
   lines executed from strict parse to solver, but neither coverage nor its
   self-recorded source digests prove semantics or constitute the independent
   production snapshot required for reviewed admission. Public `SimResult`
   data and the candidate footprint remain untrusted.
4. The FMI3 evidence was rerun under the `fmi` development shell for this
   review. Any future use of the candidate footprint requires a fresh capture
   after a pinned file changes, rerunning every bound scenario and rebinding
   its digest; even a fresh capture does not grant reviewed admission.

### Blockers closed since the previous revision

- FMI write eligibility is no longer decided twice. The duplicate kernel
  predicate is deleted, admission is decided by the version write-mode tables
  alone, and the generated-C route test supplies non-vacuous setter evidence.
- Target artifact facts no longer make a target-aware round trip through
  phase-codegen. Path, identity, checksum and membership authority stays in
  `rumoca-compile`, and the rendering phase receives pathless, identity-free
  bindings.
- The toolchain-free XML and generated-C assertions are executed rather than
  gated away, because the external prerequisites are now present locally.
- Missing external conformance can no longer appear as a green skipped test.
  The FMI3 lifecycle test asserts its prerequisites and fails when they are
  absent, and it now runs with them present.
- C61 is no longer a test-only alternate lowering. Production native/FMI and
  target-rendering routes obtain their complete Solve root through the same
  checked `LoweredSolveModel`; an in-profile mismatch is a typed lowering error,
  while an out-of-profile model retains a typed unclaimed disposition.

The registered eFMI EC009 behavior is not an open blocker. SPEC_0053 requires
the checked target capability profile to reject continuous states before
GALEC/eFMI product construction, while GAL-025 governs its limitation wording.
The direct phase route separately proves the typed EGT001 disposition.

## Cone review findings (2026-09-05)

The claim was demoted from `reviewed` to `candidate` when adversarial review of the
measured cone began. The future `reviewed` wire shape carries an `outstanding`
proof-debt ledger, but reviewed admission is deliberately unimplemented, so the debt
remains review prose in this candidate record.

Four slices of the 469-file, 51,472-line measured cone have been reviewed against the
frozen footprint `228a9df8cd97ab6f...`: `rumoca-ir-solve` (5,374 covered lines),
`rumoca-phase-solve` (3,394), `rumoca-phase-flatten/src/pipeline` (2,387) and
`rumoca-phase-parse` (2,893). That is 14,048 lines, 27 percent of the cone. Every
`source_sha256` in all four slices was verified against the tree before review.

**No finding produces a wrong output for this model.** Every one is either a proof that
does not establish what it claims, or a correctness that holds because of this model's
size.

### Obligations that are vacuous at one state

Two checks cannot distinguish a correct compiler from an incorrect one on a model with
a single state, because count and identity coincide:

- `rumoca-ir-solve/src/fmi.rs:634-638` proves FMI state/derivative correspondence by
  comparing a sum of scalar counts against `state_scalar_count`.
  `validate_variable_storage_runs` (`lib.rs:1229-1318`) does not prove that storage runs
  are pairwise disjoint, that State runs lie in `0..state_scalar_count`, or that catalog
  order is ascending Y order. Two states declared `a@Y{1}, b@Y{0}` satisfy the count
  check with swapped derivative links.
- The SOLVE-C61 executable-owner census
  (`rumoca-phase-solve/src/scalar_constant_derivative_refinement.rs:850-862`) was shown
  inert by a shape-preserving mutation: iterating the census over an empty range leaves
  all 171 `rumoca-phase-solve` tests and every C61 architecture gate green. The only
  failure it produces is the registry digest binding, which fails for any byte change.

A model exercising these obligations needs at least two states whose declaration order
differs from their storage order.

### Proof-strength findings

- SOLVE-C61's receipt is still constructed beside its checks rather than produced by
  them. The wrapper level is tail-check gated, but inside
  `check_scalar_constant_derivative_refinement` every sub-check returns `Result<(), E>`
  and the receipt is a separate literal. Closure would give each sub-check its own
  private receipt and make the C61 receipt constructible only from all of them by value.
- `rumoca-ir-solve`: the MLS 3.6 section 8.6 `fixed` fact and the initialization system
  are uncorrelated. `variable_catalog.rs:719-724` mints `Exact` from `(State, Fixed)`;
  the initialization aggregate is sealed without seeing the catalog. A sealed model can
  report a state as `Exact` while its storage slot is a projection unknown.
- `rumoca-phase-flatten`: `context_and_tests/component_instance.rs:214-224` attaches the
  declaring body's occurrence to references inside `start`, `min`, `max` and `nominal`,
  though those expressions are spelled in the modifier's scope and
  `attribute_source_scopes` carries the correct scope. The sibling binding path handles
  this correctly at `component_instance.rs:47-68`. This model is unaffected because its
  `start` is a literal with no references to mis-scope.
- `rumoca-phase-parse`: the `der` keyword's identity is discarded at
  `expressions.rs:1159-1186` and re-derived by string comparison at six downstream
  sites; `elements.rs:86-118` fabricates a `start` literal whose token carries no source
  location, selected by a type-name string match that duplicates
  `PredefinedComponentType::from_name`.

### Verified correct, and worth recording

- `start = 2.0, fixed = true` survives parsing and flattening as a *stated*
  modification, distinguishable from a default, and `start` is never used as a binding
  (MLS 3.6 section 8.6). Both the parse and flatten layers state this rule explicitly.
- The grammar matches MLS 3.6 Appendix B production by production over everything this
  model exercises, including operator precedence and associativity. Two deviations exist
  outside this model's path: `factor` accepts a right-hand chain that Appendix B does not
  (repaired at conversion), and `some-equation` admits keyword calls.
- An unknown component modifier is refused: `Real x(start = 2.0, fixd = true)` is
  rejected with `ET001 unknown modifier` at the correct span. The parser carries a
  duplicate of this check that no input reaches; the obligation is met by typecheck.
- No production code reads `start` as a value or falls back from a binding to a start
  value; the `instantiate_value_fabrication` gates pass.
