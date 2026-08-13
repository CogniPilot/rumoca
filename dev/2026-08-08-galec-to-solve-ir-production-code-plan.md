# GALEC to Solve IR Production Code Plan

**Date:** 2026-08-08  
**Status:** Approved implementation roadmap; Milestone 2 in progress  
**Scope:** eFMI Algorithm Code and Production Code lowering, Solve IR contracts,
shared discrete causalization, evaluation, code generation, and validation

## Progress Tracking

This section is the durable implementation ledger. Update it whenever a
milestone changes state, a gate runs, or a design decision changes. A milestone
is complete only when its exit evidence is recorded here.

**Legend:** `PENDING` · `IN PROGRESS` · `BLOCKED` · `COMPLETE`

| Milestone | Status | Exit evidence | Next action |
|---|---|---|---|
| 0. Specification amendments | **COMPLETE** | Architecture: 137/137; suite/spec gates: 16/16 | Keep implementation synchronized with accepted contracts |
| 1. Shared causal-discrete plan | **COMPLETE** | Shared B.1b orientation/DAG: 196 focused tests pass; direct Modelica-to-GALEC export: 29/29 integration tests pass; three over-generalized scheduling counterexamples captured and resolved | Keep MLS event iteration distinct from GALEC post-admissibility scheduling |
| 2. Typed Solve program foundation | **IN PROGRESS** | Third checked vocabulary increment: bounded actions/branches/loops, lexical method scopes, checked call ABI, and limit/signal effects; 197/197 `rumoca-ir-solve` tests pass (22 new) | Construct the `SolveAlgorithmBlock` lifecycle root without migrating Production C yet |
| 3. `SolveAlgorithmBlock` root (M3) | **PENDING** | — | Construct lifecycle, lexical storage, actions, calls, signals, and correlations |
| 4. Production C migration (M4) | **PENDING** | — | Render checked C ABI/program view and delete Jinja semantic lowering |
| 5. Backend reuse and optimization | **PENDING** | — | Enable only profile-proven evaluators/backends |
| 6. Repository verification and delivery | **PENDING** | — | Run focused, quick, full, CI, commit, and push gates |

Spec pending-markers reference these as M3-4.

### Fixed decisions

| Decision | Status | Consequence |
|---|---|---|
| Production Code consumes `SolveAlgorithmBlock` | Accepted | High-level GALEC views no longer feed C/H templates |
| Real arithmetic is profile-bound, not hard-coded `Real64` | Accepted | Float32 Production C rounding is reproducible in the Solve evaluator |
| Method locals are lexical storage | Accepted | Locals do not inflate persistent state or whole-block stack lifetime |
| ABI choices are checked before rendering | Accepted | Templates spell a proven ABI; they never select value/reference/output-buffer passing |
| Aggregate operations remain first-class | Accepted | Copy/fill/constructor/projection/call operations cannot regress into coordinate expansion |
| No compatibility path | Accepted | The old Algorithm-Code-to-C template lowering is deleted in the migration cutover |
| Modelica-to-GALEC remains first-class | Accepted | `.alg` continues to render directly from checked `AlgorithmCodePackage`; Solve refinement is only the Production C/H branch |

### Verification ledger

| Date | Scope | Result | Evidence / counterexample disposition |
|---|---|---|---|
| 2026-08-08 | Pre-roadmap `verify quick` | Partial pass | 566-model MSL gate passed at 230 compiled with zero trace counterexamples; architecture gate correctly rejected three files over 2,000 lines; content splits are in progress |
| 2026-08-08 | Milestone 0 architecture/spec gates | Pass | 137 architecture tests and 16 suite/spec gates passed; content-based DAE splits satisfy the 2,000-line policy; SPEC 0007 and 0034 remain within word budgets |
| 2026-08-08 | Shared causal B.1b orientation | Pass | Structural 93, Solve 60, and GALEC 43 tests pass. Both lowerers consume one forced-elimination/DAG proof; ambiguous/current-cycle rows fail closed; `pre` is not a current dependency |
| 2026-08-08 | Direct Modelica-to-GALEC non-regression | Pass | 29/29 `suite_galec_fmu` tests pass. `--target galec` compiles Modelica directly to a checked Algorithm Code eFMU, its root contains exactly `AlgorithmCode`, manifests and byte-derived checksums validate, estimator tensors/calls remain compact, and the independent GALEC parser suites pass 25 unit plus 9 integration tests |
| 2026-08-08 | MLS/eFMI scheduling boundary review | Pass after counterexample-driven correction | A proposed universal atomic discrete schedule rejected three valid Solve cases: ordinary MLS event iteration, sampled left-limit self-read, and the later instantaneous-feedback diagnostic. The proposal was removed. MLS Appendix B event iteration and first-iteration-only clock solving remain Solve semantics; eFMI's sorted explicit `DoStep` order remains a GALEC refinement after target admissibility. Structural 93, Solve 60, and GALEC 43 tests pass again |
| 2026-08-08 | Typed Solve program foundation, increment 1 | Pass | 4/4 focused tests pass. Added explicit Binary32/Binary64 Real profiles and operation-level rounding identity, checked Integer domains, non-erased Boolean/Integer types, exact tensor shapes, generatively branded construction handles, typed slots/registers, compact whole-aggregate load/store, arithmetic/comparison/select operations, dominance-by-construction, read-only/profile/type rejection, exact-value serialization, and mandatory operation spans |
| 2026-08-08 | Typed Solve program foundation, increment 2 | Pass | 13/13 focused tests pass. Added explicit typed conversions; compact aggregate fill, construction, static element/slice projection, and bounded dynamic selection; mandatory non-dummy provenance; and constructor-replayed wire decoding that rejects forged domains, tensor counts, value kinds, register dominance, storage, types, and operations |
| 2026-08-08 | Typed Solve increment 2 boundary regression | Pass | `rumoca-ir-solve` 118/118 plus Structural 93/93, Solve 60/60, and GALEC 43/43 unit tests pass; three Solve scheduling-boundary counterexamples remain green; `rumoca-ir-solve --all-targets` passes clippy with warnings denied |
| 2026-08-11 | Typed Solve program foundation, increment 3 | Pass | 197/197 `rumoca-ir-solve` tests (22 new: 5 effect, 17 method) plus 3 doctests. Added under `typed_program/`: `effect.rs` (six predefined + up to 16 user error signals over one 32-bit status word rejecting reserved bits; checked `SolveValueRange` saturation) and the `method/` tree (branded method/scope/cell/signal-closure ids, `SolveMethodInterface`/`SolveMethodBinding`, derived `SolveCallAbiPlan`, `SolveMethodTable`, ordered `Assign`/`Branch`/`Loop`/`Invoke`/`Limit`/`Signal` actions in stored = evaluation order, builder-proven dominance/bounded domains/complete branches/acyclic calls, constructor-replayed wire decoding with forgery rejection). Additive only: no root, no cutover, no existing execution or codegen path touched. Independent adversarial design review against SPEC_0043 §9 and this plan is in flight |
| 2026-08-11 | GAL-027/038 differential-oracle wiring | Partial pass with real finding | `rumoca-eval-galec` wired as a third leg of `galec_equivalence` (dev-dependency of `crates/rumoca`; oracle interprets the same checked `AlgorithmCodePackage` the C track renders). Oracle-vs-C agrees on all five fixtures; the REFERENCE leg (`rumoca_sim::simulate_dae`) disagrees on three: same-tick clocked consumers read the pre-update value (one-tick lag; IIR `y = a*pre(y)+b*u` reads stale `u`), while GALEC/C matches the committed closed-form anchor. Attribution (committed HEAD vs uncommitted) and the MLS-correct semantics adjudication are under a dedicated investigation before any leg is changed |
| 2026-08-08 | Typed Solve increment 2 Tier 1 MSL canary | Pass (partial-scope gate) | The fixed 20-model canary parsed and flattened 20/20, reached DAE and compiled 11/20, balanced 11/11 compiled models, and simulated 8/11 attempted models successfully; 3 solver failures and 9 ToDAE failures remain. All 8 comparable traces were high-agreement across 129 channels with zero bad or severe channels. This is canary-only evidence, not a full-cohort capability claim |

### Change log

- **2026-08-08:** Roadmap approved. Milestone 0 started. Existing checked DAE
  conditional-correlation and exact row-major tensor-recovery work remains in
  scope because it preserves upstream call and aggregate identity. Further
  semantic expansion in the embedded-C Jinja template is frozen pending the
  `SolveAlgorithmBlock` cutover.
- **2026-08-08:** Milestone 0 completed. SPEC 0007/0029/0034/0036 and catalogs
  0040/0041/0042/0043 now define the distinct Solve roots, typed aggregate
  operations, lexical locals, checked ABI, arithmetic profile, complete
  correlations, independent differential oracle, and no-compatibility cutover.
  Milestone 1 started.
- **2026-08-08:** Milestone 1 inventory found divergent B.1b alias orientation:
  GALEC guessed an output/left-hand direction while Solve used forced
  elimination. Introduced one branded structural plan, migrated both consumers,
  and added target-level cycle rejection. The direct Modelica-to-GALEC path is
  explicitly preserved; only Production C/H will consume `SolveAlgorithmBlock`.
- **2026-08-08:** Re-ran the complete GALEC/eFMI integration suite after the
  shared-plan migration and compact-rendering repair: 29/29 tests pass. The
  direct `.alg` branch remains `DAE -> AlgorithmCodePackage -> GALEC`; its
  exact single-representation container, parser round trip, manifest graph,
  written-byte checksums, and tensor/call compactness are release gates.
- **2026-08-08:** Standards review closed Milestone 1. MLS equations are still
  declarative: ordinary discrete rows may require event iteration, sampled
  current-coordinate reads may denote a certified left limit, and clocked
  variables run only on the first event-loop iteration. eFMI Algorithm Code,
  by contrast, requires a sorted, explicit sampled-block algorithm. A trial
  universal owner schedule violated those distinctions and failed three
  focused Solve tests, so it was removed. Shared B.1b orientation remains in
  Structural; GALEC's sequential schedule remains a checked post-admissibility
  refinement. Milestone 2 started.
- **2026-08-08:** Milestone 2 increment 1 compiled and passed its four focused
  tests. New code lives under `rumoca-ir-solve/src/typed_program/`. It is an
  additive construction vocabulary only: no existing `SolveProblem` storage,
  evaluator, wire schema, GALEC renderer, or Production C path has been cut
  over yet. This is the deliberate stopping point for agent handoff.
- **2026-08-08:** Milestone 2 increment 2 compiled and passed thirteen focused
  tests. Aggregate operations now remain first-class and shape checked, numeric
  conversions are explicit, and current typed-program wire input replays the
  same checked builder operations instead of accepting invariant fields. The
  lifecycle/action/call layer remains the next additive slice; no existing
  execution or code-generation path has been migrated.
- **2026-08-08:** The required Tier 1 canary for increment 2 passed its
  partial-scope quality gate. It produced eight high-agreement comparisons
  across 129 channels without a trace counterexample. The nine ToDAE failures
  and three solver failures are retained as bounded canary diagnostics; they
  do not establish or change a full-cohort rate.

### Resume handoff

1. Start by reading the current worktree and the accepted Solve rows
   SOLVE-C32–C38 plus SPEC_0043 §9; do not assume the additive typed module is
   the completed root.
2. Re-run `cargo test -p rumoca-ir-solve typed_program` (currently 13/13) and the
   196 focused Structural/Solve/GALEC tests before changing semantics.
3. Preserve the corrected standards boundary: do not reintroduce a universal
   acyclic schedule over every B.1b/B.1c owner. The rejected attempt broke the
   Solve tests `root_refresh_excludes_relation_bearing_follow_current_owner`,
   `ordinary_periodic_row_rejects_proved_instantaneous_feedback`, and
   `sampled_owner_reads_its_own_tick_output_from_the_left_limit`.
4. Extend `TypedProgram` with bounded action/control-flow, call ABI,
   lexical-scope, and effect vocabulary only as required by checked Algorithm
   Code. Keep every operation typed and profile-bound before committing it;
   compact aggregate constructor/fill/projection is complete.
5. Then introduce `SolveAlgorithmBlock::construct` with exactly Startup,
   Recalibrate, and DoStep. Do not route Modelica-to-GALEC `.alg` rendering
   through Solve; that direct checked `AlgorithmCodePackage` path is pinned by
   the 29/29 `suite_galec_fmu` gate.
6. Existing tracked changes in `flake.nix` and the diagnostic expansion in
   `rumoca-phase-solve/src/lower/scalar/functions.rs` were concurrent/external
   changes; preserve them and do not attribute them to this milestone without
   reviewing their provenance.

## Decision

Rumoca should retain Solve IR as its fundamental executable compiler IR and
support two source paths into distinct, valid-by-construction Solve roots:

```text
DAE ────────────────────────────────> SolveProblem
 │                                      │
 │                                      └─> simulation/JIT/codegen backends
 │
 └─> checked AlgorithmCodePackage
          │
          ├─> MiniJinja ─────────────> GALEC .alg + Algorithm Code manifest
          │
          └─> generalized typed Solve IR
                  (SolveAlgorithmBlock)
                         │
                         └─> MiniJinja ─> Production C/H + manifest
```

The eFMI route is therefore:

```text
DAE -> GALEC Algorithm Code -> Solve IR -> Production Code
```

The direct `DAE -> SolveProblem` route remains the general Modelica simulation
path. GALEC is not inserted into that path because GALEC intentionally admits
only a fixed-sample, causally executable controller subset.

No separate Controller IR, Production IR, or Compute IR is proposed initially.
Instead, Solve IR becomes an executable IR family with shared typed program
vocabulary and separate checked roots for numerical DAE execution and GALEC
block execution.

## Rationale

GALEC admissibility is a restriction over canonical DAE semantics, but checked
Algorithm Code is also an explicit semantic refinement:

- DAE owns declarative equations, conditions, clocks, and variable identity.
- GALEC owns the causally ordered `Startup`, `Recalibrate`, and `DoStep`
  algorithms required by eFMI.
- Solve IR owns target-neutral executable programs.
- Production Code is a target-profiled implementation of the checked Algorithm
  Code representation.

Lowering checked GALEC into Solve IR preserves the eFMI abstraction chain while
allowing Production Code to reuse Rumoca's executable infrastructure:

- register construction and validation;
- scalar and tensor program representation;
- evaluators and scalar fallback;
- optimization and legalization passes;
- C, Rust, WASM, JIT, MLIR, and future execution backends;
- provenance, diagnostics, and backend capability checks.

Generating Production C independently from DAE would create sibling GALEC and C
translations with no construction-level guarantee that the C implements the
co-emitted Algorithm Code. Generating C directly from the high-level GALEC view
leaves scalarization, operation selection, and control-flow lowering in
MiniJinja. The selected design instead makes checked GALEC the authoritative
eFMI controller input and Solve IR the authoritative executable lowering.

## Current Gaps

The current `SolveProblem` and `LinearOp` representation cannot serve as the
GALEC execution target unchanged:

1. Registers and constants are represented as `f64`.
2. comparisons and Boolean operations use `0.0`/`1.0` values.
3. loads are coupled to solver `Y`/`P` storage.
4. `StoreOutput` models row output, not typed mutable controller storage.
5. Solve discrete execution is snapshot/fixed-point oriented, whereas GALEC
   requires explicit statement order.
6. Solve IR has no representation for GALEC method lifecycle, bounded control
   flow, `limit`, signals, signal closures, or method escape sets.
7. the existing GALEC C templates perform expression lowering, array expansion,
   builtin mapping, reference discovery, and statement lowering that should be
   complete before rendering.
8. Solve and GALEC independently orient and schedule parts of the clocked
   discrete DAE.

These are treated as Solve IR and lowering-ownership deficiencies, not as
reasons to introduce a parallel controller execution IR.

## Target Solve IR Structure

### Shared executable vocabulary

Solve IR should gain a typed, storage-neutral program vocabulary shared by all
Solve roots.

Illustrative types follow; final names and schemas require specification review.

```rust
enum SolveValueType {
    Real64,
    Integer(IntegerDomain),
    Boolean,
}

struct SolveRegister {
    index: u32,
    value_type: SolveValueType,
}

enum SolveOp {
    Constant { dst: SolveRegister, value: SolveValue },
    Load { dst: SolveRegister, slot: SolveSlotId },
    Store { slot: SolveSlotId, src: SolveRegister },
    Unary { dst: SolveRegister, op: TypedUnaryOp, arg: SolveRegister },
    Binary {
        dst: SolveRegister,
        op: TypedBinaryOp,
        lhs: SolveRegister,
        rhs: SolveRegister,
    },
    Compare {
        dst: SolveRegister,
        op: CompareOp,
        lhs: SolveRegister,
        rhs: SolveRegister,
    },
    Select {
        dst: SolveRegister,
        condition: SolveRegister,
        if_true: SolveRegister,
        if_false: SolveRegister,
    },
    // Existing checked tensor and specialized operations remain available
    // under explicit type, shape, effect, and capability contracts.
}
```

Construction must prove register dominance, exact operand/result types, static
shape compatibility, valid storage access, mandatory provenance, and explicit
conversion semantics. Boolean values must remain Boolean; Integer values must
not travel through `f64`.

### Abstract typed storage

Shared programs should address checked slot identities rather than hard-code
solver columns into every operation. Each root owns the mapping from slots to
its execution storage.

`SolveProblem` storage includes:

- time;
- solver coordinates;
- parameters and constants;
- discrete current/pre values;
- seeds and runtime-managed values.

`SolveAlgorithmBlock` storage includes:

- control inputs;
- outputs;
- tunable and dependent parameters;
- constants;
- persistent state and previous-state values;
- method locals;
- signal closures and error status where required.

Solver-specific operations may remain, but shared arithmetic must not depend on
solver `Y`/`P` addressing.

### Separate valid-by-construction roots

The root types should remain distinct rather than using a permissive runtime
enum:

```rust
pub struct SolveProblem {
    // General numerical DAE execution and solver orchestration.
}

pub struct SolveAlgorithmBlock {
    // Explicit GALEC lifecycle and controller execution.
}
```

`SolveProblem` continues to own continuous, initialization, discrete, event,
clock, layout, and optional solver-artifact contracts.

`SolveAlgorithmBlock` owns:

- a typed interface and storage layout;
- exactly one `Startup`, `Recalibrate`, and `DoStep` method;
- ordered method actions;
- typed expression programs;
- explicit branches and statically bounded loops;
- persistent-state and previous-state commits;
- explicit limiting effects;
- error signal set, catch, closure, and propagation effects;
- the selected target arithmetic profile;
- exact correlations back to checked Algorithm Code identities and provenance.

Arithmetic programs and structured controller actions should remain separate.
An action layer can execute expression programs, perform stores, branch, invoke
statically bounded loops, limit values, and manipulate signals without encoding
all effects as floating-point scalar operations.

### Strict GALEC execution profile

GALEC-profile Solve construction and optimization must preserve:

- source-defined expression evaluation order;
- statement order;
- no arithmetic reassociation without an explicit equivalence proof;
- exact Real/Integer/Boolean distinctions;
- target Integer-domain behavior;
- GALEC conversion, limiting, NaN, and signal semantics;
- bounded execution and acyclic calls.

An optimization API that cannot prove these obligations must not be able to
return a checked `SolveAlgorithmBlock`.

## Compiler Phase Ownership

### `rumoca-phase-structural`

Own the shared checked B.1b orientation that Solve and GALEC previously derived
independently. The branded `CausalDiscretePlan` proves explicit target/value
pairs, forced alias elimination, type/shape compatibility, and acyclic current
dependencies for the rows it can causalize. It distinguishes current reads
from `pre`; it does not turn all discrete semantics into one universal
sequential schedule.

That limit is normative. Ordinary unclocked B.1b/B.1c rows may participate in
MLS Appendix B event iteration, and a sampled coordinate can have a certified
left-limit boundary even when its expression syntax names the current
coordinate. Solve retains those MLS execution semantics. GALEC constructs its
sorted explicit `DoStep` schedule only after eFMI target admissibility proves
the selected partition can be represented as one sampled block. A future
shared clock-plan artifact must model those temporal capabilities explicitly;
it may not infer them from a generic current-read graph.

### `rumoca-phase-galec`

Continue to own:

- GALEC admissibility over untouched DAE;
- consumption of the shared causal-discrete plan;
- variable classification;
- GALEC naming and builtin mapping;
- construction of `Startup`, `Recalibrate`, and `DoStep`;
- `pre`/previous-state representation;
- checked `AlgorithmCodePackage` construction;
- DAE-to-Algorithm-Code provenance and identity correlations.

It does not allocate Solve registers or emit text.

### `rumoca-phase-solve`

Own every semantic lowering into Solve IR, with separate public entry points:

```rust
lower_solve_problem(&Dae) -> Result<SolveProblem, LowerError>

lower_algorithm_block(
    &AlgorithmCodePackage,
    &SolveTargetProfile,
) -> Result<SolveAlgorithmBlock, LowerError>
```

The GALEC entry point performs exhaustive typed lowering. It must fail before
code generation if any checked Algorithm Code operation lacks an executable
representation for the selected target profile.

### `rumoca-phase-codegen`

Expose separate target-neutral, read-only template views:

- `AlgorithmCodePackage` view for `.alg` and Algorithm Code metadata;
- `SolveAlgorithmBlock` view plus checked correlations for Production C/H and
  Production Code metadata;
- existing `SolveProblem` view for numerical targets.

MiniJinja owns target syntax, filenames, packaging, manifests, checksums, and
declared ABI mappings. It must not infer types or shapes, resolve references,
scalarize arrays, schedule statements, select semantic operations, or repair an
invalid program.

## Correct-by-Construction Boundaries

### DAE to checked Algorithm Code

Construction requires a non-forgeable admission result proving:

- exactly the supported fixed-period clock model;
- no continuous or unresolved residual system;
- every exported assignment has a unique causal target;
- the current-tick dependency graph is acyclic;
- initialization is representable by GALEC `Startup`;
- every variable and expression has a GALEC type and static shape;
- unsupported events, functions, domains, and dynamic behavior remain visible
  and cause typed rejection.

### Algorithm Code to Solve Algorithm Block

Construction proves:

- every Algorithm Code declaration has exactly one typed storage owner;
- all required interface values and writable state are initialized;
- every expression and statement has exactly one executable lowering;
- every read is dominated by initialization or a prior definition;
- every method and local call is bounded and acyclic;
- evaluation order is preserved exactly;
- all Integer operations are safe in the selected target domain;
- every `limit` and signal effect is explicit;
- method escape sets agree with the checked Algorithm Code package;
- Algorithm Code variables and methods map injectively to Solve identities;
- no target-language spelling or template fragment is stored in Solve IR.

### Solve Algorithm Block to Production artifacts

Rendering receives no unchecked semantic choices. The generic artifact graph
still computes checksums from written bytes, validates XML against the vendored
schemas, and assembles the eFMU. The Production Code manifest maps generated
entities to the exact Algorithm Code package implemented by the
`SolveAlgorithmBlock`.

## Evaluation and Proof Strategy

`rumoca-eval-galec` remains an independent interpreter over checked GALEC. It
must not delegate to Solve lowering or `rumoca-eval-solve`, otherwise the two
sides could share the same defect.

`rumoca-eval-solve` should gain typed shared-program evaluation and a
`SolveAlgorithmBlock` lifecycle executor. It remains the production evaluator
and scalar fallback for Solve IR.

The differential validation chain is:

```text
checked GALEC semantics
        ==
rumoca-eval-solve(SolveAlgorithmBlock)
        ==
generated Production C
```

Required observations include:

- all interface and persistent variables after every lifecycle call;
- active and escaped error signals;
- limiting behavior;
- Integer overflow and conversion failures;
- NaN comparisons and min/max behavior;
- array element order;
- state/previous-state commit timing;
- method failure atomicity where required.

## Implementation Sequence

### Milestone 0 — specification amendments

Before implementation, amend the governing specs:

1. `SPEC_0007` — redefine Solve IR as a typed executable IR family, add the
   `SolveAlgorithmBlock` root, and retain `SolveProblem` as the DAE/solver root.
2. `SPEC_0040` — add checked contracts for typed shared programs and the GALEC
   Solve root; revise existing rows that require `f64`/solver-specific behavior.
3. `SPEC_0034` — change Production Code placement to
   `AlgorithmCodePackage -> SolveAlgorithmBlock -> typed view -> MiniJinja`.
4. `SPEC_0029` and `SPEC_0041` — assign shared causal analysis, GALEC-to-Solve
   lowering, evaluation, and template-view ownership.
5. `SPEC_0036` and `SPEC_0043` — add construction rows if the valid-by-
   construction proposal is advanced with this work.

The representation change is a current-version cutover: do not retain old
Solve readers, adapters, aliases, feature flags, or template fallbacks.

### Milestone 1 — shared causal-discrete plan

- identify the exact overlap between Solve discrete orientation and GALEC
  clocked-assignment lowering;
- introduce one checked structural result with typed DAE identities;
- migrate direct Solve lowering to consume it;
- migrate GALEC admissibility/lowering to consume it;
- prove unchanged direct Solve behavior and unchanged GALEC golden output;
- add negative tests for ambiguous targets, cycles, clock mismatches, and
  current-versus-`pre` dependencies.

### Milestone 2 — typed Solve program foundation

- introduce typed values and registers;
- make conversions explicit;
- separate abstract slot identity from solver storage layout;
- preserve existing tensor/domain representations under typed element
  contracts;
- migrate `SolveProblem` lowering, evaluation, serialization, and codegen;
- remove Boolean-as-Real and Integer-as-Real assumptions from canonical Solve
  IR;
- complete the schema cutover and update golden Solve fixtures.

### Milestone 3 — Solve Algorithm Block root

- define checked interface, storage, method, action, signal, and correlation
  data;
- define target arithmetic profiles;
- add exhaustive `AlgorithmCodePackage -> SolveAlgorithmBlock` lowering;
- add lifecycle execution to `rumoca-eval-solve`;
- establish differential tests against `rumoca-eval-galec` before codegen is
  migrated.

### Milestone 4 — Production C migration

- build a typed template view over `SolveAlgorithmBlock`;
- migrate Production C/H templates from the high-level Algorithm Code view;
- delete template-side semantic lowering, scalarization, reference discovery,
  and operation selection;
- retain Algorithm Code `.alg` rendering over `AlgorithmCodePackage`;
- retain target-owned manifests, assets, checksum graph, XSD validation, and
  package assembly;
- compile generated C with strict warnings and run differential execution.

### Milestone 5 — backend reuse and optimization

- enable appropriate existing Solve backends for `SolveAlgorithmBlock`;
- define profile-aware optimization legality;
- reuse scalar/tensor fallback without relaxing GALEC order or arithmetic
  contracts;
- add optional Rust/WASM/JIT controller targets only after the reference C path
  is proven.

## Verification Gates

Each milestone begins with the smallest behavior-proving suite and then runs the
applicable repository gates under the required fixed job budget.

The completed capability requires:

- DAE-to-GALEC positive and negative admissibility suites;
- shared causal-plan orientation and schedule tests;
- Solve typed-register and storage construction tests;
- Solve serialization current-version round trips and old-version rejection;
- GALEC parser/render round-trip stability;
- independent GALEC-versus-Solve lifecycle differential tests;
- Solve-versus-generated-C differential tests;
- Integer-domain boundary and overflow tests;
- array/tensor and static-loop tests;
- signal, limiting, NaN, conversion, and failure-atomicity tests;
- generated C compile checks with `cc -Wall -Werror`;
- full Algorithm Code and Production Code XSD/checksum/container validation;
- target smoke tests through the public CLI;
- the required Tier 1 MSL canary evidence for each compiler capability change.

No parity claim may be made from generated-C compilation or simulation
completion alone; observable traces and controller state must be compared.

## Non-Goals

- Routing every Modelica model through GALEC before simulation.
- Replacing canonical DAE with GALEC.
- Treating GALEC as a peer canonical Modelica stage.
- Encoding GALEC-specific lifecycle fields in DAE.
- Retaining `f64` encodings for Boolean and Integer in canonical typed Solve
  programs.
- Making MiniJinja an expression compiler, scheduler, scalarizer, or validator.
- Reusing Solve optimizations that cannot prove strict GALEC evaluation-order
  preservation.
- Adding compatibility readers or parallel legacy codegen paths during the
  Solve schema cutover.

## Completion Criteria

This plan is complete when:

1. direct DAE simulation lowers to the generalized typed `SolveProblem` with no
   semantic regression;
2. checked Algorithm Code lowers exhaustively to `SolveAlgorithmBlock`;
3. `rumoca-eval-galec`, `rumoca-eval-solve`, and generated Production C agree on
   the complete controller observation surface;
4. Production templates render already-lowered semantics only;
5. Algorithm Code and Production Code remain correctly correlated in a
   schema-valid, checksum-valid eFMU;
6. the superseded Solve representation and GALEC-template lowering path have
   been removed in the same reviewed cutover;
7. the applicable focused, canary, codegen, compile, differential, and eFMI
   validation gates are green and recorded.
