# Rumoca multi-agent architecture, correctness, and efficiency review

Date: 2026-08-11  
Branch: `msl-trace-parity-50`  
Reviewed HEAD: `7a24727c` (`Refresh RDD2 agent handoff`)  
Snapshot: dirty worktree, including untracked Solve typed-method/effect work

## Scope and method

This was a coordinated read-only review with three parallel specialist tracks:

1. compilation correctness and runtime efficiency;
2. spec compliance, valid-by-construction IR, and formal-verification readiness;
3. DRY, crate ownership, code generation, and maintainability.

The coordinating review reconciled the three tracks against the active specs,
the current source, the RDD2 roadmap/handoff, and the recorded benchmark/trace
artifacts. No source was edited. No formatter, build, test, simulation,
benchmark, or profiler was run. Therefore:

- source-level mechanisms marked **confirmed** are directly visible in the
  reviewed snapshot;
- causal links marked **plausible** still require a correcting patch and trace
  experiment;
- earlier test and performance results are cited from the roadmap/handoff and
  do not validate the current dirty tree;
- this Markdown document is the only write made by this review.

## Executive assessment

Rumoca is not ready to qualify the RDD2 missions or generated controller code.
The compiler has promising tensor-native pieces, a sound top-level phase order,
and a short-slice runtime that is already materially faster than the available
OMC comparison. Those strengths are currently masked by one mission-blocking
correctness failure and by repeated loss, reconstruction, and scalarization of
checked structure after lowering.

The central conclusion from all review tracks is the same:

> Solve IR should be the one proof-bearing semantic root for numerical Solve,
> AD, refresh/invalidation, sparsity, GALEC, production C, and native backends.
> Construction should issue compact tensor/domain owners and complete
> dependency/execution certificates once. Consumers may materialize a final
> target view, but must not rediscover, expand, compare, or recollapse the
> mathematical graph.

| Dimension | Assessment | Blocking reason |
|---|---|---|
| Compilation accuracy | Red | Full missions complete numerically but do not fly: rotational dynamics are dead and no full OMC trace exists. |
| Spec compliance | Red | Accepted SOLVE and crate-boundary claims exceed the implementation; several accepted contracts are directly violated. |
| Tensor-native lowering | Red | Multiple paths enumerate tensor domains or scalarize compact owners before a final backend boundary, sometimes recovering affine structure afterward. |
| Valid by construction | Red | Phase wrappers and Flat/Solve roots remain forgeable; serialized derivative artifacts can be unrelated to the problem they drive. |
| Runtime efficiency | Amber/red | 0.5 s is about 3.39x realtime, but still about 3x slower than the 10x gate; global event refresh dominates recorded time. |
| DRY / semantic ownership | Red | Function lowering, dependency interpretation, backend planning, target rendering, and policy checks have multiple authorities. |
| Maintainability | Amber/red | Large exception-bearing modules correspond to mixed ownership; split only after semantic duplication is removed. |
| GALEC / production C | Red | It does not consume the numerical Solve root and therefore does not inherit current correctness or performance work. |
| Formal-verification readiness | Red | Important invariants are checked after assembly, identities can be forged, and derived artifacts lack exact correlation proofs. |

## Release-blocking findings

### P0-1: the current full RDD2 missions are semantically wrong

The recorded schema-59 full-mission runs both reach 45 s with 9,001 samples and
no NaN/Inf, but that completion is not mission success:

- `plant.dynamics.M_b` reaches roughly 2.4 N·m;
- `bodyAngularVelocityRate` and `omega` remain exactly zero;
- the truth quaternion remains `(1, 0, 0, 0)`;
- truth East and North remain exactly zero;
- the box route is not flown; the vehicle moves vertically only.

Consequently, current GPS-versus-optical parity is vacuous: their truth states
are bit-identical because the vehicle never executes the horizontal/attitude
mission. There is no full 45 s OMC truth trace; the OMC artifacts cover only
0–0.5 s.

Evidence: `dev/2026-08-10-rdd2-tensor-native-performance-roadmap.md` under
“Full-mission Phase 3 scouting”.

Impact: this blocks estimator qualification, backend trace comparison,
production C/eFMU qualification, and meaningful performance acceptance. A
fast wrong mission is not a valid benchmark.

Required proof of closure:

1. isolate the first incorrect rotational derivative at the Solve owner and
   source equation;
2. compare the corrected Rumoca trace with OMC over the full mission, including
   attitude, angular rate, position, estimator innovations, and mission state;
3. run both GPS and optical routes and prove that their expected measurements
   differ while truth follows the intended route;
4. only then resume the 10x performance gate and firmware/eFMU qualification.

### P0-2: affine Map/Stencil dependencies omit shifted `Y` coordinates

This is a confirmed dependency-analysis defect and the strongest currently
known first-layer hypothesis for the dead rotational dynamics.

- `build_continuous_refresh_owners` derives derivative and event closures in
  `crates/rumoca-eval-solve/src/refresh_plan.rs:531-575`.
- `compute_block_dependencies` handles `ComputeNode::Map` and
  `ComputeNode::AffineStencil` by inspecting only `base_ops` at
  `refresh_plan.rs:630-664`.
- It ignores the compact `domain` and `load_strides`. An affine `LoadY`
  therefore records only its first-point coordinate; shifted vector/matrix
  coordinates can be falsely absent from the refresh closure.

The mechanism is proven by inspection. Its responsibility for RDD2 is
**plausible, not yet proven**: the suspected rotational path contains a vector
`solveSPD` derivative relation for which a first-coordinate-only dependency is
consistent with the observed dead vector derivative.

Long-term fix: add one Solve-owned, output-projection-aware dependency and
invalidation certificate whose relations can express affine images/ranges.
Refresh, sparsity, grouping, wire replay, interpreter, Cranelift, WASM, GALEC,
and production C must consume that certificate rather than interpret operations
again.

### P0-3: `SolveModel` wire can attach unrelated derivative/JVP artifacts

This is a confirmed executable-wire correctness defect.

- `SolveArtifacts` and its derivative/JVP programs are fieldwise
  deserializable at `crates/rumoca-ir-solve/src/model.rs:59-227`.
- `SolveModelWire` accepts those caller-supplied artifacts at
  `model.rs:1763-1819`.
- validation at `crates/rumoca-ir-solve/src/lib.rs:808-817` checks the problem
  and pure-call-site interfaces, but does not prove that the artifacts were
  derived from that problem;
- runtime executes the supplied programs at
  `crates/rumoca-solver/src/runtime/solve_runtime.rs:379-385` and `540-565`.

A locally well-formed but semantically unrelated JVP can therefore deserialize
successfully and drive Newton or sensitivity calculations. Structural
artifacts are also `#[serde(skip)]` at `model.rs:200-225`; deserialization does
not perform the reconstruction claimed by the comments, so round trips silently
lose sparsity/coloring/cache information.

Long-term fix: serialize one canonical executable owner and reconstruct all
derived artifacts through the production constructor. If an artifact remains
on wire, its exact derivation certificate must be replayed against the root
before an executable `SolveModel` can exist.

### P0-4: problem-only Solve APIs discard the sole pure-call body table

This is a confirmed lossy API/codegen defect, with an important scope
qualification.

- `lower_solve_problem` maps `lower_solve_package` to `package.problem` and
  drops `package.pure_calls` at
  `crates/rumoca-phase-solve/src/lib.rs:29-38`.
- the internal lowerer constructs and correlates both objects at
  `crates/rumoca-phase-solve/src/lower.rs:63-92`;
- `LinearOp::PureCall` contains an owner reference and arguments, not the body,
  at `crates/rumoca-ir-solve/src/linear_op.rs:951-970`;
- problem-only production callers include
  `crates/rumoca/src/compiler.rs:131-146,250-257`,
  `crates/rumoca/src/target_manifest.rs:515-531,616-626`, and the LSP scenario
  path;
- `FmiComponent` retains only `SolveProblem` at
  `crates/rumoca-ir-fmi/src/lib.rs:174`;
- Solve templates receive only problem/artifacts. The C ODE template has no
  `PureCall` branch and fails closed at
  `crates/rumoca-phase-codegen/src/templates/c-ode/model_ode.c.jinja:51-87`.

The in-process simulator is not affected by this loss: it uses
`lower_solve_package` and installs `package.pure_calls` in
`crates/rumoca-sim/src/solve_lowering/entry.rs:138-159`. Generated C/FMI/JSON,
however, do not have one complete executable root. Current C fails rather than
silently emitting a bad call, which is correct fail-closed behavior but still
prevents the intended target.

Long-term fix: make `{ problem, pure_calls }` an opaque inseparable checked
kernel, or move the table into the canonical problem root. Simulation, JSON,
FMI, artifacts, and every backend must accept that same root.

### P0-5: pure-call caching is under-constrained by the checked builder

`TypedProgramBuilder::call` permits repeated references to one owner with
different same-typed argument registers
(`crates/rumoca-ir-solve/src/typed_program/program.rs:1538`). Both the
interpreter (`crates/rumoca-eval-solve/src/typed_program/mod.rs:353,1043`) and
Cranelift (`crates/rumoca-exec-cranelift/src/emit/typed_program.rs:495,1247`)
cache one result per owner and consult it before considering arguments.

Production lowering intends an owner to represent one exact occurrence, so the
current compiler may not emit this shape. The public checked builder does,
however, admit a program that can evaluate with the wrong arguments.

Long-term fix: bind the exact argument-source tuple and activation scope into
the issued invocation identity, or make construction reject every same-owner
reference whose arguments differ.

## Major spec, IR, and architecture findings

### P1-1: accepted SOLVE-C56 is not implemented as specified

`SPEC_0007` binds SOLVE-C01–C56 as accepted. SOLVE-C56 in `SPEC_0040` requires
one canonical continuous refresh owner with compact target ranges and a
complete coordinate dependency/invalidation certificate spanning time,
solver `Y`, parameters, event/pre/previous/history state, external tables,
impure state, and arithmetic/AD mode. It also requires every interpreter and
backend to consume the same directional owner.

The current implementation has improved to schema 59 compact row selections,
but still falls short:

- Phase Solve constructs a public `SolveProblem`, then asks eval-solve to
  discover refresh owners at `crates/rumoca-phase-solve/src/lower.rs:71-88`;
- eval-solve reconstructs rows/order, source positions, and multiple dependency
  closures at `crates/rumoca-eval-solve/src/refresh_plan.rs:300-575` and
  `refresh_plan/source_catalog.rs:21-117`;
- dependency meaning is interpreted independently in
  `prepared/dependency.rs`, `sparsity.rs`, and
  `crates/rumoca-ir-solve/src/refresh.rs:1017-1050`;
- exact assignments clone/filter source operations at `refresh.rs:893-950`;
- runtime scalarizes and builds more source maps at
  `crates/rumoca-solver/src/runtime/solve_runtime.rs:341-370,433-485`.

The schema-59 `RefreshRowSelection` is worth preserving: it stores checked
positions into one row catalog rather than cloning full row metadata. It does
not yet constitute the complete certificate or compact target-range owner.

Long-term fix: Phase Solve construction issues one certificate directly from
the canonical tensor/typed semantic owner. Runtime preparation may attach
storage or machine code but may not infer mathematical ownership.

### P1-2: tensor families are expanded and affine structure is recovered

This is a confirmed violation of the accepted range-preserving rules in later
lowering:

- structured event lowering materializes `domain.index_tuples()`, compiles one
  scalar program per point, stores `Vec<Vec<LinearOp>>`, compares the programs,
  and recovers strides at
  `crates/rumoca-phase-solve/src/lower/events/structured.rs:171,279-352`;
- continuous derivative families reuse that path after scanning each point at
  `crates/rumoca-phase-solve/src/lower/continuous_tensor.rs:20-64`;
- initialization families scalarize during Phase Solve at
  `crates/rumoca-phase-solve/src/lower.rs:1900`;
- sparsity immediately converts `ComputeBlock` to scalar programs at
  `crates/rumoca-eval-solve/src/sparsity.rs:52` and then uses per-register
  `BTreeSet` propagation;
- `StructuralPattern` has no compact `Affine` variant at
  `crates/rumoca-ir-solve/src/structural_pattern.rs:49`.

This is exactly the expensive “expand, compare, recollapse” pattern the design
must eliminate. Cranelift cannot compensate: the runtime backend interface
accepts `ScalarProgramBlock`, not `ComputeBlock`, at
`crates/rumoca-solver/src/runtime/solve_runtime.rs:128`; maps have already been
expanded before native compilation.

Long-term fix: symbolically lower domain, binders, target map, and affine
accesses into checked `Map`/`AffineStencil` owners. A non-affine body remains a
typed compact region or fails closed. Scalar/CSR materialization is permitted
only in the final consumer adapter.

### P1-3: accepted SPEC_0032 conflicts with “never expand before final render”

This is a **spec/design conflict, not current implementation noncompliance**.

- `SPEC_0032:39-61` makes per-element Instance entries authoritative,
  prohibits a compact family descriptor there, and requires later phases to
  rederive the family domain;
- instantiation follows that rule by enumerating every point at
  `crates/rumoca-phase-instantiate/src/array_expansion/family_replication.rs:234-305`;
- flatten later recovers dimensions from element metadata at
  `crates/rumoca-phase-flatten/src/array_comprehension.rs:13`.

The requested architecture cannot be implemented compliantly until SPEC_0032
is amended. The amendment should define one authoritative compact Instance
family with typed domain, reindexing, `each`, modification/redeclare semantics,
and a lazy scalar occurrence view for the language cases that require
per-element identity. Flat and DAE should retain the same owner.

### P1-4: Flat/Solve and phase-proof types are forgeable

Accepted specs claim phase ordering and codegen-boundary validity are enforced
by types, but the APIs do not yet support that claim:

- `ParsedTree` and `TypedTree` have public tuple fields/constructors,
  deserialization, and `DerefMut` at
  `crates/rumoca-ir-ast/src/lib.rs:631-703`;
- `InstancedTree` exposes its tree/overlay, constructor, mutable overlay, and
  `DerefMut` at `crates/rumoca-ir-ast/src/instance.rs:1042-1087`;
- post-instance typechecking mutates an overlay and returns `()` rather than a
  proof type at
  `crates/rumoca-phase-typecheck/src/typechecker/api.rs:103-115`;
- flatten publicly accepts both the forgeable wrapper and raw tree/overlay at
  `crates/rumoca-phase-flatten/src/lib.rs:296-343`;
- Flat `Model` has public fields, `Default`, fieldwise deserialization, mutable
  `add_*`, and final validation at `crates/rumoca-ir-flat/src/lib.rs:87-130,
  256-295,447-450`;
- `SolveProblem`, `SolveModel`, `ComputeNode`, and `ComputeBlock.nodes` are
  publicly constructible/mutable, with post-hoc validation or unchecked child
  deserialization.

This violates accepted `SPEC_0029` phase-typing claims and `SPEC_0007`'s
codegen-boundary rule. `SPEC_0036` accurately describes the remaining root
conversion as DRAFT; that draft is the right remediation roadmap, but its
unfinished status does not make the stronger accepted claims true.

Long-term fix: mint an opaque `TypedInstancedTree`; consume each phase artifact
by value; make root fields private; remove `Default` and public whole-root
validators; replay private wire drafts through the same generative
constructors. `ResolvedTree` and the DAE root are useful patterns.

### P1-5: sparsity provenance can claim more than construction proves

Anyone can construct `PatternProvenance::derived(...)` and feed arbitrary row
dependencies to `StructuralPattern::from_row_dependencies`
(`crates/rumoca-ir-solve/src/structural_pattern.rs:29,406`). The constructor
canonicalizes storage; it cannot prove that the dependency claim is complete.
A false-negative pattern can therefore be branded derived, contrary to
accepted SOLVE-C17's soundness intent.

Keep conservative `Full` freely constructible. Exact/derived patterns should
be issued only by the private exhaustive dependency constructor over the
checked semantic root. The current uncommitted wire replay hardening is
directionally correct and should be preserved, but it does not establish
semantic derivation.

### P1-6: function semantics have three lowering authorities

The intended owner is the typed pure-function lowering in
`crates/rumoca-phase-solve/src/lower/typed_functions.rs`. Two independent
semantic paths remain:

- the 4,320-line legacy Solve scalar function/control lowerer in
  `lower/scalar/functions.rs`, including a continuous fallback when a typed
  directional site is unavailable (`functions.rs:3831-3875,4044-4108`);
- GALEC's independent DAE function traversal/lowering in
  `crates/rumoca-phase-galec/src/lower/user_functions.rs` and
  `lower/expression_functions.rs`.

`rumoca-phase-galec` has no Solve dependency. This violates accepted SOLVE-C52
and the SPEC_0041 ownership row, creates divergent support/packing/lazy-branch
semantics, and requires separate proofs for simulation and firmware.

Long-term fix: one exhaustive typed, tensor-native function graph in Solve IR.
Numerical execution and GALEC consume checked projections of it. GALEC may
perform target-admissibility rejection but must not lower the DAE function
graph independently. Delete both legacy semantic paths as part of the cutover,
not behind permanent compatibility adapters.

### P1-7: GALEC/production C is not yet a Solve projection

Both `embedded-c-galec` and `galec-production` declare
`ir = "algorithm-code"`. `target_manifest.rs` lowers DAE to
`AlgorithmCodePackage`, and phase-galec owns its own semantics. The planned
`SolveAlgorithmBlock` has no implemented Rust type/consumer in the reviewed
tree.

Therefore current numerical Solve/Cranelift fixes do **not** automatically
generalize to production C or eFMU firmware.

The correct long-term cut is:

```text
checked DAE + checked Algorithm Code
                 |
                 v
       phase-solve construction
                 |
                 v
  checked Solve kernel + SolveAlgorithmBlock
       |          |            |
       v          v            v
 interpreter   Cranelift   GALEC/Production C
```

The method/effect files currently untracked under
`crates/rumoca-ir-solve/src/typed_program/` are a promising vocabulary for this
cut. They are not yet a complete root. Their method/scope/cell/closure IDs and
some bindings/interfaces remain independently deserializable or publicly
mintable, so the table builder must become the sole identity issuer.

### P1-8: accepted specs describe future state as current enforcement

`SPEC_0000` defines ACCEPTED as implemented and enforced. However:

- `SPEC_0007` binds all SOLVE-C01–C56 even though C51/C52/C56 remain incomplete;
- accepted `SPEC_0007` says all codegen-crossing IR is valid by construction,
  while DRAFT `SPEC_0036` correctly records Flat/Solve roots as unfinished;
- `SolveAlgorithmBlock` is named as pending in accepted `SPEC_0007` and
  `SPEC_0029`, but has no implementation;
- DRAFT `SPEC_0036` still mentions DAE wire v13 while the implementation is at
  schema 31.

Pending prose inside an accepted normative row does not satisfy SPEC_0000.
Either implement/enforce the row before accepting it or move/split the future
portion into a DRAFT. This truthfulness matters because formal assurance is
only as strong as the mapping from normative claims to executable constructors.

### P1-9: target rendering still owns semantics and can silently change meaning

`rumoca-phase-codegen` contains target-language syntax and semantic lowering
rather than only typed views:

- recursive expression-to-text rendering at
  `crates/rumoca-phase-codegen/src/codegen/render_expr.rs:43`;
- Rust-side reduction/comprehension expansion at `render_expr.rs:468-499`;
- `Size` and `Interval` fallbacks that emit `0`/`0.0` at
  `render_expr.rs:637-645`;
- array assignment scalarization at `render_stmt.rs:211-220`;
- C/Python/Modelica loop syntax at `render_stmt.rs:258-319`;
- `reinit` rendered as a comment at `render_stmt.rs:712-727`;
- target dialect types in `expr_config.rs` and target syntax in
  `render_solve.rs`, including MLIR fragments emitted directly in Rust.

These are confirmed behaviors; their reachability depends on the selected
target/capability checks. Unsupported semantics must always fail closed, never
become a plausible zero or comment.

Long-term fix: Phase Solve exhaustively lowers semantics into typed operations;
phase-codegen exposes typed, read-only values; target-owned templates contain
all spelling/syntax. Eliminate `ExprConfig`, dialect switchboards, and
fragment-producing Rust helpers as the typed template vocabulary becomes
complete.

### P1-10: production numeric profiles are declared but not proved

The current dirty work adds a 32-bit integer target domain and a stronger
three-leg GALEC fixture test. Important gaps remain:

- `TargetIntegerDomain` checks only `min <= max` at
  `crates/rumoca-compile/src/codegen_target.rs:934-939`;
- C templates emit raw signed `+`, `-`, `*`, and `/`, so unproved overflow can
  invoke C undefined behavior;
- eval-galec uses `f64` while generated production C uses `float32`; the test
  uses a tolerance rather than exact target arithmetic;
- range-proof and no-UB requirements remain DRAFT in SPEC_0034;
- fixtures are not full RDD2/eFMU mission qualification, and the current dirty
  test changes were not run by this review.

Before firmware release, arithmetic profile, overflow/division behavior,
rounding, assertions, external tables, and signal ordering must be issued by
the checked root and reproduced exactly by the generated target.

## Efficiency findings

### Recorded performance status

The latest recorded schema-59 0.5 s run is:

- average `0.149262073 s`;
- best `0.147334356 s`;
- approximately 3.39x realtime;
- approximately 2.95x slower than the `0.05 s` acceptance gate for 10x
  realtime.

Full but semantically invalid missions recorded:

- optical: 13.64 s for 45 s, about 3.30x realtime;
- GPS: 16.25 s for 45 s, about 2.77x realtime.

The available OMC fixed-step GBODE/RK comparison is about 1.30177 s for the
same short window; DASSL is 6.573 s, and giant scalar C compilation took about
2,419 s. On the short slice, current Rumoca is roughly 8.8x faster than the
recorded OMC fixed-step runtime. This is encouraging but **not a correctness
qualification** because the current Rumoca mission trace is wrong and there is
no full OMC trace.

The rejected giant Cranelift SSA experiment took 14.6078 s versus roughly
0.1505 s. It confirms that expanding to one huge scalar SSA graph and hoping
Cranelift recovers structure is the wrong architecture.

### P0-E1: one global event closure runs at every event

`event_consumer_dependencies` unions runtime assignments, post-commit
assignments, roots, discrete RHS, action conditions, structured RHS, and
guarded assignments at
`crates/rumoca-eval-solve/src/refresh_plan.rs:578-627`. One global event plan is
built at `:555-562`, while every periodic clock gets a default/empty plan at
`:563`. Runtime executes the global closure at every event before active-clock
remainders at
`crates/rumoca-solver/src/runtime/solve_runtime/refresh_execution.rs:97-145`.

This is conservative for correctness but defeats purpose- and clock-specific
invalidation. It aligns directly with the recorded 0.5 s breakdown:

- 4,555 derivative evaluations: about 28 ms;
- 1,154 root evaluations: about 29 ms;
- 500 legitimate 1 kHz scheduled events: about 89 ms.

The Kalman `step` subtree is only about 7.8% inclusive; it is not the whole
order-of-magnitude gap. Issue compact dependency owners per purpose and
`ClockId`, plus checked remainder relations. Do not filter or compare graphs at
runtime.

### P1-E2: dependency and execution work is repeatedly recomputed

Confirmed mechanisms include:

- lazy evaluation clones `input.row[op_index]` before each operation at
  `crates/rumoca-eval-solve/src/lib.rs:1641-1652`; `eval_lazy_pure_op` accepts
  the owned `LinearOp` at `:1886-1891`;
- `row_reads_y_index` allocates, sorts, and merges ranges on each call at
  `crates/rumoca-eval-solve/src/prepared/dependency.rs:29-58`, including runtime
  refresh use;
- scalar DAE projection allocates a `vec![false; expression_count]` scratch at
  `crates/rumoca-eval-dae/src/projection.rs:87-109`;
- projection hot paths probe environment variables at
  `crates/rumoca-solver/src/runtime/projection.rs:856,874,903`;
- the Solve dependency query reverse-scans and clones defining operations at
  `crates/rumoca-ir-solve/src/refresh.rs:1034-1050`, with pairwise row grouping;
- Cranelift compiles/calls Jacobians one scalar row at a time at
  `crates/rumoca-exec-cranelift/src/emit.rs:356,973`;
- parsed artifact-cache hits deep-clone the AST at
  `crates/rumoca-compile/src/parsed_artifact_cache.rs:76-85,138-193`.

These are secondary to correctness and global event ownership. The durable fix
is construction-issued def/use, input-access, output-projection, and execution
certificates—not a collection of runtime caches around rediscovery.

### P1-E3: compact and scalar AD artifacts are separate authorities

Phase Solve constructs scalarized implicit/derivative blocks and scalar AD
artifacts, then separately constructs tensor-aware JVPs
(`crates/rumoca-phase-solve/src/artifacts.rs:9`). Solve IR permanently stores
both compact and scalar Jacobian variants
(`crates/rumoca-ir-solve/src/model.rs:200`), and runtime retains several
projections of the same mathematical relation.

This duplicates construction, memory, validation, and numerical proof work.
One tensor-native primal/directional owner should expose checked output views;
scalar row alignment is a final adapter, not a second mathematical artifact.

### P1-E4: secondary dependency engines disagree

`program_output_y_dependencies` asks for solver-`Y` dependencies at
`crates/rumoca-eval-solve/src/sparsity.rs:472-492`, but the `TensorLoad` arm at
`:939-959` gives every primal lane an empty dependency and only seeds the AD
lane. This is a confirmed latent API defect. The currently active refresh
construction uses a different dependency path, so it is **not proven to be the
active RDD2 cause**.

Pure-call dependency propagation also assigns every output the union of every
input (`sparsity.rs:1065`), which is sound but can over-densify patterns and
refresh closures. Both issues disappear when all consumers use one
output-sensitive compact certificate.

## DRY and maintainability findings

### Cranelift discards certificates and maintains another planner/interpreter

Cranelift APIs receive a checked `ScalarProgramBlock` but pass only
`rows.programs()`, losing the block certificate
(`crates/rumoca-exec-cranelift/src/lib.rs:305-339`). `plan_row` then re-derives
register capacity, source flow, widths, and output ranges while cloning each
operation (`emit.rs:6476-6598`). Compiled residuals retain the cloned row plans.

The crate also contains a separate 1,356-line operation interpreter at
`emit/interpreter.rs`, duplicates `modelica_sign`, and duplicates input-bound
logic. The Cranelift copy traverses `FunctionConditional` arms/fallback
(`emit/input_validation.rs:71-78`); eval-solve does not
(`eval-solve/src/lib.rs:4349-4355`) even though prepared construction consumes
that result. This is a confirmed semantic disagreement; a resulting runtime
failure was not demonstrated in this read-only review.

Issue register/output/input-access certificates once. Cranelift should consume
the checked block directly. Equivalence testing should call the canonical
eval-solve interpreter instead of maintaining another semantic engine.

### Target symbol policy is synchronized by copying

The current dirty templates copy the same C reserved-name/allocation preamble
into:

- `templates/embedded-c-galec/model.c.jinja`;
- `templates/embedded-c-galec/model.h.jinja`;
- `templates/galec-production/pc_manifest.xml.jinja`.

A test parses template text and requires all three copies to remain
byte-identical. That detects drift but is not one owner. Move the declarations
to one target-owned declarative resource/partial referenced by all artifacts.
The uncommitted generated-prefix rejection in `symbol_alloc.rs` is a positive,
fail-closed fix and should be retained while centralizing its policy input.

### Repository-policy gates have competing implementations

File-size and cross-crate re-export policies are implemented independently in
the canonical architecture tests and `xtask review-scan`. Their path skips,
exception-marker handling, alias recognition, and facade allowances differ.
This can both reject valid code and miss forbidden forms. Make one scanner or
machine-readable policy authoritative and have every command call it.

The environment-variable registry is another current compliance issue. A
static scan matching the hardening gate found 42 product `RUMOCA_*` literals
outside the registry test itself. `RUMOCA_DISABLE_NATIVE_EXECUTION` changes
backend behavior in `crates/rumoca-sim/src/rk45.rs:22`; projection debug probes
are in a hot loop. Move behavior to typed tool configuration and eliminate
runtime environment discovery.

### Other duplicated authorities

- contract status exists in `contracts.toml`, `contract_cases.toml`, and a
  393-line `IMPLEMENTED_CONTRACT_IDS` constant; derive the implemented set from
  the registry and compare only with the distinct case manifest;
- equivalent checked shape-product helpers appear in eval scalarization,
  prepared support, codegen template partitioning, sparsity, and linear solve;
  make checked shape/domain construction the owner;
- generic Rust hardcodes target asset registration in codegen; target bundles
  should declare their own dependencies/includes.

### Large modules are debt receipts, not the primary root cause

Current examples include:

- `rumoca-exec-cranelift/src/emit.rs`: 7,873 lines;
- `rumoca-eval-solve/src/lib.rs`: 4,535;
- `phase-solve/src/lower/scalar/functions.rs`: 4,320;
- `phase-solve/src/ad.rs`: 3,459;
- `ir-solve/src/linear_op.rs`: 2,941;
- `ir-solve/src/lib.rs`: 2,636;
- `phase-solve/src/lower.rs`: 2,444;
- `eval-solve/src/prepared.rs`: 2,265.

They have SPEC_0021 exception/split-plan markers, so they are not unmarked
file-size violations. Do not split them mechanically. First remove duplicate
semantic owners, then split at the resulting responsibility boundaries:
typed versus removed legacy lowering, certificate construction versus
evaluation, and native emission versus the external equivalence oracle.

## Positive findings to preserve

- The top-level compiler order is correct: instantiate → typecheck → flatten →
  DAE. ToDAE consumes the flattened artifact; no phase-order inversion was
  found in `crates/rumoca-compile/src/session/compile_support.rs`.
- The DAE root already demonstrates the desired private immutable root,
  generative construction, and wire replay through the same constructor.
- Schema-59 compact `RefreshRowSelection` removes cloned row metadata and gives
  zero-allocation borrowed views. Keep it as part of the larger C56 solution.
- The current structural-pattern diff removes child fieldwise deserialization
  and replays private wire claims through checked constructors. Keep that
  direction while tightening provenance issuance.
- The Kani changes make assumptions explicit in the manifest/runner. Proof
  coverage is still small, but silent harness assumptions are being removed.
- The new typed method/effect vocabulary is a strong foundation for a shared
  `SolveAlgorithmBlock`, once identity issuance and root integration are made
  construction-only.
- Typed pure-call lowering is the correct intended single function owner; the
  legacy and GALEC lowerers should converge into it.
- WASM now fails closed for unsupported tensor/fold/call operations. It cannot
  yet run RDD2, but it does not pretend to support the missing vocabulary.
- The expanded GALEC equivalence test now compares generated C, eval-galec, and
  a Rumoca reference for fixtures. Preserve it and extend it to exact target
  arithmetic and full mission traces after the shared root exists.
- The uncommitted symbol allocator prefix hardening fixes a real collision/
  nontermination class; centralize the policy without losing the fail-closed
  behavior.

## Recommended remediation roadmap

This order preserves correct-by-construction and tensor-native design instead
of optimizing around incorrect ownership.

### R0 — restore mission correctness

1. Add minimal counterexamples for affine `Map`/`AffineStencil` dependency
   propagation and vector `TensorLoad` solver-`Y` propagation.
2. Specify and construct the compact output-projection dependency certificate
   in Solve IR; do not patch eval/runtime with another interpreter.
3. Make derivative, root, and event refresh consume that certificate.
4. Locate the first corrected rotational value and prove the full 45 s optical
   and GPS traces against OMC.

Review gate: an independent code/spec review must confirm no false negatives,
no point enumeration, and exact source-owner correlation before merging.

### R1 — make the executable Solve root valid by construction

1. Introduce one opaque checked Solve kernel containing problem, pure-call
   table, arithmetic profile, source correlations, and issued certificates.
2. Reconstruct derivative/JVP/structural artifacts from that root; reject
   forged or unrelated wire artifacts.
3. Make call identity include exact occurrence, arguments, activation/clock,
   and domain correlation; correct the cache contract.
4. Privatize Flat/Solve/Compute child construction and add
   `TypedInstancedTree`.
5. Make JSON, simulation, FMI, codegen, and LSP consume the same root.

Review gate: wire-forgery/property tests, constructor replay review, and a Kani
assumption/coverage review.

### R2 — complete tensor-native ownership

1. Propose and accept the SPEC_0032 amendment for authoritative compact
   Instance families before changing instantiation.
2. Remove Phase Solve domain enumeration and graph recollapse.
3. Add compact affine sparsity/dependency representations.
4. Make AD one typed primal/directional owner with final scalar views only.
5. Change the native backend boundary to accept checked `ComputeBlock`/typed
   regions and emit bounded loops at final lowering.

Review gate: scaling tests must demonstrate IR size proportional to rank/domain
description, not element count, until the final target adapter.

### R3 — unify functions, algorithms, and production code

1. Finish the exhaustive typed function cutover and delete legacy scalar
   function lowering.
2. Integrate the typed method/effect vocabulary into an opaque
   `SolveAlgorithmBlock` root.
3. Make phase-galec an admissibility/projection backend of Solve; remove its
   independent DAE function lowering.
4. Move all target syntax into target-owned templates and make every unsupported
   operation fail closed.
5. Centralize C symbol policy and exact target arithmetic/range proofs.

Review gate: one semantic owner must drive interpreter, Cranelift, GALEC, and
production C trace parity; no backend may infer body equality or reconstruct a
graph.

### R4 — reach the efficiency target on the correct trace

1. Issue purpose- and `ClockId`-specific refresh closures and remainders.
2. Remove per-op cloning, per-call range allocation/sorting, projection scratch
   allocation, and hot environment probes.
3. Batch checked JVP/native regions rather than compiling one scalar row per
   function.
4. Re-profile with `perf` only after each correctness-preserving owner change.
5. Treat the 0.05 s / 0.5 s gate as mandatory; do not skip events, loosen the
   solver, or change mission semantics to meet it.

Review gate: independent performance/code review using a rebuilt binary, exact
trace comparison, dynamic call counts, zero lost `perf` samples, and a written
explanation for every accepted/rejected optimization.

### R5 — firmware and eFMU qualification

1. Generate GALEC/production C and eFMU from the same checked Solve root.
2. Prove exact float32/int32 profile behavior and no C undefined behavior.
3. Build `cerebri_rdd2`, run both missions, and compare controller, generated-C,
   eFMU, and Rumoca traces.
4. Measure controller-cycle budget on the slower target processor separately
   from host physics performance.

Review gate: mission equivalence, target-resource budget, reproducible package,
and final spec/status truthing review.

## Acceptance criteria

The roadmap should not be called complete until all of the following hold:

- optical and GPS missions fly their intended routes with nontrivial attitude
  and horizontal motion;
- full Rumoca traces match a trustworthy OMC reference within declared numeric
  tolerances;
- the 0.5 s host benchmark is at most 0.05 s on the recorded comparison setup;
- Kalman/controller code meets the real target processor cycle budget;
- no authoritative array/tensor family is enumerated before a final consumer
  boundary, and no graph is expanded then recollapsed;
- Solve construction issues one complete dependency/invalidation and execution
  certificate consumed by every backend;
- Flat, Solve, function, method, call, sparsity, and wire roots are unforgeable
  outside their constructors;
- GALEC/production C and eFMU are projections of the same checked Solve root;
- generated target arithmetic is defined and trace-equivalent;
- active specs describe implemented reality, and each major roadmap stage has
  passed an independent code/spec/performance review.

## Final disposition

The work should continue, but not by adding local runtime optimizations or
another backend-specific lowering. The immediate priority is the affine
dependency/rotational correctness failure. The long-term efficiency path is
also the formal-verification path: preserve compact mathematical ownership,
issue exact certificates during construction, and make every consumer a
mechanical projection of that one checked root.
