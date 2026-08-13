# Rumoca compiler state review

Date: 2026-07-27
Branch: `msl-trace-parity-50`
Commit reviewed: `60e1d556` ("Improve Modelica compiler and runtime parity"),
plus 5 uncommitted working-tree files.

## Scope and method

Ten parallel review tracks covering: tensor-native IR ownership (upstream and
downstream), MLS frontend/grammar, MLS lookup/instantiation, MLS
types/arrays/functions, MLS equations/events/connections, structural and
numerical analysis, project-spec compliance, verification integrity, and MSL
simulation failure-bucket analysis.

Every track was required to produce `file:line` anchors, verbatim code quotes,
and a confidence label. Each track's findings were then **adversarially
re-verified against the running compiler** rather than accepted. This document
distinguishes three states:

- **Reproduced** — the reviewer independently ran it and observed the result.
- **Confirmed (code)** — verified by reading the cited code, not executed.
- **Corrected / not reproduced** — the track's claim was wrong, overstated, or
  could not be reproduced as described.

Corrections are recorded in §9 rather than quietly dropped.

No repository code was changed. One review agent edited three tracked files and
created two new ones despite read-only instructions; those changes were backed
up to the session scratchpad and the tree was restored. `git status` at the end
of the review matches its state at the start (the same 5 modified files).

---

## 1. Headline verdict

Rumoca's **frontend is genuinely strong** and its **structural middle end is
unusually well engineered** — transactional transformations, exhibited-witness
acceptance criteria, proved termination measures, and Dulmage–Mendelsohn
diagnostics that name the offending block by source origin. Parse, resolve and
typecheck are effectively solved against the MSL corpus (0 parse errors, 0
resolve errors, 0 typecheck failures across 566 models).

Against that, the review found **four confirmed silent-wrong-answer defects**,
all reproduced. Three of them are in load-bearing paths: DAE-level code
generation, index-3 systems, and the dominant MSL `redeclare` idiom. "Silent
wrong answer" is the worst defect class for a compiler whose stated product is a
portable symbolic system, and it is the theme of this review.

The second theme is that **the gates do not protect the properties they appear
to protect**. The tensor-scaling ratchet never asserts compactness; the MSL
quality baseline was hand-lowered in the very commit under review; two of three
headline PR gates are near-vacuous.

---

## 2. MSL / OMC trace parity: current state and failure buckets

### 2.1 Numbers and provenance

A **fresh full sweep exists** at `target/msl/results/`, written 2026-07-27
04:56–04:57, recording `git_commit = a966d9e8`. `git diff --stat a966d9e8 HEAD`
is empty — **the sweep's tree is byte-identical to HEAD**, so these are current
numbers, not a stale baseline. (The committed fallback baseline
`crates/rumoca-test-msl/tests/msl_tests/msl_quality_baseline.json` is older,
commit `a499eb8f`, `sim_ok = 207` — do not quote it as current.)

| Metric | Value | % of 566 |
|---|---:|---:|
| Parse | 566 | 100 % |
| DAE reached | 554 | 97.9 % |
| Solve-IR reached | 485 | 85.7 % |
| `sim_ok` (runs at all) | 238 | 42.0 % |
| **Trace parity (high + minor)** | **217** (190 + 27) | **38.3 %** |
| Target for 50 % | 283 | — |
| **Gap** | **+66 models** | — |

Denominator: 566 discovered root MSL 4.1.0 example models
(`crates/rumoca-test-msl/tests/balance_pipeline/balance_pipeline_example_targets.rs:20-51`).
Pass predicate: `…/balance_pipeline_reporting.rs:770`; band thresholds
`crates/rumoca-sim/src/sim_trace_compare.rs:20-23`.

Note: `msl_package_trace_accuracy.json`'s `acceptable_agreement` field sums to
434, **not** 217 — that file double-counts nested packages. The authoritative
figure is the band count in `msl_quality_current.json`.

Roughly 18 of the 566 are structurally unwinnable under the current metric: 13
MSL `partial model`s in the denominator plus 5 stochastic models that are
`sim_ok` but excluded from trace comparison. Effective target is ~283 of ~548
winnable ≈ 51.6 %.

### 2.2 Failure buckets (349 misses, mutually exclusive)

Classified structurally from `phase_reached` / `sim_status` / `sim_error_code`,
never text-derived.

| Bucket | Count | % of misses | Owning crate |
|---|---:|---:|---|
| **`ES010` structurally singular after elimination** | **136** | **39.0 %** | `rumoca-phase-structural` |
| Compiles + balances, never simulated (harness gate) | 56 | 16.0 % | harness predicate |
| `EX002` initial projection / event-update non-convergence | 32 | 9.2 % | `rumoca-sim`, `rumoca-phase-solve` |
| Timeout / memory limit | 28 | 8.0 % | `phase-structural`, `phase-solve` perf |
| `EX001` numerical solver failure | 26 | 7.4 % | `rumoca-solver-diffsol` |
| `EL0xx` Solve-IR unsupported construct | 25 | 7.2 % | `rumoca-phase-solve` |
| Simulates but trace mismatch / not comparable | 21 | 6.0 % | numerics, event handling |
| `ES014` structural IR contract violation | 13 | 3.7 % | `rumoca-phase-structural` |
| Compile-phase failure | 12 | 3.4 % | mixed |

**The shape of this matters more than the totals.** Frontend buckets are
essentially empty: `ED013` — the "DAE lowering unsupported construct" bucket one
might expect to dominate — is **exactly 1 model**
(`Modelica.Electrical.Spice3.Examples.Graetz`). Do not invest there.

**39 % of all misses are one bucket in one crate.** Of the 136 `ES010` models,
**70 are under-determined after elimination** — fewer reduced equations than
unknowns. That proves equations are being *lost during structural reduction*,
not that flattening produced a bad model.

### 2.3 The single highest-yield root cause

`ES010` decomposes by fingerprint over the unmatched-unknown field:

| Fingerprint | Models |
|---|---:|
| `*.fixed.flange.tau` (conditional support flange) | **53** |
| `spacePhasor_s/_r.v_[]` / `.i_[]` (Machines `airGap`) | 29 |
| Complex `.re`/`.im` (QuasiStatic polyphase) | 24 |
| `electroMagneticConverter` over-determined rows | 15 |
| `powerBalance.*` / `thermalAmbient.Q_flowTotal` | 13 |
| MultiBody `frame_a/b.f[]`,`t[]` | 11 |

The `fixed.flange.tau` case was traced end to end. MSL source
(`Modelica/Electrical/Machines/Interfaces/PartialBasicMachine.mo:39`) declares
`Fixed fixed if (not useSupport);` and connects it to a protected
`internalSupport`. **Flattening and ToDae are correct** — the emitted DAE for
`Modelica.Magnetic.FundamentalWave.Examples.BasicMachines.InductionMachines.IMC_YD`
contains the flow sum:

```
0 = -aimc.internalSupport.tau + aimc.inertiaStator.flange_a.tau
    + aimc.fixed.flange.tau + aimc.airGap.support.tau
    + aimc.strayLoad.support.tau + aimc.friction.support.tau;
```

and the DAE is balanced (1047 equations, 1047 unknowns, balance = 0). Yet the
structural phase reports `391 matched out of 391 equations and 392 unknowns;
unmatched unknowns: aimc.fixed.flange.tau`. **One equation vanishes during
structural elimination.**

Confirmed: the defect is downstream of ToDae in `rumoca-phase-structural`.
Likely site (not bisected): connection/flow-alias elimination consuming a
flow-sum row without retiring the matching column —
`src/eliminate/connection_policy.rs:8`, `src/eliminate/flow_policy.rs:4`,
`src/eliminate/orphan_unknowns.rs:15` (which scans only `equation.rhs`),
`src/dae_prepare/connection_alias.rs`.

**Reviewer-verified count:** exactly **5** models have `*.fixed.flange.tau` as
their *sole* unmatched unknown and would flip at the structural stage from this
one fix (4 × `aimc.…`, 1 × `smpm.…`). The failure-bucket track claimed 7; 5 is
the verified number. The other 48 carry additional unmatched unknowns and need
the `spacePhasor` / `electroMagneticConverter` work as well.

### 2.4 Family cross-cut

| Family | Missing | Dominant bucket |
|---|---:|---|
| **Rotating machines** (Electrical.Machines + Magnetic.FundamentalWave + Magnetic.QuasiStatic.FundamentalWave + Electrical.QuasiStatic.Machines) | **90 of 92** | ES010 ×68, timeout ×10 |
| Fluid + Media | 46 | harness gate ×25 |
| Electrical.Analog | 42 | EX001 ×12, EX002 ×10 |
| Electrical.PowerConverters | 31 | gate ×13 |
| Mechanics.MultiBody | 25 | ES010 ×13 |
| Magnetic.FluxTubes | 20 | ES010 ×12 |

**The rotating-machine family alone (90 missing models) is arithmetically
sufficient to close the 66-model gap to 50 %.** It is one family sharing a small
set of MSL base classes, and its dominant blocker is a single structural
elimination bug.

### 2.5 The "56 excluded" bucket is a trap

`is_root_standalone_msl_example_model` rejects a model when
`dae.variables.inputs` is non-empty. The predicate
(`crates/rumoca-ir-flat/src/lib.rs:275-298`) filters on variability/`fixed`/
`binding` but **never checks array size**, so `input Real[0] …medium.Xi`
(single-substance media, `nXi = 0`) and `parameter Real[0] …a/.b` in
`Blocks.Continuous.Filter` silently exclude the model.

Three of these were forced through simulation; **none reaches parity**:

| Model | Result once past the gate |
|---|---|
| `Media.Examples.WaterIF97` | `ES010`: unmatched `der_p`, `der_T` |
| `Fluid.Examples.Tanks.ThreeTanks` | `EL005`: non-static component reference |
| `Blocks.Examples.Filter` | `EL001`: `Bessel.na` not compile-time bound |

Fixing the zero-size predicate converts this bucket into `ES010`/`EL0xx` — it
does **not** convert it into parity. Worth doing as instrumentation (it stops
the metric from flattering itself and makes ~40 hidden failures visible), but
budget it as such, not as yield. Also note 13 of the 56 are MSL `partial
model`s — pure denominator inflation.

---

## 3. Confirmed silent-wrong-answer defects

These are ranked by severity. All four were **reproduced by the reviewer**.

### D1 — DAE-level codegen emits a mathematically wrong model (REPRODUCED)

For a 40×40 heat-equation stencil written as nested `for` equations, the
generated DAE-level export contains placeholder residuals instead of the real
stencil body. Measured on the generated SymPy source: of **1444 interior cells,
only 76 carry the real body — 1368 are `- 0.0` placeholders.** The heat equation
is frozen everywhere except two cells per row.

Reproduced across three independent DAE-level targets, so this is the **DAE
payload, not a template bug**:

| Target | Placeholder rows | Real stencil bodies |
|---|---:|---:|
| `sympy` | 1368 (interior) | 76 |
| `casadi-sx` | 1516 | 77 |
| `julia-mtk` | 1516 | 76 |

Mechanism: production compiles with `materialize_structured_families: false`
(`crates/rumoca-compile/src/session/compile_support.rs:147`). This does not
reduce the row count — only the bodies. `cheapen_equation_bodies`
(`crates/rumoca-phase-flatten/src/equations/mod.rs:1356-1370`) rewrites each
`Simple` equation's RHS to a real literal `0.0`, preserving the row count so the
downstream cell-to-row layout is unchanged. The compact template remains the
only truth, and `ir=dae` templates iterate `dae.f_x` with no family awareness
(`crates/rumoca-phase-codegen/src/templates/sympy/sympy.py.jinja:240-241`).

A guard exists — `ED015` ("cheapened parameter-variability algebraic family …
survived DAE promotion with dropped per-cell bodies") — and it fires correctly
when the family is parameter-variability. **It does not cover state families**,
which is the common case for a PDE. `--emit dae-mo` renders correctly because
`render_dae_modelica.rs` is family-aware; every other DAE target is not.

This is SPEC_0032 §3 ("No parallel scalarized owner … Avoids drift") realized as
literal drift, and makes SPEC_0007's "DAE is the lean canonical MLS Appendix B
model" false on disk. It also contradicts SPEC_0031's DAE contract property
**"Complete: captures the full mathematical system"**.

### D2 — `redeclare` in a component modification is a silent no-op (REPRODUCED)

The `extends` form works; the component-modification form — **the idiom
essentially every real Modelica model uses** — is silently ignored, and the
constraining-type check is skipped with it.

Reproduced:

| Model | Expected | Actual |
|---|---|---|
| `extends Inner(redeclare package Medium = Good)` | `y = 20.0` | `y = 20.0` ✅ |
| `Inner i(redeclare package Medium = Good)` | `i.y = 20.0` | **`i.y = 10.0`** ❌ |
| `Inner i(redeclare package Medium = Bad)` (non-subtype) | constrainedby error | **compiles clean, `10.0`** ❌ |

The parser populates `source_modification_redeclare_flags`
(`crates/rumoca-phase-parse/src/elements.rs:1240`), but `grep` shows the only
readers are `rumoca-tool-fmt`. `redeclare_target_value`
(`crates/rumoca-phase-instantiate/src/traversal_adapter.rs:45`) early-returns
unless `modification.redeclare`, a field that exists only on
`ExtendModification`. `validate_component_class_redeclare_target`
(`…/type_overrides.rs:690-738`) checks only `is_final` and `is_replaceable`.

This is invisible to CI **by construction**: every negative redeclare test in
`crates/rumoca-contracts/tests/type_contracts.rs` uses the `extends` form, and
the one component-form test (`type_001_subtype_redeclare_accepted:319`) asserts
only `expect_success`.

Given that `redeclare package Medium = …` is the backbone of `Modelica.Media`
and `Modelica.Fluid`, this is a correctness defect with direct parity
consequences.

### D3 — Index-3 constraint drift, silent (REPRODUCED)

Index reduction is **replacement**-based, not dummy-derivative: the constraint
row is overwritten with its derivative and the original is pushed into the
initialization partition
(`crates/rumoca-phase-structural/src/dae_prepare/state_row_reduction.rs:904-914`,
`deficient_row_reduction.rs:1030-1039`). So `g = 0` holds only at `t = 0`. There
is no Baumgarte stabilization, no manifold re-projection, no drift monitor.

Reproduced on a Cartesian pendulum, `gcon = x² + y² − L²`:

| t (s) | 0 | 12 | 24 | 36 | 48 | 60 |
|---|---|---|---|---|---|---|
| `gcon` | 0 | 8.66e-4 | 4.33e-3 | 9.93e-3 | 1.80e-2 | 2.71e-2 |

Monotone, ~`t²` growth, **exit code 0, no warning**. The rod is 1.35 % longer at
t = 60. This is the failure mode a Modelica user expects a compiler to have
solved, and the target family (MultiBody) is 25 missing models.

**Related and spelling-dependent:** the symbolic differentiator has **no power
rule**. `differentiate_binary`
(`…/dae_prepare/symbolic.rs:546-583`) handles only `Add/Sub/Mul/Div`; `^`
(`OpBinary::Exp`) falls into `_ => None`. The *identical* pendulum written
`x^2 + y^2 = L^2` — the natural Modelica spelling, and what MSL writes — fails
loudly with `ES010` instead of reducing. Verified by the reviewer: the `x*x`
form simulates (and drifts); the `x^2` form is rejected. So index-reduction
capability currently depends on how the modeller spelled a squared term.

### D4 — Enumeration-typed parameters lose their binding (REPRODUCED)

```modelica
type Color = enumeration(red, green, blue);
parameter Color c = Color.green;
```

emits `parameter Real c;` — type degraded and **binding dropped**. `c` defaults
to 0, every equality branch is false, and simulation **completes successfully**
with `der(x) = 0` where `10` is correct. The `constant` form of the same
declaration fails loudly, so the defect is specific to the parameter path.

### D5 — `assert` in an algorithm section is silently dropped (REPRODUCED)

`is_noop_algorithm_statement`
(`crates/rumoca-phase-dae/src/algorithm_lowering.rs:1313`) classifies
`Statement::Assert` as a no-op, and the only producer of assert event-actions
(`assertion_actions.rs:10`) reads equation-section asserts only.

Reproduced: an equation-section assert halts correctly at the analytically exact
time (`t = −ln 0.4 = 0.916293385`); the identical assert moved into an
`algorithm` section never fires and the model runs to completion. MSL uses
algorithm-section asserts widely as guards in `Modelica.Fluid` and
`Modelica.Media`.

---

## 4. Tensor-native state

### 4.1 What actually holds

Reproduced with the repo's own ratchet
(`target/debug/rumoca-tensor-scaling`), current tree:

```
whole-array-first-order: exponent -0.051 (limit 1.250)
  N=128   equations=1     families=1  domain_points=128
  N=2048  equations=1     families=1  domain_points=2048
cascaded-first-order:    exponent  0.881 (limit 1.250)
  N=128   equations=128   families=1  domain_points=127
  N=2048  equations=2048  families=1  domain_points=2047
```

Whole-array equations are **genuinely tensor-native** — one equation at N=2048.
That path (`RowMajorProjection`) proves the pipeline can carry a compact owner
end to end.

### 4.2 What does not

Everything else annotates a scalarized model rather than owning it.
`DaeContinuousPartition` documents this directly
(`crates/rumoca-ir-dae/src/lib.rs:503-504`): *"Structured source equation
families whose scalar views are present in `f_x`."* Both representations are
materialized.

Measured construct survival:

| Construct | Rows | Family? | Compact domain reaches DAE? |
|---|---|---|---|
| `der(u) = w` whole-array | 1 | yes | ✅ |
| comprehension `{… for k in 1:N}` | 1 | yes | ✅ |
| `w[:] = -u[:]` colon slice | 1 | yes | ✅ |
| `w[2:5] = -u[2:5]` **range slice** | 1 (`scalar_count=1`) | **no** | ❌ |
| `for i,j loop … end for` | **N** | side index | ⚠️ N rows + placeholders (D1) |
| `connect(a.pin, b.pin)`, `Pin pin[8]` | **16** | **no** | ❌ |

The range-slice gap is a ~5-line fix:
`project_component_dims_by_subscripts`
(`crates/rumoca-phase-flatten/src/equations/shape_inference.rs:76-85`) treats
every `Subscript::Expression` as a scalar index, so `u[2:N-1]` — the dominant
CFD/FEM boundary-split idiom — infers `Scalar` and never gets a family.

### 4.3 Solve IR and backends

All five `ComputeNode` variants have real producers; none is dead IR. But:

- **`MatMul` is structurally unreachable from the residual path.**
  `lower_residual_rows_from_equations_core` returns `Vec<Vec<LinearOp>>` — plain
  scalar rows — so `z = W*x` written as an ordinary model equation never becomes
  a `MatMul`. On the repo's own `NeuralODETensor` example, two 32-wide matvecs
  lower to 64 scalar programs of ~65 ops each.
- **Stencil strides are recovered by differencing scalar rows, not read from the
  source table.** `RegularForFamily::accesses` exists, is populated by flatten,
  and `ArrayAccess::binder_index_strides` has **no production caller**. For
  non-regular families the code runs a shrinking-prefix scan over scalar rows
  (`stencil.rs:868-914`). The repo's own test calls this "the strides the
  **reassembly** inferred". This directly contradicts SPEC_0032 §4 and
  SPEC_0007 Stage 4 ("must not rediscover stencils by scanning anonymous scalar
  rows").
- **Backends: 3 native consumers vs ≥14 `to_scalar_program_block` call sites.**
  Only the MLIR template, the WGSL template, and `PreparedComputeBlock` consume
  tensor nodes. C, Rust, CUDA, FMI2/3, embedded-C, CasADi, JAX, Cranelift and
  wasm all take the scalar expansion. **CUDA advertises tensor awareness in a
  comment header while generating scalar rows**
  (`templates/cuda-c/model_solve.cu.jinja:52-59` iterates `nodes` only inside a
  comment) — contradicting SPEC_0032 §4's "the reported kernel inventory must
  match the generated work".
- No BLAS/faer anywhere; `MatMul` is a naive triple loop and `LinSolve` is dense
  Gaussian elimination with a fresh `Vec` per call.

### 4.4 The tensor gate does not test tensor-ness

`report.passed = report.exponent <= args.max_exponent` (default 1.25) —
a bound on **compile-time** growth only. The report *records*
`equations`/`structured_families`/`compact_domain_points` but **never asserts on
them**. A fully scalarizing compiler passes this gate as long as scalarization
is roughly linear, which `cascaded-first-order` demonstrates at exponent 0.881
with 2048 materialized equations.

---

## 5. Project-spec compliance

**Clean and verified:** the crate dependency DAG is strictly downward (the one
upward edge is a dev-dependency and creates no cycle); zero cross-crate
re-exports outside the three approved facades; **zero `panic!`/`todo!`/
`unimplemented!` in library code**; all 11 library `.unwrap()` sites read and
provably guarded; only 13 `Span::DUMMY` constructions in non-test library code;
no HashMap iteration order found reaching compiler output.

**Violations found:**

- **SPEC_0007 Invariant 1 is contradicted inside a single `match`.**
  `crates/rumoca-phase-solve/src/lower/scalar_ops.rs:304-334` rejects `Pre` and
  `Reinit` as upstream bugs while *lowering* `Edge`, `Change` and `Sample`;
  `previous` is handled at `function_calls.rs:184-190`. The `edge`/`change`
  expansion in `builtin_methods.rs:195-199` is a verbatim duplicate of the
  expansion SPEC_0007 assigns to `phase-dae`.
- **The spec-named enforcement gate is evaded by a rename.** `sample` is
  rewritten to `__rumoca_sample` (`rumoca-core/src/lib.rs:64`), and
  `source_temporal_function_name` (`:84-95`) matches only the literal `"sample"`.
  Solve lowering converts the alias straight back
  (`function_calls.rs:104-114`). The gate also visits only 2 of 6 partitions
  (`appendix_b_validation.rs:286`), so `f_z`/`f_m` are unprotected — verified by
  a clocked model whose `f_z` contains `hold(yc)` and `Clock(0.1)`.
- **Scalarization helper defined in an IR crate**, used at 49 call sites:
  `scalar_name_for_flat_index` / `scalar_name_text_for_flat_index`
  (`rumoca-ir-dae/src/lib.rs:1069-1082`). Several callers use the rendered
  string as a map key.
- **`rumoca-ir-dae` contains phase logic**: `event_threshold.rs` (281 lines)
  performs name lookup by rendered string with `format!`-synthesized names,
  constant/variability inference, and backend policy. The CI test meant to
  prevent this reads **one file** and checks **eight literal strings**
  (`architecture_hardening_test.rs:1656-1677`), and directs authors to
  `rumoca-analysis-dae` — **a crate that does not exist**.
- **Post-resolution identity keyed on rendered strings** in `phase-typecheck`
  (`src/lib.rs:640-716`), with subscript-stripped and prefix-stripped textual
  aliases; same pattern in `phase-dae/src/binding_conversion.rs` and
  `balance.rs:239-242`.
- **The CI ratchet for the name-hierarchy rule is blind to its largest
  offender.** `TEXTUAL_MODEL_PATH_HELPERS` omits `strip_all_subscripts` and
  exempts all `path_utils.rs` and `rumoca-phase-codegen`, so CI reports zero
  debt while 26 violating sites exist in the checked set. The direct
  `.split('.')` check is genuinely clean (2 hits, both legitimate) — the
  violations are all indirect.
- **`ET0xx` prefix collision is nine codes wide**, not the one SPEC_0008
  records. `rumoca-galec-codegen` mints `ET001`–`ET023`; typecheck mints
  `ET000`–`ET010`. SPEC_0008 also cites
  `crates/rumoca-phase-typecheck/src/errors.rs`, which **does not exist**, and
  points consumers at `error_code_matches`, which is **private**.
- **`PhaseError` is implemented by 2 of 9 phase crates.**
- **Six specs cited by code do not exist** (SPEC_0003/0004/0019/0020/0024/0027),
  across 22 source sites. Two modules cite AGENTS.md for rules AGENTS.md does
  not contain — which AGENTS.md:42 explicitly forbids.

### 5.1 Spec-set defects found directly

- **SPEC_0032 is assigned to two different ACCEPTED specs** —
  `SPEC_0032_RANGE_PRESERVING_TENSORS.md` and
  `SPEC_0032_DEVELOPMENT_PROCESS.md`.
- **`spec/README.md` indexes only the first.** The Development Process spec —
  which AGENTS.md routes to for the entire triage and evidence workflow — is
  absent from the index that AGENTS.md calls the starting point.
- Line counts in `spec/README.md` are stale for several entries.

### 5.2 File-size limits are met by clustering, not decomposition

Measured distribution of non-generated `.rs` files:

| Band | Files |
|---|---:|
| 1000–1499 | 113 |
| 1500–1799 | 62 |
| 1800–1899 | 29 |
| **1900–1999** | **40** |
| **≥2000** | **2** |

The enforced threshold is 2000 (`crates/xtask/src/review_scan_cmd.rs:403`,
`main.rs:81`). A natural size distribution does not put 40 files in the last
100-line band and 2 above it. Files are being split *at* the limit rather than
*to* the SPEC_0021 target of 200–500 lines; 195 files exceed 1000 lines.
This satisfies the check while defeating its stated purpose.

---

## 6. MLS compliance summary

**Frontend — strong.** Reviewer-verified rejections, each with a precise caret
and several citing the MLS clause: `1 < 2 < 3` (EP001), `2*-2`, Real `==`
outside a function (`ER029 … MLS §3.5`), array dimension mismatch (`ET002`),
out-of-bounds constant subscript (`ET009 … MLS §10.5.1`), unbalanced model
(`ED001`), non-ASCII identifier (LEX-001), non-nesting block comments (LEX-003).
Operator precedence and associativity match MLS §3.2 on every case tested.
`rumoca-tool-fmt` roundtrip is lossless.

The frontend's failure mode is uniform and structural: **the grammar parses a
construct, the parser generates the slot, and the `TryFrom` never reads it** —
usually because the destination AST node has no field. Confirmed instances:
external-function `annotation` (making `Library`/`Include` structurally
unrepresentable, so **no external C function can be linked**), equation and
statement `description`/`annotation`, short-class-specifier
description/annotation, top-level `final`, `break connect(...)`, and — inside
the redeclare path — the redeclared type, the `replaceable` prefix, and
enumeration literals.

Also reviewer-reproduced: **two top-level classes of the same name in one file
silently overwrite**, last one winning, with no diagnostic (the component path
correctly errors).

**Equations / events — the strongest area.** The Stage-3 contract for
`pre`/`edge`/`change`/`terminal`/`delay` holds exactly as SPEC_0007 describes,
verified by reading the lowering out of the emitted DAE row for row. `noEvent`
is a real inherited suppression flag, not a strip. Runtime behaviour is
analytically exact (assert at `−ln 0.4`, terminate at `ln 2`). Twelve EQN
contracts have real producer code.

**Connections and streams — the most complete chapter implementations.** Real
union-find, correct ±1 inside/outside flow sign convention, connect-in-`for`
with enforced evaluability, a genuine §9.4 virtual connection graph with
spanning-forest root selection, and a §15.2 mixing formula correct for N > 2
with self-exclusion and a regularized denominator. Expandable connectors are
refused rather than half-done (though users see a misleading `ET001` rather than
the intended `EF020`).

**State machines — the model of how to not implement something.** Recognized,
named, and rejected with `ER073` pointing at MLS §17.

**Weakest areas:** arrays (`end` lowers to literal `0`; vector subscripts
unsupported; `size(A)` typed as scalar rather than `Integer[ndims(A)]`;
`promote` absent; four builtin result types wrong; `floor`/`ceil` disagree
between typecheck and constant-folding), interface/type relations (§6.4 omits
variability ordering, dimension-count matching and enum-literal matching;
function compatibility compares **name vectors only**), and operator overloading
(§14 declarations validated, call sites never resolved — replaced by a
field-name heuristic that treats any `.re`/`.im` suffix as complex).

---

## 7. Verification integrity

This section is the one with the most direct consequences for how much the other
sections' numbers can be trusted.

- **The MSL quality bar was hand-lowered in the commit under review.**
  Reviewer-verified: the **only** numeric change in `60e1d556` is
  `flatten_models: 565 → 555` (in two places), while `git_commit` stays
  `a499eb8f` and every other counter is untouched (`dae_models` 545,
  `solve_models` 446, `balanced_models` 532, `sim_ok` 207). A promoted snapshot
  from a real run cannot produce that shape. The gate allows a Flatten drop of 1
  (`MSL_STAGE_COUNT_ALLOWED_DROP = 1`), so a 10-model regression ships green
  because the PR that caused it lowered the number it is judged against. The
  commit message simultaneously claims *"All 41 Magnetic FundamentalWave
  examples flatten again"*.
- **The migration escape hatch performs no value comparison.** `choose_baseline`
  (`crates/xtask/src/verify_cmd/msl_quality_baseline.rs:110-139`) compares only
  `omc_version` and `sim_target_models`; `MslQualityBaselineHeader` does not even
  *deserialize* `flatten_models`, `sim_ok`, `balanced_models` or `git_commit`.
  Once a migration is declared, any metric may be set to any value.
- **Silent network-failure fallback**: any transport error returns `Ok(None)`
  and the gate runs against the checked-in file, logging one `eprintln!`. There
  is no `--require-remote-baseline`.
- **The "assertion-heavy semantic gate" blocks on one model.**
  `modelica_test_targets_ci.json` contains exactly
  `["ModelicaTest.Blocks.MuxDemux"]`, out of ~1000 in ModelicaTest, unchanged
  since the file was created.
- **The compatibility gate integrates for zero seconds** (`--stop-time 0.0`,
  2 models).
- **Whole suites cannot fail CI**: `msl-sim-tests`, `backend-stress-tests` and
  `msl-external-tests` (including the only FMI2 cross-check, 592 lines) are
  selected by *nothing*. The `#[ignore]` ban is perfectly honored (0
  occurrences) — achieved via the mechanism the spec offered as the alternative.
- **The MLIR suite is a silent no-op**: 42 of 45 tests early-`return` on
  `ToolNotFound`, and no CI job or `flake.nix` installs an MLIR toolchain.
- **10 of 521 MLS contract cases run a simulation.** 100 contracts have
  Accept-only coverage, and 139 of 158 `expect_success` calls discard the
  result entirely. The Reject side is genuinely strong (phase + code pinned).
- **"Golden" DAE/Solve files are built in Rust, not parsed from Modelica** —
  valid schema tests, misleading names.
- **Coverage gate**: per-package regressions are non-fatal, the only fatal check
  allows ~5400 newly-uncovered lines, and the baseline is from 2026-05-31,
  lists 26 packages for a 53-crate workspace, and includes a crate that no
  longer exists. Every crate added since May has no coverage floor.
- **Fuzzing**: wired correctly but nightly-only and explicitly non-gating; one
  20-line target asserting nothing; 0 crash reproducers, and `fuzz/.gitignore`
  forbids committing them.

**Correct negative finding:** "Allow compatibility gate cold cache fallback"
(`1acf3641`) is a 3-line `timeout-minutes: 15 → 60` change. It **cannot** mask a
regression. The commit message is misleading; the change is benign.

**Process note:** `dev/2026-07-27-msl-trace-parity-branch-review.md` reports
fixes F1/F2/F3 as landed, but those edits are **uncommitted working-tree
changes** — HEAD does not contain them. The reviewer verified the fix works
(`repeated_exponentiation_is_rejected_instead_of_truncated` passes) while the
committed state still accepts `2^3^2`.

---

## 8. What is genuinely good

Recording this deliberately, because a defect list is not an assessment.

- Parse/resolve/typecheck are **solved** against MSL: 0/0/0 failures over 566
  models. The frontend is stricter than OMC in exactly the places MLS is strict.
- The structural middle end's engineering discipline is above average for a
  compiler of this age: transactional transformations with explicit revert,
  acceptance requiring an *exhibited* perfect matching rather than an improved
  bound, a stated and runtime-checked well-founded termination measure,
  attributed failure traces (`reason=not_differentiable`), and guards against
  specific silent-deletion bugs the authors evidently hit and fixed (the
  `0 = 0` vacuous-derivative guard cites `Fourbar1`).
- Diagnostics quality — Dulmage–Mendelsohn over-determined blocks named by
  source origin — is better than several production tools.
- Every convergence loop traced bails out with a **hard error naming the
  offending variable**.
- Zero `panic!`/`todo!`/`unimplemented!` in library code; no reachable
  `.unwrap()` on malformed-but-legal Modelica.
- No quantitative parity claims in `README.md`, `CHANGELOG.md` or `docs/` — all
  numbers live in machine-generated artifacts. That is real restraint and
  removes an entire class of staleness.
- The DAE/Solve IR data model (compact domains, affine descriptors,
  comprehension templates, O(rank) validation that explicitly refuses to
  materialize domain points) is well designed. The problem is ownership, not
  representation.

---

## 9. Corrections to review-track findings

Recorded so the tracks' reports are not cited uncritically.

- **"if-without-else fabricates `y = 0`" — severity corrected.** The behaviour
  is real and reproduced (`0 = (if c[1] then (y - (2*x)) else (y - 0.0))`), but
  the input is **illegal Modelica** (MLS §8.3.3 requires an else-branch and
  balanced branches for a non-parameter condition). This is over-acceptance of
  invalid input, not corruption of valid models. One track called it "the most
  severe finding"; it is not.
- **`end`-subscript severity corrected.** `v[end]` → `v[0]` is real and
  reproduced, but it fails **loudly** at simulation (`EL001`), not silently, and
  affects **12 of 2556 MSL files** — notably
  `Blocks.Continuous.TransferFunction` and `Math.Vectors.reverse`. Medium
  priority, not top.
- **Tensor F2 repro was wrong.** The claim (placeholder residuals in generated
  code) is **correct and severe**, but did not reproduce on the model shape
  given; an `ED015` guard catches the parameter-variability case. An all-state
  variant was required to trigger it. Recorded as D1 with a working repro.
- **`fixed.flange.tau` immediate-flip count corrected**: 5 verified, not 7.
- **`acceptable_agreement` cross-check rejected**: sums to 434, not 217, because
  `msl_package_trace_accuracy.json` double-counts nested packages. The
  authoritative figure is the band count (190 + 27 = 217).
- **Parity baseline corrected mid-review**: the committed
  `msl_quality_baseline.json` (`sim_ok = 207`) is stale; the fresh
  HEAD-identical sweep gives `sim_ok = 238` and trace parity 217.
- **"Compatibility gate cold cache fallback masks regressions" — refuted**
  (see §7).
- One track reported the release binary accepting `2^3^2` as a current defect;
  that binary predates HEAD. The working tree rejects it.

---

## 10. Recommended priorities

Ordered by parity yield and by severity, respectively.

### For the 50 % trace-parity goal (need +66 models)

1. **Fix the lost flow-sum row in structural elimination** (`ES010`,
   `*.fixed.flange.tau`). 5 models flip immediately at the structural stage; 53
   carry the fingerprint. Small-to-medium: the DAE input is already correct,
   which is the cheap half of any structural bug. Bisect
   `src/eliminate/{connection_policy,flow_policy,orphan_unknowns}.rs` and
   `src/dae_prepare/connection_alias.rs`.
2. **Machines `spacePhasor` array-connector flow sums and
   `electroMagneticConverter` over-determined rows.** 29 + 15 models, heavily
   overlapping (1). Together (1)+(2) cover ~68 of the 90-model rotating-machine
   family — **arithmetically sufficient to close the gap on its own**.
   Medium-to-large.
3. **Solve-phase performance on the machine/MultiBody tail.** 20 of 28 timeouts
   are in structural lowering, not the solver; 10 machine models already sit at
   the 45 s budget, so (1)+(2) will expose them. Budget alongside.

Explicitly deprioritise: `ED013` (1 model), parse/resolve/typecheck (0
failures), and the harness-gate fix *as a yield play* (it converts misses into
different misses — do it as instrumentation).

### For correctness, independent of parity

1. **D1** — stop the placeholder leak. Either refuse cheapening when the target
   consumes `dae.f_x`, or make cheapened rows structurally un-consumable (an
   explicit `EquationBody::PlaceholderOfFamily(idx)`) so templates fail loudly
   instead of emitting `der(u[2,4]) - 0.0`. Extending the `ED015` guard to state
   families is the minimum.
2. **D2** — wire `source_modification_redeclare_flags` into instantiation and
   route the component-modification path through the same constraining-type
   check as the extends path. Add negative contract tests in the
   component-modification form; the current suite structurally cannot catch this.
3. **D3** — at minimum, emit a warning whenever a constraint row is consumed by
   replacement; add the power rule to `differentiate_binary` so reduction stops
   depending on `x^2` vs `x*x`.
4. **D4/D5** — enum parameter bindings; `assert` statements in model algorithms.

### For the specs and gates

1. **Make `choose_baseline` reject a checked-in migration whose metrics are
   worse than the promoted asset's**, and make a failed baseline download a hard
   error unless `--offline` is passed. This converts §7's first finding from
   "ships green" to "blocked" and is a small change.
2. **Make the tensor ratchet assert compactness**, not just compile-time
   exponent. It currently cannot fail for the reason it exists.
3. **Resolve the SPEC_0032 number collision** and add the Development Process
   spec to `spec/README.md`.
4. **Reconcile SPEC_0007 Invariant 1 with `scalar_ops.rs:304-334` by decision.**
   Either `phase-dae` eliminates `edge`/`change`/`sample`/`previous` and the gate
   learns about `__rumoca_sample` and the other four partitions, or the spec is
   amended to state which operators legitimately cross into Solve and why. Today
   the spec and the code disagree inside one `match`.
5. **Retire or resurrect SPEC_0003/0004/0019/0020/0024/0027** (22 code sites
   cite deleted governance), fix SPEC_0008's non-existent reference file and its
   nine-wide `ET0xx` collision note.
6. **Spec the debt-ratchet mechanism or delete it.** It is now load-bearing for
   at least four rules and is governed by nothing — no rule says who may raise a
   baseline or that baselines must shrink.

---

## 11. Limitations

- The MSL sweep was **not** re-run; the analysis uses the 04:56 artifacts, whose
  tree is byte-identical to HEAD. The 5 uncommitted working-tree files are
  therefore not reflected in the parity numbers.
- **No OMC ground truth exists for the 349 failing models.**
  `omc_simulation_reference.json` records `skipped` for all of them, because the
  gate builds an OMC baseline only for models Rumoca already simulates. **We do
  not know how many of the 349 OMC itself can simulate**, so the achievable
  ceiling is unknown. Obtaining it requires
  `cargo xtask verify msl-parity --all-omc-targets`.
- All "estimated gain" figures in §10 are estimates except the 5 verified
  immediate flips — and even those could still fail later at IC, solver, or
  trace-comparison stages.
- The precise elimination site for the `fixed.flange.tau` row loss is **not
  bisected**: the equation is confirmed present in the DAE and confirmed absent
  by matching time.
- No workspace-wide `cargo test` or `cargo clippy` run. Complexity compliance is
  inferred from `deny`-level workspace lints on a compiling tree.
- Clock/synchronous coverage (§16 base-clock partitioning, sub-clock inference)
  and state-machine semantics were inventoried but not behaviourally tested
  beyond a single base clock.
- Several per-track findings marked LIKELY or SPECULATIVE in the source reports
  were not independently reproduced and are cited here only where the code
  evidence is unambiguous.
