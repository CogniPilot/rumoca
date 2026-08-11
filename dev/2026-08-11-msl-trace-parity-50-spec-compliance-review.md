# Branch Review: `msl-trace-parity-50` Against the Rumoca Specifications

**Date:** 2026-08-11
**Scope:** the working tree as it stands (uncommitted changes included), reviewed against all
24 in-tree specifications (17 ACCEPTED/DRAFT + 7 REFERENCE), plus four deep dives: the formal-verification restructuring, the
GALEC → Solve IR production-code unification, overall efficiency/performance, and the coherence
of the specification suite itself.
**Method:** 24 per-spec review agents + 4 deep-dive agents (one per dimension above) produced
270 raw findings. Every finding of medium severity or higher was then adversarially verified by
independent refuter agents (evidence lens: does the citation hold; significance lens: would the
claim survive informed maintainer pushback). Round 1 verified the 100 top-ranked findings (all
15 critical + 85 high) with two refuters each; round 2 verified the remaining 125 (121 medium +
4 high) with one refuter each. Only findings that survived refutation are asserted below;
disputed and refuted claims are reported separately, and notes drawn from disputed material are
marked as such. The tree continued to change while the review ran; gate measurements in §4.3
were re-taken at publication time.
Nothing in the repository was modified; this file is the only artifact of the review.

---

## 1. Executive Summary

**The branch's two headline goals point in the right direction, and the migration plan's own
ledger (`dev/2026-08-08-galec-to-solve-ir-production-code-plan.md`) is honest about progress —
but the specification suite is not.** Milestone 0 amended eight specs to describe the
*post*-migration architecture as current fact while Milestones 3–5 remain `PENDING`, so two
ACCEPTED specs (SPEC_0007, SPEC_0029) and their normative-by-reference annexes (SPEC_0040,
SPEC_0041) now bind obligations to `SolveAlgorithmBlock` — a type with **zero occurrences in any
Rust file**. That is a direct violation of SPEC_0000 §4 ("ACCEPTED specs that describe
unimplemented features" are prohibited) and is the single largest source of spec/code incoherence
on the branch.

**The tree is red against its own enforcement gates.** Five deterministic failures exist as the
tree stands, all branch-introduced (main is clean on each):

1. **Build break:** `rumoca-exec-wasm` cannot compile — `emit_op`'s exhaustive match over
   `LinearOp` omits the new `PureCall` variant with no wildcard arm (rustc E0004). This blocks
   the entire workspace.
2. `spec_budget_test`: SPEC_0007 (ACCEPTED) is 2,518 words against the CI-enforced 2,500 hard cap.
3. `env_var_registry` hardening test: 43 quoted `RUMOCA_*` literals across 5 crates (42 survive
   the gate's comment filter) against SPEC_0018's "literal zero" policy (registry is empty).
4. `code_size_budget_test`: 9 production files exceed 2,000 lines with no SPEC_0021 exception
   marker (largest: `rumoca-eval-solve/src/lib.rs` at 4,452 lines, up from 1,962 on main).
5. `dae_loc_trigger_test`: all three SPEC_0043 §1b LOC-ledger ceilings are exceeded
   (15,543/4,286/19,829 measured vs 15,000/4,250/19,000 acknowledged).

**One genuine soundness bug survived adversarial review:** the solver-Y dependency derivation in
`rumoca-eval-solve/src/sparsity.rs` drops `TensorLoad{input: Y}` dependencies (the primal lane is
unconditionally recorded as empty), a false negative in exactly the "no false negatives" property
SPEC_0039 exists to guarantee. Downstream, the per-output algebraic refresh plan can skip refresh
of an algebraic output that reads an array-valued state, yielding stale values.

**The formal-verification restructuring is real but substantially ahead of the code.** What is
genuine: `Dae::construct` is authentically valid-by-construction (private fields, branded
non-serializable handles, deserialization that replays construction); `FmiComponent::construct`
checks what it claims; the single Kani harness exercises the true production kernel
(`advance_event_iteration_lanes`), the xtask Kani gate is fail-closed and version-pinned, the
coverage manifests honestly report zero MLS formalization coverage, and there is no `unsafe` in
the IR crates. What is not yet real: `SolveProblem` and `flat::Model` — the roots both production
pipelines actually run on — remain public-field aggregates with `Default` and post-hoc
`validate()`, and the `SolveAlgorithmBlock` root does not exist. (These are permitted while
SPEC_0036 is DRAFT, and its Scope says it stays DRAFT until exactly these are done — but the
distance between the catalog text and the tree is large, and several ACCEPTED-spec references
already assume the end state.)

**The GALEC → Solve IR unification has not happened in code.** Both eFMI C targets
(`embedded-c-galec`, `galec-production`) still declare `ir = "algorithm-code"` and render through
`lower_to_algorithm_code` → `AlgorithmCodeTemplateRenderer` → a 475-line Jinja template that does
builtin mapping, array scalarization, ABI selection, and signal-bit encoding — precisely the
template-side semantic lowering the plan promises to delete (its own "Current Gap #7"). FMI3 C
uses the wholly separate f64-register `LinearOp` path. The two production paths share no IR, no
view, and no template vocabulary today. The typed foundation (`rumoca-ir-solve/src/typed_program/`,
Milestone 2 increment 2) exists — the plan ledger records its 13 focused tests passing (not
re-run by this review) — but it is additive vocabulary only. `rumoca-eval-galec` — the independent differential oracle the plan's proof strategy depends
on — is an orphan crate with no consumer, so the `checked GALEC == eval-solve == generated C`
chain does not run. The current (pre-migration) C path also carries three confirmed defects of
its own (§4.2).

**Performance:** the Solve-IR consolidation is architecturally sound and the prepared-block layer
already caches useful per-row metadata, but hot paths are inconsistent about using it. Three
systemic mechanisms dominate: per-op deep clones from by-value `LinearOp` dispatch on the
steady-state lazy trace-replay path; per-refresh recomputation (allocate + sort + discard) of
static per-row properties like `row_reads_y`; and DAE-sized scratch allocations inside per-scalar
loops (structural incidence allocates and zeroes a whole-arena `vec![false; expression_count()]`
per projected scalar coordinate).

**Bottom line.** The direction — one checked Solve IR family under both production-C paths, with
valid-by-construction roots and machine-checked kernels — is well-conceived, and the parts that
have landed (DAE construction, FMI component, the Kani gate, the shared B.1b causal plan) are of
high quality. The debt is concentrated in one habit: **writing the destination into normative
spec text before the code arrives**. Restore the SPEC_0000 discipline (target state belongs in
DRAFT specs and the plan ledger; ACCEPTED text describes the tree), make the five red gates green,
and fix the two code bugs, and this branch is in strong shape.

---

## 2. Per-Spec Scorecard

Grades are from per-spec agents whose claims then survived (or died under) adversarial
verification. Notes are confirmed unless explicitly marked *(disputed)* — evidence sustained,
significance contested — or *(refuted as violation)* — the fact holds but verifiers established
the governing document permits it.

| Spec | Compliance | Spec coherence | Load-bearing confirmed issue |
|---|---|---|---|
| 0000 Spec Guidelines | partial | significant issues | Own regime broken in-tree: SPEC_0007 over hard cap; 4 specs missing required sections (round-2 sustained); 12/24 README line counts stale *(disputed: values carry "~", no tolerance defined)* |
| 0001 DefId | partial | significant issues | Core structure/rules hold; "semantic identity keys" hard rule contradicted by the enforced hardening gate and Solve IR name-keyed storage (disputed significance, facts verified) |
| 0002 Scope Tree | partial | significant issues | API/lookup semantics verified compliant; spec's own struct sketch names a type not used; prohibition bullet contradicts the mandated key type |
| 0007 IR Pipeline | partial | significant issues | ACCEPTED spec names `SolveAlgorithmBlock` (nonexistent); wire "version 12" vs code 31; 2,518 words vs 2,500 CI cap |
| 0008 Phase Errors | mostly compliant | significant issues | Machinery verified genuine; "CodegenError never carries `rumoca_core::Span`" exemption rationale is factually false (4 variants carry one) |
| 0018 Tool Config | partial | minor issues | Config/LSP/workspace rules verified compliant; "literal zero" `RUMOCA_*` policy broken ~42× (branch-new); one env var is a behavior knob, not diagnostics |
| 0021 Code Complexity | partial | significant issues | 9 files > 2,000 lines unmarked → gate fails; 46/68 complexity `#[allow]`s lack the mandated exception comment |
| 0022 MLS Compliance | partial | significant issues | Deep-checked contracts genuinely hold (clock schedule, ExternalObject lifecycle, purity, SIM-010); registry missing 6 of 438 rows, asserts 432; section index wrong in every row; EXPR-040 marked Implemented with no event-generation path |
| 0025 PR Review Process | partial | significant issues | Sign-off/ignore/clippy-wrapper rules verified compliant; §4 ModelicaTest gate command is dead (its env vars are read by no code) |
| 0029 Crate Boundaries | partial | significant issues | Tier isolation verified where deep-checked; §12's mandated no-string-assembly CI check does not exist; MLIR/WGSL target-named Rust subsystems in phase-codegen |
| 0031 Compiler Philosophy | partial | significant issues | "DAE is the only contract" is false in this tree (all backends consume Solve IR — confirmed); the branch demoted it ACCEPTED→REFERENCE rather than fixing the text (demotion is a git-diff fact; its propriety *disputed*) |
| 0032 Range-Preserving Tensors | partial | minor issues | Compact-domain/DAE rules verified; §6 affine sparsity still rediscovered from scalarized rows, `AffineDomain` never produced *(disputed on reachability)* |
| 0033 Development Process | mostly compliant | minor issues | Canary, parity harness, baseline promotion, resource budget all verified as specified; the claimed residual gaps did not survive verification (counterexample-count producer claim refuted in round 2) |
| 0034 GALEC eFMI Export | partial | significant issues | Export path (admissibility, builtins, manifests, XSD/checksums) verified compliant; the whole `SolveAlgorithmBlock` production-C branch and Integer range proofs (GAL-028/030) unimplemented while the ladder marks the rung "Earned" |
| 0035 Complex Numeric Types | partial | significant issues | DRAFT and honest about it; three load-bearing citations stale (deleted symbols — confirmed). The "width rules contradicted without reconciliation" claim was **refuted**: the spec's Current State section already discloses the gap |
| 0036 Valid-by-Construction IR | partial | significant issues | DAE half is real and verified; Solve/Flat halves aspirational (permitted as DRAFT); references SPEC_0037 at `archive/deferred/` — a broken link to an active spec |
| 0037 Formally Verified Compiler | mostly compliant | significant issues | The one manifest proof is genuine, production-kernel-true, fail-closed, pinned; the manifest schema cannot record the assumptions the spec requires of every entry (round-2 sustained); an inert `#[kani::proof]` in ir-dae exists but listing it is not required *(refuted as violation)*; GALEC/FMI phase-catalog omission *(disputed)* |
| 0038 Unified FMI Execution | partial | minor issues | ME lifecycle/buffer/brand rows verified met; live Diffsol session still consumes Solve layout directly (phase 2 pending); `c-ode` remains user-visible alongside FMI targets |
| 0039 Proof-Carrying Sparsity | partial | significant issues | Checked CSR, provenance, coloring verified; **core "no false negative" invariant broken for `TensorLoad`-from-Y** (§4.1, confirmed); pattern-provenance types derive `Deserialize` against the spec's letter (round-2 sustained); the `Unknown→Full`/Affine-absence claims were refuted in round 2 |
| 0040 IR Stage Contract Catalog | partial | significant issues | Spot-checked rows genuinely hold (C09/C11/C13/C14/C21/C41/C46/C54); six rows bind a nonexistent type; SOLVE-C51 broken by the wasm build break; SOLVE-C01 purity row falsified by intentional impure ops |
| 0041 Crate Ownership Catalog | partial | significant issues | 55/56 crates verified against rows; 4 rows assign ownership of the nonexistent root; **phase-galec implements a second DAE pure-function lowerer the §4 one-lowerer row forbids (confirmed — see §3.2)**; diffsol violates the concrete-solver row (staged by SPEC_0038 phasing, so *disputed*) |
| 0042 GALEC Language Catalog | partial | significant issues | Most trap consequences (T3–T14) genuinely implemented and cited by ID in code; D2's Solve-root decision has no implementation *(refuted as violation — annex of a DRAFT parent)*; D11 span rule violated by the main mangling path *(disputed)* |
| 0043 Construction Catalog | partial | significant issues | §8 (FmiComponent) genuinely implemented; §1b LOC ledger stale → gate red; §3 arenas, §6 `SolveProblem::construct`, §7 `flat::Model::construct`, §9 root: none exist (§3 names are stylized labels for real owner scopes — contested) |
| 0044 FMI Execution Catalog | partial | minor issues | §1 bounded-ME profile fully verified (lifecycle table, 9 buffer classes, brands); §2 lacks checked-kernel trace parity and negative capability tests (both confirmed — see §8 P3); the §3 FMI-LS-DAE "nonexistent code" claim was refuted in round 2 |

Aggregate: 21 of 24 specs `partial`, 3 `mostly-compliant`; 0 of 24 fully coherent (5 minor-issues,
19 significant-issues). The dominant failure mode is not bad code — it is spec text describing a
future or past tree.

---

## 3. The Two Headline Claims, Audited

### 3.1 Restructuring for formal verification

**Genuinely landed and verified:**

- `Dae::construct` (rumoca-ir-dae) — three private fields, one construction authority, generative
  brand, borrowed non-serializable handles, and `Deserialize` that replays the same checked
  construction operations. This matches SPEC_0036's model and is the strongest artifact on the
  branch.
- `FmiComponent::construct` (SPEC_0043 §8) — private fields, by-value consumption, checks what it
  claims.
- The Kani gate: `verification/kani-proofs.json`'s single entry names a real `#[kani::proof]`
  (`crates/rumoca-solver/src/verification/event_iteration.rs:104`) that calls the genuine
  production kernel `advance_event_iteration_lanes` (compiled ungated, used at
  `runtime/pre_params.rs:83`) — not a simplified twin. `cargo xtask verify kani` enforces Kani
  0.67.0, one-harness-at-a-time, per-harness fail-closed timing and cover attribution; toolchain
  pinned via `rust-toolchain-kani.toml` + `flake.nix`; unwind(16) and `covers: 3` match the
  manifest.
- Honest manifests: `mls-formalization-coverage.json` reports 438/438 contracts at zero proof
  coverage — no inflation. `modelica-association-gaps.json` is internally consistent.
- No `unsafe` anywhere in the IR crates.
- The shared B.1b causal-discrete plan (Milestone 1) is real: one branded structural result
  consumed by both Solve and GALEC lowering, with the three scheduling counterexamples retained
  as tests.

**Not yet real (permitted as DRAFT, but load-bearing):**

- `SolveProblem` — the IR both production pipelines converge on — has all-public fields,
  `Default`, no `construct`, fieldwise-deserializing children behind a wire type, and a public
  `validate()` that production calls (`ir-fmi/src/lib.rs:189`; an earlier phase-solve call site
  cited by the review data no longer exists in the tree at publication time). It is built by
  struct literal in ~101 places. `flat::Model` is the same shape (33 public fields,
  `new() -> Self::default()`, public `validate()`).
  SPEC_0036's Scope is explicit that it stays DRAFT until these are done, so this is *tracked
  distance*, not a violation — but every "checked kernel" claim that routes through Solve today
  rests on post-hoc validation, not construction.
- Invariants in the DAE view layer are enforced by ~36 `expect`/`unreachable` sites — runtime
  panics, not type-level impossibility.
- A second `#[kani::proof]` (`rumoca-ir-dae/src/tests/wire_roundtrip_verification.rs:739`) sits
  under `#[cfg(test)]` and is compiled under no configuration the Kani driver reaches — inert.
  (Adversarial review established the manifest/spec doesn't *require* listing it; it is still
  dead verification code worth either wiring or deleting.)
- Contested but worth recording: the one proof's symbolic domain is constructed from
  already-valid typed values, so the type-fidelity clause is unfalsifiable by construction; the
  genuinely symbolic content is the transactional/ownership behavior (refuters split on whether
  the manifest's "arbitrary value kinds" phrasing overclaims).
- Round-2 sustained: the manifest loader (`crates/xtask/src/verify_cmd/kani.rs:54`) has no
  `assumptions` field at all, so the machine-readable-assumptions rule SPEC_0037 puts on every
  entry (":256 …MUST identify … assumptions") is structurally unsatisfiable — and the one
  assumption that matters here (values drawn from an always-valid quantized lattice) is exactly
  what the field exists to surface.

**Verdict:** the foundation is real, honest at the manifest level, and correctly fail-closed. The
"formally verified compiler" posture, however, currently rests on one bounded proof plus one
genuinely-checked construction root. The restructuring *enables* the program; it has not yet
*executed* it for the IRs that matter most to this branch (Solve), and several ACCEPTED-spec
references already speak as if it had.

### 3.2 eFMI production C on the shared Solve IR

The intended architecture (from the plan, which remains the honest source of truth):

```
modelica -> flat -> dae -> checked AlgorithmCodePackage -> SolveAlgorithmBlock -> jinja -> Production C/H
modelica -> flat -> dae -> SolveProblem -----------------------------------------> jinja -> FMI3 C
```

**What the tree actually runs today:**

- `embedded-c-galec` and `galec-production` both declare `ir = "algorithm-code"`
  (`target.toml:2` in each) and route through `rumoca_phase_galec::lower_to_algorithm_code` →
  `AlgorithmCodeTemplateRenderer` (`crates/rumoca/src/target_manifest.rs:609–615`) → Jinja.
- The C body template (`embedded-c-galec/model.c.jinja`, 475 lines, inherited by
  `galec-production` via `{% extends %}`) performs builtin mapping (`absolute`→`fabsf`, …),
  array scalarization/unrolling, value-vs-pointer ABI selection from `value.direction`,
  signal-bit encoding, and operator-precedence handling — the plan's "Current Gap #7", untouched.
- FMI3 C renders from the separate f64-register `LinearOp` program (Boolean as 0.0/1.0), the
  exact representation the plan's "Current Gaps" #1–2 call out.
- `rumoca-phase-solve` has no `lower_algorithm_block` entry point; `rumoca-ir-galec` does not
  depend on `rumoca-ir-solve`; the two paths share no IR, no view, no template vocabulary.
- `rumoca-ir-solve/src/typed_program/` (Milestone 2, increments 1–2) is real and additive:
  typed registers/slots, explicit conversions, aggregate ops, constructor-replayed wire decoding.
  The plan's ledger records 13/13 focused tests (not re-run by this review, which executed no
  cargo commands; the module now carries 29 `#[test]` attributes). No existing execution or
  codegen path consumes it yet — exactly as the plan's change log states.
- **Confirmed architectural duplication on the migration path:** `rumoca-phase-galec` implements
  a second, ~2,000-line DAE pure-function lowerer (`src/lower/user_functions.rs`) walking the
  same `DaeView` as the shared tensor-native lowering, which SPEC_0041 §4 explicitly forbids
  ("GALEC may reject a subset but cannot implement a second function lowerer"). Both refuters
  sustained it, noting the two lowerers can diverge on argument passing, tensor shape, and
  builtin lifting — and that phase-galec cannot even link the shared lowerer as the crate graph
  stands, so the row is structurally unsatisfiable today. This is the concrete divergence risk
  the migration exists to remove, live in the current tree.
- `rumoca-eval-galec` has **no consumer anywhere in the workspace** (round-2 sustained at
  medium: Cargo.lock shows zero reverse dependencies). The differential chain
  `checked GALEC == eval-solve(SolveAlgorithmBlock) == generated C` that the plan's Evaluation
  and Proof Strategy requires therefore does not run; what runs is
  generated-C-vs-DAE-simulation comparison under tolerance.
- The eFMI Production Code trace-parity gate is a hardcoded literal (round-2 sustained): the
  only executable check in `crates/rumoca/tests/cli_target_galec_production.rs` compares one
  fixture's driver output against the string `"2.0\n6.0\n14.0\n"` (:1212). The sibling
  `galec_equivalence.rs` harness (C ticks vs a rumoca reference simulation) shows the required
  shape already exists for `embedded-c-galec` — it just was not applied to the certified target.

**Verdict:** the unification is a well-specified plan with its foundation poured, not a landed
capability. Nothing here contradicts the plan's own ledger (M3/M4/M5 `PENDING`) — the problem is
solely that eight specs were rewritten in Milestone 0 to the target state and two of them are
ACCEPTED (§5.1). On the flexibility claim ("a new C-family target by template alone"): the
underlying facts — the Algorithm-Code Jinja semantic layer and Rust-side Solve renderers both
still exist and sit on the add-a-target path — are undisputed, though the round-2 refuter judged
the "not yet true" inference contestable given what the target-directory mechanism already
supports for Solve-IR targets. Treat it as: template-only addition works for simple Solve
targets today, not for C-family production targets.

---

## 4. Confirmed Code Defects

These survived two independent adversarial refuters each.

### 4.1 Bugs

| # | Severity | Finding | Anchor |
|---|---|---|---|
| B1 | **critical** | `rumoca-exec-wasm` `emit_op` matches `LinearOp` exhaustively but omits the new `PureCall` variant (no `_` arm) → rustc E0004; the crate is an unconditional workspace member, so **the workspace does not compile** in the current tree state. Cranelift and eval-solve both handle `PureCall`; wasm was simply missed. | `crates/rumoca-exec-wasm/src/emit.rs:324` (match ends at `StoreOutput`, :430); variant at `crates/rumoca-ir-solve/src/linear_op.rs:905` |
| B2 | **high** | Solver-Y dependency derivation drops `TensorLoad{input: Y}`: the primal lane is unconditionally `set_empty_dependency`, so a program reading a contiguous array-valued Y coordinate reports **no Y dependency**. `phase-solve/lower/scalar/coordinates.rs:104` emits exactly that op for any multi-scalar Y coordinate. The result feeds `program_output_y_dependencies` → `refresh_plan.rs:637` → per-output algebraic refresh (`solve_runtime.rs:2913`); an algebraic output depending on an array state can be skipped and go stale. This is a false negative in the exact property SPEC_0039 exists to prevent ("no unsafe under-approximation"). | `crates/rumoca-eval-solve/src/sparsity.rs:939` |
| B3 | high→medium | Three templates re-derive C symbol names under three divergent reserved-word policies, so the same variable can be spelled differently in header vs source vs Production-Code manifest (link/compile break for adversarial names; manifest `LogicalData` mismatch — the manifest is the eFMI deployment contract). | `crates/rumoca-phase-codegen/src/templates/embedded-c-galec/model.c.jinja:12` and header/manifest counterparts |

B1 deserves a process note: it exists in *uncommitted* work, which is precisely when a review
like this is cheapest — but it also means every "gate is red" statement below is currently
unreachable by `cargo test` until the arm is added.

### 4.2 Confirmed defects in the current (pre-migration) eFMI C path

Beyond B3: unguarded signed `int32_t` arithmetic is emitted verbatim in generated production C
(`model.c.jinja:53,58`) with no Integer range proofs anywhere in the pipeline — signed overflow
is C undefined behavior in safety-oriented generated code, and GAL-028/030 require fail-closed
range proofs (one refuter sustained this as a genuine hole; one argued the DRAFT status defers
it — recorded as disputed, but the UB mechanism itself is undisputed). Round 2 sustained two
supporting facts at medium: `embedded-c-galec` declares no `[integer]` domain at all (only 2 of
17 targets do), and the declared `TargetIntegerDomain` is inert everywhere — read once for a
min≤max sanity check (`rumoca-compile/src/codegen_target.rs:933`), never threaded to a proof,
the template, or an evaluator. The Float32 profile is likewise declared but unenforced (round-2
sustained at medium): the only GALEC evaluator computes in f64 (`rumoca-eval-galec/src/value.rs:51`)
while both C tracks compute in `float`, and `galec_equivalence.rs` absorbs the mismatch with a
2.0e-6 tolerance instead of a matching profile. These reinforce the plan's own rationale for the
migration — the current path makes semantic choices in templates with no construction-level
guarantee.

### 4.3 Red gates (deterministic CI failures on this tree, all branch-introduced)

| Gate | Failure | Fix shape |
|---|---|---|
| workspace build | B1 above | add one match arm |
| `spec_budget_test::test_specs_respect_size_budgets` | SPEC_0007 at 2,518 words (cap 2,500; main was 2,499) | trim 18+ words or move content to the SPEC_0040 annex |
| `architecture_hardening::env_var_registry` | 43 quoted `RUMOCA_*` literals in 5 crates (phase-solve 22, exec-cranelift 9, solver 9, sim 2, eval-solve 1), 42 surviving the gate's comment filter, vs empty registry | route through `--trace`/`--trace-profile` or CLI flags; register or remove |
| `code_size_budget_test` | 9 unmarked files > 2,000 lines, re-measured at publication (eval-solve/lib.rs 4,452; phase-solve/lower/scalar/functions.rs 4,307; solver/runtime/solve_runtime.rs 3,136; ir-solve/linear_op.rs 2,853; phase-solve/lower.rs 2,434; eval-solve/prepared.rs 2,203; ir-solve/lib.rs 2,501; ir-dae/model/wire.rs 2,109; phase-dae/construction/expression.rs 2,008) — four grew further while the review ran | split by concern or add the documented exception marker |
| `dae_loc_trigger_test` | all three SPEC_0043 §1b ceilings exceeded (needs 15,750/4,500/20,000) | re-measure and move the ledger in the same change, as §1b itself mandates |

Special case inside the env-var set: `RUMOCA_DISABLE_NATIVE_EXECUTION`
(`crates/rumoca-sim/src/rk45.rs:22`) is a **behavior knob** — it silently changes which execution
backend produces results — not a profiling probe. SPEC_0018 routes exactly this class to a
discoverable `clap` flag. Both refuters sustained this at high severity.

Adjacent (round-2 sustained): the file-size exception marker has no ceiling or expiry —
`exec-cranelift/src/emit.rs` sits at 7,702 lines (3.8× the action threshold) under one marker
comment and grew further on this branch, so the escape hatch licenses exactly the unbounded
growth the gate exists to prevent. Worth a policy decision (marker + line budget, or dated
split plans) rather than more markers.

---

## 5. The Specification Suite Itself

### 5.1 The structural problem: aspiration written as fact

Milestone 0 of the migration plan ("specification amendments … before implementation") amended
SPEC_0007/0029/0034/0036 and annexes 0040–0043 to the target architecture. For DRAFT specs
(0034, 0036, 0043 §9) this is legal — SPEC_0000 §5 explicitly allows DRAFT to describe
unimplemented features, and the adversarial layer consistently refuted "violation" claims against
them on those grounds. For the ACCEPTED specs it is not:

- SPEC_0007:51, :238 ("Current type/name" column!), :270 and SPEC_0029:129, :264 make
  `SolveAlgorithmBlock` normative now. SPEC_0007:270 binds SOLVE-C32–C38 as its "complete
  obligations"; within those, C32/C34/C38 name the type directly (3 of 55 Solve rows ≈ 5% of the
  catalog with no artifact to audit), and SPEC_0041 rows 76/77/85/90 assign its ownership.
  `grep -rn SolveAlgorithmBlock --include='*.rs' crates/` → 0.
- Consequence, verified by the reviewing agents themselves: a reader auditing
  `rumoca-ir-solve`/`rumoca-eval-solve`/`rumoca-phase-solve` against these rows cannot determine
  whether the crates are non-compliant or the spec is aspirational.
- The same Milestone-0 exit ledger claims "SPEC 0007 and 0034 remain within word budgets" — now
  false (SPEC_0007 is over the cap; see §4.3).

**Recommendation:** adopt one convention and apply it in a single truthing pass — either (a) tag
every target-state row in ACCEPTED specs/annexes with an explicit `PENDING (plan 2026-08-08 M3/M4)`
marker, or (b) move those rows into the DRAFT specs where SPEC_0000 already permits them. Option
(a) preserves the useful property that the specs describe the destination; it just stops them
lying about the present.

### 5.2 Version literals rot on contact

The DAE wire schema version is stated three incompatible ways: SPEC_0007:180 says "version 12 is
the only supported wire version"; SPEC_0036:229 says "Only DAE wire v13 exists"; the code enforces
`DAE_SCHEMA_VERSION = 31` (`rumoca-ir-dae/src/model.rs:136`, checked at `wire.rs:710`). Each spec
also forbids adapters for "every other version," so each makes the other's number illegal. This
literal has now been wrong three times; both specs should cite the constant by name instead of
restating a number. (The underlying single-version/no-adapters rule *is* correctly enforced —
verified.)

### 5.3 Confirmed coherence defects (survived refutation)

- **SPEC_0022 section index:** every row's line range and per-category count is wrong (counts sum
  510 vs the body's 438; §4.1 claimed at lines 297–314, actually 328; §5 claimed 820–845,
  actually 898). The document explicitly instructs agents to navigate by this index, so selective
  loading fetches the wrong contracts. Pre-existing on main, but the branch edited §5 totals
  without touching the index.
- **Contract registry drift (branch-introduced):** SPEC_0022 gained 7 contracts on this branch;
  only CONN-030 reached `crates/rumoca-contracts` (registry asserts 432, catalog says 438;
  ARR-041/042, FUNC-036/037/038, SIM-010 missing). `event_iteration.rs:35` cites "registry row
  SIM-010" — a row that does not exist. No test compares catalog to registry, so the drift is
  invisible. Add the 6 rows and a catalog↔registry consistency test.
- **EXPR-040 status inflation:** registered `Implemented`, but `div/ceil/floor/integer` lower to
  plain arithmetic with no event-root construction anywhere, so a discrete variable driven by
  `integer(x)` changes value mid-step without a state event (also strains SIM-008/EQN-034). The
  status rests on an accept-compiles smoke test.
- **SPEC_0008 exemption rationale is false:** the justification for CodegenError's exemption from
  the PhaseError rule claims it "never carries a `rumoca_core::Span`"; four variants do
  (`errors.rs:65,76,93,107`), populated from real source spans — confirmed. Whether those spans
  are then dropped unrendered is *disputed* (a refuter showed `errors.rs:204` retains the span
  and `dae_diagnostics.rs:33–45` renders labels where source is available). Rewrite the
  rationale either way.
- **SPEC_0025 §4 dead gate:** the mandated ModelicaTest command sets `RUMOCA_MSL_*` env vars that
  no Rust code reads (the harness moved to JSON parity config via `cargo xtask verify msl-parity`).
  A reviewer following the spec verbatim runs a no-op and reports it as gate evidence.
- **SPEC_0029 §12 enforcement gap:** the CI check the section mandates (rejecting target-language
  string assembly in phase-codegen) does not exist — confirmed. Under its absence sit two
  distinct accumulations: the confirmed target-named Rust renderers
  (`render_solve/mlir_family.rs`, 796 lines with 114 `format!` sites emitting MLIR, plus direct
  WGSL emission in `render_solve.rs:1664`), and the *disputed* generic renderers
  (`render_expr.rs`/`render_stmt.rs`, 151+138 `format!` sites — these emit flat/base-Modelica
  text, and one refuter accepts them as scheduled wave-queue debt).
- **SPEC_0031:** demoted ACCEPTED→REFERENCE on this branch while keeping its MUSTs and "Hard
  Rule" headings; its central "DAE is the only contract" claim is false for every backend in the
  tree. Either restore ACCEPTED with corrected text or strip the rule language.
- **SPEC_0035:** three load-bearing citations point at deleted symbols
  (`project_complex_mul_or_div`, the `phase-structural/scalarize/` path, `EL002`) — confirmed.
  (The stronger claim that the branch shipped the opposite of its §1 width rules "without
  reconciling the spec" was **refuted**: the spec's Current State section explicitly discloses
  the single-variant reality and schedules precision-neutrality as a later phase.)
- **SPEC_0036:317 broken link:** references SPEC_0037 at `archive/deferred/…` — the file lives
  in `spec/` as an active DRAFT (README agrees). One of the two headline formal-verification
  documents mislabels the other as deferred.
- **SPEC_0040 SOLVE-C01** states all Solve ops are pure functions of `(y,p,t)` — falsified by
  intentional `ImpureRandom`/`TableLookup`/`LoadSeed` and contradicted by SOLVE-C44 in the same
  catalog.
- **SPEC_0039 letter violations in the pattern layer (round-2 sustained):** `PatternProvenance`
  and `PatternRepresentation` derive `Deserialize` (`rumoca-ir-solve/src/structural_pattern.rs:17,51`)
  against the spec's literal "Pattern fields do not derive `Default` or `Deserialize`" — a
  deserialized `Span::DUMMY` provenance bypasses the `MissingProvenance` guard the negative test
  claims to enforce. Separately (sustained, downgraded to low), `from_row_dependencies` collapses
  rows-with-holes to `Diagonal` (`:427`), a safe over-approximation that nonetheless breaks the
  one-canonical-representation contract (same relation compares unequal across derivation paths).
- **Annex tension (recorded as disputed but structurally real):** SPEC_0000 defines REFERENCE as
  "not rules" and uncapped, while annex rows are "normative by reference"; SPEC_0040 now holds
  6,466 words of MUST rules under a 2,518-word parent. The refuter correctly notes §3a defines
  this model deliberately — but the effect remains that the binding surface has largely escaped
  the size regime the budget test enforces.
- Three pipeline diagrams disagree (SPEC_0007 vs SPEC_0029 §4 vs SPEC_0031); SPEC_0029's tier
  graph omits ~13 of 56 crates and lists a `viz` crate family that does not exist. Round 2 made
  the SPEC_0007 error concrete: its diagram swaps typecheck/instantiate relative to both
  SPEC_0029 §4 and the code (`compile_support.rs:229–260` instantiates, then typechecks, then
  flattens) and places phase-flatten on the Flat→DAE edge when it *produces* Flat
  (`rumoca-phase-flatten/src/lib.rs:307`). The stage-boundary owner's own diagram is the wrong one.
- Round-2 sustained code-hygiene notes attached to spec rows: Resolve keys declaration identity
  through a raw-string `name_to_def: IndexMap<String, DefId>` whose `alloc_def_id` insert
  unconditionally overwrites on collision (`rumoca-phase-resolve/src/lib.rs:489`, loop-index
  binding hazard); the cross-phase `EvalLookup` trait is defined over `&str` name + scope
  prefix (`rumoca-core/src/eval_lookup.rs:14`), institutionalizing the lookup pattern SPEC_0002
  prohibits; SPEC_0021's enforcement snippets show `warn` levels and a CI job that doesn't
  exist, while the workspace actually sets `deny` — which is exactly why undocumented
  `#[allow]`s accumulate; and SPEC_0029 §9's namespace list names two nonexistent
  `rumoca-compile` modules while omitting six real public ones.

### 5.4 What the specs get right

For balance, and verified deep in code by the reviewing agents: the SPEC_0008 error machinery,
SPEC_0018's config/LSP/workspace semantics, SPEC_0033's parity/canary/baseline governance
(including fail-closed trace-nonidentifiability), SPEC_0044 §1's bounded ME lifecycle profile
(every counted domain exists exactly as cataloged), SPEC_0042's trap catalog (most trap
consequences implemented and cited by ID in `validate/*`), and SPEC_0034's manifest/checksum/XSD
export web all check out against the tree. The catalogs are not fiction — they are mostly
accurate maps with a stale region concentrated around the pending migration.

---

## 6. Efficiency and Performance

Static analysis only (no builds or benchmarks were run); every mechanism below was verified by
reading the code, and the repo's own numbers (0.35–0.51 s runs, 6.5 s prepare in
`dev/2026-08-10-rdd2-tensor-native-performance-roadmap.md`) are quoted claims, not measurements
made by this review.

**Verified mechanisms (all confirmed except where tagged):**

- **P1 — per-op deep clones on the steady-state path** (`rumoca-eval-solve/src/lib.rs:1645`):
  `eval_lazy_pure_op` takes `LinearOp` by value, so the lazy trace-replay path — which the
  roadmap describes as the steady state — clones every executed op (`Box<[u32]>`,
  `Box<[TensorIndex]>`, Arc refcount traffic): O(ops) heap allocations per row per step.
- **P2 — static row properties recomputed per refresh**
  (`rumoca-solver/src/runtime/solve_runtime/refresh_batch.rs:155`): each segment probe calls
  `row_reads_y`, which allocates, recursively walks nested fold programs, sorts, merges, then
  discards — O(plan_len · row_ops log row_ops) per refresh for facts that are static per row and
  belong in the prepared block.
- **P3 — DAE-sized scratch per projected scalar** (`rumoca-eval-dae/src/projection.rs:101`):
  `for_each_scalar_coordinate_cached` allocates and zeroes a whole-arena buffer
  (`integer_stack: vec![false; view.expression_count()]`) per projected scalar coordinate —
  O(equation_scalars × total_arena_nodes) in memset alone on MSL-sized models.
- **P4 — unprepared evaluation entry re-derives register flow per call** *(disputed: one
  refuter sustained, one refuted; listed here because the mechanism is undisputed and the
  dispute is about which paths are hot)* (`rumoca-eval-solve/src/lib.rs:633`): fresh
  `RowEvalScratch` + `required_registers` (full program rescan) per call; root conditions
  dispatch elsewhere, but dynamic time-event rows and runtime assignments take this path.
- **P5 — debug env probes inside the hot projection loop** (round-2 sustained at medium):
  `project_algebraic_singleton_assignment` performs up to three `std::env::var_os` lookups per
  singleton block per projection sweep (`rumoca-solver/src/runtime/projection.rs:856,874,903`) —
  each takes std's global ENV_LOCK and scans the environment — guarding conditions that include
  **hardcoded debug row thresholds** (`*row == 646`, `*row >= 400`). This is leftover
  investigation scaffolding in a per-Newton-iteration path; the OnceLock-cached pattern already
  exists in `exec-cranelift/src/emit.rs:1113`. (Removing it also shrinks the §4.3 env-var gate
  failure.)
- **P6 — compile-time numeric evaluator lacks DAG memoization** (round-2 sustained at medium):
  `NumericEvaluator::expression` (`rumoca-eval-dae/src/numeric.rs:81`, new on this branch)
  re-evaluates shared subexpressions once per reference with a fresh `Vec<f64>` per node, and
  `function_fold_output` re-runs the entire fold body once per carried output queried (K·N body
  evaluations for K outputs over N domain points).

Round 2 also sustained, at low severity, two per-call rescan patterns worth folding into the P2
work: compiled Cranelift kernels re-sum `output_count` and rescan all rows on every native call
(`exec-cranelift/src/emit.rs:257,365`), and reverse-mode Jacobian row support is re-proved by
four full row scans per gradient evaluation (`eval-solve/src/prepared.rs:91–177`,
`reverse.rs:130`).

**Claimed but refuted or materially narrowed** (do not act on these as stated): RK45's
per-trial-step allocations and `format!` labels are pre-existing on main, not branch regressions;
the index-reduction O(E²) screening claim overstated the traversal's actual frontier; the
Cranelift interpreter's fold-program clone is on a differential-validation oracle path, not a JIT
fallback; the sparsity `BTreeSet` clone-per-read is real but not asymptotically distinct from the
achievable alternative.

**Compile-time:** the cache layer (`rumoca-compile` `cache.rs`, `parsed_artifact_cache.rs`,
`portable_source_root_cache.rs`) survived review with no confirmed invalidation-soundness finding
— notable, since a stale-cache bug was explicitly hunted. Lowering-side allocation churn
(dependency-set clones per register read in `sparsity.rs`, DAE-sized scratch in
`prepared/construction`) is the main prepare-time cost mechanism identified; the roadmap's 6.5 s
prepare figure is consistent with it but was not re-measured.

**Suggested order of attack:** P1 (switch dispatch to `&LinearOp`; mechanical), P2/P4 (move
static facts into `PreparedScalarProgramBlock`, reuse scratch), P3 (reuse one visited buffer or
switch to a generation-stamped bitmap), then re-run the roadmap's own benchmarks to keep the
claims measured.

---

## 7. Adversarial-Verification Ledger

Raw finding volume overstates; this section is what keeps the review honest.

- Round 1 (all 15 critical + 85 high): **37 confirmed, 42 disputed, 21 refuted.** Verifier
  severity corrections were overwhelmingly downward (most "critical" spec-staleness became
  medium: real, tracked, doc-only).
- Round 2 (the 125 findings past the round-1 cap — 121 medium + 4 high — one evidence-lens
  refuter each): **63 sustained, 62 refuted, 0 missing.** Of the sustained, 45 were downgraded
  to low severity; 18 held at medium and the load-bearing ones are folded into §3–§6 above
  (marked "round-2 sustained" where they first appear). Note the two cross-spec diagram findings
  in §5.3 (SPEC_0031, SPEC_0007 ordering) are among the four high-severity items that received
  only single-lens verification.
- 45 low-severity notes were not adversarially verified and are treated as leads only.

**Dominant refutation patterns**, useful as calibration for future reviews of this repo:

1. *DRAFT-spec normativity*: SPEC_0000 §5 permits DRAFT specs to describe unimplemented
   features, so "code doesn't implement DRAFT SPEC_00XX" is status, not violation. This killed
   most claims against 0034/0035/0036/0038/0042/0043 — and sharpened the real issue into the
   ACCEPTED-spec set (§5.1).
2. *Pre-existing vs branch-introduced*: several "regressions" (RK45 allocations, flatten's
   string-keyed scope walks, the SPEC_0022 index) predate the branch.
3. *Factually wrong greps*: e.g., "EventTransactionProgram has no consumer" — it has a full
   prepared-evaluator and runtime consumer chain the finder's grep missed.
4. *Staged by phasing*: diffsol's direct Solve-layout access is exactly what SPEC_0038's phase 1
   scopes out and phase 2 deletes; calling it a violation ignores the spec's own cutover table.

Of the 42 round-1 disputed items (one refuter sustained, one refuted — almost always
evidence-sustained / significance-contested), the load-bearing ones are quoted above with their
status marked; the remainder are indexed in Appendix A. The most consequential disputed cluster
is the current eFMI C path's semantic-choice surface (GAL-028 integer UB, template ABI
selection, Float32 non-enforcement): the facts are undisputed; what is contested is whether
DRAFT status defers them. Given SPEC_0038 names eFMI "the primary safety-oriented
code-generation path," this review's position is they belong on the M3/M4 critical path
regardless of normativity. One further disputed item deserves standalone mention because it is
branch-concentrated: SPEC_0025 §6's blanket ban on `#[allow(clippy::...)]` contradicts
SPEC_0021's sanctioned documented-exception mechanism and is violated 86× in-tree, 49 of them
added on this branch — the reviewer-gate checkbox is unusable as written.

---

## 8. Prioritized Recommendations

**P0 — before anything else merges from this tree**
1. Add the `LinearOp::PureCall` arm to `rumoca-exec-wasm/src/emit.rs` (B1); grep all `match`es
   over `LinearOp` for other missing-variant consumers while there.
2. Fix the solver-Y `TensorLoad` dependency false negative (B2) and add the regression test
   SPEC_0039 promises ("no false negatives" for pattern derivation); this is a wrong-results
   class, not hygiene.
3. Unify C symbol allocation into one Rust-side allocator feeding all three GALEC templates (B3).

**P1 — make the tree green against its own gates (all five items in §4.3)**
Including converting `RUMOCA_DISABLE_NATIVE_EXECUTION` into a CLI flag — it is the only
behavior-altering member of the env-var set.

**P2 — one spec truthing pass (single PR)**
Mark or relocate every `SolveAlgorithmBlock` row in ACCEPTED specs/annexes per §5.1; replace wire
version literals with references to `DAE_SCHEMA_VERSION`; sync the SPEC_0022 index and the
contracts registry (add the 6 rows + a catalog↔registry test); demote EXPR-040 from
`Implemented`; fix SPEC_0008's exemption rationale; fix the SPEC_0036→0037 link; replace
SPEC_0025 §4's dead command and reconcile its §6 clippy-allow ban with SPEC_0021; reconcile or
re-promote SPEC_0031; correct SPEC_0035's stale citations; fix SPEC_0007's pipeline-diagram
ordering; document the 46 undocumented complexity `#[allow]`s or add the missing enforcement;
update the Milestone-0 "within word budgets" ledger line.

**P3 — before the Milestone 3/4 cutover**
Wire `rumoca-eval-galec` into an executed differential gate (it is the independence guarantee
the whole proof strategy leans on) and replace the hardcoded-literal Production-C parity check
with the `galec_equivalence.rs`-shaped harness; retire the duplicate GALEC function lowerer (or
re-plumb the crate graph so the shared lowerer is linkable — the confirmed SPEC_0041 §4
violation in §3.2); land Integer range proofs (GAL-028/030) — the declared `[integer]` domains
are currently inert — and Float32 rounding enforcement before Production C is declared checked;
add the SPEC_0044 §2 checked-kernel trace parity and negative-capability tests for the four FMU
profiles (same defect class as the GALEC literal check, confirmed by both refuters); remove
`Deserialize` from the pattern-provenance types (SPEC_0039's own letter) or route them through
the checked constructors; add an `assumptions` field to the Kani manifest schema; implement the
SPEC_0029 §12 CI check so the template-purity property becomes enforced rather than aspired.

**P4 — performance**
P1→P6 of §6 (P5's env probes fall out of the P1 env-var cleanup for free), then re-measure
against the roadmap's own benchmark set.

---

## 9. Review Provenance

- Orchestration: 24 per-spec agents, 4 deep-dive agents, 325 adversarial verifier agents
  (two-lens refutation for the 100 critical/high findings, single-lens for the 125 medium),
  all read-only; 353 agents total.
- Verified-finding data and per-agent transcripts lived in the (ephemeral) session workspace
  (workflow runs `wf_3710f49f-3de`, `wf_c4a834a3-683`, critic pass `wf_5645dc29-94e`); because
  that workspace is not durable, Appendix A below indexes every disputed and refuted claim so
  this report is self-contained.
- A two-critic pass (accuracy against the evidence and the tree; completeness against the
  mandate) was run over this report before finalization; its corrections are incorporated,
  including striking one refuted claim originally stated as confirmed, correcting one erroneous
  performance citation, restoring one omitted confirmed finding (the duplicate GALEC lowerer),
  and re-measuring the §4.3 gate numbers at publication time.
- Spot-checks reproduced by the coordinating reviewer directly: `SolveAlgorithmBlock` absence,
  the wasm `PureCall` omission, the `TensorLoad` empty-primal-lane mechanism, SPEC_0036's broken
  SPEC_0037 link, and the migration plan's milestone ledger.
- Nothing in the repository was modified by this review; this file is the only addition.

---

## Appendix A — Disputed and Refuted Claim Index

One line per claim so the full adversarial ledger survives with this report. Format:
`[original severity] title — anchor` (+ short verdict note where useful).

### A.1 Round-1 DISPUTED (42) — evidence sustained, significance contested

- [critical] SPEC_0001 bans VarName as semantic identity; the repo's enforced hardening gate mandates it — `spec/SPEC_0001_DEFID.md`
- [critical] rumoca-phase-codegen Rust assembles target-language expressions and statements with format! — `crates/rumoca-phase-codegen/src/codegen/render_expr.rs`
- [critical] `SolveAlgorithmBlock` does not exist in code; both eFMI C targets still render from the GALEC Algorithm Code view — `crates/rumoca-phase-codegen/src/templates/embedded-c-galec/target.toml`
- [critical] SPEC_0034 module layout asserts crate dependencies that do not exist — `spec/SPEC_0034_GALEC_EFMI_EXPORT.md`
- [high] SPEC_0000 declares REFERENCE annexes non-rules yet also normative, letting binding rules escape all size budgets — `spec/SPEC_0000_SPEC_GUIDELINES.md`
- [high] README Lines column is stale for 12 of 24 entries, some off by more than 2x — `spec/README.md`
- [high] Table justification cells exceed SPEC_0000 §1's 15-word cap in over 100 rows, mostly in the new annexes — `spec/SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md`
- [high] Solve IR recovers variable identity by interning and re-parsing rendered names — `crates/rumoca-ir-solve/src/layout.rs`
- [high] ComponentReferenceKey uses rendered member idents plus a reused source DefId as identity fields — `crates/rumoca-ir-solve/src/layout.rs`
- [high] Flatten and DAE decide instance ownership by rendered-name prefix matching — `crates/rumoca-phase-flatten/src/outer_refs.rs`
- [high] Declaration identity recovered by hashing a rendered ComponentReference into name_map — `crates/rumoca-phase-typecheck/src/constant_collection.rs`
- [high] Spec's PROHIBITED hashing rule contradicts the scope key type the tree actually mandates — `spec/SPEC_0002_SCOPE_TREE.md`
- [high] WGSL and MLIR are target-language renderers written in Rust, dispatched by target name — `crates/rumoca/src/target_manifest.rs`
- [high] CodegenError source spans are captured then silently dropped (no #[label], no consumer) — `crates/rumoca-phase-codegen/src/errors.rs`
- [high] SPEC_0021 has no `## Specification` section and states its rules as prose, violating SPEC_0000 — `spec/SPEC_0021_CODE_COMPLEXITY.md`
- [high] Spec's file-size exemption ("cohesive modules are exempt") contradicts the gate, which requires an undocumented marker phrase — `spec/SPEC_0021_CODE_COMPLEXITY.md`
- [high] §6 blanket ban on `#[allow(clippy::...)]` contradicts SPEC_0021 and is violated 86× in-tree — `spec/SPEC_0025_PR_REVIEW_PROCESS.md`
- [high] src/codegen/ is not reserved for the MiniJinja extension-command surface — `crates/rumoca-phase-codegen/src/codegen/dae_backend.rs`
- [high] rumoca-solver-diffsol violates the concrete-solver-backend row, and CI cannot see its target-cfg dep — `crates/rumoca-solver-diffsol/Cargo.toml`
- [high] Status downgraded ACCEPTED→REFERENCE on this branch while the spec still carries MUST rules and a "Hard Rule" — `spec/SPEC_0031_COMPILER_PHILOSOPHY.md`
- [high] §6 affine sparsity is derived by scalarizing compute blocks and rediscovering the pattern; PatternDerivation::AffineDomain is never produced — `crates/rumoca-eval-solve/src/sparsity.rs`
- [high] Spec's central `SolveAlgorithmBlock` production-C path does not exist in the tree — `spec/SPEC_0034_GALEC_EFMI_EXPORT.md`
- [high] Emitted C performs unguarded signed int32_t arithmetic with no Integer range proof (GAL-028/030) — `crates/rumoca-phase-codegen/src/templates/embedded-c-galec/model.c.jinja`
- [high] C templates scalarize aggregates and choose call ABI — semantic choices forbidden by GAL-008/026/037 — `crates/rumoca-phase-codegen/src/templates/embedded-c-galec/model.c.jinja`
- [high] Module Layout dependency graph names three crate edges that do not exist — `spec/SPEC_0034_GALEC_EFMI_EXPORT.md`
- [high] SPEC_0037 phase-obligation catalog omits the GALEC and FMI production lowering phases — `spec/SPEC_0037_FORMALLY_VERIFIED_COMPILER.md`
- [high] Live Diffsol simulation session bypasses the FMI 3 ME boundary and reads Solve layout directly — `crates/rumoca-solver-diffsol/src/session.rs`
- [high] `c-ode` raw derivative-only C kernel is a user-visible target while FMI 2/3 are exposed — `crates/rumoca-phase-codegen/src/templates/c-ode/target.toml`
- [high] Acceptance-contract owner `require_state_only_bdf` / `StateOnlyRejection` is cfg(test)-only dead code in production — `spec/SPEC_0038_UNIFIED_FMI_EXECUTION.md`
- [high] GALEC production C/H is rendered from `AlgorithmCodePackage`, not from a checked Solve root — `crates/rumoca-phase-codegen/src/codegen/algorithm_code_renderer.rs`
- [high] rumoca-solver-diffsol takes production Solve IR, eval, and JIT deps the catalog forbids for solver backends — `crates/rumoca-solver-diffsol/Cargo.toml`
- [high] D11's "Production `Span::DUMMY` is prohibited" is violated by the primary GALEC name-mangling path — `crates/rumoca-phase-galec/src/mangle.rs`
- [high] §3 canonical arenas/systems: 9 of the 10 named types do not exist in rumoca-ir-dae — `spec/SPEC_0043_CONSTRUCTION_CATALOG.md`
- [high] Production phases run prohibited whole-root validation passes over finalized Solve IR — `crates/rumoca-phase-solve/src/lower.rs`
- [high] Kani manifest claims arbitrary typed values, but the harness can only generate valid ones — `verification/kani-proofs.json`
- [high] MiniJinja still performs the complete C semantic lowering the migration was meant to delete — `crates/rumoca-phase-codegen/src/templates/embedded-c-galec/model.c.jinja`
- [high] `rumoca-eval-galec` is an orphan crate: the differential oracle required by GAL-027/GAL-038 never runs — `crates/rumoca-eval-galec/src/lib.rs`
- [high] Float32 profile is unenforced: `.alg` and evaluator are f64 while the co-emitted C is `float` — `crates/rumoca-phase-codegen/src/templates/embedded-c-galec/model.c.jinja`
- [high] eval_scalar_program_block_with_context allocates fresh scratch, re-derives register counts, and clones every op per call — `crates/rumoca-eval-solve/src/lib.rs`
- [high] RK45 trial step allocates ~15 state vectors and builds a format! String per stage-combination check — `crates/rumoca-solver-rk45/src/lib.rs`
- [high] Index-reduction candidate screening is O(expressions^2) per residual with a DAE-sized allocation per inner node — `crates/rumoca-phase-structural/src/dae_transform/constraints.rs`
- [high] Sparsity derivation clones a BTreeSet dependency set on every register read — `crates/rumoca-eval-solve/src/sparsity.rs`

### A.2 Round-1 REFUTED (21) — did not survive; do not cite as findings

- [critical] `SolveProblem` has no `construct`, public invariant fields, `Default`, and a public `validate()` — `crates/rumoca-ir-solve/src/lib.rs`
- [critical] `SolveAlgorithmBlock` and its entire spec section/catalog describe a type absent from the tree — `spec/SPEC_0036_VALID_BY_CONSTRUCTION_IR.md`
- [critical] SolveProblem has no construction authority: public mutable fields, Default, and fieldwise wire decode — `crates/rumoca-ir-solve/src/lib.rs`
- [high] Branch adds a manual string-keyed scope walk in flatten, bypassing ScopeTree — `crates/rumoca-phase-flatten/src/boolean_eval.rs`
- [high] Production/embedded C templates resolve names and choose the call ABI during rendering — `crates/rumoca-phase-codegen/src/templates/embedded-c-galec/model.c.jinja`
- [high] EXPR-007/EXPR-029 delay bounds reworded to strict `>0` but the compiler enforces `>=0` and the registry still records `≥` — `crates/rumoca-phase-resolve/src/semantic_checks/builtin_calls.rs`
- [high] §4 MSL gate command contradicts SPEC_0033 §6a and §4's own baseline row — `spec/SPEC_0025_PR_REVIEW_PROCESS.md`
- [high] rumoca-compile, the core driver, unconditionally depends on Solve IR and codegen, past the stated core boundary — `crates/rumoca-compile/Cargo.toml`
- [high] Flat IR keeps materialized scalar rows as the authoritative owner, contradicting §1 "families stay authoritative in Flat" — `crates/rumoca-phase-flatten/src/equations/mod.rs`
- [high] §1 width rules are contradicted by this branch's shipped Solve IR, which embeds Binary32/Binary64 in the canonical type — `spec/SPEC_0035_COMPLEX_NUMERIC_TYPES.md`
- [high] SPEC_0043 §3's ten canonical arenas/systems do not exist as types in `rumoca-ir-dae` — `spec/SPEC_0036_VALID_BY_CONSTRUCTION_IR.md`
- [high] `flat::Model` root derives fieldwise `Deserialize` over 65 public fields with `Default` and public `validate()` — `crates/rumoca-ir-flat/src/lib.rs`
- [high] Advertised rumoca-ir-dae Kani harness compiles under no configuration and cannot be manifest-listed — `crates/rumoca-ir-dae/src/tests/wire_roundtrip_verification.rs`
- [high] Name-based private solver input setters are still the only input path in sim, session, and wasm bindings — `crates/rumoca-sim/src/simulation_session_api.rs`
- [high] Tensor operand patterns are wire-decoded claims, not derived; diagonal kernel trusts them — `crates/rumoca-ir-solve/src/tensor.rs`
- [high] EventTransactionProgram is constructed but has no evaluator/backend consumer, violating SOLVE-C55 and DAE-C21 — `crates/rumoca-phase-solve/src/lower/events.rs`
- [high] D2's `SolveAlgorithmBlock` C-text owner does not exist anywhere in crates/ — `spec/SPEC_0042_GALEC_LANGUAGE_CATALOG.md`
- [high] §6 `SolveProblem::construct` does not exist; Solve aggregate is pub-field with Default and post-hoc validators — `crates/rumoca-ir-solve/src/lib.rs`
- [high] §9 Solve Algorithm Block catalog describes a type that exists nowhere in the tree — `spec/SPEC_0043_CONSTRUCTION_CATALOG.md`
- [high] flat::Model has no construct(), all-public fields, Default-based new(), and a public validate() — `crates/rumoca-ir-flat/src/lib.rs`
- [high] Cranelift interpreter fallback deep-clones fold programs and allocates register/offset vectors per fold domain point — `crates/rumoca-exec-cranelift/src/emit/interpreter.rs`

### A.3 Round-2 sustained but downgraded to low (45)

- Four specs omit the MUST-have `## Specification` section; SPEC_0022 omits Status, Summary and Specification — `spec/SPEC_0022_MLS_COMPILER_COMPLIANCE.md`
- SPEC_0000's own REQUIRED code example cites a Dae type and location that no longer exist — `spec/SPEC_0000_SPEC_GUIDELINES.md`
- spec/README.md attributes workflow commands to a `rum` developer CLI that does not exist in the tree — `spec/README.md`
- Spec's DefId code block cites the wrong source file — `spec/SPEC_0001_DEFID.md`
- Encapsulation rule in spec omits the predefined-name fallback the code implements — `spec/SPEC_0002_SCOPE_TREE.md`
- Typecheck reconstructs ScopeIds by index, assuming they are sequential — `crates/rumoca-phase-typecheck/src/enum_context.rs`
- New Solve-IR public API `EventIterationOwner::ScalarRows` uses terminology the spec bans — `crates/rumoca-ir-solve/src/model.rs`
- SOLVE-C01's op inventory contradicts the actual `LinearOp` set (impure and stateful ops) — `spec/SPEC_0007_IR_PIPELINE.md`
- EFM0xx eFMI packaging range in the code table exists nowhere in the tree — `spec/SPEC_0008_PHASE_ERRORS.md`
- EL0xx and EGT0xx range descriptions overstate the codes actually minted — `spec/SPEC_0008_PHASE_ERRORS.md`
- Multi-rule sections written as narrative prose, violating SPEC_0000 §1 — `spec/SPEC_0008_PHASE_ERRORS.md`
- Spec Summary and title cover only formatter/linter TOML, while ~70% of the document is unrelated normative content — `spec/SPEC_0018_TOOL_CONFIG.md`
- README spec index reports SPEC_0018 as ~155 lines; the file is 329 lines — `spec/README.md`
- Exception code example uses types that do not exist anywhere in the tree — `spec/SPEC_0021_CODE_COMPLEXITY.md`
- CLK-002 and CLK-020 are the same MLS §16.3 rule under two contract IDs — `spec/SPEC_0022_MLS_COMPILER_COMPLIANCE.md`
- LEX-011 requirement text is garbled and unusable as a normative statement — `spec/SPEC_0022_MLS_COMPILER_COMPLIANCE.md`
- SPEC_0025 omits the mandatory `## Specification` section and blows the 15-word table-cell limit — `spec/SPEC_0025_PR_REVIEW_PROCESS.md`
- Dependency Tiers omit ~12 workspace crates, making the tier rule untestable for them — `spec/SPEC_0029_CRATE_BOUNDARIES.md`
- SPEC_0029 uses narrative prose for every rule section, violating SPEC_0000 §1 and size guidance — `spec/SPEC_0029_CRATE_BOUNDARIES.md`
- Tensor kernel inventory omits discrete structured B.1c Map nodes and Jacobian artifact blocks — `crates/rumoca-ir-solve/src/lib.rs`
- ACCEPTED spec §1 documents an unimplemented follow-up (compaction counter) plus historical narrative about a removed descriptor — `spec/SPEC_0032_RANGE_PRESERVING_TENSORS.md`
- Branch ledger quotes cohort numbers from a partial run with no commit or models_compared — `dev/2026-08-08-galec-to-solve-ir-production-code-plan.md`
- GAL-011 omits `--target galec-production`, contradicting the same document's Status and ladder — `spec/SPEC_0034_GALEC_EFMI_EXPORT.md`
- §'Current State' debt row and Phase 6 describe textual `Complex` name matches that no longer exist in lowering — `spec/SPEC_0035_COMPLEX_NUMERIC_TYPES.md`
- `EL002` is not a diagnostic code emitted by Solve lowering — `spec/SPEC_0035_COMPLEX_NUMERIC_TYPES.md`
- References link SPEC_0037 to a nonexistent `archive/deferred/` path and mislabels an active DRAFT as deferred — `spec/SPEC_0036_VALID_BY_CONSTRUCTION_IR.md`
- Emitted modelDescription.xml omits `needsCompletedIntegratorStep` and drops all declared variable units — `crates/rumoca-phase-codegen/src/templates/fmi3/modelDescription.xml.jinja`
- `ModelExchangeKernel::observe` is documented as `fmi3GetFMUState`, contradicting the module docs and the strict-surface rule — `crates/rumoca-solver/src/fmi_me.rs`
- Spec alternates between `SolveProblem` and `SolveModel` for the single projection input — `spec/SPEC_0038_UNIFIED_FMI_EXECUTION.md`
- from_row_dependencies classifies rows-with-holes as Diagonal, adding spurious entries — `crates/rumoca-ir-solve/src/structural_pattern.rs`
- STRUCT-T01/T02 placement contradicts SPEC_0007's DAE-to-DAE structural-lowering placement requirement — `spec/SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md`
- Catalog tables violate SPEC_0000 §1: 53/77 justification cells exceed the 15-word cap and rule cells are 100+ word prose — `spec/SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md`
- Clock variable resolution accepts a `tunableParameter`, contradicting D6's XSD-strict `constant` rule — `crates/rumoca-ir-galec/src/package.rs`
- T11 lists `String` as a GALEC reserved word but the reservation surface omits it — `crates/rumoca-ir-galec/src/builtins.rs`
- SPEC_0000 §1 15-word justification cap violated by 25 cells; rule cells reach 68 words of prose — `spec/SPEC_0043_CONSTRUCTION_CATALOG.md`
- §4 cross-form parity rows are declared normative but have no corresponding tests — `crates/rumoca/tests/fmi_ls_wasm_runtime.rs`
- DAE wire version: code is 31, SPEC_0036 says v13, ACCEPTED SPEC_0007 says v12 — `spec/SPEC_0036_VALID_BY_CONSTRUCTION_IR.md`
- Layering inversion: phase-codegen hardcodes one target directory's templates as the shared base for another target — `crates/rumoca-phase-codegen/src/codegen/mod.rs`
- Reverse-mode Jacobian row support is re-proved by four full row scans per gradient evaluation — `crates/rumoca-eval-solve/src/prepared.rs`
- Compiled Cranelift residual re-sums output_count and rescans all rows on every native call — `crates/rumoca-exec-cranelift/src/emit.rs`
- SPEC_0029's dependency-tier graph omits 13 of 56 crates and lists a `viz` family that does not exist — `spec/SPEC_0029_CRATE_BOUNDARIES.md`
- SPEC_0038 and SPEC_0041 both bar concrete solver backends from Solve IR, but rumoca-solver-diffsol depends on it in production — `spec/SPEC_0041_CRATE_OWNERSHIP_CATALOG.md`
- SPEC_0036 links to SPEC_0037 as an archived deferred spec; SPEC_0037 is an active DRAFT — `spec/SPEC_0036_VALID_BY_CONSTRUCTION_IR.md`
- spec/README.md Lines column and SPEC_0022's self-description and section index are substantially stale — `spec/README.md`
- rumoca-phase-fmi / rumoca-ir-fmi are a first-class phase in SPEC_0029/0041/0043 but have no SPEC_0008 error-code range — `spec/SPEC_0008_PHASE_ERRORS.md`

### A.4 Round-2 REFUTED (62)

- SPEC_0040 annex carries the bulk of the normative Solve rules, which SPEC_0000 §3a forbids — `spec/SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md`
- `SolveModel` vs `SolveProblem`: SPEC_0040 SOLVE-C51 makes a root normative that SPEC_0007's terminology table omits — `spec/SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md`
- Status enumeration contradicts the lifecycle: PROPOSED specs are required by §5 but disallowed by Required Sections and §8 — `spec/SPEC_0000_SPEC_GUIDELINES.md`
- Solve IR exposes String-keyed public identity maps and a name-reparsing lookup helper — `crates/rumoca-ir-solve/src/model.rs`
- DefId(0) is specified as root/global scope but the code treats it as the unresolved sentinel — `spec/SPEC_0001_DEFID.md`
- STRUCT-T02 alias elimination is catalogued as in-scope but is unimplemented in `rumoca-phase-dae` — `crates/rumoca-ir-dae/src/provenance.rs`
- render_index defaults a missing subscript count to 0 and silently skips subscripts — `crates/rumoca-phase-codegen/src/codegen/render_expr.rs`
- Propagation table omits typecheck and codegen; '0xx' notation contradicted by live codes — `spec/SPEC_0008_PHASE_ERRORS.md`
- Multi-rule sections use prose bullets instead of the required rules table, and table cells exceed the 15-word cap — `spec/SPEC_0018_TOOL_CONFIG.md`
- FUNC-022 and FUNC-032 requirement cells are 120-word implementation essays, violating SPEC_0000 §1's 15-word MUST — `spec/SPEC_0022_MLS_COMPILER_COMPLIANCE.md`
- Duplicate, diverging Cranelift SolveExecutionBackend adapters left side by side — `crates/rumoca-sim/src/native_execution.rs`
- §4a bans bespoke test-selection env vars that §4 mandates — `spec/SPEC_0025_PR_REVIEW_PROCESS.md`
- §5 requires a public-API diff tool that does not exist in the repository — `spec/SPEC_0025_PR_REVIEW_PROCESS.md`
- §3 'IR Crates Are Pure Data' enumerates only 4 of the 6 rumoca-ir-* crates — `spec/SPEC_0029_CRATE_BOUNDARIES.md`
- WASM core-build rule violated: rumoca-bind-wasm links GALEC lowering, lint, and LSP — `crates/rumoca-bind-wasm/Cargo.toml`
- Cited SPEC_0029 §12 does not contain the rule SPEC_0031 attributes to it — `spec/SPEC_0031_COMPILER_PHILOSOPHY.md`
- Determinism requirement ("bit-for-bit") is normative but has no cited or discoverable enforcement — `spec/SPEC_0031_COMPILER_PHILOSOPHY.md`
- Solve and connection scalar views carry no parent owner id, index tuple, or row id, violating §2 "Views carry provenance" — `crates/rumoca-ir-solve/src/lib.rs`
- Stop-the-line "actionable counterexample count" is untestable: nothing in the tree produces it — `spec/SPEC_0033_DEVELOPMENT_PROCESS.md`
- §6 derived Cargo/rayon budgets and §6a's fixed 4/4/4 mandate conflict, and §6a's rayon rationale is false — `spec/SPEC_0033_DEVELOPMENT_PROCESS.md`
- Checked Construction Scope claims branded function IDs that rumoca-ir-galec does not have — `spec/SPEC_0034_GALEC_EFMI_EXPORT.md`
- GAL-022's pinned profile string `efmi-1.0.0-beta-1` appears nowhere in the tree — `spec/SPEC_0034_GALEC_EFMI_EXPORT.md`
- §3's `(DefId, offset)` layout identity is documented as unimplemented in the layout code it names — `crates/rumoca-ir-solve/src/layout.rs`
- `TensorNodeMetadata` — the spec's designated element-type extension point — is write-only dead metadata in production — `crates/rumoca-ir-solve/src/tensor.rs`
- Summary/Scope call Flat and Solve construction future work while the body states them as present authority — `spec/SPEC_0036_VALID_BY_CONSTRUCTION_IR.md`
- `SolveProblem` children derive fieldwise `Deserialize` and expose public collections (60 derives in one file) — `crates/rumoca-ir-solve/src/model.rs`
- Kani manifest inventory guard is scoped to one crate of 56, so harnesses elsewhere escape silently — `crates/xtask/src/verify_cmd/kani.rs`
- Assurance Evidence Objective rules are stated in in-force normative voice but nothing in the tree implements them — `spec/SPEC_0037_FORMALLY_VERIFIED_COMPILER.md`
- Three overlapping roadmaps (V1-V7, W1-W3, Promotion Criteria) with contradictory ordering — `spec/SPEC_0037_FORMALLY_VERIFIED_COMPILER.md`
- No linked-versus-packaged parity evidence exists; packaged FMI CI compares only to the analytic solution — `crates/rumoca/tests/cli_target_fmi.rs`
- Conservative lattice top `Unknown` is never constructed; opaque ops never become Full — `crates/rumoca-eval-solve/src/sparsity.rs`
- Spec contradicts itself on whether derived patterns appear in canonical Solve wire data — `spec/SPEC_0039_PROOF_CARRYING_SPARSITY.md`
- `Affine` canonical representation and compact affine propagation do not exist — `spec/SPEC_0039_PROOF_CARRYING_SPARSITY.md`
- Certification receipts lack required fields and mislabel a conservative Full pattern — `crates/rumoca-phase-solve/src/ad.rs`
- No versioned execution policy; compressed AD is used regardless of color count — `crates/rumoca-eval-solve/src/tensor_policy.rs`
- STRUCT-T02 alias elimination is catalogued as in-scope and owned by rumoca-phase-dae but does not exist — `spec/SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md`
- Annex claims "no rules of its own" while being the sole home of ~50 MUST/MUST NOT obligations — `spec/SPEC_0040_IR_STAGE_CONTRACT_CATALOG.md`
- rumoca-test-msl shells out to `npm run build` for packages/rumoca-web, which §5 prohibits — `crates/rumoca-test-msl/src/web_assets.rs`
- Annex declares it "holds no rules of its own" but §5 and several cells state MUSTs absent from SPEC_0029 — `spec/SPEC_0041_CRATE_OWNERSHIP_CATALOG.md`
- D9 and D2 cite GAL-NNN rules whose SPEC_0034 text governs something else — `spec/SPEC_0042_GALEC_LANGUAGE_CATALOG.md`
- Annex introduces requirements with no governing rule in the parent, and contradicts its own "no rules of its own" claim — `spec/SPEC_0042_GALEC_LANGUAGE_CATALOG.md`
- §7 Flat aggregate catalog: `flat::Model::construct` and `FlatReferenceTarget` do not exist; flat::Model fields are all public — `spec/SPEC_0043_CONSTRUCTION_CATALOG.md`
- §6 "defaults ... are absent" contradicted by serde defaults in Solve IR, including a permissive default refresh role — `crates/rumoca-ir-solve/src/model.rs`
- Annex contradicts itself: "holds no rules of its own" yet §1b originates five MUST rules absent from SPEC_0036 — `spec/SPEC_0043_CONSTRUCTION_CATALOG.md`
- §3 FMI-LS-DAE assigns owners and asserts an aggregate constructor that do not exist in the tree — `spec/SPEC_0044_FMI_EXECUTION_CATALOG.md`
- §1 ME-BUF-001 makes the event-boundary and MeEventEntry surface normative evidence that SPEC_0038 orders removed — `spec/SPEC_0044_FMI_EXECUTION_CATALOG.md`
- Kani harness inventory is enforced only inside rumoca-solver; an ir-dae proof escapes the manifest — `crates/xtask/src/verify_cmd/kani.rs`
- Property-test fallback shares the proof harness's exact name and runs green in the default dev shell — `crates/rumoca-solver/src/verification/event_iteration.rs`
- SPEC_0039 mandates an Affine canonical pattern that PatternRepresentation does not have — `spec/SPEC_0039_PROOF_CARRYING_SPARSITY.md`
- SIM-010 claim spans three incompatible registries and the clocked-vs-ordinary classification is outside the proof — `crates/rumoca-solver/src/verification/event_iteration.rs`
- Finalized-DAE invariants are enforced by runtime panics in the view layer, not by types — `crates/rumoca-ir-dae/src/model/view.rs`
- Canonical Solve IR still encodes Boolean/Integer as f64 and hard-codes solver Y/P storage — `crates/rumoca-phase-codegen/src/templates/fmi3/model.c.jinja`
- eFMI C and FMI3 C templates share zero rendering vocabulary; the shared-IR consolidation win is not realized — `crates/rumoca-phase-codegen/src/templates/fmi3/model.c.jinja`
- Generated C and target.toml cite "GAL-024, two-track rule", but this branch rewrote GAL-024 to say something else — `crates/rumoca-phase-codegen/src/templates/embedded-c-galec/target.toml`
- Production C supports a strictly smaller GALEC subset than the `.alg` co-emitted in the same eFMU — `crates/rumoca-phase-codegen/src/templates/embedded-c-galec/model.c.jinja`
- A new C-family target cannot be added by template alone; only the conformance banner is overridable — `crates/rumoca-phase-codegen/src/templates/galec-production/model.c.jinja`
- Fold-kernel canonicalization deep-compares each new program against every previously emitted fold program — `crates/rumoca-exec-cranelift/src/emit.rs`
- PreparedLazyRowPlan::specialization re-scans and clones the whole row once per missing register — `crates/rumoca-eval-solve/src/lib.rs`
- Parsed-artifact in-memory cache deep-clones the whole StoredDefinition on every hit — `crates/rumoca-compile/src/parsed_artifact_cache.rs`
- SPEC_0034's module-layout diagram asserts two crate dependency edges that do not exist — `spec/SPEC_0034_GALEC_EFMI_EXPORT.md`
- SPEC_0035 forbids deployment width in canonical Solve IR; SPEC_0034/0040/0043 require the Solve root to carry it — `spec/SPEC_0035_COMPLEX_NUMERIC_TYPES.md`
- SPEC_0036 names phase newtypes and constructors that SPEC_0029 §4 and the code do not have — `spec/SPEC_0036_VALID_BY_CONSTRUCTION_IR.md`

### A.5 Low-severity leads, never adversarially verified (45) — treat as unconfirmed

- (spec-staleness) SPEC_0000 §3a's annex enumeration omits SPEC_0044, which the README lists as an annex — `spec/SPEC_0000_SPEC_GUIDELINES.md`
- (spec-incoherence) Enforcement test's documented budgets disagree with SPEC_0000 §3a's table and scope — `crates/rumoca/tests/spec_budget_test.rs`
- (spec-incoherence) The spec's most normative section is narrative prose, violating SPEC_0000 §1 — `spec/SPEC_0001_DEFID.md`
- (spec-staleness) Spec operation catalog and README line count are out of date — `spec/SPEC_0002_SCOPE_TREE.md`
- (spec-incoherence) SPEC_0007 rule tables violate SPEC_0000 §1's 15-word justification limit and prose prohibition — `spec/SPEC_0007_IR_PIPELINE.md`
- (design-risk) product_filter substitutes 1 for unknown length and non-integer dims in generated array sizes — `crates/rumoca-phase-codegen/src/codegen/mod.rs`
- (spec-staleness) Spec references a `rum` CLI and an errors.rs-only convention the tree does not have — `spec/SPEC_0008_PHASE_ERRORS.md`
- (spec-staleness) Spec's description of the env-var enforcement scan omits the test's blanket architecture_hardening exemption — `spec/SPEC_0018_TOOL_CONFIG.md`
- (spec-incoherence) Spec's [[viewer.frame]] position arity contradicts the validator's accepted range — `spec/SPEC_0018_TOOL_CONFIG.md`
- (spec-incoherence) Module Decomposition rule ordering ("submodules before imports") is untestable and universally unfollowed — `spec/SPEC_0021_CODE_COMPLEXITY.md`
- (spec-incoherence) SPEC_0022 omits the ## Summary and ## Specification sections SPEC_0000 requires of every spec — `spec/SPEC_0022_MLS_COMPILER_COMPLIANCE.md`
- (spec-staleness) SIM-010 carries a Kani proof but the MLS formalization manifest records it as unformalized — `verification/mls-formalization-coverage.json`
- (non-compliance) spec_budget_test omits §0 Branch Naming from the enforced PR-template section list — `crates/rumoca/tests/spec_budget_test.rs`
- (spec-staleness) spec/README.md lists SPEC_0025 under a different title than the spec itself — `spec/README.md`
- (spec-incoherence) §8 import-namespace rule is unenforceable guidance and its example names symbols ir-flat does not export — `spec/SPEC_0029_CRATE_BOUNDARIES.md`
- (spec-incoherence) Rule tables omit the SPEC_0000-required Owner/Where column — `spec/SPEC_0031_COMPILER_PHILOSOPHY.md`
- (spec-staleness) spec/README.md records SPEC_0032 as ~85 lines; the file is 191 lines — `spec/SPEC_0032_RANGE_PRESERVING_TENSORS.md`
- (spec-incoherence) §1's exhaustive list of compaction-refusal reasons omits the source-scoped array modifier refusal that the code implements — `spec/SPEC_0032_RANGE_PRESERVING_TENSORS.md`
- (spec-incoherence) ACCEPTED SPEC_0032 §6 delegates normative sparsity requirements to DRAFT SPEC_0039 — `spec/SPEC_0032_RANGE_PRESERVING_TENSORS.md`
- (spec-incoherence) Justification cells exceed SPEC_0000's 15-word cap in at least four rows — `spec/SPEC_0033_DEVELOPMENT_PROCESS.md`
- (spec-incoherence) ACCEPTED spec sits in SPEC_0000's "Too long" band (2486 words) with no section index — `spec/SPEC_0033_DEVELOPMENT_PROCESS.md`
- (spec-staleness) spec/README.md line count for SPEC_0033 is stale (~185 vs 173) — `spec/README.md`
- (design-risk) `PatternDerivation::ComplexLaneExpansion` is a dead enum variant with no producer — `crates/rumoca-ir-solve/src/structural_pattern.rs`
- (spec-incoherence) §2 states four normative requirements as narrative prose instead of the required rules table — `spec/SPEC_0035_COMPLEX_NUMERIC_TYPES.md`
- (spec-incoherence) §4's central REQUIRED equality is currently untestable and no verification section anchors it — `spec/SPEC_0035_COMPLEX_NUMERIC_TYPES.md`
- (spec-staleness) spec/README.md records SPEC_0035 as ~135 lines; the file is 188 lines — `spec/README.md`
- (spec-incoherence) Multi-rule sections are written as narrative prose, violating the mandatory rules-table shape — `spec/SPEC_0036_VALID_BY_CONSTRUCTION_IR.md`
- (spec-staleness) Stale in-tree links to SPEC_0037 still point at archive/deferred/, a path that no longer exists — `spec/SPEC_0036_VALID_BY_CONSTRUCTION_IR.md`
- (spec-incoherence) "Silently substitute a property-test fallback" is undefined, and fallbacks reuse the exact harness names — `spec/SPEC_0037_FORMALLY_VERIFIED_COMPILER.md`
- (non-compliance) Published Kani summary omits the profile, target, theorem, and trusted assumptions the spec requires of a verification report — `crates/xtask/src/verify_cmd/kani.rs`
- (spec-staleness) Phase-2 disposition table cites `MeStepCompletion`, a type that does not exist anywhere in the tree — `spec/SPEC_0038_UNIFIED_FMI_EXECUTION.md`
- (spec-incoherence) Spec violates SPEC_0000 code-reference and rules-table conventions; Verification claims unsupported — `spec/SPEC_0039_PROOF_CARRYING_SPARSITY.md`
- (spec-incoherence) §5 uses prohibited narrative prose and justification cells blow the 15-word cap — `spec/SPEC_0041_CRATE_OWNERSHIP_CATALOG.md`
- (spec-staleness) `balance_detail` is attributed to module `rumoca-phase-dae::balance` but lives at the crate root — `spec/SPEC_0041_CRATE_OWNERSHIP_CATALOG.md`
- (spec-staleness) `rumoca-signal-frame` owns `SignalFrame` yet appears in no catalog row; §5 assigns that ownership to rumoca-codec — `spec/SPEC_0041_CRATE_OWNERSHIP_CATALOG.md`
- (design-risk) Catalogued helper `project_algebraics_and_detect_changes` has no callers; a same-named `project_algebraics` is forked in fmi_me — `crates/rumoca-solver/src/fmi_me/kernel.rs`
- (spec-staleness) T2's `firstTick` first-sample mechanism is never generated by the GALEC lowering — `spec/SPEC_0042_GALEC_LANGUAGE_CATALOG.md`
- (spec-staleness) T4 prescribes a `(-1.0)*(expr)` rewrite that exists nowhere and omits the Integer form actually emitted — `spec/SPEC_0042_GALEC_LANGUAGE_CATALOG.md`
- (spec-incoherence) SPEC_0042 tables violate SPEC_0000 §1 column shape and the 15-word cell cap — `spec/SPEC_0042_GALEC_LANGUAGE_CATALOG.md`
- (non-compliance) §5 evidence row "Compile-fail tests cover private construction" has no implementation in the workspace — `spec/SPEC_0043_CONSTRUCTION_CATALOG.md`
- (spec-staleness) spec/README.md records SPEC_0043 as ~180 lines; the file is 254 lines — `spec/README.md`
- (spec-incoherence) §2 and §1 state normative rules as narrative prose and use non-conforming table columns, violating SPEC_0000 §1 — `spec/SPEC_0044_FMI_EXECUTION_CATALOG.md`
- (non-compliance) ME-AUTO-002 'only when every basis direction has a finite derivative' is bypassed when initialization requests termination — `crates/rumoca-solver-diffsol/src/lib.rs`
- (spec-incoherence) Broken SPEC_0037 archive link in SPEC_0036 and split MLS 3.6/3.7 pinning across specs — `spec/SPEC_0036_VALID_BY_CONSTRUCTION_IR.md`
- (spec-incoherence) SPEC_0034 Status/Conformance Ladder claim "eFMI Production Code export: Earned" for a path the same spec forbids — `spec/SPEC_0034_GALEC_EFMI_EXPORT.md`
