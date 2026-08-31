# SPEC_0052: Embedded-C Competitor Matrix Catalog

## Status

REFERENCE

## Summary

Catalog of authenticated embedded-C competitor rows governed by
[SPEC_0033](SPEC_0033_DEVELOPMENT_PROCESS.md#6b-authenticated-embedded-c-competitor-matrix).

## Specification

The implementation states and row fields below are normative by reference
from SPEC_0033 Section 6b and add no independent process requirements.
`IMPLEMENTED` means that the row definition and executor exist; a claim still
requires a current authenticated receipt under SPEC_0033. It does not imply a
Rumoca win. `PENDING` rows are unavailable as evidence and cannot support
match-or-beat or dominance claims.

### 1. Current Row

#### ECM-003: lawful eFMI Production C

| Field | ECM-003 value |
|---|---|
| Implementation state | `PENDING`; the sole schema-9 row exists, but execution and claims refuse until its whole-scope evidence closes |
| Manifest row | `exp-mixed-efmu-production-c-casadi-3-7-2-cortex-m7`, schema 9 |
| Metric | `ExpMixed.GuestInstructions`; lower is better |
| Workload source | `infra/verification/embedded-head-to-head/exp-mixed/ExpMixedStep.mo`; SHA-256 `1fdbc228f306d1e8db906f9c3c908506e1d8d306768076ebd7b1fe46fc6f7023` |
| Semantic relation | Reviewed Modelica `ExpMixedPkg.exp_mixed(X0,l,r,B)` and independent CasADi SX `exp_mixed_full(X0,l,r,B)` transcriptions of the same named operation; eventual executable evidence is limited to the authenticated four-case closed cohort and does not prove universal equivalence |
| Compiler path | Untouched DAE → checked `AlgorithmCodePackage` → correlated `SolveAlgorithmBlock` → `efmu` Production C; no DAE/SolveProblem or Algorithm-Code-to-C shortcut |
| Rumoca setting | Shipped target `efmu`; `[arithmetic].real_matrix_multiply = "separate_mul_add_ascending_first_product"`; initial boundary profile `FinalEmissionPolicy { execution: Loop, calls: RetainCalls }`. A later `BoundedInline` promotion must replace `RetainCalls` here before row introduction with explicit finite `max_call_sites` and `max_expanded_instructions`; no scalarization policy exists, and benchmark-only policy substitution is forbidden |
| Compiler build authority | Explicit host target `x86_64-unknown-linux-gnu`; the complete authenticated Cargo.lock registry catalog supplies resolver manifests plus non-buildable Rust-source sentinels, enriched by declared metadata target entries, while only the host-filtered Rumoca-root-reachable subset supplies complete buildable package source; the host subset MUST be contained in the catalog and compiling any misclassified package MUST fail at its sentinel in the fresh offline build |
| Compiler feature closure | Authenticated offline compiler build includes `fmu-packaging` through the same frozen feature list used for Cargo resolution and compilation |
| Competitor | CasADi `3.7.2`; SX backend; generated C; profile `casadi372-full-f32`; CSE enabled; float-libm wrapper |
| Competitor source | `exp-mixed/casadi/gen_exp_mixed.py` SHA-256 `e99aea137a916c2c131061463493a5459af31d5ccab1f0fcb94bf6290addd092`; `float_libm_wrapper.c` SHA-256 `be48e3064059c085db37971675092dc43045c410d09cc9401469d278a9f02157`; generated C/H digests are re-authenticated from their exact raw bytes under this row |
| Cross-build setting | `gcc15-cortex-m7-o3-lto-no-fp-contract-hot-auto700-single700-exact-v3`, applied symmetrically; executable and runtime inputs belong to the manifest-pinned transitive Nix closure |
| Accounting scope | `guest-instructions-exclusive-markers-v1`: QEMU one-instruction translation-block trace entries strictly between exactly one `trace_begin` and `trace_end` |
| Correctness oracle | Successful execution and exact ten-word Binary32 equality for the ordered cases `current-closed-closed`, `tiny-all-series`, `middle-coefficient-series`, and `different-dense-coupling`. Each case owns ten lowercase eight-hex-digit Binary32 words in the manifest; the receipt re-authenticates those exact raw row bytes and observed words and never imports a predecessor-row receipt |
| Match-or-beat threshold | Delta `Rumoca - CasADi <= 0`; equality is a tie; a strict negative delta is a win |
| Evidence status | `PENDING-WHOLE-SCOPE-EVIDENCE`; accepted baseline, delta, outcome, and `rumoca_artifact_history` are absent. Missing or partial evidence cannot support a claim |
| Artifact history after promotion | One nonempty in-row `rumoca_artifact_history`; its last element is active. Every element binds the exact metric, one-line rationale, explicit canonical UTC-second generation instant, canonical lowercase-hyphenated UUID namespace seed, eFMU package SHA-256, the exact ordered 52-member schema-9 `efmu` target inventory with every member SHA-256, exact Production C/H archive paths and SHA-256s, the ordered six-edge checksum web (`.alg`→AC; C/H→PC; AC→PC; AC/PC→`__content.xml`) with producer SHA-1s, and an oracle-success receipt bound to the package and closed cohort. Every receipt and digest is recomputed from the opened archive and member raw bytes; no predecessor-row pin or receipt is borrowed |
| Transition authority | Schema 9 is the one-shot authority that deletes ECM-001 and installs pending ECM-003. No schema-8 reader, adapter, alias, fallback, measurement, or receipt survives. Within schema 9 the pending row is immutable; closing the evidence ladder requires a later explicit schema/SPEC migration |
| Runner identity | Immutable normalized profile owns benchmark protocol, not runner bytes. Exact runner byte closures form one nonempty append-only history whose active identity is derived as the last revision and must match the compiled-in runner closure. Runner replacement cannot authorize tool, comparator, protocol, oracle, threshold, row, baseline, or receipt changes |
| Bootstrap authority | Bootstrap requires complete non-shallow Git history and no manifest in any strict `HEAD` ancestor. A manifest first committed at `HEAD` must byte-match the checked current manifest; any older path history requires an authenticated predecessor containing the manifest |
| Pending result | Selection names `pending-whole-scope-evidence` and refuses before execution. Any bypassed completed evidence is `Rejected { observed evidence, findings }` without delta or outcome |
| Missing promotion evidence | Complete Production-C package emission; full exp-mixed capability-profile coverage; exact C/H extraction; cross-build and execution; raw-byte-authenticated package and oracle-success receipt; accepted baseline; and current closed receipt |
| Evidence entry point | `infra/verification/embedded-head-to-head.json`; `cargo xtask verify embedded-head-to-head`; authenticated summary `target/verification/embedded-head-to-head-summary.json` |

### 2. Pending Rows and Families

| Reserved row or family | State | Intended accounting or relation | Missing promotion evidence |
|---|---|---|---|
| `ExpMixed.LinkedTextSectionBytes` | `PENDING` | Complete linked ELF unique `.text`, using its own symmetric size profile | Authenticated size tool, exact profile, closed row, and baseline |
| `ExpMixed.LinkedStaticRamBytes` | `PENDING` | Complete linked ELF `.data + .bss`, with no operand-specific subtraction | Sealed ABI/storage setting, authenticated accounting, and baseline |
| `ExpMixed.StackBytes` | `PENDING` | Whole declared execution scope; generated-source `.su` alone is insufficient | Bound stack tool, library coverage, closed row, and baseline |
| `LawfulSymForce.*` | `PENDING` | A declared common arithmetic relation with pinned version, backend, and settings | Lawful workload relation, oracle, authenticated harness, and baseline |
| `SparseJacobian*` | `PENDING` | Pattern quality, coloring versus a structural lower bound, compressed-evaluation cost, and value parity across declared sparse families | Exact row identities, CasADi backend/settings, oracles, authenticated harnesses, and baselines |

#### ECM-002 candidate binding: linked text

| Field | Pending ECM-002 value |
|---|---|
| Implementation state | `PENDING`; unavailable as evidence until promotion |
| Metric | `ExpMixed.LinkedTextSectionBytes`; lower is better |
| Workload and semantic relation | `infra/verification/embedded-head-to-head/exp-mixed/ExpMixedStep.mo`, SHA-256 `1fdbc228f306d1e8db906f9c3c908506e1d8d306768076ebd7b1fe46fc6f7023`; reviewed Modelica `ExpMixedPkg.exp_mixed(X0,l,r,B)` and independent CasADi SX `exp_mixed_full(X0,l,r,B)` transcriptions; evidence is limited to the exact four-case, ten-Binary32-word cohort |
| Rumoca setting | Shipped `efmu` Production C; the exact linked-text compiler setting remains to be sealed by the authenticated receipt |
| Competitor | CasADi `3.7.2`; SX backend; generated C; profile `casadi372-full-f32`; CSE enabled; float-libm wrapper. Generator SHA-256 `e99aea137a916c2c131061463493a5459af31d5ccab1f0fcb94bf6290addd092`; wrapper SHA-256 `be48e3064059c085db37971675092dc43045c410d09cc9401469d278a9f02157`; any promotion re-authenticates exact raw generated C/H bytes |
| Correctness oracle | Successful execution and exact ten-word Binary32 equality for each ordered case: `current-closed-closed`, `tiny-all-series`, `middle-coefficient-series`, and `different-dense-coupling` |
| Cross-build setting | Symmetric `-Os -flto`; all remaining target, ABI, linker, and tool identities remain to be sealed |
| Accounting scope | Complete linked ELF unique `.text` section; no generated-function, symbol, operand, or library subtraction |
| Match-or-beat threshold | Delta `Rumoca - CasADi <= 0`; equality is a tie; a strict negative delta is a win |
| Missing promotion evidence | M5-M8 harness closure, authenticated size-tool and build closure, exact manifest row, frozen artifacts, accepted baseline, and current closed receipt |

Profile dependence is load-bearing: the pre-promotion discovery measured
Rumoca/CasADi `8552/7656` bytes (delta `+896`) under symmetric `-O3 -flto`,
but `6872/7376` bytes (delta `-504`) under symmetric `-Os -flto`. These
historical measurements are non-normative and are not evidence. Any eventual
claim is scoped only to ECM-002's authenticated profile and receipt.

Historical measurements, generated artifacts, or partial receipts for a
`PENDING` row do not promote it. Promotion requires the complete binding and
authenticated closed evidence required by SPEC_0033 Section 6b.

## References

- [SPEC_0033](SPEC_0033_DEVELOPMENT_PROCESS.md) — governing development and
  competitor-evidence contract.
- [SPEC_0025](SPEC_0025_PR_REVIEW_PROCESS.md) — review evidence and done
  criteria.
- [SPEC_0037](SPEC_0037_FORMALLY_VERIFIED_COMPILER.md) — trusted evidence and
  checker discipline.
