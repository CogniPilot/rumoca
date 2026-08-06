# SPEC_0034: eFMI/GALEC Algorithm Code Export

## Status
DRAFT

Design contract; `--target galec` (Algorithm Code) and `--target galec-production`
(Production Code) landed as schema-valid eFMU containers, and GALEC language
conformance is **Earned** (round-trip through `rumoca-phase-parse-galec`, plus
the `.alg` language server).

## Summary
Rumoca exports eFMI Algorithm Code and Production Code (GALEC `.alg`, C99, and
XML manifests in an eFMU container) as a target-language projection over
canonical artifacts; GALEC is never a canonical IR stage.

## Pipeline Placement

```text
AST -> Flat -> DAE -> Solve                    canonical (SPEC_0007)
DAE (+ optional provenance)
  -> rumoca-phase-galec
  -> AlgorithmCodePackage = checked GALEC + target-neutral export facts
  -> typed view -> target.toml + MiniJinja -> .alg/XML/C artifacts
```

## Module Layout and Dependency Direction

```text
rumoca-compile -> rumoca-phase-galec -> rumoca-ir-dae/solve
                                      -> rumoca-ir-galec
rumoca-phase-parse-galec -> rumoca-ir-galec
rumoca-eval-galec -> rumoca-ir-galec
rumoca-phase-codegen -> typed Algorithm Code view -> MiniJinja
rumoca -> generic artifact/checksum/container graph + vendored schemas
```

## Rules

| ID | Rule | Owner/Where | Why |
|----|------|-------------|-----|
| GAL-001 | GALEC is a selectable checked export IR, but is not a canonical AST/Flat/DAE/Solve peer in phase ordering, caches, or wire schemas. | SPEC_0007 pipeline | A template input is not a compiler stage. |
| GAL-002 | Projection returns a separate `AlgorithmCodePackage`; MUST NOT mutate canonical DAE, clear symbol tables, delete condition/event/clock metadata, or store snapshots. | `rumoca-phase-galec` | One DAE contract for all consumers. |
| GAL-003 | No GALEC-only data in `rumoca_ir_dae::Dae`; algorithm structure rides as auxiliary provenance beside DAE (D4) unless SPEC_0007 is amended first. | `rumoca-ir-dae` | Backend fields rot the canonical schema. |
| GAL-004 | Generic capability checks and pre-projection admissibility run on untouched canonical artifacts; checked construction closes the package after lowering; destructive preparation MUST NOT erase unsupported constructs before checks run. | `rumoca-phase-galec` | Prevents vacuous gates. |
| GAL-005 | Parity source of truth is the §3.2.6 builtin catalog: accepted constructs lower to semantic operations that templates render exactly; Appendix C names are rejected. | `rumoca-phase-galec` + `rumoca-ir-galec` | Gate/template drift emits nonexistent functions (T8). |
| GAL-006 | Generic capability validation always runs; GALEC admissibility is additive. Manifests select checked GALEC; construction completes before rendering. | `rumoca-compile` | No validator bypass or render-time lowering (SPEC_0029 §12). |
| GAL-007 | Unsupported features fail with stable `unsupported-feature:<feature_id>` diagnostics; errors are structured phase-local enums with stable codes and spans (SPEC_0008); no silent defaults. | `rumoca-phase-galec` | Fail early; CI-aggregatable. |
| GAL-008 | Generated C **and every eFMI package detail** are owned by MiniJinja templates and `target.toml`; Rust exposes only a closed target-neutral semantic view plus generic documented template/artifact commands. No eFMI schema/context model, dynamic-value transport, or C/XML fragment exists in Rust; dispatch fails closed. | target directories | SPEC_0029 §12. |
| GAL-009 | MiniJinja renders `.alg` from the checked semantic view. Rust exposes typed semantics and provenance; it MUST NOT print fragments. | `rumoca-phase-codegen` templates | Same boundary as every IR. |
| GAL-010 | `rumoca-ir-galec` owns only opaque checked Algorithm Code, target-neutral correlations, and constructors. `rumoca-phase-parse-galec` owns `.alg` parsing and private recoverable syntax state. `rumoca-phase-galec` owns DAE/Solve projection and admissibility. `rumoca-phase-codegen` owns only generic rendering, generic documented commands, and a target-neutral typed Algorithm Code view; it has no `src/galec/` or eFMI subsystem. Target directories own all GALEC/C/XML/package policy. No codegen or parser compatibility facade exists. | workspace layout | Enforce ownership. |
| GAL-011 | GALEC output via `--target galec` / `--target embedded-c-galec`; `--emit` stays reserved for canonical IR inspection. | `rumoca` CLI | Preserves the CLI contract. |
| GAL-012 | Template CI renders GALEC targets against a dedicated smoke fixture; skipped targets MUST NOT be marked covered; generated C is compile-checked (Testing Requirements). | template CI (xtask) | False coverage hides broken output. |
| GAL-013 | Generated C/H/object outputs MUST NOT be committed except as intentional, small, documented fixtures. | CI | Repository hygiene. |
| GAL-014 | The parser constructs a checked GALEC block only — never DAE/Solve, never Modelica input. Invalid documents use a private recoverable CST for diagnostics/navigation. | `rumoca-phase-parse-galec` | Keep syntax recovery out of checked IR. |
| GAL-015 | Checked names MUST be injective AND disjoint from keywords/reserved words/builtins/Appendix C names/`__` prefix space; quoted identifiers retain source identity. | `rumoca-ir-galec` + `rumoca-phase-galec` | Injectivity alone still emits illegal names (T13). |
| GAL-016 | Discrete-time semantics derive from structured compiler metadata, never string/unit/value heuristics; one static base period per block; dynamic clocks and multi-rate rejected pre-projection with stable diagnostics. | `rumoca-phase-galec` | **Why** below. |
| GAL-017 | Block interface: exactly `Startup`/`Recalibrate`/`DoStep` (§3.1.3), stateful, parameter-free; I/O via `self.*`; Startup initializes ALL writable block variables, builtins only (control inputs read-only); Recalibrate emitted even when empty; all other functions reachable from DoStep; acyclic call graph. | `rumoca-ir-galec` construction | §3.1.3–3.1.4. |
| GAL-018 | Runtime error signaling is language machinery, not SPEC_0008 diagnostics: checked GALEC data models signals/checks/closures/`limit`; construction enforces §3.2.5 escape-set dataflow; package data carries per-method Signals + ErrorSignalStatus. | `rumoca-ir-galec` | Not SPEC_0008 diagnostics. |
| GAL-019 | Template conformance: parenthesize every cross-precedence-class mix; no unary minus over non-references (T4); strict Real literal format; `/* */` comments only; mandatory `else`; parenthesized `not`; no re-association. | GALEC target templates | T4–T7, T12; evaluation order is normative. |
| GAL-020 | Variables classify per the Variable Classification table; independent parameters never constant-folded; dependents recomputed in Recalibrate (inline in Startup); every variable has `start`; dimensions are literal integers ≥ 1. | `rumoca-phase-galec` + checked construction | §3.1.6 + repo policy. |
| GAL-021 | Claims follow the Conformance Ladder, machine-checked per rung; no placeholder checksums, ever. `target.toml` declares the checksum/artifact graph and post-render schema gates; generic commands compute from exact bytes; CI independently recomputes from disk. Targets below a rung self-describe honestly. | target directories + generic artifact commands | Wrong checksum invalidates an eFMU. |
| GAL-022 | Version pinning: profile string `efmi-1.0.0-beta-1`; container XSD `0.11.0` / AlgorithmCode `0.14.0` / ProductionCode `0.17.0`; `efmiVersion` fixed `"1.0.0"`. These are literals declared by the owning target's `target.toml` and templates, never Rust constants or context fields. | target directories | Beta-fixed constants change at 1.0.0 final. |
| GAL-023 | Vendored Beta-1 XSDs (BSD 3-Clause) live under the owning target assets, retain the LICENSE verbatim, and are copied into every emitted `schemas/` by generic declared asset operations; CC-BY-SA-4.0 standard text/grammar/examples NEVER copied into repo specs/fixtures beyond short attributed quotes; no Modelica Association endorsement implied. | target directories | License terms. |
| GAL-024 | Embedded C is two-track: `embedded-c-galec` is a non-eFMI export; `galec-production` earns the Production Code rung. Neither fabricates a higher claim. | target templates | **Why** below. |
| GAL-025 | v1 scope rejections say "not yet supported by the Rumoca GALEC projection" — never "unsupported by eFMI". | `rumoca-phase-galec` | eFMI expects discretized models. |
| GAL-026 | Checked GALEC data, package data, semantic views, and templates are array-native; scalarized lowering is an implementation stage, never a language-layer assumption. | IR + phase + templates | Scalarization curtails optimization. |
| GAL-027 | `rumoca-eval-galec` defines explicit semantics for checked blocks: statement order, method transitions, signals, escape sets, `limit`, NaN comparisons, and conversions. It returns typed failures and has no lowering/codegen dependency. | `rumoca-eval-galec` | Independent proof/differential oracle. |
| GAL-028 | Each target declares its Integer domain. Generic range proofs must cover every emitted Integer operation; an unproved operation fails with provenance. The evaluator requires the same explicit domain. Wrapping, saturation, guessed values, and signed-C overflow are prohibited. | target config + semantic proof view | Beta-1 leaves arithmetic overflow undefined. |

**Why (GAL-016):** GALEC has no `previous()`/`sample()` (T2); `pre(x)` becomes
protected state `'previous(x)'` committed at end of DoStep; the sample period is a
`constant` (seconds) named by the XSD-strict manifest `<Clock>` (§3.1.2).

**Why (GAL-024):** The manifest LogicalData mapping (every interface variable and
BlockMethod), not C naming, is the conformance surface (ch. 5); PC-only is
non-conformant (§2.2).

### Resolved Decisions (Phase 1 gates)

Decisions `D1`–`D11` are recorded in
[SPEC_0042 §1](SPEC_0042_GALEC_LANGUAGE_CATALOG.md#1-resolved-decisions-spec_0034-phase-1-gates).
Reopening one requires amending this spec.

### Conformance Ladder (GAL-021, GAL-024)

| Claim | Machine-checked requirement | Status |
|-------|-----------------------------|--------|
| "GALEC-derived text export" | `.alg` + `manifest.xml` render; honest self-description only | Earned (`galec`; `embedded-c-galec` is the honest non-eFMI track) |
| "eFMI Algorithm Code export" | Schema-valid eFMU: `__content.xml` + `schemas/` + Algorithm Code container; correct SHA-1s, UUID/ids, strict UTC timestamps | Earned (`galec`) |
| "GALEC language conformance" | Above + round-trip parse of emitted `.alg`: render∘parse∘render idempotence | Earned (`galec`; `rumoca-phase-parse-galec` round-trip integration tests) |
| "eFMI Production Code export" | Schema-valid eFMU co-emitting Algorithm Code **and** Production Code (§2.2); PC `manifest.xml` xmllint-valid; LogicalData maps every AC variable + all three BlockMethods once; PC `ManifestReference@checksum` = SHA-1 of the AC manifest bytes, `@manifestRefId` = AC root UUID; whole SHA-1 web recomputed from written bytes, no placeholders | Earned (`galec-production`) |

### Variable Classification (GAL-020, normative)

| Modelica (DAE) | GALEC declaration position | Manifest `blockCausality` |
|----------------|---------------------------|---------------------------|
| input | `input` before `protected` | `input` |
| output | `output` before `protected` | `output` |
| independent parameter | `parameter` before `protected` | `tunableParameter` |
| parameter-derived value | `parameter` after `protected` | `dependentParameter` |
| true constant | `constant` | `constant` |
| discrete state / pre-value | plain declaration (protected) | `state`, `start` mirroring Startup |

XSD enum `dependentParameter` (not `calculatedParameter`); `start` row-major,
scalar broadcast; method-local variables unlisted; structurally-parametric
array sizes rejected.

### Checked Construction Scope (`rumoca-ir-galec`, per §3.2.2)

| Analysis | Checks |
|----------|--------|
| Name | constructors reject keyword/reserved/`__`/builtin/Appendix C collisions and malformed quoted names |
| Type/shape | expressions carry exact type/extents; `/` is Real-only; `^`→Real; no implicit promotion; `else` mandatory |
| Static domain | dimensions, subscripts, and loop bounds carry checked constant-Integer proofs |
| Calls/effects | branded function IDs make unresolved/recursive calls impossible; body capabilities restrict writes and stateful calls |
| Signals | construction derives §3.2.5 escape sets, including NAN from Real comparisons; only settable signals testable; ≤16 user signals; method escape ⊆ predefined 6 |

### Language Traps (T1–T14)

Traps `T1`–`T14` catalog the GALEC-versus-Modelica language differences and the
emitter consequence each one imposes:
[SPEC_0042 §2](SPEC_0042_GALEC_LANGUAGE_CATALOG.md#2-language-traps-t1t14).
GAL-005, GAL-015, GAL-019, and GAL-028 own the governing rules; the trap rows
are normative by reference from them.

## Testing Requirements

| Test | Enforces |
|------|----------|
| Golden `.alg` from an independently-authored discrete fixture | happy path; GAL-023 |
| Negative fixtures (continuous states, runtime events, external functions, dynamic clocks) ⇒ stable diagnostics | GAL-007/016/025 |
| Gate-reachability regression through the public API | GAL-004 |
| Accept/lower/render parity anchored to the §3.2.6 catalog | GAL-005 |
| Reserved-name rejection; mangling injective + reserved-disjoint; quoted-id round-trip | GAL-015 |
| Type-inference failure ⇒ diagnostic, not default | GAL-007 |
| DoStep parameter-free; writable variables assigned in Startup; `start` mirrors Startup; empty Recalibrate emitted | GAL-017 |
| Manifest XSD-validate + SHA-1 recompute + id uniqueness; full-container validation (all XMLs vs XSDs, all checksums); negative schema cases (missing element, wrong order, bad enum, malformed UUID/timestamp, dim < 1) | GAL-021 |
| `--target galec` CLI smoke + real template-CI render | GAL-011/012 |
| Generated-C compile check (`cc -Wall -Werror`, temp dir) when C output exists | GAL-012/024 |
| Differential execution: checked source semantics ↔ `rumoca-eval-galec` ↔ generated C/eFMI, including signal/error cases | GAL-027 |
| Target Integer boundary tests prove `minInteger`/`maxInteger`, conversion, arithmetic, and fail-closed unproved-overflow behavior | GAL-028 |

## Non-Goals

- GALEC does not replace DAE/Solve; export does not change Modelica semantics
  or authorize target-specific canonical-DAE rewrites.
- No Behavioral Model (ch. 4; an eFMU is valid without one), FMU embedding, or
  Binary Code representation.
- The parser never accepts Modelica input — GALEC only (GAL-014).

## References

- Ground truth: **eFMI Standard 1.0.0 Beta 1** (CC-BY-SA text not reproduced,
  GAL-023): ch. 2 container; §3.1 manifest; §3.2 analyses/signals/builtins;
  App. C reserved names; ch. 5 Production Code.
- [SPEC_0042](SPEC_0042_GALEC_LANGUAGE_CATALOG.md) — language traps and resolved
  decisions, normative by reference from the GAL-NNN rules above.
- [SPEC_0007](SPEC_0007_IR_PIPELINE.md), [SPEC_0008](SPEC_0008_PHASE_ERRORS.md),
  [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) (§12 template boundary).
