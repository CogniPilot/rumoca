# SPEC_0034: eFMI/GALEC Algorithm Code Export

## Status
DRAFT

Design contract; CLI and container integration pending.

## Summary
Rumoca exports eFMI Algorithm Code (GALEC `.alg` + XML manifests in an eFMU
container) as a target-language projection over canonical artifacts; GALEC
is a target language, never a canonical IR stage.

## Contents

Pipeline Placement, Module Layout, Rules, Resolved Decisions,
Conformance Ladder, Variable Classification, Validator Scope,
Language Traps (T1–T14), Testing Requirements, Non-Goals, References

## Pipeline Placement

```text
AST -> Flat -> DAE -> Solve                              canonical (SPEC_0007)
DAE (+ optional provenance)
  -> AlgorithmCodePackage                                projection
     = GALEC AST (rumoca-galec) + manifest model (rumoca-efmi)
  -> Model.alg + manifest.xml + __content.xml + schemas/ (+ typed C context)
```

## Module Layout and Dependency Direction

```text
crates/rumoca-galec         language: array-native ast (signals/limit/quoted ids),
                            print (T4-T7/T12), validate (six analyses), diagnostic
crates/rumoca-efmi          packaging: content + manifest models, xml serializer,
                            checksum (SHA-1 FIPS 180-4, raw bytes), ids (brace-wrapped UUIDs,
                            per-manifest uniqueness, strict UTC), container (.efmu, schemas/),
                            assets/efmi-schemas (vendored Beta-1 XSDs + BSD-3 LICENSE verbatim)
crates/rumoca-target-galec  projection: admissibility (untouched DAE) -> lower -> package -> c_context
```

```text
rumoca-compile
  -> rumoca-target-galec
      -> rumoca-galec        # language only
      -> rumoca-efmi         # packaging only
      -> rumoca-ir-dae, rumoca-ir-solve (if needed), rumoca-core
```

## Rules

| ID | Rule | Owner/Where | Why |
|----|------|-------------|-----|
| GAL-001 | GALEC MUST NOT appear as a peer of AST/Flat/DAE/Solve (`TemplateIr`, `TargetTemplateIr`, or manifest `ir` values) without amending SPEC_0007. | SPEC_0007 pipeline | Export languages stay out of the canonical pipeline. |
| GAL-002 | Projection returns a separate `AlgorithmCodePackage`; MUST NOT mutate canonical DAE, clear symbol tables, delete condition/event/clock metadata, or store snapshots. | `rumoca-target-galec` | One DAE contract for all consumers. |
| GAL-003 | No GALEC-only data in `rumoca_ir_dae::Dae`; algorithm structure rides as auxiliary provenance beside DAE (D4) unless SPEC_0007 is amended first. | `rumoca-ir-dae` | Backend fields rot the canonical schema. |
| GAL-004 | Generic capability checks and pre-projection admissibility run on untouched canonical artifacts; the package is re-validated after lowering; destructive preparation MUST NOT erase unsupported constructs before checks run. | `rumoca-target-galec` | Prevents vacuous gates. |
| GAL-005 | Parity source of truth is the §3.2.6 builtin catalog: accepted constructs lower and render to catalog names with exact signatures; Appendix C names rejected as identifiers and emitted calls. | `rumoca-target-galec` + `rumoca-galec` | Gate/codegen drift emits nonexistent functions (T8). |
| GAL-006 | Generic capability validation always runs; GALEC admissibility is additive; the target manifest declares source IR `dae` or `solve`, never `galec`. | `rumoca-compile` | No `ir`-keyed validator bypasses (SPEC_0029 §12). |
| GAL-007 | Unsupported features fail with stable `unsupported-feature:<feature_id>` diagnostics; errors are structured phase-local enums with stable codes and spans (SPEC_0008); no silent defaults. | all GALEC crates | Fail early; CI-aggregatable. |
| GAL-008 | Generated C text is owned by minijinja templates; the target crate emits typed serializable context only — never C strings in Rust. | `rumoca-phase-codegen` | SPEC_0029 §12 template ownership. |
| GAL-009 | `.alg` text is printed from the GALEC AST (recorded SPEC_0029 §12 exception), reaching emitted files as template context via the `target.toml` + minijinja pipeline. | `rumoca-galec` printer | T4–T7/T12 need typed printing. |
| GAL-010 | Three-module split: `rumoca-galec` (language; no IR/eFMI deps), `rumoca-efmi` (packaging/schemas; no IR/GALEC deps), `rumoca-target-galec` (projection; reached via `rumoca-compile`). | workspace layout | Packaging is representation-agnostic, reusable. |
| GAL-011 | GALEC output via `--target galec` / `--target embedded-c-galec`; `--emit` stays reserved for canonical IR inspection. | `rumoca` CLI | Preserves the CLI contract. |
| GAL-012 | Template CI renders GALEC targets against a dedicated smoke fixture; skipped targets MUST NOT be marked covered; generated C is compile-checked (Testing Requirements). | template CI (xtask) | False coverage hides broken output. |
| GAL-013 | Generated C/H/object outputs MUST NOT be committed except as intentional, small, documented fixtures. | CI | Repository hygiene. |
| GAL-014 | A parser, if added, parses into the GALEC AST only — never into DAE/Solve, never Modelica input. | `rumoca-galec` | Export language stays out of the front end. |
| GAL-015 | Mangling MUST be injective AND disjoint from keywords/reserved words/builtins/Appendix C names/`__` prefix space; quoted identifiers carry original scalarized Modelica names; round-trip tested incl. `'previous(x)'`. | `rumoca-target-galec` | Injectivity alone still emits illegal names (T13). |
| GAL-016 | Discrete-time semantics derive from structured compiler metadata, never string/unit/value heuristics; one static base period per block; dynamic clocks and multi-rate rejected pre-projection with stable diagnostics (counter-encoding: future option). | `rumoca-target-galec` | **Why** below. |
| GAL-017 | Block interface: exactly `Startup`/`Recalibrate`/`DoStep` (§3.1.3), stateful, parameter-free; I/O via `self.*`; Startup initializes ALL writable block variables, builtins only (control inputs are read-only in GALEC, environment-provided; their manifest `start` stays the documented default); Recalibrate emitted even when empty; all other functions reachable from DoStep; acyclic call graph. | `rumoca-target-galec` | §3.1.3, §3.2.3–3.2.4. |
| GAL-018 | Runtime error signaling is language machinery, not SPEC_0008 diagnostics: AST models signals/checks/closures/`limit`; validator implements the §3.2.5 escape-set dataflow (declared == computed); manifest carries per-method Signals and ErrorSignalStatus (32-bit: bits 0–5 predefined, 6–15 reserved-zero, ≤16 user signals). | `rumoca-galec` + `rumoca-efmi` | Conflating the two loses the language feature. |
| GAL-019 | Printer conformance: parenthesize every cross-precedence-class mix; no unary minus over non-references (rewrite as `0.0 - (e)` or `(-1.0)*(e)`); strict Real literal format; `/* */` comments only; mandatory `else`; parenthesized `not`; no re-association. | `rumoca-galec` printer | T4–T7, T12; evaluation order is normative. |
| GAL-020 | Variables classify per the Variable Classification table; independent parameters never constant-folded; dependents recomputed in Recalibrate (inline in Startup); every variable has `start`; dimensions are literal integers ≥ 1. | `rumoca-target-galec` + `rumoca-efmi` | §3.1.6 + repo parameters-stay-tunable policy. |
| GAL-021 | Claims follow the Conformance Ladder, machine-checked per rung; no placeholder checksums, ever; targets below a rung self-describe honestly. | `rumoca-efmi` | Ch. 2: wrong checksum ⇒ invalid eFMU. |
| GAL-022 | Version pinning: profile constant `Efmi_1_0_0_Beta_1`; profile string `efmi-1.0.0-beta-1`; container XSD `0.11.0` / AlgorithmCode `0.14.0` / ProductionCode `0.17.0`; `efmiVersion` fixed `"1.0.0"`. | `rumoca-efmi` | Beta-fixed constants change at 1.0.0 final. |
| GAL-023 | Vendored Beta-1 XSDs (BSD 3-Clause, Modelica Association) retain the LICENSE verbatim and are copied into every emitted `schemas/`; CC-BY-SA-4.0 standard text/grammar/examples NEVER copied into repo specs/fixtures beyond short attributed quotes; fixtures authored independently; no Modelica Association endorsement implied. | `rumoca-efmi` assets | License terms (verified). |
| GAL-024 | Embedded C is two-track: a non-eFMI export self-described as "GALEC-derived embedded C, NOT an eFMI Production Code container" may exist independently; the "eFMI Production Code" claim requires the ladder's full-container rung. | `rumoca-target-galec` | **Why** below. |
| GAL-025 | v1 scope rejections (continuous states, external functions, runtime events) are labeled "not yet supported by the Rumoca GALEC projection" — never "unsupported by eFMI". | `rumoca-target-galec` | §3.2.1(b), §1.3.3: eFMI expects discretized models. |
| GAL-026 | GALEC AST, manifest model, printer, and validator are array-native (dimensions, row-major `start`, for-loops, lifted builtins, indexed quoted identifiers); scalarized lowering is an implementation stage, never a language-layer assumption. | `rumoca-galec` + `rumoca-efmi` | Scalarization curtails Production Code optimization. |

**Why (GAL-016):** GALEC has no `previous()`/`sample()` constructs (T2);
`<Clock>` is XSD-strict (§3.1.2): `pre(x)` becomes protected state
`'previous(x)'` committed at end of DoStep; the sample period is a
`constant` (seconds) referenced by manifest `<Clock>`.

**Why (GAL-024):** The Production Code rung requires LogicalData mapping of
all interface variables and BlockMethods, a checksummed ManifestReference,
and a co-emitted Algorithm Code container (§2.2); the manifest mapping, not
C naming, is the conformance surface (ch. 5).

### Resolved Decisions (Phase 1 gates)

| # | Decision | Resolution |
|---|----------|------------|
| D1 | `.alg` text ownership | `rumoca-galec` printer per GAL-009. |
| D2 | C text ownership | minijinja templates over typed `c_template_context` per GAL-008. |
| D3 | eFMI XML ownership | Typed serializer in `rumoca-efmi`. minijinja REJECTED: XML escaping, UUID/id-uniqueness discipline, and SHA-1-over-exact-bytes require typed emission; packaging metadata, not target-language text (no SPEC_0029 exception needed). |
| D4 | Provenance shape | Auxiliary artifact beside DAE with an equation-correspondence map (GAL-003); never "algorithms present ⇒ ignore f_z/f_m"; kept out of serialized DAE. |
| D5 | Manifest `renderer` extension | Rejected: covered by D1. |
| D6 | Clock strictness | XSD-strict per GAL-016 (`constant`, seconds), diverging from Beta-1's nonconforming `tunableParameter` examples. |
| D7 | Beta-1 grammar gaps | AST adopts `(min=, max=)` attributes, the error-signal statement, and input/output block-entity prefixes; emitter rejects `//` comments and unsigned exponents regardless. |
| D8 | Slice-1 signal scope | Full signal machinery in AST + validator; slice-1 lowering emits Real relationals with empty escape sets (matching the validator's NAN deferral) and rejects constructs needing non-empty sets (Real→Integer narrowing, signaling solver builtins) with stable diagnostics; NAN accounting (T9) is slice 2. |
| D9 | Embedded-C sequencing | Per GAL-024: non-eFMI C export may land after the projection crate; Production Code container deferred until Algorithm Code packaging is solid. |
| D10 | XSD vendoring | Under `crates/rumoca-efmi/assets/efmi-schemas/` per GAL-023. |

### Conformance Ladder (GAL-021, GAL-024)

| Claim | Machine-checked requirement |
|-------|-----------------------------|
| "GALEC-derived text export" | `.alg` + `manifest.xml` render; honest self-description only |
| "eFMI Algorithm Code export" | Schema-valid eFMU container: `__content.xml` + `schemas/` + Algorithm Code container, correct SHA-1s, valid UUID/ids, strict UTC timestamps |
| "GALEC language conformance" | Above + round-trip parse of emitted `.alg` (or external compliance-checker gate) |
| "eFMI Production Code export" | Full Production Code container + co-emitted Algorithm Code container |

### Variable Classification (GAL-020, normative)

| Modelica (DAE) | GALEC declaration position | Manifest `blockCausality` |
|----------------|---------------------------|---------------------------|
| input | `input` before `protected` | `input` |
| output | `output` before `protected` | `output` |
| independent parameter | `parameter` before `protected` | `tunableParameter` |
| parameter-derived value | `parameter` after `protected` | `dependentParameter` |
| true constant | `constant` | `constant` |
| discrete state / pre-value | plain declaration (protected) | `state`, `start` mirroring Startup |

XSD enum is `dependentParameter` (not prose `calculatedParameter`); `start` is
row-major with scalar broadcast; method-local variables unlisted;
structurally-parametric array sizes rejected.

### Validator Scope (`rumoca-galec::validate`, per §3.2.2)

| Analysis | Checks |
|----------|--------|
| Name | keyword/reserved/`__`/builtin/Appendix C legality; quoted-identifier well-formedness; matching `end` names |
| Type | equal-typed binary operands; `/` Real-only; `^`→Real; no implicit promotion; mandatory `else` |
| Dimensionality | subscripts/dims/loop bounds are constant scalar Integer expressions |
| Termination | acyclic call graph; DoStep-reachability (dead functions illegal); Startup builtins-only |
| Side-effect | stateless functions never write state; stateful-call isolation; none in if-expressions |
| Signals | §3.2.5 escape-set dataflow: declared == computed; only settable signals testable; ≤16 user signals; method escape ⊆ predefined 6 |

### Language Traps (T1–T14)

| # | Trap | Emitter consequence |
|---|------|---------------------|
| T1 | Methods are parameter-free; I/O via `self.*` | Never emit parameters on block interface methods |
| T2 | `previous()`/`derivative()` are NOT operators — only quoted-name conventions for materialized state | `pre(x)` lowers to `'previous(x)'` state assigned at END of DoStep; `firstTick` for first-sample behavior |
| T3 | min/max ranges saturate (implicit `limit self` at method entry/return; Startup: return only); no assertions exist | Opposite of Modelica semantics; `limit` leaves qNaN as qNaN |
| T4 | Unary minus binds only to references: `a := -b^2` means `(-b)^2`; `-(b^2)`, `-f(x)` don't parse | Rewrite as `0.0 - (expr)` or `(-1.0)*(expr)` |
| T5 | No implicit Integer↔Real promotion; equal-typed operands; `/` Real-only; no `%`; `^` returns Real | Insert explicit `real()`/`integer()` casts during lowering |
| T6 | Cross-precedence-class mixes need explicit parentheses (`a^2*b` invalid); evaluation order normative (no re-association, even `a+b+c`) | Parenthesize every cross-class mix; preserve order end-to-end |
| T7 | Real literals: decimal places and exponent sign mandatory (`1.0e+5` ✓; `1e5`, `1.`, `.5`, `1.0e5` ✗); `//` comments invalid | Strict literal formatter |
| T8 | `absolute`/`ln`/`lg`/`roundDown`/`roundUp`/`imin`/`imax`, not `abs`/`log`/`log10`/`floor`/`ceil`/Integer min-max; `min`/`max` 2-arg scalar Real only; no array reductions; `atan2(y,x)`; `sign` returns Real; `integer()` truncates toward zero, can signal; `mod` → reserved `remainderDown`, not callable in Beta-1 | §3.2.6 mapping table is normative; unlowerable ops get stable `unsupported-feature` diagnostics |
| T9 | Relational operators on qNaN signal NAN and return false — comparisons are side-effecting | Escape sets account NAN for any Real comparison (slice 2, D8) |
| T10 | Signal checks catch (unset before the branch); `signals` clause must EXACTLY equal the computed escape set; testing an unsettable/caught signal is a compile error | Signal dataflow mandatory in the validator |
| T11 | No `while`/`break`/`return`/`String`/enumerations (reserved words only); loop bounds, dims, subscripts statically evaluable | No runtime-computed subscripts |
| T12 | `not` requires parenthesized argument; if-expressions self-parenthesized, mandatory `else`, no stateful calls inside; a stateful call has no sibling calls/state-refs in its expression | Printer + validator rules |
| T13 | ASCII-letter-first identifiers, not keywords/reserved, no `__` prefix, no builtin/Appendix C collisions; quoted identifiers `'a.b[2].c'` (literal positive indices, no whitespace) are the traceability device | Prefer quoted identifiers (GAL-015) |
| T14 | Division by zero / Real overflow silently IEEE-754 (±inf); only `integer()` and the three linear-solver builtins signal | Don't invent error checks; don't omit the four that signal |

## Testing Requirements

| Test | Enforces |
|------|----------|
| Golden `.alg` from an independently-authored fixed-sample discrete fixture | happy path; GAL-023 |
| Negative fixtures: continuous states, runtime events, external functions, dynamic clocks ⇒ stable diagnostics labeled per GAL-025 | GAL-007/016/025 |
| Gate-reachability regression through the public API | GAL-004 |
| Accept/lower/render parity anchored to the §3.2.6 catalog | GAL-005 |
| Reserved-name rejection; mangling injective + reserved-disjoint; quoted-identifier round-trip incl. `'previous(x)'` | GAL-015 |
| Type-inference failure ⇒ diagnostic, not default | GAL-007 |
| DoStep parameter-free; all writable (non-input) manifest variables assigned in Startup; `start` mirrors Startup; empty Recalibrate still emitted | GAL-017 |
| Manifest emit-then-XSD-validate; SHA-1 recomputation; per-manifest id uniqueness | GAL-021 |
| `rumoca-efmi` negative schema cases: missing elements, wrong child order, bad causality enum, malformed UUID/timestamp, dimension < 1 | GAL-021 |
| Full-container validation: all eFMU XMLs vs vendored XSDs, all checksums | GAL-021 |
| `--target galec` CLI smoke + real template-CI render | GAL-011/012 |
| Generated-C compile check (`cc -Wall -Werror`, temp dir) when C output exists | GAL-012/024 |

## Non-Goals

- GALEC does not replace DAE/Solve; export does not change Modelica semantics.
- No Behavioral Model containers (an eFMU is valid without one, ch. 4), no FMU
  embedding (`extra/org.efmi-standard`), no Binary Code representation.
- Parser deferred to the language-conformance rung or real external GALEC
  input; never Modelica input (GAL-014). The eventual parser is parol into
  the GALEC AST (round-trip print→parse→compare); `.alg` LSP follows
  (diagnostics keep spans).
- Embedded-C generation does not authorize target-specific rewrites in canonical DAE.

## References

- Ground truth: **eFMI Standard 1.0.0 Beta 1**, cited by section; CC-BY-SA
  text not reproduced (GAL-023): ch. 2 container; §3.1 manifest; §3.1.2 Clock; §3.1.3 methods; §3.2.2 analyses;
  §3.2.5 signals; §3.2.6 builtins; App. C reserved names; ch. 5 Production Code.
- [SPEC_0007](SPEC_0007_IR_PIPELINE.md), [SPEC_0008](SPEC_0008_PHASE_ERRORS.md),
  [SPEC_0029](SPEC_0029_CRATE_BOUNDARIES.md) (§12 GALEC printer exception).
