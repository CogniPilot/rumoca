# SPEC_0042: GALEC Language and Decision Catalog

## Status
REFERENCE

## Summary

Lookup catalog of the GALEC language traps and the resolved eFMI export
decisions referenced by [SPEC_0034](SPEC_0034_GALEC_EFMI_EXPORT.md).

## How To Use This Catalog

This annex holds no rules of its own. Every row below is a SPEC_0034 obligation
or a recorded SPEC_0034 decision and is **normative by reference from
SPEC_0034**; the GAL-NNN rule in SPEC_0034 states the governing requirement and
links here. Cite rows by their catalog ID (`T4`, `D3`) as the surrounding code,
tests, and templates already do.

## Specification

### 1. Resolved Decisions (SPEC_0034 Phase 1 gates)

| # | Decision | Resolution |
|---|----------|------------|
| D1 | `.alg` text ownership | MiniJinja over the checked GALEC semantic view (GAL-009). |
| D2 | C text ownership | MiniJinja over the same target-neutral checked GALEC semantic view (GAL-008). |
| D3 | eFMI package ownership | MiniJinja and `target.toml` over a closed target-neutral semantic view; no open `serde_json::Value` or Rust eFMI schema/context types. Templates own XML hierarchy, constants, references, filenames, and C mapping. Generic declared commands compute checksums, validate schemas, and assemble artifacts from rendered bytes. `rumoca-efmi` dissolves. |
| D4 | Provenance shape | Auxiliary artifact beside DAE with an equation-correspondence map (GAL-003); never "algorithms present ⇒ ignore f_z/f_m". |
| D5 | Manifest `renderer` extension | Rejected: covered by D1. |
| D6 | Clock strictness | XSD-strict (GAL-016): `constant`, seconds; Beta-1's `tunableParameter` examples are nonconforming. |
| D7 | Beta-1 grammar gaps | AST adopts `(min=,max=)`, the error-signal statement, input/output prefixes; emitter rejects `//` comments and unsigned exponents. |
| D8 | Slice-1 signal scope | Full signal machinery in AST + validator; lowering emits Real relationals with empty escape sets and rejects constructs needing non-empty sets; NAN accounting (T9) is slice 2. |
| D9 | Embedded-C sequencing | GAL-024: non-eFMI C export after checked projection; PC container after AC packaging. |
| D10 | XSD vendoring | Asset trees owned and named by the eFMI target directories; builtin discovery embeds arbitrary declared target assets recursively, while external targets resolve them relative to their own directory (GAL-008/GAL-023). |
| D11 | GALEC AST source spans | GALEC AST nodes carry `rumoca_core::Span` (the *foundation* crate, not an IR stage — GAL-001/GAL-010 intent holds). Parsed nodes span `.alg` bytes; generated nodes require typed source/generated provenance and the nearest responsible Modelica span. Production `Span::DUMMY` is prohibited. Spans are provenance, not identity (round-trip equality is span-insensitive). |

### 2. Language Traps (T1–T14)

Each trap records an eFMI Standard 1.0.0 Beta 1 language property that differs
from Modelica, plus the consequence it imposes on the emitter. The consequence
column is the obligation; SPEC_0034 GAL-005/GAL-015/GAL-019/GAL-028 own the
governing rules.

| # | Trap | Emitter consequence |
|---|------|---------------------|
| T1 | Methods are parameter-free; I/O via `self.*` | Never emit parameters on block interface methods |
| T2 | `previous()`/`derivative()` are NOT operators — only quoted-name conventions for materialized state | `pre(x)` lowers to `'previous(x)'` state assigned at END of DoStep; `firstTick` for first-sample behavior |
| T3 | min/max ranges saturate (implicit `limit self` at method entry/return; Startup: return only); no assertions exist | Opposite of Modelica semantics; `limit` leaves qNaN as qNaN |
| T4 | Unary minus binds only to references: `a := -b^2` means `(-b)^2`; `-(b^2)`, `-f(x)` don't parse | Template renders the checked semantic rewrite as `0.0 - (expr)` or `(-1.0)*(expr)` |
| T5 | No implicit Integer↔Real promotion; equal-typed operands; `/` Real-only; no `%`; `^` returns Real | Insert explicit `real()`/`integer()` casts during lowering |
| T6 | Cross-precedence-class mixes need explicit parentheses (`a^2*b` invalid); evaluation order normative (no re-association, even `a+b+c`) | Parenthesize every cross-class mix; preserve order end-to-end |
| T7 | Real literals: decimal places and exponent sign mandatory (`1.0e+5` ✓; `1e5`, `1.`, `.5`, `1.0e5` ✗); `//` comments invalid | Strict literal formatter |
| T8 | `absolute`/`ln`/`lg`/`roundDown`/`roundUp`/`imin`/`imax`, not `abs`/`log`/`log10`/`floor`/`ceil`/Integer min-max; `min`/`max` 2-arg scalar Real only; no array reductions; `atan2(y,x)`; `sign` returns Real; `integer()` truncates toward zero, can signal; `mod` → reserved `remainderDown`, not callable in Beta-1 | §3.2.6 mapping table is normative; unlowerable ops get stable `unsupported-feature` diagnostics |
| T9 | Relational operators on qNaN signal NAN and return false — comparisons are side-effecting | Escape sets account NAN for any Real comparison (slice 2, D8) |
| T10 | Signal checks catch (unset before the branch); `signals` clause must EXACTLY equal the computed escape set; testing an unsettable/caught signal is a compile error | Signal dataflow mandatory in the validator |
| T11 | No `while`/`break`/`return`/`String`/enumerations (reserved words only); loop bounds, dims, subscripts statically evaluable | No runtime-computed subscripts |
| T12 | `not` requires parenthesized argument; if-expressions self-parenthesized, mandatory `else`, no stateful calls inside; a stateful call has no sibling calls/state-refs in its expression | Checked construction + template rules |
| T13 | ASCII-letter-first identifiers, not keywords/reserved, no `__` prefix, no builtin/Appendix C collisions; quoted identifiers `'a.b[2].c'` (literal positive indices, no whitespace) are the traceability device | Prefer quoted identifiers (GAL-015) |
| T14 | Division by zero / Real overflow silently IEEE-754 (±inf); only `integer()` and the three linear-solver builtins signal | Don't invent error checks; don't omit the four that signal |

## References

- [SPEC_0034](SPEC_0034_GALEC_EFMI_EXPORT.md) — owning GAL-NNN rules,
  conformance ladder, and testing requirements.
- Ground truth: **eFMI Standard 1.0.0 Beta 1** (CC-BY-SA text not reproduced,
  SPEC_0034 GAL-023): §3.1 manifest; §3.2 analyses/signals/builtins;
  App. C reserved names.
