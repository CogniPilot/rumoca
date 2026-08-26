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
| D2 | C text ownership | MiniJinja over the checked `SolveAlgorithmBlock` executable view and Algorithm Code correlations (GAL-008/GAL-038). Decided, not yet implemented (pending: 2026-08-08 plan, M3-4). |
| D3 | eFMI package ownership | MiniJinja and `target.toml` over a closed target-neutral semantic view; no open `serde_json::Value` or Rust eFMI schema/context types. Templates own XML hierarchy, constants, references, filenames, and C mapping. Generic declared commands compute checksums, validate schemas, and assemble artifacts from rendered bytes. `rumoca-efmi` dissolves. |
| D4 | Provenance shape | Auxiliary artifact beside DAE with an equation-correspondence map (GAL-003); never "algorithms present ⇒ ignore f_z/f_m". |
| D5 | Manifest `renderer` extension | Rejected: covered by D1. |
| D6 | Clock strictness | XSD-strict (GAL-016): `constant`, seconds; Beta-1's `tunableParameter` examples are nonconforming. |
| D7 | Beta-1 grammar gaps | AST adopts `(min=,max=)`, the error-signal statement, input/output prefixes; emitter rejects `//` comments and unsigned exponents. |
| D8 | Runtime signal scope | Full signal machinery in AST + validator; typed escape-set construction accounts for NAN from Real relational and equality operators (T9). |
| D9 | Embedded-C sequencing | GAL-024: non-eFMI C export after checked projection; PC container after AC packaging. |
| D10 | XSD vendoring | Asset trees owned and named by the eFMI target directories; builtin discovery embeds arbitrary declared target assets recursively, while external targets resolve them relative to their own directory (GAL-008/GAL-023). |
| D11 | GALEC AST source spans | GALEC AST nodes carry `rumoca_core::Span` (the *foundation* crate, not an IR stage — GAL-001/GAL-010 intent holds). Parsed nodes span `.alg` bytes; generated nodes require typed source/generated provenance and the nearest responsible Modelica span. Production `Span::DUMMY` is prohibited. Spans are provenance, not identity (round-trip equality is span-insensitive). |
| D12 | C working-memory ownership | The generated header reports the checked slot-storage budget and exposes the concrete instance/scratch types for `sizeof`-based target accounting. Working slots are execution storage, not GALEC LogicalData, so Production Code manifests do not map them as block variables (GAL-039). |

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
| T8 | `absolute`/`ln`/`lg`/`roundDown`/`roundUp`/`imin`/`imax`, not `abs`/`log`/`log10`/`floor`/`ceil`/Integer min-max; `min`/`max` 2-arg scalar Real only; no array reductions; `atan2(y,x)`; `sign` returns Real; GALEC `integer()` truncates toward zero and can signal, so MLS floor conversion lowers as `integer(roundDown(x))`; `mod` → reserved `remainderDown`, not callable in Beta-1 | §3.2.6 mapping table is normative; unlowerable ops get stable `unsupported-feature` diagnostics |
| T9 | Relational operators on qNaN signal NAN and return false — comparisons are side-effecting | Escape sets account NAN for any Real comparison (slice 2, D8) |
| T10 | Signal checks catch (unset before the branch); `signals` clause must EXACTLY equal the computed escape set; testing an unsettable/caught signal is a compile error | Signal dataflow mandatory in the validator |
| T11 | No `while`/`break`/`return`/`String`/enumerations (reserved words only); loop bounds, dims, subscripts statically evaluable | No runtime-computed subscripts |
| T12 | `not` requires parenthesized argument; if-expressions self-parenthesized, mandatory `else`, no stateful calls inside; a stateful call has no sibling calls/state-refs in its expression | Checked construction + template rules |
| T13 | ASCII-letter-first identifiers, not keywords/reserved, no `__` prefix, no builtin/Appendix C collisions; quoted identifiers `'a.b[2].c'` (literal positive indices, no whitespace) are the traceability device | Prefer quoted identifiers (GAL-015) |
| T14 | Division by zero / Real overflow silently IEEE-754 (±inf); only `integer()` and the three linear-solver builtins signal | Don't invent error checks; don't omit the four that signal |

### 3. Rule Rationale (SPEC_0034 GAL-016, GAL-024)

**GAL-016.** GALEC has no `previous()`/`sample()` (T2); `pre(x)` becomes
protected state `'previous(x)'` committed at end of DoStep; the sample period is a
`constant` (seconds) named by the XSD-strict manifest `<Clock>` (§3.1.2).

**GAL-024.** The manifest LogicalData mapping (every interface variable and
BlockMethod), not C naming, is the conformance surface (ch. 5); PC-only is
non-conformant (§2.2).

### 4. Rule Rationale (SPEC_0034 GAL-040, the error-signal accumulation contract)

Real comparisons signal (T9), so a guard is not a pure test: it writes
`ErrorSignalStatus`. Any optimization that changes how often a guard runs is
therefore changing an eFMI-visible output unless something says otherwise.
GAL-040 is that something. It is a contract on the emitted artifact, not an
observation about today's templates, and each clause is mechanically enforced.

1. **Reset.** `ErrorSignalStatus` is assigned `0` exactly once, at the top of
   `Startup`, `Recalibrate` and `DoStep`, before any statement of that method.
   The word a consumer reads after a method returns records that invocation
   alone.
2. **Accumulation.** Every other write is `status |= <constant mask>`: the six
   predefined bits (§3.2.5 §1.6) OR-ed in by the emitted `signal` statement, by
   the Real comparison kernels, and by `integer`. `|=` with a constant mask is
   idempotent (`x | m | m == x | m`) and commutative (`x | a | b == x | b | a`),
   so within one method the final word depends on the SET of raising evaluations
   that ran, never on their count or their order.
3. **No read.** No emitted construct reads the word. `limit` saturation raises
   nothing and reads nothing.
4. **The one construct that breaks it.** A signal check (`if signal …`)
   *catches*: it clears the bits it tests (T10) and branches on them. That is
   both a read and a non-monotone write, so clauses 2 and 3 hold only between a
   method's reset and its first signal check. The Modelica → GALEC projection
   emits no signal check, so every rumoca-generated artifact is one region; a
   hand-written `.alg` need not be, which is why the permission is a proof
   obligation rather than a global assumption.

**Permitted rewrites.** Inside one region, and only there: evaluate a construct
fewer times than the source form does, or at a different point in the region,
provided it is still evaluated on exactly the executions where the source form
evaluated it at least once, and provided the bits it raises are the same on each
of those evaluations. Concretely this licenses hoisting a loop-invariant guard
out of a loop whose trip count is *proved* to be at least one. It does not
license hoisting out of a possibly-empty loop, out of a conditional, or across a
signal check: each can make a raise reachable that the source form never
reaches, which changes the word.

**Enforcement.** `rumoca_ir_galec::signal_effect` classifies every expression,
condition and statement as `Inert`, `AccumulateOr`, `Consume` or `Opaque` over
exhaustive matches, so a construct added to the AST cannot be silently treated as
safe; `RepeatableSignalEffect` is a branded token with a private constructor,
mintable only from a repeatable classification. An optimizer holds that token
plus its own invariance and non-empty-range proofs before it may rewrite; there
is no boolean predicate to bypass. Clauses 1 to 3 are claims about emitted C,
which no Rust type constrains, so a test scans both the C template source and
rendered C and accepts only a reset, an OR with a decimal-literal mask, a
declaration, or an address handed to a helper the same scan covers.

### 5. Variable Classification and Checked Construction Scope

**Variable classification (SPEC_0034 GAL-020, normative).**

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

**Checked construction scope (`rumoca-ir-galec`, per §3.2.2).**

| Analysis | Checks |
|----------|--------|
| Name | constructors reject keyword/reserved/`__`/builtin/Appendix C collisions and malformed quoted names |
| Type/shape | expressions carry exact type/extents; `/` is Real-only; `^`→Real; no implicit promotion; `else` mandatory |
| Static domain | dimensions, subscripts, and loop bounds carry checked constant-Integer proofs |
| Calls/effects | branded function IDs make unresolved/recursive calls impossible; body capabilities restrict writes and stateful calls |
| Signals | construction derives §3.2.5 escape sets, including NAN from Real comparisons; only settable signals testable; ≤16 user signals; method escape ⊆ predefined 6 |


## References

- [SPEC_0034](SPEC_0034_GALEC_EFMI_EXPORT.md) — owning GAL-NNN rules,
  conformance ladder, and testing requirements.
- Ground truth: **eFMI Standard 1.0.0 Beta 1** (CC-BY-SA text not reproduced,
  SPEC_0034 GAL-023): §3.1 manifest; §3.2 analyses/signals/builtins;
  App. C reserved names.
