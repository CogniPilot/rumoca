# SPEC_0051: Jacobian Synthesis by Forward-Mode Differentiation

## Status
PROPOSED

## Summary
`jacobian(f(a, b), a)` expands, before flattening, into a generated Modelica
function whose forward-mode tangent is machine-checked against finite
differences; every construct outside the rules refuses by rule id.

## Motivation
- Hand-written Jacobians are the flight filter's largest correctness risk and
  its largest maintenance cost.
- A derivative only this compiler can produce is not verifiable; the expansion
  must be ordinary Modelica another tool elaborates.
- A differentiator that guesses is worse than one that refuses: a wrong
  Jacobian degrades an estimator silently.

**The dichotomy.** A construct is either differentiated correctly, with a
finite-difference row to say so, or refused at expansion time, identically in
both artifacts. There is no third state.

## Specification

### 1. Surface

| Rule | Owner/Where |
|---|---|
| JAC-S1: the recognized form is `jacobian(f(a1, …, ak), ai)`, already valid call syntax | `rumoca-phase-autodiff/src/sites.rs` |
| JAC-S2: `ai` must be one actual argument, a plain reference, appearing exactly once, so the differentiated formal is decidable by text | `sites.rs::read_call_site` |
| JAC-S3: the result is `Real[m, n]`: `n` from the differentiated input, `m` from the output, scalars included | `emit.rs::jacobian_wrapper` |
| JAC-S4: recognition is by name and shape after parsing, never in the grammar, so a file without `jacobian` parses bit-identically | `rumoca-phase-parse` untouched |
| JAC-S5: a file declaring a class named `jacobian` opts out; a declared name wins, as lookup says | `sites.rs::collect` |
| JAC-S6: a source is scanned, not parsed, to decide whether it can expand: `right_jacobian(` and `Lib.jacobian(` are other names | `lib.rs::may_expand` |
| JAC-S7: every actual must carry the rank its formal declares, read from declarations at the call site; a wider actual is one call per element (MLS 12.4.6), whose result is not the `Real[m, n]` of JAC-S3 | `actuals.rs::check_actual_ranks` |

**Why this spelling:** a declaration form (`Jacobian J(of = f, wrt = x)`) needs
a class restriction, a modifier vocabulary and a resolution rule for `of`. The
call form needs none of that.

### 2. Expansion and provenance

| Rule | Owner/Where |
|---|---|
| JAC-E1: expansion runs on the document text before any tree is resolved, so later phases and every target see plain Modelica | `rumoca-compile/src/parse.rs::expanded_document_source` |
| JAC-E2: the stored document text *is* the expanded text, because spans are offsets into the text carried | `session_impl.rs`, `session_impl_inputs.rs` |
| JAC-E3: `compile --emit-standard-modelica` writes that same expansion, so the portable and compiled programs are one artifact | `rumoca/src/standard_modelica.rs::run` |
| JAC-E4: every generated function carries a description naming its minting site | `emit.rs`, `lib.rs::provenance_text` |
| JAC-E5: generated functions are minted as siblings of the one they differentiate, so they resolve what their source resolved | `lib.rs::synthesize` |
| JAC-E6: a refusal is reported where that file's parse diagnostics go, with its rule id | `parse.rs::refusal_parse_error` |

### 3. Generated shape

For `f` with inputs `x[3], k` and output `y[2]`, expansion mints two functions
(actual `--emit-standard-modelica` output, elided at `...`):

```modelica
function f_ad_tangent "forward tangent of f, Probe.mo:17:18"
  input Real x[3]; input Real k; input Real x_ad[3]; input Real k_ad;
  output Real y_ad[2];
protected
  Real y[2]; Real t; Real t_ad;
algorithm
  t_ad := (k_ad) * (x[1]) + (k) * (x_ad[1]);
  t := k * x[1];
  ...
end f_ad_tangent;

function f_jacobian_x "jacobian(f(x, k), x) at Probe.mo:17:18"
  input Real x[3]; input Real k;
  output Real J_ad[2, 3];
algorithm
  J_ad[:, 1] := f_ad_tangent(x, k, {1.0, 0.0, 0.0}, 0.0*(k));
  ...
end f_jacobian_x;
```

| Rule | Owner/Where |
|---|---|
| JAC-G1: the tangent statement is emitted before the primal it pairs with, so the chain rule is stated at incoming values | `engine.rs::assignment` |
| JAC-G2: a structurally zero tangent is `0.0*(value)`, never `0`, because the value's own shape is the only right one | `engine.rs::zero_like` |
| JAC-G3: seeds are unrolled columns, not a loop | `emit.rs::jacobian_wrapper` |
| JAC-G4: `_ad` names the tangent companion, `J_ad` the wrapper output; both are collision-checked (JAC-R7) | `model.rs::TANGENT_SUFFIX`, `emit.rs::JACOBIAN_OUTPUT` |


### 4. Admission table

The admitted and refused surface is stated once, as machine-readable data, in
`rumoca-phase-autodiff/src/admission`: a construct, its operand shapes, the
probe that exercises it and one verdict. Section 6's gates generate their
probes from those rows, so the table and the battery are one statement.

| Verdict | Checked by |
|---|---|
| Differentiated | a finite-difference row in this compiler |
| DifferentiatedInOpenModelica | a finite-difference row in OpenModelica, plus this compiler failing on the row's own probe with the diagnostic printed |
| Refused | the same refusal, by rule id and to the column, in both artifacts at the same site |
| Untypable | OpenModelica declining to instantiate the primal, **and** this compiler's expander producing no expansion for it |

Every family below has rows. The statements, the array literal, the `if`
expression and the call are matched by AST shape rather than by name, so
`NAMED_FORMS` and `SHAPED_FORMS` enumerate them; that is what puts them inside
the closure check.

| Admitted | Rule | Shape restriction |
|---|---|---|
| Statements | assignment, `for`, `if`/`elseif`/`else`, `assert` (copied unchanged) | a `for` index must range over a range whose bounds carry no tangent |
| Declaration bindings | an assignment run before the algorithm, so its tangent is stated there, in declaration order, always | it may read only declarations before it, and a still variability may not be bound to a moving value (JAC-R9) |
| Operators | `+ - * /` and their elementwise forms, unary `+ - .+ .-` | the operand ranks must be a pair Modelica defines the operator at, by JAC-T1 (`engine.rs::arithmetic_rank`). Each rule is written in the operator it differentiates; an additive rule writes the still operand as a structural zero |
| Composite expressions | array literals, `if` expressions | none: a literal stacks elements under a new first dimension, an `if` expression keeps every branch's shape |
| Power | `^` | base and exponent must be rank 0 by JAC-T1 |
| Elementwise power | `.^` | the exponent must be rank 0 by JAC-T1; the base may have any shape |
| Builtins with a rule | `sin cos tan exp log log10 sqrt asin acos atan sinh cosh tanh atan2 transpose sum diagonal cross outerProduct cat fill` | the operands must give a result rank JAC-T1 states, per builtin (`builtins.rs::result_rank`) |
| Builtin with a branching rule | `abs` | the argument must be rank 0 by JAC-T1 |
| Builtins with a zero tangent | `zeros ones identity size` | a result rank JAC-T1 states is still required: a zero has a shape, and `size` is undefined on rank 0 |
| Calls | a function in scope whose tangent this engine can synthesize | the name must not also be a builtin; at the `jacobian` site every actual must carry its formal's rank (JAC-S7, JAC-R10) |
| Types | `Real` carries a tangent; `Integer`, `Boolean`, `String` are constant | a `constant` or `parameter` prefix holds still whatever the type says |

Conventions: at a conditional, and at `abs`, the derivative is that of the
branch the value takes; `abs` is not differentiable at zero, which is why its
rule is a scalar one. A name a function body does not declare is a
translation-time constant.

### 4a. Shape

Modelica spells two multiplications with adjacent syntax: `a*b` between two
vectors is the scalar product (MLS 10.6.3), `a .* b` is the elementwise one. A
chain rule written with the scalar product where the primal was elementwise
stays well typed whenever the surrounding expression absorbs the rank it
collapsed, so it simulates and answers a different question. Shape is part of
every rule, not a property checked afterwards.

| Rule | Owner/Where | Why |
|---|---|---|
| JAC-T1: an expression's rank is stated exactly or not at all, and a construct whose result rank is unstated refuses | `engine.rs::rank`, `arithmetic_rank`, `check_operand_shapes`, `builtin_derivative`, `call_derivative`, `builtins.rs::result_rank`, `actuals.rs` | A rank this engine cannot state is a refusal, never a guess |
| JAC-T2: a rule for a construct with an elementwise form is written in elementwise operators | `builtins.rs::unary`, `binary`, `engine.rs::power_derivative` | On rank-0 values they are the ordinary operators (MLS 10.6.5) |
| JAC-T3: a rule needing a constant of the value's own shape is rewritten to need none | `builtins.rs::unary` | Modelica has no rank-0 constant taking an array's shape; `cos(atan(v))^2`, `cos(asin(v))` and `sin(acos(v))` avoid one |
| JAC-T4: a rule this compiler has no runtime owner for is not stated | `builtins.rs::rule`, `CONSTANT` | The obvious rule would hold, but no finite-difference row could check it here |
| JAC-T5: a term that does not move is still written when its shape is part of the result | `engine.rs::sum_text`, `difference_text`, `zero_like` | Dropping it drops the shape it contributed (MLS 10.6.5) |
| JAC-T6: a call inside a differentiated body may vectorize; a call at the `jacobian` site may not | `engine.rs::user_call_derivative`, `actuals.rs` | Vectorization commutes with the tangent, whose formals vectorize the same way; the wrapper is the one place a shape is *declared* |

### 5. Refusal set

Each row refuses with `jacobian refusal [<id>] at <file>:<line>:<column>: …`,
in both artifacts by construction: both reach the expansion through one
`lib.rs::expand_source`, so neither can accept what the other declines. The
reason each row states is the shape or scope rule section 4 or 4a names.

| Id | Refuses |
|---|---|
| JAC-R1 | a call that is not the recognized form, or an argument that is not a plain reference appearing once |
| JAC-R2 | a callee not reachable from the call site in this file; a callee whose name is both declared here and a builtin; every other name carrying no rule |
| JAC-R3 | more than one output, a non-Real output, a non-literal or rank > 1 differentiated input, an output shape the wrapper cannot state |
| JAC-R4 | `while`, `when`, `break`, `return`, `reinit`, call statements other than `assert`, equation sections; a `for` index over anything but a range, or over a range whose bounds carry a tangent |
| JAC-R5 | `sign floor ceil integer div mod rem max min noEvent smooth pre edge change sample semiLinear homotopy der delay terminal initial product linspace cardinality`, and any unlisted expression form; any construct called outside the shape restrictions section 4 states for it; `^` or `.^` with an exponent carrying a tangent |
| JAC-R6 | an external function, a function without a synthesizable tangent, a recursive call chain, a mismatched argument count |
| JAC-R7 | a declared name colliding with a generated `_ad` companion or with `J_ad` |
| JAC-R8 | a port whose declared type is not `Real`, `Integer`, `Boolean` or `String` |
| JAC-R9 | a binding reading a declaration no earlier than its own; a `constant` or `parameter` binding that carries a tangent |
| JAC-R10 | at the `jacobian` site, an actual whose rank differs from its formal's, or whose rank the call site's declarations do not state |

### 6. Gates

The battery is generated from the admission table, and the table is closed
against a universe of three parts: the builtin name lists in `builtins.rs`,
the operator lists in `engine.rs`, and the form lists `NAMED_FORMS` and
`SHAPED_FORMS`. `admission::unstated_pairs` enumerates all three at every
shape of operands and reports any pair the table does not state exactly once.

**The scope of the claim.** The first two parts are read out of the engine, so
a rule added there without a row is reported without anyone remembering. The
third is hand-written, because its constructs are matched by AST shape and
have no name list to read. A form added to the engine's `match` arms and not
to that list is the one way a construct can still be admitted without a row,
and no gate proves otherwise: proving it needs a list of the AST variants the
engine handles, which is the very thing this is. Everything downstream of the
list is enforced: its contents are pinned by a test, so a form leaving the
closure is an edit a reviewer reads; every entry owes rows at every shape; and
every construct section 4 admits is on it today. Subject to that, an admitted
construct either has a generated finite-difference row and passes it, here or
in OpenModelica, or has no row and fails the closure check.

| Gate | Where |
|---|---|
| Every construct the engine reaches, stated exactly once at every shape | `rumoca-phase-autodiff/src/tests/admission.rs` |
| The hand-written half of the universe, pinned, so a form leaving the closure is an edit | `tests/admission.rs::the_forms_the_engine_matches_by_shape_are_pinned` |
| No probe reduces its result, so nothing absorbs a collapsed rank | `tests/admission.rs::no_probe_reduces_its_result` |
| Every admitted pair against central differences here, magnitude asserted non-zero | `jacobian_admission_battery.rs` |
| Every pair this compiler cannot run, against central differences under `omc`, its failure here demanded and its diagnostic printed | `jacobian_admission_battery.rs::pairs_this_compiler_cannot_run_…` |
| Every refused pair refused by both artifacts, citing the same rule | `jacobian_admission_battery.rs::refused_pairs_refuse_identically_in_both_artifacts` |
| Every untypable pair declined by `omc` *and* left unexpanded here | `jacobian_admission_battery.rs::untypable_pairs_are_not_modelica_programs` |
| Every refused named form at one `file:line:column` in both artifacts, vectorized call included | `jacobian_refused_forms.rs::both_artifacts_refuse_with_the_same_rule_and_site` |
| Every probe expansion elaborates under `omc` | `jacobian_admission_battery.rs::the_generated_battery_elaborates_under_omc` |
| Named functions over every statement form and the retrodiction chain, 3 scales | `jacobian_finite_difference.rs` |
| The named battery *simulates* under `omc` and agrees with differences taken there | `jacobian_standard_modelica.rs` |
| One test per refusal-set row and per shape rule | `rumoca-phase-autodiff/src/tests/refusals.rs`, `tests/shapes.rs`, `tests/bindings.rs` |
| The call boundary: a matching call keeps its stated shape, a vectorized one refuses | `rumoca-phase-autodiff/src/tests/calls.rs` |
| A refusal reaches the compiler user with its rule and line | `jacobian_refusal_diagnostic.rs` |

**Why elaboration is not enough.** A tangent that wrote `*` where the shape
called for `.*` elaborates cleanly, simulates cleanly, and reports a Jacobian
wrong by order-one amounts whenever the surrounding expression absorbs the
rank it collapsed. Only running the expansion against differences taken in the
same tool separates those cases.

## Rationale

**Why a second differentiator.** `rumoca-phase-structural` differentiates a
checked DAE graph with respect to *time*. This engine differentiates *AST
statements* with respect to a *named argument*, before any DAE exists. Neither
the input type, the independent variable nor the output is shared, so the two
are cross-tested rather than merged; the DAE-level rules keep their own oracle
in `forward_param_jacobian_test.rs`.

**Why text.** Emitting Modelica text and re-parsing it makes the compiled
program and the portable artifact the same bytes, and makes a generated
function reviewable. Building AST nodes directly produces spans belonging to
no source file (`EF017`).

## References
- Roadmap "Twelve-hour window two", `dev/2026-08-22-verifiability-roadmap.md`
- MLS §12 (functions), §12.4.6 (vectorized calls), §3.7.1 (mathematical
  functions), §8.3.2 (assert), §10.6 (array operators)
- SPEC_0008 (diagnostics), SPEC_0021 (complexity), SPEC_0029 (crate boundaries)
- Acceptance requires resolving the SPEC_0000 §3 active-spec cap, at its limit
  of 20; PROPOSED until that vote.
