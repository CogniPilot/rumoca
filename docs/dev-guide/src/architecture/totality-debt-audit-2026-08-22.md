# Totality debt audit: the five back-end crates

Base commit: `8b5a6f87c462` ("Join proven integer ranges across conditional arms").

SPEC_0008 sanctions `panic!` / `expect("invariant")` for internal contract
violations, and SPEC_0037 requires each phase to prove its target well-formed.
Every such call is therefore a live proof obligation whose justification is a
string. This audit counts them, classifies them by how they can be discharged,
and ranks the migrations that remove the most obligations per unit of work.

The goal is not to convert these into `Result`. A `Result` at every call site is
itself proof surface. The goal is to make the maximum number of them impossible
to write, and to hand a prover the smallest possible residue.

---

## 1. Baseline

### Counted forms

`.expect(`, `panic!(`, `unreachable!(`, `todo!(`, `unimplemented!(`, counted as
occurrences (a line with two `.expect(` counts twice), in code only. Comment
text, string-literal contents and character literals are removed before
matching: line comments, block comments including nested ones and ones spanning
several lines, cooked, raw and byte strings in their plain, `b` and `c` forms,
and character literals as distinct from the lifetimes that share their opening
quote.

Removing them is not only about refusing to count a form that a message merely
names. A brace, bracket or separator inside a comment or a literal is not
structure either, and letting one through corrupts the `#[cfg(test)]` tracking
rather than adding a stray match: an unbalanced `}` closes a test block early,
an unbalanced `{` extends it past its real end, and either way production lines
leave the count without a sound.

Comments and strings both span lines, so the stripper carries which one a line
ended inside into the next line. The 219 production files hold no block comment
at all, but they hold 145 lines that continue a multi-line string, and four of
those carry an unbalanced brace, all in
`crates/rumoca-ir-solve/src/typed_program/types.rs`, where JSON wire fixtures
are written as multi-line raw strings inside an inline `#[cfg(test)]` block.
Before the string state was carried, the `}` on the first of those lines ended
that block seventeen lines early and the rest of it was read as production
source. It changed no number only because those seventeen lines happen to spell
`.expect_err(` rather than `.expect(`. The same accident in the other direction
deletes a shipped obligation instead of adding a spurious one, which is why the
carry is not optional.

`.unwrap()` is counted separately. It carries the same obligation with the
message deleted, so pinning it separately stops "rewrite `.expect("x")` as
`.unwrap()`" from making the debt look smaller.

### Excluded source

Every `.rs` file under a crate's `src/` directory is counted. Two things and
only two remove a line from the count, and both are facts the compiler itself
acts on:

* inline `#[cfg(test)]` items,
* files reached transitively from a `#[cfg(test)] mod <name>;` declaration.

No part of a path is an exclusion: not the file name, not any directory above
it. Neither name keeps anything out of a release build. A
`src/lower/tests/mod.rs` declared with a plain `mod tests;` compiles into the
library exactly like its siblings, and rustc lists it in the dependency file of
the non-test target. Reading either name as proof would let a rename or a move
delete an obligation from the count while every line of it still ships, and the
directory form is the worse of the two, because it takes a whole subtree out at
once instead of a single file.

Dropping both path rules costs nothing here and closes that hole. All 49 files
these five crates keep under a `src/**/tests/` directory are reachable from a
`#[cfg(test)] mod tests;` declaration, and no crate has a `src/**/generated/`
directory at all, so the count with and without the path rules is identical at
878. `gate.rs` pins the redundancy in both forms, so a production module that
acquires a test-sounding file name, or that moves under a directory called
`tests`, is reported rather than silently dropped.

The `#[cfg(test)] mod` closure is not cosmetic either.
`crates/rumoca-phase-dae/src/construction/analysis/loop_compaction/mod.rs`
declares `#[cfg(test)] mod liveness;` and `#[cfg(test)] mod liveness_corpus;`.
Neither `liveness.rs` nor `liveness_corpus.rs` matches any test-name pattern,
yet neither reaches a release build; without the closure, both would contribute
obligations that do not exist.

The `#[cfg(test)] mod tests;` semicolon form is handled as an item that ends at
its semicolon. A scan that instead treats the declaration as the start of a
block discards the whole rest of the file: 24 of the 41 `#[cfg(test)] mod`
declarations in these five crates use the semicolon form, so that failure mode
silently blanks a large fraction of the tree.

### Counts

| crate | production files | asserted | `.unwrap()` |
|---|---:|---:|---:|
| rumoca-phase-solve | 41 | 304 | 0 |
| rumoca-phase-galec | 22 | 257 | 0 |
| rumoca-phase-dae | 77 | 235 | 2 |
| rumoca-ir-dae | 52 | 66 | 0 |
| rumoca-ir-solve | 27 | 16 | 0 |
| **total** | **219** | **878** | **2** |

By form: `.expect(` 677, `unreachable!(` 201, `panic!(` 0, `todo!(` 0,
`unimplemented!(` 0. Those three zeros are not luck:
`test_production_code_has_no_panic_todo_or_unimplemented` in
`crates/rumoca/tests/architecture_hardening_test/main.rs:996` already forbids
`panic!(`, `todo!(` and `unimplemented!(` in every non-test file under
`crates/`. They are counted here anyway, so the two gates cross-check each
other: if either scan ever disagrees with the other about what production source
is, one of them reports a nonzero count the other does not.

What that existing gate does not cover, and what this audit is about, is
`.expect(` and `unreachable!(`: 878 of them, all of the "an earlier stage
established this" shape, none of them forbidden by any existing rule.

The two production `.unwrap()` calls are both in
`crates/rumoca-phase-dae/src/construction/variable_construction/plan.rs:238` and
`:241`, in `first_internal_binding_dependency`, indexing `flat.variables` by an
index that the caller took from the same collection.

### Difference from the previous estimate

The working estimate was ~932 with rumoca-ir-solve at ~50. The recount puts
rumoca-ir-solve at 16. The gap is test code:
`crates/rumoca-ir-solve/src/structural_pattern.rs` has 18 matching lines, 17 of
which are below its `#[cfg(test)]` at line 3107. Similar trailing test modules
account for the rest. The per-crate figures in the table above are the ones the
gate pins.

### What the scan deliberately does not count

* `assert!`, `assert_eq!`, `debug_assert!`. SPEC_0008 sanctions `debug_assert!`
  for hot-loop invariants set up by construction, and `assert!` on a
  user-reachable condition is a diagnostic, not a totality claim.
* Panicking language constructs with no call form: `a[i]`, slicing, integer
  division, and arithmetic overflow in debug builds.
* A call form split across lines so that `.expect(` is not contiguous. No such
  site exists today in these crates (verified: no line in the five crates begins
  with a bare `expect(`).

These are honest gaps. Closing them raises the counts; it does not invalidate
the ranking below, because the ranked clusters are all `.expect(`/`unreachable!(`
shaped.

### Message-free obligations

Five sites assert with no justification at all. All five are class (a):

* `lower/scalar/operators.rs:20` and `:33` (rumoca-phase-solve):
  `UnaryOperator::Plus => unreachable!()`. Closed by W5's `LoweredUnaryOperator`.
* `lower/scalar/builtins.rs:67` (rumoca-phase-solve): a `_ => unreachable!()`
  inside a match already narrowed to `Zeros | Ones | Fill` by its enclosing arm.
  Closed by matching once instead of twice.
* `lower/scalar/arrays.rs:192` (rumoca-phase-solve): the code proves
  `indices.iter().all(TensorIndex::Constant)` and then re-matches each index.
  Closed by collecting into `Option<Vec<_>>` in one pass, so the proof and the
  use are the same expression.
* `model/function_loop_capability.rs:17` (rumoca-ir-dae): `FunctionLoop::domain`
  unwraps `self.body.domain: Option<DomainId>`, which is always `Some` for a
  loop capability. Closed by giving `FunctionLoop` its own non-optional
  `DomainId` at the point the capability is minted.

Even where the enclosing worklist item is large, these five are individually
small and each removes an obligation nobody can currently check.

---

## 2. Classification

Four classes:

* **(a) closable by a branded or witness type.** The fact is local and a type
  could carry it: an id that cannot outlive its arena, a non-empty collection, a
  refined enum, a scope guard that owns the state it guards.
* **(b) closable by a typed handover.** An earlier phase proved it; the handover
  type just does not carry the proof. The `PreparedStructuralAnalysis` pattern.
* **(c) untypeable in Rust, genuine prover obligation.** Numeric invariants,
  cross-collection coherence.
* **(d) genuinely fallible, belongs in a `Result` at a trust boundary.**

### Method

Stratified sample: every Nth site in file-sorted order, N chosen to give at
least 30 sites per large crate. rumoca-ir-solve is a complete census (16 of 16).
Each sampled site was read with its surrounding code before being tagged.

| crate | population | sample | stride | coverage |
|---|---:|---:|---:|---:|
| rumoca-phase-solve | 304 | 34 | 9 | 11% |
| rumoca-phase-galec | 257 | 33 | 8 | 13% |
| rumoca-phase-dae | 235 | 34 | 7 | 14% |
| rumoca-ir-dae | 66 | 33 | 2 | 50% |
| rumoca-ir-solve | 16 | 16 | 1 | 100% |

The sample is systematic, not random, so the proportions below are estimates
with no confidence interval attached. They are corroborated by two whole-
population counts that need no sampling: 293 of 878 sites carry the word
`resolves` in their message (the arena-accessor family, W1), and 42 carry
`dense` (the index round-trip family, W2).

### Result

| crate | (a) | (b) | (c) | (d) | sample |
|---|---:|---:|---:|---:|---:|
| rumoca-phase-solve | 21 (62%) | 8 (24%) | 5 (15%) | 0 | 34 |
| rumoca-phase-galec | 26 (79%) | 4 (12%) | 3 (9%) | 0 | 33 |
| rumoca-phase-dae | 6 (18%) | 25 (74%) | 3 (9%) | 0 | 34 |
| rumoca-ir-dae | 27 (82%) | 1 (3%) | 3 (9%) | 2 (6%) | 33 |
| rumoca-ir-solve | 11 (69%) | 1 (6%) | 4 (25%) | 0 | 16 |

Extrapolated to the full population:

| crate | (a) | (b) | (c) | (d) | total |
|---|---:|---:|---:|---:|---:|
| rumoca-phase-solve | ~188 | ~72 | ~45 | 0 | 304 |
| rumoca-phase-galec | ~203 | ~31 | ~23 | 0 | 257 |
| rumoca-phase-dae | ~41 | ~173 | ~21 | 0 | 235 |
| rumoca-ir-dae | ~54 | ~2 | ~6 | ~4 | 66 |
| rumoca-ir-solve | 11 | 1 | 4 | 0 | 16 |
| **total** | **~497 (57%)** | **~279 (32%)** | **~99 (11%)** | **~4 (0.5%)** | **878** |

The headline: about 89% of the debt is closable by construction, about 11% is a
genuine prover obligation, and essentially none of it is a missing `Result`.
The two phase crates that consume the DAE (solve, galec) are dominated by class
(a); the phase crate that builds it (dae) is dominated by class (b). That split
is what makes the worklist below sequenceable: W1/W2 and W3 touch disjoint
crates and can land in parallel.

### Sampled evidence

Class in brackets. Paths are relative to the crate's `src/`.

#### rumoca-phase-solve (34 sampled of 304)

| site | class | why |
|---|---|---|
| `layout.rs:218` | a | `ScalarType::Record` arm; layout needs a `Record`-free scalar type |
| `layout.rs:501` | a | dense B.1c owner index round-trip |
| `layout.rs:834` | a | `responsible_span()` is `Option` by design; caller holds a non-empty DAE |
| `lower/clocks.rs:344` | a | `scalar_count()` is `Option`; residual value type is scalar-capable |
| `lower/events/clock_partition.rs:197` | a | `view.variable(id)` accessor |
| `lower/events/structured.rs:57` | a | `branches().get(0)` on an owner with one branch |
| `lower/events.rs:203` | a | `view.delay_id(index)` inside `0..view.delay_count()` |
| `lower/events.rs:1030` | a | dense discrete-Real equation index |
| `lower/events.rs:1529` | a | `view.condition(id)` accessor |
| `lower/events.rs:1783` | a | `view.structured_root(id)` accessor |
| `lower/implicit_derivative.rs:137` | b | layout returns a general `ScalarSlot`; a state's slot is always `Y` |
| `lower/scalar/arrays.rs:291` | a | `Option` field used as a scope flag |
| `lower/scalar/builtins.rs:142` | b | `arguments.get(1)` on an arity-checked builtin |
| `lower/scalar/builtins.rs:267` | b | `let [_, columns] = dimensions` on a rank-2 result |
| `lower/scalar/builtins.rs:424` | b | `arguments.get(0)` on an arity-checked builtin |
| `lower/scalar/builtins.rs:528` | c | `u32` domain of a linspace extent |
| `lower/scalar/builtins.rs:597` | b | `arguments.get(0)` on an arity-checked builtin |
| `lower/scalar/conditions.rs:421` | a | `view.condition(id)` accessor |
| `lower/scalar/constants.rs:287` | b | conditional operand layout |
| `lower/scalar/functions.rs:1634` | a | `Option` field used as a scope flag |
| `lower/scalar/functions.rs:1960` | c | domain point count versus loop bound |
| `lower/scalar/functions.rs:3573` | a | `view.function_fold(id)` accessor |
| `lower/scalar/operators.rs:383` | b | conditional operand layout |
| `lower/scalar/selector.rs:241` | c | `checked_add` on a concatenation extent |
| `lower/scalar.rs:501` | a | `context_stack.pop()` on a stack with a parent |
| `lower/typed_functions/model_events.rs:388` | a | `view.expression(id)` accessor |
| `lower/typed_functions/tensor.rs:559` | b | `arguments.get(0)` on an arity-checked builtin |
| `lower.rs:287` | a | `view.domain(id)` accessor |
| `lower.rs:699` | a | `view.expression(id)` accessor |
| `lower.rs:964` | a | `view.expression(id)` accessor |
| `lower.rs:1157` | c | domain point ordinal in range |
| `lower.rs:1486` | a | `view.expression(id)` accessor |
| `lower.rs:2106` | c | domain point count versus loop bound |
| `lower.rs:2652` | a | `scalar_count()` on a scalar-capable value type |

#### rumoca-phase-galec (33 sampled of 257)

| site | class | why |
|---|---|---|
| `admissibility.rs:81` | a | `view.clock_id(index)` round-trip |
| `admissibility.rs:178` | a | `view.expression(id)` accessor |
| `lower/clock_schedule.rs:56` | c | `usize::try_from` on a clock index |
| `lower/clocked_assignments.rs:212` | a | `view.discrete_value_owner(id)` accessor |
| `lower/clocked_assignments.rs:841` | a | `view.condition(id)` accessor |
| `lower/clocked_assignments.rs:1270` | a | `view.condition(id)` accessor |
| `lower/expression_array_update.rs:45` | a | `view.expression(id)` accessor |
| `lower/expression_function_folds.rs:167` | c | fold domain point in range |
| `lower/expression_functions.rs:133` | a | `view.expression(id)` accessor |
| `lower/expression_functions.rs:305` | a | `view.expression(id)` accessor |
| `lower/expression_functions.rs:468` | a | `view.record_field(type, ordinal)` round-trip |
| `lower/expression_functions.rs:648` | a | `view.function(id)` accessor |
| `lower/expression_functions.rs:801` | a | `parameters().find(id)` linear search for an owner-local ordinal |
| `lower/expression_functions.rs:1086` | a | `view.expression(id)` accessor |
| `lower/expression_functions.rs:1411` | a | `view.function(id)` accessor |
| `lower/expression_functions.rs:1525` | a | `view.record_field(...)` round-trip |
| `lower/expression_helpers.rs:286` | a | `view.expression(id)` accessor |
| `lower/expression_helpers.rs:621` | a | `PureBuiltin::NoEvent` arm; lowering erases it before this match |
| `lower/expression_projection.rs:775` | a | `pop()` on a non-empty array constructor |
| `lower/expression_projection.rs:892` | a | `view.expression(id)` accessor |
| `lower/expression_projection.rs:1095` | b | `let [index] = indices` on a rank-1 result |
| `lower/expression_projection.rs:1161` | b | `arguments.get(1)` on an arity-checked builtin |
| `lower/pre_references.rs:26` | a | dense discrete-Real equation index |
| `lower/user_functions/indexed_update.rs:87` | a | `view.expression(id)` accessor |
| `lower/user_functions/indexed_update.rs:532` | b | peel result should carry only index subscripts |
| `lower/user_functions.rs:284` | a | `view.value_type(id)` accessor |
| `lower/user_functions.rs:540` | a | `values().find(id)` linear search for an owner-local ordinal |
| `lower/user_functions.rs:690` | a | `view.function(id)` accessor |
| `lower/user_functions.rs:892` | a | `get(len-1)` on non-empty operands |
| `lower/user_functions.rs:1197` | a | `view.function_fold(id)` accessor |
| `lower.rs:140` | a | `view.clock(id)` accessor |
| `lower.rs:1027` | c | side map keyed by variable id must be total |
| `lower.rs:1940` | b | conditional operand layout |

#### rumoca-phase-dae (34 sampled of 235)

| site | class | why |
|---|---|---|
| `construction/algorithm.rs:114` | b | analysis proved the assignment leaves; construction re-derives them |
| `construction/algorithm_lowering.rs:236` | b | `environment.function_calls` is `Option` on a path analysis always fills |
| `construction/algorithm_lowering.rs:654` | b | same |
| `construction/analysis/discrete_values.rs:558` | a | `targets[i].take()` needs a permutation witness |
| `construction/analysis/expression_validation.rs:419` | b | plan keyed by field access, then re-matched on the raw expression |
| `construction/analysis/function_bodies.rs:1170` | b | statement re-destructured after analysis proved its shape |
| `construction/analysis/function_bodies.rs:1640` | b | same |
| `construction/analysis/function_externals.rs:33` | b | `function.external` is `Option` on an external-only path |
| `construction/analysis/function_record_assemblies.rs:359` | b | target shape re-derived after validation |
| `construction/analysis/function_returns.rs:330` | a | `split_last()` on a block that ends in a return |
| `construction/analysis/loop_compaction/mod.rs:1256` | b | collapsed conditional re-matched on the raw statement |
| `construction/analysis/record_array_fields.rs:872` | c | part identity coherence across a rebuilt reference |
| `construction/discrete_values.rs:445` | b | `Vec<Option<T>>` handover after the fill step |
| `construction/expression/operators.rs:30` | a | `OpBinary::Empty`/`Assign` arms; a lowering-only operator enum |
| `construction/expression/temporal.rs:38` | b | analysis proved the pre-value role; not carried |
| `construction/expression.rs:1118` | b | `symbols.function_body` is `Option` on a function-body path |
| `construction/expression.rs:1702` | b | row shape re-derived after the predicate proved it |
| `construction/function_array_assembly.rs:76` | b | suffix-loop assignment re-destructured |
| `construction/function_body.rs:127` | b | unit runtime range re-destructured |
| `construction/function_body.rs:623` | b | statement/plan pair re-matched |
| `construction/function_body.rs:1552` | b | compaction result re-matched |
| `construction/function_construction.rs:257` | b | `value.type_def_id` is `Option` after analysis resolved it |
| `construction/function_construction.rs:468` | c | field-name lookup into the constructor input list |
| `construction/function_record_assembly.rs:143` | b | statement re-destructured after certification |
| `construction/function_shapes/mod.rs:276` | a | `IntegerRange` arm; a settled-value enum without it |
| `construction/model_algorithm.rs:51` | b | loop shape re-destructured |
| `construction/model_algorithm.rs:248` | a | non-declarative coordinate arms; a refined coordinate enum |
| `construction/model_events.rs:522` | b | reinit target re-matched |
| `construction/record_equation.rs:18` | b | residual shape re-destructured after certification |
| `construction/variable_construction.rs:133` | a | non-runtime `PlannedRole` arms; a runtime-role enum |
| `construction.rs:670` | b | statement/plan pair re-matched |
| `construction.rs:805` | b | statement/plan pair re-matched |
| `construction.rs:1520` | b | partition uniformity proved by analysis, not carried |
| `construction.rs:1718` | c | `checked_mul` over structured domain extents |

#### rumoca-ir-dae (33 sampled of 66)

| site | class | why |
|---|---|---|
| `clocks.rs:278` | a | arena index |
| `discrete_values.rs:181` | a | `(0..len).map(get(i).expect(..))` iterator |
| `expr_query.rs:99` | a | `dae.expression(id)` accessor |
| `expr_query.rs:202` | a | `dae.expression(id)` accessor |
| `expression/call_nodes.rs:62` | a | `pop()` on a one-result vector |
| `expression/record_nodes.rs:29` | a | record field ordinal in range |
| `expression/type_rules.rs:29` | a | quotient-only builtin subset |
| `expression/type_rules.rs:141` | a | quotient-only builtin subset |
| `expression/type_rules.rs:413` | a | `Homotopy` arm already returned |
| `expression/type_rules.rs:447` | a | array-constructor arms already returned |
| `expression/type_rules.rs:564` | a | compact-shaped builtin subset |
| `model/construction_checks.rs:112` | d | `u32` capacity of a decoded arena |
| `model/function_checks.rs:39` | a | construction build state as an `Option` |
| `model/function_conditionals.rs:95` | a | `last_mut()` after a non-empty append |
| `model/function_loop_capability.rs:17` | a | `Option<DomainId>` field that is always `Some` for this capability |
| `model/storage.rs:734` | c | `u32` capacity of a dense arena |
| `model/storage.rs:773` | c | forward-definition count versus arena length |
| `model/view.rs:384` | a | `(0..count).map(id(i).expect(..))` iterator |
| `model/view.rs:436` | a | same |
| `model/view.rs:513` | a | same |
| `model/view.rs:704` | a | `PeriodicClockId` does not carry the clock kind |
| `model/view.rs:859` | a | `scalar_count()` on a final value type |
| `model/view.rs:925` | c | `u32::try_from` on a parameter ordinal |
| `model/view.rs:955` | a | `entry.definition` is `Option` in frozen storage |
| `model/view.rs:1002` | a | same |
| `model/view.rs:1450` | a | operation family re-matched after the discriminant check |
| `model/view.rs:1549` | a | same |
| `model/view.rs:1623` | a | operand range into the arena |
| `model/view.rs:1676` | a | `(0..len).map(get(i).expect(..))` iterator |
| `model/view.rs:1771` | b | range-bound handover does not carry "literal Integer" |
| `model/wire/function_replay.rs:246` | d | decoded wire components |
| `model/wire.rs:1128` | a | wire node family re-matched after selection |
| `model.rs:1650` | a | construction build state as an `Option` |

#### rumoca-ir-solve (all 16)

| site | class | why |
|---|---|---|
| `lib.rs:2354` | a | Y/P-only slot subset |
| `lib.rs:2420` | a | Y/P-only slot subset |
| `linear_op.rs:2559` | a | random-operation subset |
| `linear_op.rs:2763` | c | `Reg::try_from` on `ordinal * stride` |
| `model.rs:1087` | a | fixed one-second lattice; a checked constant |
| `model.rs:1089` | a | fixed one-second schedule; a checked constant |
| `refresh.rs:156` | c | `usize::try_from` on a row index |
| `refresh.rs:166` | c | refresh selection index in bounds |
| `refresh.rs:1924` | c | `usize::try_from` on a row index |
| `shape_error.rs:548` | a | affine-only error subset |
| `shape_error.rs:609` | a | index-only error subset |
| `structural_pattern.rs:2807` | a | runtime-dependency operation subset |
| `tensor.rs:582` | a | tensor-node subset |
| `typed_program/call.rs:300` | a | the full `i64` range is a non-empty constant |
| `typed_program/program.rs:1107` | b | map body output count proved upstream |
| `typed_program/types.rs:285` | a | kind/type pair re-checked after validation |

---

## 3. Type-away-first worklist

Ranked by obligations removed per unit of work. W1 and W2 are sequential with
each other; W3 is independent and can land in parallel.

### W1. Total branded-id accessors on `DaeView`

**Reach.** 293 of 878 sites carry `resolves` in their message: 155 in
rumoca-phase-galec, 125 in rumoca-phase-solve, 11 in rumoca-ir-dae, 2 in
rumoca-phase-dae. The great majority are literally
`view.<thing>(id).expect("checked ... resolves")`; the remainder are the dense
index lookups that feed them, which W2 removes.

**Why it is already true.** `crates/rumoca-ir-dae/src/ids.rs` gives every id an
invariant brand `PhantomData<&'dae mut &'dae ()>` and keeps `from_raw`
`pub(crate)`, so no consumer can forge one. `Dae::inspect`
(`crates/rumoca-ir-dae/src/model.rs:630`) hands out `DaeView<'dae>` under a
`for<'dae>` closure, so an id from one inspection cannot reach another. The
storage behind the view is `FrozenStorage`: finalized, never appended to. The
only reason the accessor returns `Option` is that it reaches the arena through
`slice::get`.

**Design.** Add to `Dae::construct`'s finalization a check that every raw index
stored anywhere in the arena is less than the length of the arena it addresses,
and record that as the frozen-storage well-formedness predicate. Then change the
21 accessors of the form
`pub fn <thing>(self, id: <Thing>Id<'dae>) -> Option<<Thing>View<'dae>>`
(`crates/rumoca-ir-dae/src/model/view.rs:218`-`746`) to return the view
directly, indexing with `[]`. No call site keeps an `Option`.

**Residue.** One arena-well-formedness obligation, discharged once at
construction, on a `FrozenStorage` with no `Clone` and no `Deserialize`. That is
a bounded, self-contained predicate over `Vec` lengths and `u32` indices: a
natural Kani target and the exact shape that translates to Lean later.

**Do not migrate blindly.** Two of the 21 return legitimately absent data rather
than an arena entry: `effective_flat_type` (which ends in `.flatten()`, so the
absence is real) and `packed_scalar_count` (which delegates to
`packed_value_count`, whose `checked_extent_product` can genuinely fail). Those
keep their `Option`. The migration is per-accessor, and the two that stay
optional should say why in one line.

### W2. Arena iterators instead of dense index round-trips

**Reach.** 42 sites carry `dense` in their message (24 solve, 12 galec, 6
ir-dae), plus the `(0..count).map(|i| self.get(i).expect(...))` iterator bodies
inside ir-dae itself (`model/view.rs:384`, `:436`, `:513`, `:1676`,
`discrete_values.rs:181`).

**Shape.** Every one is a caller writing
`for index in 0..view.x_count() { let id = view.x_id(index).expect("dense ..."); let x = view.x(id).expect("checked ..."); }`,
which pays two obligations to walk a `Vec`.

**Design.** For each `x_count()` / `x_id(index) -> Option<XId>` pair, add
`fn xs(self) -> impl ExactSizeIterator<Item = XView<'dae>>` built from
`self.dae.storage.xs.iter().enumerate()`, minting the id from the enumerate
index inside the arena. Callers become `for x in view.xs()`. The index never
leaves ir-dae, so there is nothing left to assert.

**Order.** Land after W1: the iterator body is written in terms of the total
accessor, so doing W1 first makes W2 a mechanical rewrite of the loop headers.

### W3. Fuse each analysis plan with the statement it was derived from

**Reach.** rumoca-phase-dae's class (b) share, about 173 sites. The core of it
is the `match (statement, plan)` pairing in construction:
`construction.rs:670`, `construction.rs:805`, `function_body.rs:623`,
`analysis/function_bodies.rs:1170` and `:1640`,
`analysis/function_record_assemblies.rs:359`, `record_equation.rs:18`,
`model_algorithm.rs:51` and `:248`, `model_events.rs:522`,
`function_record_assembly.rs:143`, `analysis/loop_compaction/mod.rs:1256`.

**Shape.** `FunctionStatementPlan`
(`crates/rumoca-phase-dae/src/construction/analysis.rs:239`) describes a
statement that analysis already destructured and validated. Construction then
walks the raw `rumoca_core::Statement` tree *in parallel* with the plan tree and
re-destructures each node, asserting that the two agree. Every
`unreachable!("analysis proves ...")` and `unreachable!("... plans remain
aligned")` is a guard on that desynchronisation.

**Design.** Give each plan variant ownership of the fragment it was derived
from, so there is only one tree to walk:

* `Assignment(FunctionAssignmentPlan)` becomes
  `Assignment { value: Expression, span: Span, plan: FunctionAssignmentPlan }`,
  carrying the assignment's own value rather than pointing back at a statement
  that construction must re-match.
* `If { branches, fallback, targets }` carries its `Vec<CondBlock>` and its
  else-part directly instead of leaving construction to re-read them from the
  source statement.
* `For { domain, binder_spans, lowering, statements, source_depth }` carries the
  loop's index expressions rather than re-deriving them
  (`function_body.rs:127`, `function_array_assembly.rs:76`).

Then `lower_function_statement(construction, symbols, body, plan)` takes the
plan alone. The `(statement, plan)` tuple disappears, and with it the whole
class of "the two trees agree" assertions.

**Residue.** No proof obligation is left over: this is a refactor of an owned
type, and the analysis pass already destructures every fragment it would now
carry. The cost is that the plan owns those fragments instead of borrowing the
source tree, so measure the construction-phase allocation on the RDD2 bench
before and after rather than assuming it is free.

**Cost.** The largest of the five. `FunctionStatementPlan` has fifteen
variants and construction's walker is spread across `construction.rs`,
`construction/function_body.rs` and `construction/analysis/*`. Worth doing as
one variant at a time, each landing with the `unreachable!` it removes.

### W4. Arity-carrying operands for DAE expression nodes

**Reach.** 99 sites across all five crates (56 solve, 29 galec, 8 dae, 5 ir-dae,
1 ir-solve) of the form `arguments.get(N).expect("checked <builtin> <role>")` or
`operands.get(ordinal).expect("checked conditional ...")`.

**Why it is already true.** ir-dae validates builtin arity at construction:
`expect_arity(arguments, 2, at)?` in
`crates/rumoca-ir-dae/src/expression/type_rules.rs:410`. The check happens, then
its result is thrown away and the operands are handed on as a flat slice view
(`ExpressionOperands<'dae>`).

**Design.** Two additions on `ExpressionOperands<'dae>`:

* `fn exact<const N: usize>(self) -> [ExprId<'dae>; N]`, mintable only from the
  construction path that ran `expect_arity` with the same `N`. Fixed-arity
  builtins (`atan2`, `smooth`, `skew`, `cross`, quotients) then destructure
  directly: `let [lhs, rhs] = arguments.exact();`.
* A `ConditionalOperands<'dae>` view with `conditions()`, `values()` and
  `fallback() -> ExprId<'dae>`, replacing the `get(ordinal)` and
  `get(len - 1)` arithmetic at `lower/scalar/constants.rs:287`,
  `lower/scalar/operators.rs:383`, `galec/lower/user_functions.rs:892` and
  `galec/lower.rs:1940`. The conditional node's operand layout is fixed at
  construction; the view is where that layout should be spoken.

**Residue.** One arity obligation per builtin, at construction, where the check
already lives.

### W5. Refined enums at the lowering boundaries

**Reach.** The `_ => unreachable!("only X uses this")` family: 11 of
rumoca-ir-solve's 16 sites, five in `ir-dae/expression/type_rules.rs`, two in
`ir-dae/model/view.rs`, plus `solve/layout.rs:218`,
`solve/lower/scalar/operators.rs:20` and `:33`,
`galec/lower/expression_helpers.rs:621`,
`dae/construction/expression/operators.rs:30`,
`dae/construction/variable_construction.rs:133`,
`dae/construction/function_shapes/mod.rs:276`. Roughly 60 to 80 sites, including
two of the five message-free obligations.

**Design.** Each is a plain enum plus one `TryFrom` at the boundary that already
checks. Five concrete splits, in descending value:

* `WritableSlot`, the `ScalarSlot` subset that an assignment target can name:
  `Y { .. }` and `P { .. }`, without `Time` and `Constant(_)`. Every assignment
  target type in `rumoca-ir-solve` uses it instead of the full `ScalarSlot`,
  which kills `ir-solve/lib.rs:2354` and `:2420` and the ten `Y slot` and
  `Y/P slot` assertions in rumoca-phase-solve, including
  `solve/lower/implicit_derivative.rs:137`.
* `PrimitiveScalarType` (Boolean, Integer, Real, Enumeration, String; no
  `Record`) as the Solve layout's scalar type, since the layout stores only
  primitive rectangular values. Kills `solve/layout.rs:218`.
* `LoweredUnaryOperator` without `Plus`, which lowering erases before the match.
  Kills the two message-free `unreachable!()` at
  `solve/lower/scalar/operators.rs:20` and `:33`.
* `NamedGalecBuiltin` without `Smooth`, `NoEvent` and `Homotopy`, all three of
  which GALEC lowering rewrites to a value operand before reaching the name
  mapping. Kills the three `unreachable!` arms at
  `galec/lower/expression_helpers.rs:620`-`:622`.
* `RuntimeVariableRole` as `PlannedRole`'s runtime subset. Kills
  `dae/construction/variable_construction.rs:133`.

**Why start here.** Cheapest per site, no cross-crate type surgery, and each
split is independently reviewable. A good first landing that also proves the
gate ratchets down.

### Landed so far

The table in section 1 is the measurement at this document's base commit and
stays there. This subsection is the ledger of what has since been converted,
and `gate.rs` holds the matching ceilings.

| crate | audited | before | now | removed |
|---|---:|---:|---:|---:|
| rumoca-phase-solve | 304 | 303 | 272 | 31 |
| rumoca-phase-galec | 257 | 257 | 240 | 17 |
| rumoca-phase-dae | 235 | 235 | 232 | 3 |
| rumoca-ir-dae | 66 | 66 | 60 | 6 |
| rumoca-ir-solve | 16 | 16 | 15 | 1 |
| **total** | **878** | **877** | **819** | **58** |

The "before" column is the scan immediately ahead of this landing;
rumoca-phase-solve had already shed one site between the audit's base commit
and it.

* **W2, complete for the single-entry families.** `DaeView` now walks an arena
  instead of being indexed into it:
  `crates/rumoca-ir-dae/src/model/view/arena_walks.rs` holds the walk and the
  single-entry accessor for every family whose view is built from one entry
  (model-event transactions, relations, structured roots, conditions, roots,
  time events, event actions, clocks, clock ownerships, previous values,
  terminals, discrete Real equations, initialization-instant discrete values).
  A step mints the identity from the same slice element that yields the entry,
  so the paired `dense … identity` and `… resolves` assertions have nowhere
  left to stand. Every `0..x_count()` loop over those families in the five
  crates now reads `for (id, x) in view.xs()`.

  Clock ownership joined that set through a typed handover:
  `ClockOwnershipEntry` carries the `ClockedVariableKind` that `Clocks::own`
  checked, so the view no longer re-reads the variable arena and no longer has
  a role arm it cannot reach. The wire form omits the field and replay
  reproduces it through the same checked operation.

  What W2 does not reach is the families whose view crosses into a second
  arena: delays, B.1c discrete-value owners, variables, expressions, and the
  continuous and initialization owners. Those are W1's arena-well-formedness
  obligation, not a loop shape, and their `0..x_count()` loops are still
  written by index.

* **W5, four of the five splits.** `RuntimeVariableRole` is `PlannedRole`'s
  runtime subset, converted at the one filter in `insert_variable_identities`
  that already skipped the other four roles, so both role mappings in
  `construction/variable_construction.rs` are exhaustive.
  `LoweredUnaryOperator` is `UnaryOperator` without the identity `Plus` that
  scalar lowering erases. `SolveIntegerDomain::FULL` replaces a checked
  construction of the whole `i64` range with a constant.
  `FunctionLoop` carries its own non-optional `DomainId` rather than unwrapping
  the body's option.

  `WritableSlot` is not attempted. It is the largest of the five and reaches
  the wire form of every Solve assignment target, so it wants an item of its
  own rather than a corner of this one. `PrimitiveScalarType` is not attempted
  either: `layout.rs`'s `ScalarType::Record` arm asserts a fact no earlier
  phase visibly establishes, so refining the type there would move an unproven
  claim rather than a proven one.

* **Message-free obligations: all five closed.** The two in
  `solve/lower/scalar/operators.rs` fell to `LoweredUnaryOperator`;
  `ir-dae/model/function_loop_capability.rs` to the loop's own domain;
  `solve/lower/scalar/builtins.rs` by naming each filling generator's value in
  the match that selects it; and `solve/lower/scalar/arrays.rs` by collecting
  the constant subscripts into `Option<Vec<_>>` in one pass, so deciding that
  every subscript is constant and reading the constants out are the same
  expression.

### Not on the worklist

* **Class (c), about 99 sites.** Two sub-families. The numeric one
  (`u32::try_from`, `checked_mul`, `checked_add` on extents and ordinals) needs a
  proven bound on arena and domain cardinality; it is the right first target for
  a bounded proof once W1 has given `FrozenStorage` a well-formedness predicate
  to hang it on. The coherence one (side maps keyed by id that must be total,
  `analysis/discrete_values.rs:558`'s permutation, `storage.rs:773`'s
  forward-definition count) is what a prover is for. Do not spend type surgery
  on these.
* **Class (d), about 4 sites**, all at the wire-decode boundary in ir-dae
  (`model/construction_checks.rs:112`, `model/wire/function_replay.rs:246`).
  These should become `Result` on the decode path, and only there.

---

## 4. The gate

`crates/rumoca/tests/architecture_hardening_test/totality_debt/` pins the counts
above.

* `scan.rs` is the pure text scan: a stripper that removes comment and literal
  text, carrying which comment or literal a line ended inside into the next
  line, feeding a four-state machine over the stripped lines. No Rust parsing.
  It is unit-tested on fixtures covering the semicolon form, the brace form,
  both in one file, a multi-line `#[cfg(test)]` signature, a `#[cfg(test)]`
  struct field, `#[cfg(test)] use`/`let`, an attribute joined to its item on one
  line in the statement, block, stacked and body-opening spellings, a semicolon
  inside an array length, string literals and comments that merely name a form,
  a commented-out `mod` declaration, `.unwrap()` versus `.unwrap_or(`, a stacked
  attribute list, a file that ends mid-item, a multi-line block comment carrying
  an unbalanced brace in each direction, nested block comments, character
  literals carrying braces and quotes, and lifetimes that must not be read as
  character literals.
* Multi-line string literals have their own fixtures, in both corruption
  directions: a string whose brace would spuriously close a `#[cfg(test)]` body
  and one whose brace would spuriously extend it over shipped code, a raw string
  carrying its hash count across lines, byte and C strings in their cooked and
  raw spellings, an attribute quoted inside a string that is not an attribute, a
  file that ends inside a string, and a trailing backslash whose escape must not
  reach the next line. The extending case ends with a second string that closes
  the spurious depth again, so the file reads as well formed at its end: that
  fixture is the one the end-of-file assertion cannot catch.
* `gate.rs` walks the five crates, applies the two `#[cfg(test)]` exclusions,
  and asserts each crate's count is at or below its pinned ceiling. A second
  test pins `.unwrap()` separately. A third asserts the two ceiling tables name
  the same crates. A fourth asserts every pinned crate still contributes
  production source, so a crate rename cannot silently zero a counter. A fifth
  asserts the scan reached the end of every file it counted. A sixth asserts no
  counted file is named like a test, and a seventh that no counted file sits
  under a directory called `tests`. Five more work on fixture trees written to
  a scratch directory rather than on the crates: one builds a tree whose
  `tests/mod.rs` and `generated/mod.rs` are declared with plain `mod`
  declarations and asserts both are counted, one asserts a `#[cfg(test)] mod`
  declaration excludes its file and everything that file in turn declares, one
  asserts a tree of known contents produces the counts the gate claims, one
  asserts a multi-line string holding braces does not hide the obligations
  below it, and one asserts a `#[cfg(test)] mod <name>;` quoted inside a string
  does not pull the named file out of the count.

### Why the gate cannot quietly under-report

Misclassifying a new file as production can only raise the count and trip the
gate; it can never hide an obligation. The `#[cfg(test)]` exclusions run the
other way, and they are sound for the same reason the compiler is: a
`#[cfg(test)]` item is not in the release build, so it carries no shipped
obligation.

No part of a path is an exclusion, in the file name or in any directory above
it. That is the one class of exclusion an author could trigger, by accident or
on purpose, without changing a line of what ships: renaming a module to
`foo_tests.rs`, or moving a subtree under `src/lower/tests/`, changes nothing
the compiler does and would have changed everything the count says. The two
redundancy pins in `gate.rs` are what keep the `#[cfg(test)]` closure the only
route out of the count.

The remaining way the scan could under-report is by losing track of where a
`#[cfg(test)]` item, a comment or a literal ends and dropping the rest of a
file, which is exactly the failure that produced the earlier blanked-file
problem. `scan_tracks_file_to_its_end` exists for that, and the gate asserts it
over all 219 files rather than trusting the state machine.

That assertion is a backstop, not the guarantee. It sees only what is still open
when the file runs out: a construct that opens and closes leaves it satisfied
even when the text between corrupted the item scan, and it never sees an early
stop at a later `}` at all. Two multi-line strings with opposite brace
imbalances are enough to lose a block of shipped lines and still end the file in
code. The guarantee therefore has to come from the stripper, which is why every
kind of comment and literal, including the ones that span lines, is removed
before the state machine sees a single line.

### What the gate still does not catch

The ceilings are per crate and assert only `<=`, so two things pass that should
not:

* Moving a phase's assertions into a sixth crate lowers a pinned crate's count
  without removing any obligation. Adding the destination crate to both ceiling
  tables is the fix, and `test_totality_ceiling_tables_cover_the_same_crates`
  makes adding it a one-line change rather than an easy omission.
* Slack accumulates: once a migration lands and the count falls, the ceiling
  stays where it was until someone lowers it, so the same number of new
  assertions can be added back for free.

When a migration lands, lower the ceiling in the same change. That is the whole
discipline the gate depends on; the gate itself only stops the number going up.

---

## Appendix: where the obligations actually are

Every file carrying five or more sites, so a migration can be scoped to a file
rather than to a crate: 48 files holding 739 of the 878 sites. Paths are
relative to the crate's `src/`. The worklist column names the item that covers
the largest share of a file's sites, not all of them; most files carry a tail of
other shapes, and a file whose sites are dominated by no worklist item is
labelled with its class instead.

| crate | file | sites | dominant worklist item |
|---|---|---:|---|
| rumoca-phase-galec | `lower/expression_functions.rs` | 62 | W1 |
| rumoca-phase-solve | `lower.rs` | 59 | W1 |
| rumoca-phase-galec | `lower/user_functions.rs` | 47 | W1 |
| rumoca-phase-solve | `lower/scalar/builtins.rs` | 43 | W4 |
| rumoca-phase-galec | `lower/expression_projection.rs` | 37 | W1, W4 |
| rumoca-phase-solve | `lower/events.rs` | 36 | W1, W2 |
| rumoca-phase-dae | `construction.rs` | 28 | W3 |
| rumoca-phase-solve | `lower/scalar/functions.rs` | 27 | W1 |
| rumoca-ir-dae | `model/view.rs` | 25 | W1, W2 |
| rumoca-phase-dae | `construction/function_body.rs` | 23 | W3 |
| rumoca-phase-galec | `lower/clocked_assignments.rs` | 21 | W1 |
| rumoca-phase-solve | `layout.rs` | 19 | W1, W2, W5 |
| rumoca-phase-dae | `construction/expression.rs` | 17 | W3 |
| rumoca-phase-galec | `lower.rs` | 17 | W1 |
| rumoca-phase-solve | `lower/scalar/selector.rs` | 16 | class (c) |
| rumoca-phase-galec | `admissibility.rs` | 13 | W1, W2 |
| rumoca-phase-galec | `lower/expression_helpers.rs` | 13 | W1, W5 |
| rumoca-phase-dae | `construction/algorithm_lowering.rs` | 12 | W3 |
| rumoca-phase-dae | `construction/function_construction.rs` | 12 | W3 |
| rumoca-phase-solve | `lower/events/structured.rs` | 12 | W1, W2 |
| rumoca-ir-dae | `expression/type_rules.rs` | 11 | W5 |
| rumoca-phase-dae | `construction/analysis/function_bodies.rs` | 10 | W3 |
| rumoca-phase-galec | `lower/expression_function_folds.rs` | 10 | W1 |
| rumoca-phase-galec | `lower/user_functions/indexed_update.rs` | 10 | W1 |
| rumoca-phase-solve | `lower/clocks.rs` | 10 | W2, W1 |
| rumoca-phase-solve | `lower/scalar/arrays.rs` | 10 | class (c) |
| rumoca-phase-dae | `construction/model_algorithm.rs` | 9 | W3 |
| rumoca-phase-dae | `construction/model_events.rs` | 9 | W3 |
| rumoca-phase-solve | `lower/scalar.rs` | 9 | class (c) |
| rumoca-phase-solve | `lower/typed_functions/tensor.rs` | 8 | W4 |
| rumoca-phase-dae | `construction/algorithm.rs` | 7 | W3 |
| rumoca-phase-dae | `construction/analysis/function_record_assemblies.rs` | 7 | W3 |
| rumoca-phase-galec | `lower/pre_references.rs` | 7 | W2 |
| rumoca-phase-dae | `construction/analysis/function_returns.rs` | 6 | W3 |
| rumoca-phase-dae | `construction/expression/calls.rs` | 6 | W3 |
| rumoca-phase-dae | `construction/function_array_assembly.rs` | 6 | W3 |
| rumoca-phase-galec | `lower/clock_schedule.rs` | 6 | W1 |
| rumoca-phase-galec | `lower/conditionals.rs` | 6 | W1 |
| rumoca-phase-solve | `lower/scalar/conditions.rs` | 6 | W1 |
| rumoca-phase-solve | `lower/scalar/constants.rs` | 6 | W4 |
| rumoca-phase-solve | `lower/scalar/operators.rs` | 6 | W4, W5 |
| rumoca-ir-dae | `model/storage.rs` | 5 | class (c) |
| rumoca-phase-dae | `construction/function_record_assembly.rs` | 5 | W3 |
| rumoca-phase-dae | `construction/function_shapes/expression_rules.rs` | 5 | W5 |
| rumoca-phase-dae | `construction/function_shapes/mod.rs` | 5 | W3, W5 |
| rumoca-phase-dae | `construction/variable_construction.rs` | 5 | W5 |
| rumoca-phase-galec | `lower/expression_array_update.rs` | 5 | W1 |
| rumoca-phase-solve | `model_values.rs` | 5 | W2, W1 |

The two densest files are `galec/lower/expression_functions.rs` (62 sites, 55 of
them W1-shaped) and `solve/lower.rs` (59 sites, 29 W1-shaped). Landing W1 across
just those two removes 84 obligations, which is a visible move on the total from
two files. `solve/lower.rs` keeps a long tail afterwards: its remaining 30 sites
are spread across the domain-coherence and scalar-capacity families rather than
concentrated in one shape.

Four rows carry no worklist item, and they are the honest shape of the residue.
`solve/lower/scalar/selector.rs` (16), `solve/lower/scalar/arrays.rs` (10) and
`solve/lower/scalar.rs` (9) are dominated by coordinate and shape coherence
("belongs to its concatenation shape", "checked domain remains valid", "non-root
scalar context has a frame") rather than by an arena lookup, and
`ir-dae/model/storage.rs` (5) by the numeric family. Together with the 99-site
class (c) estimate in section 2, they are where a prover is worth pointing
first, and where type surgery is not.
