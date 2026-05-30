# Defensive Coding Review

## Executive Summary

This review scanned every Rust source file under `crates/` across all compiler phase crates
(`rumoca-phase-parse`, `rumoca-phase-resolve`, `rumoca-phase-instantiate`, `rumoca-phase-typecheck`,
`rumoca-phase-flatten`, `rumoca-phase-dae`, `rumoca-phase-structural`, `rumoca-phase-solve`,
`rumoca-phase-codegen`, `rumoca-eval-dae`, and `rumoca-compile`).
Several patterns were found that violate the fail-fast contract: builtins with missing arguments
silently emit `0.0` instead of returning an error; a missing record field in function-call lowering
silently synthesises a fake `Real` parameter, producing semantically incorrect Solve-IR; and
`Connections.potentialRoot` silently accepts priority `0` when the second argument is not an integer
literal.  The vast majority of remaining `unwrap_or*` calls are either in display/diagnostic code
(where a fallback string is acceptable) or in span-recovery paths (where `Span::DUMMY` is the
agreed sentinel).

---

## Findings Table

| # | File | Line(s) | Pattern | Severity | Suggested Fix |
|---|------|---------|---------|----------|---------------|
| 1 | `rumoca-phase-solve/src/lower/function_calls.rs` | 1309 | `unwrap_or_else(|| FunctionParam::new(&field_name, "Real"))` — creates a fake scalar `Real` param when the named field is not found in the function signature | **High** | Return `Err(LowerError::…)` if `fields.iter().find(…)` yields `None`; a missing field is a contract violation, not a "default Real" |
| 2 | `rumoca-phase-flatten/src/connections_builtin.rs` | 22 | `return Ok(0)` when `Connections.potentialRoot(p, expr)` second arg is present but not an integer literal — silently assigns priority 0 | **High** | Return `Err(FlattenError::…)` with a span; MLS §9.4 requires a literal integer |
| 3 | `rumoca-phase-solve/src/lower.rs` | 1763, 1767, 1781, 1785 | `arg()` closure inside `lower_div_builtin` and `lower_truncating_remainder_builtin` returns `Ok(emit_const(0.0))` when `args.get(i)` is `None` — div/mod with 0 args emits `0 ÷ 0` silently | **High** | Assert `args.len() == 2` (or return `Err`) before entering the function body; both MLS builtins require exactly two arguments |
| 4 | `rumoca-phase-solve/src/lower.rs` | 1826–1830 | `delay()` with no args falls back to `emit_const(0.0)` via `unwrap_or_else` — any model that reaches this code produces silently wrong simulation output | **High** | Return `Err(LowerError::…)` if `args.is_empty()`; `delay` requires at least one argument |
| 5 | `rumoca-phase-solve/src/lower.rs` | 1680–1686 | The `arg` closure (used throughout the `lower_complex_operator_call` range) silently emits `0.0` for any out-of-range argument index — affects `SemiLinear`, complex arithmetic, and all other callers | **High** | Replace the closure with a helper that returns `Err` and propagate it; all caller sites should already know exactly how many arguments are expected |
| 6 | `rumoca-eval-dae/src/eval/builtin_runtime.rs` | 26–35 (and mirror at 55–63) | The `arg` closure inside `eval_builtin_trigonometric` and `eval_builtin_event_like` returns `T::zero()` for missing arguments — any miscompiled/malformed Solve-IR that omits an arg computes `sin(0)`, `cos(0)`, etc. without any runtime diagnostic | **High** | Panic with a descriptive message (`panic!("builtin {function:?} called with {} args, expected ≥ {n}", args.len())`) or return an error variant; this is a correctness firewall for the runtime |
| 7 | `rumoca-phase-solve/src/initial_values.rs` | 330 | `i64::try_from(*dim).unwrap_or(i64::MAX)` — an overflow in shape dimension conversion produces `i64::MAX` which is passed to `scalar_name_text_for_flat_index`, generating garbage variable names silently | **High** | Return `Err` (or panic with `expect`) if any dimension exceeds `i64::MAX`; real models never have arrays that large |
| 8 | `rumoca-phase-structural/src/eliminate/tearing_elimination.rs` | 44 | `eliminated_eq_flags.get(eq_idx).copied().unwrap_or(true)` — if `eq_idx` is out of bounds for the flag slice, tearing silently aborts (returns early) instead of signalling an invariant violation | **High** | Replace with `assert!(eq_idx < eliminated_eq_flags.len(), "eq_idx {eq_idx} out of bounds …")` before the `.get()`; the slice is always sized to `dae.continuous.equations.len()` at construction and eq_idx is derived from the same collection |
| 9 | `rumoca-phase-solve/src/lib.rs` | 592 | `rows.get(row_idx).map(Vec::as_slice).unwrap_or(&[])` — a `row_idx` that exceeds `rows.len()` silently produces an empty algebraic row, hiding a structural mismatch between the solve layout and the equation list | **High** | Use `rows[row_idx].as_slice()` (panics on OOB) or return `Err`; `row_idx` is taken from `row_indices` which must be a valid range into `rows` |
| 10 | `rumoca-phase-instantiate/src/inheritance.rs` | 1684 | `get_effective_components(tree, class).unwrap_or_else(|_| class.components.clone())` — silently ignores an inheritance resolution error and returns only the class's own components | **Med** | Propagate the `InstantiateResult` error; callers should receive the error and surface it as a diagnostic instead of silently operating on an incomplete component set |
| 11 | `rumoca-phase-dae/src/analysis/variable_analysis.rs` | 349 | `get_top_level_prefix(name.as_str()).unwrap_or_default()` — returns `""` when the prefix cannot be extracted from a name that has already been confirmed to contain a dot (via `has_top_level_dot`); the empty prefix is then used in `flat.top_level_connectors.contains("")`, silently misclassifying the input variable | **Med** | Replace with `expect("has_top_level_dot guarantees a prefix is extractable")` or return an error; the two predicates must remain consistent |
| 12 | `rumoca-phase-resolve/src/cycles.rs` | 92 | `path.iter().position(|&id| id == base_id).unwrap_or(0)` — if `base_id` is somehow not in `path`, the cycle diagnostic starts from index 0 (the wrong node), producing a misleading error message | **Med** | Replace `unwrap_or(0)` with `unwrap_or_else(|| { tracing::warn!("…"); 0 })` or assert; if `base_id` is not in `path` the algorithm has an internal inconsistency |
| 13 | `rumoca-phase-solve/src/lib.rs` | 490 | `state_derivative_rows.get(*row_idx).copied().unwrap_or(false)` — if `state_derivative_rows` is shorter than `dae.continuous.equations`, extra equations are silently classified as non-derivative | **Med** | Assert `row_idx < state_derivative_rows.len()` or use `unwrap_or_else(|| panic!("…"))` |
| 14 | `rumoca-phase-solve/src/lower/derivative_rhs.rs` | 109, 181 | `analysis.components.get(&analysis.component_roots[i]).cloned().unwrap_or_else(|| vec![i])` — if the component map is missing a root key, the state is treated as a scalar singleton instead of its true group | **Med** | Replace with `expect("every root inserted into components during derivative_state_components")` — the fallback is dead code that masks future breakage if the invariant changes |
| 15 | `rumoca-phase-dae/src/runtime_precompute/clock.rs` | 843 | `timings.entry(item).or_insert(timing.clone())` — if a clock variable already has a different timing, the conflict is silently ignored (old value kept) | **Med** | Check if the existing timing conflicts with the new one and emit a diagnostic if they differ |
| 16 | `rumoca-phase-flatten/src/boolean_eval.rs` | 385 | `ctx.cardinality_counts.get(&path).unwrap_or(&0)` — returns 0 for paths not in the map; MLS `cardinality()` must return 0 for unconnected connectors so this is semantically correct, but the pattern should be documented with a comment | **Low** | Add a comment citing MLS §9.4.2; the pattern is intentional |
| 17 | `rumoca-phase-flatten/src/connections/mod.rs` | 655 | `type_roots.get(&type_id).copied().unwrap_or(type_id)` — if a type has no root in the map, its own id is used as root; this is correct (identity mapping) but should be guarded | **Low** | No code change required; add a comment |
| 18 | `rumoca-phase-dae/src/algorithm_lowering.rs` | 1251, 1258 | `unwrap_or_else(|| algorithm_if_fallback_expr(dae, &target))` — when an if-branch does not assign a target, the fallback is `pre(target)` for discrete vars or `target` for continuous; this is correct per MLS §11.1.3 but the fallback silently diverges from a missed assignment | **Low** | Document the MLS citation in a comment; the behaviour is intentional |
| 19 | `rumoca-phase-flatten/src/vcg.rs` | 671 | `component.iter().copied().min().unwrap_or(component[0])` — `min()` on a non-empty iterator never returns `None`, so `unwrap_or` is dead code; `component[0]` would also panic on empty | **Low** | Replace with `component.iter().copied().min().expect("component is non-empty")` to make the invariant explicit |
| 20 | `rumoca-phase-dae/src/dae_lowering.rs` | 1057 | `scalarize_var_ref_at(…).map(Ok).unwrap_or_else(|| Ok(expr.clone()))` — if scalarisation does not apply, the original expression is returned unchanged; this is correct pass-through behaviour | **Low** | No change needed; add a comment |
| 21 | `rumoca-phase-solve/src/lower/derivative_rhs.rs` | 1989 | `declared_function_output_dims(dae_model, name).unwrap_or_default()` — unknown function output dims become `[]` (scalar), which may be wrong for array-returning functions | **Med** | Return a diagnostic when `declared_function_output_dims` returns `None` for a name that appears in a differentiated expression |
| 22 | `rumoca-phase-dae/src/initial.rs` | 105 | `var.start.clone().unwrap_or_else(|| default_real_start(span))` — if no `start` attribute is set for a fixed-initialization variable, the default `0.0` is used silently | **Low** | This is correct per MLS §8.6 ("start default is 0 for Real"); document with citation |
| 23 | Multiple files (`rumoca-phase-solve/src/lower.rs`, `rumoca-phase-dae/src/algorithm_lowering.rs`, etc.) | Various | `expr.span().unwrap_or(Span::DUMMY)` — dozens of span recovery sites return a dummy span; this does not affect IR correctness but makes error messages point to the wrong location | **Low** | Acceptable pattern; ensure `Span::DUMMY` is rendered as "unknown location" in diagnostics |
| 24 | `rumoca-phase-solve/src/lower/array_values/inference.rs` | 17–18, 34, 40, 74, 237–284, 324–390 | Dimension inference returns `unwrap_or_default()` (i.e. `Vec::new()`, scalar) whenever array shape is unknown — propagates scalar assumption through codegen | **Med** | Add a diagnostic warning (or debug-assert) when a non-trivial expression yields unknown dims; callers consuming the result for scalarisation should treat `Vec::new()` as "could not infer, not necessarily scalar" |
| 25 | `rumoca-phase-solve/src/lower/function_calls.rs` | 559 | `unwrap_or_else(|| random_generator_state_len(generator))` — if `random_state_len_arg` returns `None`, a fallback generator state length is used silently; wrong length would corrupt the random state slot | **Med** | Assert or return `Err` if the arg cannot be evaluated for a context where the length is critical |

---

## Performance-Sensitive Checks

The following checks could be expensive on large models but are worth having; they should be
gated behind `debug_assert!` rather than always-on `assert!`:

| Location | Suggested Gate | Reason |
|---|---|---|
| `tearing_elimination.rs:44` bounds check for `eq_idx` | `debug_assert!(eq_idx < eliminated_eq_flags.len())` | Called in a tight loop over BLT blocks; the invariant is established by construction so `debug_assert!` catches regressions in testing without paying in release |
| `derivative_rhs.rs:107,179` component map lookup | `debug_assert!(analysis.components.contains_key(&root))` | Per-state in the derivative RHS assembly loop; always-on would be O(n_states) map lookups |
| `lib.rs:490` `state_derivative_rows` bounds | `debug_assert!(row_idx < state_derivative_rows.len())` | Iterated over all equations; always-on would slow down the filter pass |
| `inference.rs` array dimension fallback | `debug_assert!(inferred_dims.is_some() || expr_is_trivially_scalar(expr))` | Called during Solve-IR lowering for every expression node |

For the **High severity** items (items 1–9 above) no gating is needed: the conditions they check
are either impossible in correct code (so the assert never fires in the hot path) or infrequent
(one check per builtin call site, not per residual evaluation).

---

## Prioritised Fix List

### High Priority (can produce silently wrong compiler output / wrong IR)

1. **`lower/function_calls.rs:1309`** — Fake `Real` param for missing record field: replace
   `unwrap_or_else(|| FunctionParam::new(…))` with `ok_or_else(|| LowerError::…)?`.

2. **`connections_builtin.rs:22`** — Silent priority-0 for non-literal arg: replace `return Ok(0)`
   with `return Err(FlattenError::…)`.

3. **`lower.rs:1763,1767,1781,1785`** — `div` and `mod` with missing args emit `0.0`:
   add `if args.len() < 2 { return Err(…) }` guards at the top of `lower_div_builtin` and
   `lower_truncating_remainder_builtin`.

4. **`lower.rs:1826–1830`** — `delay()` with no args emits `0.0`: add
   `if args.is_empty() { return Err(…) }`.

5. **`lower.rs:1680–1686`** — The `arg` closure used for complex operators and SemiLinear:
   change the closure to return `Result` and propagate; add an `assert` or `Err` for the
   missing-arg case.

6. **`eval/builtin_runtime.rs`** — Runtime `arg` closure returns `T::zero()` for OOB:
   change to `panic!("…")` with a message naming the builtin and the expected count.

7. **`initial_values.rs:330`** — `i64::try_from` overflow → `i64::MAX`:
   replace `unwrap_or(i64::MAX)` with `unwrap_or_else(|_| panic!("array dim overflows i64: {dim}"))`.

8. **`tearing_elimination.rs:44`** — Silent tearing abort on OOB flag index:
   add `assert!(eq_idx < eliminated_eq_flags.len(), "…")` before the `.get()`.

9. **`lib.rs:592`** — Missing algebraic row silently treated as empty:
   use direct indexing `rows[row_idx].as_slice()`.

### Medium Priority (produces confusing downstream error rather than a source-pointing one)

10. `inheritance.rs:1684` — propagate `InstantiateResult` error instead of silently discarding it.
11. `variable_analysis.rs:349` — assert prefix is extractable after confirming dot presence.
12. `cycles.rs:92` — warn/assert if `base_id` is missing from path.
13. `lib.rs:490` — assert `state_derivative_rows` size matches equations.
14. `derivative_rhs.rs:109,181` — replace `unwrap_or_else` with `expect`.
15. `clock.rs:843` — validate that conflicting timings are detected before `or_insert`.
16. `derivative_rhs.rs:1989` — emit diagnostic for unknown function output dims in differentiated exprs.
17. `inference.rs` — add medium-verbosity diagnostic when dims inference falls back to scalar for non-trivial exprs.
18. `function_calls.rs:559` — assert or propagate error for random state length inference failure.

### Low Priority (defensive assertions for future development safety)

19. `vcg.rs:671` — replace redundant `unwrap_or` with `expect`.
20. `boolean_eval.rs:385` — add MLS citation comment.
21. `connections/mod.rs:655` — add identity-mapping comment.
22. All `expr.span().unwrap_or(Span::DUMMY)` sites — ensure dummy spans render as "unknown location" in diagnostics.
23. `algorithm_lowering.rs:1251,1258` — add MLS §11.1.3 citation comment for `pre`/current fallback.
