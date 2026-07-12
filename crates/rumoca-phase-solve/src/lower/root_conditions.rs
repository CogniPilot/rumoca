use super::*;
use std::collections::HashSet;

const I64_TEXT_CAPACITY: usize = 20;

struct RootRuntime<'a> {
    dae_model: &'a dae::Dae,
    layout: &'a VarLayout,
    functions: &'a IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    clock_intervals: &'a IndexMap<String, f64>,
    clock_timings: &'a IndexMap<String, dae::ClockSchedule>,
    triggered_clock_conditions: &'a [rumoca_core::Expression],
    variable_starts: &'a IndexMap<String, rumoca_core::Expression>,
    structural_bindings: Arc<IndexMap<String, f64>>,
    direct_assignments: Arc<IndexMap<String, DirectAssignmentValue>>,
    indexed_bindings: IndexedBindingMap,
}

fn root_lower_builder<'a>(runtime: &'a RootRuntime<'a>) -> LowerBuilder<'a> {
    LowerBuilder::new_with_metadata(
        runtime.layout,
        runtime.functions,
        LowerBuilderMetadata {
            clock_intervals: Some(runtime.clock_intervals),
            clock_timings: Some(runtime.clock_timings),
            triggered_clock_conditions: Some(runtime.triggered_clock_conditions),
            discrete_valued_names: Some(&runtime.dae_model.variables.discrete_valued),
            variable_starts: Some(runtime.variable_starts),
            dae_variables: Some(&runtime.dae_model.variables),
            indexed_bindings: Some(&runtime.indexed_bindings),
            is_initial_mode: false,
        },
    )
    .with_structural_bindings(Arc::clone(&runtime.structural_bindings))
    .with_direct_assignments(Arc::clone(&runtime.direct_assignments))
}

pub(super) fn lower_root_conditions(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let structural_bindings = compile_time::structural_bindings(dae_model)?;
    let direct_assignments =
        derivative_rhs::collect_runtime_direct_assignments(dae_model, &structural_bindings)?;
    let runtime = RootRuntime {
        dae_model,
        layout,
        functions: &dae_model.symbols.functions,
        clock_intervals: &dae_model.clocks.intervals,
        clock_timings: &dae_model.clocks.timings,
        triggered_clock_conditions: &dae_model.clocks.triggered_conditions,
        variable_starts: &dae_model.metadata.variable_starts,
        structural_bindings: Arc::new(structural_bindings),
        direct_assignments: Arc::new(direct_assignments),
        indexed_bindings: Arc::new(build_indexed_binding_map(layout)),
    };
    let span = root_condition_context_span(dae_model);
    let row_count = root_condition_count(dae_model, span)?;
    let mut rows = root_vec_with_capacity(row_count, "root condition row count", span)?;
    for (condition_index, condition) in dae_model.conditions.relations.iter().enumerate() {
        if root_condition_is_inactive(dae_model, condition) {
            rows.push(
                lower_inactive_root_row(condition, layout, &dae_model.symbols.functions).map_err(
                    |err| {
                        err.with_context(format!(
                            "root condition {condition_index}: {}",
                            short_expr(condition, 160)
                        ))
                    },
                )?,
            );
        } else {
            rows.push(
                lower_root_condition_row(condition, layout, &runtime).map_err(|err| {
                    err.with_context(format!(
                        "root condition {condition_index}: {}",
                        short_expr(condition, 160)
                    ))
                })?,
            );
        }
    }
    for (condition_index, condition) in dae_model
        .events
        .synthetic_root_conditions
        .iter()
        .enumerate()
    {
        if root_condition_is_inactive(dae_model, condition) {
            rows.push(
                lower_inactive_root_row(condition, layout, &dae_model.symbols.functions).map_err(
                    |err| {
                        err.with_context(format!(
                            "synthetic root condition {condition_index}: {}",
                            short_expr(condition, 160)
                        ))
                    },
                )?,
            );
        } else {
            rows.push(
                lower_synthetic_root_condition_row(condition, layout, &runtime).map_err(|err| {
                    err.with_context(format!(
                        "synthetic root condition {condition_index}: {}",
                        short_expr(condition, 160)
                    ))
                })?,
            );
        }
    }
    for condition in &dae_model.clocks.triggered_conditions {
        rows.push(lower_triggered_clock_condition_row(
            condition, layout, &runtime,
        )?);
    }
    Ok(rows)
}

pub(super) fn lower_root_relation_memory_targets(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Option<rumoca_ir_solve::ScalarSlot>>, LowerError> {
    let span = root_condition_context_span(dae_model);
    let target_count = root_condition_count(dae_model, span)?;
    let mut targets =
        root_vec_with_capacity(target_count, "root relation memory target count", span)?;
    for relation_idx in 0..dae_model.conditions.relations.len() {
        targets.push(condition_memory_slot_for_relation(
            dae_model,
            layout,
            relation_idx,
        )?);
    }
    let synthetic_target_count = checked_root_count_add(
        dae_model.events.synthetic_root_conditions.len(),
        dae_model.clocks.triggered_conditions.len(),
        span,
        "synthetic root relation memory target count",
    )?;
    for _ in 0..synthetic_target_count {
        targets.push(None);
    }
    Ok(targets)
}

pub(super) fn lower_scheduled_root_conditions(
    dae_model: &dae::Dae,
) -> Result<Vec<rumoca_ir_solve::ScheduledRootCondition>, LowerError> {
    let span = root_condition_context_span(dae_model);
    let root_count = root_condition_count(dae_model, span)?;
    let mut roots =
        root_vec_with_capacity(root_count, "scheduled root condition metadata count", span)?;
    for root in &dae_model.events.scheduled_root_conditions {
        if root.root_index >= root_count {
            return Err(root_contract_error(
                format!(
                    "scheduled root condition index {} is outside {root_count} root conditions",
                    root.root_index
                ),
                span,
            ));
        }
        roots.push(rumoca_ir_solve::ScheduledRootCondition {
            root_index: root.root_index,
            period_seconds: root.period_seconds,
            phase_seconds: root.phase_seconds,
        });
    }
    Ok(roots)
}

fn root_condition_count(
    dae_model: &dae::Dae,
    span: Option<rumoca_core::Span>,
) -> Result<usize, LowerError> {
    let relation_count = dae_model.conditions.relations.len();
    let synthetic_count = dae_model.events.synthetic_root_conditions.len();
    let triggered_count = dae_model.clocks.triggered_conditions.len();
    let root_count = checked_root_count_add(
        relation_count,
        synthetic_count,
        span,
        "root relation and synthetic condition count",
    )?;
    checked_root_count_add(
        root_count,
        triggered_count,
        span,
        "root condition and triggered clock count",
    )
}

fn checked_root_count_add(
    lhs: usize,
    rhs: usize,
    span: Option<rumoca_core::Span>,
    context: &'static str,
) -> Result<usize, LowerError> {
    lhs.checked_add(rhs)
        .ok_or_else(|| root_contract_error(format!("{context} overflows host index range"), span))
}

fn root_contract_error(
    reason: impl Into<String>,
    span: impl Into<Option<rumoca_core::Span>>,
) -> LowerError {
    let reason = reason.into();
    match span.into().filter(|span| !span.is_dummy()) {
        Some(span) => LowerError::ContractViolation { reason, span },
        None => LowerError::UnspannedContractViolation { reason },
    }
}

fn root_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: impl Into<Option<rumoca_core::Span>>,
) -> Result<Vec<T>, LowerError> {
    let span = span.into();
    let mut values = Vec::new();
    values.try_reserve_exact(capacity).map_err(|_| {
        root_contract_error(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })?;
    Ok(values)
}

fn root_condition_context_span(dae_model: &dae::Dae) -> Option<rumoca_core::Span> {
    for condition in &dae_model.conditions.relations {
        if let Some(span) = condition.span()
            && !span.is_dummy()
        {
            return Some(span);
        }
    }
    for condition in &dae_model.events.synthetic_root_conditions {
        if let Some(span) = condition.span()
            && !span.is_dummy()
        {
            return Some(span);
        }
    }
    for condition in &dae_model.clocks.triggered_conditions {
        if let Some(span) = condition.span()
            && !span.is_dummy()
        {
            return Some(span);
        }
    }
    None
}

fn lower_root_condition_row(
    condition: &rumoca_core::Expression,
    _layout: &VarLayout,
    runtime: &RootRuntime<'_>,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = root_lower_builder(runtime);
    let scope = Scope::new();
    let span = root_condition_span(condition)?;
    let root_value = match condition {
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => match op {
            rumoca_core::OpBinary::Lt | rumoca_core::OpBinary::Le => {
                let l = builder.lower_expr(lhs, &scope, 0)?;
                let r = builder.lower_expr(rhs, &scope, 0)?;
                builder.emit_binary_at(BinaryOp::Sub, l, r, span)?
            }
            rumoca_core::OpBinary::Gt | rumoca_core::OpBinary::Ge => {
                let l = builder.lower_expr(lhs, &scope, 0)?;
                let r = builder.lower_expr(rhs, &scope, 0)?;
                builder.emit_binary_at(BinaryOp::Sub, r, l, span)?
            }
            _ => lower_bool_condition_as_root(condition, &mut builder, &scope)?,
        },
        _ => lower_bool_condition_as_root(condition, &mut builder, &scope)?,
    };
    builder.ops.push(LinearOp::StoreOutput { src: root_value });
    Ok(builder.ops)
}

fn condition_memory_expr_for_relation(
    dae_model: &dae::Dae,
    relation_idx: usize,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let mut offset = 0usize;
    for eq in &dae_model.conditions.equations {
        let scalar_count = eq.scalar_count.max(1);
        let end = checked_relation_offset_end(offset, scalar_count, eq.span)?;
        if relation_idx < end {
            let Some(lhs) = eq.lhs.as_ref() else {
                return Ok(None);
            };
            let flat_index = relation_idx - offset;
            return Ok(Some(condition_memory_scalar_expr(
                dae_model,
                lhs.var_name(),
                flat_index,
                scalar_count,
                eq.span,
            )?));
        }
        offset = end;
    }
    Ok(None)
}

fn checked_relation_offset_end(
    offset: usize,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    offset.checked_add(scalar_count).ok_or_else(|| {
        root_contract_error(
            "condition relation scalar offset overflows host index range",
            span,
        )
    })
}

fn condition_memory_slot_for_relation(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    relation_idx: usize,
) -> Result<Option<rumoca_ir_solve::ScalarSlot>, LowerError> {
    let Some(condition_memory) = condition_memory_expr_for_relation(dae_model, relation_idx)?
    else {
        return Ok(None);
    };
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = condition_memory
    else {
        return Ok(None);
    };
    let key = relation_memory_key(name.as_str(), &subscripts)?;
    Ok(layout.binding(&key))
}

fn relation_memory_key(
    name: &str,
    subscripts: &[rumoca_core::Subscript],
) -> Result<String, LowerError> {
    let span = first_subscript_span(subscripts);
    let mut key = String::new();
    let reserve = if subscripts.is_empty() {
        name.len()
    } else {
        name.len()
            .checked_add(2)
            .and_then(|base| {
                subscripts
                    .len()
                    .checked_mul(I64_TEXT_CAPACITY + 2)
                    .and_then(|suffix| base.checked_add(suffix))
            })
            .ok_or_else(|| {
                root_contract_error("relation memory key capacity exceeds host limits", span)
            })?
    };
    key.try_reserve(reserve).map_err(|_| {
        root_contract_error(
            "relation memory key capacity exceeds host memory limits",
            span,
        )
    })?;
    key.push_str(name);
    if subscripts.is_empty() {
        return Ok(key);
    }

    let indices = generated_index_subscripts(subscripts)?;
    key.push('[');
    for (idx, value) in indices.iter().enumerate() {
        if idx > 0 {
            key.push(',');
        }
        key.push_str(value.to_string().as_str());
    }
    key.push(']');
    Ok(key)
}

fn generated_index_subscripts(
    subscripts: &[rumoca_core::Subscript],
) -> Result<Vec<i64>, LowerError> {
    let span = first_subscript_span(subscripts);
    let mut indices = root_vec_with_capacity(
        subscripts.len(),
        "relation memory generated subscript count",
        span,
    )?;
    for subscript in subscripts {
        match subscript {
            rumoca_core::Subscript::Index { value, .. } => indices.push(*value),
            rumoca_core::Subscript::Colon { span } | rumoca_core::Subscript::Expr { span, .. } => {
                return Err(root_contract_error(
                    "relation memory target contains a non-index generated subscript",
                    *span,
                ));
            }
        }
    }
    Ok(indices)
}

fn condition_memory_scalar_expr(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    flat_index: usize,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, LowerError> {
    if scalar_count <= 1 {
        return Ok(rumoca_core::Expression::VarRef {
            name: lhs.clone().into(),
            subscripts: Vec::new(),
            span,
        });
    }
    let Some(dims) = dae_model
        .variables
        .discrete_valued
        .get(lhs)
        .or_else(|| dae_model.variables.discrete_reals.get(lhs))
        .map(|var| var.dims.as_slice())
    else {
        return Err(root_contract_error(
            format!(
                "relation memory target `{lhs}` has scalar_count={scalar_count} but no DAE variable metadata"
            ),
            span,
        ));
    };
    let Some(indices) = dae::flat_index_to_subscripts(dims, flat_index) else {
        return Err(root_contract_error(
            format!(
                "relation memory target `{lhs}` cannot map flat index {flat_index} of {scalar_count} through dims {dims:?}"
            ),
            span,
        ));
    };
    let mut subscripts =
        root_vec_with_capacity(indices.len(), "relation memory subscript count", span)?;
    for index in indices {
        subscripts.push(checked_relation_subscript(index, span)?);
    }
    Ok(rumoca_core::Expression::VarRef {
        name: lhs.clone().into(),
        subscripts,
        span,
    })
}

fn first_subscript_span(subscripts: &[rumoca_core::Subscript]) -> Option<rumoca_core::Span> {
    for subscript in subscripts {
        let span = match subscript {
            rumoca_core::Subscript::Index { span, .. }
            | rumoca_core::Subscript::Colon { span }
            | rumoca_core::Subscript::Expr { span, .. } => *span,
        };
        if !span.is_dummy() {
            return Some(span);
        }
    }
    None
}

fn checked_relation_subscript(
    index: usize,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Subscript, LowerError> {
    let index = i64::try_from(index).map_err(|_| {
        root_contract_error(
            format!("condition relation subscript index {index} exceeds i64 range"),
            span,
        )
    })?;
    Ok(rumoca_core::Subscript::try_generated_index(
        index,
        span,
        "condition relation subscript",
    )?)
}

fn lower_synthetic_root_condition_row(
    condition: &rumoca_core::Expression,
    layout: &VarLayout,
    runtime: &RootRuntime<'_>,
) -> Result<Vec<LinearOp>, LowerError> {
    if is_relational_root_condition(condition) {
        return lower_root_condition_row(condition, layout, runtime);
    }

    // MLS Appendix B: root functions are real-valued zero-crossing functions.
    // Synthetic roots are precomputed signed residuals, not Boolean relation
    // expressions, so render the numeric residual directly.
    let mut builder = root_lower_builder(runtime);
    let root_value = builder.lower_expr(condition, &Scope::new(), 0)?;
    builder.ops.push(LinearOp::StoreOutput { src: root_value });
    Ok(builder.ops)
}

fn lower_triggered_clock_condition_row(
    condition: &rumoca_core::Expression,
    _layout: &VarLayout,
    runtime: &RootRuntime<'_>,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = root_lower_builder(runtime);
    let root_value = lower_bool_condition_as_root(condition, &mut builder, &Scope::new())?;
    builder.ops.push(LinearOp::StoreOutput { src: root_value });
    Ok(builder.ops)
}

fn root_condition_span(
    condition: &rumoca_core::Expression,
) -> Result<rumoca_core::Span, LowerError> {
    Ok(condition.require_span("root condition")?.span())
}

fn is_relational_root_condition(condition: &rumoca_core::Expression) -> bool {
    matches!(
        condition,
        rumoca_core::Expression::Binary { op, .. } if op.is_relational()
    )
}

fn lower_bool_condition_as_root(
    condition: &rumoca_core::Expression,
    builder: &mut LowerBuilder<'_>,
    scope: &Scope,
) -> Result<Reg, LowerError> {
    let span = root_condition_span(condition)?;
    let cond = builder.lower_expr(condition, scope, 0)?;
    let neg_one = builder.emit_const_at(-1.0, span)?;
    let pos_one = builder.emit_const_at(1.0, span)?;
    builder.emit_select_at(cond, neg_one, pos_one, span)
}

fn lower_inactive_root_row(
    condition: &rumoca_core::Expression,
    layout: &VarLayout,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = LowerBuilder::new(layout, functions);
    let positive = builder.emit_const_at(1.0, root_condition_span(condition)?)?;
    builder.ops.push(LinearOp::StoreOutput { src: positive });
    Ok(builder.ops)
}

fn root_condition_is_inactive(dae_model: &dae::Dae, expr: &rumoca_core::Expression) -> bool {
    // MLS §3.7.3: a relation of the form `time >= threshold` is a time-event
    // generating expression. When `threshold` is piecewise-constant between
    // events (parameters, constants, or `pre(discrete)` held values) the root
    // surface `threshold - time` is a clean, continuous function of time, so it
    // can — and must — fire as an active root condition. This is the firing
    // mechanism for self-rescheduling counters such as the periodic-source
    // pattern `when time >= (pre(count)+1)*period + startTime then
    // count = pre(count)+1`. Without this carve-out the `pre(count)` reference
    // would mark the relation inactive and the `when` would never fire (the
    // dynamic time event stops the integrator but does not, on its own, fire
    // the guarded discrete update).
    if is_piecewise_constant_time_threshold_relation(dae_model, expr) {
        return false;
    }
    uses_runtime_discrete_condition(expr)
        || expression_uses_runtime_discrete_bindings(dae_model, expr)
        || !expression_uses_known_root_bindings(dae_model, expr)
}

/// A relation `time <rel> threshold` (or `threshold <rel> time`) where the
/// non-time side is piecewise-constant between events.
fn is_piecewise_constant_time_threshold_relation(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
) -> bool {
    dae::is_event_constant_time_threshold_relation(dae_model, expr)
}

fn expression_uses_known_root_bindings(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
) -> bool {
    let mut refs: HashSet<rumoca_core::VarName> = HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.into_iter().all(|name| {
        if name.as_str() == "time" {
            return true;
        }
        if dae_model
            .symbols
            .enum_literal_ordinals
            .contains_key(name.as_str())
        {
            return true;
        }
        if has_runtime_binding(dae_model, &name) {
            return true;
        }
        dae::component_base_name(name.as_str())
            .map(|base| has_runtime_binding(dae_model, &rumoca_core::VarName::new(base)))
            .unwrap_or(false)
    })
}

fn has_runtime_binding(dae_model: &dae::Dae, name: &rumoca_core::VarName) -> bool {
    dae_model.variables.states.contains_key(name)
        || dae_model.variables.algebraics.contains_key(name)
        || dae_model.variables.outputs.contains_key(name)
        || dae_model.variables.inputs.contains_key(name)
        || dae_model.variables.parameters.contains_key(name)
        || dae_model.variables.constants.contains_key(name)
        || dae_model.variables.discrete_reals.contains_key(name)
        || dae_model.variables.discrete_valued.contains_key(name)
}

fn expression_uses_runtime_discrete_bindings(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
) -> bool {
    let mut refs: HashSet<rumoca_core::VarName> = HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.into_iter().any(|name| {
        is_runtime_discrete_binding(dae_model, &name)
            || dae::component_base_name(name.as_str())
                .map(|base| {
                    is_runtime_discrete_binding(dae_model, &rumoca_core::VarName::new(base))
                })
                .unwrap_or(false)
    })
}

fn is_runtime_discrete_binding(dae_model: &dae::Dae, name: &rumoca_core::VarName) -> bool {
    dae_model.variables.discrete_reals.contains_key(name)
        || dae_model.variables.discrete_valued.contains_key(name)
}

fn uses_runtime_discrete_condition(expr: &rumoca_core::Expression) -> bool {
    match expr {
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            if matches!(
                function,
                rumoca_core::BuiltinFunction::Sample
                    | rumoca_core::BuiltinFunction::Pre
                    | rumoca_core::BuiltinFunction::Edge
                    | rumoca_core::BuiltinFunction::Change
                    | rumoca_core::BuiltinFunction::Reinit
                    | rumoca_core::BuiltinFunction::Initial
            ) {
                return true;
            }
            args.iter().any(uses_runtime_discrete_condition)
        }
        rumoca_core::Expression::FunctionCall { name, args, .. } => {
            if is_runtime_discrete_clock_call(name) {
                return true;
            }
            args.iter().any(uses_runtime_discrete_condition)
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            uses_runtime_discrete_condition(lhs) || uses_runtime_discrete_condition(rhs)
        }
        rumoca_core::Expression::Unary { rhs, .. } => uses_runtime_discrete_condition(rhs),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(cond, value)| {
                uses_runtime_discrete_condition(cond) || uses_runtime_discrete_condition(value)
            }) || uses_runtime_discrete_condition(else_branch)
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            elements.iter().any(uses_runtime_discrete_condition)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            uses_runtime_discrete_condition(start)
                || step.as_deref().is_some_and(uses_runtime_discrete_condition)
                || uses_runtime_discrete_condition(end)
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            uses_runtime_discrete_condition(expr)
                || indices
                    .iter()
                    .any(|index| uses_runtime_discrete_condition(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(uses_runtime_discrete_condition)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            uses_runtime_discrete_condition(base)
                || subscripts.iter().any(|sub| match sub {
                    rumoca_core::Subscript::Expr { expr, .. } => {
                        uses_runtime_discrete_condition(expr)
                    }
                    _ => false,
                })
        }
        rumoca_core::Expression::FieldAccess { base, .. } => uses_runtime_discrete_condition(base),
        rumoca_core::Expression::VarRef { .. }
        | rumoca_core::Expression::Literal { value: _, .. }
        | rumoca_core::Expression::Empty { .. } => false,
    }
}

fn is_runtime_discrete_clock_call(name: &rumoca_core::Reference) -> bool {
    // MLS §16.5: Clock constructor/conversion calls may remain FunctionCall
    // nodes in DAE clock metadata. Other Modelica operators must be lowered to
    // BuiltinCall or eliminated before the solve boundary (SPEC_0007).
    let short = name.last_segment();
    matches!(
        short,
        "Clock"
            | "subSample"
            | "superSample"
            | "shiftSample"
            | "backSample"
            | "firstTick"
            | "previous"
            | "hold"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unspanned_root_condition_test_span() -> rumoca_core::Span {
        rumoca_core::Span::DUMMY
    }

    #[test]
    fn checked_relation_offset_end_rejects_overflow_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_root_conditions_source_41.mo",
            ),
            3,
            9,
        );
        let err = checked_relation_offset_end(usize::MAX, 1, span)
            .expect_err("relation offset overflow must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: condition relation scalar offset overflows host index range"
        );
    }

    #[test]
    fn checked_relation_offset_end_rejects_overflow_without_dummy_span() {
        let err = checked_relation_offset_end(usize::MAX, 1, unspanned_root_condition_test_span())
            .expect_err("relation offset overflow must fail");

        assert_eq!(err.source_span(), None);
        assert_eq!(
            err.reason(),
            "invalid IR contract: condition relation scalar offset overflows host index range"
        );
    }

    #[test]
    fn root_vec_with_capacity_rejects_impossible_capacity_without_dummy_span() {
        let err = root_vec_with_capacity::<LinearOp>(
            usize::MAX,
            "root condition row count",
            unspanned_root_condition_test_span(),
        )
        .expect_err("impossible root vector capacity must fail");

        assert_eq!(err.source_span(), None);
        assert_eq!(
            err.reason(),
            "invalid IR contract: root condition row count capacity exceeds host memory limits"
        );
    }

    #[test]
    fn checked_relation_subscript_rejects_i64_overflow_with_span() {
        let Some(index) = usize::try_from(i64::MAX)
            .ok()
            .and_then(|value| value.checked_add(1))
        else {
            return;
        };
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_root_conditions_source_43.mo",
            ),
            7,
            15,
        );

        let err = checked_relation_subscript(index, span)
            .expect_err("relation subscript must fit in Modelica integer range");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            format!(
                "invalid IR contract: condition relation subscript index {index} exceeds i64 range"
            )
        );
    }
}
