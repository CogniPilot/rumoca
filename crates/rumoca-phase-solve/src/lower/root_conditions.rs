use super::*;
use std::collections::HashSet;
use std::sync::Arc;

struct RootRuntime<'a> {
    functions: &'a IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    clock_intervals: &'a IndexMap<String, f64>,
    clock_timings: &'a IndexMap<String, dae::ClockSchedule>,
    triggered_clock_conditions: &'a [rumoca_core::Expression],
    variable_starts: &'a IndexMap<String, rumoca_core::Expression>,
    scalarized_component_child_slots: ScalarizedComponentChildSlotMap,
    indexed_bindings: IndexedBindingMap,
}

pub(super) fn lower_root_conditions(
    dae_model: &dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let scalarized_component_child_slots =
        Arc::new(build_scalarized_component_child_slot_map(layout));
    let indexed_bindings = Arc::new(build_indexed_binding_map(layout));
    let runtime = RootRuntime {
        functions: &dae_model.symbols.functions,
        clock_intervals: &dae_model.clocks.intervals,
        clock_timings: &dae_model.clocks.timings,
        triggered_clock_conditions: &dae_model.clocks.triggered_conditions,
        variable_starts: &dae_model.metadata.variable_starts,
        scalarized_component_child_slots,
        indexed_bindings,
    };
    let mut rows = Vec::with_capacity(
        dae_model.conditions.relations.len()
            + dae_model.events.synthetic_root_conditions.len()
            + dae_model.clocks.triggered_conditions.len(),
    );
    for condition in &dae_model.conditions.relations {
        if root_condition_is_inactive(dae_model, condition) {
            rows.push(lower_inactive_root_row(layout, &runtime));
        } else {
            rows.push(lower_root_condition_row(condition, layout, &runtime)?);
        }
    }
    for condition in &dae_model.events.synthetic_root_conditions {
        if root_condition_is_inactive(dae_model, condition) {
            rows.push(lower_inactive_root_row(layout, &runtime));
        } else {
            rows.push(lower_synthetic_root_condition_row(
                condition, layout, &runtime,
            )?);
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
) -> Vec<Option<rumoca_ir_solve::ScalarSlot>> {
    let mut targets = Vec::with_capacity(
        dae_model.conditions.relations.len()
            + dae_model.events.synthetic_root_conditions.len()
            + dae_model.clocks.triggered_conditions.len(),
    );
    for relation_idx in 0..dae_model.conditions.relations.len() {
        targets.push(condition_memory_slot_for_relation(
            dae_model,
            layout,
            relation_idx,
        ));
    }
    targets.extend(vec![
        None;
        dae_model.events.synthetic_root_conditions.len()
            + dae_model.clocks.triggered_conditions.len()
    ]);
    targets
}

fn lower_root_condition_row(
    condition: &rumoca_core::Expression,
    layout: &VarLayout,
    runtime: &RootRuntime<'_>,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = root_builder(layout, runtime);
    let scope = Scope::new();
    let root_value = match condition {
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => match op {
            rumoca_core::OpBinary::Lt | rumoca_core::OpBinary::Le => {
                let l = builder.lower_expr(lhs, &scope, 0)?;
                let r = builder.lower_expr(rhs, &scope, 0)?;
                builder.emit_binary(BinaryOp::Sub, l, r)
            }
            rumoca_core::OpBinary::Gt | rumoca_core::OpBinary::Ge => {
                let l = builder.lower_expr(lhs, &scope, 0)?;
                let r = builder.lower_expr(rhs, &scope, 0)?;
                builder.emit_binary(BinaryOp::Sub, r, l)
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
) -> Option<rumoca_core::Expression> {
    let mut offset = 0usize;
    for eq in &dae_model.conditions.equations {
        let scalar_count = eq.scalar_count.max(1);
        if relation_idx < offset + scalar_count {
            let lhs = eq.lhs.as_ref()?;
            let flat_index = relation_idx - offset;
            return Some(condition_memory_scalar_expr(
                dae_model,
                lhs,
                flat_index,
                scalar_count,
            ));
        }
        offset += scalar_count;
    }
    None
}

fn condition_memory_slot_for_relation(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    relation_idx: usize,
) -> Option<rumoca_ir_solve::ScalarSlot> {
    let condition_memory = condition_memory_expr_for_relation(dae_model, relation_idx)?;
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = condition_memory
    else {
        return None;
    };
    let key = if subscripts.is_empty() {
        name.to_string()
    } else {
        format!(
            "{}[{}]",
            name,
            subscripts
                .iter()
                .filter_map(|subscript| match subscript {
                    rumoca_core::Subscript::Index { value, .. } => Some(*value),
                    rumoca_core::Subscript::Colon { .. } | rumoca_core::Subscript::Expr { .. } => {
                        None
                    }
                })
                .map(|value| value.to_string())
                .collect::<Vec<_>>()
                .join(",")
        )
    };
    layout.binding(&key)
}

fn condition_memory_scalar_expr(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    flat_index: usize,
    scalar_count: usize,
) -> rumoca_core::Expression {
    if scalar_count <= 1 {
        return rumoca_core::Expression::VarRef {
            name: lhs.clone().into(),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };
    }
    let dims = dae_model
        .variables
        .discrete_valued
        .get(lhs)
        .or_else(|| dae_model.variables.discrete_reals.get(lhs))
        .map(|var| var.dims.as_slice())
        .unwrap_or(&[]);
    let subscripts = dae::flat_index_to_subscripts(dims, flat_index)
        .unwrap_or_else(|| vec![flat_index.saturating_add(1)])
        .into_iter()
        .map(|index| {
            rumoca_core::Subscript::generated_index(index as i64, rumoca_core::Span::DUMMY)
        })
        .collect();
    rumoca_core::Expression::VarRef {
        name: lhs.clone().into(),
        subscripts,
        span: rumoca_core::Span::DUMMY,
    }
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
    let mut builder = root_builder(layout, runtime);
    let root_value = builder.lower_expr(condition, &Scope::new(), 0)?;
    builder.ops.push(LinearOp::StoreOutput { src: root_value });
    Ok(builder.ops)
}

fn lower_triggered_clock_condition_row(
    condition: &rumoca_core::Expression,
    layout: &VarLayout,
    runtime: &RootRuntime<'_>,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = root_builder(layout, runtime);
    let root_value = lower_bool_condition_as_root(condition, &mut builder, &Scope::new())?;
    builder.ops.push(LinearOp::StoreOutput { src: root_value });
    Ok(builder.ops)
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
    let cond = builder.lower_expr(condition, scope, 0)?;
    let neg_one = builder.emit_const(-1.0);
    let pos_one = builder.emit_const(1.0);
    Ok(builder.emit_select(cond, neg_one, pos_one))
}

fn lower_inactive_root_row(layout: &VarLayout, runtime: &RootRuntime<'_>) -> Vec<LinearOp> {
    let mut builder = root_builder(layout, runtime);
    let positive = builder.emit_const(1.0);
    builder.ops.push(LinearOp::StoreOutput { src: positive });
    builder.ops
}

fn root_builder<'a>(layout: &'a VarLayout, runtime: &'a RootRuntime<'a>) -> LowerBuilder<'a> {
    LowerBuilder::new_with_metadata(
        layout,
        runtime.functions,
        LowerBuilderMetadata {
            clock_intervals: Some(runtime.clock_intervals),
            clock_timings: Some(runtime.clock_timings),
            triggered_clock_conditions: Some(runtime.triggered_clock_conditions),
            discrete_valued_names: None,
            variable_starts: Some(runtime.variable_starts),
            indexed_bindings: Some(&runtime.indexed_bindings),
            indexed_record_field_key_index: None,
            scalarized_component_child_slots: Some(&runtime.scalarized_component_child_slots),
            external_object_indices: None,
            current_equation_origin: None,
            is_initial_mode: false,
        },
    )
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
