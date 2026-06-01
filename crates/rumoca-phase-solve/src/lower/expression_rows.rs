use std::sync::Arc;

use indexmap::IndexMap;
use rumoca_core::OpBinary;
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{
    ComputeBlock, ComputeNode, LinearOp, Reg, ScalarProgramBlock, ScalarSlot, UnaryOp, VarLayout,
};

use super::{
    DirectAssignmentValue, IndexedBindingMap, LowerBuilder, LowerBuilderMetadata, LowerError,
    Scope, compile_time, derivative_rhs,
    helpers::{build_indexed_binding_map, parse_indexed_binding_key},
    unsupported_at,
};

pub(super) type LoweredRowsAndTargets = (Vec<Vec<LinearOp>>, Vec<Option<ScalarSlot>>);

struct RowLoweringContext<'a> {
    layout: &'a VarLayout,
    functions: &'a IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    clock_intervals: Option<&'a IndexMap<String, f64>>,
    clock_timings: Option<&'a IndexMap<String, dae::ClockSchedule>>,
    triggered_clock_conditions: Option<&'a [rumoca_core::Expression]>,
    discrete_valued_names: Option<&'a IndexMap<rumoca_core::VarName, dae::Variable>>,
    variable_starts: Option<&'a IndexMap<String, rumoca_core::Expression>>,
    structural_bindings: Option<&'a IndexMap<String, f64>>,
    direct_assignments: Option<&'a IndexMap<String, DirectAssignmentValue>>,
    indexed_bindings: IndexedBindingMap,
    is_initial_mode: bool,
    guard_target_start_before_first_clock_tick: bool,
}

pub(super) struct RuntimeRowMetadata<'a> {
    pub(super) clock_intervals: &'a IndexMap<String, f64>,
    pub(super) clock_timings: &'a IndexMap<String, dae::ClockSchedule>,
    pub(super) triggered_clock_conditions: &'a [rumoca_core::Expression],
    pub(super) discrete_valued_names: &'a IndexMap<rumoca_core::VarName, dae::Variable>,
    pub(super) variable_starts: &'a IndexMap<String, rumoca_core::Expression>,
    pub(super) structural_bindings: Option<&'a IndexMap<String, f64>>,
    pub(super) guard_target_start_before_first_clock_tick: bool,
}

pub fn lower_expression_rows_from_expressions(
    expressions: &[rumoca_core::Expression],
    layout: &VarLayout,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let indexed_bindings = Arc::new(build_indexed_binding_map(layout));
    lower_expression_rows_from_expressions_with_context(
        expressions,
        RowLoweringContext {
            layout,
            functions,
            clock_intervals: None,
            clock_timings: None,
            triggered_clock_conditions: None,
            discrete_valued_names: None,
            variable_starts: None,
            structural_bindings: None,
            direct_assignments: None,
            indexed_bindings,
            is_initial_mode: false,
            guard_target_start_before_first_clock_tick: false,
        },
    )
}

pub fn lower_initial_expression_rows_from_expressions(
    expressions: &[rumoca_core::Expression],
    layout: &VarLayout,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let indexed_bindings = Arc::new(build_indexed_binding_map(layout));
    lower_expression_rows_from_expressions_with_context(
        expressions,
        RowLoweringContext {
            layout,
            functions,
            clock_intervals: None,
            clock_timings: None,
            triggered_clock_conditions: None,
            discrete_valued_names: None,
            variable_starts: None,
            structural_bindings: None,
            direct_assignments: None,
            indexed_bindings,
            is_initial_mode: true,
            guard_target_start_before_first_clock_tick: false,
        },
    )
}

pub fn lower_expression_rows_from_expressions_with_runtime_metadata(
    expressions: &[rumoca_core::Expression],
    layout: &VarLayout,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    clock_intervals: &IndexMap<String, f64>,
    clock_timings: &IndexMap<String, dae::ClockSchedule>,
    variable_starts: &IndexMap<String, rumoca_core::Expression>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let indexed_bindings = Arc::new(build_indexed_binding_map(layout));
    lower_expression_rows_from_expressions_with_context(
        expressions,
        RowLoweringContext {
            layout,
            functions,
            clock_intervals: Some(clock_intervals),
            clock_timings: Some(clock_timings),
            triggered_clock_conditions: None,
            discrete_valued_names: None,
            variable_starts: Some(variable_starts),
            structural_bindings: None,
            direct_assignments: None,
            indexed_bindings,
            is_initial_mode: false,
            guard_target_start_before_first_clock_tick: false,
        },
    )
}

pub fn lower_initial_expression_rows_from_expressions_with_runtime_metadata(
    expressions: &[rumoca_core::Expression],
    layout: &VarLayout,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    clock_intervals: &IndexMap<String, f64>,
    clock_timings: &IndexMap<String, dae::ClockSchedule>,
    variable_starts: &IndexMap<String, rumoca_core::Expression>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let indexed_bindings = Arc::new(build_indexed_binding_map(layout));
    lower_expression_rows_from_expressions_with_context(
        expressions,
        RowLoweringContext {
            layout,
            functions,
            clock_intervals: Some(clock_intervals),
            clock_timings: Some(clock_timings),
            triggered_clock_conditions: None,
            discrete_valued_names: None,
            variable_starts: Some(variable_starts),
            structural_bindings: None,
            direct_assignments: None,
            indexed_bindings,
            is_initial_mode: true,
            guard_target_start_before_first_clock_tick: false,
        },
    )
}

pub(super) fn lower_expression_rows_from_expressions_with_structural_bindings(
    expressions: &[rumoca_core::Expression],
    layout: &VarLayout,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    clock_intervals: &IndexMap<String, f64>,
    clock_timings: &IndexMap<String, dae::ClockSchedule>,
    variable_starts: &IndexMap<String, rumoca_core::Expression>,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let indexed_bindings = Arc::new(build_indexed_binding_map(layout));
    lower_expression_rows_from_expressions_with_context(
        expressions,
        RowLoweringContext {
            layout,
            functions,
            clock_intervals: Some(clock_intervals),
            clock_timings: Some(clock_timings),
            triggered_clock_conditions: None,
            discrete_valued_names: None,
            variable_starts: Some(variable_starts),
            structural_bindings: Some(structural_bindings),
            direct_assignments: None,
            indexed_bindings,
            is_initial_mode: false,
            guard_target_start_before_first_clock_tick: false,
        },
    )
}

pub(super) fn lower_observation_rows_from_expressions_with_structural_bindings(
    expressions: &[rumoca_core::Expression],
    layout: &VarLayout,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    clock_intervals: &IndexMap<String, f64>,
    clock_timings: &IndexMap<String, dae::ClockSchedule>,
    variable_starts: &IndexMap<String, rumoca_core::Expression>,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let indexed_bindings = Arc::new(build_indexed_binding_map(layout));
    lower_observation_rows_from_expressions_with_context(
        expressions,
        RowLoweringContext {
            layout,
            functions,
            clock_intervals: Some(clock_intervals),
            clock_timings: Some(clock_timings),
            triggered_clock_conditions: None,
            discrete_valued_names: None,
            variable_starts: Some(variable_starts),
            structural_bindings: Some(structural_bindings),
            direct_assignments: None,
            indexed_bindings,
            is_initial_mode: false,
            guard_target_start_before_first_clock_tick: false,
        },
    )
}

fn lower_expression_rows_from_expressions_with_context(
    expressions: &[rumoca_core::Expression],
    ctx: RowLoweringContext<'_>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let mut rows = Vec::with_capacity(expressions.len());
    for (row_idx, expression) in expressions.iter().enumerate() {
        rows.push(lower_expression_row(
            expression,
            row_idx as u64,
            None,
            &ctx,
        )?);
    }
    Ok(rows)
}

fn lower_observation_rows_from_expressions_with_context(
    expressions: &[rumoca_core::Expression],
    ctx: RowLoweringContext<'_>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let mut rows = Vec::with_capacity(expressions.len());
    for (row_idx, expression) in expressions.iter().enumerate() {
        let row_namespace = row_idx as u64;
        match lower_expression_row(expression, row_namespace, None, &ctx) {
            Ok(row) => rows.push(row),
            Err(scalar_err) => {
                rows.extend(lower_array_observation_rows(
                    expression,
                    row_namespace,
                    &ctx,
                    scalar_err,
                )?);
            }
        }
    }
    Ok(rows)
}

fn lower_array_observation_rows(
    expression: &rumoca_core::Expression,
    row_namespace: u64,
    ctx: &RowLoweringContext<'_>,
    scalar_err: LowerError,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let mut builder = lower_builder_for_context(ctx, row_namespace);
    let scope = Scope::new();
    let values = match builder.lower_array_like_values(expression, &scope, 0) {
        Ok(values) => values,
        Err(_) => return Err(scalar_err),
    };
    if values.is_empty() {
        return Err(scalar_err);
    }
    Ok(values
        .into_iter()
        .map(|src| {
            let mut ops = builder.ops.clone();
            ops.push(LinearOp::StoreOutput { src });
            ops
        })
        .collect())
}

pub(super) fn lower_expression_rows_with_mode<'a>(
    equations: impl IntoIterator<Item = &'a dae::Equation>,
    layout: &VarLayout,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    runtime: RuntimeRowMetadata<'_>,
    is_initial_mode: bool,
) -> Result<ComputeBlock, LowerError> {
    let equations: Vec<&dae::Equation> = equations.into_iter().collect();
    let mut block = ComputeBlock::default();
    let indexed_bindings = Arc::new(build_indexed_binding_map(layout));
    let ctx = RowLoweringContext {
        layout,
        functions,
        clock_intervals: Some(runtime.clock_intervals),
        clock_timings: Some(runtime.clock_timings),
        triggered_clock_conditions: Some(runtime.triggered_clock_conditions),
        discrete_valued_names: Some(runtime.discrete_valued_names),
        variable_starts: Some(runtime.variable_starts),
        structural_bindings: runtime.structural_bindings,
        direct_assignments: None,
        indexed_bindings,
        is_initial_mode,
        guard_target_start_before_first_clock_tick: runtime
            .guard_target_start_before_first_clock_tick,
    };
    for (row_idx, equation) in equations.iter().enumerate() {
        block.nodes.extend(lower_equation_expression_rows(
            equation,
            row_idx as u64,
            equation.scalar_count.max(1),
            &ctx,
        )?);
    }
    Ok(block)
}

fn lower_equation_expression_rows(
    equation: &dae::Equation,
    row_namespace: u64,
    scalar_count: usize,
    ctx: &RowLoweringContext<'_>,
) -> Result<Vec<ComputeNode>, LowerError> {
    if let Some(rows) = lower_scalarized_record_equation_rows(equation, row_namespace, ctx)? {
        return Ok(vec![ComputeNode::ScalarPrograms(
            ScalarProgramBlock::with_source_span(rows, equation.span),
        )]);
    }

    let mut builder = lower_builder_for_context(ctx, row_namespace);
    let scope = Scope::new();

    // Detect top-level matrix/vector products and emit ComputeNode::MatMul so
    // backends can use BLAS, MLIR linalg.matmul, or accelerator kernels instead
    // of scalar multiply/add chains.
    if let rumoca_core::Expression::Binary {
        op: OpBinary::Mul,
        lhs,
        rhs,
        span,
    } = &equation.rhs
    {
        let lhs_dims = builder.infer_expr_dims(lhs, &scope);
        let rhs_dims = builder.infer_expr_dims(rhs, &scope);
        if let Some(shape) =
            super::array_values::matmul_shape_from_dims(&lhs_dims, &rhs_dims, scalar_count)
        {
            let node = builder.build_matmul_node(lhs, rhs, *span, &scope, 0, shape)?;
            return Ok(vec![node]);
        }
    }

    if scalar_count <= 1 {
        let row = lower_expression_row(
            &equation.rhs,
            row_namespace,
            equation
                .lhs
                .as_ref()
                .and_then(|lhs| ctx.layout.binding(lhs.as_str())),
            ctx,
        )?;
        return Ok(vec![ComputeNode::ScalarPrograms(
            ScalarProgramBlock::with_source_span(vec![row], equation.span),
        )]);
    }

    // MLS §8.3 and §10.6: array-valued equations denote the corresponding
    // scalar element equations. Solve-IR keeps one output row per scalar slot.

    if let Some(node) =
        lower_array_sample_expression_rows(equation, row_namespace, scalar_count, ctx)?
    {
        return Ok(vec![node]);
    }

    let values = builder.lower_array_like_values(&equation.rhs, &scope, 0)?;
    let values = expand_row_values(values, scalar_count)?;
    let rows: Vec<Vec<LinearOp>> = values
        .into_iter()
        .map(|src| {
            let mut ops = builder.ops.clone();
            ops.push(LinearOp::StoreOutput { src });
            ops
        })
        .collect();
    Ok(vec![ComputeNode::ScalarPrograms(
        ScalarProgramBlock::with_source_span(rows, equation.span),
    )])
}

fn lower_scalarized_record_equation_rows(
    equation: &dae::Equation,
    row_namespace: u64,
    ctx: &RowLoweringContext<'_>,
) -> Result<Option<Vec<Vec<LinearOp>>>, LowerError> {
    if let Some(lhs) = equation.lhs.as_ref()
        && let Some(fields) = scalarized_record_fields(lhs.as_str(), ctx.layout)
    {
        let mut rows = Vec::with_capacity(fields.len());
        for (idx, field) in fields.iter().enumerate() {
            let expr =
                scalarized_record_value_projection(equation.rhs.clone(), field, equation.span);
            rows.push(lower_expression_row(
                &expr,
                scalar_row_namespace(row_namespace, idx),
                ctx.layout
                    .binding(format!("{}.{}", lhs.as_str(), field.suffix).as_str()),
                ctx,
            )?);
        }
        return Ok(Some(rows));
    }

    let rumoca_core::Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        span,
    } = &equation.rhs
    else {
        return Ok(None);
    };
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    else {
        return Ok(None);
    };
    if !subscripts.is_empty() {
        return Ok(None);
    }
    let Some(fields) = scalarized_record_fields(name.as_str(), ctx.layout) else {
        return Ok(None);
    };

    let mut rows = Vec::with_capacity(fields.len());
    for (idx, field) in fields.iter().enumerate() {
        let lhs_field = scalarized_record_binding_projection(*lhs.clone(), field, *span);
        let rhs_field = scalarized_record_value_projection(*rhs.clone(), field, *span);
        let residual = rumoca_core::Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(lhs_field),
            rhs: Box::new(rhs_field),
            span: *span,
        };
        rows.push(lower_expression_row(
            &residual,
            scalar_row_namespace(row_namespace, idx),
            ctx.layout
                .binding(format!("{}.{}", name.as_str(), field.suffix).as_str()),
            ctx,
        )?);
    }
    Ok(Some(rows))
}

struct ScalarizedRecordField {
    suffix: String,
    component: String,
    indices: Vec<usize>,
    field_index: usize,
}

fn scalarized_record_fields(base: &str, layout: &VarLayout) -> Option<Vec<ScalarizedRecordField>> {
    if layout.binding(base).is_some() && layout.shape(base).is_none() {
        return None;
    }
    let prefix = format!("{base}.");
    let mut fields = Vec::new();
    for name in layout.bindings().keys() {
        let Some(suffix) = name.strip_prefix(prefix.as_str()) else {
            continue;
        };
        if rumoca_core::has_top_level_dot(suffix) {
            continue;
        }
        if !fields
            .iter()
            .any(|existing: &ScalarizedRecordField| existing.suffix == suffix)
        {
            fields.push(parse_scalarized_record_field(suffix, fields.len()));
        }
    }
    let component_ranks = fields
        .iter()
        .map(|field| (field.component.clone(), field.indices.len()))
        .fold(
            IndexMap::<String, usize>::new(),
            |mut ranks, (component, rank)| {
                let entry = ranks.entry(component).or_default();
                *entry = (*entry).max(rank);
                ranks
            },
        );
    fields.retain(|field| {
        component_ranks
            .get(&field.component)
            .is_none_or(|rank| field.indices.len() == *rank)
    });
    (!fields.is_empty()).then_some(fields)
}

pub(super) fn scalarized_record_field_binding_names(
    base: &str,
    layout: &VarLayout,
) -> Option<Vec<String>> {
    scalarized_record_fields(base, layout).map(|fields| {
        fields
            .into_iter()
            .map(|field| format!("{base}.{}", field.suffix))
            .collect()
    })
}

fn parse_scalarized_record_field(suffix: &str, field_index: usize) -> ScalarizedRecordField {
    let (component, indices) =
        parse_indexed_binding_key(suffix).unwrap_or_else(|| (suffix.to_string(), Vec::new()));
    ScalarizedRecordField {
        suffix: suffix.to_string(),
        component,
        indices,
        field_index,
    }
}

fn scalarized_record_binding_projection(
    base: rumoca_core::Expression,
    field: &ScalarizedRecordField,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    let access = rumoca_core::Expression::FieldAccess {
        base: Box::new(base),
        field: field.component.clone(),
        span,
    };
    if field.indices.is_empty() {
        return access;
    }
    rumoca_core::Expression::Index {
        base: Box::new(access),
        subscripts: field
            .indices
            .iter()
            .map(|index| rumoca_core::Subscript::generated_index(*index as i64, span))
            .collect(),
        span,
    }
}

fn scalarized_record_value_projection(
    base: rumoca_core::Expression,
    field: &ScalarizedRecordField,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    if field.indices.is_empty()
        && let Some(element) = literal_record_field_element(&base, field.field_index)
    {
        return element;
    }
    if field.indices.is_empty() {
        return rumoca_core::Expression::FieldAccess {
            base: Box::new(base),
            field: field.component.clone(),
            span,
        };
    }
    if matches!(
        base,
        rumoca_core::Expression::Array { .. } | rumoca_core::Expression::Tuple { .. }
    ) {
        let indexed = indexed_record_value(base, field, span);
        return rumoca_core::Expression::FieldAccess {
            base: Box::new(indexed),
            field: field.component.clone(),
            span,
        };
    }
    let access = rumoca_core::Expression::FieldAccess {
        base: Box::new(base),
        field: field.component.clone(),
        span,
    };
    rumoca_core::Expression::Index {
        base: Box::new(access),
        subscripts: field
            .indices
            .iter()
            .map(|index| rumoca_core::Subscript::generated_index(*index as i64, span))
            .collect(),
        span,
    }
}

fn literal_record_field_element(
    base: &rumoca_core::Expression,
    field_index: usize,
) -> Option<rumoca_core::Expression> {
    match base {
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => elements.get(field_index).cloned(),
        _ => None,
    }
}

fn indexed_record_value(
    base: rumoca_core::Expression,
    field: &ScalarizedRecordField,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Index {
        base: Box::new(base),
        subscripts: field
            .indices
            .iter()
            .map(|index| rumoca_core::Subscript::generated_index(*index as i64, span))
            .collect(),
        span,
    }
}

fn lower_array_sample_expression_rows(
    equation: &dae::Equation,
    row_namespace: u64,
    scalar_count: usize,
    ctx: &RowLoweringContext<'_>,
) -> Result<Option<ComputeNode>, LowerError> {
    let rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sample,
        args,
        ..
    } = &equation.rhs
    else {
        return Ok(None);
    };
    if !matches!(args.as_slice(), [_] | [_, _]) {
        return Ok(None);
    }
    let Some(targets) = scalar_update_targets(equation, scalar_count, ctx.layout) else {
        return Ok(None);
    };

    let probe = lower_builder_for_context(ctx, row_namespace);
    let scope = Scope::new();
    let dims = probe.infer_expr_dims(&args[0], &scope);
    if dims.iter().product::<usize>() != scalar_count {
        return Ok(None);
    }

    let mut rows = Vec::with_capacity(scalar_count);
    for (flat_index, target) in targets.into_iter().enumerate() {
        let expr = scalar_sample_expression(args, &dims, flat_index)?;
        rows.push(lower_expression_row(
            &expr,
            scalar_row_namespace(row_namespace, flat_index),
            Some(target),
            ctx,
        )?);
    }
    Ok(Some(ComputeNode::ScalarPrograms(
        ScalarProgramBlock::with_source_span(rows, equation.span),
    )))
}

fn scalar_update_targets(
    equation: &dae::Equation,
    scalar_count: usize,
    layout: &VarLayout,
) -> Option<Vec<ScalarSlot>> {
    let lhs = equation.lhs.as_ref()?;
    let targets = layout
        .bindings()
        .iter()
        .filter_map(|(name, slot)| {
            (name != lhs.as_str()
                && dae::component_base_name(name).as_deref() == Some(lhs.as_str()))
            .then_some(*slot)
        })
        .take(scalar_count + 1)
        .collect::<Vec<_>>();
    (targets.len() == scalar_count).then_some(targets)
}

fn scalar_sample_expression(
    args: &[rumoca_core::Expression],
    dims: &[usize],
    flat_index: usize,
) -> Result<rumoca_core::Expression, LowerError> {
    let scalar_value = indexed_sample_value(&args[0], dims, flat_index)?;
    let mut scalar_args = Vec::with_capacity(args.len());
    scalar_args.push(scalar_value);
    scalar_args.extend(args.iter().skip(1).cloned());
    Ok(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sample,
        args: scalar_args,
        span: rumoca_core::Span::DUMMY,
    })
}

fn indexed_sample_value(
    value: &rumoca_core::Expression,
    dims: &[usize],
    flat_index: usize,
) -> Result<rumoca_core::Expression, LowerError> {
    let dims_i64 = dims.iter().map(|dim| *dim as i64).collect::<Vec<_>>();
    let Some(subscripts) = dae::flat_index_to_subscripts(&dims_i64, flat_index) else {
        return Err(LowerError::Unsupported {
            reason: format!("sample array index {flat_index} is outside shape {dims:?}"),
        });
    };
    Ok(rumoca_core::Expression::Index {
        base: Box::new(value.clone()),
        subscripts: subscripts
            .into_iter()
            .map(|idx| {
                rumoca_core::Subscript::generated_index(idx as i64, rumoca_core::Span::DUMMY)
            })
            .collect(),
        span: rumoca_core::Span::DUMMY,
    })
}

fn scalar_row_namespace(row_namespace: u64, flat_index: usize) -> u64 {
    row_namespace
        .saturating_mul(1_000_000)
        .saturating_add(flat_index as u64)
}

fn expand_row_values(values: Vec<Reg>, scalar_count: usize) -> Result<Vec<Reg>, LowerError> {
    match values.len() {
        1 => Ok(vec![values[0]; scalar_count]),
        len if len == scalar_count => Ok(values),
        len => Err(LowerError::Unsupported {
            reason: format!(
                "array expression width {len} does not match equation scalar count {scalar_count}"
            ),
        }),
    }
}

pub(super) fn lower_residual_rows_with_mode(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    is_initial_mode: bool,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let n_x: usize = dae_model.variables.states.values().map(|v| v.size()).sum();
    lower_residual_rows_from_equations_with_mode(
        dae_model,
        layout,
        dae_model.continuous.equations.iter().enumerate(),
        n_x,
        is_initial_mode,
    )
}

pub(super) fn lower_residual_rows_from_equations_with_mode<'a>(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    equations: impl IntoIterator<Item = (usize, &'a dae::Equation)>,
    state_scalar_count: usize,
    is_initial_mode: bool,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    lower_residual_rows_from_equations_core(
        dae_model,
        layout,
        equations,
        state_scalar_count,
        is_initial_mode,
        |_, _| Ok(()),
    )
}

pub(super) fn lower_residual_rows_and_targets_from_equations_with_mode<'a>(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    equations: impl IntoIterator<Item = (usize, &'a dae::Equation)>,
    state_scalar_count: usize,
    is_initial_mode: bool,
    mut target_rows_for_equation: impl FnMut(
        &dae::Equation,
        usize,
    ) -> Result<Vec<Option<ScalarSlot>>, LowerError>,
) -> Result<LoweredRowsAndTargets, LowerError> {
    let mut targets = Vec::new();
    let rows = lower_residual_rows_from_equations_core(
        dae_model,
        layout,
        equations,
        state_scalar_count,
        is_initial_mode,
        |eq, row_count| {
            let eq_targets = target_rows_for_equation(eq, row_count)?;
            if eq_targets.len() != row_count {
                return Err(LowerError::Unsupported {
                    reason: format!(
                        "continuous row target count {} does not match lowered row count {} for equation `{}`",
                        eq_targets.len(),
                        row_count,
                        eq.origin
                    ),
                });
            }
            targets.extend(eq_targets);
            Ok(())
        },
    )?;
    Ok((rows, targets))
}

fn lower_residual_rows_from_equations_core<'a>(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    equations: impl IntoIterator<Item = (usize, &'a dae::Equation)>,
    state_scalar_count: usize,
    is_initial_mode: bool,
    mut after_equation: impl FnMut(&dae::Equation, usize) -> Result<(), LowerError>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let structural_bindings = compile_time::structural_bindings(dae_model);
    let indexed_bindings = Arc::new(build_indexed_binding_map(layout));
    let state_names = dae_model
        .variables
        .states
        .keys()
        .map(|name| name.as_str().to_string())
        .collect::<Vec<_>>();
    let direct_assignments = derivative_rhs::collect_missing_indexed_record_field_assignments(
        dae_model,
        &state_names,
        layout,
        &structural_bindings,
    );
    let equations: Vec<(usize, &dae::Equation)> = equations.into_iter().collect();
    let mut rows = Vec::with_capacity(equations.len());
    for (row_idx, eq) in equations {
        let start = rows.len();
        let ctx = RowLoweringContext {
            layout,
            functions: &dae_model.symbols.functions,
            clock_intervals: Some(&dae_model.clocks.intervals),
            clock_timings: Some(&dae_model.clocks.timings),
            triggered_clock_conditions: Some(&dae_model.clocks.triggered_conditions),
            discrete_valued_names: Some(&dae_model.variables.discrete_valued),
            variable_starts: Some(&dae_model.metadata.variable_starts),
            structural_bindings: Some(&structural_bindings),
            direct_assignments: Some(&direct_assignments),
            indexed_bindings: Arc::clone(&indexed_bindings),
            is_initial_mode,
            guard_target_start_before_first_clock_tick: false,
        };
        if let Some(record_rows) =
            lower_scalarized_record_residual_rows(eq, row_idx, state_scalar_count, &ctx)?
        {
            rows.extend(record_rows);
            let row_count = rows.len() - start;
            validate_equation_row_count(eq, row_count)?;
            after_equation(eq, row_count)?;
            continue;
        }

        rows.extend(lower_equation_residual_rows(
            eq,
            row_idx,
            state_scalar_count,
            &ctx,
        )?);
        let row_count = rows.len() - start;
        validate_equation_row_count(eq, row_count)?;
        after_equation(eq, row_count)?;
    }
    Ok(rows)
}

fn validate_equation_row_count(eq: &dae::Equation, actual: usize) -> Result<(), LowerError> {
    let expected = eq.scalar_count.max(1);
    if actual == expected {
        return Ok(());
    }
    Err(LowerError::ContractViolation {
        reason: format!(
            "equation `{}` declares scalar_count {expected} but lowered to {actual} residual rows",
            eq.origin
        ),
        span: eq.span,
    })
}

fn lower_equation_residual_rows(
    eq: &dae::Equation,
    row_idx: usize,
    state_scalar_count: usize,
    ctx: &RowLoweringContext<'_>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let mut builder = lower_builder_for_context(ctx, row_idx as u64);
    let scope = Scope::new();
    let residual_expr;
    let expr = if row_idx >= state_scalar_count
        && let Some(lhs) = eq.lhs.as_ref()
    {
        // MLS §8.3: explicit equations are still equations in the simultaneous
        // system. Solver residual rows represent `lhs - rhs = 0`, with
        // array-valued equations expanded element-wise per MLS §10.6.
        residual_expr = rumoca_core::Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: lhs.clone().into(),
                subscripts: Vec::new(),
                span: eq.span,
            }),
            rhs: Box::new(eq.rhs.clone()),
            span: eq.span,
        };
        &residual_expr
    } else {
        &eq.rhs
    };

    let scalar_count = eq.scalar_count.max(1);
    let values = if scalar_count == 1 {
        vec![
            builder
                .lower_expr(expr, &scope, 0)
                .map_err(|err| residual_row_context(err, row_idx, eq))?,
        ]
    } else {
        let values = builder
            .lower_array_like_values(expr, &scope, 0)
            .map_err(|err| residual_row_context(err, row_idx, eq))?;
        expand_row_values(values, scalar_count)
            .map_err(|err| residual_row_context(err, row_idx, eq))?
    };

    let values = if row_idx < state_scalar_count {
        values
            .into_iter()
            .map(|value| builder.emit_unary(UnaryOp::Neg, value))
            .collect()
    } else {
        values
    };

    let mut rows = Vec::with_capacity(scalar_count);
    for value in values {
        let mut ops = builder.ops.clone();
        ops.push(LinearOp::StoreOutput { src: value });
        rows.push(ops);
    }
    Ok(rows)
}

fn lower_scalarized_record_residual_rows(
    eq: &dae::Equation,
    row_idx: usize,
    state_scalar_count: usize,
    ctx: &RowLoweringContext<'_>,
) -> Result<Option<Vec<Vec<LinearOp>>>, LowerError> {
    if row_idx < state_scalar_count {
        return Ok(None);
    }

    if let Some(lhs) = eq.lhs.as_ref()
        && let Some(fields) = scalarized_record_fields(lhs.as_str(), ctx.layout)
    {
        let residuals = fields.iter().map(|field| {
            let lhs_base = rumoca_core::Expression::VarRef {
                name: lhs.clone().into(),
                subscripts: Vec::new(),
                span: eq.span,
            };
            let lhs_field = scalarized_record_binding_projection(lhs_base, field, eq.span);
            let rhs_field = scalarized_record_value_projection(eq.rhs.clone(), field, eq.span);
            rumoca_core::Expression::Binary {
                op: OpBinary::Sub,
                lhs: Box::new(lhs_field),
                rhs: Box::new(rhs_field),
                span: eq.span,
            }
        });
        return lower_scalarized_residual_expressions(residuals, row_idx, eq, ctx).map(Some);
    }

    let rumoca_core::Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        span,
    } = &eq.rhs
    else {
        return Ok(None);
    };
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    else {
        return Ok(None);
    };
    if !subscripts.is_empty() {
        return Ok(None);
    }
    let Some(fields) = scalarized_record_fields(name.as_str(), ctx.layout) else {
        return Ok(None);
    };

    let residuals = fields.iter().map(|field| {
        let lhs_field = scalarized_record_binding_projection(*lhs.clone(), field, *span);
        let rhs_field = scalarized_record_value_projection(*rhs.clone(), field, *span);
        rumoca_core::Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(lhs_field),
            rhs: Box::new(rhs_field),
            span: *span,
        }
    });
    lower_scalarized_residual_expressions(residuals, row_idx, eq, ctx).map(Some)
}

fn lower_scalarized_residual_expressions(
    residuals: impl IntoIterator<Item = rumoca_core::Expression>,
    row_idx: usize,
    eq: &dae::Equation,
    ctx: &RowLoweringContext<'_>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let mut rows = Vec::new();
    for (field_idx, residual) in residuals.into_iter().enumerate() {
        let row = lower_expression_row(
            &residual,
            scalar_row_namespace(row_idx as u64, field_idx),
            None,
            ctx,
        )
        .map_err(|err| residual_row_context(err, row_idx, eq))?;
        rows.push(row);
    }
    Ok(rows)
}

fn residual_row_context(err: LowerError, row_idx: usize, eq: &dae::Equation) -> LowerError {
    let context = format!(
        "continuous residual row {row_idx} (origin={} scalar_count={})",
        eq.origin, eq.scalar_count
    );
    match err.with_fallback_span(eq.span) {
        err @ (LowerError::Unsupported { .. } | LowerError::UnsupportedAt { .. }) => {
            err.with_context(context)
        }
        err @ LowerError::Spanned { .. } => err.with_context(context),
        err @ LowerError::ContractViolation { .. } => err.with_context(context),
        LowerError::MissingBinding { name } => {
            unsupported_at(format!("missing variable binding `{name}`"), eq.span)
                .with_context(context)
        }
        LowerError::MissingFunction { name } => {
            unsupported_at(format!("missing function `{name}`"), eq.span).with_context(context)
        }
        LowerError::InvalidFunction { name, reason } => {
            unsupported_at(format!("invalid function `{name}`: {reason}"), eq.span)
                .with_context(context)
        }
    }
}

fn lower_expression_row(
    expression: &rumoca_core::Expression,
    row_namespace: u64,
    current_update_target: Option<rumoca_ir_solve::ScalarSlot>,
    ctx: &RowLoweringContext<'_>,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = lower_builder_for_context(ctx, row_namespace)
        .with_current_update_target(current_update_target);
    let scope = Scope::new();
    let value = builder.lower_expr(expression, &scope, 0)?;
    let value = if ctx.guard_target_start_before_first_clock_tick {
        builder.lower_current_update_target_start_before_first_clock_tick(
            value, expression, &scope, 0,
        )?
    } else {
        value
    };
    builder.ops.push(LinearOp::StoreOutput { src: value });
    Ok(builder.ops)
}

fn lower_builder_for_context<'a>(
    ctx: &'a RowLoweringContext<'a>,
    row_namespace: u64,
) -> LowerBuilder<'a> {
    LowerBuilder::new_with_metadata(
        ctx.layout,
        ctx.functions,
        LowerBuilderMetadata {
            clock_intervals: ctx.clock_intervals,
            clock_timings: ctx.clock_timings,
            triggered_clock_conditions: ctx.triggered_clock_conditions,
            discrete_valued_names: ctx.discrete_valued_names,
            variable_starts: ctx.variable_starts,
            indexed_bindings: Some(&ctx.indexed_bindings),
            is_initial_mode: ctx.is_initial_mode,
        },
    )
    .with_structural_bindings(ctx.structural_bindings.cloned().unwrap_or_default())
    .with_direct_assignments(ctx.direct_assignments.cloned().unwrap_or_default())
    .with_call_site_namespace(row_namespace)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn layout_with_bindings(names: &[&str]) -> VarLayout {
        let mut bindings = IndexMap::new();
        for (index, name) in names.iter().enumerate() {
            bindings.insert(
                (*name).to_string(),
                ScalarSlot::Y {
                    index,
                    byte_offset: index * std::mem::size_of::<f64>(),
                },
            );
        }
        VarLayout::from_parts(bindings, names.len(), 0)
    }

    #[test]
    fn scalarized_record_fields_ignore_only_top_level_nested_suffixes() {
        let layout = layout_with_bindings(&[
            "state.p",
            "state.v[index.with.dot]",
            "state.nested.q",
            "other.p",
        ]);

        let fields = scalarized_record_fields("state", &layout).expect("state fields");
        let suffixes = fields
            .iter()
            .map(|field| field.suffix.as_str())
            .collect::<Vec<_>>();

        assert!(suffixes.contains(&"p"));
        assert!(suffixes.contains(&"v[index.with.dot]"));
        assert!(!suffixes.contains(&"nested.q"));
    }

    #[test]
    fn scalarized_record_fields_ignore_scalar_binding_with_enum_alias_prefix() {
        let layout = layout_with_bindings(&["Th", "Th.default"]);

        assert!(scalarized_record_fields("Th", &layout).is_none());
    }
}
