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
    helpers::{
        build_indexed_binding_map, format_usize_dims, parse_indexed_binding_key, variable_size,
    },
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
    dae_variables: Option<&'a dae::DaeVariables>,
    structural_bindings: Option<Arc<IndexMap<String, f64>>>,
    direct_assignments: Option<Arc<IndexMap<String, DirectAssignmentValue>>>,
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
    pub(super) dae_variables: Option<&'a dae::DaeVariables>,
    pub(super) structural_bindings: Option<Arc<IndexMap<String, f64>>>,
    pub(super) guard_target_start_before_first_clock_tick: bool,
}

fn expression_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    reserve_expression_capacity(&mut values, capacity, context, span)?;
    Ok(values)
}

fn reserve_expression_capacity<T>(
    values: &mut Vec<T>,
    additional: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    values.try_reserve_exact(additional).map_err(|_| {
        LowerError::contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

fn expression_vec_with_optional_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    reserve_expression_optional_capacity(&mut values, capacity, context, span)?;
    Ok(values)
}

fn reserve_expression_optional_capacity<T>(
    values: &mut Vec<T>,
    additional: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), LowerError> {
    values.try_reserve_exact(additional).map_err(|_| {
        expression_contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

fn expression_contract_violation(
    reason: impl Into<String>,
    span: Option<rumoca_core::Span>,
) -> LowerError {
    match span.filter(|span| !span.is_dummy()) {
        Some(span) => LowerError::contract_violation(reason, span),
        None => LowerError::UnspannedContractViolation {
            reason: reason.into(),
        },
    }
}

fn expression_context_span(expressions: &[rumoca_core::Expression]) -> Option<rumoca_core::Span> {
    expressions
        .iter()
        .find_map(|expression| expression.span().filter(|span| !span.is_dummy()))
}

fn linear_ops_with_store(
    base_ops: &[LinearOp],
    src: Reg,
    span: Option<rumoca_core::Span>,
) -> Result<Vec<LinearOp>, LowerError> {
    let capacity = base_ops.len().checked_add(1).ok_or_else(|| {
        expression_contract_violation("expression row op count exceeds host memory limits", span)
    })?;
    let mut ops = expression_vec_with_optional_capacity(capacity, "expression row op count", span)?;
    ops.extend_from_slice(base_ops);
    ops.push(LinearOp::StoreOutput { src });
    Ok(ops)
}

fn scalar_program_node(
    rows: Vec<Vec<LinearOp>>,
    span: rumoca_core::Span,
) -> Result<ComputeNode, LowerError> {
    Ok(ComputeNode::ScalarPrograms(
        ScalarProgramBlock::with_source_span(rows, span),
    ))
}

fn single_compute_node(
    node: ComputeNode,
    span: rumoca_core::Span,
) -> Result<Vec<ComputeNode>, LowerError> {
    let mut nodes = expression_vec_with_capacity(1, "single compute node count", span)?;
    nodes.push(node);
    Ok(nodes)
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
            dae_variables: None,
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
            dae_variables: None,
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
            dae_variables: None,
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
            dae_variables: None,
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
    metadata: RuntimeRowMetadata<'_>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let indexed_bindings = Arc::new(build_indexed_binding_map(layout));
    lower_expression_rows_from_expressions_with_context(
        expressions,
        RowLoweringContext {
            layout,
            functions,
            clock_intervals: Some(metadata.clock_intervals),
            clock_timings: Some(metadata.clock_timings),
            triggered_clock_conditions: None,
            discrete_valued_names: None,
            variable_starts: Some(metadata.variable_starts),
            dae_variables: metadata.dae_variables,
            structural_bindings: metadata.structural_bindings,
            direct_assignments: None,
            indexed_bindings,
            is_initial_mode: false,
            guard_target_start_before_first_clock_tick: metadata
                .guard_target_start_before_first_clock_tick,
        },
    )
}

pub(super) fn lower_observation_rows_from_expressions_with_structural_bindings(
    expressions: &[rumoca_core::Expression],
    layout: &VarLayout,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    metadata: RuntimeRowMetadata<'_>,
    indexed_bindings: IndexedBindingMap,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    lower_observation_rows_from_expressions_with_context(
        expressions,
        RowLoweringContext {
            layout,
            functions,
            clock_intervals: Some(metadata.clock_intervals),
            clock_timings: Some(metadata.clock_timings),
            triggered_clock_conditions: None,
            discrete_valued_names: None,
            variable_starts: Some(metadata.variable_starts),
            dae_variables: metadata.dae_variables,
            structural_bindings: metadata.structural_bindings,
            direct_assignments: None,
            indexed_bindings,
            is_initial_mode: false,
            guard_target_start_before_first_clock_tick: metadata
                .guard_target_start_before_first_clock_tick,
        },
    )
}

fn lower_expression_rows_from_expressions_with_context(
    expressions: &[rumoca_core::Expression],
    ctx: RowLoweringContext<'_>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let span = expression_context_span(expressions);
    let mut rows =
        expression_vec_with_optional_capacity(expressions.len(), "expression row count", span)?;
    for (row_idx, expression) in expressions.iter().enumerate() {
        let row_namespace = row_namespace_from_usize(row_idx, expression.span())?;
        rows.push(lower_expression_row(
            expression,
            row_namespace,
            None,
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
    let span = expression_context_span(expressions);
    let mut rows =
        expression_vec_with_optional_capacity(expressions.len(), "observation row count", span)?;
    for (row_idx, expression) in expressions.iter().enumerate() {
        let row_namespace = row_namespace_from_usize(row_idx, expression.span())?;
        match lower_expression_row(expression, row_namespace, None, None, &ctx) {
            Ok(row) => rows.push(row),
            Err(scalar_err) => {
                let array_rows =
                    lower_array_observation_rows(expression, row_namespace, &ctx, scalar_err)?;
                reserve_expression_optional_capacity(
                    &mut rows,
                    array_rows.len(),
                    "array observation row count",
                    expression.span().or(span),
                )?;
                rows.extend(array_rows);
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
    let values = match expression
        .span()
        .filter(|span| !span.is_dummy())
        .map(|span| {
            builder.lower_array_like_values_with_source_context(expression, span, &scope, 0)
        })
        .unwrap_or_else(|| builder.lower_array_like_values(expression, &scope, 0))
    {
        Ok(values) => values,
        Err(_) => return Err(scalar_err),
    };
    if values.is_empty() {
        return Err(scalar_err);
    }
    let span = expression.span();
    let mut rows =
        expression_vec_with_optional_capacity(values.len(), "array observation row count", span)?;
    for src in values {
        rows.push(linear_ops_with_store(&builder.ops, src, span)?);
    }
    Ok(rows)
}

pub(super) fn lower_expression_rows_with_mode<'a>(
    equations: impl IntoIterator<Item = &'a dae::Equation>,
    layout: &VarLayout,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    runtime: RuntimeRowMetadata<'_>,
    is_initial_mode: bool,
) -> Result<ComputeBlock, LowerError> {
    let equation_iter = equations.into_iter();
    let (lower_bound, upper_bound) = equation_iter.size_hint();
    let mut equations = expression_vec_with_optional_capacity(
        upper_bound.unwrap_or(lower_bound),
        "expression equation count",
        None,
    )?;
    for equation in equation_iter {
        equations.push(equation);
    }
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
        dae_variables: runtime.dae_variables,
        structural_bindings: runtime.structural_bindings,
        direct_assignments: None,
        indexed_bindings,
        is_initial_mode,
        guard_target_start_before_first_clock_tick: runtime
            .guard_target_start_before_first_clock_tick,
    };
    for (row_idx, equation) in equations.iter().enumerate() {
        let row_namespace = row_namespace_from_usize(row_idx, Some(equation.span))?;
        let nodes = lower_equation_expression_rows(
            equation,
            row_namespace,
            equation.scalar_count.max(1),
            &ctx,
        )?;
        reserve_expression_capacity(
            &mut block.nodes,
            nodes.len(),
            "expression compute node count",
            equation.span,
        )?;
        block.nodes.extend(nodes);
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
        return single_compute_node(scalar_program_node(rows, equation.span)?, equation.span);
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
        let lhs_dims = builder.infer_expr_dims(lhs, &scope)?;
        let rhs_dims = builder.infer_expr_dims(rhs, &scope)?;
        if let Some(shape) =
            super::array_values::matmul_shape_from_dims(&lhs_dims, &rhs_dims, scalar_count)
        {
            let matmul_span = if span.is_dummy() {
                equation.span
            } else {
                *span
            };
            if matmul_span.is_dummy() {
                return Err(LowerError::UnspannedContractViolation {
                    reason: "MatMul expression row lowering requires a source span".to_string(),
                });
            }
            let node = builder.with_optional_source_context(Some(matmul_span), |builder| {
                builder.build_matmul_node(lhs, rhs, matmul_span, &scope, 0, shape)
            })?;
            return single_compute_node(node, matmul_span);
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
            Some(equation.span),
            ctx,
        )?;
        let mut rows =
            expression_vec_with_capacity(1, "scalar expression row count", equation.span)?;
        rows.push(row);
        return single_compute_node(scalar_program_node(rows, equation.span)?, equation.span);
    }

    // MLS §8.3 and §10.6: array-valued equations denote the corresponding
    // scalar element equations. Solve-IR keeps one output row per scalar slot.

    if let Some(node) =
        lower_array_sample_expression_rows(equation, row_namespace, scalar_count, ctx)?
    {
        return single_compute_node(node, equation.span);
    }

    let values = builder.lower_array_like_values_with_source_context(
        &equation.rhs,
        equation.span,
        &scope,
        0,
    )?;
    let values = expand_row_values(values, scalar_count, equation.span)?;
    let mut rows =
        expression_vec_with_capacity(values.len(), "array expression row count", equation.span)?;
    for src in values {
        rows.push(linear_ops_with_store(
            &builder.ops,
            src,
            Some(equation.span),
        )?);
    }
    single_compute_node(scalar_program_node(rows, equation.span)?, equation.span)
}

fn lower_scalarized_record_equation_rows(
    equation: &dae::Equation,
    row_namespace: u64,
    ctx: &RowLoweringContext<'_>,
) -> Result<Option<Vec<Vec<LinearOp>>>, LowerError> {
    if let Some(lhs) = equation.lhs.as_ref()
        && let Some(fields) = scalarized_record_fields(lhs.as_str(), ctx.layout)
    {
        let mut rows = expression_vec_with_capacity(
            fields.len(),
            "scalarized record expression row count",
            equation.span,
        )?;
        for (idx, field) in fields.iter().enumerate() {
            let expr =
                scalarized_record_value_projection(equation.rhs.clone(), field, equation.span)?;
            rows.push(lower_expression_row(
                &expr,
                scalar_row_namespace(row_namespace, idx, equation.span)?,
                ctx.layout
                    .binding(format!("{}.{}", lhs.as_str(), field.suffix).as_str()),
                Some(equation.span),
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

    let projection_span = if span.is_dummy() {
        equation.span
    } else {
        *span
    };
    let mut rows = expression_vec_with_capacity(
        fields.len(),
        "scalarized record binary row count",
        projection_span,
    )?;
    for (idx, field) in fields.iter().enumerate() {
        let lhs_field = scalarized_record_binding_projection(*lhs.clone(), field, projection_span)?;
        let rhs_field = scalarized_record_value_projection(*rhs.clone(), field, projection_span)?;
        let residual = rumoca_core::Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(lhs_field),
            rhs: Box::new(rhs_field),
            span: projection_span,
        };
        rows.push(lower_expression_row(
            &residual,
            scalar_row_namespace(row_namespace, idx, projection_span)?,
            ctx.layout
                .binding(format!("{}.{}", name.as_str(), field.suffix).as_str()),
            Some(projection_span),
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
        if crate::path_utils::is_nested_name(suffix) {
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
) -> Result<rumoca_core::Expression, LowerError> {
    let access = rumoca_core::Expression::FieldAccess {
        base: Box::new(base),
        field: field.component.clone(),
        span,
    };
    if field.indices.is_empty() {
        return Ok(access);
    }
    Ok(rumoca_core::Expression::Index {
        base: Box::new(access),
        subscripts: generated_subscripts_from_usize(&field.indices, span)?,
        span,
    })
}

fn scalarized_record_value_projection(
    base: rumoca_core::Expression,
    field: &ScalarizedRecordField,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, LowerError> {
    if field.indices.is_empty()
        && let Some(element) = literal_record_field_element(&base, field.field_index)
    {
        return Ok(element);
    }
    if field.indices.is_empty() {
        return Ok(rumoca_core::Expression::FieldAccess {
            base: Box::new(base),
            field: field.component.clone(),
            span,
        });
    }
    if matches!(
        base,
        rumoca_core::Expression::Array { .. } | rumoca_core::Expression::Tuple { .. }
    ) {
        let indexed = indexed_record_value(base, field, span)?;
        return Ok(rumoca_core::Expression::FieldAccess {
            base: Box::new(indexed),
            field: field.component.clone(),
            span,
        });
    }
    let access = rumoca_core::Expression::FieldAccess {
        base: Box::new(base),
        field: field.component.clone(),
        span,
    };
    Ok(rumoca_core::Expression::Index {
        base: Box::new(access),
        subscripts: generated_subscripts_from_usize(&field.indices, span)?,
        span,
    })
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
) -> Result<rumoca_core::Expression, LowerError> {
    Ok(rumoca_core::Expression::Index {
        base: Box::new(base),
        subscripts: generated_subscripts_from_usize(&field.indices, span)?,
        span,
    })
}

fn generated_subscripts_from_usize(
    indices: &[usize],
    span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Subscript>, LowerError> {
    let mut subscripts =
        expression_vec_with_capacity(indices.len(), "generated subscript count", span)?;
    for index in indices {
        subscripts.push(generated_subscript_from_usize(*index, span)?);
    }
    Ok(subscripts)
}

fn lower_array_sample_expression_rows(
    equation: &dae::Equation,
    row_namespace: u64,
    scalar_count: usize,
    ctx: &RowLoweringContext<'_>,
) -> Result<Option<ComputeNode>, LowerError> {
    let Some((args, span)) = sample_expression_args(&equation.rhs) else {
        return Ok(None);
    };
    if !matches!(args, [_] | [_, _]) {
        return Err(LowerError::contract_violation(
            format!(
                "array-valued sample update requires 1 or 2 arguments, got {}",
                args.len()
            ),
            span,
        ));
    }
    let targets = scalar_update_targets(equation, scalar_count, ctx.layout)?.ok_or_else(|| {
        unsupported_at(
            "array-valued sample update target does not have matching scalar bindings",
            span,
        )
    })?;

    let probe = lower_builder_for_context(ctx, row_namespace);
    let scope = Scope::new();
    let dims = probe.infer_expr_dims(&args[0], &scope)?;
    if dims.iter().product::<usize>() != scalar_count {
        return Err(unsupported_at(
            format!(
                "array-valued sample source shape {dims:?} does not match scalar count {scalar_count}"
            ),
            span,
        ));
    }

    let mut rows =
        expression_vec_with_capacity(scalar_count, "array sample row count", equation.span)?;
    for (flat_index, target) in targets.into_iter().enumerate() {
        let expr = scalar_sample_expression(args, &dims, flat_index, span)?;
        rows.push(lower_expression_row(
            &expr,
            scalar_row_namespace(row_namespace, flat_index, span)?,
            Some(target),
            Some(equation.span),
            ctx,
        )?);
    }
    Ok(Some(ComputeNode::ScalarPrograms(
        ScalarProgramBlock::with_source_span(rows, equation.span),
    )))
}

fn sample_expression_args(
    expr: &rumoca_core::Expression,
) -> Option<(&[rumoca_core::Expression], rumoca_core::Span)> {
    match expr {
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sample,
            args,
            span,
        } => Some((args, *span)),
        rumoca_core::Expression::FunctionCall {
            name, args, span, ..
        } if name.as_str() == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME => Some((args, *span)),
        _ => None,
    }
}

fn scalar_update_targets(
    equation: &dae::Equation,
    scalar_count: usize,
    layout: &VarLayout,
) -> Result<Option<Vec<ScalarSlot>>, LowerError> {
    let Some(lhs) = equation.lhs.as_ref() else {
        return Ok(None);
    };
    let mut targets =
        expression_vec_with_capacity(scalar_count, "scalar update target count", equation.span)?;
    for (name, slot) in layout.bindings() {
        if name != lhs.as_str() && dae::component_base_name(name).as_deref() == Some(lhs.as_str()) {
            if targets.len() == scalar_count {
                return Ok(None);
            }
            targets.push(*slot);
        }
    }
    Ok((targets.len() == scalar_count).then_some(targets))
}

fn scalar_sample_expression(
    args: &[rumoca_core::Expression],
    dims: &[usize],
    flat_index: usize,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, LowerError> {
    let scalar_value = indexed_sample_value(&args[0], dims, flat_index, span)?;
    let mut scalar_args = expression_vec_with_capacity(args.len(), "sample argument count", span)?;
    scalar_args.push(scalar_value);
    reserve_expression_capacity(
        &mut scalar_args,
        args.len().saturating_sub(1),
        "sample argument count",
        span,
    )?;
    scalar_args.extend(args.iter().skip(1).cloned());
    Ok(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sample,
        args: scalar_args,
        span,
    })
}

fn indexed_sample_value(
    value: &rumoca_core::Expression,
    dims: &[usize],
    flat_index: usize,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, LowerError> {
    let mut dims_i64 =
        expression_vec_with_capacity(dims.len(), "sample array dimension count", span)?;
    for dim in dims {
        dims_i64.push(checked_usize_to_i64(*dim, "sample array dimension", span)?);
    }
    let Some(subscripts) = dae::flat_index_to_subscripts(&dims_i64, flat_index) else {
        return Err(unsupported_at(
            format!(
                "sample array index {flat_index} is outside shape {}",
                format_usize_dims(dims)
            ),
            span,
        ));
    };
    Ok(rumoca_core::Expression::Index {
        base: Box::new(value.clone()),
        subscripts: generated_subscripts_from_usize(&subscripts, span)?,
        span,
    })
}

fn generated_subscript_from_usize(
    index: usize,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Subscript, LowerError> {
    let index = checked_usize_to_i64(index, "scalarized record subscript", span)?;
    Ok(rumoca_core::Subscript::try_generated_index(
        index,
        span,
        "scalarized record subscript",
    )?)
}

fn checked_usize_to_i64(
    value: usize,
    context: &str,
    span: rumoca_core::Span,
) -> Result<i64, LowerError> {
    i64::try_from(value).map_err(|_| {
        LowerError::contract_violation(format!("{context} {value} exceeds i64 range"), span)
    })
}

fn scalar_row_namespace(
    row_namespace: u64,
    flat_index: usize,
    span: rumoca_core::Span,
) -> Result<u64, LowerError> {
    let flat_index = u64::try_from(flat_index).map_err(|_| {
        LowerError::contract_violation(
            format!("scalar row flat index {flat_index} exceeds u64 namespace"),
            span,
        )
    })?;
    row_namespace
        .checked_mul(1_000_000)
        .and_then(|base| base.checked_add(flat_index))
        .ok_or_else(|| {
            LowerError::contract_violation(
                format!(
                "scalar row namespace overflows for namespace {row_namespace} and flat index {flat_index}"
            ),
                span,
            )
        })
}

fn row_namespace_from_usize(
    row_idx: usize,
    span: Option<rumoca_core::Span>,
) -> Result<u64, LowerError> {
    u64::try_from(row_idx).map_err(|_| {
        expression_contract_violation(format!("row index {row_idx} exceeds u64 namespace"), span)
    })
}

fn expand_row_values(
    values: Vec<Reg>,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<Vec<Reg>, LowerError> {
    match values.len() {
        1 => {
            let mut expanded = expression_vec_with_capacity(
                scalar_count,
                "expanded expression value count",
                span,
            )?;
            expanded.resize(scalar_count, values[0]);
            Ok(expanded)
        }
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
    let n_x = dae_model
        .variables
        .states
        .values()
        .try_fold(0usize, |acc, var| {
            variable_size(var).and_then(|size| {
                acc.checked_add(size).ok_or_else(|| {
                    LowerError::contract_violation(
                        "DAE state scalar count overflows usize",
                        var.source_span,
                    )
                })
            })
        })?;
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
    let structural_bindings = compile_time::structural_bindings(dae_model)?;
    let indexed_bindings = Arc::new(build_indexed_binding_map(layout));
    let mut state_names = if let Some(first_state) = dae_model.variables.states.values().next() {
        expression_vec_with_capacity(
            dae_model.variables.states.len(),
            "state name count",
            first_state.source_span,
        )?
    } else {
        Vec::new()
    };
    for name in dae_model.variables.states.keys() {
        state_names.push(name.as_str().to_string());
    }
    let direct_assignments = derivative_rhs::collect_missing_indexed_record_field_assignments(
        dae_model,
        &state_names,
        layout,
        &structural_bindings,
    )?;
    let structural_bindings = Arc::new(structural_bindings);
    let direct_assignments = Arc::new(direct_assignments);
    let mut equation_iter = equations.into_iter();
    let Some(first_equation) = equation_iter.next() else {
        return Ok(Vec::new());
    };
    let equation_span = first_equation.1.span;
    let (lower_bound, upper_bound) = equation_iter.size_hint();
    let equation_capacity = upper_bound
        .unwrap_or(lower_bound)
        .checked_add(1)
        .ok_or_else(|| {
            LowerError::contract_violation(
                "residual equation count exceeds host index range",
                equation_span,
            )
        })?;
    let mut equations =
        expression_vec_with_capacity(equation_capacity, "residual equation count", equation_span)?;
    equations.push(first_equation);
    for equation in equation_iter {
        equations.push(equation);
    }
    let mut rows =
        expression_vec_with_capacity(equations.len(), "residual row count", equation_span)?;
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
            dae_variables: Some(&dae_model.variables),
            structural_bindings: Some(Arc::clone(&structural_bindings)),
            direct_assignments: Some(Arc::clone(&direct_assignments)),
            indexed_bindings: Arc::clone(&indexed_bindings),
            is_initial_mode,
            guard_target_start_before_first_clock_tick: false,
        };
        if let Some(record_rows) =
            lower_scalarized_record_residual_rows(eq, row_idx, state_scalar_count, &ctx)?
        {
            reserve_expression_capacity(
                &mut rows,
                record_rows.len(),
                "scalarized record residual row count",
                eq.span,
            )?;
            rows.extend(record_rows);
            let row_count = rows.len() - start;
            validate_equation_row_count(eq, row_count)?;
            after_equation(eq, row_count)?;
            continue;
        }

        let lowered_rows = lower_equation_residual_rows(eq, row_idx, state_scalar_count, &ctx)?;
        reserve_expression_capacity(
            &mut rows,
            lowered_rows.len(),
            "equation residual row count",
            eq.span,
        )?;
        rows.extend(lowered_rows);
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
    Err(LowerError::contract_violation(
        format!(
            "equation `{}` declares scalar_count {expected} but lowered to {actual} residual rows",
            eq.origin
        ),
        eq.span,
    ))
}

fn lower_equation_residual_rows(
    eq: &dae::Equation,
    row_idx: usize,
    state_scalar_count: usize,
    ctx: &RowLoweringContext<'_>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let row_namespace = row_namespace_from_usize(row_idx, Some(eq.span))?;
    let mut builder = lower_builder_for_context(ctx, row_namespace).with_dedup_access_ops(false);
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
                name: lhs.clone(),
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
        let mut values = expression_vec_with_capacity(1, "scalar residual value count", eq.span)?;
        values.push(
            builder
                .lower_expr_with_source_context(expr, eq.span, &scope, 0)
                .map_err(|err| residual_row_context(err, row_idx, eq))?,
        );
        values
    } else {
        let values = builder
            .lower_array_like_values_with_source_context(expr, eq.span, &scope, 0)
            .map_err(|err| residual_row_context(err, row_idx, eq))?;
        expand_row_values(values, scalar_count, eq.span)
            .map_err(|err| residual_row_context(err, row_idx, eq))?
    };

    let values = if row_idx < state_scalar_count {
        let mut negated =
            expression_vec_with_capacity(values.len(), "state residual value count", eq.span)?;
        for value in values {
            negated.push(builder.emit_unary_at(UnaryOp::Neg, value, eq.span)?);
        }
        negated
    } else {
        values
    };

    let mut rows = expression_vec_with_capacity(scalar_count, "residual row count", eq.span)?;
    for value in values {
        rows.push(linear_ops_with_store(&builder.ops, value, Some(eq.span))?);
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
                name: lhs.clone(),
                subscripts: Vec::new(),
                span: eq.span,
            };
            let lhs_field = scalarized_record_binding_projection(lhs_base, field, eq.span)?;
            let rhs_field = scalarized_record_value_projection(eq.rhs.clone(), field, eq.span)?;
            Ok(rumoca_core::Expression::Binary {
                op: OpBinary::Sub,
                lhs: Box::new(lhs_field),
                rhs: Box::new(rhs_field),
                span: eq.span,
            })
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

    let projection_span = if span.is_dummy() { eq.span } else { *span };
    let residuals = fields.iter().map(|field| {
        let lhs_field = scalarized_record_binding_projection(*lhs.clone(), field, projection_span)?;
        let rhs_field = scalarized_record_value_projection(*rhs.clone(), field, projection_span)?;
        Ok(rumoca_core::Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(lhs_field),
            rhs: Box::new(rhs_field),
            span: projection_span,
        })
    });
    lower_scalarized_residual_expressions(residuals, row_idx, eq, ctx).map(Some)
}

fn lower_scalarized_residual_expressions(
    residuals: impl IntoIterator<Item = Result<rumoca_core::Expression, LowerError>>,
    row_idx: usize,
    eq: &dae::Equation,
    ctx: &RowLoweringContext<'_>,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let mut rows = Vec::new();
    for (field_idx, residual) in residuals.into_iter().enumerate() {
        let residual = residual.map_err(|err| residual_row_context(err, row_idx, eq))?;
        let row_namespace = row_namespace_from_usize(row_idx, Some(eq.span))
            .map_err(|err| residual_row_context(err, row_idx, eq))?;
        let namespace = scalar_row_namespace(row_namespace, field_idx, eq.span)
            .map_err(|err| residual_row_context(err, row_idx, eq))?;
        let row = lower_expression_row(&residual, namespace, None, Some(eq.span), ctx)
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
        // Keeps its identity so the outermost projection boundary can still
        // recover it as a decline.
        err @ LowerError::ProjectionBudgetExceeded { .. } => err,
        // `with_context` preserves every variant's typed identity, so no
        // error needs to be re-encoded as a reason string here.
        err => err.with_context(context),
    }
}

fn lower_expression_row(
    expression: &rumoca_core::Expression,
    row_namespace: u64,
    current_update_target: Option<rumoca_ir_solve::ScalarSlot>,
    owner_span: Option<rumoca_core::Span>,
    ctx: &RowLoweringContext<'_>,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = lower_builder_for_context(ctx, row_namespace)
        .with_current_update_target(current_update_target);
    let scope = Scope::new();
    let value = if let Some(span) = owner_span
        .or_else(|| expression.span())
        .filter(|span| !span.is_dummy())
    {
        builder.lower_expr_with_source_context(expression, span, &scope, 0)?
    } else {
        builder.lower_expr(expression, &scope, 0)?
    };
    let value = if ctx.guard_target_start_before_first_clock_tick {
        builder.lower_current_update_target_start_before_first_clock_tick(
            value, expression, owner_span, &scope, 0,
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
    let structural_bindings = match ctx.structural_bindings.clone() {
        Some(bindings) => bindings,
        None => Arc::new(IndexMap::new()),
    };
    let direct_assignments = match ctx.direct_assignments.clone() {
        Some(assignments) => assignments,
        None => Arc::new(IndexMap::new()),
    };

    LowerBuilder::new_with_metadata(
        ctx.layout,
        ctx.functions,
        LowerBuilderMetadata {
            clock_intervals: ctx.clock_intervals,
            clock_timings: ctx.clock_timings,
            triggered_clock_conditions: ctx.triggered_clock_conditions,
            discrete_valued_names: ctx.discrete_valued_names,
            variable_starts: ctx.variable_starts,
            dae_variables: ctx.dae_variables,
            indexed_bindings: Some(&ctx.indexed_bindings),
            is_initial_mode: ctx.is_initial_mode,
        },
    )
    .with_structural_bindings(structural_bindings)
    .with_direct_assignments(direct_assignments)
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

    #[test]
    fn scalar_row_namespace_rejects_overflow() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_expression_rows_source_19.mo",
            ),
            1,
            4,
        );
        let err = scalar_row_namespace(u64::MAX, 1, span)
            .expect_err("row namespace multiplication should reject overflow");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(err, LowerError::ContractViolation { .. }));
    }

    fn unspanned_expression_rows_test_span() -> rumoca_core::Span {
        rumoca_core::Span::DUMMY
    }

    #[test]
    fn scalar_row_namespace_rejects_overflow_without_dummy_span() {
        let err = scalar_row_namespace(u64::MAX, 1, unspanned_expression_rows_test_span())
            .expect_err("row namespace multiplication should reject overflow");

        assert_eq!(err.source_span(), None);
        assert!(
            err.reason().contains("scalar row namespace overflows"),
            "{err:?}"
        );
    }

    #[test]
    fn row_namespace_rejects_u64_overflow_with_span() {
        let Some(row_idx) = usize::try_from(u64::MAX)
            .ok()
            .and_then(|value| value.checked_add(1))
        else {
            return;
        };
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_expression_rows_source_21.mo",
            ),
            2,
            7,
        );

        let err = row_namespace_from_usize(row_idx, Some(span))
            .expect_err("row namespace must fit in u64");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(err, LowerError::ContractViolation { .. }));
        assert!(err.reason().contains("exceeds u64 namespace"));
    }

    #[test]
    fn expression_contract_violation_without_span_stays_unspanned() {
        let err = expression_contract_violation("expression row metadata mismatch", None);

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason().contains("expression row metadata mismatch"),
            "{err:?}"
        );
    }

    #[test]
    fn generated_subscript_rejects_i64_overflow_with_span() {
        let Some(index) = usize::try_from(i64::MAX)
            .ok()
            .and_then(|value| value.checked_add(1))
        else {
            return;
        };
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_expression_rows_source_20.mo",
            ),
            3,
            8,
        );

        let err = generated_subscript_from_usize(index, span)
            .expect_err("generated subscript must fit in Modelica integer range");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(
            err,
            LowerError::ContractViolation { reason, span: actual }
                if actual == span && reason.contains("exceeds i64 range")
        ));
    }

    #[test]
    fn indexed_sample_value_rejects_i64_dimension_overflow_with_span() {
        let Some(dim) = usize::try_from(i64::MAX)
            .ok()
            .and_then(|value| value.checked_add(1))
        else {
            return;
        };
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_expression_rows_source_21.mo",
            ),
            5,
            11,
        );
        let value = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span,
        };

        let err = indexed_sample_value(&value, &[dim], 0, span)
            .expect_err("sample array dimension must fit in Modelica integer range");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(
            err,
            LowerError::ContractViolation { reason, span: actual }
                if actual == span && reason.contains("sample array dimension")
        ));
    }

    #[test]
    fn expand_row_values_reports_capacity_overflow_with_owner_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_expression_rows_source_22.mo",
            ),
            1,
            4,
        );
        let err = match expand_row_values(vec![0], usize::MAX, span) {
            Ok(_) => panic!("oversized expression value expansion should fail before allocating"),
            Err(err) => err,
        };

        assert_eq!(err.source_span(), Some(span));
        assert!(
            err.reason()
                .contains("expanded expression value count capacity"),
            "unexpected error: {err}"
        );
    }
}
