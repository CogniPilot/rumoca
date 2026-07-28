use super::*;

pub(crate) type NativeResidualNode = (usize, usize, ComputeNode);

/// Preserve an algebraic residual `z - W*x = 0` as one native MatMul.
///
/// A MatMul node has no post-kernel add/subtract input, so the exact residual is
/// represented as the augmented product `[I, -W] * [z; x]`. This works for
/// matrix-matrix, matrix-vector, vector-matrix, and vector-vector products and
/// leaves scalar fallback to the shared Solve-IR scalarizer.
pub(crate) fn lower_native_algebraic_residual_nodes(
    dae_model: &dae::Dae,
    layout: &VarLayout,
    residual_equations: &[(usize, &dae::Equation)],
) -> Result<Vec<NativeResidualNode>, LowerError> {
    let structural_bindings = compile_time::structural_bindings(dae_model)?;
    let indexed_bindings = Arc::new(build_indexed_binding_map(layout));
    let state_names: std::collections::HashSet<String> = dae_model
        .variables
        .states
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();
    let direct_assignments = derivative_rhs::collect_missing_indexed_record_field_assignments(
        dae_model,
        &state_names,
        layout,
        &structural_bindings,
    )?;
    let structural_bindings = Arc::new(structural_bindings);
    let direct_assignments = Arc::new(direct_assignments);
    let mut nodes = expression_vec_with_optional_capacity(
        residual_equations.len(),
        "native residual node count",
        residual_equations
            .first()
            .map(|(_, equation)| equation.span),
    )?;
    let mut output_start = 0usize;

    for (equation_index, equation) in residual_equations {
        let output_count = equation.scalar_count.max(1);
        let is_structured = dae::structured_equation_slot(
            &dae_model.continuous.structured_equations,
            *equation_index,
        )
        .is_some();
        if !is_structured {
            let row_namespace = row_namespace_from_usize(*equation_index, Some(equation.span))?;
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
                is_initial_mode: false,
                guard_target_start_before_first_clock_tick: false,
            };
            match lower_algebraic_residual_matmul_node(equation, row_namespace, &ctx) {
                Ok(Some(node)) => nodes.push((output_start, output_count, node)),
                Ok(None) => {}
                Err(error) => {
                    // Native preservation is an optimization over scalar rows
                    // that have already lowered successfully. A MatMul
                    // sub-builder deliberately omits some scalar-only context
                    // (notably direct-assignment inlining), so inability to
                    // construct this node must decline to that exact scalar
                    // owner rather than make the model newly invalid.
                    tracing::debug!(
                        equation_index,
                        reason = %error,
                        "continuous algebraic residual MatMul preservation declined"
                    );
                }
            }
        }
        output_start = output_start.checked_add(output_count).ok_or_else(|| {
            LowerError::contract_violation(
                "native residual output range exceeds host index range",
                equation.span,
            )
        })?;
    }
    Ok(nodes)
}

fn lower_algebraic_residual_matmul_node(
    equation: &dae::Equation,
    row_namespace: u64,
    ctx: &RowLoweringContext<'_>,
) -> Result<Option<ComputeNode>, LowerError> {
    let builder = lower_builder_for_context(ctx, row_namespace);
    let scope = Scope::new();
    let Some(plan) = plan_algebraic_residual_matmul(equation, &builder, &scope)? else {
        return Ok(None);
    };

    let augmented_inner = plan.shape.m.checked_add(plan.shape.k).ok_or_else(|| {
        LowerError::contract_violation(
            "native residual augmented MatMul inner dimension overflows usize",
            plan.span,
        )
    })?;
    let Some((lhs_ops, lhs_start, lhs_next)) = lower_augmented_residual_lhs(
        &builder,
        plan.matrix,
        &scope,
        plan.shape,
        augmented_inner,
        plan.span,
    )?
    else {
        return Ok(None);
    };
    let Some((rhs_ops, rhs_start)) = lower_augmented_residual_rhs(ResidualRhsInput {
        builder: &builder,
        lhs_expression: &plan.lhs_expression,
        vector: plan.vector,
        scope: &scope,
        shape: plan.shape,
        augmented_inner,
        lhs_element_count: plan.lhs_element_count,
        start_reg: lhs_next,
        span: plan.span,
    })?
    else {
        return Ok(None);
    };

    Ok(Some(ComputeNode::MatMul {
        lhs_ops,
        lhs_start,
        rhs_ops,
        rhs_start,
        m: plan.shape.m,
        k: augmented_inner,
        n: plan.shape.n,
        lhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
        rhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
        metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
        span: plan.span,
    }))
}

struct ResidualMatMulPlan<'a> {
    matrix: &'a rumoca_core::Expression,
    vector: &'a rumoca_core::Expression,
    lhs_expression: rumoca_core::Expression,
    shape: super::super::array_values::MatMulShape,
    lhs_element_count: usize,
    span: rumoca_core::Span,
}

fn plan_algebraic_residual_matmul<'a>(
    equation: &'a dae::Equation,
    builder: &LowerBuilder<'_>,
    scope: &Scope,
) -> Result<Option<ResidualMatMulPlan<'a>>, LowerError> {
    let Some(lhs_name) = equation.lhs.as_ref() else {
        return Ok(None);
    };
    let rumoca_core::Expression::Binary {
        op: OpBinary::Mul,
        lhs: matrix,
        rhs: vector,
        span,
    } = &equation.rhs
    else {
        return Ok(None);
    };
    let span = if span.is_dummy() {
        equation.span
    } else {
        *span
    };
    if span.is_dummy() {
        return Err(LowerError::UnspannedContractViolation {
            reason: "native algebraic residual MatMul requires a source span".to_string(),
        });
    }
    let matrix_dims = builder.infer_expr_dims(matrix, scope)?;
    let vector_dims = builder.infer_expr_dims(vector, scope)?;
    let Some(shape) = super::super::array_values::matmul_shape_from_dims(
        &matrix_dims,
        &vector_dims,
        equation.scalar_count.max(1),
    ) else {
        return Ok(None);
    };
    if shape.m == 0 || shape.n == 0 {
        return Ok(None);
    }
    let lhs_expression = rumoca_core::Expression::VarRef {
        name: lhs_name.clone(),
        subscripts: Vec::new(),
        span: equation.span,
    };
    let lhs_element_count =
        checked_residual_product(shape.m, shape.n, "native residual lhs element count", span)?;
    let lhs_dims = builder.infer_expr_dims(&lhs_expression, scope)?;
    if lhs_dims
        .iter()
        .try_fold(1usize, |count, dim| count.checked_mul(*dim))
        != Some(lhs_element_count)
    {
        return Ok(None);
    }
    Ok(Some(ResidualMatMulPlan {
        matrix,
        vector,
        lhs_expression,
        shape,
        lhs_element_count,
        span,
    }))
}

fn lower_augmented_residual_lhs(
    builder: &LowerBuilder<'_>,
    matrix: &rumoca_core::Expression,
    scope: &Scope,
    shape: super::super::array_values::MatMulShape,
    augmented_inner: usize,
    span: rumoca_core::Span,
) -> Result<Option<(Vec<LinearOp>, Reg, Reg)>, LowerError> {
    let mut lhs_builder = builder.fork_with_next_reg(0);
    let matrix_values =
        lhs_builder.lower_array_like_values_with_source_context(matrix, span, scope, 0)?;
    let matrix_element_count = checked_residual_product(
        shape.m,
        shape.k,
        "native residual matrix element count",
        span,
    )?;
    if matrix_values.len() != matrix_element_count {
        return Ok(None);
    }
    let augmented_lhs_count = checked_residual_product(
        shape.m,
        augmented_inner,
        "native residual augmented lhs element count",
        span,
    )?;
    let mut values = expression_vec_with_capacity(
        augmented_lhs_count,
        "native residual augmented lhs values",
        span,
    )?;
    for row in 0..shape.m {
        for column in 0..shape.m {
            let identity_value = if row == column { 1.0 } else { 0.0 };
            values.push(lhs_builder.emit_const_at(identity_value, span)?);
        }
        let row_start =
            checked_residual_product(row, shape.k, "native residual matrix row offset", span)?;
        let row_end = row_start.checked_add(shape.k).ok_or_else(|| {
            LowerError::contract_violation("native residual matrix row end overflows usize", span)
        })?;
        for value in &matrix_values[row_start..row_end] {
            values.push(lhs_builder.emit_unary_at(UnaryOp::Neg, *value, span)?);
        }
    }
    let lhs_start = lhs_builder.try_pack_registers(&values, span)?;
    Ok(Some((lhs_builder.ops, lhs_start, lhs_builder.next_reg)))
}

struct ResidualRhsInput<'a, 'ctx> {
    builder: &'a LowerBuilder<'ctx>,
    lhs_expression: &'a rumoca_core::Expression,
    vector: &'a rumoca_core::Expression,
    scope: &'a Scope,
    shape: super::super::array_values::MatMulShape,
    augmented_inner: usize,
    lhs_element_count: usize,
    start_reg: Reg,
    span: rumoca_core::Span,
}

fn lower_augmented_residual_rhs(
    input: ResidualRhsInput<'_, '_>,
) -> Result<Option<(Vec<LinearOp>, Reg)>, LowerError> {
    let ResidualRhsInput {
        builder,
        lhs_expression,
        vector,
        scope,
        shape,
        augmented_inner,
        lhs_element_count,
        start_reg,
        span,
    } = input;
    let mut rhs_builder = builder.fork_with_next_reg(start_reg);
    let lhs_values =
        rhs_builder.lower_array_like_values_with_source_context(lhs_expression, span, scope, 0)?;
    let vector_values =
        rhs_builder.lower_array_like_values_with_source_context(vector, span, scope, 0)?;
    let vector_element_count =
        checked_residual_product(shape.k, shape.n, "native residual rhs element count", span)?;
    if lhs_values.len() != lhs_element_count || vector_values.len() != vector_element_count {
        return Ok(None);
    }
    let augmented_rhs_count = checked_residual_product(
        augmented_inner,
        shape.n,
        "native residual augmented rhs element count",
        span,
    )?;
    let mut values = expression_vec_with_capacity(
        augmented_rhs_count,
        "native residual augmented rhs values",
        span,
    )?;
    values.extend(lhs_values);
    values.extend(vector_values);
    let rhs_start = rhs_builder.try_pack_registers(&values, span)?;
    Ok(Some((rhs_builder.ops, rhs_start)))
}

fn checked_residual_product(
    lhs: usize,
    rhs: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    lhs.checked_mul(rhs)
        .ok_or_else(|| LowerError::contract_violation(format!("{context} overflows usize"), span))
}
