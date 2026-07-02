//! Appendix B invariant checks at the DAE -> Solve boundary.

use std::collections::HashSet;

use rumoca_core::{ExpressionVisitor, FallibleExpressionVisitor, Span};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use solve::SolveVisitor;

use crate::{
    function_validation::{
        collect_function_parameter_call_aliases, is_named_function_arg_marker,
        validate_sim_function_call_name,
    },
    lower::LowerError,
};

pub(super) fn validate_solve_input_appendix_b_invariants(
    dae_model: &dae::Dae,
) -> Result<(), LowerError> {
    let function_param_aliases = collect_function_parameter_call_aliases(dae_model);
    for (context, expr, span) in dae_appendix_b_expressions(dae_model, IncludeInitial::No) {
        validate_no_source_temporal_operator(expr, &context, span)?;
        validate_no_flow_action_expression(expr, &context, span)?;
    }
    for (context, expr, _span) in dae_appendix_b_expressions(dae_model, IncludeInitial::Yes) {
        validate_function_calls_resolve(dae_model, expr, &function_param_aliases, &context)?;
    }
    Ok(())
}

pub(super) fn validate_solve_problem_appendix_b_invariants(
    problem: &solve::SolveProblem,
) -> Result<(), LowerError> {
    validate_compute_block(
        "continuous.implicit_rhs",
        &problem.continuous.implicit_rhs,
        SeedUse::Forbidden,
    )?;
    validate_compute_block(
        "continuous.residual",
        &problem.continuous.residual,
        SeedUse::Forbidden,
    )?;
    validate_compute_block(
        "continuous.derivative_rhs",
        &problem.continuous.derivative_rhs,
        SeedUse::Forbidden,
    )?;
    validate_compute_block(
        "initialization.residual",
        &problem.initialization.residual,
        SeedUse::Forbidden,
    )?;
    validate_scalar_program_block(
        "discrete.runtime_assignment_rhs",
        &problem.discrete.runtime_assignment_rhs,
        SeedUse::Forbidden,
    )?;
    validate_scalar_program_block("discrete.rhs", &problem.discrete.rhs, SeedUse::Forbidden)?;
    validate_scalar_program_block(
        "events.root_conditions",
        &problem.events.root_conditions,
        SeedUse::Forbidden,
    )?;
    validate_scalar_program_block(
        "events.dynamic_time_event_rhs",
        &problem.events.dynamic_time_event_rhs,
        SeedUse::Forbidden,
    )?;
    validate_scalar_program_block(
        "events.action_conditions",
        &problem.events.action_conditions,
        SeedUse::Forbidden,
    )?;
    if problem.events.action_conditions.len() != problem.events.actions.len() {
        let span = problem
            .events
            .action_conditions
            .first_source_span()
            .or_else(|| {
                problem
                    .events
                    .actions
                    .iter()
                    .map(|action| action.span)
                    .find(|span| !span.is_dummy())
            });
        return Err(solve_validation_error(
            format!(
                "events.action_conditions has {} rows but events.actions has {} entries",
                problem.events.action_conditions.len(),
                problem.events.actions.len()
            ),
            span,
        ));
    }
    Ok(())
}

pub(super) fn validate_solve_artifacts_appendix_b_invariants(
    artifacts: &solve::SolveArtifacts,
) -> Result<(), LowerError> {
    validate_compute_block(
        "artifacts.continuous.implicit_jacobian_v",
        &artifacts.continuous.implicit_jacobian_v,
        SeedUse::Allowed,
    )?;
    validate_scalar_program_block(
        "artifacts.continuous.full_jacobian_v",
        &artifacts.continuous.full_jacobian_v,
        SeedUse::Allowed,
    )?;
    Ok(())
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum SeedUse {
    Forbidden,
    Allowed,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum IncludeInitial {
    No,
    Yes,
}

fn dae_appendix_b_expressions(
    dae_model: &dae::Dae,
    include_initial: IncludeInitial,
) -> Vec<(String, &rumoca_core::Expression, Option<Span>)> {
    let mut exprs = Vec::new();
    push_equation_rhs(&mut exprs, "f_x", &dae_model.continuous.equations);
    if include_initial == IncludeInitial::Yes {
        push_equation_rhs(
            &mut exprs,
            "initial_equations",
            &dae_model.initialization.equations,
        );
    }
    push_equation_rhs(&mut exprs, "f_z", &dae_model.discrete.real_updates);
    push_equation_rhs(&mut exprs, "f_m", &dae_model.discrete.valued_updates);
    push_equation_rhs(&mut exprs, "f_c", &dae_model.conditions.equations);
    exprs.extend(
        dae_model
            .conditions
            .relations
            .iter()
            .enumerate()
            .map(|(idx, expr)| (format!("relation[{idx}]"), expr, expr.span())),
    );
    exprs.extend(
        dae_model
            .events
            .event_actions
            .iter()
            .enumerate()
            .map(|(idx, action)| {
                (
                    format!("event_actions[{idx}].condition"),
                    &action.condition,
                    action.condition.span(),
                )
            }),
    );
    exprs
}

fn push_equation_rhs<'a>(
    exprs: &mut Vec<(String, &'a rumoca_core::Expression, Option<Span>)>,
    partition: &str,
    equations: &'a [dae::Equation],
) {
    exprs.extend(equations.iter().enumerate().map(|(idx, eq)| {
        (
            format!("{partition}[{idx}] origin='{}'", eq.origin),
            &eq.rhs,
            (!eq.span.is_dummy())
                .then_some(eq.span)
                .or_else(|| eq.rhs.span()),
        )
    }));
}

fn validate_no_source_temporal_operator(
    expr: &rumoca_core::Expression,
    context: &str,
    owner_span: Option<Span>,
) -> Result<(), LowerError> {
    if let Some((operator, span)) = find_source_temporal_operator(expr) {
        return Err(solve_validation_error(
            format!(
                "{context} contains `{operator}`; \
                 phase-dae must lower source temporal operators to Appendix B variables, \
                 conditions, schedules, and ordinary equations before Solve-IR lowering"
            ),
            span.or(owner_span),
        ));
    }
    Ok(())
}

fn find_source_temporal_operator(
    expr: &rumoca_core::Expression,
) -> Option<(&'static str, Option<Span>)> {
    let mut checker = SourceTemporalOperatorChecker { found: None };
    checker.visit_expression(expr);
    checker.found
}

fn validate_no_flow_action_expression(
    expr: &rumoca_core::Expression,
    context: &str,
    owner_span: Option<Span>,
) -> Result<(), LowerError> {
    if let Some((action, span)) = find_flow_action_expression(expr) {
        return Err(solve_validation_error(
            format!(
                "{context} contains `{action}` as a value expression; \
                 phase-dae must lower reinit to guarded discrete updates and preserve only assert/terminate as event actions"
            ),
            span.or(owner_span),
        ));
    }
    Ok(())
}

fn find_flow_action_expression(
    expr: &rumoca_core::Expression,
) -> Option<(&'static str, Option<Span>)> {
    let mut checker = FlowActionExpressionChecker { found: None };
    checker.visit_expression(expr);
    checker.found
}

struct FlowActionExpressionChecker {
    found: Option<(&'static str, Option<Span>)>,
}

impl ExpressionVisitor for FlowActionExpressionChecker {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if self.found.is_some() {
            return;
        }
        if let rumoca_core::Expression::FunctionCall { name, .. } = expr
            && let Some(action) =
                rumoca_core::runtime_flow_action_function_short_name(name.as_str())
        {
            self.found = Some((action, expr.span()));
            return;
        }
        self.walk_expression(expr);
    }
}

struct SourceTemporalOperatorChecker {
    found: Option<(&'static str, Option<Span>)>,
}

impl ExpressionVisitor for SourceTemporalOperatorChecker {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if self.found.is_some() {
            return;
        }
        match expr {
            rumoca_core::Expression::BuiltinCall { function, .. } => {
                if let Some(operator) = rumoca_core::source_temporal_builtin_name(*function) {
                    self.found = Some((operator, expr.span()));
                    return;
                }
            }
            rumoca_core::Expression::FunctionCall { name, .. } => {
                if let Some(operator) =
                    rumoca_core::source_temporal_function_short_name(name.as_str())
                {
                    self.found = Some((operator, expr.span()));
                    return;
                }
            }
            _ => {}
        }
        self.walk_expression(expr);
    }

    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if let Some(operator) = rumoca_core::source_temporal_builtin_name(*function) {
            self.found = Some((operator, None));
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
    ) {
        if let Some(operator) = rumoca_core::source_temporal_function_short_name(name.as_str()) {
            self.found = Some((operator, None));
            return;
        }
        self.walk_function_call(name, args, is_constructor);
    }
}

fn validate_function_calls_resolve(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
    function_param_aliases: &HashSet<rumoca_core::VarName>,
    context: &str,
) -> Result<(), LowerError> {
    let mut validator = FunctionCallResolveValidator {
        dae_model,
        function_param_aliases,
        context,
    };
    validator.visit_expression(expr)
}

struct FunctionCallResolveValidator<'a> {
    dae_model: &'a dae::Dae,
    function_param_aliases: &'a HashSet<rumoca_core::VarName>,
    context: &'a str,
}

impl FallibleExpressionVisitor for FunctionCallResolveValidator<'_> {
    type Error = LowerError;

    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
    ) -> Result<(), Self::Error> {
        if !is_constructor
            && !is_named_function_arg_marker(name)
            && let Err(err) =
                validate_sim_function_call_name(self.dae_model, name, self.function_param_aliases)
        {
            return Err(LowerError::InvalidFunction {
                name: err.name,
                reason: format!(
                    "Solve Appendix-B validation failed in {}: {}",
                    self.context, err.reason
                ),
            });
        }

        for arg in args {
            self.visit_expression(arg)?;
        }
        Ok(())
    }
}

fn validate_scalar_program_block(
    context: &str,
    block: &solve::ScalarProgramBlock,
    seed_use: SeedUse,
) -> Result<(), LowerError> {
    SolveBlockValidator { context, seed_use }.visit_scalar_program_block(block)
}

fn validate_compute_block(
    context: &str,
    block: &solve::ComputeBlock,
    seed_use: SeedUse,
) -> Result<(), LowerError> {
    SolveBlockValidator { context, seed_use }.visit_compute_block(block)
}

struct SolveBlockValidator<'a> {
    context: &'a str,
    seed_use: SeedUse,
}

impl SolveVisitor for SolveBlockValidator<'_> {
    type Error = LowerError;

    fn visit_compute_node(
        &mut self,
        node_index: usize,
        node: &solve::ComputeNode,
    ) -> Result<(), Self::Error> {
        let context = format!("{}.nodes[{node_index}]", self.context);
        validate_compute_node(&context, node, self.seed_use)
    }

    fn visit_scalar_program(
        &mut self,
        program_index: usize,
        span: Option<rumoca_core::Span>,
        ops: &[solve::LinearOp],
    ) -> Result<(), Self::Error> {
        validate_row_ops(
            &format!("{}.programs[{program_index}]", self.context),
            ops,
            span,
            true,
            self.seed_use,
        )
    }
}

fn validate_compute_node(
    context: &str,
    node: &solve::ComputeNode,
    seed_use: SeedUse,
) -> Result<(), LowerError> {
    match node {
        solve::ComputeNode::ScalarPrograms(block) => {
            validate_scalar_program_block(context, block, seed_use)
        }
        solve::ComputeNode::MatMul {
            lhs_ops,
            lhs_start,
            rhs_ops,
            rhs_start,
            m,
            k,
            n,
            metadata,
            span,
            ..
        } => validate_matmul_node(
            context, lhs_ops, *lhs_start, rhs_ops, *rhs_start, *m, *k, *n, metadata, *span,
            seed_use,
        ),
        solve::ComputeNode::LinSolve {
            setup_ops,
            matrix_start,
            rhs_start,
            n,
            next_reg,
            metadata,
            span,
            ..
        } => validate_linsolve_node(
            context,
            setup_ops,
            *matrix_start,
            *rhs_start,
            *n,
            *next_reg,
            metadata,
            *span,
            seed_use,
        ),
        solve::ComputeNode::Map {
            domain,
            base_ops,
            load_strides,
            const_strides,
            metadata,
            span,
            ..
        } => validate_affine_row_tensor_node(
            context,
            "Map",
            domain,
            base_ops,
            load_strides,
            const_strides,
            metadata,
            Some(*span),
            seed_use,
        ),
        solve::ComputeNode::AffineStencil {
            domain,
            base_ops,
            load_strides,
            const_strides,
            metadata,
            span,
            ..
        } => validate_affine_row_tensor_node(
            context,
            "AffineStencil",
            domain,
            base_ops,
            load_strides,
            const_strides,
            metadata,
            Some(*span),
            seed_use,
        ),
    }
}

// SPEC_0021: Exception - MatMul validation must compare both operand setup
// streams, dimensions, register ranges, node metadata, span, and seed policy.
#[allow(clippy::too_many_arguments)]
fn validate_matmul_node(
    context: &str,
    lhs_ops: &[solve::LinearOp],
    lhs_start: solve::Reg,
    rhs_ops: &[solve::LinearOp],
    rhs_start: solve::Reg,
    m: usize,
    k: usize,
    n: usize,
    metadata: &solve::TensorNodeMetadata,
    span: Span,
    seed_use: SeedUse,
) -> Result<(), LowerError> {
    let span = Some(span);
    validate_tensor_metadata(context, metadata)?;
    validate_row_ops(
        &format!("{context}.lhs_ops"),
        lhs_ops,
        span,
        false,
        seed_use,
    )?;
    validate_row_ops(
        &format!("{context}.rhs_ops"),
        rhs_ops,
        span,
        false,
        seed_use,
    )?;
    validate_defined_range(
        context,
        "lhs",
        lhs_ops,
        lhs_start,
        checked_len_product(context, "MatMul lhs", m, k, span)?,
        span,
    )?;
    validate_defined_range(
        context,
        "rhs",
        rhs_ops,
        rhs_start,
        checked_len_product(context, "MatMul rhs", k, n, span)?,
        span,
    )
}

// SPEC_0021: Exception - LinSolve validation must compare setup ops, matrix/rhs
// ranges, next-register accounting, metadata, span, and seed policy together.
#[allow(clippy::too_many_arguments)]
fn validate_linsolve_node(
    context: &str,
    setup_ops: &[solve::LinearOp],
    matrix_start: solve::Reg,
    rhs_start: solve::Reg,
    n: usize,
    next_reg: solve::Reg,
    metadata: &solve::TensorNodeMetadata,
    span: Span,
    seed_use: SeedUse,
) -> Result<(), LowerError> {
    let span = Some(span);
    validate_tensor_metadata(context, metadata)?;
    validate_row_ops(
        &format!("{context}.setup_ops"),
        setup_ops,
        span,
        false,
        seed_use,
    )?;
    validate_defined_range(
        context,
        "matrix",
        setup_ops,
        matrix_start,
        checked_len_product(context, "LinSolve matrix", n, n, span)?,
        span,
    )?;
    validate_defined_range(context, "rhs", setup_ops, rhs_start, n, span)?;
    if next_reg < next_free_reg(setup_ops, span)? {
        return Err(solve_validation_error(
            format!("{context}: LinSolve next_reg {next_reg} overlaps registers used by setup_ops"),
            span,
        ));
    }
    Ok(())
}

// SPEC_0021: Exception - affine tensor validation needs domain, row ops,
// strides, metadata, and seed policy together for one contract check.
#[allow(clippy::too_many_arguments)]
fn validate_affine_row_tensor_node(
    context: &str,
    node_name: &str,
    domain: &rumoca_core::StructuredIndexDomain,
    base_ops: &[solve::LinearOp],
    load_strides: &[solve::AffineStencilLoadStride],
    const_strides: &[solve::AffineStencilConstStride],
    metadata: &solve::TensorNodeMetadata,
    span: Option<Span>,
    seed_use: SeedUse,
) -> Result<(), LowerError> {
    validate_tensor_metadata(context, metadata)?;
    validate_row_ops(
        &format!("{context}.base_ops"),
        base_ops,
        span,
        true,
        seed_use,
    )?;
    for stride in load_strides {
        validate_index_stride_terms(context, node_name, domain, &stride.terms, span)?;
        match base_ops.get(stride.op_position) {
            Some(solve::LinearOp::LoadY { .. } | solve::LinearOp::LoadP { .. }) => {}
            Some(other) => {
                return Err(solve_validation_error(
                    format!(
                        "{context}: {node_name} stride targets non-load op {}",
                        other.kind_name()
                    ),
                    span,
                ));
            }
            None => {
                return Err(solve_validation_error(
                    format!(
                        "{context}: {node_name} stride op_position {} is out of bounds",
                        stride.op_position
                    ),
                    span,
                ));
            }
        }
    }
    for stride in const_strides {
        validate_const_stride_terms(context, node_name, domain, &stride.terms, span)?;
        match base_ops.get(stride.op_position) {
            Some(solve::LinearOp::Const { .. }) => {}
            Some(other) => {
                return Err(solve_validation_error(
                    format!(
                        "{context}: {node_name} const stride targets non-const op {}",
                        other.kind_name()
                    ),
                    span,
                ));
            }
            None => {
                return Err(solve_validation_error(
                    format!(
                        "{context}: {node_name} const stride op_position {} is out of bounds",
                        stride.op_position
                    ),
                    span,
                ));
            }
        }
    }
    Ok(())
}

fn validate_index_stride_terms(
    context: &str,
    node_name: &str,
    domain: &rumoca_core::StructuredIndexDomain,
    terms: &[solve::AffineStencilIndexStrideTerm],
    span: Option<Span>,
) -> Result<(), LowerError> {
    for term in terms {
        if term.dimension >= domain.binders.len() {
            return Err(solve_validation_error(
                format!(
                    "{context}: {node_name} load stride dimension {} is out of bounds",
                    term.dimension
                ),
                span,
            ));
        }
    }
    Ok(())
}

fn validate_const_stride_terms(
    context: &str,
    node_name: &str,
    domain: &rumoca_core::StructuredIndexDomain,
    terms: &[solve::AffineStencilConstStrideTerm],
    span: Option<Span>,
) -> Result<(), LowerError> {
    for term in terms {
        if term.dimension >= domain.binders.len() {
            return Err(solve_validation_error(
                format!(
                    "{context}: {node_name} const stride dimension {} is out of bounds",
                    term.dimension
                ),
                span,
            ));
        }
    }
    Ok(())
}

fn validate_tensor_metadata(
    _context: &str,
    metadata: &solve::TensorNodeMetadata,
) -> Result<(), LowerError> {
    match metadata.element_type {
        solve::TensorElementType::Real64 => {}
    }
    match metadata.layout {
        solve::TensorLayout::RowMajorDense => {}
    }
    match metadata.scalar_fallback {
        solve::ScalarFallback::Exact => {}
    }
    Ok(())
}

fn validate_row_ops(
    context: &str,
    ops: &[solve::LinearOp],
    span: Option<Span>,
    require_store_output: bool,
    seed_use: SeedUse,
) -> Result<(), LowerError> {
    let mut defined = HashSet::new();
    let mut saw_store_output = false;
    for (op_idx, op) in ops.iter().enumerate() {
        validate_op_inputs(context, op_idx, op, &defined, span, seed_use)?;
        if let Some(dst) = op.dst_register() {
            defined.insert(dst);
        }
        if matches!(op, solve::LinearOp::StoreOutput { .. }) {
            saw_store_output = true;
        }
    }
    if require_store_output && !saw_store_output {
        return Err(solve_validation_error(
            format!("{context}: row has no StoreOutput op"),
            span,
        ));
    }
    Ok(())
}

fn validate_op_inputs(
    context: &str,
    op_idx: usize,
    op: &solve::LinearOp,
    defined: &HashSet<solve::Reg>,
    span: Option<Span>,
    seed_use: SeedUse,
) -> Result<(), LowerError> {
    match op {
        solve::LinearOp::Const { .. }
        | solve::LinearOp::LoadTime { .. }
        | solve::LinearOp::LoadY { .. }
        | solve::LinearOp::LoadP { .. } => Ok(()),
        solve::LinearOp::LoadSeed { .. } if seed_use == SeedUse::Allowed => Ok(()),
        solve::LinearOp::LoadSeed { index, .. } => Err(solve_validation_error(
            format!(
                "{context}[{op_idx}]: LoadSeed[{index}] is only valid in derivative/JVP artifact rows"
            ),
            span,
        )),
        // A runtime-indexed seed load is a derivative/JVP-only construct, like
        // `LoadSeed`, but it also reads an index register that must be defined.
        solve::LinearOp::LoadIndexedSeed { index, .. } if seed_use == SeedUse::Allowed => {
            validate_defined_reg(context, op_idx, *index, defined, span)
        }
        solve::LinearOp::LoadIndexedSeed { .. } => Err(solve_validation_error(
            format!(
                "{context}[{op_idx}]: LoadIndexedSeed is only valid in derivative/JVP artifact rows"
            ),
            span,
        )),
        solve::LinearOp::Move { src, .. }
        | solve::LinearOp::Unary { arg: src, .. }
        | solve::LinearOp::LoadIndexedP { index: src, .. }
        | solve::LinearOp::StoreOutput { src } => {
            validate_defined_reg(context, op_idx, *src, defined, span)
        }
        solve::LinearOp::Binary { lhs, rhs, .. } | solve::LinearOp::Compare { lhs, rhs, .. } => {
            validate_defined_reg(context, op_idx, *lhs, defined, span)?;
            validate_defined_reg(context, op_idx, *rhs, defined, span)
        }
        solve::LinearOp::Select {
            cond,
            if_true,
            if_false,
            ..
        } => {
            validate_defined_reg(context, op_idx, *cond, defined, span)?;
            validate_defined_reg(context, op_idx, *if_true, defined, span)?;
            validate_defined_reg(context, op_idx, *if_false, defined, span)
        }
        solve::LinearOp::LinearSolveComponent { .. } => {
            validate_linear_solve_component_op(context, op_idx, op, defined, span)
        }
        solve::LinearOp::TableBounds { .. }
        | solve::LinearOp::TableLookup { .. }
        | solve::LinearOp::TableLookupSlope { .. }
        | solve::LinearOp::TableNextEvent { .. } => {
            validate_table_op_inputs(context, op_idx, op, defined, span)
        }
        solve::LinearOp::RandomInitialState { .. }
        | solve::LinearOp::RandomResult { .. }
        | solve::LinearOp::RandomState { .. }
        | solve::LinearOp::ImpureRandomInit { .. }
        | solve::LinearOp::ImpureRandom { .. }
        | solve::LinearOp::ImpureRandomInteger { .. } => {
            validate_random_op_inputs(context, op_idx, op, defined, span)
        }
        solve::LinearOp::ExternalCall {
            args, arg_count, ..
        } => {
            for arg in args.iter().take(*arg_count) {
                validate_defined_reg(context, op_idx, *arg, defined, span)?;
            }
            Ok(())
        }
    }
}

fn validate_linear_solve_component_op(
    context: &str,
    op_idx: usize,
    op: &solve::LinearOp,
    defined: &HashSet<solve::Reg>,
    span: Option<Span>,
) -> Result<(), LowerError> {
    let solve::LinearOp::LinearSolveComponent {
        matrix_start,
        rhs_start,
        n,
        component,
        ..
    } = *op
    else {
        return Ok(());
    };
    if component >= n {
        return Err(solve_validation_error(
            format!(
                "{context}[{op_idx}]: LinearSolveComponent component {component} is outside n={n}"
            ),
            span,
        ));
    }
    validate_defined_reg_range(
        context,
        op_idx,
        matrix_start,
        checked_len_product(context, "LinearSolveComponent matrix", n, n, span)?,
        defined,
        span,
    )?;
    validate_defined_reg_range(context, op_idx, rhs_start, n, defined, span)
}

fn validate_table_op_inputs(
    context: &str,
    op_idx: usize,
    op: &solve::LinearOp,
    defined: &HashSet<solve::Reg>,
    span: Option<Span>,
) -> Result<(), LowerError> {
    match *op {
        solve::LinearOp::TableBounds { table_id, .. } => {
            validate_defined_reg(context, op_idx, table_id, defined, span)
        }
        solve::LinearOp::TableLookup {
            table_id,
            column,
            input,
            ..
        }
        | solve::LinearOp::TableLookupSlope {
            table_id,
            column,
            input,
            ..
        } => {
            validate_defined_reg(context, op_idx, table_id, defined, span)?;
            validate_defined_reg(context, op_idx, column, defined, span)?;
            validate_defined_reg(context, op_idx, input, defined, span)
        }
        solve::LinearOp::TableNextEvent { table_id, time, .. } => {
            validate_defined_reg(context, op_idx, table_id, defined, span)?;
            validate_defined_reg(context, op_idx, time, defined, span)
        }
        _ => Ok(()),
    }
}

fn validate_random_op_inputs(
    context: &str,
    op_idx: usize,
    op: &solve::LinearOp,
    defined: &HashSet<solve::Reg>,
    span: Option<Span>,
) -> Result<(), LowerError> {
    match *op {
        solve::LinearOp::RandomInitialState {
            local_seed,
            global_seed,
            state_index,
            state_len,
            ..
        } => {
            validate_state_index(
                context,
                op_idx,
                "RandomInitialState",
                state_index,
                state_len,
                span,
            )?;
            validate_defined_reg(context, op_idx, local_seed, defined, span)?;
            validate_defined_reg(context, op_idx, global_seed, defined, span)
        }
        solve::LinearOp::RandomResult {
            state_start,
            state_len,
            ..
        } => validate_defined_reg_range(context, op_idx, state_start, state_len, defined, span),
        solve::LinearOp::RandomState {
            state_start,
            state_len,
            state_index,
            ..
        } => {
            validate_state_index(context, op_idx, "RandomState", state_index, state_len, span)?;
            validate_defined_reg_range(context, op_idx, state_start, state_len, defined, span)
        }
        solve::LinearOp::ImpureRandomInit { seed, .. } => {
            validate_defined_reg(context, op_idx, seed, defined, span)
        }
        solve::LinearOp::ImpureRandom { id, .. } => {
            validate_defined_reg(context, op_idx, id, defined, span)
        }
        solve::LinearOp::ImpureRandomInteger { id, imin, imax, .. } => {
            validate_defined_reg(context, op_idx, id, defined, span)?;
            validate_defined_reg(context, op_idx, imin, defined, span)?;
            validate_defined_reg(context, op_idx, imax, defined, span)
        }
        _ => Ok(()),
    }
}

fn validate_state_index(
    context: &str,
    op_idx: usize,
    op_name: &str,
    state_index: usize,
    state_len: usize,
    span: Option<Span>,
) -> Result<(), LowerError> {
    if state_index >= state_len {
        return Err(solve_validation_error(
            format!(
                "{context}[{op_idx}]: {op_name} state_index {state_index} is outside state_len={state_len}"
            ),
            span,
        ));
    }
    Ok(())
}

fn validate_defined_range(
    context: &str,
    name: &str,
    ops: &[solve::LinearOp],
    start: solve::Reg,
    len: usize,
    span: Option<Span>,
) -> Result<(), LowerError> {
    let defined = ops
        .iter()
        .filter_map(solve::LinearOp::dst_register)
        .collect::<HashSet<_>>();
    for offset in 0..len {
        let reg = checked_range_reg(context, name, start, offset, span)?;
        if !defined.contains(&reg) {
            return Err(solve_validation_error(
                format!(
                    "{context}: {name} register range starting at {start} has undefined register {reg}"
                ),
                span,
            ));
        }
    }
    Ok(())
}

fn validate_defined_reg(
    context: &str,
    op_idx: usize,
    reg: solve::Reg,
    defined: &HashSet<solve::Reg>,
    span: Option<Span>,
) -> Result<(), LowerError> {
    if !defined.contains(&reg) {
        return Err(solve_validation_error(
            format!("{context}[{op_idx}]: reads undefined register {reg}"),
            span,
        ));
    }
    Ok(())
}

fn validate_defined_reg_range(
    context: &str,
    op_idx: usize,
    start: solve::Reg,
    len: usize,
    defined: &HashSet<solve::Reg>,
    span: Option<Span>,
) -> Result<(), LowerError> {
    for offset in 0..len {
        validate_defined_reg(
            context,
            op_idx,
            checked_range_reg(context, "op input", start, offset, span)?,
            defined,
            span,
        )?;
    }
    Ok(())
}

fn next_free_reg(ops: &[solve::LinearOp], span: Option<Span>) -> Result<solve::Reg, LowerError> {
    ops.iter()
        .filter_map(solve::LinearOp::dst_register)
        .max()
        .map_or(Ok(0), |reg| {
            reg.checked_add(1).ok_or_else(|| {
                solve_validation_error("next free register index overflow".to_string(), span)
            })
        })
}

fn checked_len_product(
    context: &str,
    name: &str,
    lhs: usize,
    rhs: usize,
    span: Option<Span>,
) -> Result<usize, LowerError> {
    lhs.checked_mul(rhs).ok_or_else(|| {
        solve_validation_error(
            format!("{context}: {name} range length overflow for {lhs} * {rhs}"),
            span,
        )
    })
}

fn checked_range_reg(
    context: &str,
    name: &str,
    start: solve::Reg,
    offset: usize,
    span: Option<Span>,
) -> Result<solve::Reg, LowerError> {
    let offset = solve::Reg::try_from(offset).map_err(|_| {
        solve_validation_error(
            format!("{context}: {name} register range offset {offset} exceeds register index type"),
            span,
        )
    })?;
    start.checked_add(offset).ok_or_else(|| {
        solve_validation_error(
            format!(
                "{context}: {name} register range starting at {start} overflows at offset {offset}"
            ),
            span,
        )
    })
}

fn solve_validation_error(reason: String, span: Option<Span>) -> LowerError {
    let reason = format!("Solve Appendix-B validation failed: {reason}");
    match span {
        Some(span) => LowerError::contract_violation(reason, span),
        None => LowerError::UnspannedContractViolation { reason },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{Expression, Function, FunctionParam, Literal, OpBinary, Span, VarName};

    fn fixture_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("appendix_b_validation_fixture.mo"),
            1,
            2,
        )
    }

    fn real(value: f64, span: Span) -> Expression {
        Expression::Literal {
            value: Literal::Real(value),
            span,
        }
    }

    #[test]
    fn solve_input_validation_allows_record_constructor_without_body() {
        let span = fixture_span();
        let mut dae = dae::Dae::default();
        let mut constructor = Function::new("Pkg.Generic", span);
        constructor.is_constructor = true;
        constructor.add_input(FunctionParam::new("eta", "Real", span));
        dae.symbols
            .functions
            .insert(VarName::new("Pkg.Generic"), constructor);
        dae.continuous.equations.push(dae::Equation::residual(
            Expression::Binary {
                op: OpBinary::Sub,
                lhs: Box::new(real(0.0, span)),
                rhs: Box::new(Expression::FunctionCall {
                    name: VarName::new("Pkg.Generic").into(),
                    args: vec![real(0.8, span)],
                    is_constructor: true,
                    span,
                }),
                span,
            },
            span,
            "record constructor residual",
        ));

        validate_solve_input_appendix_b_invariants(&dae)
            .expect("record constructors are data constructors, not executable functions");
    }

    #[test]
    fn solve_input_validation_still_checks_constructor_arguments() {
        let span = fixture_span();
        let mut dae = dae::Dae::default();
        let mut constructor = Function::new("Pkg.Generic", span);
        constructor.is_constructor = true;
        constructor.add_input(FunctionParam::new("eta", "Real", span));
        dae.symbols
            .functions
            .insert(VarName::new("Pkg.Generic"), constructor);
        dae.continuous.equations.push(dae::Equation::residual(
            Expression::FunctionCall {
                name: VarName::new("Pkg.Generic").into(),
                args: vec![Expression::FunctionCall {
                    name: VarName::new("Pkg.missingBody").into(),
                    args: vec![],
                    is_constructor: false,
                    span,
                }],
                is_constructor: true,
                span,
            },
            span,
            "record constructor residual",
        ));
        dae.symbols.functions.insert(
            VarName::new("Pkg.missingBody"),
            Function::new("Pkg.missingBody", span),
        );

        let err = validate_solve_input_appendix_b_invariants(&dae)
            .expect_err("constructor arguments must still be validated");
        let reason = err.reason();
        assert!(
            reason.contains("Pkg.missingBody")
                && reason.contains("function has no executable body"),
            "{reason}"
        );
    }
}
