//! Appendix B invariant checks at the DAE -> Solve boundary.

use std::collections::HashSet;

use rumoca_core::{ExpressionVisitor, FallibleExpressionVisitor};
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
    for (context, expr) in dae_appendix_b_expressions(dae_model, IncludeInitial::No) {
        validate_no_source_temporal_operator(expr, &context)?;
        validate_no_flow_action_expression(expr, &context)?;
    }
    for (context, expr) in dae_appendix_b_expressions(dae_model, IncludeInitial::Yes) {
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
    validate_scalar_program_block(
        "continuous.residual",
        &problem.continuous.residual,
        SeedUse::Forbidden,
    )?;
    validate_compute_block(
        "continuous.derivative_rhs",
        &problem.continuous.derivative_rhs,
        SeedUse::Forbidden,
    )?;
    validate_scalar_program_block(
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
        return Err(LowerError::Unsupported {
            reason: format!(
                "events.action_conditions has {} rows but events.actions has {} entries",
                problem.events.action_conditions.len(),
                problem.events.actions.len()
            ),
        });
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
) -> Vec<(String, &rumoca_core::Expression)> {
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
            .map(|(idx, expr)| (format!("relation[{idx}]"), expr)),
    );
    exprs.extend(
        dae_model
            .events
            .event_actions
            .iter()
            .enumerate()
            .map(|(idx, action)| (format!("event_actions[{idx}].condition"), &action.condition)),
    );
    exprs
}

fn push_equation_rhs<'a>(
    exprs: &mut Vec<(String, &'a rumoca_core::Expression)>,
    partition: &str,
    equations: &'a [dae::Equation],
) {
    exprs.extend(equations.iter().enumerate().map(|(idx, eq)| {
        (
            format!("{partition}[{idx}] origin='{}'", eq.origin),
            &eq.rhs,
        )
    }));
}

fn validate_no_source_temporal_operator(
    expr: &rumoca_core::Expression,
    context: &str,
) -> Result<(), LowerError> {
    if let Some(operator) = find_source_temporal_operator(expr) {
        return Err(LowerError::Unsupported {
            reason: format!(
                "Solve Appendix-B validation failed: {context} contains `{operator}`; \
                 phase-dae must lower source temporal operators to Appendix B variables, \
                 conditions, schedules, and ordinary equations before Solve-IR lowering"
            ),
        });
    }
    Ok(())
}

fn find_source_temporal_operator(expr: &rumoca_core::Expression) -> Option<&'static str> {
    let mut checker = SourceTemporalOperatorChecker { found: None };
    checker.visit_expression(expr);
    checker.found
}

fn validate_no_flow_action_expression(
    expr: &rumoca_core::Expression,
    context: &str,
) -> Result<(), LowerError> {
    if let Some(action) = find_flow_action_expression(expr) {
        return Err(LowerError::Unsupported {
            reason: format!(
                "Solve Appendix-B validation failed: {context} contains `{action}` as a value expression; \
                 phase-dae must lower reinit to guarded discrete updates and preserve only assert/terminate as event actions"
            ),
        });
    }
    Ok(())
}

fn find_flow_action_expression(expr: &rumoca_core::Expression) -> Option<&'static str> {
    let mut checker = FlowActionExpressionChecker { found: None };
    checker.visit_expression(expr);
    checker.found
}

struct FlowActionExpressionChecker {
    found: Option<&'static str>,
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
            self.found = Some(action);
            return;
        }
        self.walk_expression(expr);
    }
}

struct SourceTemporalOperatorChecker {
    found: Option<&'static str>,
}

impl ExpressionVisitor for SourceTemporalOperatorChecker {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if self.found.is_some() {
            return;
        }
        match expr {
            rumoca_core::Expression::BuiltinCall { function, .. } => {
                if let Some(operator) = rumoca_core::source_temporal_builtin_name(*function) {
                    self.found = Some(operator);
                    return;
                }
            }
            rumoca_core::Expression::FunctionCall { name, .. } => {
                if let Some(operator) =
                    rumoca_core::source_temporal_function_short_name(name.as_str())
                {
                    self.found = Some(operator);
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
            self.found = Some(operator);
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
            self.found = Some(operator);
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
        _is_constructor: bool,
    ) -> Result<(), Self::Error> {
        if !is_named_function_arg_marker(name)
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
        _span: Option<rumoca_core::Span>,
        ops: &[solve::LinearOp],
    ) -> Result<(), Self::Error> {
        validate_row_ops(
            &format!("{}.programs[{program_index}]", self.context),
            ops,
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
            ..
        } => {
            validate_tensor_metadata(context, metadata)?;
            validate_row_ops(&format!("{context}.lhs_ops"), lhs_ops, false, seed_use)?;
            validate_row_ops(&format!("{context}.rhs_ops"), rhs_ops, false, seed_use)?;
            validate_defined_range(context, "lhs", lhs_ops, *lhs_start, m.saturating_mul(*k))?;
            validate_defined_range(context, "rhs", rhs_ops, *rhs_start, k.saturating_mul(*n))
        }
        solve::ComputeNode::LinSolve {
            setup_ops,
            matrix_start,
            rhs_start,
            n,
            next_reg,
            metadata,
            ..
        } => {
            validate_tensor_metadata(context, metadata)?;
            validate_row_ops(&format!("{context}.setup_ops"), setup_ops, false, seed_use)?;
            validate_defined_range(
                context,
                "matrix",
                setup_ops,
                *matrix_start,
                n.saturating_mul(*n),
            )?;
            validate_defined_range(context, "rhs", setup_ops, *rhs_start, *n)?;
            if *next_reg < next_free_reg(setup_ops) {
                return Err(solve_validation_error(format!(
                    "{context}: LinSolve next_reg {next_reg} overlaps registers used by setup_ops"
                )));
            }
            Ok(())
        }
    }
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
    require_store_output: bool,
    seed_use: SeedUse,
) -> Result<(), LowerError> {
    let mut defined = HashSet::new();
    let mut saw_store_output = false;
    for (op_idx, op) in ops.iter().enumerate() {
        validate_op_inputs(context, op_idx, op, &defined, seed_use)?;
        if let Some(dst) = op.dst_register() {
            defined.insert(dst);
        }
        if matches!(op, solve::LinearOp::StoreOutput { .. }) {
            saw_store_output = true;
        }
    }
    if require_store_output && !saw_store_output {
        return Err(solve_validation_error(format!(
            "{context}: row has no StoreOutput op"
        )));
    }
    Ok(())
}

fn validate_op_inputs(
    context: &str,
    op_idx: usize,
    op: &solve::LinearOp,
    defined: &HashSet<solve::Reg>,
    seed_use: SeedUse,
) -> Result<(), LowerError> {
    match op {
        solve::LinearOp::Const { .. }
        | solve::LinearOp::LoadTime { .. }
        | solve::LinearOp::LoadY { .. }
        | solve::LinearOp::LoadP { .. } => Ok(()),
        solve::LinearOp::LoadSeed { .. } if seed_use == SeedUse::Allowed => Ok(()),
        solve::LinearOp::LoadSeed { index, .. } => Err(solve_validation_error(format!(
            "{context}[{op_idx}]: LoadSeed[{index}] is only valid in derivative/JVP artifact rows"
        ))),
        solve::LinearOp::Move { src, .. }
        | solve::LinearOp::Unary { arg: src, .. }
        | solve::LinearOp::StoreOutput { src } => {
            validate_defined_reg(context, op_idx, *src, defined)
        }
        solve::LinearOp::Binary { lhs, rhs, .. } | solve::LinearOp::Compare { lhs, rhs, .. } => {
            validate_defined_reg(context, op_idx, *lhs, defined)?;
            validate_defined_reg(context, op_idx, *rhs, defined)
        }
        solve::LinearOp::Select {
            cond,
            if_true,
            if_false,
            ..
        } => {
            validate_defined_reg(context, op_idx, *cond, defined)?;
            validate_defined_reg(context, op_idx, *if_true, defined)?;
            validate_defined_reg(context, op_idx, *if_false, defined)
        }
        solve::LinearOp::LinearSolveComponent { .. } => {
            validate_linear_solve_component_op(context, op_idx, op, defined)
        }
        solve::LinearOp::TableBounds { .. }
        | solve::LinearOp::TableLookup { .. }
        | solve::LinearOp::TableLookupSlope { .. }
        | solve::LinearOp::TableNextEvent { .. } => {
            validate_table_op_inputs(context, op_idx, op, defined)
        }
        solve::LinearOp::RandomInitialState { .. }
        | solve::LinearOp::RandomResult { .. }
        | solve::LinearOp::RandomState { .. }
        | solve::LinearOp::ImpureRandomInit { .. }
        | solve::LinearOp::ImpureRandom { .. }
        | solve::LinearOp::ImpureRandomInteger { .. } => {
            validate_random_op_inputs(context, op_idx, op, defined)
        }
        solve::LinearOp::ExternalCall {
            args, arg_count, ..
        } => args
            .iter()
            .copied()
            .take(*arg_count)
            .try_for_each(|arg| validate_defined_reg(context, op_idx, arg, defined)),
    }
}

fn validate_linear_solve_component_op(
    context: &str,
    op_idx: usize,
    op: &solve::LinearOp,
    defined: &HashSet<solve::Reg>,
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
        return Err(solve_validation_error(format!(
            "{context}[{op_idx}]: LinearSolveComponent component {component} is outside n={n}"
        )));
    }
    validate_defined_reg_range(context, op_idx, matrix_start, n.saturating_mul(n), defined)?;
    validate_defined_reg_range(context, op_idx, rhs_start, n, defined)
}

fn validate_table_op_inputs(
    context: &str,
    op_idx: usize,
    op: &solve::LinearOp,
    defined: &HashSet<solve::Reg>,
) -> Result<(), LowerError> {
    match *op {
        solve::LinearOp::TableBounds { table_id, .. } => {
            validate_defined_reg(context, op_idx, table_id, defined)
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
            validate_defined_reg(context, op_idx, table_id, defined)?;
            validate_defined_reg(context, op_idx, column, defined)?;
            validate_defined_reg(context, op_idx, input, defined)
        }
        solve::LinearOp::TableNextEvent { table_id, time, .. } => {
            validate_defined_reg(context, op_idx, table_id, defined)?;
            validate_defined_reg(context, op_idx, time, defined)
        }
        _ => Ok(()),
    }
}

fn validate_random_op_inputs(
    context: &str,
    op_idx: usize,
    op: &solve::LinearOp,
    defined: &HashSet<solve::Reg>,
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
            )?;
            validate_defined_reg(context, op_idx, local_seed, defined)?;
            validate_defined_reg(context, op_idx, global_seed, defined)
        }
        solve::LinearOp::RandomResult {
            state_start,
            state_len,
            ..
        } => validate_defined_reg_range(context, op_idx, state_start, state_len, defined),
        solve::LinearOp::RandomState {
            state_start,
            state_len,
            state_index,
            ..
        } => {
            validate_state_index(context, op_idx, "RandomState", state_index, state_len)?;
            validate_defined_reg_range(context, op_idx, state_start, state_len, defined)
        }
        solve::LinearOp::ImpureRandomInit { seed, .. } => {
            validate_defined_reg(context, op_idx, seed, defined)
        }
        solve::LinearOp::ImpureRandom { id, .. } => {
            validate_defined_reg(context, op_idx, id, defined)
        }
        solve::LinearOp::ImpureRandomInteger { id, imin, imax, .. } => {
            validate_defined_reg(context, op_idx, id, defined)?;
            validate_defined_reg(context, op_idx, imin, defined)?;
            validate_defined_reg(context, op_idx, imax, defined)
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
) -> Result<(), LowerError> {
    if state_index >= state_len {
        return Err(solve_validation_error(format!(
            "{context}[{op_idx}]: {op_name} state_index {state_index} is outside state_len={state_len}"
        )));
    }
    Ok(())
}

fn validate_defined_range(
    context: &str,
    name: &str,
    ops: &[solve::LinearOp],
    start: solve::Reg,
    len: usize,
) -> Result<(), LowerError> {
    let defined = ops
        .iter()
        .filter_map(solve::LinearOp::dst_register)
        .collect::<HashSet<_>>();
    for offset in 0..len {
        let reg = start + offset as solve::Reg;
        if !defined.contains(&reg) {
            return Err(solve_validation_error(format!(
                "{context}: {name} register range starting at {start} has undefined register {reg}"
            )));
        }
    }
    Ok(())
}

fn validate_defined_reg(
    context: &str,
    op_idx: usize,
    reg: solve::Reg,
    defined: &HashSet<solve::Reg>,
) -> Result<(), LowerError> {
    if !defined.contains(&reg) {
        return Err(solve_validation_error(format!(
            "{context}[{op_idx}]: reads undefined register {reg}"
        )));
    }
    Ok(())
}

fn validate_defined_reg_range(
    context: &str,
    op_idx: usize,
    start: solve::Reg,
    len: usize,
    defined: &HashSet<solve::Reg>,
) -> Result<(), LowerError> {
    for offset in 0..len {
        validate_defined_reg(context, op_idx, start + offset as solve::Reg, defined)?;
    }
    Ok(())
}

fn next_free_reg(ops: &[solve::LinearOp]) -> solve::Reg {
    ops.iter()
        .filter_map(solve::LinearOp::dst_register)
        .max()
        .map_or(0, |reg| reg.saturating_add(1))
}

fn solve_validation_error(reason: String) -> LowerError {
    LowerError::Unsupported {
        reason: format!("Solve Appendix-B validation failed: {reason}"),
    }
}
