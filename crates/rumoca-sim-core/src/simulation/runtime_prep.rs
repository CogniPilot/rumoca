use std::collections::{HashMap, HashSet};

use rumoca_core::{
    Span, maybe_elapsed_seconds as trace_timer_elapsed_seconds,
    maybe_start_timer_if as trace_timer_start_if,
};
use rumoca_ir_core::{OpBinary, OpUnary};
use rumoca_ir_dae as dae;
use rumoca_phase_solve_lower::{VarEnv, build_runtime_parameter_tail_env};

use crate::runtime::timeout::{TimeoutBudget, TimeoutExceeded};
use crate::simulation::dae_prepare::{expr_contains_der_of, expr_refers_to_var};
use crate::simulation::diagnostics::sim_trace_enabled;
use crate::simulation::pipeline::MassMatrix;

#[derive(Debug, thiserror::Error)]
pub enum MassMatrixBuildError {
    #[error("timeout after {seconds:.3}s")]
    Timeout { seconds: f64 },
    #[error(
        "cannot derive DiffSol mass-matrix row {row} for state '{state_name}' from equation '{origin}': {reason}"
    )]
    NonDerivable {
        row: usize,
        state_name: String,
        origin: String,
        reason: String,
    },
}

impl From<TimeoutExceeded> for MassMatrixBuildError {
    fn from(value: TimeoutExceeded) -> Self {
        Self::Timeout {
            seconds: value.seconds,
        }
    }
}

const MASS_MATRIX_EPS: f64 = 1.0e-15;

#[cfg(not(target_arch = "wasm32"))]
type CompiledExpressionRows = rumoca_phase_solve_lower::CompiledExpressionRows;
#[cfg(target_arch = "wasm32")]
type CompiledExpressionRows = rumoca_phase_solve_lower::CompiledExpressionRowsWasm;

struct CompiledMassMatrixRow {
    active_cols: Vec<usize>,
    compiled_rows: CompiledExpressionRows,
}

struct CompiledMassMatrixContext {
    zero_y: Vec<f64>,
    compiled_p: Vec<f64>,
    row_plans: Vec<CompiledMassMatrixRow>,
    out_scratch: Vec<f64>,
}

fn real_expr(value: f64) -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Real(value))
}

fn unary_minus_expr(rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Unary {
        op: OpUnary::Minus(Default::default()),
        rhs: Box::new(rhs),
    }
}

fn binary_expr(op: OpBinary, lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn add_expr(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    binary_expr(OpBinary::Add(Default::default()), lhs, rhs)
}

fn sub_expr(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    binary_expr(OpBinary::Sub(Default::default()), lhs, rhs)
}

fn mul_expr(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    binary_expr(OpBinary::Mul(Default::default()), lhs, rhs)
}

fn div_expr(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    binary_expr(OpBinary::Div(Default::default()), lhs, rhs)
}

fn combine_add_sub_coeffs(
    lhs: Option<dae::Expression>,
    rhs: Option<dae::Expression>,
    subtract_rhs: bool,
) -> Option<dae::Expression> {
    match (lhs, rhs) {
        (None, None) => None,
        (Some(l), None) => Some(l),
        (None, Some(r)) => {
            if subtract_rhs {
                Some(unary_minus_expr(r))
            } else {
                Some(r)
            }
        }
        (Some(l), Some(r)) => {
            if subtract_rhs {
                Some(sub_expr(l, r))
            } else {
                Some(add_expr(l, r))
            }
        }
    }
}

pub fn derivative_coefficient_expr(
    expr: &dae::Expression,
    state_name: &dae::VarName,
) -> Result<Option<dae::Expression>, String> {
    match expr {
        dae::Expression::BuiltinCall { function, args }
            if *function == dae::BuiltinFunction::Der =>
        {
            if args.len() != 1 {
                return Err("der() must have exactly one argument".to_string());
            }
            if args
                .first()
                .is_some_and(|a| expr_refers_to_var(a, state_name))
            {
                return Ok(Some(real_expr(1.0)));
            }
            Ok(None)
        }
        dae::Expression::Unary {
            op: OpUnary::Minus(_),
            rhs,
        } => {
            let inner = derivative_coefficient_expr(rhs, state_name)?;
            Ok(inner.map(unary_minus_expr))
        }
        dae::Expression::Binary { op, lhs, rhs } => match op {
            OpBinary::Add(_) => {
                let lhs_coeff = derivative_coefficient_expr(lhs, state_name)?;
                let rhs_coeff = derivative_coefficient_expr(rhs, state_name)?;
                Ok(combine_add_sub_coeffs(lhs_coeff, rhs_coeff, false))
            }
            OpBinary::Sub(_) => {
                let lhs_coeff = derivative_coefficient_expr(lhs, state_name)?;
                let rhs_coeff = derivative_coefficient_expr(rhs, state_name)?;
                Ok(combine_add_sub_coeffs(lhs_coeff, rhs_coeff, true))
            }
            OpBinary::Mul(_) => {
                let lhs_has_der = expr_contains_der_of(lhs, state_name);
                let rhs_has_der = expr_contains_der_of(rhs, state_name);
                if lhs_has_der && rhs_has_der {
                    return Err("nonlinear derivative term in multiplication".to_string());
                }
                if lhs_has_der {
                    let lhs_coeff = derivative_coefficient_expr(lhs, state_name)?;
                    let Some(coeff) = lhs_coeff else {
                        return Err("unable to extract derivative coefficient from lhs".to_string());
                    };
                    return Ok(Some(mul_expr(coeff, rhs.as_ref().clone())));
                }
                if rhs_has_der {
                    let rhs_coeff = derivative_coefficient_expr(rhs, state_name)?;
                    let Some(coeff) = rhs_coeff else {
                        return Err("unable to extract derivative coefficient from rhs".to_string());
                    };
                    return Ok(Some(mul_expr(lhs.as_ref().clone(), coeff)));
                }
                Ok(None)
            }
            OpBinary::Div(_) => {
                let rhs_has_der = expr_contains_der_of(rhs, state_name);
                if rhs_has_der {
                    return Err("nonlinear derivative term in denominator".to_string());
                }
                if !expr_contains_der_of(lhs, state_name) {
                    return Ok(None);
                }
                let lhs_coeff = derivative_coefficient_expr(lhs, state_name)?;
                let Some(coeff) = lhs_coeff else {
                    return Err(
                        "unable to extract derivative coefficient from numerator".to_string()
                    );
                };
                Ok(Some(div_expr(coeff, rhs.as_ref().clone())))
            }
            _ => {
                if expr_contains_der_of(expr, state_name) {
                    Err("unsupported derivative-dependent operator".to_string())
                } else {
                    Ok(None)
                }
            }
        },
        _ => {
            if expr_contains_der_of(expr, state_name) {
                Err("unsupported derivative-dependent expression shape".to_string())
            } else {
                Ok(None)
            }
        }
    }
}

fn state_name_for_row(state_names: &[dae::VarName], row: usize) -> String {
    state_names
        .get(row)
        .map_or_else(|| format!("<state-{row}>"), ToString::to_string)
}

fn row_origin_for_error(dae_model: &dae::Dae, row: usize) -> String {
    dae_model.f_x.get(row).map_or_else(
        || "<missing equation row>".to_string(),
        |eq| eq.origin.clone(),
    )
}

fn non_derivable_error(
    row: usize,
    state_name: String,
    origin: String,
    reason: impl Into<String>,
) -> MassMatrixBuildError {
    MassMatrixBuildError::NonDerivable {
        row,
        state_name,
        origin,
        reason: reason.into(),
    }
}

fn row_non_derivable_error(
    dae_model: &dae::Dae,
    state_names: &[dae::VarName],
    row: usize,
    reason: impl Into<String>,
) -> MassMatrixBuildError {
    non_derivable_error(
        row,
        state_name_for_row(state_names, row),
        row_origin_for_error(dae_model, row),
        reason,
    )
}

fn state_non_derivable_error(
    eq: &dae::Equation,
    row: usize,
    state_name: &dae::VarName,
    reason: impl Into<String>,
) -> MassMatrixBuildError {
    non_derivable_error(row, state_name.to_string(), eq.origin.clone(), reason)
}

#[cfg(not(target_arch = "wasm32"))]
fn compile_mass_matrix_expression_rows(
    dae_model: &dae::Dae,
    expressions: &[dae::Expression],
) -> Result<CompiledExpressionRows, String> {
    rumoca_phase_solve_lower::compile_expressions_with_solver_len(
        dae_model,
        expressions,
        rumoca_phase_solve_lower::Backend::Cranelift,
        dae_model.f_x.len(),
    )
    .map_err(|err| err.to_string())
}

#[cfg(target_arch = "wasm32")]
fn compile_mass_matrix_expression_rows(
    dae_model: &dae::Dae,
    expressions: &[dae::Expression],
) -> Result<CompiledExpressionRows, String> {
    rumoca_phase_solve_lower::compile_expressions_wasm(dae_model, expressions)
        .map_err(|err| err.to_string())
}

fn build_mass_matrix_parameter_tail_env(
    dae_model: &dae::Dae,
    param_values: &[f64],
    budget: &TimeoutBudget,
    trace: bool,
) -> Result<VarEnv<f64>, MassMatrixBuildError> {
    let env_t0 = trace_timer_start_if(trace);
    if trace {
        eprintln!("[sim-trace] mass_matrix compute start: runtime tail env");
    }
    let env_zero = build_runtime_parameter_tail_env(dae_model, param_values, 0.0);
    budget.check()?;
    if trace {
        eprintln!(
            "[sim-trace] mass_matrix compute done: runtime tail env elapsed={:.3}s",
            trace_timer_elapsed_seconds(env_t0)
        );
    }
    Ok(env_zero)
}

fn build_compiled_mass_matrix_row(
    dae_model: &dae::Dae,
    row: usize,
    state_names: &[dae::VarName],
) -> Result<CompiledMassMatrixRow, MassMatrixBuildError> {
    let Some(eq) = dae_model.f_x.get(row) else {
        return Err(row_non_derivable_error(
            dae_model,
            state_names,
            row,
            "state row is missing from f_x",
        ));
    };

    let mut expressions = Vec::new();
    let mut active_cols = Vec::new();
    for (col, state_name) in state_names.iter().enumerate() {
        if !expr_contains_der_of(&eq.rhs, state_name) {
            continue;
        }
        let coeff_expr = derivative_coefficient_expr(&eq.rhs, state_name)
            .map_err(|reason| state_non_derivable_error(eq, row, state_name, reason))?;
        let Some(coeff_expr) = coeff_expr else {
            return Err(state_non_derivable_error(
                eq,
                row,
                state_name,
                "unable to isolate derivative coefficient",
            ));
        };
        expressions.push(coeff_expr);
        active_cols.push(col);
    }

    if active_cols.is_empty() {
        return Err(row_non_derivable_error(
            dae_model,
            state_names,
            row,
            "equation row does not contain any der(state) term",
        ));
    }
    let compiled_rows =
        compile_mass_matrix_expression_rows(dae_model, &expressions).map_err(|err| {
            row_non_derivable_error(
                dae_model,
                state_names,
                row,
                format!("compiled derivative coefficient row unsupported: {err}"),
            )
        })?;

    Ok(CompiledMassMatrixRow {
        active_cols,
        compiled_rows,
    })
}

fn build_compiled_mass_matrix_context(
    dae_model: &dae::Dae,
    n_x: usize,
    state_names: &[dae::VarName],
    param_values: &[f64],
    budget: &TimeoutBudget,
    trace: bool,
) -> Result<CompiledMassMatrixContext, MassMatrixBuildError> {
    let tail_env = build_mass_matrix_parameter_tail_env(dae_model, param_values, budget, trace)?;
    let sim_context =
        crate::runtime::layout::SimulationContext::from_dae(dae_model, dae_model.f_x.len());
    let compiled_p = sim_context.compiled_parameter_vector_from_env(param_values, &tail_env);
    let mut row_plans = Vec::with_capacity(n_x);
    for row in 0..n_x {
        if row.is_multiple_of(16) {
            budget.check()?;
        }
        row_plans.push(build_compiled_mass_matrix_row(dae_model, row, state_names)?);
    }
    Ok(CompiledMassMatrixContext {
        zero_y: vec![0.0; dae_model.f_x.len()],
        compiled_p,
        row_plans,
        out_scratch: Vec::new(),
    })
}

fn fill_mass_matrix_row(
    dae_model: &dae::Dae,
    row: usize,
    row_coeffs: &mut [f64],
    state_names: &[dae::VarName],
    compiled: &mut CompiledMassMatrixContext,
) -> Result<(), MassMatrixBuildError> {
    let Some(eq) = dae_model.f_x.get(row) else {
        return Err(row_non_derivable_error(
            dae_model,
            state_names,
            row,
            "state row is missing from f_x",
        ));
    };
    let plan = compiled.row_plans.get(row).ok_or_else(|| {
        row_non_derivable_error(dae_model, state_names, row, "missing compiled row plan")
    })?;
    compiled.out_scratch.resize(plan.active_cols.len(), 0.0);
    plan.compiled_rows
        .call(
            compiled.zero_y.as_slice(),
            compiled.compiled_p.as_slice(),
            0.0,
            compiled.out_scratch.as_mut_slice(),
        )
        .map_err(|err| {
            row_non_derivable_error(
                dae_model,
                state_names,
                row,
                format!("compiled derivative coefficient row call failed: {err}"),
            )
        })?;

    let mut has_nonzero = false;
    for (idx, col) in plan.active_cols.iter().copied().enumerate() {
        let coeff_val = compiled.out_scratch.get(idx).copied().unwrap_or(0.0);
        if !coeff_val.is_finite() {
            return Err(state_non_derivable_error(
                eq,
                row,
                &state_names[col],
                "derivative coefficient evaluates to a non-finite value",
            ));
        }
        if coeff_val.abs() > MASS_MATRIX_EPS {
            row_coeffs[col] = coeff_val;
            has_nonzero = true;
        }
    }

    if !has_nonzero {
        return Err(row_non_derivable_error(
            dae_model,
            state_names,
            row,
            "all derivative coefficients evaluate to approximately zero",
        ));
    }
    Ok(())
}

/// Expand state entries into scalar-level names.
///
/// Array states (e.g. `x` with dims=[2]) are expanded to `x[1]`, `x[2]` so
/// that the mass-matrix builder can match individual scalarized derivative
/// terms such as `der(x[1])`.
fn expand_state_scalar_names(dae_model: &dae::Dae) -> Vec<dae::VarName> {
    let mut names = Vec::new();
    for (name, var) in &dae_model.states {
        let sz = var.size();
        if sz <= 1 {
            names.push(name.clone());
        } else {
            for i in 1..=sz {
                names.push(dae::VarName::new(format!("{}[{}]", name.as_str(), i)));
            }
        }
    }
    names
}

pub fn compute_mass_matrix(
    dae_model: &dae::Dae,
    n_x: usize,
    param_values: &[f64],
    budget: &TimeoutBudget,
) -> Result<MassMatrix, MassMatrixBuildError> {
    let trace = sim_trace_enabled();
    if trace {
        eprintln!(
            "[sim-trace] mass_matrix compute start: n_x={} n_eq={}",
            n_x,
            dae_model.f_x.len()
        );
    }
    let state_names = expand_state_scalar_names(dae_model);
    let mut mass_matrix = vec![vec![0.0; n_x]; n_x];
    let mut compiled = build_compiled_mass_matrix_context(
        dae_model,
        n_x,
        &state_names,
        param_values,
        budget,
        trace,
    )?;

    for (row, row_coeffs) in mass_matrix.iter_mut().enumerate().take(n_x) {
        if row.is_multiple_of(16) {
            budget.check()?;
        }
        if trace && row > 0 && row.is_multiple_of(50) {
            eprintln!("[sim-trace] mass_matrix compute progress: row={row}/{n_x}");
        }
        fill_mass_matrix_row(dae_model, row, row_coeffs, &state_names, &mut compiled)?;
    }

    if trace {
        eprintln!(
            "[sim-trace] mass_matrix compute done: rows processed={}",
            n_x
        );
    }
    Ok(mass_matrix)
}

fn build_pin_equation(
    name: &dae::VarName,
    sz: usize,
    idx: usize,
    start_expr: dae::Expression,
) -> dae::Equation {
    let var_ref = if sz <= 1 {
        dae::Expression::VarRef {
            name: name.clone(),
            subscripts: vec![],
        }
    } else {
        dae::Expression::VarRef {
            name: name.clone(),
            subscripts: vec![dae::Subscript::Index((idx + 1) as i64)],
        }
    };
    dae::Equation {
        lhs: None,
        rhs: dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(var_ref),
            rhs: Box::new(start_expr),
        },
        span: Span::DUMMY,
        origin: "orphaned_variable_pin".to_string(),
        scalar_count: 1,
    }
}

#[derive(Debug, Clone)]
struct PinScalarCandidate {
    name: dae::VarName,
    scalar_name: String,
    scalar_idx: usize,
    base: String,
    size: usize,
    start_expr: dae::Expression,
    is_preferred: bool,
    is_substitution_linked: bool,
    refs: usize,
    has_direct_assignment: bool,
}

struct PinScalarInputs<'a> {
    direct_assignment_targets: &'a HashSet<String>,
    direct_assignment_scalar_starts: &'a HashMap<String, dae::Expression>,
    diff_derivative_assignment_targets: &'a HashSet<String>,
    algebraic_ref_counts: &'a HashMap<String, usize>,
    preferred_pin_scalars: &'a HashSet<String>,
    state_alias_members: &'a HashSet<String>,
    substitution_linked_names: &'a HashSet<String>,
    runtime_defined_unknowns: &'a HashSet<String>,
    ref_counts: &'a HashMap<String, usize>,
    scalarization: &'a rumoca_phase_structural::scalarize::ExpressionScalarizationContext,
}

fn add_substitution_link_name(set: &mut HashSet<String>, name: &str) {
    set.insert(name.to_string());
    if let Some(base) = dae::component_base_name(name) {
        set.insert(base);
    }
}

fn collect_substitution_linked_names(
    elim: &rumoca_phase_structural::eliminate::EliminationResult,
) -> HashSet<String> {
    let mut linked = HashSet::new();
    for sub in &elim.substitutions {
        add_substitution_link_name(&mut linked, sub.var_name.as_str());
        for key in &sub.env_keys {
            add_substitution_link_name(&mut linked, key);
        }
        let mut refs = HashSet::new();
        sub.expr.collect_var_refs(&mut refs);
        for name in refs {
            add_substitution_link_name(&mut linked, name.as_str());
        }
    }
    linked
}

fn component_base_name(name: &str) -> Option<String> {
    dae::component_base_name(name)
}

fn expr_contains_der_call(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            if *function == dae::BuiltinFunction::Der {
                return true;
            }
            args.iter().any(expr_contains_der_call)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_der_call(lhs) || expr_contains_der_call(rhs)
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_der_call(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, branch)| {
                expr_contains_der_call(cond) || expr_contains_der_call(branch)
            }) || expr_contains_der_call(else_branch)
        }
        dae::Expression::FunctionCall { args, .. } => args.iter().any(expr_contains_der_call),
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expr_contains_der_call)
        }
        dae::Expression::Range { start, step, end } => {
            expr_contains_der_call(start)
                || step.as_deref().is_some_and(expr_contains_der_call)
                || expr_contains_der_call(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_der_call(expr)
                || indices.iter().any(|idx| expr_contains_der_call(&idx.range))
                || filter.as_deref().is_some_and(expr_contains_der_call)
        }
        dae::Expression::Index { base, subscripts } => {
            expr_contains_der_call(base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(expr) => expr_contains_der_call(expr),
                    _ => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_contains_der_call(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn collect_derivative_assignment_target_bases(expr: &dae::Expression, out: &mut HashSet<String>) {
    match expr {
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
            lhs,
            rhs,
        } => {
            if let dae::Expression::VarRef { name, .. } = lhs.as_ref()
                && expr_contains_der_call(rhs)
                && let Some(base) = component_base_name(name.as_str())
            {
                out.insert(base);
            }
            if let dae::Expression::VarRef { name, .. } = rhs.as_ref()
                && expr_contains_der_call(lhs)
                && let Some(base) = component_base_name(name.as_str())
            {
                out.insert(base);
            }
        }
        dae::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_),
            rhs,
        } => collect_derivative_assignment_target_bases(rhs, out),
        _ => {}
    }
}

fn collect_diff_row_derivative_assignment_targets(
    dae_model: &dae::Dae,
    n_x: usize,
) -> HashSet<String> {
    let mut targets = HashSet::new();
    for eq in dae_model.f_x.iter().take(n_x) {
        collect_derivative_assignment_target_bases(&eq.rhs, &mut targets);
    }
    targets
}

fn build_pin_scalar_candidates(
    dae_model: &dae::Dae,
    inputs: &PinScalarInputs<'_>,
) -> Vec<PinScalarCandidate> {
    let mut all_vars = Vec::new();
    for (n, v) in dae_model.algebraics.iter().chain(dae_model.outputs.iter()) {
        let base = component_base_name(n.as_str()).unwrap_or_else(|| n.as_str().into());
        if inputs.runtime_defined_unknowns.contains(n.as_str())
            || inputs.runtime_defined_unknowns.contains(&base)
        {
            continue;
        }
        let algebraic_refs = inputs.algebraic_ref_counts.get(&base).copied().unwrap_or(0);
        let is_output = dae_model.outputs.contains_key(n);
        let skip_diff_derivative_target = inputs.diff_derivative_assignment_targets.contains(&base)
            && (is_output || algebraic_refs > 0);
        let refs = inputs.ref_counts.get(&base).copied().unwrap_or(0);
        let has_direct_assignment = inputs.direct_assignment_targets.contains(&base);
        if has_direct_assignment
            || inputs.state_alias_members.contains(n.as_str())
            || inputs.state_alias_members.contains(&base)
        {
            continue;
        }
        let size = v.size();

        if size <= 1 {
            let scalar_name = n.as_str().to_string();
            if inputs.state_alias_members.contains(&scalar_name) {
                continue;
            }
            let is_preferred = inputs.preferred_pin_scalars.contains(&scalar_name)
                || inputs.preferred_pin_scalars.contains(&base);
            if skip_diff_derivative_target && !is_preferred {
                continue;
            }
            let is_substitution_linked = inputs.substitution_linked_names.contains(&scalar_name)
                || inputs.substitution_linked_names.contains(&base);
            let start_expr = inputs
                .direct_assignment_scalar_starts
                .get(&scalar_name)
                .cloned()
                .unwrap_or_else(|| scalar_pin_start_expr(v, size, 0, inputs.scalarization));
            all_vars.push(PinScalarCandidate {
                name: n.clone(),
                scalar_name,
                scalar_idx: 0,
                base: base.clone(),
                size,
                start_expr,
                is_preferred,
                is_substitution_linked,
                refs,
                has_direct_assignment,
            });
            continue;
        }

        for scalar_idx in 0..size {
            let scalar_name = format!("{}[{}]", n.as_str(), scalar_idx + 1);
            if inputs.state_alias_members.contains(&scalar_name) {
                continue;
            }
            let is_preferred = inputs.preferred_pin_scalars.contains(&scalar_name);
            if skip_diff_derivative_target && !is_preferred {
                continue;
            }
            let is_substitution_linked = inputs.substitution_linked_names.contains(&scalar_name)
                || inputs.substitution_linked_names.contains(&base);
            let start_expr = inputs
                .direct_assignment_scalar_starts
                .get(&scalar_name)
                .cloned()
                .unwrap_or_else(|| {
                    scalar_pin_start_expr(v, size, scalar_idx, inputs.scalarization)
                });
            all_vars.push(PinScalarCandidate {
                name: n.clone(),
                scalar_name,
                scalar_idx,
                base: base.clone(),
                size,
                start_expr,
                is_preferred,
                is_substitution_linked,
                refs,
                has_direct_assignment,
            });
        }
    }
    all_vars
}

fn scalar_pin_start_expr(
    var: &dae::Variable,
    size: usize,
    scalar_idx: usize,
    scalarization: &rumoca_phase_structural::scalarize::ExpressionScalarizationContext,
) -> dae::Expression {
    let Some(start) = var.start.as_ref() else {
        return dae::Expression::Literal(dae::Literal::Real(0.0));
    };
    if size <= 1 {
        return start.clone();
    }
    let rows =
        rumoca_phase_structural::scalarize::scalarize_expression_rows(start, size, scalarization);
    rows.get(scalar_idx)
        .cloned()
        .unwrap_or_else(|| start.clone())
}

fn maybe_collect_structural_relaxation_preferences(
    dae_model: &dae::Dae,
    n_x: usize,
    n_z_vars: usize,
    n_z_slots: usize,
    preferred_pin_scalars: &mut HashSet<String>,
) {
    if n_z_vars <= n_z_slots {
        return;
    }
    let Some(hint) = rumoca_phase_structural::build_ic_relaxation_hint(dae_model, n_x) else {
        return;
    };

    preferred_pin_scalars.extend(hint.dropped_unknown_names);

    if sim_trace_enabled() && !hint.dropped_eq_global.is_empty() {
        eprintln!(
            "[sim-trace] pin_orphaned_variables: structural relaxation suggested dropped_eq={} (kept physical equations, using unknown preferences only) slots={} vars={}",
            hint.dropped_eq_global.len(),
            n_z_slots,
            n_z_vars
        );
    }
}

fn collect_direct_assignment_targets_and_starts(
    dae_model: &dae::Dae,
    scalarization: &rumoca_phase_structural::scalarize::ExpressionScalarizationContext,
) -> (HashSet<String>, HashMap<String, dae::Expression>) {
    let mut direct_assignment_targets = HashSet::new();
    let mut direct_assignment_scalar_starts = HashMap::new();
    for eq in &dae_model.f_x {
        let Some((target, solution)) =
            crate::runtime::assignment::direct_assignment_from_equation(eq)
        else {
            continue;
        };
        let base = component_base_name(target.as_str()).unwrap_or_else(|| target.clone());
        direct_assignment_targets.insert(base);
        let target_size = crate::runtime::assignment::variable_size_for_assignment_name(
            dae_model,
            target.as_str(),
        )
        .unwrap_or(1);
        if target_size <= 1 || target.contains('[') {
            maybe_insert_direct_assignment_scalar_start(
                &mut direct_assignment_scalar_starts,
                target,
                solution.clone(),
            );
            continue;
        }
        let solution_rows = rumoca_phase_structural::scalarize::scalarize_expression_rows(
            solution,
            target_size,
            scalarization,
        );
        for idx in 0..target_size {
            let scalar_name = format!("{target}[{}]", idx + 1);
            let scalar_solution = solution_rows
                .get(idx)
                .cloned()
                .unwrap_or_else(|| solution.clone());
            maybe_insert_direct_assignment_scalar_start(
                &mut direct_assignment_scalar_starts,
                scalar_name,
                scalar_solution,
            );
        }
    }
    (direct_assignment_targets, direct_assignment_scalar_starts)
}

fn maybe_insert_direct_assignment_scalar_start(
    starts: &mut HashMap<String, dae::Expression>,
    scalar_name: String,
    expr: dae::Expression,
) {
    if is_safe_direct_pin_start_expr(&expr) {
        starts.insert(scalar_name, expr);
    }
}

fn is_safe_direct_pin_start_expr(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::Literal(_) | dae::Expression::VarRef { .. } => true,
        dae::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_),
            rhs,
        } => is_safe_direct_pin_start_expr(rhs),
        _ => false,
    }
}

fn collect_runtime_defined_unknowns(dae_model: &dae::Dae) -> HashSet<String> {
    let mut runtime_defined = HashSet::new();
    for name in rumoca_analysis_dae::runtime_defined_continuous_unknown_names(dae_model) {
        runtime_defined.insert(name.clone());
        if let Some(base) = component_base_name(&name) {
            runtime_defined.insert(base);
        }
    }
    runtime_defined
}

fn collect_state_alias_members(dae_model: &dae::Dae, n_x: usize) -> HashSet<String> {
    let adjacency =
        crate::runtime::alias::build_runtime_alias_adjacency_with_known_assignments(dae_model, n_x);
    let mut protected = HashSet::new();
    let mut visited = HashSet::new();

    for node in adjacency.keys() {
        if !visited.insert(node.clone()) {
            continue;
        }
        let component =
            crate::runtime::alias::collect_alias_component(node, &adjacency, &mut visited);
        let has_state = component.iter().any(|name| {
            let base = component_base_name(name.as_str()).unwrap_or_else(|| name.clone());
            dae_model.states.contains_key(&dae::VarName::new(base))
        });
        if !has_state {
            continue;
        }
        for name in component {
            crate::runtime::alias::insert_name_and_base(&mut protected, name.as_str());
        }
    }

    protected
}

fn extend_preferred_pin_scalars_from_singular_ic(
    dae_model: &dae::Dae,
    n_x: usize,
    preferred_pin_scalars: &mut HashSet<String>,
) {
    if let Err(rumoca_phase_structural::StructuralError::Singular {
        unmatched_unknowns, ..
    }) = rumoca_phase_structural::build_ic_plan(dae_model, n_x)
    {
        preferred_pin_scalars.extend(unmatched_unknowns);
    }
}

fn collect_equation_ref_counts(dae_model: &dae::Dae) -> HashMap<String, usize> {
    let mut ref_counts: HashMap<String, usize> = HashMap::new();
    for eq in &dae_model.f_x {
        let mut refs = HashSet::new();
        eq.rhs.collect_var_refs(&mut refs);
        for name in refs {
            let base = component_base_name(name.as_str()).unwrap_or_else(|| name.as_str().into());
            *ref_counts.entry(base).or_insert(0) += 1;
        }
    }
    ref_counts
}

fn collect_algebraic_equation_ref_counts(
    dae_model: &dae::Dae,
    n_x: usize,
) -> HashMap<String, usize> {
    let mut ref_counts: HashMap<String, usize> = HashMap::new();
    for eq in dae_model.f_x.iter().skip(n_x) {
        let mut refs = HashSet::new();
        eq.rhs.collect_var_refs(&mut refs);
        for name in refs {
            let base = component_base_name(name.as_str()).unwrap_or_else(|| name.as_str().into());
            *ref_counts.entry(base).or_insert(0) += 1;
        }
    }
    ref_counts
}

fn sort_pin_candidates(candidates: &mut [PinScalarCandidate]) {
    candidates.sort_by(|a, b| {
        (!a.is_preferred)
            .cmp(&(!b.is_preferred))
            .then(a.has_direct_assignment.cmp(&b.has_direct_assignment))
            .then(a.is_substitution_linked.cmp(&b.is_substitution_linked))
            .then((a.refs == 0).cmp(&(b.refs == 0)))
            .then(a.refs.cmp(&b.refs))
            .then(a.base.cmp(&b.base))
            .then(a.scalar_name.cmp(&b.scalar_name))
            .then(a.scalar_idx.cmp(&b.scalar_idx))
    });
}

fn should_force_preferred_pin_fallback(candidate: &PinScalarCandidate) -> bool {
    !(candidate.is_substitution_linked && candidate.refs == 0)
}

struct PinTraceMeta<'a> {
    n_x: usize,
    n_z_slots: usize,
    n_z_vars_raw: usize,
    n_z_vars_effective: usize,
    excess: usize,
    preferred_pin_scalars: &'a HashSet<String>,
    diff_derivative_assignment_targets: &'a HashSet<String>,
}

fn trace_pin_candidates(meta: &PinTraceMeta<'_>, candidates: &[PinScalarCandidate]) {
    if !sim_trace_enabled() {
        return;
    }
    eprintln!(
        "[sim-trace] pin_orphaned_variables: n_x={} n_z_slots={} n_z_vars_raw={} n_z_vars_effective={} excess={} preferred={:?} diff_derivative_targets={:?}",
        meta.n_x,
        meta.n_z_slots,
        meta.n_z_vars_raw,
        meta.n_z_vars_effective,
        meta.excess,
        meta.preferred_pin_scalars,
        meta.diff_derivative_assignment_targets
    );
    for (rank, candidate) in candidates.iter().take(12).enumerate() {
        eprintln!(
            "[sim-trace] pin candidate rank={} name={} preferred={} substitution_linked={} direct={} refs={} size={} start={:?}",
            rank,
            candidate.scalar_name,
            candidate.is_preferred,
            candidate.is_substitution_linked,
            candidate.has_direct_assignment,
            candidate.refs,
            candidate.size,
            candidate.start_expr
        );
    }
}

fn append_pin_equations(
    dae_model: &mut dae::Dae,
    candidates: &[PinScalarCandidate],
    excess: usize,
) {
    for candidate in candidates.iter().take(excess) {
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] pin selected name={} size={} idx={} start={:?}",
                candidate.scalar_name, candidate.size, candidate.scalar_idx, candidate.start_expr
            );
        }
        dae_model.f_x.push(build_pin_equation(
            &candidate.name,
            candidate.size,
            candidate.scalar_idx,
            candidate.start_expr.clone(),
        ));
    }
}

pub fn pin_orphaned_variables(
    dae_model: &mut dae::Dae,
    elim: &rumoca_phase_structural::eliminate::EliminationResult,
) {
    let n_x: usize = dae_model.states.values().map(|v| v.size()).sum();
    let n_z_slots = dae_model.f_x.len().saturating_sub(n_x);

    let n_z_vars_raw: usize = dae_model
        .algebraics
        .values()
        .map(|v| v.size())
        .sum::<usize>()
        + dae_model.outputs.values().map(|v| v.size()).sum::<usize>();

    if n_z_vars_raw <= n_z_slots {
        return;
    }
    let raw_excess = n_z_vars_raw - n_z_slots;

    let diff_derivative_assignment_targets =
        collect_diff_row_derivative_assignment_targets(dae_model, n_x);

    let mut preferred_pin_scalars: HashSet<String> = HashSet::new();
    extend_preferred_pin_scalars_from_singular_ic(dae_model, n_x, &mut preferred_pin_scalars);
    if preferred_pin_scalars.is_empty() {
        maybe_collect_structural_relaxation_preferences(
            dae_model,
            n_x,
            n_z_vars_raw,
            n_z_slots,
            &mut preferred_pin_scalars,
        );
    }

    let scalarization =
        rumoca_phase_structural::scalarize::build_expression_scalarization_context(dae_model);
    let (direct_assignment_targets, direct_assignment_scalar_starts) =
        collect_direct_assignment_targets_and_starts(dae_model, &scalarization);
    let state_alias_members = collect_state_alias_members(dae_model, n_x);
    let substitution_linked_names = collect_substitution_linked_names(elim);
    let runtime_defined_unknowns = collect_runtime_defined_unknowns(dae_model);
    let ref_counts = collect_equation_ref_counts(dae_model);
    let algebraic_ref_counts = collect_algebraic_equation_ref_counts(dae_model, n_x);

    let pin_inputs = PinScalarInputs {
        direct_assignment_targets: &direct_assignment_targets,
        direct_assignment_scalar_starts: &direct_assignment_scalar_starts,
        diff_derivative_assignment_targets: &diff_derivative_assignment_targets,
        algebraic_ref_counts: &algebraic_ref_counts,
        preferred_pin_scalars: &preferred_pin_scalars,
        state_alias_members: &state_alias_members,
        substitution_linked_names: &substitution_linked_names,
        runtime_defined_unknowns: &runtime_defined_unknowns,
        ref_counts: &ref_counts,
        scalarization: &scalarization,
    };
    let mut all_vars = build_pin_scalar_candidates(dae_model, &pin_inputs);
    let n_z_vars_effective = all_vars.len();
    let excess_by_count = n_z_vars_effective.saturating_sub(n_z_slots);
    let preferred_available = all_vars
        .iter()
        .filter(|candidate| candidate.is_preferred)
        .count();
    let preferred_fallback_available = all_vars
        .iter()
        .filter(|candidate| {
            candidate.is_preferred && should_force_preferred_pin_fallback(candidate)
        })
        .count();
    let excess = if excess_by_count > 0 {
        raw_excess.min(all_vars.len())
    } else {
        preferred_fallback_available.min(raw_excess)
    };
    if excess == 0 {
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] pin_orphaned_variables: skipping pinning after exclusion/preference resolution; slots={} raw_vars={} effective_vars={} raw_excess={} preferred_available={} preferred_fallback_available={}",
                n_z_slots,
                n_z_vars_raw,
                n_z_vars_effective,
                raw_excess,
                preferred_available,
                preferred_fallback_available
            );
        }
        return;
    }
    sort_pin_candidates(&mut all_vars);
    let trace_meta = PinTraceMeta {
        n_x,
        n_z_slots,
        n_z_vars_raw,
        n_z_vars_effective,
        excess,
        preferred_pin_scalars: &preferred_pin_scalars,
        diff_derivative_assignment_targets: &diff_derivative_assignment_targets,
    };
    trace_pin_candidates(&trace_meta, &all_vars);
    append_pin_equations(dae_model, &all_vars, excess);
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_phase_structural::StructuralError;
    use rumoca_phase_structural::eliminate::{EliminationResult, Substitution};

    fn residual(rhs: dae::Expression, origin: &str) -> dae::Equation {
        dae::Equation {
            lhs: None,
            rhs,
            span: Span::DUMMY,
            origin: origin.to_string(),
            scalar_count: 1,
        }
    }

    fn lit(value: f64) -> dae::Expression {
        dae::Expression::Literal(dae::Literal::Real(value))
    }

    fn var_ref(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: vec![],
        }
    }

    fn indexed_var_ref(name: &str, idx: i64) -> dae::Expression {
        dae::Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: vec![dae::Subscript::Index(idx)],
        }
    }

    #[test]
    fn pin_sort_prioritizes_structural_preferences_before_direct_assignment_tie_breaker() {
        let mut candidates = vec![
            PinScalarCandidate {
                name: dae::VarName::new("direct"),
                scalar_name: "direct".to_string(),
                scalar_idx: 0,
                base: "direct".to_string(),
                size: 1,
                start_expr: lit(0.0),
                is_preferred: true,
                is_substitution_linked: false,
                refs: 1,
                has_direct_assignment: true,
            },
            PinScalarCandidate {
                name: dae::VarName::new("free_preferred"),
                scalar_name: "free_preferred".to_string(),
                scalar_idx: 0,
                base: "free_preferred".to_string(),
                size: 1,
                start_expr: lit(0.0),
                is_preferred: true,
                is_substitution_linked: false,
                refs: 0,
                has_direct_assignment: false,
            },
            PinScalarCandidate {
                name: dae::VarName::new("free_unpreferred"),
                scalar_name: "free_unpreferred".to_string(),
                scalar_idx: 0,
                base: "free_unpreferred".to_string(),
                size: 1,
                start_expr: lit(0.0),
                is_preferred: false,
                is_substitution_linked: false,
                refs: 0,
                has_direct_assignment: false,
            },
        ];

        sort_pin_candidates(&mut candidates);

        assert_eq!(candidates[0].scalar_name, "free_preferred");
        assert_eq!(candidates[1].scalar_name, "direct");
        assert_eq!(candidates[2].scalar_name, "free_unpreferred");
    }

    #[test]
    fn orphan_pins_preserve_scalarized_start_expressions() {
        let mut dae_model = dae::Dae::default();

        let mut p_start = dae::Variable::new(dae::VarName::new("p_start"));
        p_start.dims = vec![3];
        p_start.start = Some(dae::Expression::Array {
            elements: vec![lit(1.0), lit(2.0), lit(3.0)],
            is_matrix: false,
        });
        dae_model
            .parameters
            .insert(dae::VarName::new("p_start"), p_start);

        let mut p = dae::Variable::new(dae::VarName::new("p"));
        p.dims = vec![3];
        p.start = Some(var_ref("p_start"));
        dae_model.algebraics.insert(dae::VarName::new("p"), p);

        let elim = EliminationResult {
            substitutions: vec![],
            n_eliminated: 0,
        };

        pin_orphaned_variables(&mut dae_model, &elim);

        let pins: Vec<_> = dae_model
            .f_x
            .iter()
            .filter(|eq| eq.origin == "orphaned_variable_pin")
            .collect();
        assert_eq!(pins.len(), 3);

        let dae::Expression::Binary { lhs, rhs, .. } = &pins[2].rhs else {
            panic!("expected binary pin equation");
        };
        assert!(
            matches!(
                lhs.as_ref(),
                dae::Expression::VarRef {
                    name,
                    subscripts
                } if name.as_str() == "p"
                    && matches!(subscripts.as_slice(), [dae::Subscript::Index(3)])
            ),
            "expected third pin to target p[3], got {lhs:?}"
        );
        assert!(
            matches!(
                rhs.as_ref(),
                dae::Expression::VarRef {
                    name,
                    subscripts
                } if name.as_str() == "p_start"
                    && matches!(subscripts.as_slice(), [dae::Subscript::Index(3)])
            ),
            "expected third pin to preserve p_start[3], got {rhs:?}"
        );
    }

    #[test]
    fn direct_assignment_pin_starts_use_scalarized_solution() {
        let mut dae_model = dae::Dae::default();

        let mut source = dae::Variable::new(dae::VarName::new("source"));
        source.dims = vec![3];
        dae_model
            .algebraics
            .insert(dae::VarName::new("source"), source);

        let mut y = dae::Variable::new(dae::VarName::new("y"));
        y.dims = vec![3];
        dae_model.outputs.insert(dae::VarName::new("y"), y);
        dae_model.f_x.push(residual(
            dae::Expression::Binary {
                op: OpBinary::Sub(Default::default()),
                lhs: Box::new(var_ref("y")),
                rhs: Box::new(var_ref("source")),
            },
            "direct vector assignment",
        ));

        let scalarization =
            rumoca_phase_structural::scalarize::build_expression_scalarization_context(&dae_model);
        let (targets, starts) =
            collect_direct_assignment_targets_and_starts(&dae_model, &scalarization);

        assert!(targets.contains("y"));
        assert!(
            matches!(
                starts.get("y[3]"),
                Some(dae::Expression::VarRef { name, subscripts })
                    if name.as_str() == "source"
                        && matches!(subscripts.as_slice(), [dae::Subscript::Index(3)])
            ),
            "expected y[3] pin start to use source[3], got {:?}",
            starts.get("y[3]")
        );
    }

    #[test]
    fn computed_direct_assignment_pin_starts_are_not_reused() {
        let mut dae_model = dae::Dae::default();

        dae_model.algebraics.insert(
            dae::VarName::new("source"),
            dae::Variable::new(dae::VarName::new("source")),
        );

        let mut y = dae::Variable::new(dae::VarName::new("y"));
        y.dims = vec![1];
        y.start = Some(lit(4.0));
        dae_model.outputs.insert(dae::VarName::new("y"), y);
        dae_model.f_x.push(residual(
            dae::Expression::Binary {
                op: OpBinary::Sub(Default::default()),
                lhs: Box::new(indexed_var_ref("y", 1)),
                rhs: Box::new(dae::Expression::Binary {
                    op: OpBinary::Add(Default::default()),
                    lhs: Box::new(var_ref("source")),
                    rhs: Box::new(lit(1.0)),
                }),
            },
            "computed direct assignment",
        ));

        let scalarization =
            rumoca_phase_structural::scalarize::build_expression_scalarization_context(&dae_model);
        let (targets, starts) =
            collect_direct_assignment_targets_and_starts(&dae_model, &scalarization);

        assert!(targets.contains("y"));
        assert!(
            !starts.contains_key("y[1]"),
            "computed direct assignment should not become a pin start: {:?}",
            starts.get("y[1]")
        );
    }

    #[test]
    fn preferred_diff_derivative_targets_remain_pin_candidates() {
        let mut dae_model = dae::Dae::default();

        let mut v_w = dae::Variable::new(dae::VarName::new("v_w"));
        v_w.dims = vec![2];
        v_w.start = Some(dae::Expression::Array {
            elements: vec![lit(1.0), lit(2.0)],
            is_matrix: false,
        });
        dae_model.algebraics.insert(dae::VarName::new("v_w"), v_w);

        let scalarization =
            rumoca_phase_structural::scalarize::build_expression_scalarization_context(&dae_model);
        let mut diff_derivative_assignment_targets = HashSet::new();
        diff_derivative_assignment_targets.insert("v_w".to_string());
        let mut algebraic_ref_counts = HashMap::new();
        algebraic_ref_counts.insert("v_w".to_string(), 1);
        let mut preferred_pin_scalars = HashSet::new();
        preferred_pin_scalars.insert("v_w[2]".to_string());
        let empty_names = HashSet::new();
        let empty_starts = HashMap::new();
        let empty_counts = HashMap::new();

        let inputs = PinScalarInputs {
            direct_assignment_targets: &empty_names,
            direct_assignment_scalar_starts: &empty_starts,
            diff_derivative_assignment_targets: &diff_derivative_assignment_targets,
            algebraic_ref_counts: &algebraic_ref_counts,
            preferred_pin_scalars: &preferred_pin_scalars,
            state_alias_members: &empty_names,
            substitution_linked_names: &empty_names,
            runtime_defined_unknowns: &empty_names,
            ref_counts: &empty_counts,
            scalarization: &scalarization,
        };
        let candidates = build_pin_scalar_candidates(&dae_model, &inputs);

        assert!(
            candidates
                .iter()
                .any(|candidate| candidate.scalar_name == "v_w[2]")
        );
        assert!(
            candidates
                .iter()
                .all(|candidate| candidate.scalar_name != "v_w[1]")
        );
    }

    #[test]
    fn pin_orphaned_variables_does_not_pin_direct_assignment_targets() {
        let mut dae_model = dae::Dae::default();
        dae_model.outputs.insert(
            dae::VarName::new("y"),
            dae::Variable::new(dae::VarName::new("y")),
        );
        dae_model.algebraics.insert(
            dae::VarName::new("z"),
            dae::Variable::new(dae::VarName::new("z")),
        );
        dae_model.f_x.push(residual(
            dae::Expression::Binary {
                op: OpBinary::Sub(Default::default()),
                lhs: Box::new(var_ref("y")),
                rhs: Box::new(lit(1.0)),
            },
            "direct assignment",
        ));

        let elim = EliminationResult {
            substitutions: vec![],
            n_eliminated: 0,
        };

        pin_orphaned_variables(&mut dae_model, &elim);

        let pins: Vec<_> = dae_model
            .f_x
            .iter()
            .filter(|eq| eq.origin == "orphaned_variable_pin")
            .collect();
        assert_eq!(pins.len(), 1);
        let dae::Expression::Binary { lhs, .. } = &pins[0].rhs else {
            panic!("expected binary pin equation");
        };
        assert!(
            matches!(
                lhs.as_ref(),
                dae::Expression::VarRef { name, .. } if name.as_str() == "z"
            ),
            "expected only free algebraic z to be pinned, got {lhs:?}"
        );
    }

    #[test]
    fn pin_orphaned_variables_skips_substitution_linked_preferred_fallback_helper() {
        let mut dae_model = dae::Dae::default();

        let mut expr = dae::Variable::new(dae::VarName::new("multiSwitch.expr"));
        expr.dims = vec![2];
        expr.start = Some(dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Fill,
            args: vec![
                dae::Expression::Literal(dae::Literal::Real(0.0)),
                dae::Expression::Literal(dae::Literal::Integer(2)),
            ],
        });
        dae_model
            .algebraics
            .insert(dae::VarName::new("multiSwitch.expr"), expr);

        dae_model.outputs.insert(
            dae::VarName::new("multiSwitch.y"),
            dae::Variable::new(dae::VarName::new("multiSwitch.y")),
        );
        dae_model.discrete_valued.insert(
            dae::VarName::new("flag"),
            dae::Variable::new(dae::VarName::new("flag")),
        );
        dae_model.discrete_valued.insert(
            dae::VarName::new("multiSwitch.firstActiveIndex"),
            dae::Variable::new(dae::VarName::new("multiSwitch.firstActiveIndex")),
        );

        // MLS Appendix B runtime-defined output: keep this excluded from the
        // orphan-pin candidate set so only the unmatched helper remains.
        dae_model.f_x.push(residual(
            dae::Expression::Binary {
                op: OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("multiSwitch.y"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Pre,
                    args: vec![dae::Expression::VarRef {
                        name: dae::VarName::new("flag"),
                        subscripts: vec![],
                    }],
                }),
            },
            "runtime-defined output",
        ));
        dae_model.f_x.push(residual(
            dae::Expression::Literal(dae::Literal::Real(0.0)),
            "constant slack",
        ));

        match rumoca_phase_structural::build_ic_plan(&dae_model, 0) {
            Err(StructuralError::Singular {
                unmatched_unknowns, ..
            }) => {
                assert!(
                    unmatched_unknowns
                        .iter()
                        .any(|name| name == "multiSwitch.expr[1]")
                );
                assert!(
                    unmatched_unknowns
                        .iter()
                        .any(|name| name == "multiSwitch.expr[2]")
                );
            }
            other => panic!("expected singular IC plan, got {other:?}"),
        }

        let elim = EliminationResult {
            substitutions: vec![Substitution {
                var_name: dae::VarName::new("multiSwitch.y"),
                expr: dae::Expression::If {
                    branches: vec![(
                        dae::Expression::Binary {
                            op: OpBinary::Eq(Default::default()),
                            lhs: Box::new(dae::Expression::VarRef {
                                name: dae::VarName::new("multiSwitch.firstActiveIndex"),
                                subscripts: vec![],
                            }),
                            rhs: Box::new(dae::Expression::Literal(dae::Literal::Integer(0))),
                        },
                        dae::Expression::Literal(dae::Literal::Real(2.0)),
                    )],
                    else_branch: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("multiSwitch.expr"),
                        subscripts: vec![dae::Subscript::Expr(Box::new(dae::Expression::VarRef {
                            name: dae::VarName::new("multiSwitch.firstActiveIndex"),
                            subscripts: vec![],
                        }))],
                    }),
                },
                env_keys: vec!["multiSwitch.y".to_string()],
            }],
            n_eliminated: 1,
        };

        pin_orphaned_variables(&mut dae_model, &elim);

        assert!(
            dae_model
                .f_x
                .iter()
                .all(|eq| eq.origin != "orphaned_variable_pin"),
            "substitution-linked unmatched helpers should not be force-pinned when effective slots already suffice"
        );
    }
}
