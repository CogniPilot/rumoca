use std::collections::{HashMap, HashSet};

use rumoca_core::Span;
use rumoca_eval_dae::runtime::{VarEnv, build_env, eval_expr};
use rumoca_ir_core::{OpBinary, OpUnary};
use rumoca_ir_dae as dae;

use crate::runtime::timeout::{TimeoutBudget, TimeoutExceeded};
use crate::simulation::dae_prepare::expr_contains_der_of;
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

fn coeff_expr_for_derivative(
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
            if let dae::Expression::VarRef { name, subscripts } = &args[0]
                && name == state_name
                && subscripts.is_empty()
            {
                return Ok(Some(real_expr(1.0)));
            }
            Ok(None)
        }
        dae::Expression::Unary {
            op: OpUnary::Minus(_),
            rhs,
        } => {
            let inner = coeff_expr_for_derivative(rhs, state_name)?;
            Ok(inner.map(unary_minus_expr))
        }
        dae::Expression::Binary { op, lhs, rhs } => match op {
            OpBinary::Add(_) => {
                let lhs_coeff = coeff_expr_for_derivative(lhs, state_name)?;
                let rhs_coeff = coeff_expr_for_derivative(rhs, state_name)?;
                Ok(combine_add_sub_coeffs(lhs_coeff, rhs_coeff, false))
            }
            OpBinary::Sub(_) => {
                let lhs_coeff = coeff_expr_for_derivative(lhs, state_name)?;
                let rhs_coeff = coeff_expr_for_derivative(rhs, state_name)?;
                Ok(combine_add_sub_coeffs(lhs_coeff, rhs_coeff, true))
            }
            OpBinary::Mul(_) => {
                let lhs_has_der = expr_contains_der_of(lhs, state_name);
                let rhs_has_der = expr_contains_der_of(rhs, state_name);
                if lhs_has_der && rhs_has_der {
                    return Err("nonlinear derivative term in multiplication".to_string());
                }
                if lhs_has_der {
                    let lhs_coeff = coeff_expr_for_derivative(lhs, state_name)?;
                    let Some(coeff) = lhs_coeff else {
                        return Err("unable to extract derivative coefficient from lhs".to_string());
                    };
                    return Ok(Some(mul_expr(coeff, rhs.as_ref().clone())));
                }
                if rhs_has_der {
                    let rhs_coeff = coeff_expr_for_derivative(rhs, state_name)?;
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
                let lhs_coeff = coeff_expr_for_derivative(lhs, state_name)?;
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

fn build_mass_matrix_env(
    dae_model: &dae::Dae,
    state_names: &[dae::VarName],
    param_values: &[f64],
    budget: &TimeoutBudget,
    trace: bool,
) -> Result<VarEnv<f64>, MassMatrixBuildError> {
    let env_t0 = trace_timer_start_if(trace);
    if trace {
        eprintln!("[sim-trace] mass_matrix compute start: build_env");
    }
    let mut env_zero = build_env(
        dae_model,
        &vec![0.0; dae_model.f_x.len()],
        param_values,
        0.0,
    );
    for state_name in state_names {
        let key = format!("der({})", state_name.as_str());
        env_zero.set(&key, 0.0);
    }
    budget.check()?;
    if trace {
        eprintln!(
            "[sim-trace] mass_matrix compute done: build_env elapsed={:.3}s",
            trace_timer_elapsed_seconds(env_t0)
        );
    }
    Ok(env_zero)
}

fn fill_mass_matrix_row(
    dae_model: &dae::Dae,
    row: usize,
    row_coeffs: &mut [f64],
    state_names: &[dae::VarName],
    env_zero: &VarEnv<f64>,
) -> Result<(), MassMatrixBuildError> {
    let Some(eq) = dae_model.f_x.get(row) else {
        return Err(row_non_derivable_error(
            dae_model,
            state_names,
            row,
            "state row is missing from f_x",
        ));
    };

    let mut row_has_der = false;
    for (col, state_name) in state_names.iter().enumerate().take(row_coeffs.len()) {
        if !expr_contains_der_of(&eq.rhs, state_name) {
            continue;
        }
        row_has_der = true;
        let coeff_expr = coeff_expr_for_derivative(&eq.rhs, state_name)
            .map_err(|reason| state_non_derivable_error(eq, row, state_name, reason))?;
        let Some(coeff_expr) = coeff_expr else {
            return Err(state_non_derivable_error(
                eq,
                row,
                state_name,
                "unable to isolate derivative coefficient",
            ));
        };
        let coeff_val = eval_expr::<f64>(&coeff_expr, env_zero);
        if !coeff_val.is_finite() {
            return Err(state_non_derivable_error(
                eq,
                row,
                state_name,
                "derivative coefficient evaluates to a non-finite value",
            ));
        }
        if coeff_val.abs() > MASS_MATRIX_EPS {
            row_coeffs[col] = coeff_val;
        }
    }

    if !row_has_der {
        return Err(row_non_derivable_error(
            dae_model,
            state_names,
            row,
            "equation row does not contain any der(state) term",
        ));
    }
    if row_coeffs.iter().all(|c| c.abs() <= MASS_MATRIX_EPS) {
        return Err(row_non_derivable_error(
            dae_model,
            state_names,
            row,
            "all derivative coefficients evaluate to approximately zero",
        ));
    }

    Ok(())
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
    let state_names: Vec<dae::VarName> = dae_model.states.keys().cloned().collect();
    let mut mass_matrix = vec![vec![0.0; n_x]; n_x];
    let env_zero = build_mass_matrix_env(dae_model, &state_names, param_values, budget, trace)?;

    for (row, row_coeffs) in mass_matrix.iter_mut().enumerate().take(n_x) {
        if row.is_multiple_of(16) {
            budget.check()?;
        }
        if trace && row > 0 && row.is_multiple_of(50) {
            eprintln!("[sim-trace] mass_matrix compute progress: row={row}/{n_x}");
        }
        fill_mass_matrix_row(dae_model, row, row_coeffs, &state_names, &env_zero)?;
    }

    if trace {
        eprintln!(
            "[sim-trace] mass_matrix compute done: rows processed={}",
            n_x
        );
    }
    Ok(mass_matrix)
}

#[inline]
fn trace_timer_start_if(trace: bool) -> Option<std::time::Instant> {
    if !trace {
        return None;
    }
    #[cfg(target_arch = "wasm32")]
    {
        None
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        Some(std::time::Instant::now())
    }
}

#[inline]
fn trace_timer_elapsed_seconds(start: Option<std::time::Instant>) -> f64 {
    start.map_or(0.0, |t0| t0.elapsed().as_secs_f64())
}

fn build_pin_equation(name: &dae::VarName, sz: usize, idx: usize, start_val: f64) -> dae::Equation {
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
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(start_val))),
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
    start: Option<dae::Expression>,
    is_preferred: bool,
    is_substitution_linked: bool,
    refs: usize,
    has_direct_assignment: bool,
}

struct PinScalarInputs<'a> {
    direct_assignment_targets: &'a HashSet<String>,
    diff_derivative_assignment_targets: &'a HashSet<String>,
    algebraic_ref_counts: &'a HashMap<String, usize>,
    preferred_pin_scalars: &'a HashSet<String>,
    substitution_linked_names: &'a HashSet<String>,
    runtime_defined_unknowns: &'a HashSet<String>,
    ref_counts: &'a HashMap<String, usize>,
}

fn add_substitution_link_name(set: &mut HashSet<String>, name: &str) {
    set.insert(name.to_string());
    if let Some(base) = dae::component_base_name(name) {
        set.insert(base);
    }
}

fn collect_substitution_linked_names(
    elim: &rumoca_phase_solve::eliminate::EliminationResult,
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

fn direct_assignment_target_bases(expr: &dae::Expression, out: &mut HashSet<String>) {
    match expr {
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
            lhs,
            rhs,
        } => {
            if let dae::Expression::VarRef { name, .. } = lhs.as_ref()
                && let Some(base) = component_base_name(name.as_str())
            {
                out.insert(base);
            }
            if let dae::Expression::VarRef { name, .. } = rhs.as_ref()
                && let Some(base) = component_base_name(name.as_str())
            {
                out.insert(base);
            }
        }
        dae::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_),
            rhs,
        } => direct_assignment_target_bases(rhs, out),
        _ => {}
    }
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
        if inputs.diff_derivative_assignment_targets.contains(&base)
            && (is_output || algebraic_refs > 0)
        {
            continue;
        }
        let refs = inputs.ref_counts.get(&base).copied().unwrap_or(0);
        let has_direct_assignment = inputs.direct_assignment_targets.contains(&base);
        let size = v.size();

        if size <= 1 {
            let scalar_name = n.as_str().to_string();
            let is_preferred = inputs.preferred_pin_scalars.contains(&scalar_name);
            let is_substitution_linked = inputs.substitution_linked_names.contains(&scalar_name)
                || inputs.substitution_linked_names.contains(&base);
            all_vars.push(PinScalarCandidate {
                name: n.clone(),
                scalar_name,
                scalar_idx: 0,
                base: base.clone(),
                size,
                start: v.start.clone(),
                is_preferred,
                is_substitution_linked,
                refs,
                has_direct_assignment,
            });
            continue;
        }

        for scalar_idx in 0..size {
            let scalar_name = format!("{}[{}]", n.as_str(), scalar_idx + 1);
            let is_preferred = inputs.preferred_pin_scalars.contains(&scalar_name);
            let is_substitution_linked = inputs.substitution_linked_names.contains(&scalar_name)
                || inputs.substitution_linked_names.contains(&base);
            all_vars.push(PinScalarCandidate {
                name: n.clone(),
                scalar_name,
                scalar_idx,
                base: base.clone(),
                size,
                start: v.start.clone(),
                is_preferred,
                is_substitution_linked,
                refs,
                has_direct_assignment,
            });
        }
    }
    all_vars
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
    let Some(hint) = rumoca_phase_solve::build_ic_relaxation_hint(dae_model, n_x) else {
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

fn collect_direct_assignment_targets(dae_model: &dae::Dae) -> HashSet<String> {
    let mut direct_assignment_targets = HashSet::new();
    for eq in &dae_model.f_x {
        direct_assignment_target_bases(&eq.rhs, &mut direct_assignment_targets);
    }
    direct_assignment_targets
}

fn collect_runtime_defined_unknowns(dae_model: &dae::Dae) -> HashSet<String> {
    let mut runtime_defined = HashSet::new();
    for name in rumoca_eval_dae::analysis::runtime_defined_continuous_unknown_names(dae_model) {
        runtime_defined.insert(name.clone());
        if let Some(base) = component_base_name(&name) {
            runtime_defined.insert(base);
        }
    }
    runtime_defined
}

fn extend_preferred_pin_scalars_from_singular_ic(
    dae_model: &dae::Dae,
    n_x: usize,
    preferred_pin_scalars: &mut HashSet<String>,
) {
    if let Err(rumoca_phase_solve::StructuralError::Singular {
        unmatched_unknowns, ..
    }) = rumoca_phase_solve::build_ic_plan(dae_model, n_x)
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
            .then(a.is_substitution_linked.cmp(&b.is_substitution_linked))
            .then((a.refs == 0).cmp(&(b.refs == 0)))
            .then(a.has_direct_assignment.cmp(&b.has_direct_assignment))
            .then(a.refs.cmp(&b.refs))
            .then(a.base.cmp(&b.base))
            .then(a.scalar_name.cmp(&b.scalar_name))
            .then(a.scalar_idx.cmp(&b.scalar_idx))
    });
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
            "[sim-trace] pin candidate rank={} name={} preferred={} substitution_linked={} direct={} refs={} size={}",
            rank,
            candidate.scalar_name,
            candidate.is_preferred,
            candidate.is_substitution_linked,
            candidate.has_direct_assignment,
            candidate.refs,
            candidate.size
        );
    }
}

fn append_pin_equations(
    dae_model: &mut dae::Dae,
    candidates: &[PinScalarCandidate],
    excess: usize,
) {
    for candidate in candidates.iter().take(excess) {
        let start_val = candidate
            .start
            .as_ref()
            .map(rumoca_eval_dae::runtime::eval_const_expr)
            .unwrap_or(0.0);
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] pin selected name={} size={} idx={} start={:?}",
                candidate.scalar_name, candidate.size, candidate.scalar_idx, candidate.start
            );
        }
        dae_model.f_x.push(build_pin_equation(
            &candidate.name,
            candidate.size,
            candidate.scalar_idx,
            start_val,
        ));
    }
}

pub fn pin_orphaned_variables(
    dae_model: &mut dae::Dae,
    elim: &rumoca_phase_solve::eliminate::EliminationResult,
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

    let direct_assignment_targets = collect_direct_assignment_targets(dae_model);
    let substitution_linked_names = collect_substitution_linked_names(elim);
    let runtime_defined_unknowns = collect_runtime_defined_unknowns(dae_model);
    let ref_counts = collect_equation_ref_counts(dae_model);
    let algebraic_ref_counts = collect_algebraic_equation_ref_counts(dae_model, n_x);

    let pin_inputs = PinScalarInputs {
        direct_assignment_targets: &direct_assignment_targets,
        diff_derivative_assignment_targets: &diff_derivative_assignment_targets,
        algebraic_ref_counts: &algebraic_ref_counts,
        preferred_pin_scalars: &preferred_pin_scalars,
        substitution_linked_names: &substitution_linked_names,
        runtime_defined_unknowns: &runtime_defined_unknowns,
        ref_counts: &ref_counts,
    };
    let mut all_vars = build_pin_scalar_candidates(dae_model, &pin_inputs);
    let n_z_vars_effective = all_vars.len();
    let excess_by_count = n_z_vars_effective.saturating_sub(n_z_slots);
    let preferred_available = all_vars
        .iter()
        .filter(|candidate| candidate.is_preferred)
        .count();
    let excess = if excess_by_count > 0 {
        excess_by_count
    } else {
        preferred_available.min(raw_excess)
    };
    if excess == 0 {
        if sim_trace_enabled() {
            eprintln!(
                "[sim-trace] pin_orphaned_variables: skipping pinning after exclusion/preference resolution; slots={} raw_vars={} effective_vars={} raw_excess={} preferred_available={}",
                n_z_slots, n_z_vars_raw, n_z_vars_effective, raw_excess, preferred_available
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
