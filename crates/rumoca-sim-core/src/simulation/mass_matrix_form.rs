//! Symbolic validation of DAE rows for fixed (runtime-invariant) mass-matrix use.

use rumoca_ir_dae as dae;

use crate::runtime::timeout::{TimeoutBudget, TimeoutExceeded};
use crate::simulation::dae_prepare::{expr_contains_der_of, expr_refers_to_var};
use crate::simulation::pipeline::MassMatrix;

#[derive(Debug, Clone, thiserror::Error)]
/// Errors emitted while validating constant mass-matrix eligibility.
pub enum MassMatrixFormError {
    /// Timeout while traversing/validating the expression graph.
    #[error(transparent)]
    Timeout(#[from] TimeoutExceeded),
    /// Validation failure with row/context-specific reason text.
    #[error("{0}")]
    Invalid(String),
}

fn collect_active_derivative_indices(
    rhs: &dae::Expression,
    state_names: &[dae::VarName],
) -> Vec<usize> {
    state_names
        .iter()
        .enumerate()
        .filter_map(|(state_idx, state_name)| {
            expr_contains_der_of(rhs, state_name).then_some(state_idx)
        })
        .collect()
}

#[derive(Clone)]
pub(crate) struct AffineDerivativeForm {
    pub(crate) remainder: dae::Expression,
    pub(crate) coeffs: std::collections::HashMap<usize, dae::Expression>,
}

fn zero_expr() -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Real(0.0))
}

fn one_expr() -> dae::Expression {
    dae::Expression::Literal(dae::Literal::Real(1.0))
}

fn add_expr(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Binary {
        op: dae::OpBinary::Add(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn sub_expr(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Binary {
        op: dae::OpBinary::Sub(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn mul_expr(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Binary {
        op: dae::OpBinary::Mul(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn div_expr(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Binary {
        op: dae::OpBinary::Div(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn neg_expr(rhs: dae::Expression) -> dae::Expression {
    dae::Expression::Unary {
        op: dae::OpUnary::Minus(Default::default()),
        rhs: Box::new(rhs),
    }
}

fn derivative_free_form(expr: &dae::Expression) -> AffineDerivativeForm {
    AffineDerivativeForm {
        remainder: expr.clone(),
        coeffs: std::collections::HashMap::new(),
    }
}

fn derivative_symbol_form(state_idx: usize) -> AffineDerivativeForm {
    let mut coeffs = std::collections::HashMap::new();
    coeffs.insert(state_idx, one_expr());
    AffineDerivativeForm {
        remainder: zero_expr(),
        coeffs,
    }
}

fn add_coeff_map_entry(
    out: &mut std::collections::HashMap<usize, dae::Expression>,
    idx: usize,
    expr: dae::Expression,
) {
    if let Some(existing) = out.remove(&idx) {
        out.insert(idx, add_expr(existing, expr));
    } else {
        out.insert(idx, expr);
    }
}

fn subtract_coeff_map_entry(
    out: &mut std::collections::HashMap<usize, dae::Expression>,
    idx: usize,
    expr: dae::Expression,
) {
    if let Some(existing) = out.remove(&idx) {
        out.insert(idx, sub_expr(existing, expr));
    } else {
        out.insert(idx, neg_expr(expr));
    }
}

fn add_coeff_maps(
    lhs: std::collections::HashMap<usize, dae::Expression>,
    rhs: std::collections::HashMap<usize, dae::Expression>,
) -> std::collections::HashMap<usize, dae::Expression> {
    let mut out = lhs;
    for (idx, expr) in rhs {
        add_coeff_map_entry(&mut out, idx, expr);
    }
    out
}

fn subtract_coeff_maps(
    lhs: std::collections::HashMap<usize, dae::Expression>,
    rhs: std::collections::HashMap<usize, dae::Expression>,
) -> std::collections::HashMap<usize, dae::Expression> {
    let mut out = lhs;
    for (idx, expr) in rhs {
        subtract_coeff_map_entry(&mut out, idx, expr);
    }
    out
}

fn scale_coeff_map_mul(
    coeffs: std::collections::HashMap<usize, dae::Expression>,
    factor: &dae::Expression,
) -> std::collections::HashMap<usize, dae::Expression> {
    coeffs
        .into_iter()
        .map(|(idx, coeff)| (idx, mul_expr(coeff, factor.clone())))
        .collect()
}

fn scale_coeff_map_div(
    coeffs: std::collections::HashMap<usize, dae::Expression>,
    factor: &dae::Expression,
) -> std::collections::HashMap<usize, dae::Expression> {
    coeffs
        .into_iter()
        .map(|(idx, coeff)| (idx, div_expr(coeff, factor.clone())))
        .collect()
}

fn negate_coeff_map(
    coeffs: std::collections::HashMap<usize, dae::Expression>,
) -> std::collections::HashMap<usize, dae::Expression> {
    coeffs
        .into_iter()
        .map(|(idx, coeff)| (idx, neg_expr(coeff)))
        .collect()
}

pub(crate) fn expr_contains_any_der_call(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            if *function == dae::BuiltinFunction::Der {
                return true;
            }
            args.iter().any(expr_contains_any_der_call)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_any_der_call(lhs) || expr_contains_any_der_call(rhs)
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_any_der_call(rhs),
        dae::Expression::FunctionCall { args, .. } => args.iter().any(expr_contains_any_der_call),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, val)| {
                expr_contains_any_der_call(cond) || expr_contains_any_der_call(val)
            }) || expr_contains_any_der_call(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expr_contains_any_der_call)
        }
        dae::Expression::Range { start, step, end } => {
            expr_contains_any_der_call(start)
                || step.as_deref().is_some_and(expr_contains_any_der_call)
                || expr_contains_any_der_call(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_any_der_call(expr)
                || indices
                    .iter()
                    .any(|idx| expr_contains_any_der_call(&idx.range))
                || filter.as_deref().is_some_and(expr_contains_any_der_call)
        }
        dae::Expression::Index { base, subscripts } => {
            expr_contains_any_der_call(base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(sub_expr) => expr_contains_any_der_call(sub_expr),
                    _ => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => expr_contains_any_der_call(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn derivative_state_index(
    der_arg: &dae::Expression,
    state_names: &[dae::VarName],
) -> Option<usize> {
    let mut matched_idx = None;
    for (idx, state_name) in state_names.iter().enumerate() {
        if !expr_refers_to_var(der_arg, state_name) {
            continue;
        }
        if matched_idx.is_some() {
            return None;
        }
        matched_idx = Some(idx);
    }
    matched_idx
}

fn var_ref_is_parameter_or_constant(dae: &dae::Dae, var_name: &dae::VarName) -> bool {
    if dae.parameters.contains_key(var_name) || dae.constants.contains_key(var_name) {
        return true;
    }
    let Some(base_name) = dae::component_base_name(var_name.as_str()) else {
        return false;
    };
    let base_var = dae::VarName::new(base_name);
    dae.parameters.contains_key(&base_var) || dae.constants.contains_key(&base_var)
}

fn coefficient_is_runtime_invariant(dae: &dae::Dae, expr: &dae::Expression) -> bool {
    if expr_contains_any_der_call(expr) {
        return false;
    }
    let mut refs = std::collections::HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.iter()
        .all(|name| var_ref_is_parameter_or_constant(dae, name))
}

fn combine_if_branch_coefficients(
    branches: &[(dae::Expression, AffineDerivativeForm)],
    else_form: &AffineDerivativeForm,
) -> std::collections::HashMap<usize, dae::Expression> {
    let mut active_coeff_indices = std::collections::HashSet::new();
    for (_, branch_form) in branches {
        active_coeff_indices.extend(branch_form.coeffs.keys().copied());
    }
    active_coeff_indices.extend(else_form.coeffs.keys().copied());

    let mut coeffs = std::collections::HashMap::new();
    for idx in active_coeff_indices {
        let branch_coeffs = branches
            .iter()
            .map(|(cond, branch_form)| {
                (
                    cond.clone(),
                    branch_form
                        .coeffs
                        .get(&idx)
                        .cloned()
                        .unwrap_or_else(zero_expr),
                )
            })
            .collect();
        let else_coeff = else_form
            .coeffs
            .get(&idx)
            .cloned()
            .unwrap_or_else(zero_expr);
        coeffs.insert(
            idx,
            dae::Expression::If {
                branches: branch_coeffs,
                else_branch: Box::new(else_coeff),
            },
        );
    }
    coeffs
}

fn symbolic_affine_builtin_call(
    function: dae::BuiltinFunction,
    args: &[dae::Expression],
    state_names: &[dae::VarName],
    expr: &dae::Expression,
) -> Result<AffineDerivativeForm, String> {
    if function == dae::BuiltinFunction::Der {
        if args.len() != 1 {
            return Err("der(...) call does not have exactly one argument".to_string());
        }
        let Some(state_idx) = derivative_state_index(&args[0], state_names) else {
            return Err("der(...) does not reference a known state variable".to_string());
        };
        return Ok(derivative_symbol_form(state_idx));
    }

    if args.iter().any(expr_contains_any_der_call) {
        return Err(format!(
            "built-in function {:?} wraps derivative-dependent terms",
            function
        ));
    }
    Ok(derivative_free_form(expr))
}

fn symbolic_affine_unary(
    op: &dae::OpUnary,
    rhs: &dae::Expression,
    state_names: &[dae::VarName],
    expr: &dae::Expression,
) -> Result<AffineDerivativeForm, String> {
    let rhs_form = symbolic_affine_derivative_form(rhs, state_names)?;
    match op {
        dae::OpUnary::Minus(_) | dae::OpUnary::DotMinus(_) => Ok(AffineDerivativeForm {
            remainder: neg_expr(rhs_form.remainder),
            coeffs: negate_coeff_map(rhs_form.coeffs),
        }),
        dae::OpUnary::Plus(_) | dae::OpUnary::DotPlus(_) => Ok(rhs_form),
        _ => {
            if rhs_form.coeffs.is_empty() {
                Ok(derivative_free_form(expr))
            } else {
                Err("unsupported unary operator on derivative-dependent term".to_string())
            }
        }
    }
}

fn symbolic_affine_binary(
    op: &dae::OpBinary,
    lhs: &dae::Expression,
    rhs: &dae::Expression,
    state_names: &[dae::VarName],
    expr: &dae::Expression,
) -> Result<AffineDerivativeForm, String> {
    let lhs_form = symbolic_affine_derivative_form(lhs, state_names)?;
    let rhs_form = symbolic_affine_derivative_form(rhs, state_names)?;
    let AffineDerivativeForm {
        remainder: lhs_remainder,
        coeffs: lhs_coeffs,
    } = lhs_form;
    let AffineDerivativeForm {
        remainder: rhs_remainder,
        coeffs: rhs_coeffs,
    } = rhs_form;
    let lhs_has_der = !lhs_coeffs.is_empty();
    let rhs_has_der = !rhs_coeffs.is_empty();

    match op {
        dae::OpBinary::Add(_) | dae::OpBinary::AddElem(_) => Ok(AffineDerivativeForm {
            remainder: add_expr(lhs_remainder, rhs_remainder),
            coeffs: add_coeff_maps(lhs_coeffs, rhs_coeffs),
        }),
        dae::OpBinary::Sub(_) | dae::OpBinary::SubElem(_) => Ok(AffineDerivativeForm {
            remainder: sub_expr(lhs_remainder, rhs_remainder),
            coeffs: subtract_coeff_maps(lhs_coeffs, rhs_coeffs),
        }),
        dae::OpBinary::Mul(_) | dae::OpBinary::MulElem(_) => {
            if lhs_has_der && rhs_has_der {
                return Err("product of derivative-dependent terms is non-affine".to_string());
            }
            if lhs_has_der {
                return Ok(AffineDerivativeForm {
                    remainder: mul_expr(lhs_remainder, rhs_remainder.clone()),
                    coeffs: scale_coeff_map_mul(lhs_coeffs, &rhs_remainder),
                });
            }
            if rhs_has_der {
                return Ok(AffineDerivativeForm {
                    remainder: mul_expr(lhs_remainder.clone(), rhs_remainder),
                    coeffs: scale_coeff_map_mul(rhs_coeffs, &lhs_remainder),
                });
            }
            Ok(AffineDerivativeForm {
                remainder: mul_expr(lhs_remainder, rhs_remainder),
                coeffs: std::collections::HashMap::new(),
            })
        }
        dae::OpBinary::Div(_) | dae::OpBinary::DivElem(_) => {
            if rhs_has_der {
                return Err(
                    "derivative-dependent denominator is not affine mass-matrix form".to_string(),
                );
            }
            Ok(AffineDerivativeForm {
                remainder: div_expr(lhs_remainder, rhs_remainder.clone()),
                coeffs: scale_coeff_map_div(lhs_coeffs, &rhs_remainder),
            })
        }
        _ => {
            if lhs_has_der || rhs_has_der {
                Err(format!(
                    "operator '{}' does not preserve affine derivative form",
                    op
                ))
            } else {
                Ok(derivative_free_form(expr))
            }
        }
    }
}

fn symbolic_affine_if(
    branches: &[(dae::Expression, dae::Expression)],
    else_branch: &dae::Expression,
    state_names: &[dae::VarName],
) -> Result<AffineDerivativeForm, String> {
    let mut affine_branches = Vec::with_capacity(branches.len());
    let mut remainder_branches = Vec::with_capacity(branches.len());
    for (cond, branch_val) in branches {
        let cond_form = symbolic_affine_derivative_form(cond, state_names)?;
        if !cond_form.coeffs.is_empty() {
            return Err("if-condition depends on derivative terms".to_string());
        }
        let branch_form = symbolic_affine_derivative_form(branch_val, state_names)?;
        let cond_remainder = cond_form.remainder;
        remainder_branches.push((cond_remainder.clone(), branch_form.remainder.clone()));
        affine_branches.push((cond_remainder, branch_form));
    }
    let else_form = symbolic_affine_derivative_form(else_branch, state_names)?;
    let coeffs = combine_if_branch_coefficients(&affine_branches, &else_form);
    Ok(AffineDerivativeForm {
        remainder: dae::Expression::If {
            branches: remainder_branches,
            else_branch: Box::new(else_form.remainder),
        },
        coeffs,
    })
}

fn symbolic_affine_index(
    base: &dae::Expression,
    subscripts: &[dae::Subscript],
    state_names: &[dae::VarName],
    expr: &dae::Expression,
) -> Result<AffineDerivativeForm, String> {
    let base_form = symbolic_affine_derivative_form(base, state_names)?;
    if subscripts.iter().any(
        |sub| matches!(sub, dae::Subscript::Expr(sub_expr) if expr_contains_any_der_call(sub_expr)),
    ) {
        return Err("index subscripts contain derivative terms".to_string());
    }
    if !base_form.coeffs.is_empty() {
        return Err("indexed derivative-dependent expressions are unsupported".to_string());
    }
    Ok(derivative_free_form(expr))
}

fn symbolic_affine_field_access(
    base: &dae::Expression,
    state_names: &[dae::VarName],
    expr: &dae::Expression,
) -> Result<AffineDerivativeForm, String> {
    let base_form = symbolic_affine_derivative_form(base, state_names)?;
    if !base_form.coeffs.is_empty() {
        return Err("field access on derivative-dependent expressions is unsupported".to_string());
    }
    Ok(derivative_free_form(expr))
}

pub(crate) fn symbolic_affine_derivative_form(
    expr: &dae::Expression,
    state_names: &[dae::VarName],
) -> Result<AffineDerivativeForm, String> {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            symbolic_affine_builtin_call(*function, args, state_names, expr)
        }
        dae::Expression::FunctionCall { args, .. } => {
            if args.iter().any(expr_contains_any_der_call) {
                return Err("user function call wraps derivative-dependent terms".to_string());
            }
            Ok(derivative_free_form(expr))
        }
        dae::Expression::Unary { op, rhs } => symbolic_affine_unary(op, rhs, state_names, expr),
        dae::Expression::Binary { op, lhs, rhs } => {
            symbolic_affine_binary(op, lhs, rhs, state_names, expr)
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => symbolic_affine_if(branches, else_branch, state_names),
        dae::Expression::Index { base, subscripts } => {
            symbolic_affine_index(base, subscripts, state_names, expr)
        }
        dae::Expression::FieldAccess { base, .. } => {
            symbolic_affine_field_access(base, state_names, expr)
        }
        dae::Expression::Array { .. }
        | dae::Expression::Tuple { .. }
        | dae::Expression::Range { .. }
        | dae::Expression::ArrayComprehension { .. } => {
            if expr_contains_any_der_call(expr) {
                return Err("non-scalar derivative-dependent expression is unsupported".to_string());
            }
            Ok(derivative_free_form(expr))
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            Ok(derivative_free_form(expr))
        }
    }
}

/// Validate that differential rows are representable in fixed mass-matrix form.
///
/// Accepted rows are affine in `der(state_i)` with coefficients that only depend
/// on parameters/constants (runtime-invariant for solver setup).
pub fn validate_constant_mass_matrix_form(
    dae: &dae::Dae,
    n_x: usize,
    mass_matrix: &MassMatrix,
    budget: &TimeoutBudget,
) -> Result<(), MassMatrixFormError> {
    if n_x == 0 {
        return Ok(());
    }

    let n_rows = n_x.min(dae.f_x.len());
    let state_names: Vec<dae::VarName> = dae.states.keys().cloned().collect();

    for row_idx in 0..n_rows {
        if row_idx.is_multiple_of(16) {
            budget.check()?;
        }
        let eq = &dae.f_x[row_idx];
        let row_origin = eq.origin.as_str();
        let active_der_indices = collect_active_derivative_indices(&eq.rhs, &state_names);
        if active_der_indices.is_empty() {
            return Err(MassMatrixFormError::Invalid(format!(
                "at differential row {row_idx} (origin='{row_origin}'): row has no der(state) terms"
            )));
        }
        let Some(row_coeffs) = mass_matrix.get(row_idx) else {
            return Err(MassMatrixFormError::Invalid(format!(
                "at differential row {row_idx} (origin='{row_origin}'): missing mass-matrix row"
            )));
        };
        let row_form = symbolic_affine_derivative_form(&eq.rhs, &state_names);
        let active_state_names: Vec<&str> = active_der_indices
            .iter()
            .filter_map(|idx| state_names.get(*idx))
            .map(|name| name.as_str())
            .collect();
        let row_form = row_form.map_err(|reason| {
            MassMatrixFormError::Invalid(format!(
                "at differential row {row_idx} (origin='{row_origin}'): symbolic affine check failed: {reason} (active_derivatives={active_state_names:?})"
            ))
        })?;
        if row_form.coeffs.is_empty() {
            return Err(MassMatrixFormError::Invalid(format!(
                "at differential row {row_idx} (origin='{row_origin}'): row has no effective affine der(state) coefficients after symbolic decomposition (active_derivatives={active_state_names:?})"
            )));
        }
        if let Some((col_idx, _)) = row_form
            .coeffs
            .iter()
            .find(|(_, coeff)| !coefficient_is_runtime_invariant(dae, coeff))
        {
            return Err(MassMatrixFormError::Invalid(format!(
                "at differential row {row_idx} (origin='{row_origin}'): symbolic coefficient for derivative column {col_idx} depends on runtime variables; coefficients must depend only on parameters/constants"
            )));
        }
        let symbolic_coeff_indices: std::collections::HashSet<usize> =
            row_form.coeffs.keys().copied().collect();
        let unexpected_numeric_cols: Vec<usize> = row_coeffs
            .iter()
            .enumerate()
            .filter_map(|(col_idx, coeff)| {
                (coeff.abs() > 1.0e-15 && !symbolic_coeff_indices.contains(&col_idx))
                    .then_some(col_idx)
            })
            .collect();
        if !unexpected_numeric_cols.is_empty() {
            return Err(MassMatrixFormError::Invalid(format!(
                "at differential row {row_idx} (origin='{row_origin}'): computed mass-matrix row has coefficients at columns {unexpected_numeric_cols:?} that are not symbolically present as der(state) terms"
            )));
        }
    }
    budget.check()?;
    Ok(())
}
