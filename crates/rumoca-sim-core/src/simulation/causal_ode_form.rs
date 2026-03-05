use rumoca_ir_dae as dae;

use crate::simulation::mass_matrix_form::symbolic_affine_derivative_form;

#[derive(Debug, Clone, thiserror::Error)]
pub enum CausalOdeFormError {
    #[error(
        "causal ODE backend requires scalarized states: found {n_state_variables} state variables for {n_state_scalars} state scalars"
    )]
    RequiresScalarizedStates {
        n_state_variables: usize,
        n_state_scalars: usize,
    },

    #[error(
        "causal ODE backend requires one differential row per state scalar: found {n_equations} equations for {n_state_scalars} states"
    )]
    NonCausalEquationCount {
        n_equations: usize,
        n_state_scalars: usize,
    },

    #[error("causal ODE row {row_idx} (origin='{origin}') is invalid: {reason}")]
    InvalidRow {
        row_idx: usize,
        origin: String,
        reason: String,
    },
}

#[derive(Debug, Clone)]
pub struct CausalOdeForm {
    pub state_rhs: Vec<dae::Expression>,
    pub state_names: Vec<String>,
}

fn zero_literal(expr: &dae::Expression) -> bool {
    if let dae::Expression::Literal(dae::Literal::Real(value)) = expr {
        return value.abs() <= 1.0e-15;
    }
    false
}

fn one_literal(expr: &dae::Expression) -> bool {
    if let dae::Expression::Literal(dae::Literal::Real(value)) = expr {
        return (*value - 1.0).abs() <= 1.0e-15;
    }
    false
}

fn explicit_rhs_expr(remainder: dae::Expression, coeff: dae::Expression) -> dae::Expression {
    let negated = dae::Expression::Unary {
        op: dae::OpUnary::Minus(Default::default()),
        rhs: Box::new(remainder),
    };
    if one_literal(&coeff) {
        return negated;
    }
    dae::Expression::Binary {
        op: dae::OpBinary::Div(Default::default()),
        lhs: Box::new(negated),
        rhs: Box::new(coeff),
    }
}

fn invalid_row(
    dae_model: &dae::Dae,
    row_idx: usize,
    reason: impl Into<String>,
) -> CausalOdeFormError {
    let origin = dae_model
        .f_x
        .get(row_idx)
        .map(|eq| eq.origin.clone())
        .unwrap_or_default();
    CausalOdeFormError::InvalidRow {
        row_idx,
        origin,
        reason: reason.into(),
    }
}

pub fn extract_causal_ode_form(
    dae_model: &dae::Dae,
    n_x: usize,
) -> Result<CausalOdeForm, CausalOdeFormError> {
    let state_names: Vec<dae::VarName> = dae_model.states.keys().cloned().collect();
    if state_names.len() != n_x {
        return Err(CausalOdeFormError::RequiresScalarizedStates {
            n_state_variables: state_names.len(),
            n_state_scalars: n_x,
        });
    }
    if dae_model.f_x.len() != n_x {
        return Err(CausalOdeFormError::NonCausalEquationCount {
            n_equations: dae_model.f_x.len(),
            n_state_scalars: n_x,
        });
    }

    let mut state_rhs = Vec::with_capacity(n_x);
    for row_idx in 0..n_x {
        let row = &dae_model.f_x[row_idx];
        let row_form = symbolic_affine_derivative_form(&row.rhs, state_names.as_slice())
            .map_err(|reason| invalid_row(dae_model, row_idx, reason))?;
        if row_form.coeffs.is_empty() {
            return Err(invalid_row(
                dae_model,
                row_idx,
                "row has no der(state) term after symbolic decomposition",
            ));
        }

        if row_form.coeffs.len() != 1 || !row_form.coeffs.contains_key(&row_idx) {
            let coupled_states: Vec<String> = row_form
                .coeffs
                .keys()
                .map(|idx| {
                    state_names
                        .get(*idx)
                        .map_or_else(|| format!("col#{idx}"), |name| name.as_str().to_string())
                })
                .collect();
            return Err(invalid_row(
                dae_model,
                row_idx,
                format!(
                    "row must reference exactly der({}) only; found derivative columns {:?}",
                    state_names
                        .get(row_idx)
                        .map_or("<unknown>", |name| name.as_str()),
                    coupled_states
                ),
            ));
        }

        let coeff = row_form.coeffs.get(&row_idx).cloned().ok_or_else(|| {
            invalid_row(
                dae_model,
                row_idx,
                "missing diagonal derivative coefficient",
            )
        })?;
        if zero_literal(&coeff) {
            return Err(invalid_row(
                dae_model,
                row_idx,
                "diagonal derivative coefficient is identically zero",
            ));
        }

        state_rhs.push(explicit_rhs_expr(row_form.remainder, coeff));
    }

    let state_names = state_names
        .iter()
        .map(|name| name.as_str().to_string())
        .collect();
    Ok(CausalOdeForm {
        state_rhs,
        state_names,
    })
}

#[cfg(test)]
mod tests {
    use super::{CausalOdeFormError, extract_causal_ode_form};
    use rumoca_eval_flat::eval::{build_env, eval_expr};
    use rumoca_ir_dae as dae;

    fn scalar_state(name: &str) -> dae::Variable {
        dae::Variable::new(dae::VarName::new(name))
    }

    fn var(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: vec![],
        }
    }

    fn der(name: &str) -> dae::Expression {
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var(name)],
        }
    }

    fn sub(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
        dae::Expression::Binary {
            op: dae::OpBinary::Sub(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    fn add(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
        dae::Expression::Binary {
            op: dae::OpBinary::Add(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    fn lit(value: f64) -> dae::Expression {
        dae::Expression::Literal(dae::Literal::Real(value))
    }

    fn eq(rhs: dae::Expression, origin: &str) -> dae::Equation {
        dae::Equation {
            lhs: None,
            rhs,
            span: rumoca_core::Span::DUMMY,
            scalar_count: 1,
            origin: origin.to_string(),
        }
    }

    #[test]
    fn extract_causal_ode_form_accepts_simple_derivative_row() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .states
            .insert(dae::VarName::new("x"), scalar_state("x"));
        dae_model.f_x.push(eq(sub(der("x"), lit(2.0)), "ode_x"));

        let form = extract_causal_ode_form(&dae_model, 1).expect("causal form");
        assert_eq!(form.state_names, vec!["x".to_string()]);
        assert_eq!(form.state_rhs.len(), 1);

        let env = build_env(&dae_model, &[0.0], &[], 0.0);
        let rhs_value = eval_expr::<f64>(&form.state_rhs[0], &env);
        assert!((rhs_value - 2.0).abs() <= 1.0e-12);
    }

    #[test]
    fn extract_causal_ode_form_rejects_equation_count_mismatch() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .states
            .insert(dae::VarName::new("x"), scalar_state("x"));

        let err = extract_causal_ode_form(&dae_model, 1).expect_err("must reject mismatch");
        assert!(matches!(
            err,
            CausalOdeFormError::NonCausalEquationCount {
                n_equations: 0,
                n_state_scalars: 1
            }
        ));
    }

    #[test]
    fn extract_causal_ode_form_rejects_cross_derivative_coupling() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .states
            .insert(dae::VarName::new("x"), scalar_state("x"));
        dae_model
            .states
            .insert(dae::VarName::new("y"), scalar_state("y"));
        dae_model
            .f_x
            .push(eq(sub(add(der("x"), der("y")), lit(1.0)), "ode_xy"));
        dae_model.f_x.push(eq(sub(der("y"), lit(2.0)), "ode_y"));

        let err = extract_causal_ode_form(&dae_model, 2).expect_err("must reject coupling");
        assert!(matches!(
            err,
            CausalOdeFormError::InvalidRow { row_idx: 0, .. }
        ));
    }
}
