//! Solve preparation phase.
//!
//! This phase transforms canonical DAE IR (`rumoca-ir-dae`) into symbolic
//! prepared solve IR (`rumoca-ir-solve`). It performs no runtime execution.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

/// Requested solve form to materialize in `SolveIr`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SolveFormRequest {
    /// Baseline form: symbolic implicit residual system.
    #[default]
    ImplicitResidual,
    /// Optional symbolic mass-matrix form.
    MassMatrix,
    /// Optional symbolic causal form.
    Causal,
}

/// Options for solve-phase lowering.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct SolvePhaseOptions {
    pub form: SolveFormRequest,
}

/// Solve-phase transform error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolvePhaseError {
    FormNotAvailable {
        form: SolveFormRequest,
        reason: String,
    },
}

impl std::fmt::Display for SolvePhaseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FormNotAvailable { form, reason } => {
                write!(f, "requested form is not available ({form:?}): {reason}")
            }
        }
    }
}

impl std::error::Error for SolvePhaseError {}

/// Check whether a requested form is derivable for the provided DAE.
///
/// Returns `Ok(())` if form construction should succeed, otherwise returns
/// `FormNotAvailable` with an explicit reason.
pub fn ensure_form_available(
    dae_model: &dae::Dae,
    form: SolveFormRequest,
) -> Result<(), SolvePhaseError> {
    match form {
        SolveFormRequest::ImplicitResidual => Ok(()),
        SolveFormRequest::MassMatrix => build_symbolic_mass_matrix_form(dae_model)
            .map(|_| ())
            .map_err(|reason| SolvePhaseError::FormNotAvailable { form, reason }),
        SolveFormRequest::Causal => build_symbolic_causal_form(dae_model)
            .map(|_| ())
            .map_err(|reason| SolvePhaseError::FormNotAvailable { form, reason }),
    }
}

/// Transform canonical DAE IR into symbolic Solve IR.
pub fn to_solve_ir(
    dae_model: &dae::Dae,
    options: SolvePhaseOptions,
) -> Result<solve::SolveIr, SolvePhaseError> {
    let mut ir = solve::SolveIr::new();

    insert_symbols(&mut ir, &dae_model.states, solve::SolveVariableKind::State);
    insert_symbols(
        &mut ir,
        &dae_model.algebraics,
        solve::SolveVariableKind::Algebraic,
    );
    insert_symbols(
        &mut ir,
        &dae_model.parameters,
        solve::SolveVariableKind::Parameter,
    );
    // Constants are treated as symbolic parameter-class channels in Solve IR.
    insert_symbols(
        &mut ir,
        &dae_model.constants,
        solve::SolveVariableKind::Parameter,
    );
    insert_symbols(&mut ir, &dae_model.inputs, solve::SolveVariableKind::Input);
    insert_symbols(
        &mut ir,
        &dae_model.outputs,
        solve::SolveVariableKind::Output,
    );
    insert_symbols(
        &mut ir,
        &dae_model.discrete_reals,
        solve::SolveVariableKind::DiscreteReal,
    );
    insert_symbols(
        &mut ir,
        &dae_model.discrete_valued,
        solve::SolveVariableKind::DiscreteValued,
    );

    ir.implicit_residual.residuals = dae_model
        .f_x
        .iter()
        .map(|eq| solve::ResidualEquation {
            residual: eq.rhs.clone(),
            origin: eq.origin.clone(),
        })
        .collect();

    ir.implicit_residual.initial_residuals = dae_model
        .initial_equations
        .iter()
        .map(|eq| solve::ResidualEquation {
            residual: eq.rhs.clone(),
            origin: eq.origin.clone(),
        })
        .collect();

    ir.implicit_residual.event_relations = dae_model.relation.clone();
    ir.implicit_residual
        .event_relations
        .extend(dae_model.synthetic_root_conditions.iter().cloned());

    ir.implicit_residual.propagation = dae_model
        .f_z
        .iter()
        .chain(dae_model.f_m.iter())
        .filter_map(|eq| {
            eq.lhs.as_ref().map(|lhs| solve::PropagationAssignment {
                target: lhs.clone(),
                expr: eq.rhs.clone(),
                origin: eq.origin.clone(),
            })
        })
        .collect();

    match options.form {
        SolveFormRequest::ImplicitResidual => {}
        SolveFormRequest::MassMatrix => {
            ir.mass_matrix = Some(build_symbolic_mass_matrix_form(dae_model).map_err(
                |reason| SolvePhaseError::FormNotAvailable {
                    form: SolveFormRequest::MassMatrix,
                    reason,
                },
            )?);
        }
        SolveFormRequest::Causal => {
            ir.causal = Some(build_symbolic_causal_form(dae_model).map_err(|reason| {
                SolvePhaseError::FormNotAvailable {
                    form: SolveFormRequest::Causal,
                    reason,
                }
            })?);
        }
    }

    Ok(ir)
}

fn insert_symbols(
    ir: &mut solve::SolveIr,
    vars: &indexmap::IndexMap<dae::VarName, dae::Variable>,
    kind: solve::SolveVariableKind,
) {
    for (name, var) in vars {
        ir.symbols
            .entry(name.clone())
            .or_insert_with(|| solve::SolveVariable {
                kind,
                start: var.start.clone(),
                nominal: var.nominal.clone(),
                unit: var.unit.clone(),
            });
    }
}

fn build_symbolic_mass_matrix_form(dae_model: &dae::Dae) -> Result<solve::MassMatrixForm, String> {
    let state_order: Vec<dae::VarName> = dae_model.states.keys().cloned().collect();
    if state_order.is_empty() {
        return Err("model has no states".to_string());
    }
    let rhs = extract_state_derivative_rhs_rows(dae_model, &state_order)?;
    let n = state_order.len();
    let mut mass_matrix = Vec::with_capacity(n);
    for i in 0..n {
        let mut row = Vec::with_capacity(n);
        for j in 0..n {
            row.push(dae::Expression::Literal(dae::Literal::Real(if i == j {
                1.0
            } else {
                0.0
            })));
        }
        mass_matrix.push(row);
    }

    Ok(solve::MassMatrixForm {
        state_order,
        mass_matrix,
        rhs,
    })
}

fn build_symbolic_causal_form(dae_model: &dae::Dae) -> Result<solve::CausalForm, String> {
    let state_order: Vec<dae::VarName> = dae_model.states.keys().cloned().collect();
    if state_order.is_empty() {
        return Err("model has no states".to_string());
    }
    let derivatives = extract_state_derivative_rhs_rows(dae_model, &state_order)?;
    Ok(solve::CausalForm {
        state_order,
        derivatives,
        algebraic_assignments: Vec::new(),
    })
}

fn extract_state_derivative_rhs_rows(
    dae_model: &dae::Dae,
    state_order: &[dae::VarName],
) -> Result<Vec<dae::Expression>, String> {
    let mut out = Vec::with_capacity(state_order.len());
    for state in state_order {
        let rhs = dae_model
            .f_x
            .iter()
            .find_map(|eq| extract_derivative_rhs_for_state(&eq.rhs, state))
            .ok_or_else(|| {
                format!(
                    "no derivable row found for state '{}' (expected der({}) +/- rhs = 0 pattern)",
                    state.as_str(),
                    state.as_str()
                )
            })?;
        out.push(rhs);
    }
    Ok(out)
}

fn extract_derivative_rhs_for_state(
    expr: &dae::Expression,
    state: &dae::VarName,
) -> Option<dae::Expression> {
    let dae::Expression::Binary { op, lhs, rhs } = expr else {
        return None;
    };

    if !matches!(op, dae::OpBinary::Sub(_) | dae::OpBinary::SubElem(_)) {
        return None;
    }

    if is_der_of_state(lhs, state) && !contains_any_derivative(rhs) {
        return Some((**rhs).clone());
    }
    if is_der_of_state(rhs, state) && !contains_any_derivative(lhs) {
        return Some(negate((**lhs).clone()));
    }
    None
}

fn is_der_of_state(expr: &dae::Expression, state: &dae::VarName) -> bool {
    let dae::Expression::BuiltinCall { function, args } = expr else {
        return false;
    };
    if *function != dae::BuiltinFunction::Der || args.len() != 1 {
        return false;
    }
    match args.first() {
        Some(dae::Expression::VarRef { name, subscripts }) => {
            name == state && subscripts.is_empty()
        }
        _ => false,
    }
}

fn contains_any_derivative(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            if *function == dae::BuiltinFunction::Der {
                return true;
            }
            args.iter().any(contains_any_derivative)
        }
        dae::Expression::FunctionCall { args, .. } => args.iter().any(contains_any_derivative),
        dae::Expression::Binary { lhs, rhs, .. } => {
            contains_any_derivative(lhs) || contains_any_derivative(rhs)
        }
        dae::Expression::Unary { rhs, .. } => contains_any_derivative(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                contains_any_derivative(cond) || contains_any_derivative(value)
            }) || contains_any_derivative(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(contains_any_derivative)
        }
        dae::Expression::Range { start, step, end } => {
            contains_any_derivative(start)
                || step.as_deref().is_some_and(contains_any_derivative)
                || contains_any_derivative(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            contains_any_derivative(expr)
                || indices
                    .iter()
                    .any(|idx| contains_any_derivative(&idx.range))
                || filter.as_deref().is_some_and(contains_any_derivative)
        }
        dae::Expression::Index { base, subscripts } => {
            contains_any_derivative(base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(expr) => contains_any_derivative(expr),
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => contains_any_derivative(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn negate(expr: dae::Expression) -> dae::Expression {
    dae::Expression::Unary {
        op: dae::OpUnary::Minus(Default::default()),
        rhs: Box::new(expr),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;

    fn lit(v: f64) -> dae::Expression {
        dae::Expression::Literal(dae::Literal::Real(v))
    }

    fn der(name: &str) -> dae::Expression {
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![dae::Expression::VarRef {
                name: dae::VarName::new(name),
                subscripts: vec![],
            }],
        }
    }

    #[test]
    fn to_solve_ir_builds_implicit_residual_baseline() {
        let mut d = dae::Dae::new();
        let mut x = dae::Variable::new(dae::VarName::new("x"));
        x.start = Some(lit(0.0));
        d.states.insert(dae::VarName::new("x"), x);
        d.parameters.insert(
            dae::VarName::new("p"),
            dae::Variable::new(dae::VarName::new("p")),
        );
        d.relation.push(lit(3.0));
        d.synthetic_root_conditions.push(lit(4.0));
        d.f_x
            .push(dae::Equation::residual(lit(1.0), Span::DUMMY, "fx row"));
        d.initial_equations
            .push(dae::Equation::residual(lit(2.0), Span::DUMMY, "init row"));
        d.f_z.push(dae::Equation::explicit(
            dae::VarName::new("z"),
            lit(5.0),
            Span::DUMMY,
            "fz row",
        ));

        let ir = to_solve_ir(&d, SolvePhaseOptions::default()).expect("implicit solve-ir");
        assert_eq!(ir.ir_version, solve::SOLVE_IR_VERSION);
        assert_eq!(ir.implicit_residual.residuals.len(), 1);
        assert_eq!(ir.implicit_residual.initial_residuals.len(), 1);
        assert_eq!(ir.implicit_residual.event_relations.len(), 2);
        assert_eq!(ir.implicit_residual.propagation.len(), 1);
        assert!(ir.mass_matrix.is_none());
        assert!(ir.causal.is_none());

        let x_sym = ir
            .symbols
            .get(&dae::VarName::new("x"))
            .expect("state symbol");
        assert_eq!(x_sym.kind, solve::SolveVariableKind::State);
        let p_sym = ir
            .symbols
            .get(&dae::VarName::new("p"))
            .expect("parameter symbol");
        assert_eq!(p_sym.kind, solve::SolveVariableKind::Parameter);
    }

    #[test]
    fn to_solve_ir_builds_requested_mass_matrix_when_derivable() {
        let mut d = dae::Dae::new();
        d.states.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        d.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: dae::OpBinary::Sub(Default::default()),
                lhs: Box::new(der("x")),
                rhs: Box::new(lit(2.0)),
            },
            Span::DUMMY,
            "ode_x",
        ));

        let ir = to_solve_ir(
            &d,
            SolvePhaseOptions {
                form: SolveFormRequest::MassMatrix,
            },
        )
        .expect("mass-matrix should be derivable");
        let mm = ir.mass_matrix.expect("mass-matrix form");
        assert_eq!(mm.state_order.len(), 1);
        assert_eq!(mm.mass_matrix.len(), 1);
        assert_eq!(mm.rhs.len(), 1);
    }

    #[test]
    fn to_solve_ir_builds_requested_causal_when_derivable() {
        let mut d = dae::Dae::new();
        d.states.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        d.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: dae::OpBinary::Sub(Default::default()),
                lhs: Box::new(der("x")),
                rhs: Box::new(lit(3.0)),
            },
            Span::DUMMY,
            "ode_x",
        ));

        let ir = to_solve_ir(
            &d,
            SolvePhaseOptions {
                form: SolveFormRequest::Causal,
            },
        )
        .expect("causal form should be derivable");
        let causal = ir.causal.expect("causal form");
        assert_eq!(causal.state_order.len(), 1);
        assert_eq!(causal.derivatives.len(), 1);
    }

    #[test]
    fn to_solve_ir_returns_form_not_available_when_not_derivable() {
        let mut d = dae::Dae::new();
        d.states.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        d.f_x
            .push(dae::Equation::residual(lit(1.0), Span::DUMMY, "no_der"));

        let err = to_solve_ir(
            &d,
            SolvePhaseOptions {
                form: SolveFormRequest::Causal,
            },
        )
        .expect_err("causal should fail when no derivable der(state) row exists");
        assert!(matches!(
            err,
            SolvePhaseError::FormNotAvailable {
                form: SolveFormRequest::Causal,
                ..
            }
        ));
    }

    #[test]
    fn ensure_form_available_reports_derivability() {
        let mut d = dae::Dae::new();
        d.states.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        d.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: dae::OpBinary::Sub(Default::default()),
                lhs: Box::new(der("x")),
                rhs: Box::new(lit(1.0)),
            },
            Span::DUMMY,
            "ode_x",
        ));
        ensure_form_available(&d, SolveFormRequest::MassMatrix)
            .expect("mass-matrix should be available");

        let empty = dae::Dae::new();
        let err = ensure_form_available(&empty, SolveFormRequest::Causal)
            .expect_err("causal should be unavailable without states");
        assert!(matches!(
            err,
            SolvePhaseError::FormNotAvailable {
                form: SolveFormRequest::Causal,
                ..
            }
        ));
    }
}
