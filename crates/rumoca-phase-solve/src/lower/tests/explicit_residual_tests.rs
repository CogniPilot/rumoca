use super::*;

#[test]
fn lower_residual_lowers_explicit_algebraic_equation_as_lhs_minus_rhs() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model.continuous.equations.push(dae::Equation::explicit(
        rumoca_core::VarName::new("y"),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(2.0),
            span: lower_test_span(),
        },
        rumoca_core::Span::DUMMY,
        "y = 2",
    ));

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_residual(&dae_model, &layout).expect("explicit residual should lower");
    let (_, output_at_zero) = eval_linear_ops(&rows[0], &[0.0], &[], 0.0);
    let (_, output_at_solution) = eval_linear_ops(&rows[0], &[2.0], &[], 0.0);

    // MLS §8.3: explicit equations remain equations. The solver residual must
    // be y - 2, so algebraic projection has a y-column Jacobian to relax.
    assert_eq!(output_at_zero, Some(-2.0));
    assert_eq!(output_at_solution, Some(0.0));
}
