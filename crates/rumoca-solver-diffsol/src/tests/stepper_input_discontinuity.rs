use super::*;

/// Test that the stepper survives a discontinuous input change.
///
/// Models the drone-startup scenario: a state `x` driven by input `u`
/// (der(x) = u).  We step for ~1 s with u=0, building up BDF history
/// that assumes a quiescent system, then suddenly set u=100 — a step
/// discontinuity.  Without the solver-history reset the BDF polynomial
/// extrapolation diverges and the step fails.
#[test]
fn test_stepper_survives_discontinuous_input_step() {
    use crate::stepper::{SimStepper, StepperOptions};

    let mut dae = Dae::new();

    // State: x (starts at 0)
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));

    // Input: u (externally driven)
    dae.inputs
        .insert(VarName::new("u"), Variable::new(VarName::new("u")));

    // Equation: 0 = der(x) - u
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            var_ref("u"),
        ),
        span: Span::DUMMY,
        origin: "ode_x".to_string(),
        scalar_count: 1,
    });

    let opts = StepperOptions {
        max_wall_seconds_per_step: Some(5.0),
        ..StepperOptions::default()
    };
    let mut stepper = SimStepper::new(&dae, opts).expect("stepper creation should succeed");

    assert!(
        stepper.input_names().contains(&"u".to_string()),
        "expected 'u' in input names: {:?}",
        stepper.input_names()
    );

    // Phase 1: step with u=0 for ~1 s to build up BDF history.
    let dt = 0.01;
    for _ in 0..100 {
        stepper
            .step(dt)
            .expect("step with zero input should succeed");
    }
    let t_before = stepper.time();
    assert!(
        (t_before - 1.0).abs() < 0.01,
        "expected t≈1.0, got {t_before}"
    );
    let x_before = stepper.get("x").expect("should read x");
    assert!(
        x_before.abs() < 1e-6,
        "x should be ~0 with zero input, got {x_before}"
    );

    // Phase 2: sudden input change — this is the discontinuity that
    // would crash the old solver.
    stepper
        .set_input("u", 100.0)
        .expect("set_input should work");

    // Step for another 0.5 s.
    for i in 0..50 {
        stepper
            .step(dt)
            .unwrap_or_else(|e| panic!("step {i} after input change failed: {e}"));
    }
    let t_after = stepper.time();
    assert!(
        (t_after - 1.5).abs() < 0.01,
        "expected t≈1.5, got {t_after}"
    );

    // x should have integrated u=100 for ~0.5 s → x ≈ 50
    let x_after = stepper.get("x").expect("should read x");
    assert!((x_after - 50.0).abs() < 1.0, "expected x≈50, got {x_after}");
}

/// Stiffer variant: a nonlinear system `der(x) = -1000*(x^3 - u)` that
/// exercises BDF at high order with a large step.  When `u` jumps, the stiff
/// Jacobian coupling makes the extrapolated predictor wildly wrong without a
/// history reset.
#[test]
fn test_stepper_stiff_nonlinear_input_discontinuity() {
    use crate::stepper::{SimStepper, StepperOptions};

    let mut dae = Dae::new();

    // State x, input u
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.inputs
        .insert(VarName::new("u"), Variable::new(VarName::new("u")));

    // 0 = der(x) - (-1000*(x*x*x - u))
    //   = der(x) + 1000*(x^3 - u)
    // Residual form: der(x) + 1000*x^3 - 1000*u
    let derx = Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var_ref("x")],
    };
    // 1000 * x * x * x
    let x_cubed = Expression::Binary {
        op: OpBinary::Mul(Default::default()),
        lhs: Box::new(var_ref("x")),
        rhs: Box::new(Expression::Binary {
            op: OpBinary::Mul(Default::default()),
            lhs: Box::new(var_ref("x")),
            rhs: Box::new(var_ref("x")),
        }),
    };
    let stiff_term = Expression::Binary {
        op: OpBinary::Mul(Default::default()),
        lhs: Box::new(real(1000.0)),
        rhs: Box::new(x_cubed),
    };
    // 1000 * u
    let input_term = Expression::Binary {
        op: OpBinary::Mul(Default::default()),
        lhs: Box::new(real(1000.0)),
        rhs: Box::new(var_ref("u")),
    };
    // der(x) + 1000*x^3 - 1000*u
    let rhs = Expression::Binary {
        op: OpBinary::Add(Default::default()),
        lhs: Box::new(sub(derx, input_term)),
        rhs: Box::new(stiff_term),
    };

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs,
        span: Span::DUMMY,
        origin: "stiff_ode".to_string(),
        scalar_count: 1,
    });

    let opts = StepperOptions {
        max_wall_seconds_per_step: Some(5.0),
        ..StepperOptions::default()
    };
    let mut stepper = SimStepper::new(&dae, opts).expect("stepper creation should succeed");

    // Phase 1: run with u=0 for 1 s.  x stays at 0 (stable equilibrium).
    // BDF ramps up order and step size because the system is at steady state.
    for _ in 0..100 {
        stepper.step(0.01).expect("zero-input step should succeed");
    }
    let x0 = stepper.get("x").unwrap();
    assert!(x0.abs() < 1e-6, "x should be ~0 at equilibrium, got {x0}");

    // Phase 2: jump u to 1.0.  Equilibrium shifts to x = u^(1/3) = 1.0.
    // The stiffness coefficient is 3000*x^2 ≈ 3000 near the new equilibrium,
    // so the Jacobian changes drastically.
    stepper.set_input("u", 1.0).unwrap();
    for i in 0..100 {
        stepper
            .step(0.01)
            .unwrap_or_else(|e| panic!("step {i} after u→1.0 failed: {e}"));
    }
    // x should have settled to the new equilibrium x ≈ 1.0
    let x1 = stepper.get("x").unwrap();
    assert!(
        (x1 - 1.0).abs() < 0.05,
        "expected x≈1.0 at new equilibrium, got {x1}"
    );

    // Phase 3: jump again to u=8.0 → equilibrium x=2.0.
    stepper.set_input("u", 8.0).unwrap();
    for i in 0..100 {
        stepper
            .step(0.01)
            .unwrap_or_else(|e| panic!("step {i} after u→8.0 failed: {e}"));
    }
    let x2 = stepper.get("x").unwrap();
    assert!(
        (x2 - 2.0).abs() < 0.05,
        "expected x≈2.0 at new equilibrium, got {x2}"
    );
}

/// Verify that setting the same input value repeatedly does not cause
/// unnecessary solver restarts (the solver should still converge normally).
#[test]
fn test_stepper_repeated_same_input_value() {
    use crate::stepper::{SimStepper, StepperOptions};

    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.inputs
        .insert(VarName::new("u"), Variable::new(VarName::new("u")));
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            var_ref("u"),
        ),
        span: Span::DUMMY,
        origin: "ode_x".to_string(),
        scalar_count: 1,
    });

    let opts = StepperOptions {
        max_wall_seconds_per_step: Some(5.0),
        ..StepperOptions::default()
    };
    let mut stepper = SimStepper::new(&dae, opts).expect("stepper creation should succeed");

    // Set u=10, step, then keep setting u=10 every step — should not crash
    // even though inputs_dirty gets set each time.
    stepper.set_input("u", 10.0).unwrap();
    for i in 0..200 {
        stepper.set_input("u", 10.0).unwrap();
        stepper
            .step(0.01)
            .unwrap_or_else(|e| panic!("step {i} failed: {e}"));
    }

    let x = stepper.get("x").expect("should read x");
    // u=10 for 2.0 s → x ≈ 20
    assert!((x - 20.0).abs() < 1.0, "expected x≈20, got {x}");
}
