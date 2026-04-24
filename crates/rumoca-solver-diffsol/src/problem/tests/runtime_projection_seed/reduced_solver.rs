use super::*;

pub(crate) fn add_trapezoid_parameter_starts(dae: &mut Dae) {
    for (name, start) in [
        ("vIn.signalSource.offset", lit(-5.0)),
        ("vIn.signalSource.amplitude", lit(10.0)),
        ("vIn.signalSource.startTime", lit(-0.035)),
        ("vIn.signalSource.T_rising", lit(0.01)),
        ("vIn.signalSource.T_width", lit(0.05)),
        ("vIn.signalSource.T_falling", lit(0.07)),
        ("vIn.signalSource.rising", lit(0.01)),
        ("vIn.signalSource.falling", lit(0.02)),
    ] {
        let mut var = Variable::new(VarName::new(name));
        var.start = Some(start);
        dae.parameters.insert(VarName::new(name), var);
    }

    let mut nperiod = Variable::new(VarName::new("vIn.signalSource.nperiod"));
    nperiod.start = Some(Expression::Unary {
        op: rumoca_ir_core::OpUnary::Minus(Default::default()),
        rhs: Box::new(Expression::Literal(dae::Literal::Integer(1))),
    });
    dae.parameters
        .insert(VarName::new("vIn.signalSource.nperiod"), nperiod);

    let mut count = Variable::new(VarName::new("vIn.signalSource.count"));
    count.start = Some(lit(0.0));
    dae.discrete_valued
        .insert(VarName::new("vIn.signalSource.count"), count);

    let mut t_start = Variable::new(VarName::new("vIn.signalSource.T_start"));
    t_start.start = Some(lit(-0.035));
    dae.discrete_reals
        .insert(VarName::new("vIn.signalSource.T_start"), t_start);
}

fn trapezoid_inactive_expr() -> Expression {
    Expression::Binary {
        op: rumoca_ir_core::OpBinary::Or(Default::default()),
        lhs: Box::new(Expression::Binary {
            op: rumoca_ir_core::OpBinary::Or(Default::default()),
            lhs: Box::new(binop(
                OpBinary::Lt(Default::default()),
                var("time"),
                var("vIn.signalSource.startTime"),
            )),
            rhs: Box::new(binop(
                OpBinary::Eq(Default::default()),
                var("vIn.signalSource.nperiod"),
                Expression::Literal(dae::Literal::Integer(0)),
            )),
        }),
        rhs: Box::new(Expression::Binary {
            op: rumoca_ir_core::OpBinary::And(Default::default()),
            lhs: Box::new(binop(
                OpBinary::Gt(Default::default()),
                var("vIn.signalSource.nperiod"),
                Expression::Literal(dae::Literal::Integer(0)),
            )),
            rhs: Box::new(binop(
                OpBinary::Ge(Default::default()),
                var("vIn.signalSource.count"),
                var("vIn.signalSource.nperiod"),
            )),
        }),
    }
}

fn trapezoid_rising_expr() -> Expression {
    binop(
        OpBinary::Div(Default::default()),
        binop(
            OpBinary::Mul(Default::default()),
            var("vIn.signalSource.amplitude"),
            binop(
                OpBinary::Sub(Default::default()),
                var("time"),
                var("vIn.signalSource.T_start"),
            ),
        ),
        var("vIn.signalSource.rising"),
    )
}

fn trapezoid_falling_expr() -> Expression {
    binop(
        OpBinary::Div(Default::default()),
        binop(
            OpBinary::Mul(Default::default()),
            var("vIn.signalSource.amplitude"),
            binop(
                OpBinary::Sub(Default::default()),
                binop(
                    OpBinary::Add(Default::default()),
                    var("vIn.signalSource.T_start"),
                    var("vIn.signalSource.T_falling"),
                ),
                var("time"),
            ),
        ),
        var("vIn.signalSource.falling"),
    )
}

pub(crate) fn trapezoid_drive_expr() -> Expression {
    let rising_end = binop(
        OpBinary::Add(Default::default()),
        var("vIn.signalSource.T_start"),
        var("vIn.signalSource.T_rising"),
    );
    let width_end = binop(
        OpBinary::Add(Default::default()),
        var("vIn.signalSource.T_start"),
        var("vIn.signalSource.T_width"),
    );
    let falling_end = binop(
        OpBinary::Add(Default::default()),
        var("vIn.signalSource.T_start"),
        var("vIn.signalSource.T_falling"),
    );
    binop(
        OpBinary::Add(Default::default()),
        var("vIn.signalSource.offset"),
        Expression::If {
            branches: vec![(
                trapezoid_inactive_expr(),
                Expression::Literal(dae::Literal::Integer(0)),
            )],
            else_branch: Box::new(Expression::If {
                branches: vec![(
                    binop(OpBinary::Lt(Default::default()), var("time"), rising_end),
                    trapezoid_rising_expr(),
                )],
                else_branch: Box::new(Expression::If {
                    branches: vec![(
                        binop(OpBinary::Lt(Default::default()), var("time"), width_end),
                        var("vIn.signalSource.amplitude"),
                    )],
                    else_branch: Box::new(Expression::If {
                        branches: vec![(
                            binop(OpBinary::Lt(Default::default()), var("time"), falling_end),
                            trapezoid_falling_expr(),
                        )],
                        else_branch: Box::new(Expression::Literal(dae::Literal::Integer(0))),
                    }),
                }),
            }),
        },
    )
}

fn build_reduced_solver_trapezoid_dae(include_alias: bool) -> Dae {
    let mut dae = Dae::new();
    dae.algebraics.insert(
        VarName::new("vIn.p.v"),
        Variable::new(VarName::new("vIn.p.v")),
    );
    if include_alias {
        dae.algebraics.insert(
            VarName::new("r1.p.v"),
            Variable::new(VarName::new("r1.p.v")),
        );
    }
    add_trapezoid_parameter_starts(&mut dae);

    // MLS Blocks.Sources.Trapezoid / MLS §8.3.5: reduced runtime direct-seed
    // extraction must still recognize the solver-target residual shape
    // `(solver - 0) - source_expr` produced after DAE reduction.
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        binop(OpBinary::Sub(Default::default()), var("vIn.p.v"), lit(0.0)),
        trapezoid_drive_expr(),
    )));
    if include_alias {
        dae.f_x.push(eq_from(binop(
            OpBinary::Sub(Default::default()),
            var("vIn.p.v"),
            var("r1.p.v"),
        )));
    }
    dae
}

#[test]
fn test_runtime_direct_seed_handles_reduced_solver_minus_zero_source_shape() {
    let dae = build_reduced_solver_trapezoid_dae(false);
    let params = default_params(&dae);
    let ctx = build_runtime_direct_seed_context(&dae, 1, 0);
    let mut y = vec![0.0];

    let updates =
        seed_runtime_direct_assignment_values_with_context(&ctx, &dae, &mut y, &params, 0.0);

    assert!(
        updates > 0,
        "runtime direct seed should update reduced solver target"
    );
    assert!(
        (y[0] - 5.0).abs() <= 1.0e-12,
        "runtime direct seed should keep the reduced trapezoid source active, got {}",
        y[0]
    );
}

#[test]
fn test_runtime_direct_seed_skips_alias_after_reduced_solver_wrapper_definition() {
    let dae = build_reduced_solver_trapezoid_dae(true);
    let params = default_params(&dae);
    let ctx = build_runtime_direct_seed_context(&dae, 2, 0);
    let mut y = vec![0.0, -4.5];

    let updates =
        seed_runtime_direct_assignment_values_with_context(&ctx, &dae, &mut y, &params, 0.0);

    assert!(
        updates > 0,
        "runtime direct seed should update reduced solver target"
    );
    assert!(
        (y[0] - 5.0).abs() <= 1.0e-12,
        "runtime direct seed should keep the reduced defining equation instead of stale alias value, got {}",
        y[0]
    );
    assert!(
        (y[1] + 4.5).abs() <= 1.0e-12,
        "plain alias source should stay untouched, got {}",
        y[1]
    );
}
