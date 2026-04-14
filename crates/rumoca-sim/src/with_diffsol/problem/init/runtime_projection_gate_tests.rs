use super::*;
use crate::with_diffsol::test_support::{binop, lit, var};
use rumoca_core::Span;
use rumoca_ir_dae::Literal;

#[test]
fn runtime_projection_needs_settled_discrete_env_skips_ignored_rows() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.discrete_valued
        .insert(VarName::new("flag"), Variable::new(VarName::new("flag")));
    dae.f_m.push(Equation::explicit(
        VarName::new("flag"),
        Expression::Literal(Literal::Real(1.0)),
        Span::DUMMY,
        "test flag update",
    ));
    dae.f_x.push(Equation::explicit(
        VarName::new("x"),
        Expression::VarRef {
            name: VarName::new("flag"),
            subscripts: vec![],
        },
        Span::DUMMY,
        "test x=flag",
    ));

    assert!(runtime_projection_needs_settled_discrete_env(&dae, None));
    assert!(!runtime_projection_needs_settled_discrete_env(
        &dae,
        Some(&[true]),
    ));
}

#[test]
fn runtime_projection_masks_mark_single_free_branch_local_analog_unknown() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("v"), Variable::new(VarName::new("v")));

    dae.f_x.push(Equation::residual(
        Expression::Literal(Literal::Real(0.0)),
        Span::DUMMY,
        "state row",
    ));
    dae.f_x.push(Equation::explicit(
        VarName::new("v"),
        Expression::BuiltinCall {
            function: BuiltinFunction::Smooth,
            args: vec![
                lit(0.0),
                Expression::If {
                    branches: vec![(
                        binop(OpBinary::Gt(Default::default()), var("x"), lit(0.0)),
                        var("v"),
                    )],
                    else_branch: Box::new(lit(0.0)),
                },
            ],
        },
        Span::DUMMY,
        "branch-local analog row",
    ));

    let masks = build_runtime_projection_masks(&dae, 1, dae.f_x.len());
    let names = solver_vector_names(&dae, dae.f_x.len());
    let v_idx = names.iter().position(|name| name == "v").expect("v idx");

    assert_eq!(masks.branch_local_analog_unknowns[1], Some(v_idx));
    assert!(masks.branch_local_analog_cols[v_idx]);
}

#[test]
fn runtime_projection_masks_ignore_non_smooth_branch_rows() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("v"), Variable::new(VarName::new("v")));

    dae.f_x.push(Equation::residual(
        Expression::Literal(Literal::Real(0.0)),
        Span::DUMMY,
        "state row",
    ));
    dae.f_x.push(Equation::explicit(
        VarName::new("v"),
        Expression::If {
            branches: vec![(
                binop(OpBinary::Gt(Default::default()), var("x"), lit(0.0)),
                var("v"),
            )],
            else_branch: Box::new(lit(0.0)),
        },
        Span::DUMMY,
        "plain branch row",
    ));

    let masks = build_runtime_projection_masks(&dae, 1, dae.f_x.len());

    assert_eq!(masks.branch_local_analog_unknowns[1], None);
}

#[test]
fn runtime_projection_masks_mark_coupled_branch_local_analog_row_pair() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("vin"), Variable::new(VarName::new("vin")));
    dae.algebraics
        .insert(VarName::new("vout"), Variable::new(VarName::new("vout")));

    let limiter = Expression::BuiltinCall {
        function: BuiltinFunction::Smooth,
        args: vec![
            lit(0.0),
            Expression::BuiltinCall {
                function: BuiltinFunction::NoEvent,
                args: vec![Expression::If {
                    branches: vec![(
                        binop(OpBinary::Gt(Default::default()), var("vin"), lit(0.0)),
                        lit(5.0),
                    )],
                    else_branch: Box::new(var("vin")),
                }],
            },
        ],
    };
    dae.f_x.push(Equation::residual(
        binop(OpBinary::Sub(Default::default()), var("vout"), limiter),
        Span::DUMMY,
        "limiter row",
    ));
    dae.f_x.push(Equation::residual(
        binop(
            OpBinary::Sub(Default::default()),
            binop(OpBinary::Add(Default::default()), var("vin"), var("vout")),
            lit(1.0),
        ),
        Span::DUMMY,
        "coupling row",
    ));

    let masks = build_runtime_projection_masks(&dae, 0, dae.f_x.len());

    assert!(
        masks
            .branch_local_analog_unknowns
            .iter()
            .all(Option::is_none)
    );
    assert_eq!(masks.branch_local_analog_row_pairs, vec![(0, 1, [0, 1])]);
    assert!(masks.branch_local_analog_cols[0]);
    assert!(masks.branch_local_analog_cols[1]);
}

#[test]
fn runtime_projection_masks_propagate_fixed_state_alias_closure_into_branch_row() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    for name in ["x_node", "x_probe", "z"] {
        dae.algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    dae.f_x.push(Equation::residual(
        Expression::Literal(Literal::Real(0.0)),
        Span::DUMMY,
        "state row",
    ));
    dae.f_x.push(Equation::residual(
        binop(
            OpBinary::Sub(Default::default()),
            var("x_node"),
            binop(OpBinary::Sub(Default::default()), var("x"), lit(0.0)),
        ),
        Span::DUMMY,
        "state alias row",
    ));
    dae.f_x.push(Equation::explicit(
        VarName::new("x_probe"),
        var("x_node"),
        Span::DUMMY,
        "state alias closure row",
    ));
    dae.f_x.push(Equation::explicit(
        VarName::new("z"),
        Expression::BuiltinCall {
            function: BuiltinFunction::Smooth,
            args: vec![
                lit(0.0),
                Expression::BuiltinCall {
                    function: BuiltinFunction::NoEvent,
                    args: vec![Expression::If {
                        branches: vec![(
                            binop(OpBinary::Gt(Default::default()), var("x_probe"), lit(0.0)),
                            var("z"),
                        )],
                        else_branch: Box::new(lit(0.0)),
                    }],
                },
            ],
        },
        Span::DUMMY,
        "branch-local analog row",
    ));

    let masks = build_runtime_projection_masks(&dae, 1, dae.f_x.len());
    let names = solver_vector_names(&dae, dae.f_x.len());
    let z_idx = names.iter().position(|name| name == "z").expect("z idx");

    assert_eq!(masks.fixed_cols, vec![true, true, true, false]);
    assert_eq!(masks.ignored_rows, vec![true, true, true, false]);
    assert_eq!(masks.branch_local_analog_unknowns[3], Some(z_idx));
    assert!(masks.branch_local_analog_cols[z_idx]);
}

#[test]
fn runtime_projection_masks_mark_alias_connected_branch_local_groups() {
    let mut dae = Dae::new();
    for name in ["out", "out_alias", "vin", "vin_alias"] {
        dae.algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    dae.f_x.push(Equation::explicit(
        VarName::new("out_alias"),
        var("out"),
        Span::DUMMY,
        "output alias row",
    ));
    dae.f_x.push(Equation::explicit(
        VarName::new("vin_alias"),
        var("vin"),
        Span::DUMMY,
        "input alias row",
    ));
    dae.f_x.push(Equation::residual(
        binop(
            OpBinary::Sub(Default::default()),
            var("out"),
            Expression::BuiltinCall {
                function: BuiltinFunction::Smooth,
                args: vec![
                    lit(0.0),
                    Expression::BuiltinCall {
                        function: BuiltinFunction::NoEvent,
                        args: vec![Expression::If {
                            branches: vec![(
                                binop(OpBinary::Gt(Default::default()), var("vin_alias"), lit(0.0)),
                                lit(5.0),
                            )],
                            else_branch: Box::new(var("vin_alias")),
                        }],
                    },
                ],
            },
        ),
        Span::DUMMY,
        "branch-local analog row",
    ));
    dae.f_x.push(Equation::residual(
        binop(
            OpBinary::Sub(Default::default()),
            binop(
                OpBinary::Add(Default::default()),
                var("out_alias"),
                var("vin"),
            ),
            lit(1.0),
        ),
        Span::DUMMY,
        "coupling row",
    ));

    let masks = build_runtime_projection_masks(&dae, 0, dae.f_x.len());
    let names = solver_vector_names(&dae, dae.f_x.len());
    let mut marked = names
        .iter()
        .zip(masks.branch_local_analog_cols.iter())
        .filter_map(|(name, &marked)| marked.then_some(name.as_str()))
        .collect::<Vec<_>>();
    marked.sort_unstable();

    assert!(
        masks
            .branch_local_analog_unknowns
            .iter()
            .all(Option::is_none)
    );
    assert!(masks.branch_local_analog_row_pairs.is_empty());
    assert_eq!(marked, vec!["out", "out_alias", "vin", "vin_alias"]);
}

#[test]
fn runtime_projection_masks_reduce_grounded_difference_rows_to_fixed_aliases() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    for name in ["gnd", "node", "probe", "z"] {
        dae.algebraics
            .insert(VarName::new(name), Variable::new(VarName::new(name)));
    }

    dae.f_x.push(Equation::residual(
        Expression::Literal(Literal::Real(0.0)),
        Span::DUMMY,
        "state row",
    ));
    dae.f_x.push(Equation::explicit(
        VarName::new("gnd"),
        lit(0.0),
        Span::DUMMY,
        "ground row",
    ));
    dae.f_x.push(Equation::explicit(
        VarName::new("node"),
        binop(OpBinary::Sub(Default::default()), var("x"), var("gnd")),
        Span::DUMMY,
        "grounded state alias row",
    ));
    dae.f_x.push(Equation::explicit(
        VarName::new("probe"),
        var("node"),
        Span::DUMMY,
        "probe alias row",
    ));
    dae.f_x.push(Equation::explicit(
        VarName::new("z"),
        Expression::BuiltinCall {
            function: BuiltinFunction::Smooth,
            args: vec![
                lit(0.0),
                Expression::BuiltinCall {
                    function: BuiltinFunction::NoEvent,
                    args: vec![Expression::If {
                        branches: vec![(
                            binop(OpBinary::Gt(Default::default()), var("probe"), lit(0.0)),
                            var("z"),
                        )],
                        else_branch: Box::new(lit(0.0)),
                    }],
                },
            ],
        },
        Span::DUMMY,
        "branch-local analog row",
    ));

    let masks = build_runtime_projection_masks(&dae, 1, dae.f_x.len());
    let names = solver_vector_names(&dae, dae.f_x.len());
    let z_idx = names.iter().position(|name| name == "z").expect("z idx");

    assert_eq!(masks.fixed_cols, vec![true, true, true, true, false]);
    assert_eq!(masks.ignored_rows, vec![true, true, true, true, false]);
    assert_eq!(masks.branch_local_analog_unknowns[4], Some(z_idx));
    assert!(masks.branch_local_analog_cols[z_idx]);
}
