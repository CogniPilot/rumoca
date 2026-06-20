// SPEC_0021 file-size exception: derivative projection tests cover many array
// operator shapes in one suite. split plan: group direct, function, and stencil
// projection cases into focused test modules.

use super::*;
mod record_projection_tests;
use record_projection_tests::{projection_assignment, projection_call};
mod vector_projection_tests;

#[test]
fn lower_derivative_rhs_inlines_direct_matrix_vector_assignment() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("p"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("p")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("v_b"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("v_b")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("R"),
        dae::Variable {
            dims: vec![3, 3],
            ..scalar_var("R")
        },
    );
    dae_model.variables.outputs.insert(
        rumoca_core::VarName::new("v_w"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("v_w")
        },
    );

    for idx in 1..=3 {
        dae_model.continuous.equations.push(residual(sub(
            der(indexed_var("p", idx)),
            indexed_var("v_w", idx),
        )));
    }
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("v_w").into()),
        // MLS §10.6.5: matrix * vector is a vector expression. ODE RHS
        // extraction must preserve that direct algebraic relation instead of
        // reading a stale scalarized solver slot.
        rhs: mul(var("R"), var("v_b")),
        span: lower_test_span(),
        origin: "direct matrix-vector assignment".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = scalar_program_block_fixture(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("direct matrix-vector assignment should lower into derivative RHS"),
    );
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "v_b[3]", 24.0);
    set_y_value(&layout, &mut y, "R[3,3]", 1.0);
    set_y_value(&layout, &mut y, "v_w[3]", 0.0);

    let (_, output) = eval_block_output(&rows, 2, &y, &[], 0.0);

    assert_eq!(output, Some(24.0));
}

#[test]
fn lower_derivative_rhs_keeps_structured_map_through_direct_assignment() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), array_var("x", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("y"), array_var("y", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("tmp"), array_var("tmp", &[3]));

    for idx in 1..=3 {
        dae_model.continuous.equations.push(residual(sub(
            der(indexed_var("x", idx)),
            indexed_var("tmp", idx),
        )));
    }
    dae_model
        .continuous
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 3,
                    step: 1,
                }],
            },
            first_equation_index: 0,
            equation_counts: vec![1; 3],
            span: lower_test_span(),
            origin: "for i in 1:3".to_string(),
        });
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("tmp").into()),
        rhs: var("y"),
        span: lower_test_span(),
        origin: "direct vector alias".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("structured derivative alias should lower");

    assert!(
        matches!(
            block.nodes.as_slice(),
            [rumoca_ir_solve::ComputeNode::Map { .. }]
        ),
        "{:#?}",
        block.nodes
    );
}

#[test]
fn lower_derivative_rhs_keeps_structured_map_through_guarded_direct_assignment() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), array_var("x", &[3]));
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("source"),
        array_var("source", &[3]),
    );
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("mask"), array_var("mask", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("tmp"), array_var("tmp", &[3]));
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("fallback"),
        scalar_var("fallback"),
    );

    for idx in 1..=3 {
        dae_model.continuous.equations.push(residual(sub(
            der(indexed_var("x", idx)),
            indexed_var("tmp", idx),
        )));
    }
    dae_model
        .continuous
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 3,
                    step: 1,
                }],
            },
            first_equation_index: 0,
            equation_counts: vec![1; 3],
            span: lower_test_span(),
            origin: "for i in 1:3".to_string(),
        });
    for idx in 1..=3 {
        dae_model.continuous.equations.push(dae::Equation {
            lhs: Some(rumoca_core::VarName::new(format!("tmp[{idx}]")).into()),
            rhs: rumoca_core::Expression::If {
                branches: vec![(
                    binary(
                        rumoca_core::OpBinary::Gt,
                        indexed_var("mask", idx),
                        real_lit(0.0),
                    ),
                    builtin(
                        rumoca_core::BuiltinFunction::Sqrt,
                        vec![builtin(
                            rumoca_core::BuiltinFunction::Max,
                            vec![indexed_var("source", idx), real_lit(0.0)],
                        )],
                    ),
                )],
                else_branch: Box::new(var("fallback")),
                span: lower_test_span(),
            },
            span: lower_test_span(),
            origin: "guarded direct scalar assignment".to_string(),
            scalar_count: 1,
        });
    }

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("structured guarded derivative alias should lower");

    assert!(
        matches!(
            block.nodes.as_slice(),
            [rumoca_ir_solve::ComputeNode::Map { .. }]
        ),
        "{:#?}",
        block.nodes
    );
}

#[test]
fn lower_derivative_rhs_keeps_structured_map_through_direct_coefficient_assignment() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("temp"), array_var("temp", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("mass"), array_var("mass", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("heat"), array_var("heat", &[3]));
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("mass_source"),
        array_var("mass_source", &[3]),
    );

    for idx in 1..=3 {
        dae_model.continuous.equations.push(residual(sub(
            mul(indexed_var("mass", idx), der(indexed_var("temp", idx))),
            indexed_var("heat", idx),
        )));
    }
    dae_model
        .continuous
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 3,
                    step: 1,
                }],
            },
            first_equation_index: 0,
            equation_counts: vec![1; 3],
            span: lower_test_span(),
            origin: "for i in 1:3".to_string(),
        });
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("mass").into()),
        rhs: var("mass_source"),
        span: lower_test_span(),
        origin: "direct shell mass assignment".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("structured derivative coefficient assignment should lower");
    assert!(
        matches!(
            block.nodes.as_slice(),
            [rumoca_ir_solve::ComputeNode::Map { .. }]
        ),
        "{:#?}",
        block.nodes
    );

    let rows = scalar_program_block_fixture(&block);
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    for idx in 1..=3 {
        set_y_value(&layout, &mut y, &format!("heat[{idx}]"), (idx * 12) as f64);
        set_p_value(&layout, &mut p, &format!("mass_source[{idx}]"), idx as f64);
    }

    let outputs = rows
        .programs
        .iter()
        .map(|row| {
            eval_linear_ops(row, &y, &p, 0.0)
                .1
                .expect("derivative output")
        })
        .collect::<Vec<_>>();
    assert_eq!(outputs, vec![12.0, 12.0, 12.0]);
}

#[test]
// SPEC_0021: Exception - chained flux stencil regression keeps the full source
// model and tensor assertions together.
#[allow(clippy::too_many_lines)]
fn lower_derivative_rhs_keeps_structured_stencil_through_chained_flux_assignments() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("temp"), array_var("temp", &[5]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("mass"), array_var("mass", &[5]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("q"), array_var("q", &[6]));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("area"), array_var("area", &[6]));
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("mass_source"),
        array_var("mass_source", &[5]),
    );

    dae_model
        .continuous
        .equations
        .push(residual(sub(der(indexed_var("temp", 1)), real_lit(0.0))));
    for cell in 2..=4 {
        dae_model.continuous.equations.push(residual(sub(
            mul(indexed_var("mass", cell), der(indexed_var("temp", cell))),
            sub(indexed_var("q", cell), indexed_var("q", cell + 1)),
        )));
    }
    dae_model
        .continuous
        .equations
        .push(residual(sub(der(indexed_var("temp", 5)), real_lit(0.0))));
    dae_model
        .continuous
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 2,
                    upper: 4,
                    step: 1,
                }],
            },
            first_equation_index: 1,
            equation_counts: vec![1; 3],
            span: lower_test_span(),
            origin: "for i in 2:4".to_string(),
        });
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("mass").into()),
        rhs: var("mass_source"),
        span: lower_test_span(),
        origin: "direct shell mass assignment".to_string(),
        scalar_count: 5,
    });
    for edge in 2..=5 {
        dae_model.continuous.equations.push(dae::Equation {
            lhs: Some(rumoca_core::VarName::new(format!("q[{edge}]")).into()),
            rhs: mul(
                indexed_var("area", edge),
                sub(indexed_var("temp", edge - 1), indexed_var("temp", edge)),
            ),
            span: lower_test_span(),
            origin: "direct conductive flux assignment".to_string(),
            scalar_count: 1,
        });
    }

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("structured chained flux assignments should lower");
    assert!(
        block.nodes.iter().any(|node| matches!(
            node,
            rumoca_ir_solve::ComputeNode::AffineStencil { domain, .. }
                if domain.scalar_count().is_ok_and(|count| count == 3)
        )),
        "{:#?}",
        block.nodes
    );

    let rows = scalar_program_block_fixture(&block);
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    for (idx, value) in [(1, 10.0), (2, 8.0), (3, 5.0), (4, 1.0), (5, -4.0)] {
        set_y_value(&layout, &mut y, &format!("temp[{idx}]"), value);
    }
    for idx in 1..=5 {
        set_p_value(&layout, &mut p, &format!("mass_source[{idx}]"), idx as f64);
    }
    for idx in 2..=5 {
        set_p_value(&layout, &mut p, &format!("area[{idx}]"), 1.0);
    }

    let outputs = rows
        .programs
        .iter()
        .map(|row| {
            eval_linear_ops(row, &y, &p, 0.0)
                .1
                .expect("derivative output")
        })
        .collect::<Vec<_>>();
    assert_eq!(outputs[0], 0.0);
    assert!((outputs[1] + 0.5).abs() < 1.0e-12);
    assert!((outputs[2] + 1.0 / 3.0).abs() < 1.0e-12);
    assert!((outputs[3] + 0.25).abs() < 1.0e-12);
    assert_eq!(outputs[4], 0.0);
}

#[test]
// SPEC_0021: Exception - geometry-chain stencil regression keeps the full
// multi-equation setup and tensor assertions together.
#[allow(clippy::too_many_lines)]
fn lower_derivative_rhs_keeps_structured_stencil_through_turkey_geometry_chain() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("temp"), array_var("temp", &[5]));
    for name in ["r", "area", "q"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), array_var(name, &[6]));
    }
    for name in ["volume", "mass"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), array_var(name, &[5]));
    }
    for name in ["dr", "pi", "rho", "cp", "kc"] {
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    let div = |lhs, rhs| binary(rumoca_core::OpBinary::Div, lhs, rhs);
    let pow = |lhs, rhs| binary(rumoca_core::OpBinary::Exp, lhs, rhs);

    dae_model
        .continuous
        .equations
        .push(residual(sub(der(indexed_var("temp", 1)), real_lit(0.0))));
    for cell in 2..=4 {
        dae_model.continuous.equations.push(residual(sub(
            mul(
                mul(indexed_var("mass", cell), var("cp")),
                der(indexed_var("temp", cell)),
            ),
            sub(indexed_var("q", cell), indexed_var("q", cell + 1)),
        )));
    }
    dae_model
        .continuous
        .equations
        .push(residual(sub(der(indexed_var("temp", 5)), real_lit(0.0))));
    dae_model
        .continuous
        .structured_equations
        .push(dae::StructuredEquationFamily {
            domain: rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 2,
                    upper: 4,
                    step: 1,
                }],
            },
            first_equation_index: 1,
            equation_counts: vec![1; 3],
            span: lower_test_span(),
            origin: "for i in 2:4".to_string(),
        });
    for idx in 1..=6 {
        dae_model.continuous.equations.push(dae::Equation {
            lhs: Some(rumoca_core::VarName::new(format!("r[{idx}]")).into()),
            rhs: mul(real_lit((idx - 1) as f64), var("dr")),
            span: lower_test_span(),
            origin: "direct radius assignment".to_string(),
            scalar_count: 1,
        });
        dae_model.continuous.equations.push(dae::Equation {
            lhs: Some(rumoca_core::VarName::new(format!("area[{idx}]")).into()),
            rhs: mul(
                mul(real_lit(4.0), var("pi")),
                pow(indexed_var("r", idx), real_lit(2.0)),
            ),
            span: lower_test_span(),
            origin: "direct area assignment".to_string(),
            scalar_count: 1,
        });
    }
    for idx in 1..=5 {
        dae_model.continuous.equations.push(dae::Equation {
            lhs: Some(rumoca_core::VarName::new(format!("volume[{idx}]")).into()),
            rhs: mul(
                mul(div(real_lit(4.0), real_lit(3.0)), var("pi")),
                sub(
                    pow(indexed_var("r", idx + 1), real_lit(3.0)),
                    pow(indexed_var("r", idx), real_lit(3.0)),
                ),
            ),
            span: lower_test_span(),
            origin: "direct volume assignment".to_string(),
            scalar_count: 1,
        });
        dae_model.continuous.equations.push(dae::Equation {
            lhs: Some(rumoca_core::VarName::new(format!("mass[{idx}]")).into()),
            rhs: mul(var("rho"), indexed_var("volume", idx)),
            span: lower_test_span(),
            origin: "direct mass assignment".to_string(),
            scalar_count: 1,
        });
    }
    for edge in 2..=5 {
        dae_model.continuous.equations.push(dae::Equation {
            lhs: Some(rumoca_core::VarName::new(format!("q[{edge}]")).into()),
            rhs: div(
                mul(
                    mul(var("kc"), indexed_var("area", edge)),
                    sub(indexed_var("temp", edge - 1), indexed_var("temp", edge)),
                ),
                var("dr"),
            ),
            span: lower_test_span(),
            origin: "direct conductive flux assignment".to_string(),
            scalar_count: 1,
        });
    }

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let block = lower_derivative_rhs(&dae_model, &layout)
        .expect("structured Turkey geometry chain should lower");
    assert!(
        block.nodes.iter().any(|node| matches!(
            node,
            rumoca_ir_solve::ComputeNode::AffineStencil { domain, .. }
                if domain.scalar_count().is_ok_and(|count| count == 3)
        )),
        "{:#?}",
        block.nodes
    );
}

#[test]
fn lower_derivative_rhs_projects_cross_alias_components() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), array_var("x", &[3]));
    for name in ["a", "b", "tmp"] {
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new(name), array_var(name, &[3]));
    }

    for idx in 1..=3 {
        dae_model.continuous.equations.push(residual(sub(
            der(indexed_var("x", idx)),
            indexed_var("tmp", idx),
        )));
    }
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("tmp").into()),
        rhs: builtin(
            rumoca_core::BuiltinFunction::Cross,
            vec![var("a"), var("b")],
        ),
        span: lower_test_span(),
        origin: "direct cross alias".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = scalar_program_block_fixture(
        &lower_derivative_rhs(&dae_model, &layout).expect("cross alias should lower"),
    );
    let mut y = vec![0.0; layout.y_scalars()];
    for (name, value) in [
        ("a[1]", 1.0),
        ("a[2]", 2.0),
        ("a[3]", 3.0),
        ("b[1]", 4.0),
        ("b[2]", 5.0),
        ("b[3]", 6.0),
    ] {
        set_y_value(&layout, &mut y, name, value);
    }

    let mut outputs = vec![0.0; rows.output_count()];
    rumoca_eval_solve::eval_scalar_program_block(&rows, &y, &[], 0.0, None, &mut outputs)
        .expect("multi-output derivative rows should evaluate");

    assert_eq!(outputs, vec![-3.0, 6.0, -3.0]);
}

#[test]
fn lower_derivative_rhs_projects_cross_alias_with_scalarized_operands() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), array_var("x", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("tmp"), array_var("tmp", &[3]));
    for base in ["a", "b"] {
        for idx in 1..=3 {
            let name = format!("{base}[{idx}]");
            dae_model
                .variables
                .algebraics
                .insert(rumoca_core::VarName::new(&name), scalar_var(&name));
        }
    }

    for idx in 1..=3 {
        dae_model.continuous.equations.push(residual(sub(
            der(indexed_var("x", idx)),
            indexed_var("tmp", idx),
        )));
    }
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("tmp").into()),
        rhs: builtin(
            rumoca_core::BuiltinFunction::Cross,
            vec![var("a"), var("b")],
        ),
        span: lower_test_span(),
        origin: "direct cross alias with scalarized operands".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = scalar_program_block_fixture(
        &lower_derivative_rhs(&dae_model, &layout).expect("scalarized cross alias should lower"),
    );
    let mut y = vec![0.0; layout.y_scalars()];
    for (name, value) in [
        ("a[1]", 1.0),
        ("a[2]", 2.0),
        ("a[3]", 3.0),
        ("b[1]", 4.0),
        ("b[2]", 5.0),
        ("b[3]", 6.0),
    ] {
        set_y_value(&layout, &mut y, name, value);
    }

    let mut outputs = vec![0.0; rows.output_count()];
    rumoca_eval_solve::eval_scalar_program_block(&rows, &y, &[], 0.0, None, &mut outputs)
        .expect("multi-output derivative rows should evaluate");

    assert_eq!(outputs, vec![-3.0, 6.0, -3.0]);
}

#[test]
fn lower_derivative_rhs_projects_cross_alias_inside_coupled_solve() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), array_var("x", &[3]));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("tmp"), array_var("tmp", &[3]));
    for base in ["a", "b"] {
        for idx in 1..=3 {
            let name = format!("{base}[{idx}]");
            dae_model
                .variables
                .algebraics
                .insert(rumoca_core::VarName::new(&name), scalar_var(&name));
        }
    }

    dae_model.continuous.equations.push(residual(sub(
        add(der(indexed_var("x", 1)), der(indexed_var("x", 2))),
        add(indexed_var("tmp", 1), indexed_var("tmp", 2)),
    )));
    dae_model.continuous.equations.push(residual(sub(
        add(der(indexed_var("x", 2)), der(indexed_var("x", 3))),
        add(indexed_var("tmp", 2), indexed_var("tmp", 3)),
    )));
    dae_model.continuous.equations.push(residual(sub(
        der(indexed_var("x", 3)),
        indexed_var("tmp", 3),
    )));
    dae_model.continuous.equations.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("tmp").into()),
        rhs: builtin(
            rumoca_core::BuiltinFunction::Cross,
            vec![var("a"), var("b")],
        ),
        span: lower_test_span(),
        origin: "direct cross alias for coupled solve".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = scalar_program_block_fixture(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("coupled scalarized cross alias should lower"),
    );
    let mut y = vec![0.0; layout.y_scalars()];
    for (name, value) in [
        ("a[1]", 1.0),
        ("a[2]", 2.0),
        ("a[3]", 3.0),
        ("b[1]", 4.0),
        ("b[2]", 5.0),
        ("b[3]", 6.0),
    ] {
        set_y_value(&layout, &mut y, name, value);
    }

    let mut outputs = vec![0.0; rows.output_count()];
    rumoca_eval_solve::eval_scalar_program_block(&rows, &y, &[], 0.0, None, &mut outputs)
        .expect("multi-output derivative rows should evaluate");

    assert_eq!(outputs, vec![-3.0, 6.0, -3.0]);
}

#[test]
fn lower_derivative_rhs_indexes_builtin_array_rhs() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("A"),
        dae::Variable {
            dims: vec![2, 2],
            ..scalar_var("A")
        },
    );
    let rhs = rumoca_core::Expression::Index {
        base: Box::new(builtin(
            rumoca_core::BuiltinFunction::Transpose,
            vec![var("A")],
        )),
        subscripts: vec![
            rumoca_core::Subscript::generated_index(2, lower_test_span()),
            rumoca_core::Subscript::generated_index(1, lower_test_span()),
        ],
        span: lower_test_span(),
    };
    let mut equation = residual(sub(der(var("x")), rhs));
    equation.span = lower_test_span();
    dae_model.continuous.equations.push(equation);

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = scalar_program_block_fixture(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("indexed builtin array RHS should lower in derivative rows"),
    );
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "A[1,2]", 42.0);

    let (_, output) = eval_block_output(&rows, 0, &y, &[], 0.0);

    assert_eq!(output, Some(42.0));
}

#[test]
fn lower_derivative_rhs_projects_scalarized_vector_function_output_with_vector_alias_arg() {
    let dae_model = scalarized_vector_function_alias_fixture();
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = scalar_program_block_fixture(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("projected vector function output should bind vector alias argument"),
    );
    let mut y = vec![0.0; layout.y_scalars()];
    set_y_value(&layout, &mut y, "omega[1]", 12.0);

    let (_, output) = eval_block_output(&rows, 0, &y, &[], 0.0);

    assert_eq!(output, Some(12.0));
}

fn scalarized_vector_function_alias_fixture() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("q"),
        dae::Variable {
            dims: vec![4],
            ..scalar_var("q")
        },
    );
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("omega"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("omega")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("alias_omega"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("alias_omega")
        },
    );
    add_quat_derivative_function(&mut dae_model);
    add_scalarized_vector_alias_projection_equations(&mut dae_model);
    dae_model
}

fn add_quat_derivative_function(dae_model: &mut dae::Dae) {
    let mut quat_derivative = rumoca_core::Function::new("Pkg.quatDerivative", lower_test_span());
    quat_derivative
        .inputs
        .push(function_param_with_dims("q", &[4]));
    quat_derivative
        .inputs
        .push(function_param_with_dims("omega", &[3]));
    quat_derivative
        .outputs
        .push(function_param_with_dims("q_dot", &[4]));
    quat_derivative
        .body
        .push(rumoca_core::Statement::Assignment {
            comp: component_ref_index("q_dot", 1),
            value: indexed_var("omega", 1),
            span: lower_test_span(),
        });
    for idx in 2..=4 {
        quat_derivative
            .body
            .push(rumoca_core::Statement::Assignment {
                comp: component_ref_index("q_dot", idx),
                value: real_lit(0.0),
                span: lower_test_span(),
            });
    }
    dae_model
        .symbols
        .functions
        .insert(quat_derivative.name.clone(), quat_derivative);
}

fn add_scalarized_vector_alias_projection_equations(dae_model: &mut dae::Dae) {
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(
            der(indexed_var("q", 1)),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.quatDerivative.q_dot[1]"),
                args: vec![var("q"), var("alias_omega")],
                is_constructor: false,
                span: lower_test_span(),
            },
        ),
        span: lower_test_span(),
        origin: "selected vector function output".to_string(),
        scalar_count: 1,
    });
    for idx in 2..=4 {
        dae_model
            .continuous
            .equations
            .push(residual(sub(der(indexed_var("q", idx)), real_lit(0.0))));
    }
    for idx in 1..=3 {
        dae_model
            .continuous
            .equations
            .push(residual(sub(der(indexed_var("omega", idx)), real_lit(0.0))));
    }
    for idx in 1..=3 {
        dae_model.continuous.equations.push(dae::Equation {
            lhs: Some(rumoca_core::VarName::new(format!("alias_omega[{idx}]")).into()),
            rhs: indexed_var("omega", idx),
            span: lower_test_span(),
            origin: "scalarized vector alias".to_string(),
            scalar_count: 1,
        });
    }
}

#[test]
fn lower_derivative_rhs_lowers_compact_matrix_times_state_derivative() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("omega"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("omega")
        },
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("J"),
        dae::Variable {
            dims: vec![3, 3],
            ..scalar_var("J")
        },
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("M_body"),
        dae::Variable {
            dims: vec![3],
            ..scalar_var("M_body")
        },
    );
    dae_model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: sub(mul(var("J"), der(var("omega"))), var("M_body")),
        span: lower_test_span(),
        origin: "compact angular dynamics".to_string(),
        scalar_count: 3,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    lower_residual(&dae_model, &layout)
        .expect("array der() should preserve vector shape in residual lowering");
    let rows = scalar_program_block_fixture(
        &lower_derivative_rhs(&dae_model, &layout)
            .expect("compact matrix-vector derivative equation should lower"),
    );
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    for idx in 1..=3 {
        set_p_value(&layout, &mut p, &format!("J[{idx},{idx}]"), idx as f64);
        set_y_value(&layout, &mut y, &format!("M_body[{idx}]"), 6.0);
    }

    let outputs = eval_block_all_outputs(&rows, &y, &p, 0.0);
    assert_eq!(outputs, vec![6.0, 3.0, 2.0]);
}
