//! The kinematic state derivatives reuse the last derivative refresh exactly.

use super::{MeError, MeTime, block, instantiate, solve};

/// States `q, w, s` and the algebraic `a` with `a * a = q`:
/// `der(q) = w`, `der(w) = -a`, `der(s) = a`. The Solve IR partition makes `q`
/// and `s` kinematic (`s` reads `a`, which depends on `q` only) and `w` the
/// velocity of `q`.
fn position_algebraic_model() -> solve::SolveModel {
    let [derivative, implicit, implicit_jvp, derivative_jvp] = programs();
    assemble(Fixture {
        names: &["q", "w", "s", "a"],
        state_count: 3,
        derivative_rhs: solve::ComputeBlock::from_scalar_program_block(derivative),
        implicit,
        implicit_jvp,
        derivative_jvp,
        blocks: &[(3, 3)],
        initial_y: vec![2.25, 0.5, 0.0, 1.5],
    })
}

/// A Solve model with one single-unknown projection block per `(row, y)`.
struct Fixture<'a> {
    names: &'a [&'a str],
    state_count: usize,
    derivative_rhs: solve::ComputeBlock,
    implicit: solve::ScalarProgramBlock,
    implicit_jvp: solve::ScalarProgramBlock,
    derivative_jvp: solve::ScalarProgramBlock,
    blocks: &'a [(usize, usize)],
    initial_y: Vec<f64>,
}

fn assemble(fixture: Fixture<'_>) -> solve::SolveModel {
    let names: Vec<String> = fixture
        .names
        .iter()
        .map(|name| (*name).to_string())
        .collect();
    let width = names.len();
    let mut model = crate::fixture_layout::with_visible_expressions(
        solve::SolveModel {
            problem: solve::SolveProblem {
                layout: crate::fixture_layout::named_y_layout(
                    rumoca_ir_solve::VarLayout::from_parts(Default::default(), 0, 0),
                    names.clone(),
                ),
                continuous: solve::ContinuousSolveSystem {
                    implicit_rhs: solve::ComputeBlock::from_scalar_program_block(fixture.implicit),
                    implicit_row_targets: (0..width)
                        .map(|index| Some(solve::scalar_slot_y(index)))
                        .collect::<Vec<_>>()
                        .into(),
                    derivative_rhs: fixture.derivative_rhs,
                    algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                        blocks: fixture
                            .blocks
                            .iter()
                            .map(|&(row, y)| solve::AlgebraicProjectionBlock {
                                rows: vec![row],
                                y_indices: vec![y],
                                tearing: None,
                                alternate_charts: Vec::new(),
                            })
                            .collect::<Vec<_>>()
                            .into(),
                    },
                    ..Default::default()
                },
                solve_layout: solve::SolveLayout {
                    state_scalar_count: fixture.state_count,
                    algebraic_scalar_count: width - fixture.state_count,
                    ..Default::default()
                },
                ..Default::default()
            },
            artifacts: solve::SolveArtifacts {
                discrete: Default::default(),
                continuous: solve::ContinuousSolveArtifacts {
                    implicit_jacobian_v: solve::ComputeBlock::from_scalar_program_block(
                        fixture.implicit_jvp.clone(),
                    ),
                    implicit_jacobian_v_scalar: fixture.implicit_jvp,
                    full_jacobian_v: fixture.derivative_jvp,
                    ..Default::default()
                },
                ..Default::default()
            },
            initial_y: fixture.initial_y.into(),
            solver_nominals: vec![1.0; width],
            ..Default::default()
        },
        names,
        Default::default(),
        Vec::new(),
    );
    crate::test_fixtures::derive_structural_artifacts(&mut model);
    model
}

/// The derivative, implicit, implicit JVP and derivative JVP programs of
/// [`position_algebraic_model`].
fn programs() -> [solve::ScalarProgramBlock; 4] {
    use solve::LinearOp::{Binary, LoadSeed, LoadY, StoreOutput, Unary};
    let derivative = block(
        vec![
            vec![LoadY { dst: 0, index: 1 }, StoreOutput { src: 0 }],
            vec![
                LoadY { dst: 0, index: 3 },
                Unary {
                    dst: 1,
                    op: solve::UnaryOp::Neg,
                    arg: 0,
                },
                StoreOutput { src: 1 },
            ],
            vec![LoadY { dst: 0, index: 3 }, StoreOutput { src: 0 }],
        ],
        "fmi_me_kinematic_derivative.mo",
    );
    let square = |load: fn(u32, usize) -> solve::LinearOp| {
        vec![
            load(0, 3),
            LoadY { dst: 1, index: 3 },
            Binary {
                dst: 2,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
        ]
    };
    let mut residual = square(|dst, index| LoadY { dst, index });
    residual.extend([
        LoadY { dst: 3, index: 0 },
        Binary {
            dst: 4,
            op: solve::BinaryOp::Sub,
            lhs: 2,
            rhs: 3,
        },
        StoreOutput { src: 4 },
    ]);
    let placeholder = |index| vec![LoadY { dst: 0, index }, StoreOutput { src: 0 }];
    let implicit = block(
        vec![placeholder(0), placeholder(1), placeholder(2), residual],
        "fmi_me_kinematic_implicit.mo",
    );
    // d(a * a - q) = 2 a da - dq.
    let mut residual_jvp = square(|dst, index| LoadSeed { dst, index });
    residual_jvp.extend([
        solve::LinearOp::Const { dst: 3, value: 2.0 },
        Binary {
            dst: 4,
            op: solve::BinaryOp::Mul,
            lhs: 2,
            rhs: 3,
        },
        LoadSeed { dst: 5, index: 0 },
        Binary {
            dst: 6,
            op: solve::BinaryOp::Sub,
            lhs: 4,
            rhs: 5,
        },
        StoreOutput { src: 6 },
    ]);
    let seed = |index| vec![LoadSeed { dst: 0, index }, StoreOutput { src: 0 }];
    let implicit_jvp = block(
        vec![seed(0), seed(1), seed(2), residual_jvp],
        "fmi_me_kinematic_implicit_jvp.mo",
    );
    let derivative_jvp = block(
        vec![
            seed(1),
            vec![
                LoadSeed { dst: 0, index: 3 },
                Unary {
                    dst: 1,
                    op: solve::UnaryOp::Neg,
                    arg: 0,
                },
                StoreOutput { src: 1 },
            ],
            seed(3),
        ],
        "fmi_me_kinematic_derivative_jvp.mo",
    );
    [derivative, implicit, implicit_jvp, derivative_jvp]
}

#[test]
fn the_solve_ir_partition_reaches_the_component() {
    let model = position_algebraic_model();
    assert_eq!(
        model.artifacts.continuous.structural.kinematic_states(),
        [0, 2]
    );
    assert_eq!(instantiate(&model).kinematic_state_indices(), [0, 2]);
}

#[test]
fn kinematic_derivatives_equal_a_full_evaluation_at_the_moved_point_bit_for_bit() {
    let model = position_algebraic_model();
    let mut kernel = instantiate(&model);
    let time = 0.5;
    let evaluated = [2.25, 0.5, 0.75];
    let moved = [2.25, -1.25, 0.75];
    kernel.set_time(MeTime::at(time)).expect("a legal time");
    kernel
        .set_continuous_states(&evaluated)
        .expect("finite states");
    let mut full = Vec::new();
    kernel
        .get_continuous_state_derivatives(&mut full)
        .expect("the first point settles");

    kernel.set_continuous_states(&moved).expect("finite states");
    let mut kinematic = vec![f64::NAN; 3];
    kernel
        .kinematic_state_derivatives_into(&mut kinematic)
        .expect("the evaluated point is current");
    // The velocity entry is not written.
    assert!(kinematic[1].is_nan());

    let mut reference = Vec::new();
    kernel
        .get_continuous_state_derivatives(&mut reference)
        .expect("the moved point settles");
    for state in [0, 2] {
        assert_eq!(kinematic[state].to_bits(), reference[state].to_bits());
    }
    assert_eq!(kinematic[0], -1.25);
    assert_eq!(kinematic[2], 1.5);
}

#[test]
fn kinematic_derivatives_without_a_current_evaluation_are_refused() {
    let model = position_algebraic_model();
    let mut kernel = instantiate(&model);
    kernel.set_time(MeTime::at(0.5)).expect("a legal time");
    kernel
        .set_continuous_states(&[2.25, 0.5, 0.75])
        .expect("finite states");
    let mut out = vec![0.0; 3];
    let refusal = kernel.kinematic_state_derivatives_into(&mut out);
    assert!(
        matches!(refusal, Err(MeError::Contract { .. })),
        "{refusal:?}"
    );

    let mut full = Vec::new();
    kernel
        .get_continuous_state_derivatives(&mut full)
        .expect("the point settles");
    // A kinematic state moved: the settled algebraics are not its.
    kernel
        .set_continuous_states(&[4.0, 0.5, 0.75])
        .expect("finite states");
    let refusal = kernel.kinematic_state_derivatives_into(&mut out);
    assert!(
        matches!(refusal, Err(MeError::Contract { .. })),
        "{refusal:?}"
    );
    // So did the time.
    kernel
        .set_continuous_states(&[2.25, 0.5, 0.75])
        .expect("finite states");
    kernel.set_time(MeTime::at(0.75)).expect("a legal time");
    let refusal = kernel.kinematic_state_derivatives_into(&mut out);
    assert!(
        matches!(refusal, Err(MeError::Contract { .. })),
        "{refusal:?}"
    );
}

/// One tensor cell per `i in 1:2` writing output `output + i - 1` from
/// `base_ops`, whose first operation loads `y[index + i - 1]`.
fn map(output: usize, base_ops: Vec<solve::LinearOp>) -> solve::ComputeNode {
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: 2,
            step: 1,
        }],
    };
    solve::ComputeNode::Map {
        output_map: solve::TensorOutputMap::dense_contiguous(output, &domain)
            .expect("a dense output map"),
        domain,
        base_ops,
        load_strides: vec![solve::AffineStencilLoadStride {
            op_position: 0,
            terms: vec![solve::AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            }],
        }],
        const_strides: Vec::new(),
        metadata: solve::TensorNodeMetadata::default(),
        span: rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("fmi_me_kinematic_tensor.mo"),
            1,
            2,
        ),
    }
}

/// [`position_algebraic_model`] with vectors `q, w, s, a` of two cells and
/// every derivative row a cell of a tensor map: `der(q[i]) = w[i]`,
/// `der(w[i]) = -a[i]`, `der(s[i]) = a[i]`, `a[i] * a[i] = q[i]`.
fn vector_position_algebraic_model() -> solve::SolveModel {
    use solve::LinearOp::{Binary, Const, LoadSeed, LoadY, StoreOutput, Unary};
    let neg = |dst, arg| Unary {
        dst,
        op: solve::UnaryOp::Neg,
        arg,
    };
    let mul = |dst, lhs, rhs| Binary {
        dst,
        op: solve::BinaryOp::Mul,
        lhs,
        rhs,
    };
    let sub = |dst, lhs, rhs| Binary {
        dst,
        op: solve::BinaryOp::Sub,
        lhs,
        rhs,
    };
    let load = |index| vec![LoadY { dst: 0, index }, StoreOutput { src: 0 }];
    let seed = |index| vec![LoadSeed { dst: 0, index }, StoreOutput { src: 0 }];
    let derivative_rhs = solve::ComputeBlock {
        nodes: vec![
            map(0, load(2)),
            map(
                2,
                vec![
                    LoadY { dst: 0, index: 6 },
                    neg(1, 0),
                    StoreOutput { src: 1 },
                ],
            ),
            map(4, load(6)),
        ],
    };
    let mut implicit: Vec<_> = (0..6).map(load).collect();
    let mut implicit_jvp: Vec<_> = (0..6).map(seed).collect();
    for cell in 0..2 {
        implicit.push(vec![
            LoadY {
                dst: 0,
                index: 6 + cell,
            },
            mul(1, 0, 0),
            LoadY {
                dst: 2,
                index: cell,
            },
            sub(3, 1, 2),
            StoreOutput { src: 3 },
        ]);
        implicit_jvp.push(vec![
            LoadY {
                dst: 0,
                index: 6 + cell,
            },
            LoadSeed {
                dst: 1,
                index: 6 + cell,
            },
            mul(2, 0, 1),
            Const { dst: 3, value: 2.0 },
            mul(4, 2, 3),
            LoadSeed {
                dst: 5,
                index: cell,
            },
            sub(6, 4, 5),
            StoreOutput { src: 6 },
        ]);
    }
    let negated_seed = |index| {
        vec![
            LoadSeed { dst: 0, index },
            neg(1, 0),
            StoreOutput { src: 1 },
        ]
    };
    let derivative_jvp = vec![
        seed(2),
        seed(3),
        negated_seed(6),
        negated_seed(7),
        seed(6),
        seed(7),
    ];
    assemble(Fixture {
        names: &["q1", "q2", "w1", "w2", "s1", "s2", "a1", "a2"],
        state_count: 6,
        derivative_rhs,
        implicit: block(implicit, "fmi_me_kinematic_vector_implicit.mo"),
        implicit_jvp: block(implicit_jvp, "fmi_me_kinematic_vector_implicit_jvp.mo"),
        derivative_jvp: block(derivative_jvp, "fmi_me_kinematic_vector_derivative_jvp.mo"),
        blocks: &[(6, 6), (7, 7)],
        initial_y: vec![2.25, 4.0, 0.5, 0.5, 0.0, 0.0, 1.5, 2.0],
    })
}

#[test]
fn tensor_derivative_rows_give_kinematic_derivatives_equal_to_a_full_evaluation() {
    let model = vector_position_algebraic_model();
    let kinematic = [0, 1, 4, 5];
    assert_eq!(
        model.artifacts.continuous.structural.kinematic_states(),
        kinematic
    );
    let mut kernel = instantiate(&model);
    assert_eq!(kernel.kinematic_state_indices(), kinematic);
    kernel.set_time(MeTime::at(0.5)).expect("a legal time");
    kernel
        .set_continuous_states(&[2.25, 4.0, 0.5, -0.25, 0.75, 1.0])
        .expect("finite states");
    let mut full = Vec::new();
    kernel
        .get_continuous_state_derivatives(&mut full)
        .expect("the first point settles");

    let moved = [2.25, 4.0, -1.25, 3.0, 0.75, 1.0];
    kernel.set_continuous_states(&moved).expect("finite states");
    let mut selected = vec![f64::NAN; 6];
    kernel
        .kinematic_state_derivatives_into(&mut selected)
        .expect("the evaluated point is current");
    assert!(selected[2].is_nan() && selected[3].is_nan());

    let mut reference = Vec::new();
    kernel
        .get_continuous_state_derivatives(&mut reference)
        .expect("the moved point settles");
    for state in kinematic {
        assert_eq!(selected[state].to_bits(), reference[state].to_bits());
    }
    assert_eq!(
        [selected[0], selected[1], selected[4], selected[5]],
        [-1.25, 3.0, 1.5, 2.0]
    );
}
