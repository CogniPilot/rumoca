use super::*;

fn multiplied_values() -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

#[test]
fn block_affinity_requires_one_independent_product_operand() {
    let program = multiplied_values();
    assert_eq!(
        program_degree(&program, &BTreeSet::from([0]), 0),
        Some(Degree::Affine)
    );
    assert_eq!(
        program_degree(&program, &BTreeSet::from([1]), 0),
        Some(Degree::Affine)
    );
    assert_eq!(
        program_degree(&program, &BTreeSet::from([0, 1]), 0),
        Some(Degree::Nonlinear)
    );
    assert_eq!(
        program_degree(&program, &BTreeSet::new(), 0),
        Some(Degree::Independent)
    );
}

#[test]
fn block_affinity_uses_the_selected_logical_output() {
    let mut program = multiplied_values();
    program.push(LinearOp::StoreOutputRange {
        start: 0,
        count: 2,
        stride: 1,
    });
    let targets = BTreeSet::from([0, 1]);
    assert_eq!(
        program_degree(&program, &targets, 0),
        Some(Degree::Nonlinear)
    );
    assert_eq!(program_degree(&program, &targets, 1), Some(Degree::Affine));
    assert_eq!(program_degree(&program, &targets, 2), Some(Degree::Affine));
    assert_eq!(program_degree(&program, &targets, 3), None);
}

#[test]
fn compact_tensor_products_certify_only_independent_coefficients() {
    let program = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 9,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: 9,
            input: TensorInputKind::Y,
            input_start: 9,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::MatrixMultiply {
            dst_start: 12,
            lhs_start: 0,
            rhs_start: 9,
            rows: 3,
            inner: 3,
            columns: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 12,
            count: 3,
            stride: 1,
        },
    ];
    assert_eq!(
        program_degree(&program, &BTreeSet::from([9, 10, 11]), 1),
        Some(Degree::Affine)
    );
    assert_eq!(
        program_degree(&program, &BTreeSet::from([0, 9]), 1),
        Some(Degree::Nonlinear)
    );
}

#[test]
fn compact_output_selection_does_not_expand_a_large_tensor() {
    let program = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 1_000_000,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 0,
            count: 1_000_000,
            stride: 1,
        },
    ];
    assert_eq!(
        program_degree(&program, &BTreeSet::from([999_999]), 999_999),
        Some(Degree::Affine)
    );
}

#[test]
fn seed_inputs_and_nonfinite_constants_do_not_issue_a_proof() {
    let mut program = multiplied_values();
    program[0] = LinearOp::Const {
        dst: 0,
        value: f64::NAN,
    };
    assert_eq!(program_degree(&program, &BTreeSet::new(), 0), None);
    program[0] = LinearOp::LoadSeed { dst: 0, index: 0 };
    assert_eq!(program_degree(&program, &BTreeSet::new(), 0), None);
}

#[test]
fn affine_profiles_follow_output_indices_and_refuse_conflicting_block_ids() {
    let mut program = multiplied_values();
    program.extend([
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutput { src: 1 },
    ]);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("block_affinity.mo"),
        1,
        2,
    );
    let source = ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_output_indices(vec![program], vec![span], vec![1, 0, 2]).unwrap(),
    );
    let mut plan = RefreshPlan {
        simultaneous_plan: crate::AlgebraicProjectionPlan {
            blocks: vec![
                crate::AlgebraicProjectionBlock {
                    rows: vec![0, 2],
                    y_indices: vec![0, 1],
                    tearing: None,
                },
                crate::AlgebraicProjectionBlock {
                    rows: vec![1, 2],
                    y_indices: vec![0, 1],
                    tearing: None,
                },
            ],
        },
        simultaneous_block_indices: vec![10, 11],
        ..Default::default()
    };
    assert_eq!(
        projection_affinities(&source, &plan),
        BTreeMap::from([(10, true), (11, false)])
    );
    plan.simultaneous_block_indices = vec![10, 10];
    assert_eq!(
        projection_affinities(&source, &plan),
        BTreeMap::from([(10, false)])
    );
}

#[test]
fn affinity_uses_compute_block_output_identity_across_program_nodes() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("block_affinity.mo"),
        1,
        2,
    );
    let mut nonlinear = multiplied_values();
    nonlinear[1] = LinearOp::LoadY { dst: 1, index: 0 };
    let linear = vec![
        LinearOp::LoadY { dst: 0, index: 1 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let mut source = ComputeBlock {
        nodes: [nonlinear, linear]
            .into_iter()
            .map(|program| {
                ComputeNode::ScalarPrograms(
                    ScalarProgramBlock::with_output_indices(vec![program], vec![span], vec![0])
                        .unwrap(),
                )
            })
            .collect(),
    };
    assert_eq!(
        source.produced_output_indices("affinity test").unwrap(),
        vec![0, 1]
    );
    let plan = RefreshPlan {
        simultaneous_plan: crate::AlgebraicProjectionPlan {
            blocks: (0..2)
                .map(|index| crate::AlgebraicProjectionBlock {
                    rows: vec![index],
                    y_indices: vec![index],
                    tearing: None,
                })
                .collect(),
        },
        simultaneous_block_indices: vec![0, 1],
        ..Default::default()
    };
    assert_eq!(
        projection_affinities(&source, &plan),
        BTreeMap::from([(0, false), (1, true)])
    );
    for node in &mut source.nodes {
        let ComputeNode::ScalarPrograms(block) = node else {
            unreachable!();
        };
        *block =
            ScalarProgramBlock::with_output_indices(block.programs().to_vec(), vec![span], vec![1])
                .unwrap();
    }
    assert!(projection_affinities(&source, &plan).is_empty());
}
