use super::*;
use crate::{
    BinaryOp, PatternDerivation, PatternProvenance, StructuralPattern, TensorNodeMetadata,
};
use rumoca_core::{SourceId, Span};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("coupled_mechanics.mo"), 1, 2)
}

fn fixture() -> ComputeBlock {
    let positions = ScalarProgramBlock::with_program_spans(
        (2..4)
            .map(|index| {
                vec![
                    LinearOp::LoadY { dst: 0, index },
                    LinearOp::StoreOutput { src: 0 },
                ]
            })
            .collect(),
        vec![span(); 2],
    )
    .unwrap();
    let mut setup: Vec<_> = [2.0, 0.25, 0.25, 3.0]
        .into_iter()
        .enumerate()
        .map(|(dst, value)| LinearOp::Const {
            dst: dst as u32,
            value,
        })
        .collect();
    for row in 0..2u32 {
        setup.extend([
            LinearOp::LoadP {
                dst: 4 + row,
                index: row as usize,
            },
            LinearOp::LoadP {
                dst: 6 + row,
                index: 2 + row as usize,
            },
            LinearOp::LoadY {
                dst: 8 + row,
                index: 2 + row as usize,
            },
            LinearOp::Binary {
                dst: 10 + row,
                op: BinaryOp::Mul,
                lhs: 6 + row,
                rhs: 8 + row,
            },
            LinearOp::Binary {
                dst: 12 + row,
                op: BinaryOp::Sub,
                lhs: 4 + row,
                rhs: 10 + row,
            },
        ]);
    }
    ComputeBlock {
        nodes: vec![
            ComputeNode::ScalarPrograms(positions),
            ComputeNode::LinSolve {
                setup_ops: setup,
                matrix_start: 0,
                rhs_start: 12,
                n: 2,
                next_reg: 14,
                matrix_pattern: StructuralPattern::full(
                    2,
                    2,
                    PatternProvenance::derived(PatternDerivation::TensorOperand, span()).unwrap(),
                )
                .unwrap(),
                metadata: TensorNodeMetadata::default(),
                span: span(),
            },
        ],
    }
}

fn plan(source: &ComputeBlock) -> Result<MechanicalStepPlan, MechanicalStepError> {
    let pairs = CartesianPairPlan::checked(4, vec![[0, 2], [1, 3]], source)
        .map_err(|e| refused(e.to_string()))?;
    MechanicalStepPlan::checked(pairs, 1, source)
}

#[test]
fn affine_identity_map_keeps_source_indices_and_refuses_extra_work() {
    use crate::{AffineStencilIndexStrideTerm, AffineStencilLoadStride, TensorOutputMap};
    use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder, StructuredIndexDomain};
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 3,
            upper: 5,
            step: 2,
        }],
    };
    let mut source = fixture();
    source.nodes[0] = ComputeNode::Map {
        output_map: TensorOutputMap::dense_contiguous(0, &domain).unwrap(),
        domain,
        base_ops: vec![
            LinearOp::LoadY { dst: 0, index: 2 },
            LinearOp::StoreOutput { src: 0 },
        ],
        load_strides: vec![AffineStencilLoadStride {
            op_position: 0,
            terms: vec![AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            }],
        }],
        const_strides: vec![],
        metadata: TensorNodeMetadata::default(),
        span: span(),
    };
    assert!(plan(&source).is_ok());
    for change in 0..4 {
        let mut bad = source.clone();
        let ComputeNode::Map {
            base_ops,
            load_strides,
            output_map,
            ..
        } = &mut bad.nodes[0]
        else {
            unreachable!()
        };
        match change {
            0 => load_strides[0].terms[0].stride = 2,
            1 => output_map.strides.clear(),
            2 => base_ops.push(LinearOp::LoadY { dst: 1, index: 99 }),
            _ => base_ops.push(LinearOp::Const { dst: 1, value: 0.0 }),
        }
        assert!(plan(&bad).is_err(), "case {change}");
    }
}

#[test]
fn coupled_matrix_and_force_setup_remain_one_exact_source() {
    let source = fixture();
    let plan = plan(&source).unwrap();
    assert_eq!(
        plan.damping(),
        [
            MechanicalDampingSource::Parameter { index: 2 },
            MechanicalDampingSource::Parameter { index: 3 }
        ]
    );
    let coefficients = plan.coefficient_program(&source).unwrap();
    let ComputeNode::LinSolve { setup_ops, .. } = &source.nodes[1] else {
        unreachable!()
    };
    let original = bincode::serialize(setup_ops).unwrap();
    assert_eq!(
        bincode::serialize(&coefficients.programs()[0][..setup_ops.len()]).unwrap(),
        original
    );
    assert_eq!(coefficients.stored_output_count(), 8);
    assert_eq!(coefficients.program_spans(), [span()]);
}

#[test]
fn damping_requires_its_original_velocity_and_reaching_coefficient() {
    for change in 0..3 {
        let mut source = fixture();
        let ComputeNode::LinSolve { setup_ops, .. } = &mut source.nodes[1] else {
            unreachable!()
        };
        match change {
            0 => setup_ops[6] = LinearOp::LoadY { dst: 8, index: 3 },
            1 => setup_ops.insert(
                7,
                LinearOp::Const {
                    dst: 6,
                    value: -1.0,
                },
            ),
            _ => setup_ops.push(LinearOp::Const {
                dst: 12,
                value: 0.0,
            }),
        }
        assert!(plan(&source).is_err());
    }
}

#[test]
fn mechanical_admission_cannot_drop_a_kinematic_check_or_choose_another_owner() {
    let mut source = fixture();
    let pairs = CartesianPairPlan::checked(4, vec![[0, 2], [1, 3]], &source).unwrap();
    assert!(MechanicalStepPlan::checked(pairs.clone(), 0, &source).is_err());
    let ComputeNode::ScalarPrograms(rows) = &source.nodes[0] else {
        unreachable!()
    };
    let mut programs = rows.programs().to_vec();
    programs[0].insert(0, LinearOp::Const { dst: 1, value: 7.0 });
    source.nodes[0] = ComputeNode::ScalarPrograms(
        ScalarProgramBlock::with_program_spans(programs, vec![span(); 2]).unwrap(),
    );
    assert!(MechanicalStepPlan::checked(pairs, 1, &source).is_err());
}

#[test]
fn mechanical_root_replay_rechecks_damping_source_and_current_wire() {
    let source = fixture();
    let layout = crate::VarLayout::from_parts(
        (0..4)
            .map(|index| (format!("x{index}"), crate::scalar_slot_y(index)))
            .chain((0..4).map(|index| (format!("p{index}"), crate::layout::scalar_slot_p(index))))
            .collect(),
        4,
        4,
    );
    let mut problem = crate::SolveProblem::with_derivative_rhs(source, layout).unwrap();
    let mechanical = plan(&problem.continuous.derivative_rhs).unwrap();
    problem.continuous.cartesian_pairs = Some(mechanical.cartesian().clone());
    problem.continuous.mechanical_step = Some(mechanical);
    let wire = serde_json::to_value(&problem).unwrap();
    let replay: crate::SolveProblem = serde_json::from_value(wire.clone()).unwrap();
    assert_eq!(
        replay.continuous.mechanical_step,
        problem.continuous.mechanical_step
    );
    let binary: crate::SolveProblem =
        bincode::deserialize(&bincode::serialize(&problem).unwrap()).unwrap();
    assert_eq!(
        binary.continuous.mechanical_step,
        problem.continuous.mechanical_step
    );
    let mut forged = wire.clone();
    forged["continuous"]["mechanical_step"]["damping"][0]["parameter"]["index"] = 99.into();
    assert!(serde_json::from_value::<crate::SolveProblem>(forged).is_err());
    let mut old = wire;
    old["schema_version"] = 90.into();
    assert!(serde_json::from_value::<crate::SolveProblem>(old).is_err());
}
