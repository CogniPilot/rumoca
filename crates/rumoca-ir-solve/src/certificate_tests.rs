use rumoca_core::{SourceId, Span};

use crate::*;

fn block(programs: Vec<Vec<LinearOp>>) -> ScalarProgramBlock {
    let spans = vec![
        Span::from_offsets(SourceId::from_source_name("certificate_test.mo"), 0, 1);
        programs.len()
    ];
    ScalarProgramBlock::with_program_spans(programs, spans).expect("valid test programs")
}

fn load_p(index: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadP { dst: 0, index },
        LinearOp::StoreOutput { src: 0 },
    ]
}

#[test]
fn only_the_canonical_initial_mode_can_hide_a_nested_relation_effect() {
    let comparison = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::Compare {
            dst: 2,
            op: CompareOp::Gt,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    for (guard, initial_mode, fallback, expected) in [
        (
            load_p(7),
            Some(7),
            load_p(8),
            RuntimeAssignmentRole::RelationFree,
        ),
        (
            load_p(6),
            Some(7),
            load_p(8),
            RuntimeAssignmentRole::RelationEvaluating,
        ),
        (
            load_p(7),
            None,
            load_p(8),
            RuntimeAssignmentRole::RelationEvaluating,
        ),
        (
            load_p(7),
            Some(7),
            comparison.clone(),
            RuntimeAssignmentRole::RelationEvaluating,
        ),
        (
            comparison.clone(),
            Some(7),
            load_p(8),
            RuntimeAssignmentRole::RelationEvaluating,
        ),
    ] {
        let conditional = FunctionConditionalProgram::checked(
            0,
            vec![1],
            vec![(guard, comparison.clone())],
            fallback,
        )
        .unwrap();
        let rhs = block(vec![vec![
            LinearOp::FunctionConditional {
                dst_start: 0,
                capture_start: 0,
                program: std::sync::Arc::new(conditional),
            },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        assert_eq!(
            derive_runtime_assignment_roles(&rhs, &[scalar_slot_p(9)], &[8], initial_mode).unwrap(),
            [expected],
        );
    }
}

#[test]
fn a_runtime_assignment_cannot_borrow_a_jacobian_seed_owner() {
    for dst in [0, 1] {
        let rhs = block(vec![vec![
            LinearOp::LoadP { dst: 0, index: 7 },
            LinearOp::LoadSeed { dst, index: 3 },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        assert!(
            derive_runtime_assignment_roles(&rhs, &[scalar_slot_p(9)], &[8], Some(7))
                .unwrap_err()
                .to_string()
                .contains("dependency program is not certifiable")
        );
    }
}

#[test]
fn an_opaque_typed_call_cannot_authorize_relation_free_replay() {
    let span = Span::from_offsets(SourceId::from_source_name("opaque_call.mo"), 0, 1);
    let profile =
        SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL);
    let table = SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![],
            vec![SolvePureCallOutput::result(SolveValueType::scalar(
                SolveScalarType::real(profile),
            ))],
            span,
            |body, _, outputs| {
                let value = body.constant(SolveValue::real(profile, 1.0), span)?;
                body.store(outputs[0], value, span)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let rhs = block(vec![vec![
        LinearOp::PureCall {
            dst_start: 0,
            input_starts: vec![].into(),
            site: table.owners()[0].call_site(),
        },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    assert_eq!(
        derive_runtime_assignment_roles(&rhs, &[scalar_slot_p(1)], &[], Some(2)).unwrap(),
        [RuntimeAssignmentRole::RelationEvaluating],
    );
}

#[test]
fn runtime_roles_prove_direct_and_transitive_relation_dependencies() {
    let rhs = block(vec![
        vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::Const { dst: 1, value: 0.0 },
            LinearOp::Compare {
                dst: 2,
                op: CompareOp::Gt,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ],
        load_p(1),
        load_p(3),
    ]);
    let roles = derive_runtime_assignment_roles(
        &rhs,
        &[scalar_slot_p(1), scalar_slot_p(2), scalar_slot_p(4)],
        &[0],
        None,
    )
    .expect("base runtime programs are certifiable");
    assert_eq!(
        roles,
        [
            RuntimeAssignmentRole::RelationEvaluating,
            RuntimeAssignmentRole::RelationEvaluating,
            RuntimeAssignmentRole::RelationFree,
        ]
    );
}

#[test]
fn root_roles_prove_indirect_algebraic_dependency_and_frozen_negative() {
    let runtime = block(vec![
        vec![
            LinearOp::LoadY { dst: 0, index: 1 },
            LinearOp::StoreOutput { src: 0 },
        ],
        load_p(1),
    ]);
    let roots = block(vec![load_p(2), load_p(3)]);
    let roles = derive_root_relation_refresh_roles(
        &roots,
        &runtime,
        &[scalar_slot_p(1), scalar_slot_p(2)],
        1,
        2,
    )
    .expect("base root programs are certifiable");
    assert_eq!(
        roles,
        [
            RootRelationRefreshRole::AlgebraicDependent,
            RootRelationRefreshRole::Frozen,
        ]
    );
}

#[test]
fn unsupported_dependency_program_cannot_issue_a_certificate() {
    let rhs = block(vec![vec![
        LinearOp::LoadSeed { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    assert!(derive_runtime_assignment_roles(&rhs, &[scalar_slot_p(0)], &[], None).is_err());

    let unsupported_impure_programs = [
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::ImpureRandomInit { dst: 1, seed: 0 },
            LinearOp::StoreOutput { src: 1 },
        ],
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::ImpureRandom {
                dst: 1,
                id: 0,
                call_site: 7,
            },
            LinearOp::StoreOutput { src: 1 },
        ],
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::Const {
                dst: 1,
                value: -2.0,
            },
            LinearOp::Const { dst: 2, value: 2.0 },
            LinearOp::ImpureRandomInteger {
                dst: 3,
                id: 0,
                imin: 1,
                imax: 2,
                call_site: 11,
            },
            LinearOp::StoreOutput { src: 3 },
        ],
    ];
    for program in unsupported_impure_programs {
        let impure = block(vec![program]);
        assert!(derive_runtime_assignment_roles(&impure, &[scalar_slot_p(0)], &[], None).is_err());
    }
}

#[test]
fn post_commit_reachability_does_not_cross_relation_evaluation() {
    let rhs = block(vec![
        vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::Const { dst: 1, value: 0.0 },
            LinearOp::Compare {
                dst: 2,
                op: CompareOp::Gt,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ],
        load_p(1),
    ]);
    let targets = [scalar_slot_p(1), scalar_slot_p(2)];
    let roles = derive_runtime_assignment_roles(&rhs, &targets, &[], None)
        .expect("base programs are certifiable");
    assert_eq!(
        roles,
        [
            RuntimeAssignmentRole::RelationEvaluating,
            RuntimeAssignmentRole::RelationEvaluating,
        ]
    );
    assert_eq!(
        derive_root_reachable_runtime_rows(&rhs, &targets, &[Some(scalar_slot_p(0))], &roles,)
            .expect("reachability is certifiable"),
        [false, false]
    );
}
