use super::*;

pub(crate) fn guarded_site() -> crate::SolvePureCallSite {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    let real = crate::SolveValueType::scalar(crate::SolveScalarType::real(profile));
    let span = crate::source_span_from_offsets(812, 1, 2);
    let table = crate::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            crate::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(812).unwrap()),
            vec![real.clone(), real.clone()],
            vec![crate::SolvePureCallOutput::result(real)],
            span,
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span)?;
                let q = b.load(inputs[1], span)?;
                b.check_finite(q, crate::SolveFiniteCheck::Derivative, span)?;
                b.store(outputs[0], x, span)
            },
        )?;
        Ok(())
    })
    .unwrap();
    table.owners()[0].call_site()
}

pub(crate) fn guarded_program() -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: 3 },
        LinearOp::LoadP { dst: 1, index: 7 },
        LinearOp::PureCall {
            dst_start: 2,
            input_starts: vec![0, 1].into(),
            site: guarded_site(),
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

#[test]
fn execution_policy_keeps_the_guard_parameter_without_changing_values() {
    let program = guarded_program();
    assert_eq!(
        StructuralPattern::derive_p_dependency_union(&program, None).unwrap(),
        BTreeSet::new()
    );
    assert_eq!(
        StructuralPattern::derive_execution_p_dependency_union(&program, None).unwrap(),
        BTreeSet::from([7])
    );
    assert_eq!(
        StructuralPattern::derive_y_dependency_union(&program, None).unwrap(),
        BTreeSet::from([3])
    );
    assert_eq!(
        StructuralPattern::derive_execution_y_dependency_union(&program, None).unwrap(),
        BTreeSet::from([3])
    );
}

#[test]
fn an_unread_call_retains_its_guard_but_not_its_unused_result_reads() {
    let mut program = guarded_program();
    program.pop();
    program.extend([
        LinearOp::Const { dst: 3, value: 1.0 },
        LinearOp::StoreOutput { src: 3 },
    ]);
    assert!(
        StructuralPattern::derive_y_dependency_union(&program, None)
            .unwrap()
            .is_empty()
    );
    assert!(
        StructuralPattern::derive_execution_y_dependency_union(&program, None)
            .unwrap()
            .is_empty()
    );
    assert_eq!(
        StructuralPattern::derive_execution_p_dependency_union(&program, None).unwrap(),
        BTreeSet::from([7])
    );
}

#[test]
fn guard_reads_follow_time_and_directional_inputs() {
    let mut ordinary = guarded_program();
    ordinary[1] = LinearOp::LoadTime { dst: 1 };
    assert!(
        !StructuralPattern::derive_output_time_dependencies(&ordinary, None)
            .unwrap()
            .contains(&true)
    );
    assert!(StructuralPattern::execution_reads_time(&ordinary, None).unwrap());
    let site = guarded_site().directional().unwrap().clone();
    let directional = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::LoadP { dst: 2, index: 7 },
        LinearOp::Const { dst: 3, value: 0.0 },
        LinearOp::PureCallDirectional {
            dst_start: 4,
            input_starts: vec![0, 1, 2, 3].into(),
            site,
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    assert!(
        StructuralPattern::derive_p_dependency_union(&directional, None)
            .unwrap()
            .is_empty()
    );
    assert_eq!(
        StructuralPattern::derive_execution_p_dependency_union(&directional, None).unwrap(),
        BTreeSet::from([7])
    );
}

#[test]
fn a_guard_in_a_lazy_scalar_branch_retains_its_capture_and_control() {
    let branch = vec![
        LinearOp::LoadFunctionConditionalCaptureRange {
            dst_start: 0,
            index_start: 0,
            count: 2,
        },
        LinearOp::PureCall {
            dst_start: 2,
            input_starts: vec![0, 1].into(),
            site: guarded_site(),
        },
        LinearOp::StoreOutput { src: 0 },
    ];
    let program = crate::FunctionConditionalProgram::checked(
        3,
        vec![1],
        vec![(
            vec![
                LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 2 },
                LinearOp::StoreOutput { src: 0 },
            ],
            branch,
        )],
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .unwrap();
    let ops = [
        LinearOp::LoadY { dst: 0, index: 3 },
        LinearOp::LoadP { dst: 1, index: 7 },
        LinearOp::LoadP { dst: 2, index: 9 },
        LinearOp::FunctionConditional {
            dst_start: 3,
            capture_start: 0,
            program: Arc::new(program),
        },
        LinearOp::StoreOutput { src: 3 },
    ];
    assert_eq!(
        StructuralPattern::derive_p_dependency_union(&ops, None).unwrap(),
        BTreeSet::from([9])
    );
    assert_eq!(
        StructuralPattern::derive_execution_p_dependency_union(&ops, None).unwrap(),
        BTreeSet::from([7, 9])
    );
}

#[test]
fn shared_prefix_proof_preserves_earlier_guards_across_refused_suffixes() {
    let mut program = guarded_program();
    let before = 2;
    let after = 3;
    program[1] = LinearOp::LoadY { dst: 1, index: 7 };
    program.push(LinearOp::Move { dst: 5, src: 99 });
    let proof = ScalarProgramExecutionYDependency::new(&program);
    assert!(proof.prefix(before).unwrap().is_empty());
    assert_eq!(proof.prefix(after).unwrap().iter().collect::<Vec<_>>(), [7]);
    assert!(proof.prefix(program.len()).is_none());
}

#[test]
fn a_guard_in_a_fold_update_retains_the_fold_captures_without_value_edges() {
    let update = vec![
        LinearOp::LoadFoldCarried { dst: 0, index: 0 },
        LinearOp::LoadFoldCapture { dst: 1, index: 0 },
        LinearOp::PureCall {
            dst_start: 2,
            input_starts: vec![0, 1].into(),
            site: guarded_site(),
        },
        LinearOp::StoreOutput { src: 0 },
    ];
    let domain = StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: 2,
            step: 1,
        }],
    };
    let program = crate::FunctionFoldProgram::checked(domain, 1, 1, update).unwrap();
    let ops = [
        LinearOp::LoadY { dst: 0, index: 3 },
        LinearOp::LoadP { dst: 1, index: 7 },
        LinearOp::FunctionFold {
            dst_start: 2,
            initial_start: 0,
            capture_start: 1,
            program: Arc::new(program),
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    assert!(
        StructuralPattern::derive_p_dependency_union(&ops, None)
            .unwrap()
            .is_empty()
    );
    assert_eq!(
        StructuralPattern::derive_y_dependency_union(&ops, None).unwrap(),
        BTreeSet::from([3])
    );
    assert_eq!(
        StructuralPattern::derive_execution_p_dependency_union(&ops, None).unwrap(),
        BTreeSet::from([7])
    );
}

#[test]
fn an_unused_failable_scalar_operation_retains_its_operands() {
    // A one-by-one solve can fail on a singular matrix whether or not any
    // output uses its component.
    let ops = [
        LinearOp::LoadY { dst: 0, index: 4 },
        LinearOp::LoadP { dst: 1, index: 7 },
        LinearOp::LinearSolveComponent {
            dst: 2,
            matrix_start: 0,
            rhs_start: 1,
            n: 1,
            component: 0,
        },
        LinearOp::Const { dst: 3, value: 1.0 },
        LinearOp::StoreOutput { src: 3 },
    ];
    assert!(
        StructuralPattern::derive_y_dependency_union(&ops, None)
            .unwrap()
            .is_empty()
    );
    assert_eq!(
        StructuralPattern::derive_execution_y_dependency_union(&ops, None).unwrap(),
        BTreeSet::from([4])
    );
    assert_eq!(
        StructuralPattern::derive_execution_p_dependency_union(&ops, None).unwrap(),
        BTreeSet::from([7])
    );
}
