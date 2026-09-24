use super::*;
use rumoca_ir_solve::{BinaryOp, TensorIndex};

fn inverse() -> Vec<LinearOp> {
    vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Div,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

fn legacy_indexed() -> Vec<LinearOp> {
    vec![
        LinearOp::Const {
            dst: 0,
            value: 42.0,
        },
        LinearOp::LoadP { dst: 1, index: 1 },
        LinearOp::LoadIndexedRegister {
            dst: 2,
            base: 0,
            stride: 1,
            dimensions: vec![1].into_boxed_slice(),
            indices: vec![TensorIndex::Runtime(1)].into_boxed_slice(),
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

fn typed_index() -> (rumoca_ir_solve::SolvePureCallTable, Vec<LinearOp>) {
    use rumoca_ir_solve as solve;
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
    let vector =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(arithmetic), vec![1]).unwrap();
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("ordered_guard.mo"),
        0,
        1,
    );
    let table = solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![vector, real.clone()],
            vec![solve::SolvePureCallOutput::result(real)],
            at,
            |builder, inputs, outputs| {
                let array = builder.load(inputs[0], at)?;
                let index = builder.load(inputs[1], at)?;
                let index = builder.convert(
                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                    index,
                    at,
                )?;
                let value = builder.project_element_dynamic(array, &[index], at)?;
                builder.store(outputs[0], value, at)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let row = vec![
        LinearOp::Const {
            dst: 0,
            value: 42.0,
        },
        LinearOp::LoadP { dst: 1, index: 1 },
        LinearOp::PureCall {
            dst_start: 2,
            input_starts: vec![0, 1].into(),
            site: table.owners()[0].call_site(),
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    (table, row)
}

#[test]
fn torn_native_declines_before_later_invalid_access_and_rechecks_each_call() {
    let (table, indexed) = typed_index();
    let calls = compile_pure_call_table(&table).unwrap();
    let compiled =
        compile_torn_assignment_schedule(&[inverse(), indexed], &[0, 1], Some(&calls)).unwrap();
    for coefficient in [0.0, 1.0, 0.0, f64::NAN, 2.0] {
        let mut y = [7.0, 9.0];
        let result = compiled.call_torn(&mut y, &[coefficient, 2.0], 0.0, &[]);
        if coefficient == 0.0 || coefficient.is_nan() {
            assert_eq!(result.unwrap(), Some(false));
            assert_eq!(y, [7.0, 9.0], "declining target is not published");
        } else {
            assert!(result.is_err(), "a reached invalid access is an error");
            assert_eq!(y, [1.0 / coefficient, 9.0]);
        }
    }
    let mut y = [7.0, 9.0];
    assert_eq!(
        compiled.call_torn(&mut y, &[2.0, 1.0], 0.0, &[]).unwrap(),
        Some(true)
    );
    assert_eq!(y, [0.5, 42.0]);
}

#[test]
fn torn_native_propagates_error_before_singular_step_and_refuses_legacy_protocol() {
    let (table, indexed) = typed_index();
    let calls = compile_pure_call_table(&table).unwrap();
    let compiled =
        compile_torn_assignment_schedule(&[indexed, inverse()], &[0, 1], Some(&calls)).unwrap();
    let mut y = [7.0, 9.0];
    assert!(compiled.call_torn(&mut y, &[0.0, 2.0], 0.0, &[]).is_err());
    assert_eq!(y, [7.0, 9.0]);
    let legacy = compile_assignment_schedule(&[inverse()], &[0]).unwrap();
    assert_eq!(legacy.call_torn(&mut y, &[0.0], 0.0, &[]).unwrap(), None);
    assert_eq!(
        y,
        [7.0, 9.0],
        "unsupported protocol refuses before execution"
    );
}

#[test]
fn legacy_untyped_index_error_refuses_torn_native_admission() {
    assert!(
        compile_torn_assignment_schedule(&[inverse(), legacy_indexed()], &[0, 1], None).is_err()
    );
}

#[test]
fn tensor_affine_native_isolator_declines_before_later_invalid_access() {
    use rumoca_eval_solve::PreparedScalarProgramBlock;
    use rumoca_ir_solve::{ScalarProgramBlock, TargetAssignmentShape, TensorInputKind};
    let row = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::P,
            input_start: 0,
            seed_start: None,
            count: 3,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: 3,
            input: TensorInputKind::Y,
            input_start: 0,
            seed_start: None,
            count: 3,
            lanes: 1,
        },
        LinearOp::TensorCross {
            dst_start: 6,
            lhs_start: 0,
            rhs_start: 3,
            lanes: 1,
        },
        LinearOp::LoadY { dst: 9, index: 3 },
        LinearOp::Binary {
            dst: 10,
            op: BinaryOp::Sub,
            lhs: 9,
            rhs: 8,
        },
        LinearOp::StoreOutput { src: 10 },
    ];
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("tensor_guard.mo"),
        0,
        1,
    );
    let prepared = PreparedScalarProgramBlock::new(
        ScalarProgramBlock::with_source_span(
            vec![row],
            span.require_provenance("tensor guard").unwrap(),
        )
        .unwrap(),
    )
    .unwrap();
    assert!(matches!(
        rumoca_ir_solve::derive_target_assignment_shape_for_output(
            &prepared.block().programs()[0],
            0,
            1
        ),
        Some(TargetAssignmentShape::TensorAffine { .. })
    ));
    let sweep = prepared.prepare_torn_sweep(&[(0, 1)], &[]).unwrap();
    let mut composite = prepared.torn_sweep_composite(&sweep).unwrap();
    let (table, indexed) = typed_index();
    let calls = compile_pure_call_table(&table).unwrap();
    composite.assignment_rows.push(indexed);
    composite.assignment_targets.push(4);
    let compiled = compile_torn_assignment_schedule(
        &composite.assignment_rows,
        &composite.assignment_targets,
        Some(&calls),
    )
    .unwrap();
    for coefficient in [0.0, 2.0, 0.0] {
        let mut y = [4.0, 99.0, 7.0, 18.0, 23.0];
        let result = compiled.call_torn(&mut y, &[coefficient, 3.0, 5.0], 0.0, &[]);
        if coefficient == 0.0 {
            assert_eq!(result.unwrap(), Some(false));
            assert_eq!(y[1], 99.0);
        } else {
            assert!(result.is_err());
            assert_eq!(y[1], 15.0);
        }
        assert_eq!(y[4], 23.0);
    }
}

#[test]
fn nonlinear_isolator_rechecks_tear_coefficient_at_each_trial() {
    use rumoca_eval_solve::PreparedScalarProgramBlock;
    use rumoca_ir_solve::ScalarProgramBlock;
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 2 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Const { dst: 3, value: 1.0 },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 2,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("nonlinear_trial.mo"),
        0,
        1,
    );
    let prepared = PreparedScalarProgramBlock::new(
        ScalarProgramBlock::with_source_span(vec![row], span.require_provenance("trial").unwrap())
            .unwrap(),
    )
    .unwrap();
    let sweep = prepared.prepare_torn_sweep(&[(0, 0)], &[]).unwrap();
    let composite = prepared.torn_sweep_composite(&sweep).unwrap();
    let compiled = compile_torn_assignment_schedule(
        &composite.assignment_rows,
        &composite.assignment_targets,
        None,
    )
    .unwrap();
    for z in [1.0, 0.0, 2.0, 0.0] {
        let mut y = [7.0, 9.0, z];
        let completed = compiled.call_torn(&mut y, &[], 0.0, &[]).unwrap();
        assert_eq!(completed, Some(z != 0.0));
        assert_eq!(y[0], if z == 0.0 { 7.0 } else { 1.0 / z });
    }
}
