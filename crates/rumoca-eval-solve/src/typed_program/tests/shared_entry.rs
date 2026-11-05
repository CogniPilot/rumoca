use super::*;
use crate::typed_program::eval_shared_entry_checked;
use rumoca_ir_solve::{
    SharedHelperProgramEntry, SolveAssertionLevel, SolveRecursionProfile, SolveRecursiveMember,
    SolveSlotAccess, SolveStorageClass,
};
use std::sync::Arc;

fn division_calls(format: SolveRealFormat) -> Arc<SolvePureCallTable> {
    let arithmetic = profile(format);
    let ty = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    Arc::new(
        SolvePureCallTable::construct(arithmetic, |table| {
            let inner = table.add_owner(
                identity(810),
                vec![ty.clone(); 2],
                vec![SolvePureCallOutput::result(ty.clone())],
                span(810),
                |b, inputs, outputs| {
                    let x = b.load(inputs[0], span(811))?;
                    let y = b.load(inputs[1], span(812))?;
                    let value = b.binary(SolveBinaryOperator::Divide, x, y, span(813))?;
                    b.store(outputs[0], value, span(814))
                },
            )?;
            table.add_owner(
                identity(815),
                vec![ty.clone(); 2],
                vec![SolvePureCallOutput::result(ty.clone())],
                span(815),
                |b, inputs, outputs| {
                    let x = b.load(inputs[0], span(816))?;
                    let y = b.load(inputs[1], span(817))?;
                    let values = b.call(inner, &[x, y], span(818))?;
                    b.store(outputs[0], values[0], span(819))
                },
            )?;
            Ok(())
        })
        .unwrap(),
    )
}

fn mixed_entry(calls: Arc<SolvePureCallTable>) -> SharedHelperProgramEntry {
    let owner = calls.owners()[1].id();
    let ty = SolveValueType::scalar(SolveScalarType::real(calls.arithmetic()));
    SharedHelperProgramEntry::construct(calls, span(820), |b, catalog| {
        // Deliberately interleave declarations: ABI follows issued slot IDs,
        // not the contiguous layout of an ordinary pure-call owner's slots.
        let raw_output = b.declare_slot(
            ty.clone(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(821),
        )?;
        let x = b.declare_slot(
            ty.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(822),
        )?;
        let primal_output = b.declare_slot(
            ty.clone(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(823),
        )?;
        let dx = b.declare_slot(
            ty.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(824),
        )?;
        let y = b.declare_slot(
            ty.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(825),
        )?;
        let dy = b.declare_slot(
            ty.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(826),
        )?;
        let tangent_output = b.declare_slot(
            ty.clone(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(827),
        )?;
        let values = [x, dx, y, dy]
            .map(|slot| b.load(slot, span(828)))
            .into_iter()
            .collect::<Result<Vec<_>, _>>()?;
        let raw = b.emit_entry_call(
            catalog.original(owner, span(829))?,
            &[values[0], values[2]],
            span(830),
        )?;
        let dual = b.emit_entry_call(catalog.directional(owner, span(831))?, &values, span(832))?;
        b.store(raw_output, raw.registers()[0], span(833))?;
        b.store(primal_output, dual.registers()[0], span(834))?;
        b.store(tangent_output, dual.registers()[1], span(835))
    })
    .unwrap()
}

fn real(format: SolveRealFormat, value: f64) -> TypedValue {
    TypedValue::construct(
        SolveValueType::scalar(SolveScalarType::real(profile(format))),
        vec![real_kind(format, value)],
    )
    .unwrap()
}

#[test]
fn mixed_calls_keep_original_and_directional_ieee_results_and_slot_abi() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        let calls = division_calls(format);
        let owner = calls.owners()[1].id();
        let entry = mixed_entry(calls.clone());
        let plan = entry.partition_calls().unwrap();
        for inputs in [[6., 2., 3., 1.], [0., 0., 0., 0.], [-0., 1., 1., 0.]] {
            let arguments = inputs.map(|value| real(format, value));
            let mut expected =
                stopping_call(&calls, owner, &[arguments[0].clone(), arguments[2].clone()])
                    .unwrap();
            expected.extend(stopping_directional_call(&calls, owner, &arguments).unwrap());
            let actual = eval_shared_entry_checked(&plan, &arguments).unwrap();
            assert_eq!(actual, expected);
        }
        assert_eq!(plan.partitions().len(), 5);
    }
}

#[test]
fn mixed_entry_preflights_argument_count_type_and_arithmetic_profile() {
    let entry = mixed_entry(division_calls(SolveRealFormat::Binary64));
    let plan = entry.partition_calls().unwrap();
    let valid = std::array::from_fn::<_, 4, _>(|_| real(SolveRealFormat::Binary64, 1.));
    assert!(matches!(
        eval_shared_entry_checked(&plan, &valid[..3]),
        Err(TypedProgramEvalError::InvalidArgument { index: 3, provenance }) if provenance == span(820)
    ));
    let mut wrong = valid.to_vec();
    wrong[0] = TypedValue::scalar(&SolveValue::boolean(true));
    assert!(matches!(
        eval_shared_entry_checked(&plan, &wrong),
        Err(TypedProgramEvalError::InvalidArgument { index: 0, .. })
    ));
    wrong[0] = real(SolveRealFormat::Binary32, 1.);
    assert!(matches!(
        eval_shared_entry_checked(&plan, &wrong),
        Err(TypedProgramEvalError::InvalidArgument { index: 0, .. })
    ));
}

#[test]
fn lazy_entry_regions_resolve_local_calls_without_executing_inactive_fault() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let integer = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let calls = Arc::new(
        SolvePureCallTable::construct(arithmetic, |table| {
            table.add_owner(
                identity(840),
                vec![integer.clone()],
                vec![SolvePureCallOutput::result(integer.clone())],
                span(840),
                |b, inputs, outputs| {
                    let n = b.load(inputs[0], span(841))?;
                    let zero =
                        b.constant(SolveValue::integer(arithmetic, 0).unwrap(), span(842))?;
                    let result =
                        b.binary(SolveBinaryOperator::IntegerQuotient, n, zero, span(843))?;
                    b.store(outputs[0], result, span(844))
                },
            )?;
            Ok(())
        })
        .unwrap(),
    );
    let owner = calls.owners()[0].id();
    let entry = SharedHelperProgramEntry::construct(calls, span(845), |b, catalog| {
        let condition = b.declare_slot(
            SolveValueType::scalar(SolveScalarType::Boolean),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(846),
        )?;
        let output = b.declare_slot(
            integer.clone(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(847),
        )?;
        let condition = b.load(condition, span(848))?;
        let captured = b.constant(SolveValue::integer(arithmetic, 17).unwrap(), span(849))?;
        let target = catalog.original(owner, span(850))?;
        let value = b.conditional(
            condition,
            &[captured],
            vec![integer.clone()],
            span(851),
            |region, inputs, outputs| {
                let n = region.load(inputs[0], span(852))?;
                region.store(outputs[0], n, span(853))
            },
            |region, inputs, outputs| {
                let target = region.rebind_entry_call_target(target, span(854))?;
                let n = region.load(inputs[0], span(855))?;
                let call = region.emit_entry_call(target, &[n], span(856))?;
                region.store(outputs[0], call.registers()[0], span(857))
            },
        )?;
        b.store(output, value[0], span(858))
    })
    .unwrap();
    let plan = entry.partition_calls().unwrap();
    let result =
        eval_shared_entry_checked(&plan, &[TypedValue::scalar(&SolveValue::boolean(true))])
            .unwrap();
    assert_eq!(result[0].elements(), &[SolveValueKind::Integer(17)]);
    assert!(
        matches!(eval_shared_entry_checked(&plan, &[TypedValue::scalar(&SolveValue::boolean(false))]),
        Err(TypedProgramEvalError::IntegerArithmetic { provenance, .. }) if provenance == span(843))
    );
}

#[test]
fn mixed_entry_preserves_helper_assertion_and_later_fault_order() {
    for level in [SolveAssertionLevel::Error, SolveAssertionLevel::Warning] {
        let (builder, owner) = super::assertions::assertion_then_fault_builder(level, false);
        let calls = Arc::new(builder.finish());
        let argument = TypedValue::scalar(&SolveValue::boolean(false));
        let expected = stopping_call(&calls, owner, std::slice::from_ref(&argument)).unwrap_err();
        let entry = SharedHelperProgramEntry::construct(calls, span(860), |b, catalog| {
            let predicate = b.constant(SolveValue::boolean(false), span(861))?;
            b.emit_entry_call(catalog.original(owner, span(862))?, &[predicate], span(863))?;
            Ok(())
        })
        .unwrap();
        let plan = entry.partition_calls().unwrap();
        assert_eq!(eval_shared_entry_checked(&plan, &[]).unwrap_err(), expected);
    }
}

#[test]
fn equal_foreign_local_program_cannot_resolve_a_partition_call() {
    let calls = division_calls(SolveRealFormat::Binary64);
    let entry = mixed_entry(calls.clone());
    let foreign = mixed_entry(calls);
    let plan = entry.partition_calls().unwrap();
    let call = foreign.call_occurrences().next().unwrap().unwrap();
    let result = crate::typed_program::shared_entry::resolve_entry_call(
        &plan,
        call.program(),
        call.operation_index(),
        span(865),
        crate::typed_program::AssertionPolicy::Stop,
    );
    assert!(
        matches!(result, Err(TypedProgramEvalError::InvalidCheckedProgram {
        operation: "resolve source-bound shared-entry call", provenance,
    }) if provenance == span(865))
    );
}

#[test]
fn mixed_entry_keeps_the_original_recursive_group_limit() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let integer = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let calls = Arc::new(
        SolvePureCallTable::construct(arithmetic, |table| {
            table.add_recursive_group(
                vec![SolveRecursiveMember::new(
                    identity(870),
                    vec![integer.clone()],
                    vec![SolvePureCallOutput::result(integer.clone())],
                    span(870),
                )],
                |_, owners, b, inputs, outputs| {
                    let n = b.load(inputs[0], span(871))?;
                    let next = b.call(owners[0], &[n], span(872))?;
                    b.store(outputs[0], next[0], span(873))
                },
            )?;
            Ok(())
        })
        .unwrap(),
    );
    let owner = calls.owners()[0].id();
    let entry = SharedHelperProgramEntry::construct(calls, span(874), |b, catalog| {
        let n = b.constant(SolveValue::integer(arithmetic, 0).unwrap(), span(875))?;
        b.emit_entry_call(catalog.original(owner, span(876))?, &[n], span(877))?;
        Ok(())
    })
    .unwrap();
    let plan = entry.partition_calls().unwrap();
    assert!(matches!(eval_shared_entry_checked(&plan, &[]),
        Err(TypedProgramEvalError::RecursionDepthExceeded { limit, provenance })
            if limit == SolveRecursionProfile::HOSTED.depth_limit() && provenance == span(872)));
}

#[test]
fn empty_shared_entry_has_empty_output_and_refuses_extra_arguments() {
    let entry = SharedHelperProgramEntry::construct(
        Arc::new(SolvePureCallTable::default()),
        span(880),
        |_, _| Ok(()),
    )
    .unwrap();
    let plan = entry.partition_calls().unwrap();
    assert!(eval_shared_entry_checked(&plan, &[]).unwrap().is_empty());
    assert!(matches!(
        eval_shared_entry_checked(&plan, &[TypedValue::scalar(&SolveValue::boolean(true))]),
        Err(TypedProgramEvalError::InvalidArgument { index: 0, provenance }) if provenance == span(880)
    ));
}

#[test]
fn mixed_tensor_calls_preserve_shape_and_existing_directional_abi() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        let arithmetic = profile(format);
        let ty = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2, 2]).unwrap();
        let calls = Arc::new(
            SolvePureCallTable::construct(arithmetic, |table| {
                table.add_owner(
                    identity(890),
                    vec![ty.clone(); 2],
                    vec![SolvePureCallOutput::result(ty.clone())],
                    span(890),
                    |b, inputs, outputs| {
                        let x = b.load(inputs[0], span(891))?;
                        let y = b.load(inputs[1], span(892))?;
                        let result = b.binary(SolveBinaryOperator::Divide, x, y, span(893))?;
                        b.store(outputs[0], result, span(894))
                    },
                )?;
                Ok(())
            })
            .unwrap(),
        );
        let owner = calls.owners()[0].id();
        let arguments = [
            [6., -4., 0., 8.],
            [1., 2., 0., -1.],
            [3., 2., 1., 4.],
            [0., -1., 1., 2.],
        ]
        .map(|values| {
            TypedValue::construct(
                ty.clone(),
                values.into_iter().map(|v| real_kind(format, v)).collect(),
            )
            .unwrap()
        });
        let mut expected =
            stopping_call(&calls, owner, &[arguments[0].clone(), arguments[2].clone()]).unwrap();
        expected.extend(stopping_directional_call(&calls, owner, &arguments).unwrap());
        let entry = SharedHelperProgramEntry::construct(calls, span(895), |b, catalog| {
            let mut inputs = Vec::new();
            for _ in 0..4 {
                inputs.push(b.declare_slot(
                    ty.clone(),
                    SolveStorageClass::Input,
                    SolveSlotAccess::ReadOnly,
                    span(896),
                )?);
            }
            let mut outputs = Vec::new();
            for _ in 0..3 {
                outputs.push(b.declare_slot(
                    ty.clone(),
                    SolveStorageClass::Output,
                    SolveSlotAccess::ReadWrite,
                    span(897),
                )?);
            }
            let values = inputs
                .into_iter()
                .map(|s| b.load(s, span(898)))
                .collect::<Result<Vec<_>, _>>()?;
            let original = b.emit_entry_call(
                catalog.original(owner, span(899))?,
                &[values[0], values[2]],
                span(900),
            )?;
            let directional =
                b.emit_entry_call(catalog.directional(owner, span(901))?, &values, span(902))?;
            for (output, value) in outputs.into_iter().zip([
                original.registers()[0],
                directional.registers()[0],
                directional.registers()[1],
            ]) {
                b.store(output, value, span(903))?;
            }
            Ok(())
        })
        .unwrap();
        let plan = entry.partition_calls().unwrap();
        let actual = eval_shared_entry_checked(&plan, &arguments).unwrap();
        assert_eq!(actual, expected);
        assert!(actual.iter().all(|value| value.value_type() == &ty));
        assert_eq!(plan.partitions().len(), 3);
    }
}
