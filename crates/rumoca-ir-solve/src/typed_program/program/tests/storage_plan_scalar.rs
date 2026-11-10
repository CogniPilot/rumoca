use super::*;
use crate::{SolveFiniteCheck, SolveRegisterHome};

#[derive(Clone, Copy)]
enum Reader {
    Scalar,
    Store,
    Aggregate,
    Branch,
    Finite,
}

fn real() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::real(profile()))
}

fn program(reader: Reader) -> TypedProgram {
    TypedProgram::construct(profile(), |b| {
        let input = b.declare_slot(
            real(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(1),
        )?;
        let output = b.declare_slot(
            real(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(2),
        )?;
        let local = b.declare_slot(
            real(),
            SolveStorageClass::MethodLocal,
            SolveSlotAccess::ReadWrite,
            span(3),
        )?;
        let x = b.load(input, span(4))?;
        let temporary = b.unary(SolveUnaryOperator::Exp, x, span(5))?;
        match reader {
            Reader::Store => b.store(local, temporary, span(6))?,
            Reader::Aggregate => {
                let vector = b.fill(temporary, vec![4], span(6))?;
                let negated = b.unary(SolveUnaryOperator::Negate, vector, span(7))?;
                b.reduce(SolveReductionOperator::Sum, negated, span(8))?;
            }
            Reader::Branch => {
                let condition = b.constant(SolveValue::boolean(true), span(6))?;
                b.conditional(
                    condition,
                    &[],
                    vec![],
                    span(7),
                    |_, _, _| Ok(()),
                    |_, _, _| Ok(()),
                )?;
            }
            Reader::Finite => b.check_finite(temporary, SolveFiniteCheck::Derivative, span(6))?,
            Reader::Scalar => {}
        }
        let result = b.unary(SolveUnaryOperator::Sin, temporary, span(9))?;
        b.store(output, result, span(10))
    })
    .unwrap()
}

#[test]
fn a_scalar_lifetime_has_a_definition_reader_and_no_frame_cell() {
    for reader in [Reader::Scalar, Reader::Finite] {
        let p = program(reader);
        let SolveRegisterHome::Scalar { lifetime } = p.storage_plan().aliases().registers()[1]
        else {
            panic!("the scalar interval needs no reload");
        };
        assert_eq!(lifetime.definition(), 1);
        assert_eq!(Some(lifetime.last_read()), p.register_last_reads()[1]);
        assert_eq!(p.storage_plan().cells().offsets()[1], None);
        assert_eq!(p.storage_plan().cells().extent(), 0);
    }
}

#[test]
fn a_nonlast_store_or_aggregate_reader_requires_a_home() {
    for reader in [Reader::Store, Reader::Aggregate] {
        let p = program(reader);
        assert_eq!(
            p.storage_plan().aliases().registers()[1],
            SolveRegisterHome::Own
        );
        assert!(p.storage_plan().cells().offsets()[1].is_some());
        assert!(p.register_last_reads()[1].unwrap() > 2);
    }
}

#[test]
fn a_definition_live_across_a_branch_keeps_its_frame() {
    let p = program(Reader::Branch);
    assert!(matches!(
        p.operations()[3].operation(),
        SolveOperation::Conditional { .. }
    ));
    assert!(p.register_last_reads()[1].unwrap() > 3);
    assert_eq!(
        p.storage_plan().aliases().registers()[1],
        SolveRegisterHome::Own
    );
    assert!(p.storage_plan().cells().offsets()[1].is_some());
}

#[test]
fn a_fused_consumer_extends_the_addressable_input_lifetime() {
    let p = program(Reader::Aggregate);
    let plan = p.storage_plan();
    assert!(plan.fusion().registers().iter().any(Option::is_some));
    assert!(plan.fusion().last_reads()[1] >= p.register_last_reads()[1]);
    assert_eq!(plan.aliases().registers()[1], SolveRegisterHome::Own);
    assert!(plan.cells().offsets()[1].is_some());
}

#[test]
fn call_arguments_are_addressable_even_when_a_later_scalar_reads_them() {
    let identity =
        |id| crate::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(id).unwrap());
    let table = crate::SolvePureCallTable::construct(profile(), |t| {
        let callee = t.add_owner(
            identity(701),
            vec![real()],
            vec![crate::SolvePureCallOutput::result(real())],
            span(1),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(2))?;
                b.store(outputs[0], x, span(3))
            },
        )?;
        t.add_owner(
            identity(702),
            vec![real()],
            vec![crate::SolvePureCallOutput::result(real())],
            span(4),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(5))?;
                let temporary = b.unary(SolveUnaryOperator::Exp, x, span(6))?;
                let called = b.call(callee, &[temporary], span(7))?;
                let later = b.unary(SolveUnaryOperator::Sin, temporary, span(8))?;
                let result = b.binary(SolveBinaryOperator::Add, called[0], later, span(9))?;
                b.store(outputs[0], result, span(10))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let p = table.owners()[1].body();
    assert_eq!(
        p.storage_plan().aliases().registers()[1],
        SolveRegisterHome::Own
    );
    assert!(p.storage_plan().cells().offsets()[1].is_some());
}

#[test]
fn a_dead_addressable_value_can_share_cells_without_aliasing_scalar_values() {
    let p = TypedProgram::construct(profile(), |b| {
        let input = b.declare_slot(
            real(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(1),
        )?;
        let output = b.declare_slot(
            real(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(2),
        )?;
        let local = b.declare_slot(
            real(),
            SolveStorageClass::MethodLocal,
            SolveSlotAccess::ReadWrite,
            span(3),
        )?;
        let x = b.load(input, span(4))?;
        let a = b.unary(SolveUnaryOperator::Exp, x, span(5))?;
        b.store(local, a, span(6))?;
        let a = b.unary(SolveUnaryOperator::Sin, a, span(7))?;
        let b_value = b.unary(SolveUnaryOperator::Cos, x, span(8))?;
        b.store(local, b_value, span(9))?;
        let b_value = b.unary(SolveUnaryOperator::Sin, b_value, span(10))?;
        let result = b.binary(SolveBinaryOperator::Add, a, b_value, span(11))?;
        b.store(output, result, span(12))
    })
    .unwrap();
    let offsets = p.storage_plan().cells().offsets();
    assert!(offsets[1].is_some());
    assert_eq!(offsets[1], offsets[3]);
    assert!(matches!(
        p.storage_plan().aliases().registers()[4],
        SolveRegisterHome::Scalar { .. }
    ));
}

#[test]
fn wire_claims_cannot_change_a_scalar_lifetime() {
    let p = program(Reader::Scalar);
    let mut wire = serde_json::to_value(&p).unwrap();
    assert!(
        wire["storage"]["aliases"]["homes"][1]
            .get("lifetime")
            .is_none()
    );
    wire["storage"] = serde_json::json!({"aliases":{"homes":[{"home":"own"}]},"cells":{"extent":0},"lifetime":{"definition":999,"last_read":999}});
    let replay: TypedProgram = serde_json::from_value(wire).unwrap();
    assert_eq!(replay.storage_plan(), p.storage_plan());
}

#[test]
fn observation_alias_members_retain_their_shared_predicate_cells() {
    let table = crate::SolvePureCallTable::construct(profile(), |t| {
        t.add_owner(
            crate::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(711).unwrap()),
            vec![real()],
            vec![crate::SolvePureCallOutput::assertion_predicate()],
            span(1),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(2))?;
                let zero = b.constant(SolveValue::real(profile(), 0.0), span(3))?;
                let positive = b.compare(SolveCompareOperator::Greater, x, zero, span(4))?;
                let yes = b.constant(SolveValue::boolean(true), span(5))?;
                let updated = b.update_element(positive, yes, &[], span(6))?;
                b.store(outputs[0], updated, span(7))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[0];
    let ordinary = owner.body().storage_plan().aliases();
    assert!(matches!(
        ordinary.registers()[4],
        SolveRegisterHome::OutputSlot { .. }
    ));
    let observation = owner.observation_aliases();
    assert_eq!(observation.registers()[2], SolveRegisterHome::Own);
    assert_eq!(
        observation.registers()[4],
        SolveRegisterHome::Register { register: 2 }
    );
    let cells = owner.observation_cells();
    assert!(cells.offsets()[2].is_some());
    assert_eq!(cells.offsets()[2], cells.offsets()[4]);
}
