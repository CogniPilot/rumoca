use super::*;
use crate::{FaultClass, SolveIntegerDomain};

fn arithmetic() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL)
}

/// A program with a Real input of `dimensions`, its loaded register and a
/// Real scalar, handed to `body`.
fn with_aggregate(
    dimensions: Vec<u32>,
    body: impl for<'p> FnOnce(
        &mut TypedProgramBuilder<'p>,
        ProgramRegister<'p>,
        ProgramRegister<'p>,
    ) -> Result<(), SolveProgramConstructionError>,
) -> TypedProgram {
    let arithmetic = arithmetic();
    TypedProgram::construct(arithmetic, |b| {
        let real = SolveScalarType::real(arithmetic);
        let input = b.declare_slot(
            SolveValueType::tensor(real, dimensions).unwrap(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let aggregate = b.load(input, span(1))?;
        let value = b.constant(SolveValue::real(arithmetic, 7.0), span(2))?;
        body(b, aggregate, value)
    })
    .unwrap()
}

#[test]
fn a_static_element_update_is_a_one_element_slice_update_that_cannot_fault() {
    let program = with_aggregate(vec![2, 3], |b, aggregate, value| {
        b.update_element_static(aggregate, value, vec![1, 2], span(3))?;
        Ok(())
    });
    let operations: Vec<_> = program
        .operations()
        .iter()
        .map(SolveSpannedOperation::operation)
        .collect();
    let [_, value, cell, update] = operations.as_slice() else {
        panic!("load, constant, one-element aggregate, slice update: {operations:?}");
    };
    let (
        SolveOperation::Constant {
            destination: value, ..
        },
        SolveOperation::ConstructAggregate {
            destination: cell,
            elements,
        },
    ) = (value, cell)
    else {
        panic!("{operations:?}");
    };
    assert_eq!(elements.as_ref(), &[*value]);
    assert_eq!(program.register_types()[cell.index()].dimensions(), &[1, 1]);
    let SolveOperation::UpdateSlice {
        value: written,
        origin,
        ..
    } = update
    else {
        panic!("{operations:?}");
    };
    assert_eq!(written, cell);
    assert_eq!(origin.as_ref(), &[1, 2]);
    assert_eq!(program.fault_class(2), FaultClass::CannotFault);
    assert_eq!(program.fault_class(3), FaultClass::CannotFault);
}

#[test]
fn only_full_rank_indices_inside_every_extent_are_in_bounds() {
    with_aggregate(vec![2, 3], |b, aggregate, _| {
        assert!(b.element_in_bounds(aggregate, &[0, 0]), "first element");
        assert!(b.element_in_bounds(aggregate, &[1, 2]), "last element");
        assert!(!b.element_in_bounds(aggregate, &[2, 0]), "extent + 1");
        assert!(!b.element_in_bounds(aggregate, &[0, 3]), "extent + 1");
        assert!(!b.element_in_bounds(aggregate, &[0]), "partial rank");
        assert!(!b.element_in_bounds(aggregate, &[0, 0, 0]), "excess rank");
        assert!(!b.element_in_bounds(aggregate, &[]), "no index");
        Ok(())
    });
    // No register has an empty extent: the type is refused before any index
    // can address it.
    assert_eq!(
        SolveValueType::tensor(SolveScalarType::real(arithmetic()), vec![0]),
        Err(SolveTypeConstructionError::ZeroTensorExtent)
    );
}

#[test]
fn an_index_not_proved_in_bounds_is_refused_without_an_operation() {
    let refused = |dimensions: Vec<u32>, indices: Vec<u32>| {
        with_aggregate(dimensions, |b, aggregate, value| {
            let count = b.operations.len();
            assert_eq!(
                b.update_element_static(aggregate, value, indices, span(3)),
                Err(SolveProgramConstructionError::InvalidProjection {
                    provenance: span(3)
                })
            );
            assert_eq!(b.operations.len(), count);
            Ok(())
        });
    };
    refused(vec![2, 3], vec![2, 0]);
    refused(vec![2, 3], vec![0, 3]);
    refused(vec![2, 3], vec![0]);
    // An element of another type is refused the same way.
    with_aggregate(vec![2], |b, aggregate, _| {
        let integer = b.constant(SolveValue::integer(arithmetic(), 1).unwrap(), span(4))?;
        let count = b.operations.len();
        assert_eq!(
            b.update_element_static(aggregate, integer, vec![0], span(5)),
            Err(SolveProgramConstructionError::InvalidProjection {
                provenance: span(5)
            })
        );
        assert_eq!(b.operations.len(), count);
        Ok(())
    });
}
