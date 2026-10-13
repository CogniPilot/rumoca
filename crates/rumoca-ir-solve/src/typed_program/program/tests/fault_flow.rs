use super::*;
use crate::{FaultClass, FaultKind, SolveIntegerDomain};

fn integer(arithmetic: SolveArithmeticProfile) -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::integer(arithmetic))
}

fn class_of_last(program: &TypedProgram) -> FaultClass {
    program.fault_class(program.operations().len() - 1)
}

#[test]
fn integer_arithmetic_over_constants_inside_the_domain_cannot_fault() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let two = builder.constant(SolveValue::integer(arithmetic, 2).unwrap(), span(0))?;
        let three = builder.constant(SolveValue::integer(arithmetic, 3).unwrap(), span(1))?;
        builder.binary(SolveBinaryOperator::Multiply, two, three, span(2))?;
        Ok(())
    })
    .unwrap();
    assert_eq!(class_of_last(&program), FaultClass::CannotFault);
    assert!(program.is_prunable(program.operations().len() - 1));
}

#[test]
fn integer_arithmetic_leaving_the_integer_domain_may_fault_even_when_it_fits_i64() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let large = builder.constant(
            SolveValue::integer(arithmetic, 2_000_000_000).unwrap(),
            span(0),
        )?;
        builder.binary(SolveBinaryOperator::Add, large, large, span(1))?;
        Ok(())
    })
    .unwrap();
    assert_eq!(
        class_of_last(&program),
        FaultClass::MayFault(FaultKind::IntegerArithmetic)
    );
    assert!(!program.is_prunable(program.operations().len() - 1));
}

#[test]
fn integer_arithmetic_over_an_unbounded_input_may_fault() {
    let arithmetic = profile();
    let program = TypedProgram::construct(arithmetic, |builder| {
        let input = builder.declare_slot(
            integer(arithmetic),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let value = builder.load(input, span(1))?;
        let one = builder.constant(SolveValue::integer(arithmetic, 1).unwrap(), span(2))?;
        builder.binary(SolveBinaryOperator::Add, value, one, span(3))?;
        Ok(())
    })
    .unwrap();
    assert_eq!(
        class_of_last(&program),
        FaultClass::MayFault(FaultKind::IntegerArithmetic)
    );
}

#[test]
fn real_arithmetic_cannot_fault_and_a_store_always_runs() {
    let arithmetic = profile();
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let program = TypedProgram::construct(arithmetic, |builder| {
        let output = builder.declare_slot(
            real,
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(0),
        )?;
        let one = builder.constant(SolveValue::real(arithmetic, 1.0), span(1))?;
        let sum = builder.binary(SolveBinaryOperator::Divide, one, one, span(2))?;
        builder.store(output, sum, span(3))
    })
    .unwrap();
    assert_eq!(program.fault_class(1), FaultClass::CannotFault);
    assert_eq!(program.fault_class(2), FaultClass::HasEffect);
    assert_eq!(program.region_fault_class(), FaultClass::CannotFault);
}

fn map_squaring_binder(upper: i64) -> TypedProgram {
    let arithmetic = profile();
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper,
            step: 1,
        }],
    };
    TypedProgram::construct(arithmetic, |builder| {
        builder.map(
            domain.clone(),
            &[],
            integer(arithmetic),
            span(0),
            |region, _, binders, output| {
                let binder = region.load(binders[0], span(1))?;
                let square =
                    region.binary(SolveBinaryOperator::Multiply, binder, binder, span(2))?;
                region.store(output, square, span(3))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn the_interval_of_a_loop_binder_is_its_domain() {
    // 1..=4 squared is at most 16: the body cannot fault, so the map is prunable.
    let program = map_squaring_binder(4);
    assert_eq!(program.fault_class(0), FaultClass::CannotFault);
    // 1..=100000 squared leaves the i32 domain: the map may fault and always runs.
    let program = map_squaring_binder(100_000);
    assert_eq!(
        program.fault_class(0),
        FaultClass::MayFault(FaultKind::IntegerArithmetic)
    );
}

#[test]
fn a_binder_bound_outside_the_integer_domain_has_no_interval() {
    let domain = SolveIntegerDomain::construct(-8, 8).unwrap();
    assert!(crate::SolveIntegerInterval::span(domain, 1, 9).is_none());
    assert!(crate::SolveIntegerInterval::point(domain, -9).is_none());
    let interval = crate::SolveIntegerInterval::span(domain, 8, 1).unwrap();
    assert_eq!((interval.low(), interval.high()), (1, 8));
    assert!(interval.multiply(interval, domain).is_none());
    assert_eq!(
        interval.negate(domain).map(|n| (n.low(), n.high())),
        Some((-8, -1))
    );
}

#[test]
fn scalar_folds_agree_with_point_intervals() {
    use crate::{SolveIntegerInterval, fold_scalar_binary};
    let domain = SolveIntegerDomain::construct(-1000, 1000).unwrap();
    let integer = SolveScalarType::Integer(domain);
    for operator in [
        SolveBinaryOperator::Add,
        SolveBinaryOperator::Subtract,
        SolveBinaryOperator::Multiply,
        SolveBinaryOperator::Min,
        SolveBinaryOperator::Max,
    ] {
        for (lhs, rhs) in [(-40, 25), (31, 31), (999, 2), (-1000, 1), (0, -7)] {
            let folded = fold_scalar_binary(integer, operator, (lhs, rhs));
            let interval = SolveIntegerInterval::point(domain, lhs).unwrap().binary(
                operator,
                SolveIntegerInterval::point(domain, rhs).unwrap(),
                domain,
            );
            let point = interval.map(|interval| (interval.low(), interval.high()));
            assert_eq!(
                folded.map(|value| (value, value)),
                point,
                "{operator:?}({lhs}, {rhs})"
            );
        }
    }
}
