use super::*;
use crate::typed_program::program::print_plan::{SolveTensorKernel, SolveTensorPart};

fn binary64() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        crate::SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
    )
}

/// A body with an input view, owned register frames, a broadcast operand,
/// a narrowed integer domain, concatenated parts, a static element
/// coordinate and three output copies.
fn spelled_program() -> TypedProgram {
    let arithmetic = binary64();
    let real = SolveScalarType::real(arithmetic);
    let vector = SolveValueType::tensor(real, vec![3]).unwrap();
    let integer = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    TypedProgram::construct(arithmetic, |builder| {
        let input = builder.declare_slot(
            vector.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let scaled_out = builder.declare_slot(
            vector.clone(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(1),
        )?;
        let element_out = builder.declare_slot(
            SolveValueType::scalar(real),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(2),
        )?;
        let sum_out = builder.declare_slot(
            integer.clone(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(3),
        )?;
        let x = builder.load(input, span(4))?;
        let two = builder.constant(SolveValue::real(arithmetic, 2.0), span(5))?;
        let scaled =
            builder.broadcast_binary(SolveBinaryOperator::Multiply, x, two, false, span(6))?;
        let joined = builder.concatenate(0, &[x, scaled], span(7))?;
        let picked = builder.project_element(joined, vec![4, 0], span(8))?;
        let one = builder.constant(SolveValue::integer(arithmetic, 1).unwrap(), span(9))?;
        let sum = builder.binary(SolveBinaryOperator::Add, one, one, span(10))?;
        builder.store(scaled_out, scaled, span(11))?;
        builder.store(element_out, picked, span(12))?;
        builder.store(sum_out, sum, span(13))
    })
    .unwrap()
}

/// The issued print plan of one body names its frame, register frames,
/// items and output copies by the values a printer spells.
#[test]
fn a_print_plan_issues_every_spelled_value() {
    let program = spelled_program();
    let plan = program.print_plan();

    assert!(matches!(
        plan.slots()[0],
        SolveFrameItem::ArgumentView { slot: 0, .. }
    ));
    let SolveFrameItem::Frame { name, .. } = &plan.slots()[1] else {
        panic!("an output slot is an owned frame: {:?}", plan.slots());
    };
    assert_eq!((name.space(), name.id()), (SolveNameSpace::Slot, 1));

    // The loaded input is a view; the scaled vector is the first owned frame.
    let frame = plan.registers()[1];
    assert_eq!(
        (frame.register(), frame.element(), frame.extent()),
        (2, SolveElementToken::Real64, 3)
    );

    let copies: Vec<_> = plan
        .outputs()
        .iter()
        .map(|copy| (copy.slot(), copy.output()))
        .collect();
    assert_eq!(copies, [(1, 0), (2, 1), (3, 2)]);

    let mut seen = 0;
    for item in plan.body() {
        match item {
            SolveBodyItem::Elementwise {
                kernel: SolveKernel::Binary { lhs, rhs, .. },
                ..
            } => {
                assert_eq!((lhs.register(), lhs.is_broadcast()), (0, false));
                assert_eq!((rhs.register(), rhs.is_broadcast()), (1, true));
                seen |= 1;
            }
            SolveBodyItem::Elementwise {
                domain: Some(domain),
                ..
            } => {
                assert_eq!(
                    (domain.minimum(), domain.maximum()),
                    (
                        SolveIntegerLiteral::Negative { magnitude: 1 << 31 },
                        SolveIntegerLiteral::Nonnegative {
                            magnitude: (1 << 31) - 1
                        }
                    )
                );
                seen |= 2;
            }
            SolveBodyItem::Tensor {
                kernel: SolveTensorKernel::Concatenate { parts, .. },
                ..
            } => {
                let spans: Vec<_> = parts
                    .iter()
                    .map(|part: &SolveTensorPart| (part.operand(), part.offset(), part.width()))
                    .collect();
                assert_eq!(spans, [(0, 0, 3), (2, 3, 3)]);
                seen |= 4;
            }
            SolveBodyItem::Tensor {
                kernel: SolveTensorKernel::ProjectElement { axes, .. },
                ..
            } => {
                let axes: Vec<_> = axes
                    .iter()
                    .map(|axis| (axis.extent(), axis.coordinate()))
                    .collect();
                assert_eq!(axes, [(6, 4), (1, 0)]);
                seen |= 8;
            }
            _ => {}
        }
    }
    assert_eq!(
        seen,
        15,
        "every spelled value was issued: {:?}",
        plan.body()
    );
}
