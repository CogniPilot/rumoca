use super::*;
use crate::*;
use rumoca_core::{SourceId, Span};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("body_quaternion.mo"), 1, 2)
}

fn profile() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL)
}

/// `0.5 * Omega(w) * q` for scalar-first `q` and body-angular `w`, in the
/// authored operation order. `flip` turns the first row's last subtraction
/// into an addition, which is no longer the Hamilton relation.
fn kinematics(flip: bool) -> SolvePureCallTable {
    use SolveBinaryOperator::{Add, Multiply as Mul, Subtract as Sub};
    let real = |n| SolveValueType::tensor(SolveScalarType::real(profile()), vec![n]).unwrap();
    SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![real(4), real(3)],
            vec![SolvePureCallOutput::result(real(4))],
            span(),
            |body, inputs, outputs| {
                let q = body.load(inputs[0], span())?;
                let w = body.load(inputs[1], span())?;
                let q = (0..4)
                    .map(|i| body.project_element(q, vec![i], span()))
                    .collect::<Result<Vec<_>, _>>()?;
                let w = (0..3)
                    .map(|i| body.project_element(w, vec![i], span()))
                    .collect::<Result<Vec<_>, _>>()?;
                let half = body.constant(SolveValue::real(profile(), 0.5), span())?;
                // half * ((-(q1*w0) - q2*w1) - q3*w2)
                let a = body.binary(Mul, q[1], w[0], span())?;
                let a = body.unary(SolveUnaryOperator::Negate, a, span())?;
                let b = body.binary(Mul, q[2], w[1], span())?;
                let a = body.binary(Sub, a, b, span())?;
                let b = body.binary(Mul, q[3], w[2], span())?;
                let a = body.binary(if flip { Add } else { Sub }, a, b, span())?;
                let r0 = body.binary(Mul, half, a, span())?;
                // half * ((q0*w0 - q3*w1) + q2*w2)
                let a = body.binary(Mul, q[0], w[0], span())?;
                let b = body.binary(Mul, q[3], w[1], span())?;
                let a = body.binary(Sub, a, b, span())?;
                let b = body.binary(Mul, q[2], w[2], span())?;
                let a = body.binary(Add, a, b, span())?;
                let r1 = body.binary(Mul, half, a, span())?;
                // half * ((q3*w0 + q0*w1) - q1*w2)
                let a = body.binary(Mul, q[3], w[0], span())?;
                let b = body.binary(Mul, q[0], w[1], span())?;
                let a = body.binary(Add, a, b, span())?;
                let b = body.binary(Mul, q[1], w[2], span())?;
                let a = body.binary(Sub, a, b, span())?;
                let r2 = body.binary(Mul, half, a, span())?;
                // half * ((-(q2*w0) + q1*w1) + q0*w2)
                let a = body.binary(Mul, q[2], w[0], span())?;
                let a = body.unary(SolveUnaryOperator::Negate, a, span())?;
                let b = body.binary(Mul, q[1], w[1], span())?;
                let a = body.binary(Add, a, b, span())?;
                let b = body.binary(Mul, q[0], w[2], span())?;
                let a = body.binary(Add, a, b, span())?;
                let r3 = body.binary(Mul, half, a, span())?;
                let derivative = body.construct_aggregate(&[r0, r1, r2, r3], vec![4], span())?;
                body.store(outputs[0], derivative, span())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

/// States `q = y[0..4]`, `w = y[4..7]`. One program calls the owner on them and
/// stores its four results; three opaque programs give `der(w)`. With
/// `call_first == false` the opaque rows come first, so the call's results land
/// on rows 3..7 instead of on `q`'s own rows.
fn derivative(calls: &SolvePureCallTable, call_first: bool) -> ComputeBlock {
    let mut call: Vec<LinearOp> = (0..7)
        .map(|index| LinearOp::LoadY {
            dst: index as u32,
            index,
        })
        .collect();
    call.push(LinearOp::PureCall {
        dst_start: 7,
        input_starts: vec![0, 4].into(),
        site: calls.owners()[0].call_site(),
    });
    call.push(LinearOp::StoreOutputRange {
        start: 7,
        count: 4,
        stride: 1,
    });
    let opaque = (0..3).map(|_| {
        vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ]
    });
    let programs: Vec<_> = if call_first {
        std::iter::once(call).chain(opaque).collect()
    } else {
        opaque.chain(std::iter::once(call)).collect()
    };
    ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_program_spans(programs, vec![span(); 4]).unwrap(),
    )
}

#[test]
fn hamilton_call_issues_the_quaternion_and_angular_velocity_states_it_reads() {
    let calls = kinematics(false);
    let source = derivative(&calls, true);
    let plan = GeometricStepPlan::from_derivative(&source, 7, &calls)
        .expect("the recognized call owns q's derivative rows");
    let [group] = plan.quaternions() else {
        panic!("exactly one quaternion group: {plan:?}");
    };
    assert_eq!(group.quaternion(), &[0, 1, 2, 3]);
    assert_eq!(group.angular_velocity(), &[4, 5, 6]);
    assert!(plan.cartesian_pairs().is_empty());
    assert!(plan.auxiliary().is_empty());

    let calls_wire: SolvePureCallTable =
        serde_json::from_str(&serde_json::to_string(&calls).unwrap()).unwrap();
    // A call site decodes only inside a wire whose pure calls come first, so the
    // replay rebuilds the block on the decoded table and derives the plan again.
    assert_eq!(
        GeometricStepPlan::from_derivative(&derivative(&calls_wire, true), 7, &calls_wire),
        Some(plan)
    );
}

#[test]
fn a_changed_owner_or_misplaced_rows_issue_no_quaternion_group() {
    let changed = kinematics(true);
    assert_eq!(
        GeometricStepPlan::from_derivative(&derivative(&changed, true), 7, &changed),
        None
    );
    let calls = kinematics(false);
    assert_eq!(
        GeometricStepPlan::from_derivative(&derivative(&calls, false), 7, &calls),
        None
    );
}
