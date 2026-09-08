//! Binary32 transcendental relation witnesses across the evaluator and the
//! native backend.
//!
//! The two arms compute Binary32 transcendentals through different shapes:
//! the evaluator applies the platform float routine directly, while the
//! native backend promotes to Binary64, calls the platform double routine,
//! and demotes. Square root and the basic operations are exempt from that
//! divergence: promoting one Binary32 operand set through a correctly
//! rounded Binary64 operation and demoting yields the correctly rounded
//! Binary32 result, because Binary64 carries more than twice the Binary32
//! precision plus two bits. The witnesses below therefore assert exact
//! parity where it is provable, assert the exceptionally defined values
//! bit-exactly where the general relation is only library-quality, and pin
//! the observed one-ulp shape divergence so no broad parity claim can ride
//! an accidental agreement.

use std::num::NonZeroU64;

use rumoca_core::{RealMatrixMultiplySemantics, SourceId, Span};
use rumoca_eval_solve::{TypedValue, eval_pure_call};
use rumoca_exec_cranelift::compile_pure_call_table;
use rumoca_ir_solve::{
    SolveArithmeticProfile, SolveBinaryOperator, SolveIntegerDomain, SolvePureCallIdentity,
    SolvePureCallOutput, SolvePureCallTable, SolveRealFormat, SolveScalarType, SolveUnaryOperator,
    SolveValueKind, SolveValueType,
};

fn span(start: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("transcendental_contract.mo"),
        start,
        start + 1,
    )
}

fn profile() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
        RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    )
}

fn identity(value: u64) -> SolvePureCallIdentity {
    SolvePureCallIdentity::issued(NonZeroU64::new(value).unwrap())
}

/// Owner 0: pow(lhs, rhs). Owner 1: atan2(lhs, rhs). Owner 2: sqrt(x).
fn contract_table() -> SolvePureCallTable {
    let arithmetic = profile();
    let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(1),
            vec![scalar.clone(), scalar.clone()],
            vec![SolvePureCallOutput::result(scalar.clone())],
            span(0),
            |builder, inputs, outputs| {
                let lhs = builder.load(inputs[0], span(1))?;
                let rhs = builder.load(inputs[1], span(2))?;
                let result = builder.binary(SolveBinaryOperator::Power, lhs, rhs, span(3))?;
                builder.store(outputs[0], result, span(4))
            },
        )?;
        table.add_owner(
            identity(2),
            vec![scalar.clone(), scalar.clone()],
            vec![SolvePureCallOutput::result(scalar.clone())],
            span(5),
            |builder, inputs, outputs| {
                let lhs = builder.load(inputs[0], span(6))?;
                let rhs = builder.load(inputs[1], span(7))?;
                let result = builder.binary(SolveBinaryOperator::Atan2, lhs, rhs, span(8))?;
                builder.store(outputs[0], result, span(9))
            },
        )?;
        table.add_owner(
            identity(3),
            vec![scalar.clone()],
            vec![SolvePureCallOutput::result(scalar)],
            span(10),
            |builder, inputs, outputs| {
                let value = builder.load(inputs[0], span(11))?;
                let result = builder.unary(SolveUnaryOperator::Sqrt, value, span(12))?;
                builder.store(outputs[0], result, span(13))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn scalar_value(value: f32) -> TypedValue {
    TypedValue::construct(
        SolveValueType::scalar(SolveScalarType::real(profile())),
        vec![SolveValueKind::Real32(value.to_bits())],
    )
    .unwrap()
}

fn evaluator_bits(table: &SolvePureCallTable, owner: usize, inputs: &[f32]) -> u32 {
    let arguments: Vec<TypedValue> = inputs.iter().map(|value| scalar_value(*value)).collect();
    let outputs = eval_pure_call(table, table.owners()[owner].id(), &arguments).unwrap();
    match outputs[0].elements()[0] {
        SolveValueKind::Real32(bits) => bits,
        ref other => panic!("expected a Binary32 result, got {other:?}"),
    }
}

fn native_bits(
    compiled: &rumoca_exec_cranelift::CompiledPureCallTable,
    table: &SolvePureCallTable,
    owner: usize,
    inputs: &[f32],
) -> u32 {
    let payload: Vec<f64> = inputs.iter().map(|value| f64::from(*value)).collect();
    let mut output = [f64::NAN];
    compiled
        .call_scalar_payload(
            &table.owners()[owner].call_site(),
            &payload,
            &mut output,
            &mut Vec::new(),
            &mut Vec::new(),
        )
        .unwrap();
    (output[0] as f32).to_bits()
}

/// Square root is correctly rounded on both shapes, so exact cross-arm
/// parity is provable and asserted for ordinary, subnormal, and boundary
/// inputs.
#[test]
fn binary32_sqrt_is_bit_exact_across_evaluator_and_native() {
    let table = contract_table();
    let compiled = compile_pure_call_table(&table).unwrap();
    let inputs = [
        0.0_f32,
        -0.0,
        1.0,
        2.0,
        0.25,
        3.0,
        5.0e-40,
        f32::MIN_POSITIVE,
        1.9999999,
        16777215.0,
        3.4e38,
        f32::INFINITY,
    ];
    for value in inputs {
        let expected = value.sqrt().to_bits();
        assert_eq!(
            evaluator_bits(&table, 2, &[value]),
            expected,
            "evaluator sqrt({value}) must be the correctly rounded Binary32 result"
        );
        assert_eq!(
            native_bits(&compiled, &table, 2, &[value]),
            expected,
            "native sqrt({value}) must be the correctly rounded Binary32 result"
        );
    }
    let evaluator_nan = evaluator_bits(&table, 2, &[-1.0]);
    let native_nan = native_bits(&compiled, &table, 2, &[-1.0]);
    assert!(f32::from_bits(evaluator_nan).is_nan());
    assert!(f32::from_bits(native_nan).is_nan());
}

/// The exceptionally defined pow and atan2 results are exact values, not
/// library approximations, so both arms must agree bit for bit even though
/// the general relation is only library quality. NaN results are compared
/// as a class per the quiet-NaN quotient.
#[test]
fn binary32_pow_and_atan2_special_values_are_bit_exact_across_arms() {
    let table = contract_table();
    let compiled = compile_pure_call_table(&table).unwrap();

    let pow_exact: [(f32, f32, f32); 8] = [
        (2.5, 0.0, 1.0),
        (-3.0, 0.0, 1.0),
        (f32::INFINITY, 0.0, 1.0),
        (1.0, 7.5, 1.0),
        (1.0, f32::NEG_INFINITY, 1.0),
        (0.0, 3.0, 0.0),
        (-0.0, 3.0, -0.0),
        (0.0, -2.0, f32::INFINITY),
    ];
    for (lhs, rhs, expected) in pow_exact {
        let expected = expected.to_bits();
        assert_eq!(
            evaluator_bits(&table, 0, &[lhs, rhs]),
            expected,
            "evaluator pow({lhs}, {rhs})"
        );
        assert_eq!(
            native_bits(&compiled, &table, 0, &[lhs, rhs]),
            expected,
            "native pow({lhs}, {rhs})"
        );
    }
    for (lhs, rhs) in [(f32::NAN, 0.0_f32), (1.0, f32::NAN)] {
        let expected = 1.0_f32.to_bits();
        assert_eq!(evaluator_bits(&table, 0, &[lhs, rhs]), expected);
        assert_eq!(native_bits(&compiled, &table, 0, &[lhs, rhs]), expected);
    }
    assert!(f32::from_bits(evaluator_bits(&table, 0, &[f32::NAN, 1.0])).is_nan());
    assert!(f32::from_bits(native_bits(&compiled, &table, 0, &[f32::NAN, 1.0])).is_nan());

    let atan2_exact: [(f32, f32, f32); 6] = [
        (0.0, 1.0, 0.0),
        (-0.0, 1.0, -0.0),
        (0.0, -1.0, std::f32::consts::PI),
        (-0.0, -1.0, -std::f32::consts::PI),
        (1.0, 0.0, std::f32::consts::FRAC_PI_2),
        (-1.0, 0.0, -std::f32::consts::FRAC_PI_2),
    ];
    for (lhs, rhs, expected) in atan2_exact {
        let expected = expected.to_bits();
        assert_eq!(
            evaluator_bits(&table, 1, &[lhs, rhs]),
            expected,
            "evaluator atan2({lhs}, {rhs})"
        );
        assert_eq!(
            native_bits(&compiled, &table, 1, &[lhs, rhs]),
            expected,
            "native atan2({lhs}, {rhs})"
        );
    }
}

/// The general-input relation between the two arms is NOT parity: the
/// evaluator applies the platform float routine while the native backend
/// promotes through the platform double routine. Each arm is pinned to its
/// own shape, and the pinned inputs document a live one-ulp divergence. If
/// every divergence below disappears, the host has unified the shapes and
/// the catalog row must be reclassified before any parity claim is made.
#[test]
fn binary32_pow_general_inputs_pin_shapes_and_document_divergence() {
    let table = contract_table();
    let compiled = compile_pure_call_table(&table).unwrap();
    let pinned: [(u32, u32); 3] = [
        (0x1E21_6B64, 0x3E4F_70B2),
        (0x3B57_A9AA, 0x3A07_2C79),
        (0x1DFF_FD91, 0x3CDD_462A),
    ];
    let mut divergences = 0usize;
    for (lhs_bits, rhs_bits) in pinned {
        // black_box forces genuine runtime library calls; on literal
        // arguments the compiler constant-folds powf with a shape of its
        // own, which is neither arm's runtime shape.
        let lhs = std::hint::black_box(f32::from_bits(lhs_bits));
        let rhs = std::hint::black_box(f32::from_bits(rhs_bits));
        let direct_shape = lhs.powf(rhs).to_bits();
        let promoted_shape = (f64::from(lhs).powf(f64::from(rhs)) as f32).to_bits();
        assert_eq!(
            evaluator_bits(&table, 0, &[lhs, rhs]),
            direct_shape,
            "the evaluator arm is pinned to the direct float-routine shape"
        );
        assert_eq!(
            native_bits(&compiled, &table, 0, &[lhs, rhs]),
            promoted_shape,
            "the native arm is pinned to the promote-through-double shape"
        );
        if direct_shape != promoted_shape {
            divergences += 1;
        }
    }
    assert!(
        divergences >= 1,
        "no pinned input diverges between the float-routine and \
         promote-through-double shapes on this host; the Binary32 \
         transcendental relation row must be reclassified before any \
         cross-arm parity claim"
    );
}
