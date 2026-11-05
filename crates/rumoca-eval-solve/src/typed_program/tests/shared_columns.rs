//! Packed-column numerical comparisons against the existing single-column AD.
use super::*;

#[derive(Clone, Copy)]
enum Rule {
    Unary(SolveUnaryOperator),
    Binary(SolveBinaryOperator),
}

fn owner_table(format: SolveRealFormat, rule: Rule, fused: bool) -> SolvePureCallTable {
    let p = profile(format);
    let tensor = SolveValueType::tensor(SolveScalarType::real(p), vec![4]).unwrap();
    let packed = SolveValueType::tensor(SolveScalarType::real(p), vec![4, 3]).unwrap();
    let output_types = if fused {
        vec![packed]
    } else {
        vec![tensor.clone(); 3]
    };
    SolvePureCallTable::construct(p, |table| {
        table.add_owner(
            identity(1100),
            vec![tensor.clone(); 4],
            output_types
                .into_iter()
                .map(SolvePureCallOutput::result)
                .collect(),
            span(1100),
            |b, inputs, outputs| {
                let values = inputs
                    .iter()
                    .map(|slot| b.load(*slot, span(1101)))
                    .collect::<Result<Vec<_>, _>>()?;
                let zero = b.constant(SolveValue::real(p, 0.0), span(1102))?;
                let absent = b.fill(zero, vec![4], span(1103))?;
                let result = match rule {
                    Rule::Unary(operator) => emit_unary(b, operator, &values, absent, fused)?,
                    Rule::Binary(operator) => emit_binary(b, operator, &values, absent, fused)?,
                };
                for (slot, value) in outputs.iter().zip(result) {
                    b.store(*slot, value, span(1106))?;
                }
                Ok(())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn emit_unary<'program>(
    b: &mut TypedProgramBuilder<'program>,
    operator: SolveUnaryOperator,
    values: &[ProgramRegister<'program>],
    absent: ProgramRegister<'program>,
    fused: bool,
) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
    let source = b.aggregate_unary_directional_source(operator, values[0], span(1104))?;
    let directions = [Some(values[2]), None, Some(values[3])];
    if fused {
        let columns = b.aggregate_unary_directional_columns(source, &directions)?;
        Ok(vec![columns.packed().expect("two explicit directions")])
    } else {
        directions
            .iter()
            .map(|direction| {
                b.aggregate_unary_directional_tangent(source, *direction)
                    .map(|value| value.unwrap_or(absent))
            })
            .collect()
    }
}

fn emit_binary<'program>(
    b: &mut TypedProgramBuilder<'program>,
    operator: SolveBinaryOperator,
    values: &[ProgramRegister<'program>],
    absent: ProgramRegister<'program>,
    fused: bool,
) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
    let source =
        b.aggregate_binary_directional_source(operator, [values[0], values[1]], span(1105))?;
    let directions = [
        [Some(values[2]), None],
        [None, None],
        [Some(values[2]), Some(values[3])],
    ];
    if fused {
        let columns = b.aggregate_binary_directional_columns(source, &directions)?;
        Ok(vec![columns.packed().expect("two explicit directions")])
    } else {
        directions
            .iter()
            .map(|direction| {
                b.aggregate_binary_directional_tangent(source, *direction)
                    .map(|value| value.unwrap_or(absent))
            })
            .collect()
    }
}

fn tensor_value(format: SolveRealFormat, values: [f64; 4]) -> TypedValue {
    TypedValue::construct(
        SolveValueType::tensor(SolveScalarType::real(profile(format)), vec![4]).unwrap(),
        values
            .into_iter()
            .map(|value| real_kind(format, value))
            .collect(),
    )
    .unwrap()
}

fn compare_columns(format: SolveRealFormat, rule: Rule, x: [f64; 4], y: [f64; 4]) {
    let packed = owner_table(format, rule, true);
    let separate = owner_table(format, rule, false);
    let input = [
        tensor_value(format, x),
        tensor_value(format, y),
        tensor_value(format, [0.0, -0.0, 1.0, -2.0]),
        tensor_value(format, [-0.0, 2.0, -3.0, 0.5]),
    ];
    let actual = stopping_call(&packed, packed.owners()[0].id(), &input).unwrap();
    let reference = stopping_call(&separate, separate.owners()[0].id(), &input).unwrap();
    let expected = (0..4)
        .flat_map(|row| reference.iter().map(move |column| column.elements()[row]))
        .collect::<Vec<_>>();
    assert_eq!(actual.len(), 1);
    assert_eq!(actual[0].elements(), expected);
}

#[test]
fn packed_unary_columns_keep_existing_binary32_and_binary64_output_bits() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        for operator in [
            SolveUnaryOperator::Negate,
            SolveUnaryOperator::Abs,
            SolveUnaryOperator::Sin,
            SolveUnaryOperator::Cos,
            SolveUnaryOperator::Tan,
            SolveUnaryOperator::Atan,
            SolveUnaryOperator::Sinh,
            SolveUnaryOperator::Cosh,
            SolveUnaryOperator::Tanh,
            SolveUnaryOperator::Exp,
            SolveUnaryOperator::Sqrt,
            SolveUnaryOperator::Asin,
            SolveUnaryOperator::Acos,
            SolveUnaryOperator::Log,
            SolveUnaryOperator::Log10,
            SolveUnaryOperator::Sign,
            SolveUnaryOperator::Floor,
            SolveUnaryOperator::Ceiling,
            SolveUnaryOperator::Truncate,
        ] {
            for values in [
                [0.0, -0.0, 0.5, 2.0],
                [
                    -2.0,
                    f64::INFINITY,
                    f64::NEG_INFINITY,
                    f64::from_bits(0x7ff8_0000_0000_0123),
                ],
            ] {
                compare_columns(format, Rule::Unary(operator), values, [1.0; 4]);
            }
        }
    }
}

#[test]
fn packed_binary_columns_keep_existing_operand_order_and_nonfinite_bits() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        for operator in [
            SolveBinaryOperator::Add,
            SolveBinaryOperator::Subtract,
            SolveBinaryOperator::Multiply,
            SolveBinaryOperator::Divide,
            SolveBinaryOperator::Power,
            SolveBinaryOperator::Min,
            SolveBinaryOperator::Max,
            SolveBinaryOperator::Atan2,
        ] {
            compare_columns(
                format,
                Rule::Binary(operator),
                [0.0, -0.0, 0.5, 2.0],
                [0.0, 1.0, -2.0, 0.5],
            );
            compare_columns(
                format,
                Rule::Binary(operator),
                [
                    -2.0,
                    f64::INFINITY,
                    f64::NEG_INFINITY,
                    f64::from_bits(0x7ff8_0000_0000_0123),
                ],
                [0.5, 2.0, 0.0, -0.0],
            );
        }
    }
}
