use super::*;
use crate::affinity::Degree::{Affine, Independent, Nonlinear};
use crate::{SolveBinaryOperator, SolveUnaryOperator};

fn nested_scale(width: u32) -> SolvePureCallTable {
    let scalar = SolveValueType::scalar(SolveScalarType::real(profile()));
    let vector = SolveValueType::tensor(SolveScalarType::real(profile()), vec![width]).unwrap();
    SolvePureCallTable::construct(profile(), |table| {
        let inner = table.add_owner(
            identity(1),
            vec![vector.clone(), scalar.clone()],
            vec![SolvePureCallOutput::result(vector.clone())],
            span(0),
            |builder, inputs, outputs| {
                let vector = builder.load(inputs[0], span(1))?;
                let scalar = builder.load(inputs[1], span(2))?;
                let value = builder.scale(vector, scalar, span(3))?;
                builder.store(outputs[0], value, span(4))
            },
        )?;
        table.add_owner(
            identity(2),
            vec![scalar, vector.clone()],
            vec![SolvePureCallOutput::result(vector)],
            span(5),
            |builder, inputs, outputs| {
                let scalar = builder.load(inputs[0], span(6))?;
                let vector = builder.load(inputs[1], span(7))?;
                let value = builder.call(inner, &[vector, scalar], span(8))?;
                builder.store(outputs[0], value[0], span(9))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn call_affinity_distinguishes_coefficients_from_coupled_unknowns() {
    let table = nested_scale(3);
    let site = table.owners()[1].call_site();
    assert_eq!(
        site.output_degrees(&[Independent, Affine]),
        Some(vec![Affine])
    );
    assert_eq!(
        site.output_degrees(&[Affine, Independent]),
        Some(vec![Affine])
    );
    assert_eq!(
        site.output_degrees(&[Affine, Affine]),
        Some(vec![Nonlinear])
    );
    assert_eq!(
        site.output_degrees(&[Independent, Nonlinear]),
        Some(vec![Nonlinear])
    );
    assert_eq!(
        site.output_degrees(&[Independent, Independent]),
        Some(vec![Independent])
    );
    assert_eq!(site.output_degrees(&[Independent]), None);
    let wide = nested_scale(1_000_000);
    assert_eq!(site.affinity, wide.owners()[1].call_site().affinity);
}

#[test]
fn directional_affinity_preserves_primal_and_tangent_interactions() {
    let table = nested_scale(3);
    let site = table.owners()[1].call_site();
    let directional = site.directional().unwrap();
    assert_eq!(
        directional.output_degrees(&[Affine, Independent, Affine, Independent]),
        Some(vec![Nonlinear, Affine])
    );
    assert_eq!(
        directional.output_degrees(&[Independent, Affine, Independent, Affine]),
        Some(vec![Independent, Affine])
    );
    assert_eq!(
        directional.output_degrees(&[Affine, Affine, Affine, Affine]),
        Some(vec![Nonlinear, Nonlinear])
    );
}

#[test]
fn affine_call_claims_cannot_delete_nonlinear_interactions() {
    let table = nested_scale(3);
    let restored: SolvePureCallTable =
        serde_json::from_value(serde_json::to_value(&table).unwrap()).unwrap();
    let site = table.owners()[1].call_site();
    assert!(restored.matches_site(&site));
    let original = serde_json::to_value(&site).unwrap();
    for directional in [false, true] {
        let mut wire = original.clone();
        let profile = if directional {
            &mut wire["directional"]["affinity"]
        } else {
            &mut wire["affinity"]
        };
        profile[0]["nonlinear"] = serde_json::json!([]);
        let forged: SolvePureCallSite = serde_json::from_value(wire).unwrap();
        assert!(!restored.matches_site(&forged));
    }
}

#[test]
fn division_and_nonlinear_functions_require_independent_arguments() {
    let scalar = SolveValueType::scalar(SolveScalarType::real(profile()));
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1),
            vec![scalar.clone(), scalar.clone()],
            vec![
                SolvePureCallOutput::result(scalar.clone()),
                SolvePureCallOutput::result(scalar.clone()),
            ],
            span(0),
            |builder, inputs, outputs| {
                let numerator = builder.load(inputs[0], span(1))?;
                let denominator = builder.load(inputs[1], span(2))?;
                let quotient =
                    builder.binary(SolveBinaryOperator::Divide, numerator, denominator, span(3))?;
                let logarithm = builder.unary(SolveUnaryOperator::Log, denominator, span(4))?;
                builder.store(outputs[0], quotient, span(5))?;
                builder.store(outputs[1], logarithm, span(6))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let site = table.owners()[0].call_site();
    assert_eq!(
        site.output_degrees(&[Affine, Independent]),
        Some(vec![Affine, Independent])
    );
    assert_eq!(
        site.output_degrees(&[Independent, Affine]),
        Some(vec![Nonlinear, Nonlinear])
    );
}

#[test]
fn identity_tensor_preserves_separate_rotation_and_velocity_affinity() {
    let scalar = SolveValueType::scalar(SolveScalarType::real(profile()));
    let matrix = SolveValueType::tensor(SolveScalarType::real(profile()), vec![2, 2]).unwrap();
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1),
            vec![scalar.clone(), scalar.clone()],
            vec![
                SolvePureCallOutput::result(matrix),
                SolvePureCallOutput::result(scalar),
            ],
            span(0),
            |builder, inputs, outputs| {
                let angle = builder.load(inputs[0], span(1))?;
                let rate = builder.load(inputs[1], span(2))?;
                let identity = builder.identity(SolveScalarType::real(profile()), 2, span(3))?;
                let cosine = builder.unary(SolveUnaryOperator::Cos, angle, span(4))?;
                let rotation = builder.scale(identity, cosine, span(5))?;
                builder.store(outputs[0], rotation, span(6))?;
                builder.store(outputs[1], rate, span(7))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let site = table.owners()[0].call_site();
    assert_eq!(
        site.output_degrees(&[Independent, Affine]),
        Some(vec![Independent, Affine])
    );
    assert_eq!(
        site.output_degrees(&[Affine, Affine]),
        Some(vec![Nonlinear, Affine])
    );
    let restored: SolvePureCallTable =
        serde_json::from_value(serde_json::to_value(&table).unwrap()).unwrap();
    assert!(restored.matches_site(&site));
}

fn alternate_coefficient<'program>(
    builder: &mut crate::TypedProgramBuilder<'program>,
    rate: crate::ProgramSlot<'program>,
    cosine: crate::ProgramRegister<'program>,
    nonlinear_branch: bool,
) -> Result<crate::ProgramRegister<'program>, SolveProgramConstructionError> {
    if nonlinear_branch {
        builder.load(rate, span(15))
    } else {
        builder.unary(SolveUnaryOperator::Negate, cosine, span(15))
    }
}

fn branched_rate_call(nonlinear_branch: bool) -> SolvePureCallTable {
    let real = SolveValueType::scalar(SolveScalarType::real(profile()));
    let integer = SolveValueType::scalar(SolveScalarType::integer(profile()));
    let row = SolveValueType::tensor(SolveScalarType::real(profile()), vec![1, 2]).unwrap();
    SolvePureCallTable::construct(profile(), |table| {
        let coefficients = table.add_owner(
            identity(1),
            vec![integer.clone(), real.clone(), real.clone()],
            vec![SolvePureCallOutput::result(row.clone())],
            span(0),
            |builder, inputs, outputs| {
                let axis = builder.load(inputs[0], span(1))?;
                let angle = builder.load(inputs[1], span(2))?;
                let rate = builder.load(inputs[2], span(3))?;
                let one = builder.constant(SolveValue::integer(profile(), 1).unwrap(), span(4))?;
                let condition =
                    builder.compare(crate::SolveCompareOperator::Equal, axis, one, span(5))?;
                let result = builder.conditional(
                    condition,
                    &[angle, rate],
                    vec![row.clone()],
                    span(6),
                    |branch, captures, outputs| {
                        let angle = branch.load(captures[0], span(7))?;
                        let sine = branch.unary(SolveUnaryOperator::Sin, angle, span(8))?;
                        let one =
                            branch.constant(SolveValue::integer(profile(), 1).unwrap(), span(9))?;
                        let one = branch.convert(
                            crate::SolveConversionOperator::IntegerToReal,
                            one,
                            span(10),
                        )?;
                        let row = branch.concatenate(1, &[sine, one], span(11))?;
                        branch.store(outputs[0], row, span(12))
                    },
                    |branch, captures, outputs| {
                        let angle = branch.load(captures[0], span(13))?;
                        let cosine = branch.unary(SolveUnaryOperator::Cos, angle, span(14))?;
                        let other =
                            alternate_coefficient(branch, captures[1], cosine, nonlinear_branch)?;
                        let row =
                            branch.construct_aggregate(&[cosine, other], vec![1, 2], span(16))?;
                        branch.store(outputs[0], row, span(17))
                    },
                )?;
                builder.store(outputs[0], result[0], span(18))
            },
        )?;
        table.add_owner(
            identity(2),
            vec![integer, real.clone(), real],
            vec![
                SolvePureCallOutput::result(row.clone()),
                SolvePureCallOutput::result(row),
            ],
            span(19),
            |builder, inputs, outputs| {
                let axis = builder.load(inputs[0], span(20))?;
                let angle = builder.load(inputs[1], span(21))?;
                let rate = builder.load(inputs[2], span(22))?;
                let matrix = builder.call(coefficients, &[axis, angle, rate], span(23))?;
                let velocity = builder.scale(matrix[0], rate, span(24))?;
                builder.store(outputs[0], matrix[0], span(25))?;
                builder.store(outputs[1], velocity, span(26))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn conditional_tensor_coefficients_are_independent_of_the_solved_rates() {
    let table = branched_rate_call(false);
    let site = table.owners()[1].call_site();
    assert_eq!(
        site.output_degrees(&[Independent, Independent, Affine]),
        Some(vec![Independent, Affine])
    );
    assert_eq!(
        site.output_degrees(&[Independent, Affine, Independent]),
        Some(vec![Nonlinear, Nonlinear])
    );
    assert_eq!(
        site.output_degrees(&[Affine, Independent, Independent]),
        Some(vec![Nonlinear, Nonlinear])
    );
    assert_eq!(
        site.output_degrees(&[Independent, Independent, Nonlinear]),
        Some(vec![Independent, Nonlinear])
    );
    let restored: SolvePureCallTable =
        serde_json::from_value(serde_json::to_value(&table).unwrap()).unwrap();
    assert!(restored.matches_site(&site));
}

#[test]
fn conditional_tensor_affinity_includes_the_unselected_branch() {
    let table = branched_rate_call(true);
    let site = table.owners()[1].call_site();
    assert_eq!(
        site.output_degrees(&[Independent, Independent, Affine]),
        Some(vec![Affine, Nonlinear])
    );
}
