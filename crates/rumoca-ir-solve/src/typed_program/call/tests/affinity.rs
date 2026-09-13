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
