use super::*;
use crate::{SolveOperation, SolveReductionOperator};

fn reduction_table(
    arithmetic: SolveArithmeticProfile,
    operator: SolveReductionOperator,
    element: SolveScalarType,
    dimensions: Vec<u32>,
) -> SolvePureCallTable {
    let aggregate = SolveValueType::tensor(element, dimensions).unwrap();
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(1911),
            vec![aggregate],
            vec![SolvePureCallOutput::result(SolveValueType::scalar(element))],
            span(0),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1))?;
                let result = builder.reduce(operator, input, span(2))?;
                builder.store(outputs[0], result, span(3))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn reduction_extrema_directional_keeps_compact_shape_and_checked_index_profile() {
    for operator in [
        SolveReductionOperator::Minimum,
        SolveReductionOperator::Maximum,
    ] {
        for dimensions in [vec![1], vec![2, 3, 4], vec![1_000_000]] {
            assert_compact_extremum(operator, dimensions);
        }
        for (minimum, maximum, extent, admitted) in [
            (-128, 127, 4, true),
            (-128, 127, 128, false),
            (2, 127, 4, false),
        ] {
            let arithmetic = SolveArithmeticProfile::construct(
                SolveRealFormat::Binary64,
                SolveIntegerDomain::construct(minimum, maximum).unwrap(),
            );
            let table = reduction_table(
                arithmetic,
                operator,
                SolveScalarType::real(arithmetic),
                vec![extent],
            );
            assert_eq!(table.owners()[0].directional().is_some(), admitted);
        }
        for element in [
            SolveScalarType::integer(profile()),
            SolveScalarType::Boolean,
        ] {
            let table = reduction_table(profile(), operator, element, vec![2, 3]);
            let owner = &table.owners()[0];
            assert_eq!(
                owner.directional().unwrap().body(),
                owner.body(),
                "discrete reduction has only its exact primal"
            );
        }
    }
}

fn assert_compact_extremum(operator: SolveReductionOperator, dimensions: Vec<u32>) {
    let table = reduction_table(
        profile(),
        operator,
        SolveScalarType::real(profile()),
        dimensions,
    );
    let owner = &table.owners()[0];
    let directional = owner
        .directional()
        .expect("extrema have a checked compact directional rule");
    assert_eq!(directional.inputs().len(), 2);
    assert_eq!(directional.outputs().len(), 2);
    let operations = directional.body().operations();
    assert_eq!(
        operations
            .iter()
            .filter(|op| matches!(op.operation(), SolveOperation::Reduce { .. }))
            .count(),
        1
    );
    assert_eq!(
        operations
            .iter()
            .filter(|op| matches!(op.operation(), SolveOperation::Fold { .. }))
            .count(),
        1
    );
    assert!(
        operations.len() < 20,
        "IR does not grow with the element count"
    );
    let json = serde_json::to_string(&table).unwrap();
    let decoded: SolvePureCallTable = serde_json::from_str(&json).unwrap();
    assert_eq!(serde_json::to_string(&decoded).unwrap(), json);
}
