use super::*;
use crate::SolveBinaryOperator;

fn broadcast_table(
    operator: SolveBinaryOperator,
    element: SolveScalarType,
    dimensions: Vec<u32>,
    scalar_on_lhs: bool,
) -> SolvePureCallTable {
    let aggregate = SolveValueType::tensor(element, dimensions).unwrap();
    let scalar = SolveValueType::scalar(element);
    SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1901),
            vec![aggregate.clone(), scalar],
            vec![SolvePureCallOutput::result(aggregate)],
            span(0),
            |builder, inputs, outputs| {
                let aggregate = builder.load(inputs[0], span(1))?;
                let scalar = builder.load(inputs[1], span(2))?;
                let result = builder.broadcast_binary(
                    operator,
                    aggregate,
                    scalar,
                    scalar_on_lhs,
                    span(3),
                )?;
                builder.store(outputs[0], result, span(4))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn discrete_broadcast_directional_retains_one_exact_primal_and_no_tangent() {
    for (operator, element) in [
        (
            SolveBinaryOperator::IntegerQuotient,
            SolveScalarType::integer(profile()),
        ),
        (
            SolveBinaryOperator::IntegerModulo,
            SolveScalarType::integer(profile()),
        ),
        (
            SolveBinaryOperator::IntegerRemainder,
            SolveScalarType::integer(profile()),
        ),
        (SolveBinaryOperator::And, SolveScalarType::Boolean),
        (SolveBinaryOperator::Or, SolveScalarType::Boolean),
    ] {
        for dimensions in [vec![2, 3], vec![1_000_000]] {
            for scalar_on_lhs in [false, true] {
                assert_discrete_broadcast(operator, element, dimensions.clone(), scalar_on_lhs);
            }
        }
    }
}

fn assert_discrete_broadcast(
    operator: SolveBinaryOperator,
    element: SolveScalarType,
    dimensions: Vec<u32>,
    scalar_on_lhs: bool,
) {
    let table = broadcast_table(operator, element, dimensions, scalar_on_lhs);
    let owner = &table.owners()[0];
    let directional = owner
        .directional()
        .expect("discrete broadcast has no variable tangent");
    assert_eq!(directional.inputs(), owner.inputs());
    assert_eq!(directional.outputs(), owner.outputs());
    assert_eq!(
        directional.body(),
        owner.body(),
        "retain exact typed arithmetic and one compact broadcast"
    );
    let json = serde_json::to_string(&table).unwrap();
    let decoded: SolvePureCallTable = serde_json::from_str(&json).unwrap();
    assert_eq!(serde_json::to_string(&decoded).unwrap(), json);
}
