use super::*;

#[test]
fn correlated_function_conditional_round_trips_and_rejects_duplicate_targets() {
    let source = TestSource::new(
        "function choose input Boolean c; output Real x; output Real y; end choose;",
    );
    let at = source.source("function choose", 0);
    let dae = Dae::construct(source.map, |dae| {
        let boolean =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Boolean), at))?;
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("choose"), [boolean], [real, real], at),
            |dae, reservation| {
                let condition = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("c"), 0, at)
                })?;
                let x = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("x"), 0, at)
                })?;
                let y = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 1, at)
                })?;
                let condition = dae
                    .expressions(|expressions| expressions.at(at).function_parameter(condition))?;
                let values = dae.expressions(|expressions| {
                    [1.0, 2.0, 3.0, 4.0]
                        .into_iter()
                        .map(|value| expressions.at(at).literal(DaeLiteral::Real(value)))
                        .collect::<Result<Vec<_>, _>>()
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                let duplicate = dae.functions(|functions| {
                    functions.assign_conditional_all(
                        &mut body,
                        &[x, x],
                        &[condition],
                        &[vec![values[0], values[1]]],
                        &[values[2], values[3]],
                        at,
                    )
                });
                assert!(matches!(
                    duplicate,
                    Err(DaeConstructionError::DuplicateDefinition {
                        kind: "function conditional target",
                        index: 0,
                        ..
                    })
                ));
                dae.functions(|functions| {
                    functions.assign_conditional_all(
                        &mut body,
                        &[x, y],
                        &[condition],
                        &[vec![values[0], values[1]]],
                        &[values[2], values[3]],
                        at,
                    )
                })?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .expect("checked conditional correlation constructs atomically");

    let assert_correlation = |view: DaeView<'_>| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let statements = function.statements().collect::<Vec<_>>();
        let [
            FunctionStatementView::AssignmentGroup {
                definitions,
                conditional: Some(conditional),
            },
        ] = statements.as_slice()
        else {
            panic!("function must retain one correlated assignment group")
        };
        assert_eq!(definitions.len(), 2);
        assert_eq!(conditional.conditions().len(), 1);
        assert_eq!(conditional.branch_count(), 1);
        assert_eq!(conditional.branch(0).unwrap().len(), 2);
        assert_eq!(conditional.fallback().len(), 2);
        for definition in definitions.iter() {
            assert!(matches!(
                view.expression(definition.rhs()).unwrap().operation(),
                ExpressionOperation::Conditional(operands) if operands.len() == 3
            ));
        }
    };
    dae.inspect(assert_correlation);
    let json = serde_json::to_string(&dae).unwrap();
    serde_json::from_str::<Dae>(&json)
        .unwrap()
        .inspect(assert_correlation);
    let binary = bincode::serialize(&dae).unwrap();
    bincode::deserialize::<Dae>(&binary)
        .unwrap()
        .inspect(assert_correlation);
}
