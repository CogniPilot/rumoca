use super::*;

/// A statically empty compact loop domain is well-formed MLS §11.2.2, and the
/// guarded transition inside it therefore joins no value. Construction must
/// admit that vacuous conditional and store no assignment group for it, because
/// there is no definition an assignment group could own.
#[test]
fn a_conditional_that_joins_no_value_is_admitted_and_stores_no_assignment_group() {
    let source = TestSource::new(
        "function f output Real y; algorithm y := 0; for k in 1:0 loop if k > 0 then end if; end for; end f;",
    );
    let function_at = source.source("function f", 0);
    let output_at = source.source("output Real y", 0);
    let assignment_at = source.source("y := 0", 0);
    let zero_at = source.source("0", 0);
    let loop_at = source.source("for k in 1:0 loop", 0);
    let condition_at = source.source("k > 0", 0);
    let condition_zero_at = source.source("0", 2);
    let conditional_at = source.source("if k > 0 then end if", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let zero = dae.expressions(|expressions| {
                    expressions.at(zero_at).literal(DaeLiteral::Real(0.0))
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, zero, assignment_at)
                })?;
                let domain = empty_loop_domain(dae, loop_at)?;
                let mut loop_body = dae
                    .functions(|functions| functions.begin_loop(body, domain, [output], loop_at))?;
                let k = dae.expressions(|expressions| {
                    expressions
                        .at(condition_at)
                        .binder(DomainBinderId::from_raw(domain.index(), 0))
                })?;
                let bound = dae.expressions(|expressions| {
                    expressions
                        .at(condition_zero_at)
                        .literal(DaeLiteral::Integer(0))
                })?;
                let condition = dae.expressions(|expressions| {
                    expressions
                        .at(condition_at)
                        .binary(BinaryOperator::Greater, k, bound)
                })?;
                assert_forged_vacuous_joins(dae, &mut loop_body, k, condition, conditional_at);
                let no_condition = dae.functions(|functions| {
                    functions.assign_conditional_all_loop(
                        &mut loop_body,
                        &[],
                        &[],
                        &[],
                        &[],
                        conditional_at,
                    )
                });
                assert!(matches!(
                    no_condition,
                    Err(DaeConstructionError::InvalidArity {
                        expected: 0,
                        found: 0,
                        ..
                    })
                ));

                dae.functions(|functions| {
                    functions.assign_conditional_all_loop(
                        &mut loop_body,
                        &[],
                        &[condition],
                        &[Vec::new()],
                        &[],
                        conditional_at,
                    )
                })?;
                let body = dae.functions(|functions| functions.finish_loop(loop_body, loop_at))?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )?;
        Ok(())
    })
    .expect("a conditional over a statically empty domain constructs vacuously");

    dae.inspect(assert_vacuous_conditional);
    let encoded = serde_json::to_string(&dae).unwrap();
    serde_json::from_str::<Dae>(&encoded)
        .unwrap()
        .inspect(assert_vacuous_conditional);
    let binary = bincode::serialize(&dae).unwrap();
    bincode::deserialize::<Dae>(&binary)
        .unwrap()
        .inspect(assert_vacuous_conditional);
}

/// The statically empty `for k in 1:0` domain the vacuous conditional lives in.
fn empty_loop_domain<'dae>(
    dae: &mut DaeConstruction<'dae>,
    loop_at: DaeProvenance,
) -> Result<DomainId<'dae>, DaeConstructionError> {
    dae.domains(|domains| {
        domains.structured(
            StructuredIndexDomain {
                binders: vec![StructuredIndexBinder {
                    id: rumoca_core::StructuredIndexBinderId::new(0),
                    display_name: "k".to_string(),
                    lower: 1,
                    upper: 0,
                    step: 1,
                }],
            },
            loop_at,
        )
    })
}

fn assert_forged_vacuous_joins<'dae>(
    dae: &mut DaeConstruction<'dae>,
    loop_body: &mut FunctionLoop<'dae>,
    k: ExprId<'dae>,
    condition: ExprId<'dae>,
    conditional_at: DaeProvenance,
) {
    // A forged join keeps its rectangularity proof: an empty target
    // list admits only empty branches and an empty fallback.
    for (branches, fallback) in [(vec![vec![k]], Vec::new()), (vec![Vec::new()], vec![k])] {
        let forged = dae.functions(|functions| {
            functions.assign_conditional_all_loop(
                loop_body,
                &[],
                &[condition],
                &branches,
                &fallback,
                conditional_at,
            )
        });
        assert!(
            matches!(
                forged,
                Err(DaeConstructionError::InvalidArity {
                    expected: 0,
                    found: 1,
                    ..
                })
            ),
            "a forged vacuous join must keep its arity proof: {forged:?}"
        );
    }
}

fn assert_vacuous_conditional(view: DaeView<'_>) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    let statements = function.statements().collect::<Vec<_>>();
    let FunctionStatementView::For {
        fold, statements, ..
    } = statements[1].clone()
    else {
        panic!("the empty-domain loop stays a compact fold");
    };
    assert_eq!(
        statements.count(),
        0,
        "a vacuous conditional owns no assignment group"
    );
    let fold = view.function_fold(fold).unwrap();
    assert_eq!(view.domain(fold.domain()).unwrap().scalar_count(), 0);
    // The carried value leaves the fold exactly as it entered it.
    assert_eq!(fold.update_values().len(), 1);
    assert_eq!(
        fold.update_values().rhs(0),
        fold.parameter_values().rhs(0),
        "an empty domain carries its value through unchanged"
    );
}

#[test]
fn correlated_function_conditional_round_trips_and_rejects_duplicate_targets() {
    let source = TestSource::new(
        "function choose input Boolean c; output Real x; output Real y; end choose;",
    );
    let at = source.source("function choose", 0);
    let dae = correlated_conditional_fixture(source, at);

    assert_correlated_conditional_roundtrip(&dae);
}

fn correlated_conditional_fixture(source: TestSource, at: DaeProvenance) -> Dae {
    Dae::construct(source.map, |dae| {
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
    .expect("checked conditional correlation constructs atomically")
}

fn assert_correlated_conditional(view: DaeView<'_>) {
    view.with_callable_source_inventory(|inventory| {
        let conditional = inventory
            .conditionals()
            .next()
            .expect("one exact callable conditional occurrence");
        assert_eq!(inventory.conditionals().len(), 1);
        assert_eq!(conditional.definitions().len(), 2);
        assert_eq!(conditional.conditions().len(), 1);
        assert_eq!(conditional.branch_count(), 1);
        assert_eq!(conditional.branch(0).unwrap().len(), 2);
        assert_eq!(conditional.fallback().len(), 2);
    });
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
}

fn assert_correlated_conditional_roundtrip(dae: &Dae) {
    dae.inspect(assert_correlated_conditional);
    let json = serde_json::to_string(dae).unwrap();
    serde_json::from_str::<Dae>(&json)
        .unwrap()
        .inspect(assert_correlated_conditional);
    let binary = bincode::serialize(dae).unwrap();
    bincode::deserialize::<Dae>(&binary)
        .unwrap()
        .inspect(assert_correlated_conditional);
}
