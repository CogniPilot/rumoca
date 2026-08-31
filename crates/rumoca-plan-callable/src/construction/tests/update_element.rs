use rumoca_core::{OperationContractKey, SourceMap, Span, VarName};
use rumoca_ir_dae::{
    BinaryOperator, Dae, DaeLiteral, ExpressionOperation, FunctionSignature, ScalarType, ValueType,
};

use crate::{
    CallableDefinitionSource, CallableExpressionSource, CallableFunctionSource, CallablePlan,
    CallablePlanConstruction, PlanConstructionError,
};

#[derive(Clone, Copy)]
enum CoordinateCase {
    Exact,
    Omitted,
    WrongKind,
    WrongSource,
    ForeignOwner,
}

fn static_array_update_function() -> Dae {
    let mut source_map = SourceMap::new();
    let source = source_map.add(
        "callable-static-update.mo",
        "function update output Real y[2]; algorithm y := array_update({1.0,2.0}, 3.0, 2); end update;",
    );
    let span = Span::from_offsets(source, 0, 1);
    let at = rumoca_ir_dae::DaeProvenance::source(span).expect("test span is source-backed");
    Dae::construct(source_map, |dae| {
        let reals =
            dae.types(|types| types.derived(ValueType::array(ScalarType::Real, [2]), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("update"), [], [reals], at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let one = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(1.0)))?;
                let two = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(2.0)))?;
                let base = dae.expressions(|expressions| expressions.at(at).array([one, two]))?;
                let value = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(3.0)))?;
                let coordinate = dae.expressions(|expressions| {
                    expressions.at(at).literal(DaeLiteral::Integer(2))
                })?;
                let update = dae.expressions(|expressions| {
                    expressions.at(at).array_update(
                        base,
                        value,
                        [rumoca_ir_dae::Subscript::Index {
                            expression: coordinate,
                            provenance: at,
                        }],
                    )
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                dae.functions(|functions| functions.assign(&mut body, output, update, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .expect("static update fixture is construction-valid")
}

fn computed_array_update_functions() -> Dae {
    let mut source_map = SourceMap::new();
    let source = source_map.add(
        "callable-computed-update.mo",
        "function donor output Integer i; algorithm i := 2; end donor; function update input Integer i; output Real y[2]; algorithm y := array_update({1.0,2.0}, 3.0, i + 0); end update;",
    );
    let span = Span::from_offsets(source, 0, 1);
    let at = rumoca_ir_dae::DaeProvenance::source(span).expect("test span is source-backed");
    Dae::construct(source_map, |dae| {
        let integer =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Integer), at))?;
        let reals =
            dae.types(|types| types.derived(ValueType::array(ScalarType::Real, [2]), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("donor"), [], [integer], at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("i"), 0, at)
                })?;
                let value = dae.expressions(|expressions| {
                    expressions.at(at).literal(DaeLiteral::Integer(2))
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                dae.functions(|functions| functions.assign(&mut body, output, value, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        dae.function(
            FunctionSignature::new(VarName::new("update"), [integer], [reals], at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("i"), 0, at)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let one = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(1.0)))?;
                let two = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(2.0)))?;
                let base = dae.expressions(|expressions| expressions.at(at).array([one, two]))?;
                let replacement = dae
                    .expressions(|expressions| expressions.at(at).literal(DaeLiteral::Real(3.0)))?;
                let parameter_value = dae
                    .expressions(|expressions| expressions.at(at).function_parameter(parameter))?;
                let zero = dae.expressions(|expressions| {
                    expressions.at(at).literal(DaeLiteral::Integer(0))
                })?;
                let coordinate = dae.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(BinaryOperator::Add, parameter_value, zero)
                })?;
                let update = dae.expressions(|expressions| {
                    expressions.at(at).array_update(
                        base,
                        replacement,
                        [rumoca_ir_dae::Subscript::Index {
                            expression: coordinate,
                            provenance: at,
                        }],
                    )
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                dae.functions(|functions| functions.assign(&mut body, output, update, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .expect("computed update fixtures are construction-valid")
}

fn construct_static_update() -> Result<CallablePlan, PlanConstructionError> {
    CallablePlan::construct::<PlanConstructionError, _>(
        static_array_update_function(),
        |dae, construction| {
            let function = construction.functions().next().expect("one function");
            let sources = construction.expressions().collect::<Vec<_>>();
            let definition = construction.definitions().next().expect("one definition");
            let real_literal = |wanted| {
                sources
                    .iter()
                    .copied()
                    .find(|source| {
                        matches!(
                            dae.exact_expression(source.expression()).operation(),
                            ExpressionOperation::Literal(DaeLiteral::Real(value)) if *value == wanted
                        )
                    })
                    .expect("exact Real literal")
            };
            let coordinate = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        ExpressionOperation::Literal(DaeLiteral::Integer(2))
                    )
                })
                .expect("literal coordinate");
            let aggregate = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        ExpressionOperation::Array(_)
                    )
                })
                .expect("aggregate");
            let update = sources
                .iter()
                .copied()
                .find(|source| {
                    matches!(
                        dae.exact_expression(source.expression()).operation(),
                        ExpressionOperation::ArrayUpdate { .. }
                    )
                })
                .expect("update");
            let owner = construction.add_owner(function)?;
            let scope = construction.root_scope(owner);
            let one = construction.add_value_operation(
                scope,
                real_literal(1.0),
                OperationContractKey::Constant,
                &[],
            )?;
            let two = construction.add_value_operation(
                scope,
                real_literal(2.0),
                OperationContractKey::Constant,
                &[],
            )?;
            let base = construction.add_value_operation(
                scope,
                aggregate,
                OperationContractKey::ConstructAggregate,
                &[one, two],
            )?;
            let replacement = construction.add_value_operation(
                scope,
                real_literal(3.0),
                OperationContractKey::Constant,
                &[],
            )?;
            let coordinate = construction.add_value_operation(
                scope,
                coordinate,
                OperationContractKey::Constant,
                &[],
            )?;
            let updated =
                construction.add_array_update(scope, update, base, replacement, &[coordinate])?;
            construction.add_store(scope, definition, updated)
        },
    )
}

/// Every exact DAE source the computed-update fixture publishes, located once
/// so the construction below reads as the plan it builds.
struct ComputedUpdateSources<'plan, 'dae> {
    donor: CallableFunctionSource<'plan, 'dae>,
    donor_expression: CallableExpressionSource<'plan, 'dae>,
    donor_definition: CallableDefinitionSource<'plan, 'dae>,
    update_function: CallableFunctionSource<'plan, 'dae>,
    definition: CallableDefinitionSource<'plan, 'dae>,
    one: CallableExpressionSource<'plan, 'dae>,
    two: CallableExpressionSource<'plan, 'dae>,
    three: CallableExpressionSource<'plan, 'dae>,
    parameter: CallableExpressionSource<'plan, 'dae>,
    zero: CallableExpressionSource<'plan, 'dae>,
    coordinate: CallableExpressionSource<'plan, 'dae>,
    aggregate: CallableExpressionSource<'plan, 'dae>,
    update: CallableExpressionSource<'plan, 'dae>,
}

/// The one expression of `sources` whose DAE operation satisfies `predicate`.
fn expression_matching<'plan, 'dae>(
    dae: rumoca_ir_dae::DaeView<'dae>,
    sources: &[CallableExpressionSource<'plan, 'dae>],
    label: &str,
    predicate: impl Fn(ExpressionOperation<'dae>) -> bool,
) -> CallableExpressionSource<'plan, 'dae> {
    sources
        .iter()
        .copied()
        .find(|source| predicate(dae.exact_expression(source.expression()).operation()))
        .unwrap_or_else(|| panic!("{label}"))
}

fn computed_update_sources<'plan, 'dae>(
    dae: rumoca_ir_dae::DaeView<'dae>,
    construction: &CallablePlanConstruction<'plan, 'dae>,
) -> ComputedUpdateSources<'plan, 'dae> {
    let functions = construction.functions().collect::<Vec<_>>();
    let named = |wanted: &str| {
        functions
            .iter()
            .copied()
            .find(|source| dae.exact_function(source.function()).name().as_str() == wanted)
            .unwrap_or_else(|| panic!("{wanted} function"))
    };
    let donor = named("donor");
    let update_function = named("update");
    let expressions = construction.expressions().collect::<Vec<_>>();
    let definitions = construction.definitions().collect::<Vec<_>>();
    let owned_definition = |owner: rumoca_ir_dae::FunctionId<'dae>, label: &str| {
        definitions
            .iter()
            .copied()
            .find(|source| source.function() == owner)
            .unwrap_or_else(|| panic!("{label}"))
    };
    let donor_expression = expressions
        .iter()
        .copied()
        .find(|source| source.function() == donor.function())
        .expect("donor literal");
    let sources = expressions
        .iter()
        .copied()
        .filter(|source| source.function() == update_function.function())
        .collect::<Vec<_>>();
    let real_literal = |wanted: f64| {
        expression_matching(
            dae,
            &sources,
            "exact Real literal",
            move |operation| matches!(operation, ExpressionOperation::Literal(DaeLiteral::Real(value)) if *value == wanted),
        )
    };
    ComputedUpdateSources {
        donor,
        donor_expression,
        donor_definition: owned_definition(donor.function(), "donor definition"),
        update_function,
        definition: owned_definition(update_function.function(), "update definition"),
        one: real_literal(1.0),
        two: real_literal(2.0),
        three: real_literal(3.0),
        parameter: expression_matching(dae, &sources, "dynamic coordinate source", |operation| {
            matches!(operation, ExpressionOperation::Coordinate(_))
        }),
        zero: expression_matching(dae, &sources, "integer zero", |operation| {
            matches!(
                operation,
                ExpressionOperation::Literal(DaeLiteral::Integer(0))
            )
        }),
        coordinate: expression_matching(dae, &sources, "computed coordinate", |operation| {
            matches!(
                operation,
                ExpressionOperation::Binary {
                    operator: BinaryOperator::Add,
                    ..
                }
            )
        }),
        aggregate: expression_matching(dae, &sources, "aggregate", |operation| {
            matches!(operation, ExpressionOperation::Array(_))
        }),
        update: expression_matching(dae, &sources, "update", |operation| {
            matches!(operation, ExpressionOperation::ArrayUpdate { .. })
        }),
    }
}

fn construct_computed_update(case: CoordinateCase) -> Result<CallablePlan, PlanConstructionError> {
    CallablePlan::construct::<PlanConstructionError, _>(
        computed_array_update_functions(),
        |dae, construction| {
            let found = computed_update_sources(dae, construction);
            let donor_owner = construction.add_owner(found.donor)?;
            let update_owner = construction.add_owner(found.update_function)?;
            let donor_scope = construction.root_scope(donor_owner);
            let update_scope = construction.root_scope(update_owner);
            let donor_value = construction.add_value_operation(
                donor_scope,
                found.donor_expression,
                OperationContractKey::Constant,
                &[],
            )?;
            construction.add_store(donor_scope, found.donor_definition, donor_value)?;
            let one = construction.add_value_operation(
                update_scope,
                found.one,
                OperationContractKey::Constant,
                &[],
            )?;
            let two = construction.add_value_operation(
                update_scope,
                found.two,
                OperationContractKey::Constant,
                &[],
            )?;
            let base = construction.add_value_operation(
                update_scope,
                found.aggregate,
                OperationContractKey::ConstructAggregate,
                &[one, two],
            )?;
            let replacement = construction.add_value_operation(
                update_scope,
                found.three,
                OperationContractKey::Constant,
                &[],
            )?;
            let parameter_value = construction.add_value_operation(
                update_scope,
                found.parameter,
                OperationContractKey::Load,
                &[],
            )?;
            let zero_value = construction.add_value_operation(
                update_scope,
                found.zero,
                OperationContractKey::Constant,
                &[],
            )?;
            let coordinate_value = construction.add_value_operation(
                update_scope,
                found.coordinate,
                OperationContractKey::BinaryAddInteger,
                &[parameter_value, zero_value],
            )?;
            let indices = match case {
                CoordinateCase::Exact => vec![coordinate_value],
                CoordinateCase::Omitted => Vec::new(),
                CoordinateCase::WrongKind => vec![replacement],
                CoordinateCase::WrongSource => vec![parameter_value],
                CoordinateCase::ForeignOwner => vec![donor_value],
            };
            let updated = construction.add_array_update(
                update_scope,
                found.update,
                base,
                replacement,
                &indices,
            )?;
            construction.add_store(update_scope, found.definition, updated)
        },
    )
}

#[test]
fn static_update_retains_its_exact_integer_coordinate_operand() {
    let plan = construct_static_update().expect("ExpMixed-style static update constructs");
    plan.inspect(|view| {
        let update = view
            .operations()
            .find(|operation| operation.contract() == OperationContractKey::UpdateElement)
            .expect("one update operation");
        assert_eq!(update.operands().len(), 3);
    });
}

#[test]
fn computed_dynamic_update_retains_its_exact_integer_coordinate_operand() {
    let plan = construct_computed_update(CoordinateCase::Exact)
        .expect("a checked dynamic Integer coordinate constructs");
    plan.inspect(|view| {
        let update = view
            .operations()
            .find(|operation| operation.contract() == OperationContractKey::UpdateElement)
            .expect("one update operation");
        let coordinate = update.operands().nth(2).expect("coordinate operand");
        let coordinate = view
            .values()
            .find(|value| value.id() == coordinate)
            .expect("coordinate value view");
        assert!(matches!(
            coordinate.source_expression().operation(),
            ExpressionOperation::Binary {
                operator: BinaryOperator::Add,
                ..
            }
        ));
    });
}

#[test]
fn update_refuses_omitted_wrong_kind_wrong_source_and_foreign_coordinates() {
    for case in [
        CoordinateCase::Omitted,
        CoordinateCase::WrongKind,
        CoordinateCase::WrongSource,
        CoordinateCase::ForeignOwner,
    ] {
        assert!(matches!(
            construct_computed_update(case),
            Err(PlanConstructionError::InvalidOperation { .. })
        ));
    }
}
