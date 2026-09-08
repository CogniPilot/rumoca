use super::*;

#[derive(Clone, Copy)]
enum ClockedAliasUse {
    InheritedFirst,
    SelectedFirst,
    Whole,
    DynamicFirst,
    Unrelated,
}

struct IndexedCausalFixture {
    model: dae::Dae,
    dynamic_span: Option<Span>,
}

struct FixtureExpressions<'dae> {
    first_residual: dae::ExprId<'dae>,
    second_residual: dae::ExprId<'dae>,
    output_residual: Option<dae::ExprId<'dae>>,
    clock_target: dae::ExprId<'dae>,
    clock_value: dae::ExprId<'dae>,
}

#[derive(Clone, Copy)]
struct FixtureSpans {
    declaration: dae::DaeProvenance,
    call: dae::DaeProvenance,
    clock: dae::DaeProvenance,
    output: Option<dae::DaeProvenance>,
    dynamic: Option<dae::DaeProvenance>,
}

#[derive(Clone, Copy)]
struct FixtureVariables<'dae> {
    pair: dae::FunctionId<'dae>,
    input: dae::InputId<'dae>,
    dynamic_index: Option<dae::InputId<'dae>>,
    alias: dae::AlgebraicId<'dae>,
    output: Option<dae::AlgebraicId<'dae>>,
    clocked: dae::DiscreteRealId<'dae>,
}

#[derive(Clone, Copy)]
struct ExpressionFixture<'dae> {
    variables: FixtureVariables<'dae>,
    mode: ClockedAliasUse,
    spans: FixtureSpans,
}

fn fixture_source(mode: ClockedAliasUse) -> &'static str {
    match mode {
        ClockedAliasUse::InheritedFirst => {
            "input Real u; Real alias[2]; discrete Real z; equation \
             alias[1] = 0.0; alias[2] = pair(u); \
             when sample(0, 1) then z = (-alias)[1]; end when;"
        }
        ClockedAliasUse::SelectedFirst => {
            "input Real u; Real alias[2]; discrete Real z; equation \
             alias[1] = pair(u); alias[2] = 0.0; \
             when sample(0, 1) then z = alias[1]; end when;"
        }
        ClockedAliasUse::Whole => {
            "input Real u; Real alias[2]; discrete Real z[2]; equation \
             alias[1] = pair(u); alias[2] = 0.0; \
             when sample(0, 1) then z = alias[:]; end when;"
        }
        ClockedAliasUse::DynamicFirst => {
            "input Real u; input Integer i; Real alias[2]; discrete Real z; equation \
             alias[1] = pair(u); alias[2] = 0.0; \
             when sample(0, 1) then z = alias[i]; end when;"
        }
        ClockedAliasUse::Unrelated => {
            "input Real u; Real alias[2]; output Real y; discrete Real z; equation \
             alias[1] = pair(u); alias[2] = 0.0; y = alias[1]; \
             when sample(0, 1) then z = 0.0; end when;"
        }
    }
}

fn literal_index<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    base: dae::ExprId<'dae>,
    ordinal: i64,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let index = expressions
        .at(provenance)
        .literal(dae::DaeLiteral::Integer(ordinal))?;
    expressions.at(provenance).index(
        base,
        [dae::Subscript::Index {
            expression: index,
            provenance,
        }],
    )
}

fn construct_expressions<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    fixture: ExpressionFixture<'dae>,
) -> Result<FixtureExpressions<'dae>, dae::DaeConstructionError> {
    let ExpressionFixture {
        variables,
        mode,
        spans,
    } = fixture;
    let FixtureVariables {
        pair,
        input,
        dynamic_index,
        alias,
        output,
        clocked,
    } = variables;
    let provenance = spans.declaration;
    model.expressions(|expressions| {
        let input = expressions
            .at(spans.call)
            .coordinate(dae::CoordinateInput::Input(input))?;
        let call = expressions.at(spans.call).call(pair, 0, [input])?;
        let zero = expressions
            .at(provenance)
            .literal(dae::DaeLiteral::Real(0.0))?;
        let alias_value = expressions
            .at(provenance)
            .coordinate(dae::CoordinateInput::Algebraic(alias))?;
        let first = literal_index(expressions, alias_value, 1, provenance)?;
        let second = literal_index(expressions, alias_value, 2, provenance)?;
        let (first_value, second_value) = match mode {
            ClockedAliasUse::InheritedFirst => (zero, call),
            ClockedAliasUse::SelectedFirst
            | ClockedAliasUse::Whole
            | ClockedAliasUse::DynamicFirst
            | ClockedAliasUse::Unrelated => (call, zero),
        };
        let first_residual =
            expressions
                .at(provenance)
                .binary(dae::BinaryOperator::Subtract, first, first_value)?;
        let second_residual = expressions.at(provenance).binary(
            dae::BinaryOperator::Subtract,
            second,
            second_value,
        )?;
        let clock_target = expressions
            .at(provenance)
            .coordinate(dae::CoordinateInput::DiscreteReal(clocked))?;
        let clock_value = match mode {
            ClockedAliasUse::InheritedFirst => {
                let negated = expressions
                    .at(provenance)
                    .unary(dae::UnaryOperator::Negate, alias_value)?;
                literal_index(expressions, negated, 1, provenance)?
            }
            ClockedAliasUse::SelectedFirst => {
                literal_index(expressions, alias_value, 1, provenance)?
            }
            ClockedAliasUse::Whole => expressions
                .at(provenance)
                .index(alias_value, [dae::Subscript::Whole { provenance }])?,
            ClockedAliasUse::DynamicFirst => {
                let dynamic_at = spans.dynamic.expect("dynamic selection has provenance");
                let alias_value = expressions
                    .at(dynamic_at)
                    .coordinate(dae::CoordinateInput::Algebraic(alias))?;
                let index = expressions
                    .at(dynamic_at)
                    .coordinate(dae::CoordinateInput::Input(
                        dynamic_index.expect("dynamic selection has an index input"),
                    ))?;
                expressions.at(dynamic_at).index(
                    alias_value,
                    [dae::Subscript::Index {
                        expression: index,
                        provenance: dynamic_at,
                    }],
                )?
            }
            ClockedAliasUse::Unrelated => zero,
        };
        let output_residual = output
            .map(|output| {
                let output = expressions
                    .at(provenance)
                    .coordinate(dae::CoordinateInput::Algebraic(output))?;
                let selected = literal_index(expressions, alias_value, 1, provenance)?;
                expressions
                    .at(provenance)
                    .binary(dae::BinaryOperator::Subtract, output, selected)
            })
            .transpose()?;
        Ok(FixtureExpressions {
            first_residual,
            second_residual,
            output_residual,
            clock_target,
            clock_value,
        })
    })
}

fn declare_fixture_variables<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    mode: ClockedAliasUse,
    spans: FixtureSpans,
) -> Result<FixtureVariables<'dae>, dae::DaeConstructionError> {
    let (real, vector, integer) = model.types(|types| {
        Ok((
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                spans.declaration,
            )?,
            types.intern(
                TypeId::new(1),
                dae::ValueType::array(dae::ScalarType::Real, [2]),
                spans.declaration,
            )?,
            types.intern(
                TypeId::new(2),
                dae::ValueType::scalar(dae::ScalarType::Integer),
                spans.declaration,
            )?,
        ))
    })?;
    let pair = define_scalar_pair(model, real, spans.call)?;
    let input = model.variables(|variables| {
        variables.input(
            VarName::new("u"),
            rumoca_core::InstanceId::new(1),
            real,
            dae::InputVariability::Continuous,
            spans.declaration,
            dae::VariableAttributes {
                causality: dae::VariableCausality::Input,
                ..Default::default()
            },
        )
    })?;
    let dynamic_index = spans
        .dynamic
        .map(|provenance| {
            model.variables(|variables| {
                variables.input(
                    VarName::new("i"),
                    rumoca_core::InstanceId::new(2),
                    integer,
                    dae::InputVariability::Discrete,
                    provenance,
                    dae::VariableAttributes {
                        causality: dae::VariableCausality::Input,
                        ..Default::default()
                    },
                )
            })
        })
        .transpose()?;
    let alias = model.variables(|variables| {
        variables.algebraic(
            VarName::new("alias"),
            rumoca_core::InstanceId::new(3),
            vector,
            spans.declaration,
            dae::VariableAttributes::default(),
        )
    })?;
    let output = spans
        .output
        .map(|provenance| {
            model.variables(|variables| {
                variables.output(
                    VarName::new("y"),
                    rumoca_core::InstanceId::new(4),
                    real,
                    provenance,
                    dae::VariableAttributes {
                        causality: dae::VariableCausality::Output,
                        ..Default::default()
                    },
                )
            })
        })
        .transpose()?;
    let clocked_type = if matches!(mode, ClockedAliasUse::Whole) {
        vector
    } else {
        real
    };
    let clocked = model.variables(|variables| {
        variables.discrete_real(
            VarName::new("z"),
            rumoca_core::InstanceId::new(5),
            clocked_type,
            spans.declaration,
            dae::VariableAttributes::default(),
        )
    })?;
    Ok(FixtureVariables {
        pair,
        input,
        dynamic_index,
        alias,
        output,
        clocked,
    })
}

fn construct_fixture_model<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    mode: ClockedAliasUse,
    spans: FixtureSpans,
) -> Result<(), dae::DaeConstructionError> {
    let variables = declare_fixture_variables(model, mode, spans)?;
    let expressions = construct_expressions(
        model,
        ExpressionFixture {
            variables,
            mode,
            spans,
        },
    )?;
    model.continuous(|continuous| {
        continuous.equation(spans.declaration, |equation| {
            equation.residual(expressions.first_residual)
        })?;
        continuous.equation(spans.declaration, |equation| {
            equation.residual(expressions.second_residual)
        })?;
        if let Some(residual) = expressions.output_residual {
            continuous.equation(spans.declaration, |equation| equation.residual(residual))?;
        }
        Ok(())
    })?;
    let clock = periodic_clock(model, spans.clock)?;
    model.clocks(|clocks| clocks.own_discrete_real(clock, variables.clocked, spans.declaration))?;
    let tick = model.conditions(|conditions| {
        let tick = conditions.reserve(spans.clock)?;
        conditions.define(tick, dae::ConditionInput::Clock(clock), spans.clock)?;
        Ok(tick)
    })?;
    define_when_real_equation(
        model,
        tick,
        tick,
        spans.declaration,
        expressions.clock_target,
        expressions.clock_value,
    )
}

fn indexed_causal_fixture(mode: ClockedAliasUse) -> IndexedCausalFixture {
    let text = fixture_source(mode);
    let mut sources = SourceMap::new();
    let source = sources.add("indexed-causal-call.mo", text);
    let spans = FixtureSpans {
        declaration: at(source, text, "Real alias[2]"),
        call: at(source, text, "pair(u)"),
        clock: at(source, text, "sample(0, 1)"),
        output: matches!(mode, ClockedAliasUse::Unrelated)
            .then(|| at(source, text, "output Real y")),
        dynamic: matches!(mode, ClockedAliasUse::DynamicFirst)
            .then(|| at(source, text, "alias[i]")),
    };
    let dynamic_span = spans.dynamic.map(dae::DaeProvenance::span);
    let model = dae::Dae::construct(sources, |model| construct_fixture_model(model, mode, spans))
        .expect("checked indexed causal-call fixture");
    IndexedCausalFixture {
        model,
        dynamic_span,
    }
}

fn project_package<'inv>(
    brand: rumoca_core::TargetInvocationBrand<'inv>,
    model: &dae::Dae,
) -> Result<rumoca_ir_galec::TracedAlgorithmCodeProduct<'inv>, Vec<GalecTargetError>> {
    lower_to_algorithm_code(
        brand,
        &GalecInput::new(model, "IndexedCausalCall"),
        &GalecOptions::new(
            rumoca_ir_galec::package::AlgorithmCodeArithmeticProfile::construct(
                rumoca_ir_galec::package::AlgorithmCodeRealFormat::Binary64,
                rumoca_ir_galec::package::AlgorithmCodeIntegerFormat::I32,
                rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
            ),
        ),
    )
}

#[test]
fn inherited_projection_does_not_claim_an_unselected_scalar_call() {
    rumoca_core::with_target_invocation_brand(|brand| {
        let fixture = indexed_causal_fixture(ClockedAliasUse::InheritedFirst);
        let package = project_package(brand, &fixture.model)
            .expect("the clock projection selects the call-free scalar definition");
        assert_eq!(
            multi_assignment_call_sites(&package.package().block().do_step.statements, "pair"),
            1,
            "only the causal assignment of alias[2] evaluates the issued call"
        );
    });
}

#[test]
fn selected_scalar_call_is_retained_across_clocked_and_causal_regions() {
    rumoca_core::with_target_invocation_brand(|brand| {
        let fixture = indexed_causal_fixture(ClockedAliasUse::SelectedFirst);
        let package = project_package(brand, &fixture.model)
            .expect("the selected scalar call has one retained execution owner");
        assert_eq!(
            multi_assignment_call_sites(&package.package().block().do_step.statements, "pair"),
            1,
            "the causal store reuses the clock-scheduled scalar result"
        );
    });
}

#[test]
fn whole_subscript_reuses_the_inherited_scalar_call_owner() {
    rumoca_core::with_target_invocation_brand(|brand| {
        let fixture = indexed_causal_fixture(ClockedAliasUse::Whole);
        let package = project_package(brand, &fixture.model)
            .expect("the whole subscript retains its selected scalar call result");
        assert_eq!(
            multi_assignment_call_sites(&package.package().block().do_step.statements, "pair"),
            1,
            "whole and scalar causal consumers share one issued call"
        );
    });
}

#[test]
fn dynamic_scalar_causal_read_fails_before_uninitialized_storage_is_emitted() {
    let fixture = indexed_causal_fixture(ClockedAliasUse::DynamicFirst);
    let errors = rumoca_core::with_target_invocation_brand(|brand| {
        project_package(brand, &fixture.model)
            .expect_err("a dynamic scalar definition has no scheduled materialization owner")
    });
    assert!(matches!(
        errors.as_slice(),
        [GalecTargetError::UnsupportedFeature {
            feature,
            span: Some(span),
            ..
        }] if feature == "dynamic-scalar-causal-read"
            && Some(*span) == fixture.dynamic_span
    ));
}

#[test]
fn causal_output_reads_materialized_indexed_local_without_reexecuting_call() {
    rumoca_core::with_target_invocation_brand(|brand| {
        let fixture = indexed_causal_fixture(ClockedAliasUse::Unrelated);
        let package = project_package(brand, &fixture.model)
            .expect("the causal output reads the already materialized local");
        let statements = &package.package().block().do_step.statements;
        assert_eq!(multi_assignment_call_sites(statements, "pair"), 1);
        let call = statements
        .iter()
        .position(|statement| {
            matches!(
                &statement.node,
                gast::Statement::MultiAssignment { call, .. } if call.function.lexeme() == "pair"
            )
        })
        .expect("the causal local evaluates its issued call");
        let alias = statements
            .iter()
            .position(|statement| {
                matches!(
                    &statement.node,
                    gast::Statement::Assignment {
                        target: gast::Reference::Local(part),
                        ..
                    } if part.name.lexeme() == "alias"
                        && part.subscripts == [gast::Expression::Integer(1)]
                )
            })
            .expect("the call result is committed to alias[1]");
        let output = statements
            .iter()
            .position(|statement| {
                matches!(
                    &statement.node,
                    gast::Statement::Assignment {
                        target: gast::Reference::State(parts),
                        value: gast::Expression::Ref(gast::Reference::Local(part)),
                    } if parts.len() == 1
                        && parts[0].name.lexeme() == "y"
                        && part.name.lexeme() == "alias"
                        && part.subscripts == [gast::Expression::Integer(1)]
                )
            })
            .expect("the output reads the materialized causal local");
        assert!(call < alias && alias < output);
    });
}
