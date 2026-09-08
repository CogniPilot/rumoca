use super::*;

/// One index-one constraint block: `c = p*d`, `der(d) = e`, `der(c) = rate`.
///
/// The block is singular on its own and becomes regular once `c` is demoted,
/// so repeating it with disjoint variables builds a system that needs exactly
/// that many simultaneous demotions.
fn independent_constraint_residuals<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    parameter: dae::ParameterId<'dae>,
    constrained: dae::StateId<'dae>,
    driver: dae::StateId<'dae>,
    algebraic: dae::AlgebraicId<'dae>,
    rate: f64,
    spans: [dae::DaeProvenance; 3],
) -> Result<[dae::ExprId<'dae>; 3], dae::DaeConstructionError> {
    let [constraint_at, driver_at, constrained_at] = spans;
    let constrained_value = expressions
        .at(constraint_at)
        .coordinate(dae::CoordinateInput::State(constrained))?;
    let driver_value = expressions
        .at(constraint_at)
        .coordinate(dae::CoordinateInput::State(driver))?;
    let parameter_value = expressions
        .at(constraint_at)
        .coordinate(dae::CoordinateInput::Parameter(parameter))?;
    let scaled = expressions.at(constraint_at).binary(
        dae::BinaryOperator::Multiply,
        parameter_value,
        driver_value,
    )?;
    let constraint = expressions.at(constraint_at).binary(
        dae::BinaryOperator::Subtract,
        constrained_value,
        scaled,
    )?;
    let driver_derivative = expressions
        .at(driver_at)
        .coordinate(dae::CoordinateInput::Derivative(driver))?;
    let algebraic_value = expressions
        .at(driver_at)
        .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
    let driver_definition = expressions.at(driver_at).binary(
        dae::BinaryOperator::Subtract,
        driver_derivative,
        algebraic_value,
    )?;
    let constrained_derivative = expressions
        .at(constrained_at)
        .coordinate(dae::CoordinateInput::Derivative(constrained))?;
    let rate = expressions
        .at(constrained_at)
        .literal(dae::DaeLiteral::Real(rate))?;
    let constrained_definition = expressions.at(constrained_at).binary(
        dae::BinaryOperator::Subtract,
        constrained_derivative,
        rate,
    )?;
    Ok([constraint, driver_definition, constrained_definition])
}

const INDEPENDENT_CONSTRAINT_TEXT: &str = "parameter Real p; Real x; Real y; Real u; Real v; \
     Real a; Real b; Real spare; equation x = p*y; der(y) = a; der(x) = 1; u = p*v; der(v) = b; \
     der(u) = 2;";

/// Build the two-block fixture, optionally adding an algebraic that no equation
/// determines so that the system stays singular after every demotion.
pub(crate) fn independent_constraint_model(spare_unknown: bool) -> dae::Dae {
    let text = INDEPENDENT_CONSTRAINT_TEXT;
    let mut sources = SourceMap::new();
    let source = sources.add("independent_constraints.mo", text);
    let declaration = source_provenance(source, text, "parameter Real p");
    dae::Dae::construct(sources, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let declare = |name: &str| source_provenance(source, text, name);
        let variables = model.variables(|variables| {
            let parameter = variables.parameter(
                VarName::new("p"),
                InstanceId::new(1),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )?;
            let mut state = |name: &str, occurrence| {
                variables.state(
                    VarName::new(name),
                    InstanceId::new(occurrence),
                    real,
                    declare(&format!("Real {name}")),
                    dae::VariableAttributes::default(),
                )
            };
            let states = [
                state("x", 2)?,
                state("y", 3)?,
                state("u", 4)?,
                state("v", 5)?,
            ];
            let mut algebraic = |name: &str, occurrence| {
                variables.algebraic(
                    VarName::new(name),
                    InstanceId::new(occurrence),
                    real,
                    declare(&format!("Real {name}")),
                    dae::VariableAttributes::default(),
                )
            };
            let algebraics = [algebraic("a", 6)?, algebraic("b", 7)?];
            if spare_unknown {
                algebraic("spare", 8)?;
            }
            Ok((parameter, states, algebraics))
        })?;
        let (parameter, [x, y, u, v], [a, b]) = variables;
        let first_spans = [
            declare("x = p*y"),
            declare("der(y) = a"),
            declare("der(x) = 1"),
        ];
        let second_spans = [
            declare("u = p*v"),
            declare("der(v) = b"),
            declare("der(u) = 2"),
        ];
        let residuals = model.expressions(|expressions| {
            let first = independent_constraint_residuals(
                expressions,
                parameter,
                x,
                y,
                a,
                1.0,
                first_spans,
            )?;
            let second = independent_constraint_residuals(
                expressions,
                parameter,
                u,
                v,
                b,
                2.0,
                second_spans,
            )?;
            Ok([first, second])
        })?;
        let equations = [first_spans, second_spans]
            .into_iter()
            .zip(residuals)
            .flat_map(|(spans, block)| spans.into_iter().zip(block))
            .collect::<Vec<_>>();
        model.continuous(|continuous| {
            for (provenance, residual) in equations {
                continuous.value_equation(provenance, residual)?;
            }
            Ok(())
        })?;
        Ok(())
    })
    .expect("independent constraint fixture is valid")
}

#[test]
fn accumulated_state_demotions_reduce_independent_constraints_together() {
    let model = independent_constraint_model(false);
    assert!(
        matches!(
            model.inspect(|view| sort(view).map(|_| ())),
            Err(StructuralError::Singular { .. })
        ),
        "fixture is singular before preparation"
    );

    let prepared =
        prepare_for_solve(&model).expect("independent constraints demote to a regular system");
    assert!(matches!(prepared, PreparedDae::Transformed { .. }));
    prepared.as_dae().inspect(|view| {
        assert!(
            sort(view).is_ok(),
            "accumulated replacement DAE has a perfect matching"
        );
        let role = |name: &str| {
            view.variables()
                .find(|(_, variable)| variable.name().as_str() == name)
                .map(|(_, variable)| variable.role())
                .expect("fixture declaration survives reconstruction")
        };
        assert_eq!(role("x"), dae::VariableRole::Algebraic);
        assert_eq!(role("u"), dae::VariableRole::Algebraic);
        assert_eq!(role("y"), dae::VariableRole::State);
        assert_eq!(role("v"), dae::VariableRole::State);
        assert!(
            (0..view.expression_count())
                .filter_map(|index| view.expression_id(index))
                .filter_map(|id| view.expression(id))
                .all(|expression| {
                    !matches!(
                        expression.operation(),
                        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state))
                            if view
                                .variable(state.into())
                                .is_some_and(|variable| matches!(
                                    variable.name().as_str(),
                                    "x" | "u"
                                ))
                    )
                }),
            "no derivative of a demoted state survives"
        );
        let generated = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter(|expression| {
                expression.provenance().origin()
                    == dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::IndexReduction)
            })
            .count();
        assert!(
            generated >= 2,
            "each accumulated demotion contributes generated derivative expressions"
        );
    });
}

#[test]
fn partial_demotion_progress_still_reports_the_original_singularity() {
    let model = independent_constraint_model(true);
    let Err(StructuralError::Singular {
        n_equations,
        n_unknowns,
        n_matched,
        ..
    }) = model.inspect(|view| sort(view).map(|_| ()))
    else {
        panic!("fixture with a spare unknown is singular")
    };

    let error = match prepare_for_solve(&model) {
        Ok(_) => panic!("an undetermined unknown must not be reported as prepared"),
        Err(error) => error,
    };
    assert!(
        matches!(
            error,
            StructuralError::Singular {
                n_equations: reported_equations,
                n_unknowns: reported_unknowns,
                n_matched: reported_matched,
                ..
            } if (reported_equations, reported_unknowns, reported_matched)
                == (n_equations, n_unknowns, n_matched)
        ),
        "the original singularity is reported, not the partially demoted one"
    );
}

#[test]
fn source_free_reconstruction_failure_has_no_diagnostic_label() {
    use rumoca_core::PhaseError;

    let error = construction_failure(dae::DaeConstructionError::DuplicateTopology {
        kind: "clock ownership",
        span: None,
    });
    assert!(matches!(
        error,
        StructuralError::UnspannedContractViolation { .. }
    ));
    let diagnostic = error.to_diagnostic();
    assert_eq!(
        diagnostic.code.as_deref(),
        Some(crate::diagnostic_codes::ES014_CONTRACT_VIOLATION)
    );
    assert!(diagnostic.labels.is_empty());
}

#[test]
fn source_bearing_reconstruction_failure_retains_its_exact_span() {
    let span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("reconstruction_failure.mo"),
        17,
        24,
    );
    let error = construction_failure(dae::DaeConstructionError::ShapeMismatch { span });
    assert!(matches!(
        error,
        StructuralError::ContractViolation {
            span: retained,
            ..
        } if retained == span
    ));
}
