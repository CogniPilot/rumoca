use super::*;
use crate::dae_transform::equalities::{EqualityAnchor, EqualitySign, SystemEqualities};

const SOURCE: &str = r#"
function velocity
  input Real rotation[3,3];
  input Real angular[3];
  output Real result[3];
algorithm
  result := angular;
end velocity;
model ForwardedVelocity
  Real w[3]; Real R[3,3]; Real connector[3];
equation
  w = velocity(R, connector);
end ForwardedVelocity;
"#;

#[derive(Clone, Copy)]
enum FunctionBodyKind {
    Forward,
    Negated,
    Asserting,
}

fn velocity_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    matrix: dae::ValueTypeId<'dae>,
    vector: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
    kind: FunctionBodyKind,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let signature =
        dae::FunctionSignature::new(VarName::new("velocity"), [matrix, vector], [vector], at);
    let (function, ()) = model.function(signature, |model, reservation| {
        model.functions(|functions| {
            functions.parameter(&reservation, VarName::new("rotation"), 0, at)
        })?;
        let angular = model.functions(|functions| {
            functions.parameter(&reservation, VarName::new("angular"), 1, at)
        })?;
        let result = model
            .functions(|functions| functions.output(&reservation, VarName::new("result"), 0, at))?;
        let mut body = model.functions(|functions| functions.begin(reservation, at))?;
        let rhs = model.expressions(|expressions| {
            let angular = expressions.at(at).function_parameter(angular)?;
            if matches!(kind, FunctionBodyKind::Negated) {
                expressions
                    .at(at)
                    .unary(dae::UnaryOperator::Negate, angular)
            } else {
                Ok(angular)
            }
        })?;
        model.functions(|functions| functions.assign(&mut body, result, rhs, at))?;
        if matches!(kind, FunctionBodyKind::Asserting) {
            let (condition, message) = model.expressions(|expressions| {
                Ok((
                    expressions
                        .at(at)
                        .literal(dae::DaeLiteral::Boolean(false))?,
                    expressions
                        .at(at)
                        .literal(dae::DaeLiteral::String("invalid".into()))?,
                ))
            })?;
            model.functions(|functions| functions.assertion(&mut body, condition, message, at))?;
        }
        model.functions(|functions| functions.define(body, at))
    })?;
    Ok(function)
}

fn forwarded_velocity_model(kind: FunctionBodyKind) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("forwarded_velocity.mo", SOURCE);
    dae::Dae::construct(sources, |model| {
        let at = source_provenance(source, SOURCE, "w = velocity(R, connector)");
        let function_at = source_provenance(source, SOURCE, "function velocity");
        let (matrix, vector) = model.types(|types| {
            Ok((
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::array(dae::ScalarType::Real, [3, 3]),
                    at,
                )?,
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::array(dae::ScalarType::Real, [3]),
                    at,
                )?,
            ))
        })?;
        let (w, rotation, connector) = model.variables(|variables| {
            Ok((
                variables.state(VarName::new("w"), vector, at, Default::default())?,
                variables.algebraic(VarName::new("R"), matrix, at, Default::default())?,
                variables.algebraic(VarName::new("connector"), vector, at, Default::default())?,
            ))
        })?;
        let function = velocity_function(model, matrix, vector, function_at, kind)?;
        let residual = model.expressions(|expressions| {
            let w = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(w))?;
            let rotation = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(rotation))?;
            let connector = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(connector))?;
            let rhs = expressions
                .at(at)
                .call(function, 0, [rotation, connector])?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, w, rhs)
        })?;
        model.continuous(|continuous| continuous.value_equation(at, residual).map(|_| ()))
    })
    .expect("checked forwarding function fixture")
}

#[test]
fn a_forwarded_vector_argument_proves_the_connector_state_equality() {
    forwarded_velocity_model(FunctionBodyKind::Forward).inspect(|view| {
        let equalities = SystemEqualities::collect(view);
        assert_eq!(
            equalities.value_anchor_of(2),
            Some((EqualityAnchor::State(0), EqualitySign::Same)),
            "the second argument is the entire returned vector, independent of the first argument"
        );
        assert_eq!(
            equalities.value_anchor_of(1),
            None,
            "the unused matrix is unrelated"
        );
    });
}

#[test]
fn a_nonforwarding_or_asserting_function_does_not_prove_a_forwarding_equality() {
    for kind in [FunctionBodyKind::Negated, FunctionBodyKind::Asserting] {
        forwarded_velocity_model(kind).inspect(|view| {
            assert_eq!(SystemEqualities::collect(view).value_anchor_of(2), None);
        });
    }
}

/// The result can be differentiated without reading R, but retaining the
/// original call requires its whole matrix argument to have an exact value.
fn retained_forwarding_model(define_rotation: bool) -> (dae::Dae, u32) {
    let mut sources = SourceMap::new();
    let source = sources.add("retained_forwarding.mo", SOURCE);
    let at = source_provenance(source, SOURCE, "w = velocity(R, connector)");
    let mut retained = 0;
    let model = dae::Dae::construct(sources, |model| {
        let (matrix, vector) = model.types(|types| {
            Ok((
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::array(dae::ScalarType::Real, [3, 3]),
                    at,
                )?,
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::array(dae::ScalarType::Real, [3]),
                    at,
                )?,
            ))
        })?;
        let (w, driver, rotation) = model.variables(|variables| {
            Ok((
                variables.state(VarName::new("w"), vector, at, Default::default())?,
                variables.state(VarName::new("driver"), vector, at, Default::default())?,
                variables.algebraic(VarName::new("R"), matrix, at, Default::default())?,
            ))
        })?;
        let function = velocity_function(model, matrix, vector, at, FunctionBodyKind::Forward)?;
        let residuals = model.expressions(|expressions| {
            let w = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(w))?;
            let value = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(driver))?;
            let rotation = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(rotation))?;
            let rhs = expressions.at(at).call(function, 0, [rotation, value])?;
            let constraint = expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, w, rhs)?;
            retained = expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, w, value)?
                .index();
            let derivative = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Derivative(driver))?;
            let ode =
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, derivative, value)?;
            let mut residuals = vec![constraint, ode];
            if define_rotation {
                let extent = expressions.at(at).literal(dae::DaeLiteral::Integer(3))?;
                let identity = expressions
                    .at(at)
                    .builtin(dae::PureBuiltin::Identity, [extent])?;
                residuals.push(expressions.at(at).binary(
                    dae::BinaryOperator::Subtract,
                    rotation,
                    identity,
                )?);
            }
            Ok(residuals)
        })?;
        model.continuous(|continuous| {
            for residual in residuals {
                continuous.value_equation(at, residual)?;
            }
            Ok(())
        })
    })
    .expect("checked retained forwarding fixture");
    (model, retained)
}

fn forwarding_candidate(model: &dae::Dae) -> DirectStateConstraint {
    model.inspect(|view| {
        constraints::direct_state_constraints(view)
            .admissible
            .into_iter()
            .find(|candidate| {
                candidate.state == 0
                    && matches!(
                        view.expression(view.expression_id(candidate.rhs as usize).unwrap())
                            .unwrap()
                            .operation(),
                        dae::ExpressionOperation::Call { .. }
                    )
            })
            .expect("the forwarded function has a proved derivative")
    })
}

#[test]
fn a_derivative_proof_does_not_authorize_an_unavailable_manifold_value() {
    let (model, residual) = retained_forwarding_model(false);
    let candidate = forwarding_candidate(&model);
    let retained = ManifoldConstraint {
        expression: residual,
        lifted: None,
    };
    assert!(manifold_is_state_only(&model, &[retained]));
    reconstruction::rebuild_with_state_demotion(&model, candidate)
        .expect("without a retained constraint only the derivative is needed");
    let attempt =
        attempt_direct_candidate(&model, usize::MAX, &[], &candidate, &[retained], &mut ())
            .expect("an unavailable value rejects this candidate without aborting reduction");
    assert!(matches!(attempt, DirectAttempt::Rejected));
}

#[test]
fn a_materializable_whole_call_survives_manifold_state_demotion() {
    let (model, residual) = retained_forwarding_model(true);
    let candidate = forwarding_candidate(&model);
    let retained = ManifoldConstraint {
        expression: residual,
        lifted: None,
    };
    let (rebuilt, manifold) =
        reconstruction::rebuild_with_state_demotion_and_manifold(&model, candidate, &[retained])
            .expect("the supplied matrix value permits exact call reconstruction");
    assert!(manifold_is_state_only(&rebuilt, &manifold));
}
