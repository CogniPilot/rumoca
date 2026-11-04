use super::*;
use rumoca_core::{SourceMap, Span, StateSelect, VarName};

/// Checked DAE for the non-differentiable Integer preference used by the
/// compile-only Modelica regression in suite_core/formal_analysis_reuse.rs.
fn integer_preference() -> dae::Dae {
    let text = "model Withheld Real x; Real y(stateSelect=StateSelect.prefer); Integer k; equation der(x)=-x; k=integer(3*x); y=x+k; end Withheld;";
    let mut sources = SourceMap::new();
    let source = sources.add("Withheld.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |model| {
        let (real, integer) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
            ))
        })?;
        let (x, y, k) = model.variables(|variables| {
            Ok((
                variables.state(VarName::new("x"), real, at, Default::default())?,
                variables.algebraic(
                    VarName::new("y"),
                    real,
                    at,
                    dae::VariableAttributes {
                        state_select: StateSelect::Prefer,
                        ..Default::default()
                    },
                )?,
                variables.discrete_value(VarName::new("k"), integer, at, Default::default())?,
            ))
        })?;
        let (dynamics, observation, discrete) = model.expressions(|expressions| {
            let x_value = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(x))?;
            let dx = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Derivative(x))?;
            let y_value = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(y))?;
            let k_value = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::DiscreteValue(k))?;
            let dynamics = expressions
                .at(at)
                .binary(dae::BinaryOperator::Add, dx, x_value)?;
            let observed = expressions
                .at(at)
                .binary(dae::BinaryOperator::Add, x_value, k_value)?;
            let observation =
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, y_value, observed)?;
            let three = expressions.at(at).literal(dae::DaeLiteral::Real(3.0))?;
            let product =
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Multiply, three, x_value)?;
            let discrete = expressions
                .at(at)
                .builtin(dae::PureBuiltin::Integer, [product])?;
            Ok((dynamics, observation, discrete))
        })?;
        model.continuous(|continuous| {
            continuous.value_equation(at, dynamics)?;
            continuous.value_equation(at, observation)?;
            Ok(())
        })?;
        model.b1c([k], |topology| {
            topology
                .owner(at, [k], |owner| owner.always(at, [(discrete, at)]))
                .map(|_| ())
        })?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn withholding_a_nondifferentiable_preference_reuses_source_analysis() {
    let source = integer_preference();
    let before = serde_json::to_vec(&source).unwrap();
    crate::differential_structure::ANALYSES.with(|count| count.set(0));
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|system| {
        assert_eq!(system.formal_dimension(), 1);
        for (id, variable) in system.source.variables() {
            match variable.name().as_str() {
                "x" => assert!(system.coordinate(id, 1).is_some()),
                "y" => assert!(system.coordinate(id, 1).is_none()),
                _ => {}
            }
        }
    });
    assert_eq!(serde_json::to_vec(&source).unwrap(), before);
    crate::differential_structure::ANALYSES.with(|count| {
        assert_eq!(
            count.get(),
            1,
            "withholding leaves the analyzed source and order bounds unchanged"
        );
    });
}
