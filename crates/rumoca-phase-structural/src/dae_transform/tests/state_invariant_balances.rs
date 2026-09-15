//! Invariant support elimination must preserve state/connector equalities.

use super::*;
use crate::dae_transform::equalities::{EqualityAnchor, EqualitySign, SystemEqualities};

fn state_balance(offset: f64, opposite: bool) -> dae::Dae {
    let sign = if opposite { "-" } else { "" };
    let balance = format!("x = {sign}port - support");
    let support = format!("support = {offset}");
    let text = format!("Real x; Real port; Real support; equation {support}; {balance};");
    let mut sources = SourceMap::new();
    let source = sources.add("state_balance.mo", text.as_str());
    let at = |needle: &str| source_provenance(source, &text, needle);
    dae::Dae::construct(sources, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                at("Real x"),
            )
        })?;
        let (x, port, support_var) = model.variables(|variables| {
            Ok((
                variables.state(VarName::new("x"), real, at("Real x"), Default::default())?,
                variables.algebraic(
                    VarName::new("port"),
                    real,
                    at("Real port"),
                    Default::default(),
                )?,
                variables.algebraic(
                    VarName::new("support"),
                    real,
                    at("Real support"),
                    Default::default(),
                )?,
            ))
        })?;
        let residuals = model.expressions(|expressions| {
            let owner = at(&support);
            let support_value = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(support_var))?;
            let offset = expressions
                .at(owner)
                .literal(dae::DaeLiteral::Real(offset))?;
            let pinned = expressions.at(owner).binary(
                dae::BinaryOperator::Subtract,
                support_value,
                offset,
            )?;
            let owner = at(&balance);
            let x = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::State(x))?;
            let port = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(port))?;
            let port = if opposite {
                expressions
                    .at(owner)
                    .unary(dae::UnaryOperator::Negate, port)?
            } else {
                port
            };
            let support = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(support_var))?;
            let rhs = expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, port, support)?;
            let balance = expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, x, rhs)?;
            Ok([pinned, balance])
        })?;
        model.continuous(|continuous| {
            continuous.value_equation(at(&support), residuals[0])?;
            continuous.value_equation(at(&balance), residuals[1])?;
            Ok(())
        })
    })
    .expect("the source-backed state balance constructs")
}

#[test]
fn invariant_support_preserves_state_value_and_derivative_distinctions() {
    for (offset, opposite) in [(0.0, false), (0.0, true), (0.5, false), (0.5, true)] {
        state_balance(offset, opposite).inspect(|view| {
            let index = |name: &str| {
                view.variables()
                    .find(|(_, variable)| variable.name().as_str() == name)
                    .unwrap()
                    .0
                    .index()
            };
            let equalities = SystemEqualities::collect(view);
            let sign = if opposite {
                EqualitySign::Opposite
            } else {
                EqualitySign::Same
            };
            let expected = Some((EqualityAnchor::State(index("x")), sign));
            assert_eq!(
                equalities.anchor_of(index("port")),
                expected,
                "a fixed support disappears from the differentiated balance"
            );
            assert_eq!(
                equalities.value_anchor_of(index("port")),
                if offset == 0.0 { expected } else { None },
                "only a proved zero support disappears from the value balance"
            );
        });
    }
}
