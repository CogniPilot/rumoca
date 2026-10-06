use super::*;
use rumoca_core::{SourceMap, TypeId, VarName};

#[derive(Clone, Copy)]
enum Probe {
    Scalar,
    Dot,
    Indexed,
    LargeInteger,
    Tunable,
}

fn model(probe: Probe) -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "translation scalar guard";
    let id = sources.add("guard.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(id, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |model| {
        let literal = model
            .expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Integer(1)))?;
        let integer = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Integer),
                at,
            )
        })?;
        let parameter = model.variables(|variables| {
            variables.parameter(
                VarName::new("p"),
                integer,
                at,
                dae::VariableAttributes {
                    binding: Some(literal),
                    is_tunable: true,
                    ..Default::default()
                },
            )
        })?;
        model.expressions(|expressions| {
            let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
            let zero = expressions.at(at).literal(dae::DaeLiteral::Integer(0))?;
            let lhs = match probe {
                Probe::Scalar => one,
                Probe::Dot => {
                    let minus = expressions.at(at).literal(dae::DaeLiteral::Integer(-1))?;
                    let a = expressions.at(at).array([one, one])?;
                    let b = expressions.at(at).array([one, minus])?;
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Multiply, a, b)?
                }
                Probe::Indexed => {
                    let a = expressions.at(at).array([one, zero])?;
                    expressions.at(at).index(
                        a,
                        [dae::Subscript::Index {
                            expression: one,
                            provenance: at,
                        }],
                    )?
                }
                Probe::LargeInteger => expressions
                    .at(at)
                    .literal(dae::DaeLiteral::Integer((1_i64 << 53) + 1))?,
                Probe::Tunable => expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::Parameter(parameter))?,
            };
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Equal, lhs, zero)?;
            Ok(())
        })
    })
    .unwrap()
}

fn result(probe: Probe) -> Option<bool> {
    model(probe).inspect(|view| {
        let guard = view.expression_id(view.expression_count() - 1).unwrap();
        ScalarSelector::new(view, None)
            .translation_guard(guard)
            .unwrap()
    })
}

#[test]
fn translation_scalar_guard_is_exact_and_rejects_other_projection_rules() {
    assert_eq!(result(Probe::Scalar), Some(false));
    // The canonical dot is zero, not its first scalar product. This probe
    // declines it rather than manufacturing a false branch selection.
    assert_eq!(result(Probe::Dot), None);
    assert_eq!(result(Probe::Indexed), None);
    assert_eq!(result(Probe::LargeInteger), None);
    assert_eq!(result(Probe::Tunable), None);
}
