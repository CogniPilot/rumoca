//! Bounded reductions of source-constructed repeated conditional forwarding.

use super::*;
use crate::tests::{TestSource, scoped_assignment_body};
use rumoca_core::VarName;

fn repeated_forwarding(count: usize) -> dae::Dae {
    let prefix = "function forwarding input Real im; input Real re; input Boolean guard; output Real p; p:=atan2(im,re); ";
    let branch = "if guard then p:=p; else p:=p; end if; ";
    let text = format!(
        "{prefix}{}end forwarding; Real im; Real re; Boolean guard;",
        branch.repeat(count)
    );
    let source = TestSource::new(&text);
    let at = source.at(0, prefix.len() + count * branch.len());
    let branch_spans = (0..count)
        .map(|index| {
            let start = prefix.len() + index * branch.len();
            source.at(start, start + branch.len())
        })
        .collect::<Vec<_>>();
    dae::Dae::construct(source.map, |model| {
        let (real, boolean) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
            ))
        })?;
        let function = define_forwarding(model, real, boolean, at, &branch_spans)?;
        let (im, re, guard) = model.variables(|variables| {
            Ok((
                variables.algebraic(
                    VarName::new("im"),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("re"),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("guard"),
                    boolean,
                    dae::InputVariability::Discrete,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        model.expressions(|expressions| {
            let im = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(im))?;
            let re = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(re))?;
            let guard = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Input(guard))?;
            expressions.at(at).call(function, 0, [im, re, guard])
        })?;
        Ok(())
    })
    .unwrap()
}

fn define_forwarding<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    boolean: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
    branch_spans: &[dae::DaeProvenance],
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let signature = dae::FunctionSignature::new(
        VarName::new("forwarding"),
        [real, real, boolean],
        [real],
        at,
    );
    let (function, ()) = model.function(signature, |model, reservation| {
        let im = model
            .functions(|functions| functions.parameter(&reservation, VarName::new("im"), 0, at))?;
        let re = model
            .functions(|functions| functions.parameter(&reservation, VarName::new("re"), 1, at))?;
        let guard = model.functions(|functions| {
            functions.parameter(&reservation, VarName::new("guard"), 2, at)
        })?;
        let p = model
            .functions(|functions| functions.output(&reservation, VarName::new("p"), 0, at))?;
        let (angle, guard) = model.expressions(|expressions| {
            let im = expressions.at(at).function_parameter(im)?;
            let re = expressions.at(at).function_parameter(re)?;
            let guard = expressions.at(at).function_parameter(guard)?;
            let angle = expressions
                .at(at)
                .builtin(dae::PureBuiltin::Atan2, [im, re])?;
            Ok((angle, guard))
        })?;
        let mut body = model.functions(|functions| functions.begin(reservation, at))?;
        model.functions(|functions| functions.assign(&mut body, p, angle, at))?;
        for &branch_at in branch_spans {
            let incoming = model.functions(|functions| functions.read(&body, p, branch_at))?;
            body = model.functions(|functions| {
                scoped_assignment_body(
                    functions,
                    body,
                    &[p],
                    &[guard],
                    &[vec![incoming]],
                    &[incoming],
                    branch_at,
                )
            })?;
        }
        model.functions(|functions| functions.define(body, at))
    })?;
    Ok(function)
}

#[test]
fn thirty_and_sixty_four_sequential_forwarding_owners_have_linear_completed_work() {
    for count in [30, 64] {
        let model = repeated_forwarding(count);
        model.inspect(|view| {
            let call = (0..view.expression_count())
                .filter_map(|index| view.expression_id(index))
                .find(|&id| {
                    matches!(
                        view.expression(id).unwrap().operation(),
                        dae::ExpressionOperation::Call { .. }
                    )
                })
                .unwrap();
            let conditional_reads = (0..view.expression_count())
                .filter_map(|index| view.expression_id(index))
                .filter(|&id| {
                    matches!(
                        view.expression(id).unwrap().operation(),
                        dae::ExpressionOperation::FunctionConditionalOutput { .. }
                    )
                })
                .count();
            assert!(
                conditional_reads >= count,
                "original issued forwarding chain must remain present"
            );
            let query = ProofQuery::default();
            let result = angle_operands(view, call, None, &query)
                .expect("every original outgoing arm forwards the same angle");
            assert_ne!(result.im, result.re);
            let reductions = query.completed_count();
            assert!(
                reductions <= 8 * (view.expression_count() + count),
                "{count} owners: {reductions} reductions for {} expressions",
                view.expression_count()
            );
            assert_eq!(angle_operands(view, call, None, &query), Some(result));
            assert_eq!(
                query.completed_count(),
                reductions,
                "a repeated query performs no new reductions"
            );
        });
    }
}

#[test]
fn unsupported_literal_reduction_is_completed_once_without_rounding_or_overflow() {
    use NumericSourceScalar::{Integer, Real};
    use dae::BinaryOperator as Op;
    assert_eq!(
        proof::binary_literal(
            Op::Equal,
            Integer(9_007_199_254_740_992),
            Integer(9_007_199_254_740_993)
        ),
        Some(NumericSourceScalar::Boolean(false))
    );
    assert!(
        proof::binary_literal(
            Op::Equal,
            Integer(9_007_199_254_740_993),
            Real(9_007_199_254_740_992.0)
        )
        .is_none()
    );
    assert!(proof::binary_literal(Op::Add, Integer(i64::MAX), Integer(1)).is_none());
    let memo = proof::Memo::<usize, ScalarCoordinate>::default();
    let calls = std::cell::Cell::new(0);
    for _ in 0..64 {
        assert_eq!(
            memo.get_or_compute(0, || {
                calls.set(calls.get() + 1);
                None
            }),
            None
        );
    }
    assert_eq!(
        calls.get(),
        1,
        "a completed unsupported path is not reconsidered"
    );
}

/// `angle(im, re, guard)` takes `atan2(t, re)` where `t` is `im` on both
/// ordered arms, so the angle is of the same two coordinates whichever way the
/// guard falls.
fn agreeing_arms_model() -> dae::Dae {
    let text = "function angle input Real im; input Real re; input Boolean guard; output Real p; Real t; if guard then t:=im; else t:=im; end if; p:=atan2(t,re); end angle; Real x; Real y; Boolean guard;";
    let source = TestSource::new(text);
    let at = source.at(0, text.find(" Real x;").unwrap());
    dae::Dae::construct(source.map, |model| {
        let (real, boolean) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
            ))
        })?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("angle"), [real, real, boolean], [real], at),
            |model, reservation| {
                let (im, re, guard, p, t) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("im"), 0, at)?,
                        functions.parameter(&reservation, VarName::new("re"), 1, at)?,
                        functions.parameter(&reservation, VarName::new("guard"), 2, at)?,
                        functions.output(&reservation, VarName::new("p"), 0, at)?,
                        functions.local(&reservation, VarName::new("t"), real, at)?,
                    ))
                })?;
                let (im, re, guard) = model.expressions(|expressions| {
                    Ok((
                        expressions.at(at).function_parameter(im)?,
                        expressions.at(at).function_parameter(re)?,
                        expressions.at(at).function_parameter(guard)?,
                    ))
                })?;
                let body = model.functions(|functions| functions.begin(reservation, at))?;
                let mut body = model.functions(|functions| {
                    scoped_assignment_body(functions, body, &[t], &[guard], &[vec![im]], &[im], at)
                })?;
                let selected = model.functions(|functions| functions.read(&body, t, at))?;
                let angle = model.expressions(|expressions| {
                    expressions
                        .at(at)
                        .builtin(dae::PureBuiltin::Atan2, [selected, re])
                })?;
                model.functions(|functions| functions.assign(&mut body, p, angle, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        let (x, y, guard) = model.variables(|variables| {
            Ok((
                variables.algebraic(
                    VarName::new("x"),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("y"),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.input(
                    VarName::new("guard"),
                    boolean,
                    dae::InputVariability::Discrete,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        model.expressions(|expressions| {
            let x = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            let y = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(y))?;
            let guard = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Input(guard))?;
            expressions.at(at).call(function, 0, [x, y, guard])
        })?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn arms_that_select_the_same_argument_reduce_to_that_one_coordinate() {
    agreeing_arms_model().inspect(|view| {
        let call = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|&id| {
                matches!(
                    view.expression(id).unwrap().operation(),
                    dae::ExpressionOperation::Call { .. }
                )
            })
            .unwrap();
        let coordinate = |name: &str| {
            let (id, _) = view
                .variables()
                .find(|(_, variable)| variable.name().as_str() == name)
                .unwrap();
            (id.index(), 0)
        };
        assert_eq!(
            angle_operands(view, call, None, &ProofQuery::default()),
            Some(AngleOperands {
                im: coordinate("x"),
                re: coordinate("y"),
            })
        );
    });
}
