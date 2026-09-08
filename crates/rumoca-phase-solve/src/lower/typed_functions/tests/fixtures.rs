//! Checked DAE fixtures the typed-lowering tests share.
//!
//! Each builder here constructs one complete function whose loop nesting is the
//! point of the test that uses it. Keeping the constructions in one module lets
//! a test state the property it proves - the numbers it asserts - instead of
//! restating a hundred lines of loop construction first.

use std::num::NonZeroU64;

use rumoca_core::{SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, VarName};

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::super::lower_exact_call;

/// Two sibling loops in one enclosing loop, where the second reads a value the
/// first completed.
///
/// The enclosing fold carries `beta` before `alpha`, so its first tuple member
/// is the one whose value depends on the other sibling. Lowering that member
/// demands the second sibling loop first, and the read of `alpha` inside it
/// names the definition the first sibling issued - not the enclosing loop's
/// entry value.
pub(super) fn sibling_folds() -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "typed_sibling_folds.mo",
        "for i in 1:2 loop for j in 1:3 loop alpha := alpha + 1; end for; \
         for k in 1:2 loop beta := beta + alpha; end for; end for",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 16)).unwrap();
    dae::Dae::construct(sources, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("siblings"), [], [real, real], at),
            |model, reservation| {
                let alpha = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("alpha"), 0, at)
                })?;
                let beta = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("beta"), 1, at)
                })?;
                let (zero, one) = model.expressions(|expressions| {
                    Ok((
                        expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?,
                        expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?,
                    ))
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| {
                    functions.assign(&mut body, alpha, zero, at)?;
                    functions.assign(&mut body, beta, zero, at)
                })?;
                let outer_domain =
                    model.domains(|domains| domains.structured(structured_range("i", 2), at))?;
                let (first_domain, second_domain) = model.domains(|domains| {
                    Ok((
                        domains.nested(outer_domain, structured_range("j", 3), at)?,
                        domains.nested(outer_domain, structured_range("k", 2), at)?,
                    ))
                })?;
                // `beta` precedes `alpha`, so the enclosing fold demands the
                // sibling that reads `alpha` before the sibling that writes it.
                let outer = model.functions(|functions| {
                    functions.begin_loop(body, outer_domain, [beta, alpha], at)
                })?;
                let mut first = model.functions(|functions| {
                    functions.begin_nested_loop(outer, first_domain, [alpha], at)
                })?;
                let carried_alpha =
                    model.functions(|functions| functions.read(first.body(), alpha, at))?;
                let incremented = model.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, carried_alpha, one)
                })?;
                model.functions(|functions| {
                    functions.assign_loop(&mut first, alpha, incremented, at)
                })?;
                let outer = model.functions(|functions| functions.finish_nested_loop(first, at))?;
                let mut second = model.functions(|functions| {
                    functions.begin_nested_loop(outer, second_domain, [beta], at)
                })?;
                let completed_alpha =
                    model.functions(|functions| functions.read(second.body(), alpha, at))?;
                let carried_beta =
                    model.functions(|functions| functions.read(second.body(), beta, at))?;
                let accumulated = model.expressions(|expressions| {
                    expressions.at(at).binary(
                        dae::BinaryOperator::Add,
                        carried_beta,
                        completed_alpha,
                    )
                })?;
                model.functions(|functions| {
                    functions.assign_loop(&mut second, beta, accumulated, at)
                })?;
                let outer =
                    model.functions(|functions| functions.finish_nested_loop(second, at))?;
                let body = model.functions(|functions| functions.finish_loop(outer, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, []))?;
        Ok(())
    })
    .unwrap()
}

pub(super) fn integer_to_real_sibling_folds() -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "typed_coerced_siblings.mo",
        "for i in 1:2 loop for j in 1:3 loop count := 1; end for; \
         alpha := count; for k in 1:2 loop beta := beta + alpha; end for; end for",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 16)).unwrap();
    dae::Dae::construct(sources, |model| {
        let (real, integer) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
            ))
        })?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("coerced_siblings"), [], [real, real], at),
            |model, reservation| {
                let (beta, alpha, count) = model.functions(|functions| {
                    Ok((
                        functions.output(&reservation, VarName::new("beta"), 0, at)?,
                        functions.output(&reservation, VarName::new("alpha"), 1, at)?,
                        functions.local(&reservation, VarName::new("count"), integer, at)?,
                    ))
                })?;
                let (origin, zero, one) = model.expressions(|expressions| {
                    Ok((
                        expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?,
                        expressions.at(at).literal(dae::DaeLiteral::Integer(0))?,
                        expressions.at(at).literal(dae::DaeLiteral::Integer(1))?,
                    ))
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| {
                    functions.assign(&mut body, beta, origin, at)?;
                    functions.assign(&mut body, alpha, origin, at)?;
                    functions.assign(&mut body, count, zero, at)
                })?;
                let outer_domain =
                    model.domains(|domains| domains.structured(structured_range("i", 2), at))?;
                let (first_domain, second_domain) = model.domains(|domains| {
                    Ok((
                        domains.nested(outer_domain, structured_range("j", 3), at)?,
                        domains.nested(outer_domain, structured_range("k", 2), at)?,
                    ))
                })?;
                // `beta` precedes `alpha`, so the enclosing fold demands the
                // sibling that reads `alpha` before `alpha` is assigned.
                let outer = model.functions(|functions| {
                    functions.begin_loop(body, outer_domain, [beta, alpha, count], at)
                })?;
                let mut outer = publish_integer_one(model, outer, first_domain, count, one, at)?;
                let completed_count =
                    model.functions(|functions| functions.read(outer.body(), count, at))?;
                // Integer right-hand side under the Real target.
                model.functions(|functions| {
                    functions.assign_loop(&mut outer, alpha, completed_count, at)
                })?;
                let outer = accumulate_real_alpha(model, outer, second_domain, beta, alpha, at)?;
                let body = model.functions(|functions| functions.finish_loop(outer, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, []))?;
        Ok(())
    })
    .unwrap()
}

fn publish_integer_one<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    outer: dae::FunctionLoop<'dae>,
    domain: dae::DomainId<'dae>,
    count: dae::FunctionValueId<'dae>,
    one: dae::ExprId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionLoop<'dae>, dae::DaeConstructionError> {
    let mut first =
        model.functions(|functions| functions.begin_nested_loop(outer, domain, [count], at))?;
    model.functions(|functions| functions.assign_loop(&mut first, count, one, at))?;
    model.functions(|functions| functions.finish_nested_loop(first, at))
}

fn accumulate_real_alpha<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    outer: dae::FunctionLoop<'dae>,
    domain: dae::DomainId<'dae>,
    beta: dae::FunctionValueId<'dae>,
    alpha: dae::FunctionValueId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionLoop<'dae>, dae::DaeConstructionError> {
    let mut second =
        model.functions(|functions| functions.begin_nested_loop(outer, domain, [beta], at))?;
    let (carried_beta, published_alpha) = model.functions(|functions| {
        Ok((
            functions.read(second.body(), beta, at)?,
            functions.read(second.body(), alpha, at)?,
        ))
    })?;
    let accumulated = model.expressions(|expressions| {
        expressions
            .at(at)
            .binary(dae::BinaryOperator::Add, carried_beta, published_alpha)
    })?;
    model.functions(|functions| functions.assign_loop(&mut second, beta, accumulated, at))?;
    model.functions(|functions| functions.finish_nested_loop(second, at))
}

/// Lower the one top-level call the fixture models issue.
pub(super) fn lower_root_call(model: &dae::Dae) -> solve::SolvePureCallTable {
    model
        .inspect(|view| {
            let call = (0..view.expression_count())
                .filter_map(|index| view.expression_id(index))
                .find(|expression| {
                    view.expression(*expression).is_some_and(|node| {
                        node.function_scope().is_none()
                            && matches!(node.operation(), dae::ExpressionOperation::Call { .. })
                    })
                })
                .unwrap();
            lower_exact_call(
                view,
                call,
                solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            )
        })
        .unwrap()
}

pub(super) fn structured_range(name: &str, upper: i64) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(0),
            display_name: name.to_owned(),
            lower: 1,
            upper,
            step: 1,
        }],
    }
}
