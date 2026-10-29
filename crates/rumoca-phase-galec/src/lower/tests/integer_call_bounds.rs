//! Checked call-site bounds must not be guessed from a callee name or defaults.
use super::*;
use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};

#[derive(Clone, Copy)]
enum Case {
    Valid,
    Nested,
    TooWide,
    TooDeep,
    Fuel,
    Symmetric,
    Asserted,
    Shifted,
    UnknownArgument,
    ExtraDefinition,
    Overflow,
    UnsafeDiv,
    Rebound,
}

fn fixture(case: Case, extent: u32) -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "function position input Integer i; input Integer j; output Integer k; \
                algorithm k := div(i*(i-1),2)+j; end position; \
                function copy input Real rhs[:]; input Integer hint; output Real x[:]; \
                algorithm x:=rhs; for i in 1:15 loop for j in 1:15 loop \
                x[position(i,j)]:=rhs[1]; end for; end for; end copy;";
    let source = sources.add("integer-call-range.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |dae| {
        let (integer, vector) = dae.types(|t| {
            Ok((
                t.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
                t.derived(dae::ValueType::array(dae::ScalarType::Real, [extent]), at)?,
            ))
        })?;
        let helper = helper(dae, integer, case, at)?;
        let helper = if matches!(case, Case::Nested) {
            forward(dae, integer, helper, at)?
        } else {
            helper
        };
        caller(dae, integer, vector, helper, case, at)?;
        Ok(())
    })
    .unwrap()
}

fn helper<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    integer: dae::ValueTypeId<'dae>,
    case: Case,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    dae.function(
        dae::FunctionSignature::new(VarName::new("position"), [integer, integer], [integer], at),
        |dae, reservation| {
            let (i, j, k) = dae.functions(|f| {
                Ok((
                    f.parameter(&reservation, VarName::new("i"), 0, at)?,
                    f.parameter(&reservation, VarName::new("j"), 1, at)?,
                    f.output(&reservation, VarName::new("k"), 0, at)?,
                ))
            })?;
            let locals = if matches!(case, Case::Symmetric) {
                Some(dae.functions(|f| {
                    Ok((
                        f.local(&reservation, VarName::new("r"), integer, at)?,
                        f.local(&reservation, VarName::new("c"), integer, at)?,
                    ))
                })?)
            } else {
                None
            };
            let mut body = dae.functions(|f| f.begin(reservation, at))?;
            if matches!(case, Case::Asserted) {
                add_assertion(dae, &mut body, at)?;
            }
            let rhs = if let Some(locals) = locals {
                symmetric_rhs(dae, &mut body, [i, j], locals, at)?
            } else {
                helper_rhs(dae, &body, [i, j], case, at)?
            };
            if matches!(case, Case::ExtraDefinition) {
                let zero = dae.expressions(|e| e.at(at).literal(dae::DaeLiteral::Integer(0)))?;
                dae.functions(|f| f.assign(&mut body, k, zero, at))?;
            }
            dae.functions(|f| f.assign(&mut body, k, rhs, at))?;
            dae.functions(|f| f.define(body, at))
        },
    )
    .map(|(id, ())| id)
}

fn caller<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    integer: dae::ValueTypeId<'dae>,
    vector: dae::ValueTypeId<'dae>,
    helper: dae::FunctionId<'dae>,
    case: Case,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    dae.function(
        dae::FunctionSignature::new(VarName::new("copy"), [vector, integer], [vector], at),
        |dae, reservation| {
            let (rhs, hint, x) = dae.functions(|f| {
                Ok((
                    f.parameter(&reservation, VarName::new("rhs"), 0, at)?,
                    f.parameter(&reservation, VarName::new("hint"), 1, at)?,
                    f.output(&reservation, VarName::new("x"), 0, at)?,
                ))
            })?;
            let input = dae.expressions(|e| e.at(at).function_parameter(rhs))?;
            let mut body = dae.functions(|f| f.begin(reservation, at))?;
            dae.functions(|f| f.assign(&mut body, x, input, at))?;
            let domain = dae.domains(|d| {
                d.structured(
                    StructuredIndexDomain {
                        binders: (0..2)
                            .map(|id| StructuredIndexBinder {
                                provenance: rumoca_core::IndexBinderProvenance::Generated,
                                id,
                                display_name: format!("i{id}"),
                                lower: 1,
                                upper: match case {
                                    Case::TooWide => 257,
                                    Case::Fuel => 100,
                                    _ => 15,
                                },
                                step: 1,
                            })
                            .collect(),
                    },
                    at,
                )
            })?;
            let (i, j) =
                dae.domains(|d| Ok((d.binder(domain, 0, at)?, d.binder(domain, 1, at)?)))?;
            let (i, j) = dae.expressions(|e| {
                Ok((
                    e.at(at).binder(i)?,
                    if matches!(case, Case::UnknownArgument) {
                        e.at(at).function_parameter(hint)?
                    } else {
                        e.at(at).binder(j)?
                    },
                ))
            })?;
            let mut body = dae.functions(|f| f.begin_loop(body, domain, [x], at))?;
            store(dae, &mut body, x, input, helper, [i, j], at)?;
            if matches!(case, Case::Rebound) {
                let shifted = shifted_argument(dae, i, at)?;
                store(dae, &mut body, x, input, helper, [shifted, j], at)?;
            }
            let body = dae.functions(|f| f.finish_loop(body, at))?;
            dae.functions(|f| f.define(body, at))
        },
    )
    .map(|(id, ())| id)
}

fn shifted_argument<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    i: dae::ExprId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    dae.expressions(|e| {
        let n = e.at(at).literal(dae::DaeLiteral::Integer(100))?;
        e.at(at).binary(dae::BinaryOperator::Add, i, n)
    })
}

fn store<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    body: &mut dae::FunctionLoop<'dae>,
    x: dae::FunctionValueId<'dae>,
    input: dae::ExprId<'dae>,
    helper: dae::FunctionId<'dae>,
    arguments: [dae::ExprId<'dae>; 2],
    at: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let current = dae.functions(|f| f.read(body.body(), x, at))?;
    let value = dae.expressions(|e| {
        let index = e.at(at).call(helper, 0, arguments)?;
        let one = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
        let value = e.at(at).index(
            input,
            [dae::Subscript::Index {
                expression: one,
                provenance: at,
            }],
        )?;
        e.at(at).array_update(
            current,
            value,
            [dae::Subscript::Index {
                expression: index,
                provenance: at,
            }],
        )
    })?;
    dae.functions(|f| f.assign_loop(body, x, value, at))
}

fn lower(case: Case, extent: u32) -> Result<Vec<gast::UserFunction>, String> {
    fixture(case, extent).inspect(|view| {
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        user_functions::lower_reachable(
            view,
            &definitions,
            HashSet::from([(view.function_count() - 1) as u32]),
            EmissionFacts::structured(),
        )
        .map_err(|e| format!("{e:?}"))
    })
}

#[test]
fn an_integer_helper_call_preserves_finite_binder_bounds() {
    let functions = lower(Case::Valid, 120).unwrap();
    assert_eq!(functions.len(), 2);
}
#[test]
fn an_extent_one_below_the_proven_maximum_is_refused() {
    assert!(lower(Case::Valid, 119).is_err());
}
#[test]
fn a_shifted_helper_result_is_not_clamped_to_the_output() {
    assert!(lower(Case::Shifted, 120).is_err());
}
#[test]
fn an_unbounded_runtime_argument_does_not_receive_a_fact() {
    assert!(lower(Case::UnknownArgument, 120).is_err());
}
#[test]
fn ordered_assignment_calls_use_the_selected_final_definition() {
    lower(Case::ExtraDefinition, 120).unwrap();
}
#[test]
fn overflowing_helper_arithmetic_cannot_issue_a_range() {
    assert!(lower(Case::Overflow, 120).is_err());
}
#[test]
fn a_second_call_with_different_arguments_cannot_reuse_the_first_range() {
    assert!(lower(Case::Rebound, 120).is_err());
}

fn helper_rhs<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    parameters: [dae::FunctionParameterId<'dae>; 2],
    case: Case,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let (i, j, product, denominator) = dae.expressions(|e| {
        let i = e.at(at).function_parameter(parameters[0])?;
        let j = e.at(at).function_parameter(parameters[1])?;
        let one = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
        let prior = e.at(at).binary(dae::BinaryOperator::Subtract, i, one)?;
        let product = e.at(at).binary(dae::BinaryOperator::Multiply, i, prior)?;
        let denominator = if matches!(case, Case::UnsafeDiv) {
            e.at(at).binary(dae::BinaryOperator::Subtract, j, one)?
        } else {
            e.at(at).literal(dae::DaeLiteral::Integer(2))?
        };
        Ok((i, j, product, denominator))
    })?;
    let rows =
        dae.function_runtime_quotient(body, dae::PureBuiltin::Div, [product, denominator], at)?;
    dae.expressions(|e| {
        let index = e.at(at).binary(dae::BinaryOperator::Add, rows, j)?;
        match case {
            Case::Shifted => {
                let one = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
                e.at(at).binary(dae::BinaryOperator::Add, index, one)
            }
            Case::Overflow => {
                let huge = e.at(at).literal(dae::DaeLiteral::Integer(i64::MAX))?;
                e.at(at).binary(dae::BinaryOperator::Add, huge, i)
            }
            Case::TooWide => Ok(j),
            Case::TooDeep | Case::Fuel => {
                let zero = e.at(at).literal(dae::DaeLiteral::Integer(0))?;
                let mut result = index;
                for _ in 0..if matches!(case, Case::TooDeep) {
                    70
                } else {
                    50
                } {
                    result = e.at(at).binary(dae::BinaryOperator::Add, result, zero)?;
                }
                Ok(result)
            }
            _ => Ok(index),
        }
    })
}

#[test]
fn a_divisor_interval_containing_zero_cannot_issue_a_range() {
    assert!(lower(Case::UnsafeDiv, 120).is_err());
}

fn symmetric_rhs<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    body: &mut dae::FunctionBody<'dae>,
    parameters: [dae::FunctionParameterId<'dae>; 2],
    locals: (dae::FunctionValueId<'dae>, dae::FunctionValueId<'dae>),
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let (r, c) = locals;
    let (minimum, maximum) = dae.expressions(|e| {
        let i = e.at(at).function_parameter(parameters[0])?;
        let j = e.at(at).function_parameter(parameters[1])?;
        Ok((
            e.at(at).builtin(dae::PureBuiltin::Min, [i, j])?,
            e.at(at).builtin(dae::PureBuiltin::Max, [i, j])?,
        ))
    })?;
    dae.functions(|f| {
        f.assign(body, r, minimum, at)?;
        f.assign(body, c, maximum, at)
    })?;
    let (r, c) = dae.functions(|f| Ok((f.read(body, r, at)?, f.read(body, c, at)?)))?;
    let (product, two) = dae.expressions(|e| {
        let one = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
        let two = e.at(at).literal(dae::DaeLiteral::Integer(2))?;
        let thirty_two = e.at(at).literal(dae::DaeLiteral::Integer(32))?;
        let prior = e.at(at).binary(dae::BinaryOperator::Subtract, r, one)?;
        let width = e
            .at(at)
            .binary(dae::BinaryOperator::Subtract, thirty_two, r)?;
        Ok((
            e.at(at)
                .binary(dae::BinaryOperator::Multiply, prior, width)?,
            two,
        ))
    })?;
    let rows = dae.function_runtime_quotient(body, dae::PureBuiltin::Div, [product, two], at)?;
    dae.expressions(|e| {
        let one = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
        let total = e.at(at).binary(dae::BinaryOperator::Add, rows, c)?;
        let total = e.at(at).binary(dae::BinaryOperator::Subtract, total, r)?;
        e.at(at).binary(dae::BinaryOperator::Add, total, one)
    })
}

fn add_assertion<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    body: &mut dae::FunctionBody<'dae>,
    at: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let (condition, message) = dae.expressions(|e| {
        Ok((
            e.at(at).literal(dae::DaeLiteral::Boolean(false))?,
            e.at(at)
                .literal(dae::DaeLiteral::String("must execute".into()))?,
        ))
    })?;
    dae.functions(|f| f.assertion(body, condition, message, at))
}

#[test]
fn correlated_minimum_maximum_aliases_keep_exact_binder_coordinates() {
    lower(Case::Symmetric, 120).unwrap();
}
#[test]
fn correlated_aliases_do_not_authorize_an_extent_below_the_real_maximum() {
    assert!(lower(Case::Symmetric, 119).is_err());
}
#[test]
fn a_body_with_an_assertion_is_not_a_range_only_alias() {
    assert!(lower(Case::Asserted, 120).is_err());
}

fn forward<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    integer: dae::ValueTypeId<'dae>,
    callee: dae::FunctionId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    dae.function(
        dae::FunctionSignature::new(VarName::new("forward"), [integer, integer], [integer], at),
        |dae, reservation| {
            let (a, b, y) = dae.functions(|f| {
                Ok((
                    f.parameter(&reservation, VarName::new("a"), 0, at)?,
                    f.parameter(&reservation, VarName::new("b"), 1, at)?,
                    f.output(&reservation, VarName::new("y"), 0, at)?,
                ))
            })?;
            let value = dae.expressions(|e| {
                let a = e.at(at).function_parameter(a)?;
                let b = e.at(at).function_parameter(b)?;
                e.at(at).call(callee, 0, [a, b])
            })?;
            let mut body = dae.functions(|f| f.begin(reservation, at))?;
            dae.functions(|f| f.assign(&mut body, y, value, at))?;
            dae.functions(|f| f.define(body, at))
        },
    )
    .map(|(id, ())| id)
}
#[test]
fn nested_calls_keep_each_lexical_parameter_binding() {
    lower(Case::Nested, 120).unwrap();
}
#[test]
fn coordinate_budget_exhaustion_cannot_authorize_an_index() {
    let error = lower(Case::TooWide, 257).unwrap_err();
    assert!(error.contains("dynamic-array-index"), "{error}");
}
#[test]
fn expression_depth_exhaustion_cannot_authorize_an_index() {
    let error = lower(Case::TooDeep, 120).unwrap_err();
    assert!(error.contains("dynamic-array-index"), "{error}");
}
#[test]
fn shared_expression_fuel_exhaustion_cannot_authorize_an_index() {
    let error = lower(Case::Fuel, 5050).unwrap_err();
    assert!(error.contains("dynamic-array-index"), "{error}");
}
