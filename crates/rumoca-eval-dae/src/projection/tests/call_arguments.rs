use super::conditional_activation::{model_inputs, project, reference};
use super::*;

fn scalar_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    boolean: dae::ValueTypeId<'dae>,
    real: dae::ValueTypeId<'dae>,
    pair: Option<dae::ValueTypeId<'dae>>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    model
        .function(
            dae::FunctionSignature::new(
                VarName::new(if pair.is_some() {
                    "returnPair"
                } else {
                    "returnScalar"
                }),
                [boolean, real, real],
                [pair.unwrap_or(real)],
                at,
            ),
            |m, reservation| {
                let (first, value, output) = m.functions(|f| {
                    let first = f.parameter(&reservation, VarName::new("first"), 0, at)?;
                    let value = f.parameter(&reservation, VarName::new("value"), 1, at)?;
                    f.parameter(&reservation, VarName::new("unused"), 2, at)?;
                    Ok((
                        first,
                        value,
                        f.output(&reservation, VarName::new("output"), 0, at)?,
                    ))
                })?;
                let result = m.expressions(|e| {
                    let first = e.at(at).function_parameter(first)?;
                    let value = e.at(at).function_parameter(value)?;
                    let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                    let result = e.at(at).conditional([(first, value)], zero)?;
                    match pair {
                        Some(pair) => e.at(at).record(pair, [result, zero]),
                        None => Ok(result),
                    }
                })?;
                let mut body = m.functions(|f| f.begin(reservation, at))?;
                m.functions(|f| f.assign(&mut body, output, result, at))?;
                m.functions(|f| f.define(body, at))
            },
        )
        .map(|(f, ())| f)
}

fn record_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    boolean: dae::ValueTypeId<'dae>,
    real: dae::ValueTypeId<'dae>,
    pair: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    model
        .function(
            dae::FunctionSignature::new(VarName::new("firstField"), [boolean, pair], [real], at),
            |m, reservation| {
                let (first, value, output) = m.functions(|f| {
                    Ok((
                        f.parameter(&reservation, VarName::new("first"), 0, at)?,
                        f.parameter(&reservation, VarName::new("value"), 1, at)?,
                        f.output(&reservation, VarName::new("output"), 0, at)?,
                    ))
                })?;
                let result = m.expressions(|e| {
                    let first = e.at(at).function_parameter(first)?;
                    let value = e.at(at).function_parameter(value)?;
                    let value = e.at(at).field(value, 0)?;
                    let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                    e.at(at).conditional([(first, value)], zero)
                })?;
                let mut body = m.functions(|f| f.begin(reservation, at))?;
                m.functions(|f| f.assign(&mut body, output, result, at))?;
                m.functions(|f| f.define(body, at))
            },
        )
        .map(|(f, ())| f)
}

fn selector_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    types: [dae::ValueTypeId<'dae>; 4],
    scalar: Option<dae::FunctionId<'dae>>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let [boolean, vector, integer, real] = types;
    model
        .function(
            dae::FunctionSignature::new(
                VarName::new(if scalar.is_some() {
                    "callerSelects"
                } else {
                    "calleeSelects"
                }),
                [boolean, vector, integer],
                [real],
                at,
            ),
            |m, reservation| {
                let (first, samples, index, output) = m.functions(|f| {
                    Ok((
                        f.parameter(&reservation, VarName::new("first"), 0, at)?,
                        f.parameter(&reservation, VarName::new("samples"), 1, at)?,
                        f.parameter(&reservation, VarName::new("index"), 2, at)?,
                        f.output(&reservation, VarName::new("output"), 0, at)?,
                    ))
                })?;
                let result = m.expressions(|e| {
                    let first = e.at(at).function_parameter(first)?;
                    let samples = e.at(at).function_parameter(samples)?;
                    let index = e.at(at).function_parameter(index)?;
                    let selected = e.at(at).index(
                        samples,
                        [dae::Subscript::Index {
                            expression: index,
                            provenance: at,
                        }],
                    )?;
                    let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                    match scalar {
                        Some(scalar) => e.at(at).call(scalar, 0, [first, selected, zero]),
                        None => e.at(at).conditional([(first, selected)], zero),
                    }
                })?;
                let mut body = m.functions(|f| f.begin(reservation, at))?;
                m.functions(|f| f.assign(&mut body, output, result, at))?;
                m.functions(|f| f.define(body, at))
            },
        )
        .map(|(f, ())| f)
}

fn call_roots<'dae>(
    e: &mut dae::Expressions<'_, 'dae>,
    functions: [dae::FunctionId<'dae>; 5],
    inputs: [dae::ExprId<'dae>; 3],
    pair: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<Vec<usize>, dae::DaeConstructionError> {
    let [scalar, callee, caller, record, pair_result] = functions;
    let [a, b, guard] = inputs;

    let one = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
    let two = e.at(at).literal(dae::DaeLiteral::Integer(2))?;
    let good = e.at(at).index(
        a,
        [dae::Subscript::Index {
            expression: one,
            provenance: at,
        }],
    )?;
    let bad = e.at(at).index(
        a,
        [dae::Subscript::Index {
            expression: two,
            provenance: at,
        }],
    )?;
    let strict = e.at(at).call(scalar, 0, [guard, bad, b])?;
    let unused = e.at(at).call(scalar, 0, [guard, b, bad])?;
    let guarded = e.at(at).conditional([(guard, strict)], b)?;
    let valid = e.at(at).call(scalar, 0, [guard, b, good])?;
    let internal = e.at(at).call(callee, 0, [guard, a, two])?;
    let nested = e.at(at).call(caller, 0, [guard, a, two])?;
    let guarded_nested = e.at(at).conditional([(guard, nested)], b)?;
    let bad_pair = e.at(at).record(pair, [b, bad])?;
    let good_pair = e.at(at).record(pair, [b, good])?;
    let record_bad = e.at(at).call(record, 0, [guard, bad_pair])?;
    let record_good = e.at(at).call(record, 0, [guard, good_pair])?;
    let guarded_record = e.at(at).conditional([(guard, record_bad)], b)?;
    let pair_call = e.at(at).call(pair_result, 0, [guard, bad, b])?;
    let result_field = e.at(at).field(pair_call, 0)?;
    let actual_field = e.at(at).field(bad_pair, 0)?;
    let field_call = e.at(at).call(scalar, 0, [guard, actual_field, b])?;
    let conditional_actual = e.at(at).conditional([(guard, bad)], b)?;
    let conditional_actual = e.at(at).call(scalar, 0, [guard, conditional_actual, b])?;
    Ok([
        strict,
        unused,
        guarded,
        valid,
        internal,
        nested,
        guarded_nested,
        record_bad,
        record_good,
        guarded_record,
        result_field,
        field_call,
        conditional_actual,
    ]
    .map(|r| r.index() as usize)
    .to_vec())
}

fn model() -> (dae::Dae, Vec<usize>) {
    let text = "f(first,samples[2]); f(first,samples,2); unused actual and record sibling";
    let mut sources = SourceMap::new();
    let source = sources.add("call_actual_activation.mo", text);
    let at = provenance(source, 0, text.len());
    let mut roots = Vec::new();
    let model = dae::Dae::construct(sources, |m| {
        let real = m.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let integer =
            m.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at))?;
        let boolean =
            m.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at))?;
        let vector =
            m.types(|t| t.derived(dae::ValueType::array(dae::ScalarType::Real, [1]), at))?;
        let pair = m.types(|t| {
            t.record(
                VarName::new("Pair"),
                [
                    (VarName::new("first"), real),
                    (VarName::new("second"), real),
                ],
                at,
            )
        })?;
        let scalar = scalar_function(m, boolean, real, None, at)?;
        let pair_result = scalar_function(m, boolean, real, Some(pair), at)?;
        let record = record_function(m, boolean, real, pair, at)?;
        let callee = selector_function(m, [boolean, vector, integer, real], None, at)?;
        let caller = selector_function(m, [boolean, vector, integer, real], Some(scalar), at)?;
        let [a, b, guard] = model_inputs(m, vector, real, at)?;
        roots = m.expressions(|e| {
            call_roots(
                e,
                [scalar, callee, caller, record, pair_result],
                [a, b, guard],
                pair,
                at,
            )
        })?;
        Ok(())
    })
    .unwrap();
    (model, roots)
}

#[test]
fn guaranteed_actuals_fault_even_when_formals_are_conditional_unused_or_record_siblings() {
    let (model, roots) = model();
    model.inspect(|view| {
        for filtered in [false, true] {
            for reverse in [false, true] {
                check_argument_order(view, &roots, filtered, reverse);
            }
        }
    });
}

fn check_argument_order<'dae>(
    view: dae::DaeView<'dae>,
    roots: &[usize],
    filtered: bool,
    reverse: bool,
) {
    let mut cache = ScalarCoordinateProjectionCache::default();
    let success = [2, 3, 4, 6, 8, 9, 12];
    let failures = [0, 1, 5, 7, 10, 11];
    let orders: [&[usize]; 2] = if reverse {
        [&failures, &success]
    } else {
        [&success, &failures]
    };
    for order in orders {
        for &ordinal in order {
            let root = view.expression_id(roots[ordinal]).unwrap();
            if failures.contains(&ordinal) {
                let expected = project(view, root, &mut reference(), filtered).unwrap_err();
                let actual = project(view, root, &mut cache, filtered).unwrap_err();
                assert_eq!(actual.to_string(), expected.to_string());
                assert!(matches!(
                    actual,
                    ProjectionError::IndexOutOfBounds {
                        index: 2,
                        extent: 1,
                        ..
                    }
                ));
            } else {
                let expected = project(view, root, &mut reference(), filtered).unwrap();
                assert_eq!(project(view, root, &mut cache, filtered).unwrap(), expected);
                assert_eq!(project(view, root, &mut cache, filtered).unwrap(), expected);
            }
        }
    }
}

#[test]
fn validation_of_unused_valid_actuals_preserves_result_incidence_and_query_filtering() {
    let (model, roots) = model();
    model.inspect(|view| {
        let mut cache = ScalarCoordinateProjectionCache::default();
        for ordinal in [3, 8] {
            let root = view.expression_id(roots[ordinal]).unwrap();
            let expected = project(view, root, &mut reference(), false).unwrap();
            assert!(expected.iter().all(|(coordinate, _)| !matches!(coordinate, dae::CoordinateView::Algebraic(v) if v.index() == 0)), "valid unused scalar and record sibling cannot add samples incidence");
            assert!(expected.iter().any(|(coordinate, _)| matches!(coordinate, dae::CoordinateView::Algebraic(v) if v.index() == 1)));
            let mut filtered = Vec::new();
            for_each_scalar_coordinate_filtered_cached(view, root, 0, None, &mut cache, |v| matches!(v, dae::CoordinateView::Algebraic(_)), |v, s| filtered.push((v, s))).unwrap();
            assert_eq!(filtered, expected.into_iter().filter(|(v, _)| matches!(v, dae::CoordinateView::Algebraic(_))).collect::<Vec<_>>());
        }
        for ordinal in [0, 1, 5, 7, 10, 11] {
            let root = view.expression_id(roots[ordinal]).unwrap();
            assert!(matches!(for_each_scalar_coordinate_filtered_cached(view, root, 0, None, &mut cache, |_| false, |_, _| panic!("excluded actual")), Err(ProjectionError::IndexOutOfBounds { index: 2, extent: 1, .. })));
        }
    });
}
