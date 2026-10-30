//! Slice RHS projection uses the selected interval, not the whole target loop.
use super::*;

fn sliced_sum(width: u32, start: i64, step: i64, stop: i64) -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "function place input Real v[3]; output Real y[6]; algorithm \
                y := zeros(6); y[4:6] := v + {1.0,2.0,3.0}; end place;";
    let source = sources.add("slice-value.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |dae| {
        let (input_type, output_type) = dae.types(|t| {
            Ok((
                t.derived(dae::ValueType::array(dae::ScalarType::Real, [width]), at)?,
                t.derived(
                    dae::ValueType::array(dae::ScalarType::Real, [width + 3]),
                    at,
                )?,
            ))
        })?;
        dae.function(
            dae::FunctionSignature::new(VarName::new("place"), [input_type], [output_type], at),
            |dae, reservation| {
                let (v, y) = dae.functions(|f| {
                    Ok((
                        f.parameter(&reservation, VarName::new("v"), 0, at)?,
                        f.output(&reservation, VarName::new("y"), 0, at)?,
                    ))
                })?;
                let mut body = dae.functions(|f| f.begin(reservation, at))?;
                let zeros = dae.expressions(|e| {
                    let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                    e.at(at).array(vec![zero; (width + 3) as usize])
                })?;
                dae.functions(|f| f.assign(&mut body, y, zeros, at))?;
                let current = dae.functions(|f| f.read(&body, y, at))?;
                let update = dae.expressions(|e| {
                    let input = e.at(at).function_parameter(v)?;
                    let elements = (1..=width)
                        .map(|value| e.at(at).literal(dae::DaeLiteral::Real(f64::from(value))))
                        .collect::<Result<Vec<_>, _>>()?;
                    let constants = e.at(at).array(elements)?;
                    let value = e
                        .at(at)
                        .binary(dae::BinaryOperator::Add, input, constants)?;
                    let first = e.at(at).literal(dae::DaeLiteral::Integer(start))?;
                    let stride = e.at(at).literal(dae::DaeLiteral::Integer(step))?;
                    let last = e.at(at).literal(dae::DaeLiteral::Integer(stop))?;
                    let range = e.at(at).range(first, Some(stride), last)?;
                    e.at(at).array_update(
                        current,
                        value,
                        [dae::Subscript::Slice {
                            expression: range,
                            provenance: at,
                        }],
                    )
                })?;
                dae.functions(|f| {
                    f.assign(&mut body, y, update, at)?;
                    f.define(body, at)
                })
            },
        )
        .map(|_| ())
    })
    .unwrap()
}

fn lower_slice(
    width: u32,
    start: i64,
    step: i64,
    stop: i64,
) -> Result<Vec<gast::UserFunction>, String> {
    sliced_sum(width, start, step, stop).inspect(|view| {
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        user_functions::lower_reachable(
            view,
            &definitions,
            HashSet::from([0]),
            EmissionFacts::structured(),
        )
        .map_err(|error| format!("{error:?}"))
    })
}

#[test]
fn fixed_vector_sum_in_a_slice_uses_the_guarded_rhs_extent() {
    for width in [1, 2, 3, 7] {
        for start in [1, 2, 4] {
            lower_slice(width, start, 1, start + i64::from(width) - 1)
                .expect("the slice guard proves every RHS coordinate");
        }
    }
}

#[test]
fn descending_slice_uses_the_reversed_guarded_rhs_extent() {
    for width in [1, 2, 3, 7] {
        for stop in [1, 2, 4] {
            lower_slice(width, stop + i64::from(width) - 1, -1, stop)
                .expect("the reversed slice guard proves every RHS ordinal");
        }
    }
}

#[test]
fn a_slice_outside_the_target_cannot_gain_a_clipped_rhs_proof() {
    for (start, step, stop) in [(0, 1, 2), (5, 1, 7), (2, -1, 0), (7, -1, 5)] {
        let error = lower_slice(3, start, step, stop)
            .expect_err("the complete slice must fit, not just its intersection with the loop");
        assert!(error.contains("dynamic-array-index"), "{error}");
    }
}

fn literal_slice() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "y[4:6] := {1.0,2.0,3.0};";
    let source = sources.add("literal-slice.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |dae| {
        dae.expressions(|e| {
            let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            let base = e.at(at).array([zero; 6])?;
            let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            let two = e.at(at).literal(dae::DaeLiteral::Real(2.0))?;
            let three = e.at(at).literal(dae::DaeLiteral::Real(3.0))?;
            let value = e.at(at).array([one, two, three])?;
            let start = e.at(at).literal(dae::DaeLiteral::Integer(4))?;
            let stop = e.at(at).literal(dae::DaeLiteral::Integer(6))?;
            let range = e.at(at).range(start, None, stop)?;
            e.at(at).array_update(
                base,
                value,
                [dae::Subscript::Slice {
                    expression: range,
                    provenance: at,
                }],
            )
        })
        .map(|_| ())
    })
    .unwrap()
}

#[test]
fn slice_range_proof_is_lexical_and_keeps_unknown_indices_refused() {
    literal_slice().inspect(|view| {
        let update = (0..view.expression_count())
            .filter_map(|i| view.expression_id(i))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::ArrayUpdate { .. }
                )
            })
            .unwrap();
        let span = view.expression(update).unwrap().provenance().span();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::new(view, &definitions, &variables, &previous);
        let iterator = gast::Name::ident("coordinate");
        lowerer.loop_index_bounds.push(LoopIndexBound {
            name: iterator.clone(),
            minimum: 1,
            maximum: 6,
        });
        let coordinate = gast::Expression::Ref(gast::Reference::local(iterator));
        let ordinal = gast::Expression::binary(
            gast::BinaryOp::Sub,
            coordinate.clone(),
            gast::Expression::Integer(3),
        );
        assert!(lowerer.prove_dynamic_index(&ordinal, 3, span).is_err());
        lowerer
            .lower_at(update, &[coordinate])
            .expect("guarded literal slice projects");
        assert_eq!(lowerer.loop_index_bounds.len(), 1);
        assert_eq!(
            (
                lowerer.loop_index_bounds[0].minimum,
                lowerer.loop_index_bounds[0].maximum
            ),
            (1, 6)
        );
        assert!(
            lowerer.prove_dynamic_index(&ordinal, 3, span).is_err(),
            "an index outside the selection has the original enclosing interval"
        );
        let unknown = gast::Expression::Ref(gast::Reference::local(gast::Name::ident("unknown")));
        assert!(lowerer.prove_dynamic_index(&unknown, 3, span).is_err());
    });
}
