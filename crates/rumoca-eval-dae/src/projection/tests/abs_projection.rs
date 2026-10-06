//! Abs follows its selected value operand; guards never memoize its numbers.
use super::*;

fn abs_roots(view: dae::DaeView<'_>) -> Vec<dae::ExprId<'_>> {
    (0..view.expression_count())
        .filter_map(|i| view.expression_id(i))
        .filter(|id| {
            matches!(
                view.expression(*id).unwrap().operation(),
                dae::ExpressionOperation::Builtin {
                    builtin: dae::PureBuiltin::Abs,
                    ..
                }
            )
        })
        .collect()
}

fn project<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
) -> Result<Vec<(dae::CoordinateView<'dae>, usize)>, String> {
    let mut coordinates = Vec::new();
    for_each_scalar_coordinate_cached(view, root, 0, None, cache, |coordinate, scalar| {
        coordinates.push((coordinate, scalar))
    })
    .map(|()| coordinates)
    .map_err(|error| format!("{error:?}"))
}

#[test]
fn abs_preserves_external_refusal_and_clock_value_dependencies() {
    shape_only_size::primitive_abs_model().inspect(check_value_operands);
}

fn check_value_operands(view: dae::DaeView<'_>) {
    let mut cached = ScalarCoordinateProjectionCache::default();
    let mut reference = ScalarCoordinateProjectionCache {
        ..Default::default()
    };
    let roots = abs_roots(view);
    assert_eq!(roots.len(), 5);
    for root in roots.into_iter().take(2).cycle().take(4) {
        let expected = project(view, root, &mut reference);
        assert_eq!(project(view, root, &mut cached), expected);
        let dae::ExpressionOperation::Builtin { arguments, .. } =
            view.expression(root).unwrap().operation()
        else {
            unreachable!()
        };
        match view
            .expression(arguments.get(0).unwrap())
            .unwrap()
            .operation()
        {
            dae::ExpressionOperation::Call { .. } => assert!(expected.is_err()),
            dae::ExpressionOperation::ClockTransfer { .. } => {
                assert_eq!(expected.unwrap().len(), 1)
            }
            _ => unreachable!(),
        }
    }
    assert_eq!(cached.function_results, reference.function_results);
    assert_eq!(cached.completed_folds, reference.completed_folds);
    assert_eq!(cached.fold_edges, reference.fold_edges);
}

fn value_model(parameter: bool) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("abs_value.mo", "abs(value)");
    let at = provenance(source, 0, 10);
    dae::Dae::construct(sources, |model| {
        let real = model.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let coordinate = value_coordinate(model, real, parameter, at)?;
        model.expressions(|e| {
            let value = e.at(at).coordinate(coordinate)?;
            e.at(at).builtin(dae::PureBuiltin::Abs, [value])?;
            Ok(())
        })
    })
    .unwrap()
}

fn value_coordinate<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    parameter: bool,
    at: dae::DaeProvenance,
) -> Result<dae::CoordinateInput<'dae>, dae::DaeConstructionError> {
    if parameter {
        let binding = model.expressions(|e| e.at(at).literal(dae::DaeLiteral::Real(0.0)))?;
        let variable = model.variables(|v| {
            v.parameter(
                VarName::new("value"),
                real,
                at,
                dae::VariableAttributes {
                    binding: Some(binding),
                    is_tunable: true,
                    ..Default::default()
                },
            )
        })?;
        return Ok(dae::CoordinateInput::Parameter(variable));
    }
    model.variables(|v| {
        v.input(
            VarName::new("value"),
            real,
            dae::InputVariability::Continuous,
            at,
            Default::default(),
        )
        .map(dae::CoordinateInput::Input)
    })
}

#[test]
fn abs_projection_reuse_retains_parameter_edits_nonfinite_errors_and_input_refusal() {
    for parameter in [false, true] {
        value_model(parameter).inspect(|view| check_values(view, parameter));
    }
}

fn check_values(view: dae::DaeView<'_>, parameter: bool) {
    let root = abs_roots(view)[0];
    let mut cached = ScalarCoordinateProjectionCache::default();
    let mut reference = ScalarCoordinateProjectionCache {
        ..Default::default()
    };
    for value in [
        -1.25,
        -0.0,
        0.0,
        0.5,
        1.9,
        f64::NAN,
        f64::INFINITY,
        f64::NEG_INFINITY,
        2.2,
    ] {
        assert_eq!(
            project(view, root, &mut cached),
            project(view, root, &mut reference)
        );
        let result = numerical_result(view, root, value);
        assert_eq!(result, numerical_result(view, root, value));
        assert_eq!(result, expected_value(view, root, value, parameter));
    }
}

fn expected_value<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    value: f64,
    parameter: bool,
) -> Result<u64, (crate::NumericEvaluationErrorKind, Span)> {
    let span = view.expression(root).unwrap().provenance().span();
    if !parameter {
        return Err((crate::NumericEvaluationErrorKind::NonStaticCoordinate, span));
    }
    if !value.is_finite() {
        return Err((crate::NumericEvaluationErrorKind::InvalidOverride, span));
    }
    Ok(value.abs().to_bits())
}

fn numerical_result<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    value: f64,
) -> Result<u64, (crate::NumericEvaluationErrorKind, Span)> {
    crate::NumericEvaluator::with_overrides(view, |_, _| Some(value))
        .expression(root)
        .map(|values| values[0].to_bits())
        .map_err(|error| (error.kind(), error.span()))
}
