//! The checked shape, not first-operand values, owns Size semantics.
use super::*;
use rumoca_core::{ClockLattice, ClockRational};

fn external_array<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let (function, ()) = model.function(
        dae::FunctionSignature::new(VarName::new("opaque_array"), [], [vector], at),
        |model, reservation| {
            let output =
                model.functions(|f| f.output(&reservation, VarName::new("values"), 0, at))?;
            let external = dae::ExternalFunctionBody::new(
                dae::FunctionPurity::Pure,
                dae::ExternalLanguage::C,
                VarName::new("opaqueArray"),
                [],
                Some(output),
                dae::ExternalLinkage::new([], None, None, None),
            );
            model.functions(|f| f.define_external(reservation, external, at))
        },
    )?;
    model.expressions(|e| e.at(at).call(function, 0, []))
}

fn clock_array<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let variable = model
        .variables(|v| v.discrete_real(VarName::new("sampled"), vector, at, Default::default()))?;
    let clock = model.clocks(|c| {
        let clock = c.periodic(
            ClockLattice::new(ClockRational::ONE, ClockRational::ZERO).unwrap(),
            at,
        )?;
        c.own_discrete_real(clock.into(), variable, at)?;
        Ok(clock)
    })?;
    model.expressions(|e| {
        let source = e
            .at(at)
            .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
        e.at(at).clock_transfer(
            dae::ClockTransferKind::SubSample { factor: 1 },
            source,
            clock.into(),
            clock.into(),
        )
    })
}

pub(in crate::projection) fn primitive_model(
    dimension: dae::DaeLiteral,
) -> Result<dae::Dae, dae::DaeConstructionError> {
    primitive_builtins(dimension, false)
}

pub(in crate::projection) fn primitive_floor_model() -> dae::Dae {
    primitive_builtins(dae::DaeLiteral::Integer(1), true).unwrap()
}

fn primitive_builtins(
    dimension: dae::DaeLiteral,
    floor: bool,
) -> Result<dae::Dae, dae::DaeConstructionError> {
    let text = "size(opaqueArray(), dimension); size(subSample(sampled,1),dimension);";
    let mut sources = SourceMap::new();
    let source = sources.add("shape_only_size.mo", text);
    let at = provenance(source, 0, text.len());
    dae::Dae::construct(sources, |model| {
        let vector =
            model.types(|t| t.derived(dae::ValueType::array(dae::ScalarType::Real, [7]), at))?;
        let operands = [
            external_array(model, vector, at)?,
            clock_array(model, vector, at)?,
        ];
        model.expressions(|e| define_builtins(e, dimension, operands, floor, at))
    })
}

fn define_builtins<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    dimension: dae::DaeLiteral,
    operands: [dae::ExprId<'dae>; 2],
    floor: bool,
    at: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let dimension = expressions.at(at).literal(dimension)?;
    for array in operands {
        expressions
            .at(at)
            .builtin(dae::PureBuiltin::Size, [array, dimension])?;
        if floor {
            expressions
                .at(at)
                .builtin(dae::PureBuiltin::Floor, [array])?;
        }
    }
    Ok(())
}

pub(in crate::projection) fn sizes(view: dae::DaeView<'_>) -> Vec<dae::ExprId<'_>> {
    (0..view.expression_count())
        .filter_map(|i| view.expression_id(i))
        .filter(|id| {
            matches!(
                view.expression(*id).unwrap().operation(),
                dae::ExpressionOperation::Builtin {
                    builtin: dae::PureBuiltin::Size,
                    ..
                }
            )
        })
        .collect()
}

#[test]
fn size_checked_shape_does_not_evaluate_or_capture_external_and_clock_operands() {
    primitive_model(dae::DaeLiteral::Integer(1))
        .unwrap()
        .inspect(|view| {
            for root in sizes(view) {
                assert_eq!(
                    crate::NumericEvaluator::new(view).expression(root).unwrap(),
                    [7.0]
                );
                for disabled in [false, true] {
                    let mut cache = ScalarCoordinateProjectionCache {
                        uncached_guard_memo: disabled,
                        ..Default::default()
                    };
                    let mut coordinates = Vec::new();
                    for_each_scalar_coordinate_cached(
                        view,
                        root,
                        0,
                        None,
                        &mut cache,
                        |coordinate, scalar| coordinates.push((coordinate, scalar)),
                    )
                    .unwrap();
                    assert!(coordinates.is_empty());
                    assert!(cache.function_results.is_empty());
                    assert!(cache.completed_folds.is_empty());
                    assert!(cache.fold_edges.is_empty());
                }
            }
        });
}

#[test]
fn size_dimension_type_and_rank_errors_remain_at_original_authorities() {
    assert!(matches!(
        primitive_model(dae::DaeLiteral::Real(1.5)),
        Err(dae::DaeConstructionError::InvalidSubscript { .. })
    ));
    for dimension in [0, 2, -1] {
        primitive_model(dae::DaeLiteral::Integer(dimension))
            .unwrap()
            .inspect(|view| {
                for root in sizes(view) {
                    let node = view.expression(root).unwrap();
                    let error = crate::NumericEvaluator::new(view)
                        .expression(root)
                        .unwrap_err();
                    assert_eq!(error.kind(), crate::NumericEvaluationErrorKind::OutOfBounds);
                    assert_eq!(error.span(), node.provenance().span());
                }
            });
    }
}

/// Scalar Abs traverses values; these fixtures also retain declined array/Integer forms.
pub(in crate::projection) fn primitive_abs_model() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "abs(opaque()); abs(sampled); abs(real); abs(array); abs(integer)";
    let source = sources.add("abs_value.mo", text);
    let at = provenance(source, 0, text.len());
    dae::Dae::construct(sources, |model| {
        let real = model.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let operands = [
            external_array(model, real, at)?,
            clock_array(model, real, at)?,
        ];
        model.expressions(|e| {
            for operand in operands {
                e.at(at).builtin(dae::PureBuiltin::Abs, [operand])?;
            }
            let scalar = e.at(at).literal(dae::DaeLiteral::Real(-1.0))?;
            e.at(at).builtin(dae::PureBuiltin::Abs, [scalar])?;
            let array = e.at(at).array([scalar, scalar])?;
            e.at(at).builtin(dae::PureBuiltin::Abs, [array])?;
            let integer = e.at(at).literal(dae::DaeLiteral::Integer(-1))?;
            e.at(at).builtin(dae::PureBuiltin::Abs, [integer])?;
            Ok(())
        })
    })
    .unwrap()
}
