//! Aggregate model coordinates captured by shared typed expression lowering.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use std::collections::HashMap;

/// One semantic model-storage coordinate captured by a typed owner.
///
/// This key deliberately retains the aggregate coordinate identity. A tensor
/// coordinate therefore owns one typed register regardless of its element
/// count; only the final execution/emission adapter materializes storage
/// elements.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) enum ModelCoordinateKey<'dae> {
    Parameter(dae::ParameterId<'dae>),
    Input(dae::InputId<'dae>),
    State(dae::StateId<'dae>),
    Derivative(dae::StateId<'dae>),
    Algebraic(dae::AlgebraicId<'dae>),
    DiscreteReal(dae::DiscreteRealId<'dae>),
    DiscreteValue(dae::DiscreteValueId<'dae>),
    PreDiscreteReal(dae::DiscreteRealId<'dae>),
    PreDiscreteValue(dae::DiscreteValueId<'dae>),
    PreState(dae::StateId<'dae>),
    PreAlgebraic(dae::AlgebraicId<'dae>),
    Time,
    ClockInterval(dae::PeriodicClockId<'dae>),
    Condition(dae::ConditionId<'dae>),
    Delay(dae::DelayId<'dae>),
    Previous(dae::PreviousId<'dae>),
    Terminal(dae::TerminalId<'dae>),
}

impl<'dae> ModelCoordinateKey<'dae> {
    pub(super) fn from_view(coordinate: dae::CoordinateView<'dae>) -> Option<Self> {
        Some(match coordinate {
            dae::CoordinateView::Parameter(id) => Self::Parameter(id),
            dae::CoordinateView::Input(id) => Self::Input(id),
            dae::CoordinateView::State(id) => Self::State(id),
            dae::CoordinateView::Derivative(id) => Self::Derivative(id),
            dae::CoordinateView::Algebraic(id) => Self::Algebraic(id),
            dae::CoordinateView::DiscreteReal(id) => Self::DiscreteReal(id),
            dae::CoordinateView::DiscreteValue(id) => Self::DiscreteValue(id),
            dae::CoordinateView::PreDiscreteReal(id) => Self::PreDiscreteReal(id),
            dae::CoordinateView::PreDiscreteValue(id) => Self::PreDiscreteValue(id),
            dae::CoordinateView::PreState(id) => Self::PreState(id),
            dae::CoordinateView::PreAlgebraic(id) => Self::PreAlgebraic(id),
            dae::CoordinateView::Time => Self::Time,
            dae::CoordinateView::ClockInterval(id) => Self::ClockInterval(id),
            dae::CoordinateView::Condition(id) => Self::Condition(id),
            dae::CoordinateView::Delay(id) => Self::Delay(id),
            dae::CoordinateView::Previous(id) => Self::Previous(id),
            dae::CoordinateView::Terminal(id) => Self::Terminal(id),
            dae::CoordinateView::Binder(_) | dae::CoordinateView::FunctionParameter(_) => {
                return None;
            }
        })
    }

    pub(super) const fn stable_key(self) -> (u8, u32) {
        match self {
            Self::Parameter(id) => (0, id.index()),
            Self::Input(id) => (1, id.index()),
            Self::State(id) => (2, id.index()),
            Self::Derivative(id) => (3, id.index()),
            Self::Algebraic(id) => (4, id.index()),
            Self::DiscreteReal(id) => (5, id.index()),
            Self::DiscreteValue(id) => (6, id.index()),
            Self::PreDiscreteReal(id) => (7, id.index()),
            Self::PreDiscreteValue(id) => (8, id.index()),
            Self::PreState(id) => (9, id.index()),
            Self::PreAlgebraic(id) => (10, id.index()),
            Self::Time => (11, 0),
            Self::ClockInterval(id) => (12, id.index()),
            Self::Condition(id) => (13, id.index()),
            Self::Delay(id) => (14, id.index()),
            Self::Previous(id) => (15, id.index()),
            Self::Terminal(id) => (16, id.index()),
        }
    }
}

impl<'dae> ModelCoordinateKey<'dae> {
    pub(super) fn coordinate(self) -> dae::CoordinateView<'dae> {
        match self {
            Self::Parameter(id) => dae::CoordinateView::Parameter(id),
            Self::Input(id) => dae::CoordinateView::Input(id),
            Self::State(id) => dae::CoordinateView::State(id),
            Self::Derivative(id) => dae::CoordinateView::Derivative(id),
            Self::Algebraic(id) => dae::CoordinateView::Algebraic(id),
            Self::DiscreteReal(id) => dae::CoordinateView::DiscreteReal(id),
            Self::DiscreteValue(id) => dae::CoordinateView::DiscreteValue(id),
            Self::PreDiscreteReal(id) => dae::CoordinateView::PreDiscreteReal(id),
            Self::PreDiscreteValue(id) => dae::CoordinateView::PreDiscreteValue(id),
            Self::PreState(id) => dae::CoordinateView::PreState(id),
            Self::PreAlgebraic(id) => dae::CoordinateView::PreAlgebraic(id),
            Self::ClockInterval(id) => dae::CoordinateView::ClockInterval(id),
            Self::Condition(id) => dae::CoordinateView::Condition(id),
            Self::Delay(id) => dae::CoordinateView::Delay(id),
            Self::Previous(id) => dae::CoordinateView::Previous(id),
            Self::Terminal(id) => dae::CoordinateView::Terminal(id),
            Self::Time => dae::CoordinateView::Time,
        }
    }
}

pub(super) fn collect_model_coordinate_types<'dae>(
    view: dae::DaeView<'dae>,
    expressions: impl IntoIterator<Item = dae::ExprId<'dae>>,
    initial: impl IntoIterator<Item = (ModelCoordinateKey<'dae>, dae::ValueTypeId<'dae>)>,
    provenance: rumoca_core::Span,
) -> Result<
    Vec<(ModelCoordinateKey<'dae>, dae::ValueTypeId<'dae>)>,
    solve::SolveProgramConstructionError,
> {
    let mut coordinates = initial.into_iter().collect::<HashMap<_, _>>();
    let mut mismatch = false;
    for expression in expressions {
        dae::for_each_expression(view, expression, |_, node| {
            let dae::ExpressionOperation::Coordinate(coordinate) = node.operation() else {
                return;
            };
            let Some(key) = ModelCoordinateKey::from_view(coordinate) else {
                return;
            };
            if let Some(previous) = coordinates.insert(key, node.value_type_id()) {
                mismatch |= previous != node.value_type_id();
            }
        });
    }
    if mismatch {
        return Err(solve::SolveProgramConstructionError::InvalidCallInterface { provenance });
    }
    let mut coordinates = coordinates.into_iter().collect::<Vec<_>>();
    coordinates.sort_by_key(|(key, _)| key.stable_key());
    Ok(coordinates)
}
