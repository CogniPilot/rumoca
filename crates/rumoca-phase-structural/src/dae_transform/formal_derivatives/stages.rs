//! STRUCT-T07: compact source-bound views of the differential solution stages.

use super::*;
use std::ops::Range;

pub(in crate::dae_transform) struct EquationProlongation {
    pub original: usize,
    pub derivatives: Range<usize>,
}

impl EquationProlongation {
    fn owner(&self, order: usize) -> usize {
        if order == 0 {
            self.original
        } else {
            self.derivatives.start + order - 1
        }
    }
}

/// One differential level; full numerical rank remains a separate obligation.
#[derive(Clone, Copy)]
pub struct FormalDerivativeStage<'map, 'source, 'formal> {
    system: FormalDerivativeView<'map, 'source, 'formal>,
    distance: usize,
}

/// A complete source declaration and its corresponding formal derivative tensor.
#[derive(Clone, Copy)]
pub struct FormalStageCoordinate<'source, 'formal> {
    source: dae::VariableView<'source>,
    value: dae::VariableView<'formal>,
    order: usize,
}

/// A complete equation owner and its derivative in the constructed formal root.
#[derive(Clone, Copy)]
pub struct FormalStageEquation<'source, 'formal> {
    source: dae::ContinuousOwnerView<'source>,
    value: dae::ContinuousOwnerView<'formal>,
    order: usize,
}

impl<'map, 'source, 'formal> FormalDerivativeView<'map, 'source, 'formal> {
    /// Stages run from the lowest value constraints to the highest derivatives.
    pub fn stages(
        self,
    ) -> impl DoubleEndedIterator<Item = FormalDerivativeStage<'map, 'source, 'formal>> {
        let depth = self
            .coordinates
            .iter()
            .map(|coordinates| coordinates.len() - 1)
            .max()
            .unwrap_or(0);
        (0..=depth)
            .rev()
            .map(move |distance| FormalDerivativeStage {
                system: self,
                distance,
            })
    }
}

impl<'source, 'formal> FormalDerivativeStage<'_, 'source, 'formal> {
    /// Formal derivative order minus the source's certified tensor offset.
    pub fn level(self) -> i64 {
        -(self.distance as i64)
    }

    pub fn coordinates(self) -> impl Iterator<Item = FormalStageCoordinate<'source, 'formal>> {
        self.system
            .source
            .variables()
            .filter_map(move |(id, variable)| {
                if !matches!(
                    variable.role(),
                    dae::VariableRole::State
                        | dae::VariableRole::Algebraic
                        | dae::VariableRole::Output
                ) {
                    return None;
                }
                let coordinates = &self.system.coordinates[id.index() as usize];
                let order = (coordinates.len() - 1).checked_sub(self.distance)?;
                Some(FormalStageCoordinate {
                    source: variable,
                    value: self
                        .system
                        .view
                        .variable(
                            self.system
                                .coordinate(id, order)
                                .expect("formal coordinate"),
                        )
                        .expect("formal coordinate declaration"),
                    order,
                })
            })
    }

    pub fn equations(self) -> impl Iterator<Item = FormalStageEquation<'source, 'formal>> {
        self.system.equations.iter().filter_map(move |equation| {
            let order = equation.derivatives.len().checked_sub(self.distance)?;
            Some(FormalStageEquation {
                source: self
                    .system
                    .source
                    .continuous_owner(equation.original)
                    .expect("source equation owner"),
                value: self
                    .system
                    .view
                    .continuous_owner(equation.owner(order))
                    .expect("constructed formal equation owner"),
                order,
            })
        })
    }

    pub fn scalar_coordinate_count(self) -> usize {
        self.coordinates()
            .map(|coordinate| coordinate.value.scalar_count())
            .sum()
    }

    pub fn scalar_equation_count(self) -> usize {
        self.equations()
            .map(|equation| match equation.value {
                dae::ContinuousOwnerView::Residual { .. } => 1,
                dae::ContinuousOwnerView::Structured { family, .. } => {
                    family.scalar_rows() as usize
                }
            })
            .sum()
    }

    /// Structural freedom only; singular numerical Jacobians can invalidate it.
    pub fn formal_dimension(self) -> usize {
        self.scalar_coordinate_count()
            .checked_sub(self.scalar_equation_count())
            .expect("certified matching injects stage equations into coordinates")
    }
}

impl<'source, 'formal> FormalStageCoordinate<'source, 'formal> {
    pub fn source(self) -> dae::VariableId<'source> {
        self.source.id()
    }

    pub fn value(self) -> dae::VariableId<'formal> {
        self.value.id()
    }

    pub fn source_variable(self) -> dae::VariableView<'source> {
        self.source
    }

    pub fn value_variable(self) -> dae::VariableView<'formal> {
        self.value
    }

    pub fn order(self) -> usize {
        self.order
    }
}

impl<'source, 'formal> FormalStageEquation<'source, 'formal> {
    pub fn source(self) -> dae::ContinuousOwnerView<'source> {
        self.source
    }

    pub fn value(self) -> dae::ContinuousOwnerView<'formal> {
        self.value
    }

    pub fn order(self) -> usize {
        self.order
    }
}
