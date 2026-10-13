//! Dense lookup of the structural unknown index of a continuous coordinate.
//!
//! The scalars of one variable are numbered consecutively, so a variable's
//! unknowns are one contiguous run: its first unknown index and its scalar
//! count identify every one of them. The table holds that run per variable
//! identity and resolves a coordinate with two array reads.

use rumoca_ir_dae as dae;

#[derive(Debug, Clone, Copy, Default)]
struct UnknownRun {
    first: usize,
    count: u32,
}

#[derive(Debug, Default)]
pub(super) struct UnknownIndex {
    derivatives: Vec<UnknownRun>,
    algebraics: Vec<UnknownRun>,
}

impl UnknownIndex {
    pub(super) fn insert_derivative(&mut self, state: u32, first: usize, count: u32) {
        Self::insert(&mut self.derivatives, state, first, count);
    }

    pub(super) fn insert_algebraic(&mut self, variable: u32, first: usize, count: u32) {
        Self::insert(&mut self.algebraics, variable, first, count);
    }

    fn insert(runs: &mut Vec<UnknownRun>, variable: u32, first: usize, count: u32) {
        let slot = variable as usize;
        if runs.len() <= slot {
            runs.resize(slot + 1, UnknownRun::default());
        }
        runs[slot] = UnknownRun { first, count };
    }

    /// The unknown index of scalar `scalar` of `coordinate`, when the
    /// coordinate is a derivative or algebraic unknown.
    pub(super) fn resolve(
        &self,
        coordinate: dae::CoordinateView<'_>,
        scalar: usize,
    ) -> Option<usize> {
        let scalar = u32::try_from(scalar).ok()?;
        let (runs, variable) = match coordinate {
            dae::CoordinateView::Derivative(state) => (&self.derivatives, state.index()),
            dae::CoordinateView::Algebraic(variable) => (&self.algebraics, variable.index()),
            dae::CoordinateView::Parameter(_)
            | dae::CoordinateView::Input(_)
            | dae::CoordinateView::State(_)
            | dae::CoordinateView::DiscreteReal(_)
            | dae::CoordinateView::DiscreteValue(_)
            | dae::CoordinateView::PreDiscreteReal(_)
            | dae::CoordinateView::PreDiscreteValue(_)
            // A `pre()` read is the coordinate's settled left limit at event
            // entry, not the unknown itself, so it carries no incidence: the
            // Mean shape's `y_last = f*pre(x)` must not read as an equation
            // that solves for `x`.
            | dae::CoordinateView::PreState(_)
            | dae::CoordinateView::PreAlgebraic(_)
            | dae::CoordinateView::Time
            | dae::CoordinateView::ClockInterval(_)
            | dae::CoordinateView::Condition(_)
            | dae::CoordinateView::Delay(_)
            | dae::CoordinateView::Previous(_)
            | dae::CoordinateView::Terminal(_)
            | dae::CoordinateView::Binder(_)
            | dae::CoordinateView::FunctionParameter(_) => return None,
        };
        let run = runs.get(variable as usize)?;
        (scalar < run.count).then(|| run.first + scalar as usize)
    }
}
