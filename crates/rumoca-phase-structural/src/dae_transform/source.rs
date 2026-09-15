use rumoca_ir_dae as dae;

use super::constraints::DifferentiationFacts;
use super::demotion_bounds::DemotionRowBounds;

/// One immutable candidate-round source and the facts collected from it.
pub(super) struct ReductionSource<'model> {
    model: &'model dae::Dae,
    facts: DifferentiationFacts,
    pub(super) demotion_rows: DemotionRowBounds,
}

impl<'model> ReductionSource<'model> {
    pub(super) fn new(model: &'model dae::Dae) -> Self {
        Self {
            model,
            facts: model.inspect(DifferentiationFacts::collect),
            demotion_rows: model.inspect(DemotionRowBounds::collect),
        }
    }

    pub(super) const fn model(&self) -> &'model dae::Dae {
        self.model
    }

    pub(super) fn inspect<R>(
        &self,
        inspect: impl for<'dae> FnOnce(dae::DaeView<'dae>, &DifferentiationFacts) -> R,
    ) -> R {
        self.model.inspect(|view| inspect(view, &self.facts))
    }
}
