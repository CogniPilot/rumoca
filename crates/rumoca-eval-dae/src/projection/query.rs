//! Declaration-level queries: a pure call whose actuals hold no queried
//! coordinate contributes no queried dependency.
use super::*;

#[derive(Clone, Copy)]
pub(super) enum PlainActual<'dae> {
    Literal,
    Coordinate(dae::CoordinateView<'dae>),
    Unsupported,
}

impl<'dae> Projection<'_, 'dae> {
    pub(super) fn query_free_arguments(
        &mut self,
        call: dae::ExprId<'dae>,
        arguments: dae::ExpressionOperands<'dae>,
    ) -> bool {
        let Some(relevant) = self.relevant else {
            return false;
        };
        if !self.function_frames.is_empty() {
            return false;
        }
        // Only immutable argument facts belong to this exact source call.
        // Each projection still evaluates its current predicate in argument order.
        if !self.cache.plain_call_arguments.contains_key(&call.index()) {
            let actuals = arguments
                .iter()
                .map(|argument| self.plain_actual(argument))
                .collect();
            self.cache
                .plain_call_arguments
                .insert(call.index(), actuals);
        }
        self.cache.plain_call_arguments[&call.index()]
            .iter()
            .all(|argument| match *argument {
                PlainActual::Literal => true,
                PlainActual::Coordinate(coordinate) => !relevant(coordinate),
                PlainActual::Unsupported => false,
            })
    }

    fn plain_actual(&mut self, argument: dae::ExprId<'dae>) -> PlainActual<'dae> {
        #[cfg(test)]
        self.cache
            .plain_argument_cache_lookups
            .set(self.cache.plain_argument_cache_lookups.get() + 1);
        if let Some(actual) = self.cache.plain_actuals.get(&argument.index()) {
            return *actual;
        }
        #[cfg(test)]
        self.cache
            .plain_argument_classifications
            .set(self.cache.plain_argument_classifications.get() + 1);
        let node = self.node(argument);
        let actual = match node.operation() {
            dae::ExpressionOperation::Literal(_) => PlainActual::Literal,
            dae::ExpressionOperation::Coordinate(coordinate)
                if !node.value_type().is_record()
                    && !matches!(
                        coordinate,
                        dae::CoordinateView::FunctionParameter(_) | dae::CoordinateView::Binder(_)
                    ) =>
            {
                PlainActual::Coordinate(coordinate)
            }
            _ => PlainActual::Unsupported,
        };
        self.cache.plain_actuals.insert(argument.index(), actual);
        actual
    }
}
