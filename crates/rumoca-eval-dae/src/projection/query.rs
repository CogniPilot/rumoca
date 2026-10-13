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
        let Some(query) = self.relevant else {
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
                PlainActual::Coordinate(coordinate) => !query.selects(coordinate),
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

impl<'dae> Projection<'_, 'dae> {
    /// Whether a model-level expression reads no coordinate the named query
    /// selects, so it contributes no selected dependency at any scalar.
    ///
    /// Only model coordinates are selectable, and a function body reaches the
    /// model only through its actuals, so the operands of the expression are
    /// the complete set of coordinates it can read. The answer belongs to the
    /// expression, not to a scalar view of it, and is derived once.
    pub(super) fn expression_is_query_free(&mut self, expression: dae::ExprId<'dae>) -> bool {
        let Some(query) = self.relevant.and_then(ProjectionQuery::named) else {
            return false;
        };
        if !self.function_frames.is_empty() {
            return false;
        }
        let key = (expression.index(), query);
        if let Some(free) = self.cache.query_free_expressions.get(&key) {
            return *free;
        }
        let mut free = true;
        dae::for_each_expression_pruned(self.view, expression, |_, node| {
            if let dae::ExpressionOperation::Coordinate(coordinate) = node.operation() {
                free &= !query.selects(coordinate);
            }
            free
        });
        self.cache.query_free_expressions.insert(key, free);
        free
    }
}
