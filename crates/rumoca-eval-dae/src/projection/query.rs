//! Query-free arguments remove capture work, never the authoritative error walk.
pub(super) mod guard_memo;
pub(super) mod validation_memo;

use super::*;

impl<'dae> Projection<'_, 'dae> {
    pub(super) fn query_free_arguments(&self, arguments: &[dae::ExprId<'dae>]) -> bool {
        let Some(relevant) = self.relevant else {
            return false;
        };
        if !self.function_frames.is_empty() {
            return false;
        }
        arguments
            .iter()
            .all(|argument| match self.node(*argument).operation() {
                dae::ExpressionOperation::Literal(_) => true,
                dae::ExpressionOperation::Coordinate(coordinate) => {
                    // Record actuals still require the original field-wise
                    // substitution; a coordinate may be an unsupported record
                    // source even when it has no incidence for this query.
                    if self.node(*argument).value_type().is_record() {
                        return false;
                    }
                    !matches!(
                        coordinate,
                        dae::CoordinateView::FunctionParameter(_) | dae::CoordinateView::Binder(_)
                    ) && !relevant(coordinate)
                }
                _ => false,
            })
    }

    pub(super) fn validate_query_free_call(
        &mut self,
        function: dae::FunctionId<'dae>,
        output: u32,
        field: Option<usize>,
        scalar: usize,
        arguments: Vec<dae::ExprId<'dae>>,
        span: Span,
    ) -> Result<(), ProjectionError> {
        // A function validated as a root has empty formal inventories; the
        // same function used as a nested callee needs its full inventory to
        // check substitutions/addresses. Keep those roles in separate caches.
        // A function's validation cache is created on its first validation and
        // moved out while this nested walk borrows the projection.
        let mut cache = std::mem::take(
            self.cache
                .query_validation
                .entry(function.index())
                .or_default(),
        );
        let mut ignored = |_, _| {};
        let mut validation = Projection {
            activation: self.activation,
            validating_actuals: self.validating_actuals,
            view: self.view,
            domain_contexts: domain_context::DomainContexts::new(
                self.domain_contexts.points.clone(),
            ),
            integer_stack: vec![false; self.view.expression_count()],
            function_frames: Vec::new(),
            frame_memos: Vec::new(),
            function_call_active: HashSet::default(),
            function_fold_active: HashSet::default(),
            function_summary_captures: Vec::new(),
            model_visited: visited::Visited::default(),
            cache: &mut cache,
            visit: &mut ignored,
            relevant: None,
            validation: Some(function.index()),
            validation_memo: self.new_validation_memo(function, &arguments),
            guard_memo: self.new_guard_memo(function, &arguments),
        };
        let dependency = FunctionResultDependency {
            function: function.index(),
            output,
            field,
            scalar,
        };
        let result = validation.project_function_result(dependency, function, arguments, span);
        #[cfg(test)]
        let hits = validation
            .validation_memo
            .as_ref()
            .map_or(0, |memo| memo.hits);
        #[cfg(test)]
        let guard_hits = validation.guard_memo.as_ref().map_or(0, |memo| memo.hits);
        drop(validation);
        #[cfg(test)]
        {
            self.cache.validation_memo_hits += hits;
            self.cache.guard_memo_hits += guard_hits;
        }
        self.cache.query_validation.insert(function.index(), cache);
        result
    }
}
