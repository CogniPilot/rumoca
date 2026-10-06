use super::*;

impl<'dae> Projection<'_, 'dae> {
    pub(in crate::projection) fn new_validation_memo(
        &self,
        function: dae::FunctionId<'dae>,
        actuals: &[dae::ExprId<'dae>],
    ) -> Option<ValidationMemo<'dae>> {
        #[cfg(test)]
        if self.cache.uncached_validation_memo {
            return None;
        }
        Some(ValidationMemo::new(function, actuals.to_vec()))
    }

    fn root_validation_summary(&self) -> bool {
        self.function_frames.len() == 1
            && matches!(self.function_frames.first(), Some(FunctionFrame::Summary { function, .. })
                if self.validation == Some(function.index()))
    }

    pub(in crate::projection) fn validation_memo_forces_walk(&self) -> bool {
        self.validation_memo
            .as_ref()
            .is_some_and(|memo| memo.recording != 0)
            && self.root_validation_summary()
    }

    fn begin_validation_memo(&mut self, expression: dae::ExprId<'dae>, scalar: usize) -> Start {
        if self.validating_actuals
            || self.activation == Activation::Conditional
            || self.validation_memo.is_none()
            || !self.root_validation_summary()
            || !matches!(
                self.node(expression).operation(),
                dae::ExpressionOperation::Index { .. }
            )
        {
            return Start::None;
        }
        self.validation_memo.as_mut().map_or(Start::None, |memo| {
            memo.begin(self.view, expression, scalar, &self.domain_contexts.points)
        })
    }

    pub(in crate::projection) fn expression(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<(), ProjectionError> {
        if !self.validating_actuals {
            self.guard_memo_observe(expression);
        }
        let memo = self.begin_validation_memo(expression, scalar);
        if matches!(memo, Start::Hit) {
            return Ok(());
        }
        let result = self.expression_fragment(expression, scalar);
        if let Start::Checking(key) = memo {
            self.validation_memo
                .as_mut()
                .expect("this invocation opened a check")
                .finish(key, result.is_ok());
        }
        result
    }

    fn expression_fragment(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let fragment = self.begin_parameter_fragment(expression, None, scalar_index);
        if let parameter_fragments::Start::Cached(dependencies)
        | parameter_fragments::Start::Imported(dependencies) = &fragment
        {
            self.replay_parameter_fragment(
                dependencies,
                matches!(fragment, parameter_fragments::Start::Imported(_)),
            );
            return Ok(());
        }
        let result = self.expression_uncached(expression, scalar_index);
        if matches!(fragment, parameter_fragments::Start::Checking) {
            self.finish_parameter_fragment(result.is_ok());
        }
        result
    }
}
