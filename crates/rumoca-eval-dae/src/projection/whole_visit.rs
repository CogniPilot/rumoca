//! Repeated whole-tensor walks in one exact summary membership scope.
use super::{FunctionFrame, Projection, ScalarExpressionDependency, dae, visited::WholeVisit};

impl<'dae> Projection<'_, 'dae> {
    pub(super) fn whole_visit(
        &mut self,
        expression: dae::ExprId<'dae>,
    ) -> Option<(WholeVisit, bool)> {
        #[cfg(test)]
        if self.cache.uncached_whole_visits {
            return None;
        }
        // A fold boundary must still issue its graph edge. Eligible fragments
        // must still replay their complete inventory into active recordings.
        if matches!(
            self.node(expression).operation(),
            dae::ExpressionOperation::FunctionFoldParameter { .. }
                | dae::ExpressionOperation::FunctionFoldOutput { .. }
        ) {
            return None;
        }
        let Some(FunctionFrame::Summary { function, .. }) = self.function_frames.last() else {
            return None;
        };
        let function = function.index();
        let key = ScalarExpressionDependency {
            activation: self.activation,
            expression: expression.index(),
            field: None,
            scalar: 0,
            domain_context: self.expression_domain_context(expression),
        };
        let capture = self
            .function_summary_captures
            .last_mut()
            .filter(|capture| capture.function == function)?;
        if capture.fragments.eligible(self.view, expression) {
            return None;
        }
        Some(capture.visited.whole_visit(key))
    }

    pub(super) fn finish_whole_visit(&mut self, visit: WholeVisit) {
        if let Some(capture) = self.function_summary_captures.last_mut() {
            // The membership owner/generation is checked even if a nested
            // projection changed the currently active capture or cleared it.
            capture.visited.finish_whole(visit);
        }
    }
}
