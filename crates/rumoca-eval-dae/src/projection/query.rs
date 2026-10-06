//! Declaration-level queries: a pure call whose actuals hold no queried
//! coordinate contributes no queried dependency.
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
                    // A record actual is substituted field by field, so its
                    // coordinate is not a single queried read.
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
}
