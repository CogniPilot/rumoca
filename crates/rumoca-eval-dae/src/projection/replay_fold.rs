//! Cached node bodies reenter the original queue; closures are never inlined.
use super::{Projection, ProjectionError, fold_nodes};

impl<'dae> Projection<'_, 'dae> {
    pub(super) fn project_or_replay_fold_node(
        &mut self,
        node: &fold_nodes::Node<'dae>,
    ) -> Result<(), ProjectionError> {
        let Some(recipe) = self.cache.fold_recipes.get(node).cloned() else {
            #[cfg(test)]
            {
                self.cache.fold_walks += 1;
            }
            return self.project_fold_node(node);
        };
        #[cfg(test)]
        {
            self.cache.fold_reuses += 1;
        }
        let span = self
            .view
            .function_fold(node.fold)
            .expect("checked fold resolves")
            .provenance()
            .span();
        for dependency in recipe.direct().iter().cloned() {
            self.capture_function_parameter(node.fold.function(), dependency, span)?;
        }
        let capture = self
            .fold_summary_capture(node.fold.function())
            .expect("the same summary owns the recipe");
        capture.folds.replay_edges(&recipe);
        Ok(())
    }
}
