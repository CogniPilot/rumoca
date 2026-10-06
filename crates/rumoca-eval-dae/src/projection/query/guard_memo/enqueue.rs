use super::*;

impl<'dae> Projection<'_, 'dae> {
    pub(in crate::projection) fn enqueue_fold(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        field: Option<usize>,
        scalar: usize,
    ) -> Result<(), ProjectionError> {
        let transition = self
            .view
            .function_fold(fold)
            .expect("checked fold resolves");
        let parent = self.view.domain(transition.domain()).unwrap().parent();
        let context = self.domain_contexts.for_domain(self.view, parent);
        let node = fold_graph::FoldNode {
            activation: self.activation,
            fold,
            carried,
            field,
            scalar,
            initial: transition.initial_values().rhs(carried as usize).unwrap(),
            update: transition.update_values().rhs(carried as usize).unwrap(),
            parent: self.domain_contexts.snapshot(context),
        };
        if let Some(dependencies) = self.cache.completed_folds.get(&node).cloned() {
            #[cfg(test)]
            {
                self.cache.fold_reuses += 1;
            }
            for dependency in dependencies.iter().cloned() {
                self.capture_function_parameter(
                    fold.function(),
                    dependency,
                    transition.provenance().span(),
                )?;
            }
        } else {
            self.record_guard_pending(&node);
            self.fold_summary_capture(fold.function())
                .unwrap()
                .folds
                .enqueue(node);
        }
        Ok(())
    }
}
